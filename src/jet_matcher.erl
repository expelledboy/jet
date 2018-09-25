-module(jet_matcher).

-export([match/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

match(#{<<"and">> := Ands}, Json) when is_list(Ands) ->
    lists:all(fun (And) -> match(And, Json) end, Ands);
match(#{<<"or">> := Ors}, Json) when is_list(Ors) ->
    lists:any(fun (Or) -> match(Or, Json) end, Ors);
match(#{<<"not">> := Not}, Json) when is_map(Not) ->
    not match(Not, Json);
% object containing 1..n property clauses
% treat as if wrapped in implicit 'and'
match(Pattern, Json) when is_map(Pattern) ->
    maps:fold(fun (Property, Conditions, IsMatch) ->
                    IsMatch and match_property(Property, Conditions, Json)
              end, true, Pattern).

% property clause
match_property(Property, Conditions, Json) when is_map(Conditions) ->
    %get the first (and only) property name (first key in map)
    Value1 = jet_pointer:get_prop_value(Property, Json),
    %get the conditions to iterate through
    maps:fold(fun (Operator, Value2, IsMatch) ->
                    IsMatch and match_condition(Operator, Value2, Value1)
              end, true, Conditions).

% These will fail if they aren't children of a property clause
% because the property clause match() puts the condition atom in
% a tuple with the value. This is in case there are objects to
% match that use one of these operators as a property name

%%% =
match_condition(<<"=">>, Operand, Value) when is_boolean(Operand), is_boolean(Value);
                                              is_integer(Operand), is_integer(Value);
                                              is_float(Operand),   is_float(Value);
                                              is_float(Operand),   is_integer(Value);
                                              is_integer(Operand), is_float(Value);
                                              is_binary(Operand),  is_binary(Value);
                                              is_list(Operand),    is_list(Value);
                                              is_map(Operand),     is_map(Value) ->
    Value == Operand;
match_condition(<<"=">>, Operand, Value) when is_integer(Operand), is_binary(Value) ->
    string:length(Value) == Operand;
match_condition(<<"=">>, Operand, Value) when is_integer(Operand), is_list(Value) ->
    length(Value) == Operand;
match_condition(<<"=">>, Operand, Value) ->
    throw({operands_not_comparable, <<"=">>, Value, Operand});

%%% !=
match_condition(<<"!=">>, Operand, Value) ->
    not (match_condition(<<"=">>, Operand, Value));

%%% >
match_condition(<<">">>, Operand, Value) when is_integer(Operand),  is_integer(Value);
                                              is_float(Operand),    is_float(Value);
                                              is_float(Operand),    is_integer(Value);
                                              is_integer(Operand),  is_float(Value);
                                              is_binary(Operand),   is_binary(Value) ->
        Value > Operand;
match_condition(<<">">>, Operand, Value) when is_list(Operand),     is_list(Value) ->
    length(Value) > length(Operand);
match_condition(<<">">>, Operand, Value) when is_integer(Operand),  is_binary(Value) ->
    string:length(Value) > Operand;
match_condition(<<">">>, Operand, Value) when is_integer(Operand),  is_list(Value) ->
    length(Value) > Operand;
match_condition(<<">">>, Operand, Value) ->
    throw({operands_not_comparable, <<">">>, Value, Operand});

%%% <
match_condition(<<"<">>, Operand, Value) when is_integer(Operand),  is_integer(Value);
                                              is_float(Operand),    is_float(Value);
                                              is_float(Operand),    is_integer(Value);
                                              is_integer(Operand),  is_float(Value);
                                              is_binary(Operand),   is_binary(Value) ->
    Value < Operand;
match_condition(<<"<">>, Operand, Value) when is_list(Operand), is_list(Value) ->
    length(Value) < length(Operand);
match_condition(<<"<">>, Operand, Value) when is_integer(Operand), is_binary(Value) ->
    string:length(Value) < Operand;
match_condition(<<"<">>, Operand, Value) when is_integer(Operand), is_list(Value) ->
    length(Value) < Operand;
match_condition(<<"<">>, Operand, Value) ->
    throw({operands_not_comparable, <<"<">>, Value, Operand});
%%% >=
match_condition(<<">=">>, Operand, Value) ->
    not (match_condition(<<"<">>, Operand, Value));

%%% <=
match_condition(<<"<=">>, Operand, Value) ->
    not (match_condition(<<">">>, Operand, Value));

%%% in
match_condition(<<"in">>, Operand, Value) when is_list(Operand) ->
    lists:member(Value, Operand);

%%% has
match_condition(<<"has">>, Operand, Value) when is_list(Value) ->
    lists:member(Operand, Value);

%%% type
match_condition(<<"type">>, <<"integer">>, Value) ->
    is_integer(Value);
match_condition(<<"type">>, <<"float">>, Value) ->
    is_float(Value);
match_condition(<<"type">>, <<"string">>, Value) ->
    is_binary(Value);
match_condition(<<"type">>, <<"array">>, Value) ->
    is_list(Value);
match_condition(<<"type">>, <<"object">>, Value) ->
    is_map(Value);
match_condition(<<"type">>, Operand, _Value) ->
    throw({unknown_type_operand, Operand});

%%% regex
match_condition(<<"regex">>, Operand, Value) ->
    {ok, MP} = re:compile(Operand),
    re:run(Value, MP) == {ok, match}.
