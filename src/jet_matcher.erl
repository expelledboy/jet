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

match(Pattern, Json) when is_map(Pattern) ->
    maps:fold(fun (Property, Conditions, IsMatch) ->
                    IsMatch and match_property(Property, Conditions, Json)
              end, true, Pattern).

match_property(Property, Conditions, Json) when is_map(Conditions) ->
    ValueToCompare = jet_pointer:get(Property, Json),
    maps:fold(fun (Operator, Comparator, IsMatch) ->
                    IsMatch and match_condition(Operator, Comparator, ValueToCompare)
              end, true, Conditions).

match_condition(<<"=">>, Comparator, Value) when is_boolean(Comparator), is_boolean(Value);
                                                 is_integer(Comparator), is_integer(Value);
                                                 is_float(Comparator),   is_float(Value);
                                                 is_float(Comparator),   is_integer(Value);
                                                 is_integer(Comparator), is_float(Value);
                                                 is_binary(Comparator),  is_binary(Value);
                                                 is_list(Comparator),    is_list(Value);
                                                 is_map(Comparator),     is_map(Value) ->
    Value == Comparator;
match_condition(<<"=">>, Comparator, Value) when is_integer(Comparator), is_binary(Value) ->
    string:length(Value) == Comparator;
match_condition(<<"=">>, Comparator, Value) when is_integer(Comparator), is_list(Value) ->
    length(Value) == Comparator;
match_condition(<<"=">>, Comparator, Value) ->
    throw({operands_not_comparable, <<"=">>, Value, Comparator});

match_condition(<<"!=">>, Comparator, Value) ->
    not (match_condition(<<"=">>, Comparator, Value));

match_condition(<<">">>, Comparator, Value) when is_integer(Comparator),  is_integer(Value);
                                                 is_float(Comparator),    is_float(Value);
                                                 is_float(Comparator),    is_integer(Value);
                                                 is_integer(Comparator),  is_float(Value);
                                                 is_binary(Comparator),   is_binary(Value) ->
    Value > Comparator;
match_condition(<<">">>, Comparator, Value) when is_list(Comparator),     is_list(Value) ->
    length(Value) > length(Comparator);
match_condition(<<">">>, Comparator, Value) when is_integer(Comparator),  is_binary(Value) ->
    string:length(Value) > Comparator;
match_condition(<<">">>, Comparator, Value) when is_integer(Comparator),  is_list(Value) ->
    length(Value) > Comparator;
match_condition(<<">">>, Comparator, Value) ->
    throw({operands_not_comparable, <<">">>, Value, Comparator});

match_condition(<<"<">>, Comparator, Value) when is_integer(Comparator),  is_integer(Value);
                                                 is_float(Comparator),    is_float(Value);
                                                 is_float(Comparator),    is_integer(Value);
                                                 is_integer(Comparator),  is_float(Value);
                                                 is_binary(Comparator),   is_binary(Value) ->
    Value < Comparator;
match_condition(<<"<">>, Comparator, Value) when is_list(Comparator),     is_list(Value) ->
    length(Value) < length(Comparator);
match_condition(<<"<">>, Comparator, Value) when is_integer(Comparator),  is_binary(Value) ->
    string:length(Value) < Comparator;
match_condition(<<"<">>, Comparator, Value) when is_integer(Comparator),  is_list(Value) ->
    length(Value) < Comparator;
match_condition(<<"<">>, Comparator, Value) ->
    throw({operands_not_comparable, <<"<">>, Value, Comparator});

match_condition(<<">=">>, Comparator, Value) ->
    not (match_condition(<<"<">>, Comparator, Value));

match_condition(<<"<=">>, Comparator, Value) ->
    not (match_condition(<<">">>, Comparator, Value));

match_condition(<<"in">>, Comparator, Value) when is_list(Comparator) ->
    lists:member(Value, Comparator);

match_condition(<<"contains">>, Comparator, Value) when is_list(Value) ->
    lists:member(Comparator, Value);

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
match_condition(<<"type">>, Type, _Value) ->
    throw({unknown_type, Type});

match_condition(<<"regex">>, Comparator, Value) ->
    {ok, MP} = re:compile(Comparator),
    {ok, match} == re:run(Value, MP).
