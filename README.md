# Json Erlang Tools

### Json Pointers

__Get__

```erlang
Json = jet:decode(binary_to_list('{"user":{"profiles":[{"name":"anthony"}]}}')),
<<"anthony">> = jet_pointer:get("/user/profiles/0/name", Json),
#{ <<"name">> := <<"anthony">> }  = jet_pointer:get("/user/profiles/0", Data).
```

__Put__

```erlang
Json = jet:decode(binary_to_list('{"user":{"profiles":[{"name":"anthony"}]}}')),
ok = jet_pointer:put("/user/profiles/0/name", <<"andrew">>, Json),
<<"andrew">> = jet_pointer:get("/user/profiles/0/name", Json).
```

### Json Schema

__Validate__

```erlang
GoodJson = jet:decode(binary_to_list('{"name":"anthony"}')),
BadJson = jet:decode(binary_to_list('{"name":10}')),

Schema = jet:decode(binary_to_list('{"type":"object","properties":{"name":{"type":"string"}}')),
true = jet_schema:validate(Schema, GoodJson),
{false, _Errors} = jet_schema:validate(Schema, BadJson),

SchemaFilename = "./priv/schema.v1.person.json",
true = jet_schema:validate(SchemaFilename, GoodJson),
{false, _Errors} = jet_schema:validate(SchemaFilename, BadJson),

SchemaUrl = "http://domain.com/schemas/v1.person.json",
true = jet_schema:validate(SchemaUrl, GoodJson),
{false, _Errors} = jet_schema:validate(SchemaUrl, BadJson).
```

__Transform according to Json Schema__

```erlang
Json = jet:decode(binary_to_list('{"name":"anthony"}')),
Schema = jet:decode(binary_to_list('{"type":"object","properties":{"name":{"type":"string"}}')),
Opts = #{ key_type => atom, string_type => list },
{ok, #{ name => "anthony" }} = jet_translate:decode(Schema, Json, Opts).
```

##### Error Format

Will follow the specification [to be determined](https://github.com/json-schema-org/json-schema-spec/issues/643) as part of JSON Schema standard.

```erlang
#{ keyword := "oneOf",
   schema_path := "#/oneOf",
   message := "the instance did not match any of the subschemas",
   errors := [ #{ keyword := "additionalProperties",
                  schema_path := "#/oneOf/0/additionalProperties",
                  data_path := "/age",
                  message := "additional properties are not allowed" },
               #{ keyword := "enum",
                  schema_path := "#/oneOf/1/properties/membershipType/enum",
                  data_path := "/membershipType",
                  message := "value does not match any of the allowed values" } ] }.
```
