all: build

build:
	rebar3 compile

clean:
	rebar3 clean

test:
	rebar3 do eunit --cover, cover --verbose

.PHONY: all build clean test
