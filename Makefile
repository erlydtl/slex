ERL=erl
ERLC=erlc
REBAR=./rebar

all: compile test

compile:
	@$(REBAR) compile

test:
	@$(REBAR) eunit

clean:
	@$(REBAR) clean
