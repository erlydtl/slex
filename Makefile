ERL=erl
ERLC=erlc
REBAR=./rebar

all: compile

compile:
	@$(REBAR) compile

test:
	@$(REBAR) eunit

clean:
	@$(REBAR) clean
