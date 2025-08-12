%% Makes slex work as a rebar3 plugin
-module(slex).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = slex_rebar3_plugin:init(State),
    {ok, State1}.
