-module(slex_rebar3_plugin).

-export([init/1, do/1, format_error/1]).

-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).


%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    CompileProvider = providers:create([
        {namespace, slex},            % task namespace for slex files
        {name, compile},              % task name
        {module, ?MODULE},            % task implementation module
        {bare, true},                 % task can be run by the user
        {deps, [{default, app_discovery}]},  % dependencies on other tasks
        {example, "rebar3 slex compile"},  % usage example
        {opts, []},                   % options understood by the plugin
        {short_desc, "Compiles .slex files"},
        {desc, "Plugin for compiling .slex lexer definition files"}
    ]),
    State1 = rebar_state:add_provider(State, CompileProvider),
    {ok, State1}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Running slex...", []),
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    _ = [compile_app(App) || App <- Apps],
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


compile_app(AppInfo) ->
    Opts = rebar_app_info:opts(AppInfo),
    rebar_base_compiler:run(Opts,
                            [],
                            "src", ".slex", "src", ".erl",
                            fun compile_slex/3).

compile_slex(Source, Target, Opts) ->
    try slex_compiler:compile(Source, [{target, erl}, {out_dir, src}]) of
        {ok, Target} -> ok;
        {error, Error} ->
            ?DEBUG("compile ~p -> ~p ~n  fail: ~P~n", [Source, Target, Error, 10
]),
            rebar_base_compiler:error_tuple(Opts, Source, [{Source, [Error]}],
 [], [])
    catch
        throw:Error ->
            ?DEBUG("compile ~p -> ~p ~n  throw: ~P~n", [Source, Target, Error, 10]),
            rebar_base_compiler:error_tuple(Opts, Source, [{Source, [Error]}],
 [], [])
    end.
