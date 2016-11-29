-module(rustbar_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(NAMESPACE, rs).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace, ?NAMESPACE},
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 rs compile"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    case os:find_executable("cargo") of
        false ->
            erlang:error(cargo_not_found);
        CargoPath ->
            [begin
                 Opts = rebar_app_info:opts(AppInfo),
                 OutDir = rebar_app_info:out_dir(AppInfo),
                 SourceDir = filename:join(rebar_app_info:dir(AppInfo), "rust_files"),
                 CargoPort = erlang:open_port({spawn_executable, CargoPath}, [{cd, SourceDir}, {args, ["build"]}]),
                 Res = get_result(CargoPort, []),
                 io:format("res=~p cargo build~n", [Res])
             end || AppInfo <- Apps]
    end,
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("error: ~p", [Reason]).

get_result(Port, Acc) ->
    receive
        {Port, {data, Bytes}} ->
            io:format("got bytes"),
            get_result(Port, [Acc|Bytes]);
        {Port, eof} ->
            io:format("got eof"),
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    io:format("got closed"),
                    true
            end,
            receive
                {'EXIT',  Port,  _} ->
                    io:format("got EXIT"),
                    ok
            after 1 ->              % force context switch
                    io:format("got no EXIT"),
                    ok
            end,
            ExitCode =
                receive
                    {Port, {exit_status, Code}} ->
                        io:format("got exit status"),
                        Code
                end,
            {ExitCode, lists:flatten(Acc)}
    end.
