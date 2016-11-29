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
                 TomlFile = filename:join(SourceDir, "Cargo.toml"),
                 LibName = extract_lib_name(TomlFile),
                 CargoPort = erlang:open_port({spawn_executable, CargoPath}, [{cd, SourceDir}, {args, ["build"]}, exit_status, use_stdio]),
                 case get_result(CargoPort) of
                     {ok, _} ->
                         copy_lib(LibName, SourceDir, OutDir);
                     {error, Code} ->
                         erlang:error({cargo_failure, Code})
                 end
             end || AppInfo <- Apps]
    end,
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("error: ~p", [Reason]).

get_result(Port) ->
    receive
        {Port, {exit_status, 0}} ->
            {ok, 0};
        {Port, {exit_status, Code}} ->
            io:format("got exit ~p", [Code]),
            {error, Code}
    end.

copy_lib(LibName, SourceDir, OutDir) ->
    DebugLib = filename:join(SourceDir, "target", "debug", "lib"++LibName++".so"),
    OutPriv = filename:join(OutDir, "priv"),
    os:cmd("cp "++DebugLib++" "++OutPriv).

extract_lib_name(TomlFile) ->
    {ok, Toml} = etoml:file(TomlFile),
    #{<<"lib">> := #{<<"name">> := LibName}} = maps:from_list(Toml),
    binary_to_list(LibName).
