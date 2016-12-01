-module(rs_compile).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rs_compile_prv:init(State),
    {ok, State1}.
