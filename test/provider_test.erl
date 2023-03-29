%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2023 by c50 <joq62@c50>

-module(provider_test).

-export([start/0]).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

start()->
    io:format("started ~p~n",[{?MODULE}]),
    ?assertMatch(["kube","main"],lists:sort(db_provider_spec:get_all_id())),
    ok.
    


