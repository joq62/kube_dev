%%%-------------------------------------------------------------------
%% @doc main public API
%% @end
%%%-------------------------------------------------------------------

-module(kube_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kube_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
