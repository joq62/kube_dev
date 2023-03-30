-module(kube_SUITE).

-compile(export_all).
-define(Cookie,a_cookie).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [setup,provider,
     start_nodes].


init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
 
    ok.



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_nodes(_Config)->
    ?assertMatch(glurk,host_server:create_node("c201")),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
provider(_Config)->
     ?assertMatch(ok,provider_test:start()),
    ok.
   

    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
setup(_Config)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ?assertMatch(ok,application:start(common)),
    ?assertMatch(pong,common:ping()),
    ?assertMatch(ok,application:start(sd)),
    ?assertMatch(pong,sd:ping()),
    ?assertMatch(ok,application:start(etcd)),
    ?assertMatch(pong,etcd:ping()),
    ?assertMatch(ok,application:start(kube)),
    ?assertMatch(pong,kube:ping()),
    ?assertMatch(pong,host_server:ping()),
    
    ok.
                                
