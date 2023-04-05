%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 15 Mar 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(kube).

-define(SERVER,kube_server).

%% API
-export([
	 %% host controller
	 start_host_controller/1,
	 stop_host_controller/1,
	 is_started_host_controller/1,
	 
	 %% provider 
	 load_provider/2,
	 start_provider/2,
	 unload_provider/2,
	 stop_provider/2,
	 is_loaded_provider/2,
	 is_started_provider/2,
	 
	 %% 
	 ping/0

	]).


%% Leader API
-export([
	 
	]).

-export([
	 start/0,
	 stop/0
	]).


%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_provider(ProviderSpec,HostSpec)->
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[ProviderSpec,HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=gen_server:call(?SERVER, {load_provider,ProviderSpec,HostSpec},infinity),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.

unload_provider(ProviderSpec,HostSpec)->
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[ProviderSpec,HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=gen_server:call(?SERVER, {unload_provider,ProviderSpec,HostSpec},infinity),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.
    
start_provider(ProviderSpec,HostSpec)->
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[ProviderSpec,HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=gen_server:call(?SERVER, {start_provider,ProviderSpec,HostSpec},infinity),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.

stop_provider(ProviderSpec,HostSpec)->
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[ProviderSpec,HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=gen_server:call(?SERVER, {stop_provider,ProviderSpec,HostSpec},infinity),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.

is_loaded_provider(ProviderSpec,HostSpec)->
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[ProviderSpec,HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=gen_server:call(?SERVER, {is_loaded_provider,ProviderSpec,HostSpec},infinity),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.   
 
is_started_provider(ProviderSpec,HostSpec)->
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[ProviderSpec,HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=gen_server:call(?SERVER, {is_started_provider,ProviderSpec,HostSpec},infinity),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_host_controller(HostSpec) ->
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=gen_server:call(?SERVER, {start_host_controller,HostSpec},infinity),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.
    
stop_host_controller(HostSpec) ->
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=gen_server:call(?SERVER, {stop_host_controller,HostSpec},infinity),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.

is_started_host_controller(HostSpec) ->
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[HostSpec]}]),
    T1=os:system_time(millisecond),
    Result=gen_server:call(?SERVER, {is_started_host_controller,HostSpec},infinity),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start()-> 
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[]}]),
    T1=os:system_time(millisecond),
    Result=gen_server:start_link({local, ?SERVER}, ?SERVER, [], []),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.

stop()-> 
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[]}]),
    T1=os:system_time(millisecond),
    Result=gen_server:call(?SERVER, {stop},infinity),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.


ping() ->
    io:format("Input: ~p~n",[{?MODULE,?FUNCTION_NAME,[]}]),
    T1=os:system_time(millisecond),
    Result=gen_server:call(?SERVER, {ping},infinity),
    T2=os:system_time(millisecond),
    io:format("Output: ~p~n",[{?MODULE,?FUNCTION_NAME,[Result]}]),
    io:format("Exection time (ms): ~p~n~n",[T2-T1]),
    Result.

%%%===================================================================
%%% Internal functions
%%%===================================================================

