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
    gen_server:call(?SERVER, {load_provider,ProviderSpec,HostSpec},infinity).
unload_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {unload_provider,ProviderSpec,HostSpec},infinity).
start_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {start_provider,ProviderSpec,HostSpec},infinity).
stop_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {stop_provider,ProviderSpec,HostSpec},infinity).
is_loaded_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {is_loaded_provider,ProviderSpec,HostSpec},infinity).
is_started_provider(ProviderSpec,HostSpec)->
    gen_server:call(?SERVER, {is_started_provider,ProviderSpec,HostSpec},infinity).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_host_controller(HostSpec) ->
    gen_server:call(?SERVER, {start_host_controller,HostSpec},infinity).
stop_host_controller(HostSpec) ->
    gen_server:call(?SERVER, {stop_host_controller,HostSpec},infinity).

is_started_host_controller(HostSpec) ->
    gen_server:call(?SERVER, {is_started_host_controller,HostSpec},infinity).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).


ping() ->
    gen_server:call(?SERVER, {ping},infinity).

%%%===================================================================
%%% Internal functions
%%%===================================================================

