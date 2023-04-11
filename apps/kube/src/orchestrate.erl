%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(orchestrate).

-define(SleepInterval,60*1000).
-define(LockTimeout, 3*60*1000).

%% API
-export([
	 start/1,
	 start/2
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start(LockId)->
    start(LockId,?SleepInterval).

start(LockId,SleepInterval)->
    Result=case sd:call(dbetcd,db_lock,try_lock,[LockId,?LockTimeout],5000) of
	       {error,Reason}->
		   {error,["Failed calling dbetcd,db_lock,try_lock: ",Reason,LockId,?LockTimeout,?MODULE,?FUNCTION_NAME,?LINE]};
	       {badrpc,Reason}->
		   {error,["badrpc Failed calling dbetcd,db_lock,try_lock: ",Reason,LockId,?LockTimeout,?MODULE,?FUNCTION_NAME,?LINE]};
	       locked ->
		   timer:sleep(SleepInterval),
		   locked;
	       {ok,TransactionId} ->
		   ResultHostController=check_and_start_host_controllers(),
		%   ResultProvider=check_and_start_providers(),
		   timer:sleep(SleepInterval),
		   sd:call(dbetcd,db_lock,unlock,[LockId,TransactionId],5000),
		   {ok,ResultHostController,[not_implmented]}
	   end,
    rpc:cast(node(),kube,orchestrate_result,Result).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
check_and_start_host_controllers()->
    Result=case sd:call(dbetcd,db_host_spec,read,get_all_id,[],5000) of
	       {error,Reason}->
		   {error,["Failed calling dbetcd,db_host_spec,read,get_all_id: ",Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	       {badrpc,Reason}->
		   {error,["badrpc Failed calling dbetcd,db_host_spec,read,get_all_id: ",Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	       AllHostSpecs->
		   MissingHostControllers=[HostSpec||HostSpec<-AllHostSpecs,
						     false==lib_host:is_started_host_controller(HostSpec)],
		   [lib_host:start_host_controller(HostSpec)||HostSpec<-MissingHostControllers]			   
	   end,
    Result.


