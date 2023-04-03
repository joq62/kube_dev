%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(all).      
 
-export([start/1]).
-compile(export_all).

-include_lib("kernel/include/inet.hrl").


-define(KubeNode,node()).
-define(HostSpec1,"c200").
-define(Provider1,"kube").
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start([Arg1,Arg2])->
    io:format("Start ~p~n",[{Arg1,Arg2,?MODULE,?FUNCTION_NAME}]),

    ok=setup(),
    ok=create_vms(),
    {ok,ProviderNode,App}=lib_kube:load(?Provider1,?HostSpec1),
    ok=lib_kube:start(?Provider1,?HostSpec1),
    pong=rpc:call(ProviderNode,App,ping,[],5000),
 %   ok=ssh_test(),

    io:format("End testing  SUCCESS!! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
%    init:stop(),
%    timer:sleep(3000),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_vms()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    AllHostSpecs=lists:sort(db_host_spec:get_all_id()),
    CookieStr=atom_to_list(erlang:get_cookie()),
    R2=lib_host:start_nodes(AllHostSpecs,CookieStr),
    io:format("R2 ~p~n",[{R2,?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("nodes() ~p~n",[{nodes(),?MODULE,?FUNCTION_NAME,?LINE}]),
    
    io:format("active ~p~n",[{lib_host:active_nodes(),?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("stopped ~p~n",[{lib_host:stopped_nodes(),?MODULE,?FUNCTION_NAME,?LINE}]),
    ok.
   

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
start_provider(ProviderNode,App)->
    Result=case [App||{WApp,_,_}<-rpc:call(ProviderNode,application,loaded_applications,[],10*1000),
		      WApp==App] of
	       []->
		   {error,["Failed to load ",App,?MODULE,?FUNCTION_NAME,?LINE]};
	       [App]->	  
		   case rpc:call(ProviderNode,application,start,[App],30*1000)of
		       {badrpc,Reason}->
			   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		       {error,Reason}->
			   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		       ok->
			   ok
		   end
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


ssh_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    Dir="test_dir_tabort",
    rpc:call(?KubeNode,ops_ssh,delete_dir,[?HostSpec1,Dir],5000),
    false=rpc:call(?KubeNode,ops_ssh,is_dir,[?HostSpec1,Dir],5000),
    rpc:call(?KubeNode,ops_ssh,create_dir,[?HostSpec1,Dir],5000),
    timer:sleep(2000),
    true=rpc:call(?KubeNode,ops_ssh,is_dir,[?HostSpec1,Dir],5000),
    rpc:call(?KubeNode,ops_ssh,delete_dir,[?HostSpec1,Dir],5000),
    false=rpc:call(?KubeNode,ops_ssh,is_dir,[?HostSpec1,Dir],5000),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_host()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    pong=rpc:call(?KubeNode,etcd,ping,[]),
    pong=rpc:call(?KubeNode,kube,ping,[]),
    pong=rpc:call(?KubeNode,host_server,ping,[]),
 %   AllHostSpecs=lists:sort(rpc:call(?KubeNode,db_host_spec,get_all_id,[],5000)),
  %  create_host(AllHostSpecs),
    create_host(["c201"]),
    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_host([])->
    ok;
create_host([HostSpec|T]) ->
    R=rpc:call(?KubeNode,host_server,create_node,[HostSpec],30*1000),
    io:format("Dbg HostSpec,R ~p~n",[{HostSpec,R,?MODULE,?FUNCTION_NAME}]),
    create_host(T).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


loop1(ClusterSpec,AllNodes,_PreviousNotice)->
    [AvailableNode|_]=[N2||N1<-AllNodes,
			    N2<-AllNodes,
			   pong==rpc:call(N1,net_adm,ping,[N2],5000),
			   pong==rpc:call(N2,sd,ping,[],5000),
			   N1/=N2],
    io:format("AvailableNode ~p~n ",[AvailableNode]),
    Notice=sd:call(log,log,all_parsed,[debug],5000),
    io:format(" ****************************************************** ~n"),
    io:format("~p~n ",[{date(),time()}]),
    io:format("~p~n ",[Notice]),
    NewPreviousNotice=Notice,
 %   Nodes=[AvailableNode|rpc:call(AvailableNode,erlang,nodes,[],6000)],
 %   io:format("Nodes ~p~n ",[Nodes]),
    timer:sleep(5*1000),
    loop1(ClusterSpec,AllNodes,NewPreviousNotice).
    %%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
loop(ClusterSpec,AllNodes,PreviousNotice)->
    [net_adm:ping(N)||N<-AllNodes],
    [_AvailableNode|_]=[N2||N1<-AllNodes,
			    N2<-AllNodes,
			   pong==rpc:call(N1,net_adm,ping,[N2],5000),
			   pong==rpc:call(N2,sd,ping,[],5000),
			   N1/=N2],
 %   io:format("AvailableNode ~p~n ",[AvailableNode]),
    Notice=sd:call(log,log,all_parsed,[debug],5000),
    NewPreviousNotice=case Notice==PreviousNotice of
			  true->
			      PreviousNotice;
			  false->
			      print(Notice,PreviousNotice),
			      Notice
		      end,
  %  Nodes=[AvailableNode|rpc:call(AvailableNode,erlang,nodes,[],6000)],
  %  io:format("Nodes ~p~n ",[Nodes]),
    timer:sleep(1*1000),
    loop(ClusterSpec,AllNodes,NewPreviousNotice).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
print(Notice,PreviousNotice)->
%    io:format(" ****************************************************** ~n"),
 %   io:format("~p~n ",[{date(),time()}]),
    NewItems=[Item||Item<-Notice,
		    false==lists:member(Item,PreviousNotice)],
    io:format(" ~p~n",[NewItems]),
  %  io:format("~n "),
    ok.



%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    ok=application:start(kube),
    pong=kube:ping(),  
    pong=common:ping(), 
    pong=sd:ping(),
    pong=etcd:ping(),
    pong=host_server:ping(),
    ok.


setup_2()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    timer:sleep(5000),
    []=os:cmd("_build/default/rel/kube/bin/kube daemon"),
    pong=net_adm:ping(?KubeNode),
    pong=rpc:call(?KubeNode,common,ping,[]),
    pong=rpc:call(?KubeNode,sd,ping,[]),
    pong=rpc:call(?KubeNode,etcd,ping,[]),
    pong=rpc:call(?KubeNode,kube,ping,[]),
    pong=rpc:call(?KubeNode,host_server,ping,[]),
    
    ok.
