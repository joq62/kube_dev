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
-define(Provider2,"main").

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
    []=lists:sort(nodes()),    
    ok=create_controllers(),
    ['host_controller@c200','host_controller@c201']=lists:sort(nodes()),
    ok=load(),
    ['host_controller@c200','host_controller@c201',
     'kube@c200','kube@c201'
    ]=lists:sort(nodes()),
    ok=start_providers(),
    KubeNodes=sd:get_node(kube),
    [pong,pong,pong]=[rpc:call(N,kube,ping,[],1000)||N<-KubeNodes],
    
  %  R1=lib_orchestrate:get_candidates([all_hosts],?Provider1,1),
  %  io:format("R1 ~p~n",[{R1,?MODULE,?FUNCTION_NAME}]),
  %  R11=lib_orchestrate:get_candidates([all_hosts],?Provider2,1),
  %  io:format("R11 ~p~n",[{R11,?MODULE,?FUNCTION_NAME}]),
  %  R2=lib_orchestrate:get_candidates([any_host],?Provider2,2),
  %  io:format("R2 ~p~n",[{R2,?MODULE,?FUNCTION_NAME}]),
  %  R3=lib_orchestrate:get_candidates(["c200"],?Provider2,1),
  %  io:format("R3 ~p~n",[{R3,?MODULE,?FUNCTION_NAME}]),
  %  R4=lib_orchestrate:get_candidates(["c200","c201","c300"],?Provider1,3),
  %  io:format("R4 ~p~n",[{R4,?MODULE,?FUNCTION_NAME}]),
    

    io:format("KubeNodes ~p~n",[{KubeNodes,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=stop_provider(),
    [
     pong,
     {badrpc,{'EXIT',{noproc,{gen_server,call,[kube_server,{ping},infinity]}}}},
     {badrpc,{'EXIT',{noproc,{gen_server,call,[kube_server,{ping},infinity]}}}}
    ]=[rpc:call(N,kube,ping,[],1000)||N<-KubeNodes],
    [
     'host_controller@c200','host_controller@c201',
     'kube@c200','kube@c201'
    ]=lists:sort(nodes()),  
  
   
    ok=unload(),
    ['host_controller@c200','host_controller@c201']=lists:sort(nodes()),  
    ok=stop_controllers(),
    []=lists:sort(nodes()),

    %% 
    ok=update_controllers(),
    ['host_controller@c200','host_controller@c201']=lists:sort(nodes()),
    %%
    ok=update_provider(),
    ['host_controller@c200','host_controller@c201',
     'kube@c200','kube@c201'
    ]=lists:sort(nodes()),
    [pong,pong,pong]=[rpc:call(N,kube,ping,[],1000)||N<-KubeNodes],
    
    io:format("End testing  SUCCESS!! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
%    init:stop(),
%    timer:sleep(3000),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
update_provider()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    lib_orchestrate:provider_update(),
 %   io:format("update_provider ~p~n",[{R1,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
update_controllers()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    lib_orchestrate:host_controller_update(),
%    io:format("update_controllers ~p~n",[{R1,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stop_controllers()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    [{HostSpec,kube:stop_host_controller(HostSpec)}||HostSpec<-db_host_spec:get_all_id()],
    %io:format("stop_controllers ~p~n",[{R1,?MODULE,?FUNCTION_NAME,?LINE}]),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
unload()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    AllHostSpecs=lists:sort(db_host_spec:get_all_id()),
    UnLoadR=[unload(?Provider1,HostSpec)||HostSpec<-AllHostSpecs],
    
  [
   {error,["host controller not started ","c100"]},
   {ok,"kube","c200"},
   {ok,"kube","c201"},
   {error,["host controller not started ","c202"]},
   {error,["host controller not started ","c300"]},
   {error,["host controller not started ","c50"]}
  ]=UnLoadR,
     
   %  io:format("LoadR ~p~n",[{LoadR,?MODULE,?FUNCTION_NAME}]),

    ok.
    
unload(ProviderSpec,HostSpec)->
    Result=case kube:unload_provider(ProviderSpec,HostSpec) of
	       {error,Reason}->
		 %  io:format("Error ~p~n",[{HostSpec,Reason,?MODULE,?FUNCTION_NAME,?LINE}]),
		   {error,Reason};
	       ok->
		 %  io:format("Ok ~p~n",[{ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
		   {ok,ProviderSpec,HostSpec}
	   end,
    Result.
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stop_provider()->
      io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    AllHostSpecs=lists:sort(db_host_spec:get_all_id()),
    StopR=[stop_provider(?Provider1,HostSpec)||HostSpec<-AllHostSpecs],
    
    [
     {error,["host controller not started ","c100"]},
     {ok,"kube","c200"},
     {ok,"kube","c201"},
     {error,["host controller not started ","c202"]},
     {error,["host controller not started ","c300"]},
     {error,["host controller not started ","c50"]}
    ]=StopR,

    ok.
stop_provider(ProviderSpec,HostSpec)->
    Result=case kube:stop_provider(ProviderSpec,HostSpec) of
	       {error,Reason}->
		 %  io:format("Error ~p~n",[{HostSpec,Reason,?MODULE,?FUNCTION_NAME,?LINE}]),
		   {error,Reason};
	       ok->
		 %  io:format("Ok ~p~n",[{ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
		   {ok,ProviderSpec,HostSpec}
	   end,
    Result.



    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_providers()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    AllHostSpecs=lists:sort(db_host_spec:get_all_id()),
    StartR=[start_provider(?Provider1,HostSpec)||HostSpec<-AllHostSpecs],
    
   [
    {error,["host controller not started ","c100"]},
    {ok,"kube","c200"},
    {ok,"kube","c201"},
    {error,["host controller not started ","c202"]},
    {error,["host controller not started ","c300"]},
    {error,["host controller not started ","c50"]}
   ]=StartR,

    ok.
start_provider(ProviderSpec,HostSpec)->
    Result=case kube:start_provider(ProviderSpec,HostSpec) of
	       {error,Reason}->
		 %  io:format("Error ~p~n",[{HostSpec,Reason,?MODULE,?FUNCTION_NAME,?LINE}]),
		   {error,Reason};
	       ok->
		 %  io:format("Ok ~p~n",[{ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
		   {ok,ProviderSpec,HostSpec}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    AllHostSpecs=lists:sort(db_host_spec:get_all_id()),
    LoadR=[load(?Provider1,HostSpec)||HostSpec<-AllHostSpecs],

    [
     {error,["host controller not started ","c100"]},
     {ok,'kube@c200',kube},
     {ok,'kube@c201',kube},
     {error,["host controller not started ","c202"]},
     {error,["host controller not started ","c300"]},
     {error,["host controller not started ","c50"]}
    ]=LoadR,

        
   %  io:format("LoadR ~p~n",[{LoadR,?MODULE,?FUNCTION_NAME}]),

    ok.
    
    

load(ProviderSpec,HostSpec)->
    Result=case kube:load_provider(ProviderSpec,HostSpec) of
	       {error,Reason}->
	%	   io:format("Error ~p~n",[{HostSpec,Reason,?MODULE,?FUNCTION_NAME,?LINE}]),
		   {error,Reason};
	       {ok,ProviderNode,App}->
	%	   io:format("Ok ~p~n",[{ProviderNode,App,?MODULE,?FUNCTION_NAME,?LINE}]),
		  {ok,ProviderNode,App}
	   end,
    Result.
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_controllers()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    AllHostSpecs=lists:sort(db_host_spec:get_all_id()),
    _StartR=[create_controller(HostSpec)||HostSpec<-AllHostSpecs],
    Started=[HostSpec||HostSpec<-AllHostSpecs,
		  kube:is_started_host_controller(HostSpec)],
    ["c200","c201"]=Started,
   % io:format("nodes() ~p~n",[{nodes(),?MODULE,?FUNCTION_NAME,?LINE}]),
     
    ok.
   
create_controller(HostSpec)->
    Result=case kube:start_host_controller(HostSpec) of
	       {error,Reason}->
	%	   io:format("Error ~p~n",[{HostSpec,Reason,?MODULE,?FUNCTION_NAME,?LINE}]),
		   {error,Reason};
	       {ok,Node,HostSpec}->
	%	   io:format("Ok ~p~n",[{HostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
		   {ok,Node,HostSpec}
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
    rpc:call(?KubeNode,host_server,create_node,[HostSpec],30*1000),
 %   io:format("Dbg HostSpec,R ~p~n",[{HostSpec,R,?MODULE,?FUNCTION_NAME}]),
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

    []=[{error,[HostSpec]}||HostSpec<-lists:sort(db_host_spec:get_all_id()),
			       ok/=kube:stop_host_controller(HostSpec)],
    ok.
