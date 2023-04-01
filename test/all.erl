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


-define(KubeNode,'dev_kube@c50').
-define(Host1,"c201").
-define(Appl1,"kube").
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
 %   ok=ssh_test(),
    ok=load_start(?Appl1,?Host1),

    io:format("End testing  SUCCESS!! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    init:stop(),
    timer:sleep(3000),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_start(Appl1,Host1)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    kuk=ops_ssh:call(Host1,"time",5000),
    
    {ok,Dir}=rpc:call(?KubeNode,db_provider_spec,read,[dir,Appl1],5000),
    {ok,{CloneM,CloneF,CloneArgs}}=rpc:call(?KubeNode,db_provider_spec,read,[clone_cmd,Appl1],5000),
    {ok,{TarM,TarF,TarArgs}}=rpc:call(?KubeNode,db_provider_spec,read,[tar_cmd,Appl1],5000),
    {ok,{StartM,StartF,StartArgs}}=rpc:call(?KubeNode,db_provider_spec,read,[start_cmd,Appl1],5000),
   
    rpc:call(?KubeNode,ops_ssh,delete_dir,[Host1,Dir],5000),
    false=rpc:call(?KubeNode,ops_ssh,is_dir,[Host1,Dir],5000),
    timer:sleep(2000),
    
    CloneR=rpc:call(?KubeNode,CloneM,CloneF,CloneArgs,5000),
    io:format("CloneR ~p~n",[{CloneR,?MODULE,?FUNCTION_NAME}]),
    io:format(" ~p~n",[{CloneM,CloneF,CloneArgs,?MODULE,?FUNCTION_NAME}]),
    timer:sleep(2000),
    true=rpc:call(?KubeNode,ops_ssh,is_dir,[Host1,Dir],5000),

   
    
    

    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


ssh_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    Dir="test_dir_tabort",
    rpc:call(?KubeNode,ops_ssh,delete_dir,[?Host1,Dir],5000),
    false=rpc:call(?KubeNode,ops_ssh,is_dir,[?Host1,Dir],5000),
    rpc:call(?KubeNode,ops_ssh,create_dir,[?Host1,Dir],5000),
    timer:sleep(2000),
    true=rpc:call(?KubeNode,ops_ssh,is_dir,[?Host1,Dir],5000),
    rpc:call(?KubeNode,ops_ssh,delete_dir,[?Host1,Dir],5000),
    false=rpc:call(?KubeNode,ops_ssh,is_dir,[?Host1,Dir],5000),
    
    
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
    rpc:call(?KubeNode,init,stop,[],5000),
    timer:sleep(5000),
    []=os:cmd("_build/default/rel/kube/bin/kube daemon"),
    pong=net_adm:ping(?KubeNode),
    pong=rpc:call(?KubeNode,common,ping,[]),
    pong=rpc:call(?KubeNode,sd,ping,[]),
    pong=rpc:call(?KubeNode,etcd,ping,[]),
    pong=rpc:call(?KubeNode,kube,ping,[]),
    pong=rpc:call(?KubeNode,host_server,ping,[]),
    
    ok.
