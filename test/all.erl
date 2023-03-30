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
    ok=create_host(),
    


    io:format("End testing  SUCCESS!! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
  %  init:stop(),
  %  timer:sleep(2000),
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
    X=rpc:call(?KubeNode,host_server,create_node,["c200"],10*1000),
     io:format("Dbg X ~p~n",[{X,?MODULE,?FUNCTION_NAME}]),
    

    ok.

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

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
-define(LocalTypes,[oam,nodelog,db_etcd]).
-define(TargetTypes,[oam,nodelog,db_etcd]).

setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    []=os:cmd("_build/default/rel/kube/bin/kube daemon"),
    pong=net_adm:ping(?KubeNode),
    pong=rpc:call(?KubeNode,etcd,ping,[]),
    pong=rpc:call(?KubeNode,kube,ping,[]),
    pong=rpc:call(?KubeNode,host_server,ping,[]),
    
    ok.
