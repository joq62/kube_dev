%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_host).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(TimeOut,30*1000).

-include_lib("kernel/include/inet.hrl").
%% --------------------------------------------------------------------

%% External exports

-export([
	 start_host_controller/1,
	 stop_host_controller/1,
	 is_started_host_controller/1
	]).

-export([
	 ssh_start_nodes/2,
	 ssh_create_node/2,
	 ssh_create_node/5
	]).


-export([
	 start_nodes/2,
	 create_node/2,
	 create_node/5,
	 active_nodes/0,
	 active/1,
	 stopped_nodes/0,
	 stopped/1
	]).


%% ====================================================================
%% External functions
%% ====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_host_controller(HostSpec)->
    Result=case sd:call(dbetcd,db_host_spec,member,[HostSpec],5000) of
	       false->
		   {error,["eexists ",HostSpec]};
	       true->
		   {ok,HostControllerNode}=sd:call(dbetcd,db_host_spec,read,[connect_node,HostSpec],5000),
		   rpc:call(HostControllerNode,init,stop,[],5000),
		   case vm:check_stopped_node(HostControllerNode) of
		       false->
			   {error,["Failed to stop host controller node ",HostControllerNode,?MODULE,?FUNCTION_NAME,?LINE]};
		       true->
			   PaArgs=" ",
			   EnvArgs="  ",
			   CookieStr=atom_to_list(erlang:get_cookie()),
			   {ok,NodeName}=sd:call(dbetcd,db_host_spec,read,[connect_node_name,HostSpec],5000),
			   ssh_create_node(HostSpec,NodeName,CookieStr,PaArgs,EnvArgs)
		   end
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stop_host_controller(HostSpec)->
    Result=case sd:call(dbetcd,db_host_spec,member,[HostSpec],5000) of
	       false->
		   {error,["eexists ",HostSpec]};
	       true->
		   {ok,HostControllerNode}=sd:call(dbetcd,db_host_spec,read,[connect_node,HostSpec],5000),
		   rpc:call(HostControllerNode,init,stop,[],5000),
		   case vm:check_stopped_node(HostControllerNode) of
		       false->
			   {error,["Failed to stop host controller node ",HostControllerNode,?MODULE,?FUNCTION_NAME,?LINE]};
		       true->
			   ok
		   end
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_started_host_controller(HostSpec)->
    Result=case sd:call(dbetcd,db_host_spec,member,[HostSpec],5000) of
	       false->
		   {error,["eexists ",HostSpec]};
	       true->
		   {ok,HostControllerNode}=sd:call(dbetcd,db_host_spec,read,[connect_node,HostSpec],5000),
		   case net_adm:ping(HostControllerNode) of
		       pang->
			   false;
		       pong->
			   true
		   end
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ssh_start_nodes(HostSpecs,CookieStr)->
    ssh_start_nodes(HostSpecs,CookieStr,[]).

ssh_start_nodes([],_,Acc)->
    Acc;
ssh_start_nodes([HostSpec|T],CookieStr,Acc) ->
    Result=ssh_create_node(HostSpec,CookieStr),
    ssh_start_nodes(T,CookieStr,[Result|Acc]).

ssh_create_node(HostSpec,CookieStr)->
    PaArgs=" ",
    EnvArgs="  ",
    {ok,Node}=sd:call(dbetcd,db_host_spec,read,[connect_node,HostSpec],5000),
    rpc:call(Node,init,stop,[]),
    timer:sleep(3000),
    {ok,NodeName}=sd:call(dbetcd,db_host_spec,read,[connect_node_name,HostSpec],5000),
    ssh_create_node(HostSpec,NodeName,CookieStr,PaArgs,EnvArgs).

ssh_create_node(HostSpec,NodeName,CookieStr,PaArgs,EnvArgs)->
    {ok,Ip}=sd:call(dbetcd,db_host_spec,read,[local_ip,HostSpec],5000),
    {ok,SshPort}=sd:call(dbetcd,db_host_spec,read,[ssh_port,HostSpec],5000),
    {ok,Uid}=sd:call(dbetcd,db_host_spec,read,[uid,HostSpec],5000),
    {ok,Pwd}=sd:call(dbetcd,db_host_spec,read,[passwd,HostSpec],5000),
    ErlCmd="erl "++PaArgs++" "++"-sname "++NodeName++" "++"-setcookie"++" "++CookieStr++" "++EnvArgs++" "++" -detached",
    {ok,HostName}=sd:call(dbetcd,db_host_spec,read,[hostname,HostSpec],5000),
    Node=list_to_atom(NodeName++"@"++HostName),
    CreateResult={my_ssh:ssh_send(Ip,SshPort,Uid,Pwd,ErlCmd,?TimeOut),Node,HostSpec},
    %% connect
    Result=case CreateResult of
	       {ok,Node,_}->
		   case net_adm:ping(Node) of
		       pang->
			   {error,["Failed to connect ",Node,HostSpec]};
		       pong->
			   {ok,Node,HostSpec}
		   end;
	       Reason->
		   {error,["Failed to create vm ",Reason,Node,HostSpec]}
	   end,   
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_nodes(HostSpecs,CookieStr)->
    start_nodes(HostSpecs,CookieStr,[]).

start_nodes([],_,Acc)->
    Acc;
start_nodes([HostSpec|T],CookieStr,Acc) ->
    Result=create_node(HostSpec,CookieStr),
    start_nodes(T,CookieStr,[Result|Acc]).

create_node(HostSpec,CookieStr)->
     PaArgs=" ",
    EnvArgs="  ",
    {ok,Node}=sd:call(dbetcd,db_host_spec,read,[connect_node,HostSpec],5000),
    rpc:call(Node,init,stop,[]),
    timer:sleep(3000),
    {ok,NodeName}=sd:call(dbetcd,db_host_spec,read,[connect_node_name,HostSpec],5000),
    create_node(HostSpec,NodeName,CookieStr,PaArgs,EnvArgs).

create_node(HostSpec,NodeName,CookieStr,PaArgs,EnvArgs)->
    {ok,Ip}=sd:call(dbetcd,db_host_spec,read,[local_ip,HostSpec],5000),
    {ok,SshPort}=sd:call(dbetcd,db_host_spec,read,[ssh_port,HostSpec],5000),
    {ok,Uid}=sd:call(dbetcd,db_host_spec,read,[uid,HostSpec],5000),
    {ok,Pwd}=sd:call(dbetcd,db_host_spec,read,[passwd,HostSpec],5000),
    ErlCmd="erl "++PaArgs++" "++"-sname "++NodeName++" "++"-setcookie"++" "++CookieStr++" "++EnvArgs++" "++" -detached",
    {ok,HostName}=sd:call(dbetcd,db_host_spec,read,[hostname,HostSpec],5000),
    Node=list_to_atom(NodeName++"@"++HostName),
    CreateResult={my_ssh:ssh_send(Ip,SshPort,Uid,Pwd,ErlCmd,?TimeOut),Node,HostSpec},
    %% connect
    Result=case CreateResult of
	       {ok,Node,_}->
		   case net_adm:ping(Node) of
		       pang->
			   {error,["Failed to connect ",Node,HostSpec]};
		       pong->
			   {ok,Node,HostSpec}
		   end;
	       Reason->
		   {error,["Failed to create vm ",Reason,Node,HostSpec]}
	   end,   
    
  %  io:format("Result ~p~n",[{Result,?MODULE,?FUNCTION_NAME,?LINE}]),
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
active_nodes()->
    [HostSpec||HostSpec<-sd:call(dbetcd,db_host_spec,get_all_id,[],5000),
	       true==active(HostSpec)].

active(HostSpec)->
    Result=case sd:call(dbetcd,db_host_spec,read,[connect_node,HostSpec],5000) of
	       {error,Reason}->
		   {error,["Failed to read connect_node ",HostSpec,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	       {ok,Node}->
		   case net_adm:ping(Node) of
		       pang->
			   false;
		       pong->
			   true
		   end
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stopped_nodes()->
    [HostSpec||HostSpec<-sd:call(dbetcd,db_host_spec,get_all_id,[],5000),
	       true==stopped(HostSpec)].

stopped(HostSpec)->
    Result=case sd:call(dbetcd,db_host_spec,read,[connect_node,HostSpec],5000) of
	       {error,Reason}->
		   {error,["Failed to read connect_node ",HostSpec,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	       {ok,Node}->
		   case net_adm:ping(Node) of
		       pang->
			   true;
		       pong->
			   false
		   end
	   end,
    Result.
