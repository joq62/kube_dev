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
-define(TimeOut,20*1000).
%% --------------------------------------------------------------------

%% External exports
-export([
	 start_nodes/2,
	 create_node/2,


	 create_node/1,
	 active_nodes/1,
	 stopped_nodes/1

	]).


%% ====================================================================
%% External functions
%% ====================================================================

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
    {ok,?MODULE,?LINE}.

create_node(HostSpec,CookieStr,_Tabort)->
    {ok,Node}=db_host_spec:read(connect_node,HostSpec),
    rpc:call(Node,init,stop,[]),
    timer:sleep(2000),
    {ok,NodeName}=db_host_spec:read(connect_node_name,HostSpec),
    {ok,Ip}=db_host_spec:read(local_ip,HostSpec),
    {ok,SshPort}=db_host_spec:read(ssh_port,HostSpec),
    {ok,Uid}=db_host_spec:read(uid,HostSpec),
    {ok,Pwd}=db_host_spec:read(passwd,HostSpec),
    
    PaArgs=" ",
    EnvArgs=" -detached ",
    Result=case rpc:call(node(),ops_ssh,create,[HostSpec,NodeName,CookieStr,PaArgs,EnvArgs,
						{Ip,SshPort,Uid,Pwd},?TimeOut],?TimeOut+1000) of
	       {ok,HostNode}->
		   {ok,HostNode};
	       Reason->
		   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to create host vm  ",[HostSpec,NodeName,CookieStr,PaArgs,EnvArgs,?TimeOut,Reason]]),
		   {error,[Reason,HostSpec,?MODULE,?FUNCTION_NAME,?LINE,[NodeName,Ip,SshPort,Uid,Pwd,CookieStr]]}
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
active_nodes(ClusterSpec)->
    AllNodes= sd:call(etcd,db_parent_desired_state,pods,[ClusterSpec],5000),
    RunningNodes=[Node||Node<-AllNodes,
			pong==net_adm:ping(Node)],
    Result=case sd:call(etcd,db_cluster_spec,read,[root_dir,ClusterSpec],5000) of
	       {ok,RootDir}->	
		   ActiveNodes=[Node||Node<-RunningNodes,
				      rpc:call(Node,filelib,is_dir,[RootDir],5000)],
		   [rpc:call(Node,init,stop,[],3000)||Node<-RunningNodes,
						      false==lists:member(Node,ActiveNodes)],
		   {ok,ActiveNodes};
	       Reason->
		   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get root dir",[ClusterSpec,Reason]]),
		   {error,Reason}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stopped_nodes(ClusterSpec)->
    AllNodes= sd:call(etcd,db_parent_desired_state,pods,[ClusterSpec],5000),
    Result=case active_nodes(ClusterSpec) of
	       {ok,ActiveNodes}->		 
		   StoppedNodes=[Node||Node<-AllNodes,
				       false==lists:member(Node,ActiveNodes)],
		   {ok,StoppedNodes};
	       Reason->
		    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get active nodes ",[ClusterSpec,Reason]]),
		   {error,Reason}
	   end,
    Result.
    
    
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_node(ParentNode)->
 %   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: create_node ParentNode : ",ParentNode,?MODULE,?LINE]]),
    Result=case sd:call(etcd,db_parent_desired_state,read,[host_spec,ParentNode],5000) of
	       {ok,HostSpec}->
		   case sd:call(etcd,db_parent_desired_state,read,[node_name,ParentNode],5000) of
		       {ok,NodeName}->
			   case sd:call(etcd,db_parent_desired_state,read,[cluster_spec,ParentNode],5000) of
			       {ok,ClusterSpec}-> 
				   case sd:call(etcd,db_cluster_spec,read,[cookie,ClusterSpec],5000) of
				       {ok,Cookie}->
					   EnvArgs=" -detached ",
					   PaArgs=" ",
					   TimeOut=10*1000,
					   case rpc:call(node(),ops_ssh,create,[HostSpec,NodeName,Cookie,PaArgs,EnvArgs,TimeOut],TimeOut+1000) of
					       {ok,ParentNode}->
						   case rpc:call(ParentNode,file,del_dir_r,[ClusterSpec],10*1000) of
						       ok->
							   case rpc:call(ParentNode,file,make_dir,[ClusterSpec],10*1000) of
							       ok->
								   ok;
							       Reason->
								   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to make dir  ",[ClusterSpec,ParentNode,Reason]]),
								   {error,Reason}
							   end;
						       Reason->
							   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to delete dir  ",[ClusterSpec,ParentNode,Reason]]),
							   {error,Reason}
						   end;
					       Reason->
						   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to create vm  ",[ParentNode,HostSpec,NodeName,Cookie,PaArgs,EnvArgs,TimeOut,Reason]]),
						   {error,Reason}
					   end;   
				       Reason->
					   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get cookie   ",[ParentNode,Reason]]),
					   {error,Reason}
				   end;
			       Reason->
				   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get cluster spec   ",[ParentNode,Reason]]),
				   {error,Reason}
			   end;
		       Reason->
			   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get node name   ",[ParentNode,Reason]]),
		   {error,Reason}
		   end;
	       Reason->
		   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get host spec   ",[ParentNode,Reason]]),
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error: ,db_parent_desired_state,read,[host_spec,ParentNode: ",Reason,ParentNode,?MODULE,?LINE]]),
		   {error,Reason}
	   end,
    Result.    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

