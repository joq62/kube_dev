%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_provider).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([
	 load/2,
	 start/2,
	 unload/2,
	 stop/2,
	 is_started/2,
	 is_stopped/2
	 
	]).

%% ====================================================================
%% External functions
%% ====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load(ProviderSpec,HostSpec)->
    ProviderExists=db_provider_spec:member(ProviderSpec),
    HostSpecExists=db_host_spec:member(HostSpec),
    Result=case {ProviderExists,HostSpecExists} of
	       {false,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {_,false}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {true,true}->
		   {ok,HostNode}=db_host_spec:read(connect_node,HostSpec),
		   %% 
		   {ok,ProviderDir}=db_provider_spec:read(dir,ProviderSpec),
		   case rpc:call(HostNode,file,del_dir_r,[ProviderDir],5000) of
		       {badrpc,Reason}->
			   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		       _ ->
			   {ok,GitPath}=db_provider_spec:read(git_path,ProviderSpec),
			   case rpc:call(HostNode,os,cmd,["git clone "++GitPath],60*1000) of
			       {badrpc,Reason}->
				   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       {error,Reason}->
				   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       _->
				   {ok,TarCmd}=db_provider_spec:read(tar_cmd,ProviderSpec),
				   case rpc:call(HostNode,os,cmd,[TarCmd],20*1000) of
				       {badrpc,Reason}->
					   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
				       {error,Reason}->
					   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
				       _-> 
					   %% start slave
					   {ok,ProviderNodeName}=db_provider_spec:read(node_name,ProviderSpec),
					   {ok,CookieStr}=db_provider_spec:read(cookie,ProviderSpec),
					   {ok,PaArgs}=db_provider_spec:read(pa_args,ProviderSpec),
					   {ok,HostName}=db_host_spec:read(hostname,HostSpec),
					   EnvArgs=" ",
					   Args=PaArgs++" "++"-setcookie "++CookieStr++" "++EnvArgs,
					   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
					   rpc:call(ProviderNode,init,stop,[],5000),
					   case vm:check_stopped_node(ProviderNode) of
					       false->
						   {error,["Failed to stop host controller node ",ProviderNode,?MODULE,?FUNCTION_NAME,?LINE]};
					       true->
						   case rpc:call(HostNode,slave,start,[HostName,ProviderNodeName,Args],10*1000) of
						       {error,Reason}->
							   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
						       {ok,ProviderNode}->
							   {ok,App}=db_provider_spec:read(app,ProviderSpec),
							   case rpc:call(ProviderNode,code,is_loaded,[App],10*1000)of
							       {badrpc,Reason}->
								   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
							       {file,_}->
								   {error,["Already loaded",App,?MODULE,?FUNCTION_NAME,?LINE]};
							       false->
								   case rpc:call(ProviderNode,application,load,[App],10*1000)of
								       {badrpc,Reason}->
									   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
								       {error,Reason}->
									   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
								       ok->
									   case is_loaded(ProviderSpec,HostSpec) of
									       false->
										  {error,["Not loaded  ",App,?MODULE,?FUNCTION_NAME,?LINE]};
									       true->	  
										   {ok,ProviderNode,App}
									   end
								   end
							   end
						   end
					   end
				   end
			   end
		   end
	   end,
   % io:format("Load result  ~p~n",[{Result,?MODULE,?FUNCTION_NAME}]),
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stop(ProviderSpec,HostSpec)->
    ProviderExists=db_provider_spec:member(ProviderSpec),
    HostSpecExists=db_host_spec:member(HostSpec),
    Result=case {ProviderExists,HostSpecExists} of
	       {false,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {_,false}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {true,true}->
		   case is_stopped(ProviderSpec,HostSpec) of
		       true->
			   {error,["Already stopped provider ",ProviderSpec,HostSpec]};
		       false->
			   {ok,ProviderNodeName}=db_provider_spec:read(node_name,ProviderSpec),
			   {ok,HostName}=db_host_spec:read(hostname,HostSpec),
			   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
			   {ok,App}=db_provider_spec:read(app,ProviderSpec),
			   case rpc:call(ProviderNode,application,stop,[App],10*1000)of
			       {badrpc,Reason}->
				   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       {error,Reason}->
				   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       ok->
				   ok
			   end
		   end
	   end,		       
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
unload(ProviderSpec,HostSpec)->
    ProviderExists=db_provider_spec:member(ProviderSpec),
    HostSpecExists=db_host_spec:member(HostSpec),
    Result=case {ProviderExists,HostSpecExists} of
	       {false,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {_,false}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {true,true}->
		   {ok,ProviderNodeName}=db_provider_spec:read(node_name,ProviderSpec),
		   {ok,HostName}=db_host_spec:read(hostname,HostSpec),
		   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
		   {ok,App}=db_provider_spec:read(app,ProviderSpec),
		   case rpc:call(ProviderNode,application,unload,[App],10*1000) of
		       {badrpc,Reason}->
			   {error,["Failed to unload app",badrpc,App,ProviderSpec,HostSpec,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		       {error,Reason}->
			   {error,["Failed to unload app ",ProviderSpec,HostSpec,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		       ok->
			   {ok,HostNode}=db_host_spec:read(connect_node,HostSpec),
			   {ok,ProviderDir}=db_provider_spec:read(dir,ProviderSpec),
			   case rpc:call(HostNode,file,del_dir_r,[ProviderDir],10*1000)of
			       {badrpc,Reason}->
				   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       {error,Reason}->
				   {error,["Failed to delete ProviderDir",ProviderDir, Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       ok->
				   case rpc:call(HostNode,slave,stop,[ProviderNode],10*1000) of
				       {badrpc,Reason}->
					   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
				       {error,Reason}->
					   {error,["Failed to stop ProviderDir",ProviderDir, Reason,?MODULE,?FUNCTION_NAME,?LINE]};
				       ok->
					   ok
				   end
			   end
		   end
	   end,		       
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start(ProviderSpec,HostSpec)->
    ProviderExists=db_provider_spec:member(ProviderSpec),
    HostSpecExists=db_host_spec:member(HostSpec),
    Result=case {ProviderExists,HostSpecExists} of
	       {false,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {_,false}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {true,true}->
		   {ok,App}=db_provider_spec:read(app,ProviderSpec),
		   {ok,ProviderNodeName}=db_provider_spec:read(node_name,ProviderSpec),
		   {ok,HostName}=db_host_spec:read(hostname,HostSpec),
		   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
		   case is_loaded(ProviderSpec,HostSpec) of
		       false->
			   {error,["Not loaded  ",App,?MODULE,?FUNCTION_NAME,?LINE]};
		       true->	  
			   case rpc:call(ProviderNode,application,start,[App],30*1000)of
			       {badrpc,Reason}->
				   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       {error,Reason}->
				   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       ok->
				   ok
			   end
		   end
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_loaded(ProviderSpec,HostSpec)->
    ProviderExists=db_provider_spec:member(ProviderSpec),
    HostSpecExists=db_host_spec:member(HostSpec),
    Result=case {ProviderExists,HostSpecExists} of
	       {false,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {_,false}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {true,true}->
		   {ok,App}=db_provider_spec:read(app,ProviderSpec),
		   {ok,ProviderNodeName}=db_provider_spec:read(node_name,ProviderSpec),
		   {ok,HostName}=db_host_spec:read(hostname,HostSpec),
		   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
		   case rpc:call(ProviderNode,application,loaded_applications,[],10*1000) of
		       {badrpc,_Reason}->
			   false;
		       LoadedList->
			   lists:keymember(App,1,LoadedList)
		   end
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_started(ProviderSpec,HostSpec)->
    ProviderExists=db_provider_spec:member(ProviderSpec),
    HostSpecExists=db_host_spec:member(HostSpec),
    Result=case {ProviderExists,HostSpecExists} of
	       {false,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {_,false}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {true,true}->
		   {ok,ProviderNodeName}=db_provider_spec:read(node_name,ProviderSpec),
		   {ok,HostName}=db_host_spec:read(hostname,HostSpec),
		   {ok,HostNode}=db_host_spec:read(connect_node,HostSpec),  
		   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
		   case rpc:call(HostNode,net_adm,ping,[ProviderNode],5000) of
		       {badrpc,_Reason}->
			   %% log
			   false;
		       pong->
			   true;
		       pang->
			   false
		   end
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_stopped(ProviderSpec,HostSpec)->
    ProviderExists=db_provider_spec:member(ProviderSpec),
    HostSpecExists=db_host_spec:member(HostSpec),
    Result=case {ProviderExists,HostSpecExists} of
	       {false,_}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {_,false}->
		   {error,["eexists ",{ProviderExists,ProviderSpec},{HostSpecExists,HostSpec}]};
	       {true,true}->
		   {ok,ProviderNodeName}=db_provider_spec:read(node_name,ProviderSpec),
		   {ok,HostName}=db_host_spec:read(hostname,HostSpec),
		   {ok,HostNode}=db_host_spec:read(connect_node,HostSpec),  
		   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
		   case rpc:call(HostNode,net_adm,ping,[ProviderNode],5000) of
		       {badrpc,_Reason}->
			   %% log
			   true;
		       pong->
			   false;
		       pang->
			   true
		   end
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
