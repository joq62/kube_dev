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
-define(Host1,"c200").
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
    ok=create_vms(),
    {ok,ProviderNode,App}=load(?Appl1,?Host1),
    io:format("Loaded apps on ProviderNode ~p~n",[{rpc:call(ProviderNode,application,loaded_applications,[],5000),?MODULE,?FUNCTION_NAME}]),
    ok=start_provider(ProviderNode,App),
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
load(Provider,HostSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ProviderExists=db_provider_spec:member(Provider),
    HostSpecExists=db_host_spec:member(HostSpec),
    Result=case {ProviderExists,HostSpecExists} of
	       {false,_}->
		   {error,["eexists ",{ProviderExists,Provider},{HostSpecExists,HostSpec}]};
	       {_,false}->
		   {error,["eexists ",{ProviderExists,Provider},{HostSpecExists,HostSpec}]};
	       {true,true}->
		   {ok,HostNode}=db_host_spec:read(connect_node,HostSpec),
		   %% 
		   {ok,ProviderDir}=db_provider_spec:read(dir,Provider),
		   {ok,TarFile}=db_provider_spec:read(tar_file,Provider),
		   case rpc:call(HostNode,file,del_dir_r,[ProviderDir],5000) of
		       {badrpc,Reason}->
			   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		       {error,Reason}->
			   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		       _ ->
			   {ok,GitPath}=db_provider_spec:read(git_path,Provider),
			   case rpc:call(HostNode,os,cmd,["git clone "++GitPath],20*1000) of
			       {badrpc,Reason}->
				   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       {error,Reason}->
				   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       _->
				   {ok,TarCmd}=db_provider_spec:read(tar_cmd,Provider),
				   case rpc:call(HostNode,os,cmd,[TarCmd],20*1000) of
				       {badrpc,Reason}->
					   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
				       {error,Reason}->
					   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
				       _->
					   %% start slave
					   {ok,ProviderNodeName}=db_provider_spec:read(node_name,Provider),
					   {ok,CookieStr}=db_provider_spec:read(cookie,Provider),
					   {ok,PaArgs}=db_provider_spec:read(pa_args,Provider),
					   {ok,HostName}=db_host_spec:read(hostname,HostSpec),
					   Args=PaArgs++" "++"-setcookie "++CookieStr,
					   io:format(" ~p~n",[{HostName,ProviderNodeName,Args,?MODULE,?LINE}]),
					   ProviderNode=list_to_atom(ProviderNodeName++"@"++HostName),
					   rpc:call(ProviderNode,init,stop,[],5000),
					   timer:sleep(3000),
					   EnvArgs=" ",
					   case lib_host:create_node(HostSpec,ProviderNodeName,CookieStr,PaArgs,EnvArgs) of
					       {error,Reason}->
						   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
					       {ok,ProviderNode,HostSpec}->
						   TarFilewPath=filename:join(ProviderDir,TarFile),
						   TestDir=filename:join(ProviderDir,"test"),
						   Releases=filename:join(ProviderDir,"releases"),
						   R1=rpc:call(HostNode,file,delete,[TarFilewPath],5000),
						   R2=rpc:call(HostNode,file,del_dir_r,[TestDir],5000),
						   R3=rpc:call(HostNode,file,del_dir_r,[Releases],5000),
						   case {R1,R2,R3} of
						       {ok,ok,ok}->
							   {ok,App}=db_provider_spec:read(app,Provider),
							   case rpc:call(ProviderNode,application,loaded_applications,[],10*1000)of
							       {badrpc,Reason}->
								   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
							       {error,Reason}->
								   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
							       LoadedApplications->
								   case lists:keymember(App,1,LoadedApplications) of
								       true->
									   {error,["Already loaded ",App,?MODULE,?FUNCTION_NAME,?LINE]};
								       false->
									   case rpc:call(ProviderNode,application,load,[App],10*1000)of
									       {badrpc,Reason}->
										   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
									       {error,Reason}->
										   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
									       ok->
										   {ok,ProviderNode,App}
									   end
								   end
							   end;
						       _ ->
							   {error,["Failed to load, ",R1,R2,R3,?MODULE,?LINE]}
						   end
					   end
				   end
			   end
		   end
	   end,
    io:format("Load result  ~p~n",[{Result,?MODULE,?FUNCTION_NAME}]),
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_provider(ProviderNode,App)->
     Result=case rpc:call(ProviderNode,application,loaded_applications,[],10*1000)of
		{badrpc,Reason}->
		    {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		{error,Reason}->
		    {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		LoadedApplications->
		    case lists:keymember(App,1,LoadedApplications) of
			false->
			    {error,["Not loaded ",App,?MODULE,?FUNCTION_NAME,?LINE]};
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
   
    ok=application:start(kube),
    pong=kube:ping(),  
    pong=common:ping(), 
    pong=sd:ping(),
    pong=etcd:ping(),
    pong=host_server:ping(),
    ok.


setup_2()->
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
