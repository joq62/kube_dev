%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(log).  

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
-define(SERVER,log_server).


%% External exports
-export([
	 is_config/0,
	 config/1,
	 raw/1,
	 read/1,
	 log/4,

	 debug/2,notice/2,warning/2,alert/2,
	 create_logfile/3,
	 create/1,

	 get_state/0,
	 ping/0
	]).

-export([
	 start/0,
	 stop/0
	]).



%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Application handling
%% ====================================================================

%% ====================================================================
%% Support functions
%% ====================================================================
%%---------------------------------------------------------------
%% Function:all_specs()
%% @doc: all service specs infromation       
%% @param: non 
%% @returns:State#state.service_specs_info
%%
%%---------------------------------------------------------------
create_logfile(MainLogDir,ProviderLogDir,LogFilePath)->
    gen_server:call(?SERVER, {create_logfile,MainLogDir,ProviderLogDir,LogFilePath},infinity).

is_config()->
    gen_server:call(?SERVER, {is_config},infinity).

config(LogFile)->
    gen_server:call(?SERVER, {config,LogFile},infinity).


raw(LogLevel)->
    gen_server:call(?SERVER, {raw,LogLevel},infinity).

read(LogLevel)->
    gen_server:call(?SERVER, {read,LogLevel},infinity).


create(LogFile)->
    gen_server:call(?SERVER, {create,LogFile},infinity).


log(Level,ModuleString,Line,Msg)-> 
    gen_server:cast(?SERVER, {log,Level,ModuleString,Line,Msg}).



debug(Msg,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
    gen_server:cast(?SERVER, {debug,Msg,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}}).
notice(Msg,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
    gen_server:cast(?SERVER, {notice,Msg,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}}).
warning(Msg,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
    gen_server:cast(?SERVER, {warning,Msg,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}}).
alert(Msg,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp})->
    gen_server:cast(?SERVER, {alert,Msg,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp}}).




start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).


get_state()->
    gen_server:call(?SERVER, {get_state},infinity).
%% 
%% @doc:check if service is running
%% @param: non
%% @returns:{pong,node,module}|{badrpc,Reason}
%%
-spec ping()-> {atom(),node(),module()}|{atom(),term()}.
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).

