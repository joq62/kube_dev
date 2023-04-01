-module(boot).
-export([s/0]).
-define(App,main).
-define(Node,'main@c50').
-define(Dir,"main").
-define(AppName,"main").
-define(TarFile,"main-0.1.0.tar.gz").
-define(GitPath,"https://github.com/joq62/main.git").

s()->
    
    rpc:call(?Node,init,stop,[],5000),
    
    os:cmd("rm -rf "++?Dir),
    
    os:cmd("git clone "++?GitPath),
    
    os:cmd("tar -xvf "++?Dir++"/"++?TarFile++" -C "++?Dir),
    
    os:cmd("rm "++?Dir++"/"++?TarFile),
    
    os:cmd("./"++?Dir++"/bin/"++?AppName++" "++"daemon"),

    pong=rpc:call(?Node,common, ping,[],5000),
    pong=rpc:call(?Node,sd, ping,[],5000),
    pong=rpc:call(?Node,main, ping,[],5000),

    Appl=rpc:call(?Node,sd, all,[],5000),
    io:format("Dbg All ~p~n",[Appl]),
    
    ok.
   
    
