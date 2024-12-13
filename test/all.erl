%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%%
%%% -------------------------------------------------------------------
-module(all).       
 
-include("log.api").
-export([start/0]).


%%
-define(CheckDelay,20).
-define(NumCheck,1000).


%% Change
-define(Appl,"control").
-define(Dir,"control").
-define(ApplAtom,list_to_atom(?Appl)).

-define(NodeName,?Appl).
-define(ApplDir,?Dir++"_container").
-define(TarFile,?Appl++".tar.gz").
-define(TarDir,"tar_dir").
-define(ExecDir,"exec_dir").
-define(GitUrl,"https://github.com/joq62/"++?Appl++"_x86.git ").

%-define(Foreground,"./_build/default/rel/"++?Dir++"/bin/"++?Appl++" "++"foreground").
%-define(Daemon,"./_build/default/rel/"++?Dir++"/bin/"++?Appl++" "++"daemon").

-define(Foreground,"./"++?ApplDir++"bin/"++?Appl++" "++"foreground").
-define(Daemon,"./"++?ApplDir++"/bin/"++?Appl++" "++"daemon").


-define(LogFilePath,?Appl++"_container/logs/"++?Appl++"/log.logs/file.1").


%%
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    {ok,ControlId}=setup(),
    ok=test_0(ControlId),
%    ok=test_1(ControlId),
 %   ok=test_11(ControlId),
 %   ok=test_2(ControlId),
    



%    ApplicationToTest=list_to_atom("test_"++?Appl),
 %   ok=rpc:call(get_node(?NodeName),ApplicationToTest,start,[],10*5000),

  %  file:del_dir_r(?ApplDir),   
  %  rpc:call(get_node(?NodeName),init,stop,[],5000),
  %  true=check_node_stopped(get_node(?NodeName)),
    io:format("Test OK !!! ~p~n",[?MODULE]),
    log_loop([]),

    timer:sleep(2000),
    init:stop(),
    ok.





%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

test_0(ControlId)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    LOG=?LOG_DEBUG("Debug ~p~n",[?MODULE]),
    LOG=?LOG_NOTICE("notice ~p~n",[?MODULE]),
    LOG=?LOG_WARNING("warning ~p~n",[?MODULE]),
    LOG=?LOG_ALERT("alert ~p~n",[?MODULE]),
   io:format("LOG ~p~n",[{LOG,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok.



%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
-define(TestAppFile,"add_test.application").

test_2(ControlId)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    Result=send_receive(ControlId,{install,?TestAppFile},5000),
    io:format("Result ~p~n",[{Result,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test_11(ControlId)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,42}=sd:call(ControlId,{add,20,22},5000),
    {error,_}=sd:call(glurk,{add,20,22},5000),
    {ok,43}=sd:call(ControlId,{add,20,23},5000),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test_1(ControlId)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,42}=sd:call(ControlId,{add,20,22},5000),
    {error,_}=sd:call(glurk,{add,20,22},5000),
    {ok,43}=sd:call(ControlId,{add,20,23},5000),
    ok.

send_receive(ServerId,Msg,TimeOut)->
    Self=self(),
    case rpc:call(node(),global,send,[ServerId,{Self,Msg}],5000) of
	{badrpc,Reason}->
	    {error,[badrpc,Reason]};
	Pid ->
	    ok,
	    receive
		{Pid,Reply}->
		    Reply
	    after 
		TimeOut ->
		    {error,["Timeout in call",ServerId,Msg,TimeOut]}
	    end
    end.
    

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
log_loop(Strings)->    
    Info=os:cmd("cat "++?LogFilePath),
    NewStrings=string:lexemes(Info,"\n"),
    
    [io:format("~p~n",[String])||String<-NewStrings,
				 false=:=lists:member(String,Strings)],
    timer:sleep(10*1000),
    log_loop(NewStrings).
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,Host}=net:gethostname(),
    ControlNode=list_to_atom(?Appl++"@"++Host),
    rpc:call(ControlNode,init,stop,[],5000),
    timer:sleep(2000),

    file:del_dir_r(?ApplDir),
    ok=file:make_dir(?ApplDir),
   
    %% Unpack tar file
    TarFileFullPath=filename:join([?TarDir,?TarFile]),
    os:cmd("tar -zxvf "++TarFileFullPath++" "++"-C"++" "++?ApplDir),
  
    rpc:call(get_node(?NodeName),init,stop,[],5000),
    true=check_node_stopped(get_node(?NodeName)),
    io:format("~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    %% Start application to test and check node started
    []=os:cmd(?Daemon),
    true=check_node_started(get_node(?NodeName)),
    
    %% Check applications are correct started
    pong=rpc:call(get_node(?NodeName),log,ping,[],3*5000),
  %  pong=rpc:call(get_node(?NodeName),rd,ping,[],5000),

    %% Change
   % pong=rpc:call(get_node(?NodeName),api,ping,[],3*5000),
    
    Applications=rpc:call(ControlNode,application,which_applications,[],5000),
    io:format("Applications = ~p~n",[{Applications,?MODULE,?FUNCTION_NAME,?LINE}]),
    

    %% 
    net_adm:world(),
    MyPid=self(),
    yes=global:register_name(?MODULE,MyPid),
  
    

    {ok,Hostname}=net:gethostname(),
    ControlId=list_to_atom(atom_to_list(control)++"@"++Hostname),
    global:whereis_name(ControlId),

    {ok,ControlId}.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

check_node_started(Node)->
    check_node_started(Node,?NumCheck,?CheckDelay,false).

check_node_started(_Node,_NumCheck,_CheckDelay,true)->
    true;
check_node_started(_Node,0,_CheckDelay,Boolean)->
    Boolean;
check_node_started(Node,NumCheck,CheckDelay,false)->
    case net_adm:ping(Node) of
	pong->
	    N=NumCheck,
	    Boolean=true;
	pang ->
	    timer:sleep(CheckDelay),
	    N=NumCheck-1,
	    Boolean=false
    end,
 %   io:format("NumCheck ~p~n",[{NumCheck,?MODULE,?LINE,?FUNCTION_NAME}]),
    check_node_started(Node,N,CheckDelay,Boolean).
    
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

check_node_stopped(Node)->
    check_node_stopped(Node,?NumCheck,?CheckDelay,false).

check_node_stopped(_Node,_NumCheck,_CheckDelay,true)->
    true;
check_node_stopped(_Node,0,_CheckDelay,Boolean)->
    Boolean;
check_node_stopped(Node,NumCheck,CheckDelay,false)->
    case net_adm:ping(Node) of
	pang->
	    N=NumCheck,
	    Boolean=true;
	pong ->
	    timer:sleep(CheckDelay),
	    N=NumCheck-1,
	    Boolean=false
    end,
 %   io:format("NumCheck ~p~n",[{NumCheck,?MODULE,?LINE,?FUNCTION_NAME}]),
    check_node_stopped(Node,N,CheckDelay,Boolean).    
    

get_node(NodeName)->
    {ok,Host}=net:gethostname(),
    list_to_atom(NodeName++"@"++Host).
