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
-module(lib_log).    
 
-export([
	 create_logger/4,
	 create_logger/5,
	 do_log/5,
	 parse/1

	]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/logger.hrl").

-record(state, {
		debug=[],
		notice=[],
		warning=[],
		alert=[],
		main_log_dir,
		local_log_dir,
		logfile,
		max_num_files,
		max_num_bytes,
		
		log_file_path,
		log_file,
		max_log_length
	
	       }).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
do_log(debug,Msg,Data,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp},State)->
    R= io_lib:format("~p",[Msg]),
    MsgAsString=lists:flatten(R),
    logger:debug(MsgAsString,#{timestamp=>TimeStamp,
			       sender_time=>calendar:now_to_datetime(TimeStamp),
			       sender_node=>SenderNode,
			       sender_pid=>SenderPid,
			       sender_module=>Module,
			       sender_function=>FunctionName,
			       sender_line=>Line,
			       sender_data=>Data}),
    Len=length(State#state.debug),
    if
	Len<State#state.max_log_length->
	    NewState=State#state{debug=[{TimeStamp,SenderNode,SenderPid,Module,FunctionName,Line,MsgAsString,Data}|State#state.debug]};
	true->
	    Templist=lists:delete(lists:last(State#state.notice),State#state.debug),
	    NewState=State#state{debug=[{TimeStamp,SenderNode,SenderPid,Module,FunctionName,Line,MsgAsString,Data}|Templist]}
    end,
    {ok,NewState};

do_log(notice,Msg,Data,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp},State)->
    R= io_lib:format("~p",[Msg]),
    MsgAsString=lists:flatten(R),
    logger:notice(MsgAsString,#{timestamp=>TimeStamp,
				sender_time=>calendar:now_to_datetime(TimeStamp),
				sender_node=>SenderNode,
				sender_pid=>SenderPid,
				sender_module=>Module,
				sender_function=>FunctionName,
				sender_line=>Line,
				sender_data=>Data}),
    Len=length(State#state.notice),
    if
	Len<State#state.max_log_length->
	    NewState=State#state{notice=[{TimeStamp,SenderNode,SenderPid,Module,FunctionName,Line,MsgAsString,Data}|State#state.notice]};
	true->
	    Templist=lists:delete(lists:last(State#state.notice),State#state.notice),
	    NewState=State#state{notice=[{TimeStamp,SenderNode,SenderPid,Module,FunctionName,Line,MsgAsString,Data}|Templist]}
    end,
    {ok,NewState};

do_log(warning,Msg,Data,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp},State)->
    R= io_lib:format("~p",[Msg]),
    MsgAsString=lists:flatten(R),
    logger:warning(MsgAsString,#{timestamp=>TimeStamp,
				 sender_time=>calendar:now_to_datetime(TimeStamp),
				 sender_node=>SenderNode,
				 sender_pid=>SenderPid,
				 sender_module=>Module,
				 sender_function=>FunctionName,
				 sender_line=>Line,
				 sender_data=>Data}),
    Len=length(State#state.warning),
    if
	Len<State#state.max_log_length->
	    NewState=State#state{warning=[{TimeStamp,SenderNode,SenderPid,Module,FunctionName,Line,MsgAsString,Data}|State#state.warning]};
	true->
	    Templist=lists:delete(lists:last(State#state.notice),State#state.warning),
	    NewState=State#state{warning=[{TimeStamp,SenderNode,SenderPid,Module,FunctionName,Line,MsgAsString,Data}|Templist]}
    end,
    {ok,NewState};

do_log(alert,Msg,Data,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp},State)->
    R= io_lib:format("~p",[Msg]),
    MsgAsString=lists:flatten(R),
    logger:alert(MsgAsString,#{timestamp=>TimeStamp,
			       sender_time=>calendar:now_to_datetime(TimeStamp),
			       sender_node=>SenderNode,
			       sender_pid=>SenderPid,
			       sender_module=>Module,
			       sender_function=>FunctionName,
			       sender_line=>Line,
			       sender_data=>Data}),
    Len=length(State#state.alert),
    if
	Len<State#state.max_log_length->
	    NewState=State#state{alert=[{TimeStamp,SenderNode,SenderPid,Module,FunctionName,Line,MsgAsString,Data}|State#state.alert]};
	true->
	    Templist=lists:delete(lists:last(State#state.notice),State#state.alert),
	    NewState=State#state{alert=[{TimeStamp,SenderNode,SenderPid,Module,FunctionName,Line,MsgAsString,Data}|Templist]}
    end,
    {ok,NewState};
do_log(Undefined,Msg,Data,{SenderNode,SenderPid,Module,FunctionName,Line,TimeStamp},State)->
    {error,["undefined log entry ",?MODULE,?LINE,Undefined]}.
	      

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

parse(ListRaw)->
    [parse_item(Item)||Item<-ListRaw].

parse_item({TimeStamp,_Time,SenderNode,SenderPid,Module,Function,Line,Data,MsgAsString})->
    {{Y,M,D},{H,Mi,S}}=calendar:now_to_datetime(TimeStamp),
    Year=integer_to_list(Y),
    Month=integer_to_list(M),
    Day=integer_to_list(D),
    Hour=integer_to_list(H),
    Min=integer_to_list(Mi),
    Sec=integer_to_list(S),

    SenderNodeStr=atom_to_list(SenderNode),
    SenderPidStr=pid_to_list(SenderPid),
    ModuleStr=atom_to_list(Module),
    FunctionStr=atom_to_list(Function),
    LineStr=integer_to_list(Line),
    
    
    DateTime=Year++"-"++Month++"-"++Day++" | "++Hour++":"++Min++":"++Sec++" | ",
    Text=DateTime++SenderNodeStr++" | "++SenderPidStr++" | "++ModuleStr++":"++FunctionStr++"/"++LineStr++" |  Msg : "++MsgAsString,
    [Text,Data].
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
create_logger(MainLogDir,LogFile,MaxNumFiles,MaxNumBytes)->

%    LocalLogDirFullPath=filename:join(MainLogDir,LocalLogDir),
%    LogFileFullPath=filename:join(LocalLogDirFullPath,LogFile),
    LogFileFullPath=filename:join(MainLogDir,LogFile),
  
    file:make_dir(MainLogDir),
 %   file:make_dir(LocalLogDirFullPath),
    Result=case logger:add_handler(my_standar_disk_h, logger_std_h,
			  #{formatter => {logger_formatter,
					  #{ template => [
							  timestamp," | ",
							  sender_time," | ",
							  level," | ",
							  sender_node," | ",
							  sender_pid," | ",
							  sender_module," | ",
							  sender_function," | ",
							  sender_line," | ",
							  msg," | ",
							  sender_data,"\n"
							 ]}}}) of
	       {error,{already_exist,my_standar_disk_h}}->
		  %  add_handler(LogFileFullPath,LocalLogDirFullPath,MaxNumFiles,MaxNumBytes);
		   add_handler(LogFileFullPath,MainLogDir,MaxNumFiles,MaxNumBytes);
	       {error,Reason}->
		   {error,["Error when creating LogFile :",MainLogDir,Reason,?MODULE,?LINE]};
	       ok->
		   add_handler(LogFileFullPath,MainLogDir,MaxNumFiles,MaxNumBytes)
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
%create_logger(LogDir)->

create_logger(MainLogDir,LocalLogDir,LogFile,MaxNumFiles,MaxNumBytes)->
    LocalLogDirFullPath=filename:join(MainLogDir,LocalLogDir),
    LogFileFullPath=filename:join(LocalLogDirFullPath,LogFile),
  
    file:make_dir(MainLogDir),
    file:make_dir(LocalLogDirFullPath),
    Result=case logger:add_handler(my_standar_disk_h, logger_std_h,
			  #{formatter => {logger_formatter,
					  #{ template => [
							  timestamp," | ",
							  sender_time," | ",
							  level," | ",
							  sender_node," | ",
							  sender_pid," | ",
							  sender_module," | ",
							  sender_function," | ",
							  sender_line," | ",
							  msg," | ",
							  sender_data,"\n"
							 ]}}}) of
	       {error,{already_exist,my_standar_disk_h}}->
		   add_handler(LogFileFullPath,LocalLogDirFullPath,MaxNumFiles,MaxNumBytes);
	       {error,Reason}->
		   {error,["Error when creating LogFile :",LocalLogDirFullPath,Reason,?MODULE,?LINE]};
	       ok->
		   add_handler(LogFileFullPath,LocalLogDirFullPath,MaxNumFiles,MaxNumBytes)
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
add_handler(LogFileFullPath,LocalLogDirFullPath,MaxNumFiles,MaxNumBytes)->
  %  io:format("add_handler ~p~n",[{LogFileFullPath,LocalLogDirFullPath,MaxNumFiles,MaxNumBytes,?MODULE,?LINE}]),
    case logger:add_handler(my_disk_log_h, logger_disk_log_h,
			    #{
			      config => #{file => LogFileFullPath,
					  type => wrap,
					  max_no_files => MaxNumFiles,  % 4
					  max_no_bytes => MaxNumBytes,    %1000*100,
					  filesync_repeat_interval => 1000},
			      formatter => {logger_formatter,
					    #{ template => [
							    timestamp," | ",
							    sender_time," | ",
							    level," | ",
							    sender_node," | ",
							    sender_pid," | ",
							    sender_module," | ",
							    sender_function," | ",
							    sender_line," | ",
							    msg," | ",
							    sender_data,"\n"
							   ]}}}) of
	{error,Reason}->
	    {error,["Error when creating LogFile :",LocalLogDirFullPath,Reason,?MODULE,?LINE]};
		       ok-> 
	    ok
    end.
