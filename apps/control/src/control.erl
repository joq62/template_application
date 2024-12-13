%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% 
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(control).  
 
-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").

%% To be changed when create a new server
-include("control.hrl").
-include("control.rd").
%% API

-export([

	 
	]).


%% admin




-export([
 
	 start/0,
	 ping/0,
	 stop/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
		     
-record(state, {

	       }).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).


%%--------------------------------------------------------------------
%% @doc
%% Used to check if the application has started correct
%% @end
%%--------------------------------------------------------------------
-spec ping() -> pong | Error::term().
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%stop()-> gen_server:cast(?SERVER, {stop}).
stop()-> gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.

init([]) ->
   
    net_adm:world(),
    {ok,Hostname}=net:gethostname(),
    ControlId=list_to_atom(atom_to_list(?MODULE)++"@"++Hostname),
    MyPid=self(),
    yes=global:register_name(ControlId,MyPid),
    MyPid=global:whereis_name(ControlId),
  
 
    {ok, #state{},0}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.



%%----- TemplateCode ---------------------------------------------------------------
handle_call({template_call,Args}, _From, State) ->
    Result=try erlang:apply(erlang,date,[])  of
	      {Y,M,D}->
		   {ok,Y,M,D};
	      {error,ErrorR}->
		   {error,["M:F [A]) with reason",erlang,date,[erlang,date,[]],"Reason=", ErrorR]}
	   catch
	       Event:Reason:Stacktrace ->
		   {error,[#{event=>Event,
			     module=>?MODULE,
			     function=>?FUNCTION_NAME,
			     line=>?LINE,
			     args=>Args,
			     reason=>Reason,
			     stacktrace=>[Stacktrace]}]}
	   end,
    Reply=case Result of
	      {ok,Year,Month,Day}->
		  NewState=State,
		  {ok,Year,Month,Day};
	      {error,ErrorReason}->
		  NewState=State,
		  {error,ErrorReason}
	  end,
    {reply, Reply,NewState};

%%----- Admin ---------------------------------------------------------------

handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(UnMatchedSignal, From, State) ->
   ?LOG_WARNING("Unmatched signal",[UnMatchedSignal]),
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------

handle_cast({stop}, State) ->
    
    {stop,normal,ok,State};

handle_cast(UnMatchedSignal, State) ->
    ?LOG_WARNING("Unmatched signal",[UnMatchedSignal]),
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info(timeout, State) ->
   
  %%- Create logfiles
    file:del_dir_r(?MainLogDir),
    file:make_dir(?MainLogDir),
    [NodeName,_HostName]=string:tokens(atom_to_list(node()),"@"),
    NodeNodeLogDir=filename:join(?MainLogDir,NodeName),

% {control,{<8755.284.0>,{log,notice,["Server started ",[control],{'control@c50',<8755.284.0>,control,init,129,{1734,33899,240128}
% {sd,call,[control,{log,notice,["Log dirs and file created",["logs/control"],{'control@c50',<8923.284.0>,control,init,128,{1734,35546,47273}}}}]
 %   glurk=global:send(control,{add,20,22}),
 %   glurk=sd:call(control,{add,20,22},5000),
						%  glurk=?LOG_NOTICE("Test ",[]),
    
    case log:create_logger(NodeNodeLogDir,?LocalLogDir,?LogFile,?MaxNumFiles,?MaxNumBytes) of
	ok->
	 %   ok;
	    ?LOG_NOTICE("Log dirs and file created",[NodeNodeLogDir]);
	LogError->
	    %ok
	    ?LOG_WARNING("Failed to create log dir and file ",[LogError])
    end,
    ?LOG_WARNING("Server started ",[?MODULE]),
    {noreply, State};

%%-------------- API's --------------------%%

%%--------------------------------------------------------------------
%% @doc
%% log Apis
%% @end
%%--------------------------------------------------------------------

handle_info({Pid,{log,debug,[Msg,Data,Node,PidX,M,F,Line,TimeStamp]}}, State) ->
    R=log:debug(Msg,Data,Node,PidX,M,F,Line,TimeStamp),
    Pid!{self(),{R}},
    {noreply, State};

handle_info({Pid,{log,notice,[Msg,Data,Sender]}}, State) ->
    io:format(" ~p~n",[{Msg,Data,Sender,?MODULE,?LINE}]),
    R=log:notice(Msg,Data,Sender),
    Pid!{self(),{R}},
    {noreply, State};

handle_info({Pid,{log,warning,[Msg,Data,Sender]}}, State) ->
    R=log:warning(Msg,Data,Sender),
    Pid!{self(),{R}},
    {noreply, State};

handle_info({Pid,{log,alert,[Msg,Data,Sender]}}, State) ->
    R=log:alert(Msg,Data,Sender),
    Pid!{self(),{R}},
    {noreply, State};

%%--------------------------------------------------------------------
%% @doc
%% application_server Apis
%% @end
%%--------------------------------------------------------------------

handle_info({Pid,{install,FileName}}, State) ->
    R=application_server:install(FileName),
    Pid!{self(),{R}},
  {noreply, State};

handle_info({Pid,{install_build,FileName}}, State) ->
    R=application_server:install_build(FileName),
    Pid!{self(),{R}},
  {noreply, State};

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
handle_info({Pid,{add,A,B}}, State) ->
  Pid!{self(),{ok,A+B}},
  {noreply, State};



handle_info(Info, State) ->
    ?LOG_WARNING("Unmatched signal",[Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
