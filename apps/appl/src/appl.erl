%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% 
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(appl).  
 
-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").

%% To be changed when create a new server
-include("appl.hrl").
-include("appl.rd").
%% API

-export([

	 
	]).


%% admin




-export([
	 call/5,
	 test_diff_result/0,
	 test_crash/0,
	 test_badrpc_no_nodes/0,
	 template_call/1,
	 template_cast/1,	 
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
		control_node_active,
		target_resources_status
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
-spec call(ResourceType::atom(),M::atom(),F::atom(),Args::term(),T::integer()) -> {ok,Result::term()}|{error,Reason::term()}.
call(ResourceType,M,F,Arg,T)-> 
    gen_server:call(?SERVER, {call,ResourceType,M,F,Arg,T},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Used to check if the application has started correct
%% @end
%%--------------------------------------------------------------------
-spec test_diff_result() -> ok | Error::term().
test_diff_result()-> 
    gen_server:call(?SERVER, {test_diff_result},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Used to check if the application has started correct
%% @end
%%--------------------------------------------------------------------
-spec test_crash() -> ok | Error::term().
test_crash()-> 
    gen_server:call(?SERVER, {test_crash},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Used to check if the application has started correct
%% @end
%%--------------------------------------------------------------------
-spec test_badrpc_no_nodes() -> ok | Error::term().
test_badrpc_no_nodes()-> 
    gen_server:call(?SERVER, {test_badrpc_no_nodes},infinity).

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
%% Used to check if the application has started correct
%% @end
%%--------------------------------------------------------------------
-spec template_call(Args::term()) -> {ok,Year::integer(),Month::integer(),Day::integer()} | Error::term().
template_call(Args)-> 
    gen_server:call(?SERVER, {template_call,Args},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Used to check if the application has started correct
%% @end
%%--------------------------------------------------------------------
-spec template_cast(Args::term()) -> ok.
template_cast(Args)-> 
    gen_server:cast(?SERVER, {template_cast,Args}).

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
    
    {ok, #state{
	    	    
	   },0}.


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
handle_call({call,ResourceType,M,F,Args,T}, _From, State) ->
    Result=try lib_appl:call(ResourceType,M,F,Args,T) of
	       {ok,R}->
		   {ok,R};
	       {error,ErrorR}->
		   {error,["M:F [A]) with reason",lib_appl,call,[ResourceType,M,F,Args,T],"Reason=", ErrorR]}
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
	      {ok,Res}->
		  {ok,Res};
	      {error,ErrorReason}->
		  {error,ErrorReason}
	  end,
    {reply, Reply,State};

handle_call({test_crash}, _From, State) ->
    Result=try lib_appl:test_crash(glurk,erlang,nodes,[],5000) of
	       {ok,R}->
		   {ok,R};
	       {error,ErrorR}->
		   {error,["M:F [A]) with reason",lib_appl,test_crash,[glurk,erlang,nodes,[],5000],"Reason=", ErrorR]}
	   catch
	       Event:Reason:Stacktrace ->
		   {error,[#{event=>Event,
			     module=>?MODULE,
			     function=>?FUNCTION_NAME,
			     line=>?LINE,
			     args=>[],
			     reason=>Reason,
			     stacktrace=>[Stacktrace]}]}
	   end,
    Reply=case Result of
	      {ok,Res}->
		  {ok,Res};
	      {error,ErrorReason}->
		  {error,ErrorReason}
	  end,
    {reply, Reply,State};

handle_call({test_badrpc_no_nodes}, _From, State) ->
    Result=try lib_appl:test_badrpc_no_nodes(add_test,erlang,glurk,[],5000) of
	       {ok,R}->
		   {ok,R};
	       {error,ErrorR}->
		   {error,["M:F [A]) with reason",lib_appl,test_badrpc_no_nodes,[add_test,erlang,glurk,[],5000],"Reason=", ErrorR]}
	   catch
	       Event:Reason:Stacktrace ->
		   {error,[#{event=>Event,
			     module=>?MODULE,
			     function=>?FUNCTION_NAME,
			     line=>?LINE,
			     args=>[],
			     reason=>Reason,
			     stacktrace=>[Stacktrace]}]}
	   end,
    Reply=case Result of
	      {ok,Res}->
		  {ok,Res};
	      {error,ErrorReason}->
		  {error,ErrorReason}
	  end,
    {reply, Reply,State};

handle_call({test_diff_result}, _From, State) ->
    Result=try lib_appl:test_diff_result(add_test,add_test,add,[20,22],5000) of
	       {ok,R}->
		   {ok,R};
	       {error,ErrorR}->
		   {error,["M:F [A]) with reason",lib_appl,test_diff_result,[add_test,add_test,add,[20,22],5000],"Reason=", ErrorR]}
	   catch
	       Event:Reason:Stacktrace ->
		   {error,[#{event=>Event,
			     module=>?MODULE,
			     function=>?FUNCTION_NAME,
			     line=>?LINE,
			     args=>[],
			     reason=>Reason,
			     stacktrace=>[Stacktrace]}]}
	   end,
    Reply=case Result of
	      {ok,Res}->
		  {ok,Res};
	      {error,ErrorReason}->
		  {error,ErrorReason}
	  end,
    {reply, Reply,State};


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


handle_cast({template_cast,Args}, State) ->
    Result=try erlang:apply(erlang,date,[])  of
	      {Year,Month,Day}->
		   {ok,Year,Month,Day};
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
    case Result of
	{ok,_Year,_Month,_Day}->
	    NewState=State;
	{error,_ErrorReason}->
	    NewState=State
    end,
    {noreply, NewState};



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
    case log:create_logger(NodeNodeLogDir,?LocalLogDir,?LogFile,?MaxNumFiles,?MaxNumBytes) of
	ok->
	    ?LOG_NOTICE("Log dirs and file created",[NodeNodeLogDir]);
	LogError->
	    ?LOG_WARNING("Failed to create log dir and file ",[LogError])
    end,

    %%-- Connect and trade resources
    CtrlNode=lib_vm:get_node(?ControlNodeName),
    NewControlStatus=case net_adm:ping(CtrlNode) of
			 pang->
			     ?LOG_WARNING("Error Control node ctrl is not available ",[CtrlNode]),
			     false; 
			 pong->
			     ?LOG_NOTICE("Control node ctrl is availablel",[CtrlNode]),
			     true
		     end,
    initial_trade_resources(),
    TargetTypes=rd_store:get_target_resource_types(),
    NonActiveTargetTypes=lists:sort([TargetType||TargetType<-TargetTypes,
						 []=:=rd:fetch_resources(TargetType)]),
    TargetStatus=State#state.target_resources_status,
    NewTargetStatus=if 
			TargetStatus=:=NonActiveTargetTypes->
			    NonActiveTargetTypes;
			true->
			    case NonActiveTargetTypes of
				[]->
				    ?LOG_NOTICE("All needed target types are availablel",[TargetTypes]);	    
				TargetError ->
				    ?LOG_WARNING("Error Target types missing ",[TargetError])
			    end,
			    NonActiveTargetTypes
		    end,
    NewState=State#state{target_resources_status=NewTargetStatus,
			 control_node_active=NewControlStatus},	    
    Self=self(),
    spawn_link(fun()->rd_loop(Self) end),
    {noreply, NewState};

handle_info(rd_loop_timeout, State) ->
    CtrlNode=lib_vm:get_node(?ControlNodeName),
    NewControlStatus=case net_adm:ping(CtrlNode) of
			 pang->
			     case State#state.control_node_active of
				 false->
				     false;
				 true->
				     ?LOG_WARNING("Error Control node ctrl is not available ",[CtrlNode]),
				     false
			     end; 
			 pong->
			     case State#state.control_node_active of
				 true->
				     true;
				 false->
				     ?LOG_NOTICE("Control node ctrl is availablel",[CtrlNode]),
				     true
			     end
		     end,
    TargetTypes=rd_store:get_target_resource_types(),
    NonActiveTargetTypes=lists:sort([TargetType||TargetType<-TargetTypes,
						 []=:=rd:fetch_resources(TargetType)]),
    
    TargetStatus=State#state.target_resources_status,
    NewTargetStatus=if 
			TargetStatus=:=NonActiveTargetTypes->
			    NonActiveTargetTypes;
			true->
			    case NonActiveTargetTypes of
				[]->
				    ?LOG_NOTICE("All needed target types are availablel",[TargetTypes]);	    
				Error ->
				    ?LOG_WARNING("Error Target types missing ",[Error])
			    end,
			    NonActiveTargetTypes
		    end,
    NewState=State#state{target_resources_status=NewTargetStatus,
			 control_node_active=NewControlStatus},	    
    Self=self(),
    spawn_link(fun()->rd_loop(Self) end),
    {noreply, NewState};

handle_info({'EXIT',_Pid,normal}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    ?LOG_WARNING("Unmatched signal",[Info]),
    io:format("unmatched_signal ~p~n",[{Info,?MODULE,?LINE}]),
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
rd_loop(Parent)->
    CtrlNode=lib_vm:get_node("ctrl"),
    net_adm:ping(CtrlNode),
    rd:trade_resources(),
    timer:sleep(?RdTradeInterval),
    Parent!rd_loop_timeout.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

initial_trade_resources()->
    [rd:add_local_resource(ResourceType,Resource)||{ResourceType,Resource}<-?LocalResourceTuples],
    [rd:add_target_resource_type(TargetType)||TargetType<-?TargetTypes],
    rd:trade_resources(),
    timer:sleep(3000),
    rd:trade_resources(),
    timer:sleep(3000).

