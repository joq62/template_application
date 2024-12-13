%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% 
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(application_server). 
 
-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").

-include("application.hrl").
-include("catalog.rd").


%% API

-export([
	 install_build/1,
	 install/1,
	 uninstall/1,

	 load_start/1,
	 stop_unload/1,

	 get_wanted_applications/0,
	 get_active_applications/0,
	 applications_to_start/0,
	 applications_to_stop/0
	 
	]).


-export([
	 start_node/1,
	 stop_node/1,
	 load/1,
	 start/1,
	 stop/1,
	 unload/1
	 
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
		specs_dir,
		application_maps
	        
	       }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec install_build(Filename :: string()) -> 
	  {ok,Node::node()} | {error,Reason::term()}.
install_build(Filename) ->
    gen_server:call(?SERVER,{install_build,Filename},infinity).
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec install(Filename :: string()) -> 
	  {ok,Node::node()} | {error,Reason::term()}.
install(Filename) ->
    gen_server:call(?SERVER,{install,Filename},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec uninstall(Filename :: string()) -> 
	  ok | {error,Reason::term()}.
uninstall(Filename) ->
    gen_server:call(?SERVER,{uninstall,Filename},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec load_start(Filename :: string()) -> 
	  {ok,Node::node()} | {error,Reason::term()}.
load_start(Filename) ->
    gen_server:call(?SERVER,{load_start,Filename},infinity).
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec stop_unload(Filename :: string()) -> 
	  ok | {error,Reason::term()}.
stop_unload(Filename) ->
    gen_server:call(?SERVER,{stop_unload,Filename},infinity).


%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% 
%% @end
%%--------------------------------------------------------------------
-spec applications_to_start() -> 
	  {ok,ListOfApplicationSpecs::term()} | {error,Reason::term()}.
applications_to_start()  ->
    gen_server:call(?SERVER,{applications_to_start},infinity).
%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% 
%% @end
%%--------------------------------------------------------------------
-spec applications_to_stop() -> 
	  {ok,ListOfApplicationSpecs::term()} | {error,Reason::term()}.
applications_to_stop()  ->
    gen_server:call(?SERVER,{applications_to_stop},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_wanted_applications() -> 
	  {ok,ListOfApplicationSpecs::term()} | {error,Reason::term()}.
get_wanted_applications()  ->
    gen_server:call(?SERVER,{get_wanted_applications},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_active_applications() -> 
	  {ok,ListOfApplicationSpecs::term()} | {error,Reason::term()}.
get_active_applications()  ->
    gen_server:call(?SERVER,{get_active_applications},infinity).



%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% 
%% @end
%%--------------------------------------------------------------------
-spec start_node(Filename :: string()) -> 
	  {ok,Node::node()} | {error,Reason::term()}.
start_node(Filename) ->
    gen_server:call(?SERVER,{start_node,Filename},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  
%%  
%% 
%% @end
%%--------------------------------------------------------------------
-spec stop_node(Filename :: string()) -> 
	  ok | {error,Reason::term()}.
stop_node(Filename) ->
    gen_server:call(?SERVER,{stop_node,Filename},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
-spec load(Filename :: string()) -> 
	  ok | {error,Reason::term()}.
load(Filename) ->
    gen_server:call(?SERVER,{load,Filename},infinity).



%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% @end
%%--------------------------------------------------------------------
-spec unload(Filename :: string()) -> 
	  ok | {error,Reason::term()}.
unload(Filename) ->
    gen_server:call(?SERVER,{unload,Filename},infinity).


%%--------------------------------------------------------------------
%% @doc
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
-spec start(Filename :: string()) -> 
	  ok | {error,Reason::term()}.
start(Filename) ->
    gen_server:call(?SERVER,{start,Filename},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Based on info Application file git clone the repo and 
%% extract the tar file  
%% 
%% @end
%%--------------------------------------------------------------------
-spec stop(Filename :: string()) -> 
	  ok | {error,Reason::term()}.
stop(Filename) ->
    gen_server:call(?SERVER,{stop,Filename},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% 
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
    
    {ok, #state{
	    specs_dir=?SpecsDir,
	    application_maps=[]
	    
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


handle_call({install_build,Filename}, _From, State) ->
    SpecFile=filename:join(State#state.specs_dir,Filename),
    Result=try lib_application:install_build(SpecFile,State#state.application_maps) of
	      {ok,R1,R2}->
		   {ok,R1,R2};
	      {error,ErrorR}->
		   {error,["M:F [A]) with reason",lib_application,install_build,[Filename],"Reason=", ErrorR]}
	   catch
	       Event:Reason:Stacktrace ->
		   {error,[#{event=>Event,
			     module=>?MODULE,
			     function=>?FUNCTION_NAME,
			     line=>?LINE,
			     args=>[Filename],
			     reason=>Reason,
			     stacktrace=>[Stacktrace]}]}
	   end,
    Reply=case Result of
	      {ok,Node,NewApplicationMaps}->
		  NewState=State#state{application_maps=NewApplicationMaps},
		  {ok,Node};
	      {error,ErrorReason}->
		  NewState=State,
		  {error,ErrorReason}
	  end,
    {reply, Reply,NewState};


handle_call({install,Filename}, _From, State) ->
    SpecFile=filename:join(State#state.specs_dir,Filename),
    Result=try lib_application:install(SpecFile,State#state.application_maps) of
	      {ok,R1,R2}->
		   {ok,R1,R2};
	      {error,ErrorR}->
		   {error,["M:F [A]) with reason",lib_application,install,[Filename],"Reason=", ErrorR]}
	   catch
	       Event:Reason:Stacktrace ->
		   {error,[#{event=>Event,
			     module=>?MODULE,
			     function=>?FUNCTION_NAME,
			     line=>?LINE,
			     args=>[Filename],
			     reason=>Reason,
			     stacktrace=>[Stacktrace]}]}
	   end,
    Reply=case Result of
	      {ok,Node,NewApplicationMaps}->
		  NewState=State#state{application_maps=NewApplicationMaps},
		  {ok,Node};
	      {error,ErrorReason}->
		  NewState=State,
		  {error,ErrorReason}
	  end,
    {reply, Reply,NewState};


handle_call({uninstall,Filename}, _From, State) ->
    SpecFile=filename:join(State#state.specs_dir,Filename),
    Result=try lib_application:uninstall(SpecFile,State#state.application_maps) of
	      {ok,R1}->
		   {ok,R1};
	      {error,ErrorR}->
		   {error,["M:F [A]) with reason",lib_application,uninstall,[Filename],"Reason=", ErrorR]}
	   catch
	       Event:Reason:Stacktrace ->
		   {error,[#{event=>Event,
			     module=>?MODULE,
			     function=>?FUNCTION_NAME,
			     line=>?LINE,
			     args=>[Filename],
			     reason=>Reason,
			     stacktrace=>[Stacktrace]}]}
	   end,
    Reply=case Result of
	      {ok,NewApplicationMaps}->
		  NewState=State#state{application_maps=NewApplicationMaps},
		  ok;
	      {error,ErrorReason}->
		  NewState=State,
		  {error,ErrorReason}
	  end,
    {reply, Reply,NewState};


handle_call({applications_to_start}, _From, State) ->
    {ok,WantedApplicationFiles}=lib_application:get_wanted_applications(State#state.specs_dir),
    {ok,ActiveApplicationFiles}=lib_application:get_active_applications(State#state.application_maps),
     Reply=[File||File<-WantedApplicationFiles,
			false=:=lists:member(File,ActiveApplicationFiles)],
    {reply, Reply,State};

handle_call({applications_to_stop}, _From, State) ->
    {ok,WantedApplicationFiles}=lib_application:get_wanted_applications(State#state.specs_dir),
    {ok,ActiveApplicationFiles}=lib_application:get_active_applications(State#state.application_maps),
     Reply=[File||File<-ActiveApplicationFiles,
			false=:=lists:member(File,WantedApplicationFiles)],
    {reply, Reply,State};

handle_call({get_wanted_applications}, _From, State) ->
    Reply=case lib_application:get_wanted_applications(State#state.specs_dir) of
	      {ok,ApplcationSpecFiles}->
		  {ok,ApplcationSpecFiles};
	      {error,Reason}->
		  {error,Reason}
	  end,
    {reply, Reply,State};

handle_call({get_active_applications}, _From, State) ->
    Reply=case lib_application:get_active_applications(State#state.application_maps) of
	      {ok,ApplcationSpecFiles}->
		  {ok,ApplcationSpecFiles};
	      {error,Reason}->
		  {error,Reason}
	  end,
    {reply, Reply,State};


%%--------------------------------------------------------------------



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

    %%Clean up application dirs and Mnesia dirs
    {ok,Files}=file:list_dir("."),
    ApplicationDirs=lib_application:get_application_dirs(Files),
    _=[{Dir,file:del_dir_r(Dir)}||Dir<-ApplicationDirs],
    MnesiaDirs=lib_application:get_mnesia_dirs(Files),
    _=[{Dir,file:del_dir_r(Dir)}||Dir<-MnesiaDirs],

    file:del_dir_r(?SpecsDir),
    case lib_git:update_repo(?SpecsDir) of
	{error,["Dir eexists ",_RepoDir]}->
	    case lib_git:clone(?RepoGit) of
		ok->
		    ?LOG_NOTICE("Repo dir didnt existed so a succesful cloned action is executed",[?SpecsDir]);
		{error,Reason}->
		    ?LOG_WARNING("Failed during clone action ",[Reason])
	    end;
	{error,["Already updated ","application_specs"]}->
	    ok;
	{error,Reason}->
	    ?LOG_WARNING("Failed to update ",[Reason]);
	{ok,Info} ->
	    ?LOG_NOTICE("Repo dir actions",[Info,?SpecsDir]),
	    ok
    end,    
    Self=self(),
    spawn_link(fun()->repo_check_timeout_loop(Self) end),
    ?LOG_NOTICE("Server started ",[?MODULE]),
    {noreply, State};

handle_info({timeout,check_repo_update}, State) ->
    case lib_git:update_repo(?SpecsDir) of
	{error,["Dir eexists ",_RepoDir]}->
	    ok=case lib_git:clone(?RepoGit) of
		   ok->
		       ?LOG_NOTICE("Repo dir didnt existed so a succesful cloned action is executed",[?SpecsDir]);
		   {error,Reason}->
		       ?LOG_WARNING("Failed during clone action ",[Reason])
	       end;
	{error,["Already updated ","application_specs"]}->
	    ok;
	{error,Reason}->
	    ?LOG_WARNING("Failed to update ",[Reason]);
	{ok,Info} ->
	    ?LOG_NOTICE("Repo dir actions",[Info,?SpecsDir]),
	    ok
    end,
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

repo_check_timeout_loop(Parent)->
    timer:sleep(?CheckRepoInterval),
    Parent!{timeout,check_repo_update},
    repo_check_timeout_loop(Parent).
