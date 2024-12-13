%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 24 Sep 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(test_application_server).

%% API
-export([start/0]).

%%%
-define(NodeName,"add_test").
-define(AddTestApp,add_test).
-define(AddTestFileName,"add_test.application").
-define(AddTestRepoDir,"add_test" ).
-define(CatalogDir,"application_specs").
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.api").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok=test_intent(),

    loop(),
    ok.


loop()->
    timer:sleep(20*1000),
    loop().

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
% intent_loop()
% 1. Check if repo is updated => update and 
% 2. check files to be started and start them
% 3. check files to be stopped and stop them
% 4 Wait a minute and loop
-define(AppToStop,"kvs_glurk_test.application").

test_intent()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    {ok,WantedApplicationFiles}=application_server:get_wanted_applications(),
    {ok,[]}=application_server:get_active_applications(),
    FilesToStart=application_server:applications_to_start(),
    io:format("FilesToStart ~p~n",[FilesToStart]),    
    [File1|_]=FilesToStart,
    StartResult1=application_server:install(File1),
    io:format("StartResult1 ~p~n",[StartResult1]),    
    {error,["M:F [A]) with reason",lib_application,install,
	    ["add_test.application"],
	    "Reason=",["Allready installed",
		       "application_specs/add_test.application"]
	   ]
    }=application_server:install(File1),
    {ok,["add_test.application"]}=application_server:get_active_applications(),
  %  FilesToStart2=application_server:applications_to_start(),
  %  StartResult2=application_server:install(FilesToStart2),
   % io:format("StartResul2 ~p~n",[StartResult2]),    
  %  {ok,["kvs_test.application","add_test.application"]}=application_server:get_active_applications(),
    []=application_server:applications_to_start(),
    []=application_server:applications_to_stop(),

    %% load and start application that should not be started
  %  []=os:cmd("cp test/"++?AppToStop++" application_specs"),
  %  StartResult3=application_server:load_start(?AppToStop),
  %  io:format("StartResul3 ~p~n",[StartResult3]),     
  %  []=os:cmd("rm application_specs/"++?AppToStop),    
    
  %  FilesToStop=application_server:applications_to_stop(),    
  %  ["kvs_glurk_test.application"]=FilesToStop,
    

  %  StopResult1=[application_server:uninstall(FileToStop)||FileToStop<-FilesToStop],
  %  io:format("StopResult1 ~p~n",[StopResult1]),    
    []=application_server:applications_to_start(),
    []=application_server:applications_to_stop(),

    StopResult2=[application_server:uninstall(WantedApplicationFile)||WantedApplicationFile<-WantedApplicationFiles],
    io:format("StopResult2 ~p~n",[StopResult2]),      
    {ok,[]}=application_server:get_active_applications(), 
    
    ["add_test.application"]=application_server:applications_to_start(),
    []=application_server:applications_to_stop(),

    ok.


    

%%%===================================================================
%%% Internal functions
%%%===================================================================
