%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 24 Sep 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(test_appl).

%% API
-export([start/0]).

%%%

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.api").
-define(Appl,appl).
-define(NodeName,"appl").

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
   
    ok=test_1(),
    ok=test_2(),
    ok=test_3(),
    ok=test_4(),
    ok=test_5(),
    ok=test_6(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

test_6()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,Host}=net:gethostname(),
    N=list_to_atom("appl@"++Host),
    X=rpc:call(N,appl,test_diff_result,[],6000),
    {error,["M:F [A]) with reason",lib_appl,test_diff_result,
	    [add_test,add_test,add,[20,22],5000],
	    "Reason=",["Different results",42,[glurk]]]
    }=X,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

test_5()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,Host}=net:gethostname(),
    N=list_to_atom("appl@"++Host),
    X=rpc:call(N,appl,test_badrpc_no_nodes,[],6000),
    {error,["M:F [A]) with reason",lib_appl,test_badrpc_no_nodes,
	    [add_test,erlang,glurk,[],5000],
	    "Reason=",
	    ["NonExistingNodes and BadRpcList ",
	     [glurk@c50],
	     [{badrpc,{'EXIT',{undef,[{erlang,glurk,[],[]}]}}}]]]}=X,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

test_4()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,Host}=net:gethostname(),
    N=list_to_atom("appl@"++Host),
    {error,[ErrorMap]}=rpc:call(N,appl,test_crash,[],6000),
    error=maps:get(event,ErrorMap),
    appl=maps:get(module,ErrorMap),
    handle_call=maps:get(function,ErrorMap),
    %maps:get(line,ErrorMap),
    []=maps:get(args,ErrorMap),
    badarith=maps:get(reason,ErrorMap),
    %maps:get(stacktrace,ErrorMap),
    ok.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

test_3()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,Host}=net:gethostname(),
    N=list_to_atom("appl@"++Host),
    {error,["M:F [A]) with reason",lib_appl,call,
	    [glurk,add_test,add,[20,22],5000],
	    "Reason=",["No target resources ",glurk]]
    }=rpc:call(N,appl,call,[glurk,add_test,add,[20,22],5000],6000),
    ok.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

test_2()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,Host}=net:gethostname(),
    N=list_to_atom("appl@"++Host),
    {ok,42}=rpc:call(N,appl,call,[add_test,add_test,add,[20,22],5000],6000),
    ok.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

test_1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,Host}=net:gethostname(),
    ApplNode=list_to_atom("appl@"++Host),
    pong=rpc:call(ApplNode,appl,ping,[],5000),
    {Year,Month,Day}=date(),
    {ok,Year,Month,Day}=rpc:call(ApplNode,appl,template_call,[glurk],5000),
    ok.


    

%%%===================================================================
%%% Internal functions
%%%===================================================================
