%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(api).

%% API
-export([
	
	 ping/0
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ping()->
    control:ping().
%%%===================================================================
%%% Internal functions
%%%===================================================================