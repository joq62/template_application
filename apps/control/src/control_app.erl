%%%-------------------------------------------------------------------
%% @doc template_application public API
%% @end
%%%-------------------------------------------------------------------

-module(control_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    control_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
