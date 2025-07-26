%%%-------------------------------------------------------------------
%% @doc aos_test_suite public API
%% @end
%%%-------------------------------------------------------------------

-module(aos_test_suite_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    aos_test_suite_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
