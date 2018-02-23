%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Georges Younes.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%%

-module(trcb_exp_SUITE).
-author("Georges Younes <georges.r.younes@gmail.com>").

%% common_test callbacks
-export([%% suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0]).

%% tests
-compile([nowarn_export_all, export_all]).

-include("trcb_exp.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/inet.hrl").

-define(EVENT_NUMBER_PING, 20).
-define(EVENT_NUMBER_TRCB, 15).
-define(EVENT_INTERVAL, 1000).
-define(LATENCY, 0).

%% ===================================================================
%% common_test callbacks
%% ===================================================================

% suite() ->
%     [{timetrap, {minutes, 2}}].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(Case, Config) ->
    ct:pal("Beginning test case: ~p", [Case]),
    Config.

end_per_testcase(Case, Config) ->
    ct:pal("Ending test case: ~p", [Case]),
    Config.

all() ->
    [
     % test_trcb_base,
     % test_trcb_dots,
     test_trcb_ping
    ].

%% ===================================================================
%% tests
%% ===================================================================

test_trcb_base(_Config) ->
    run_test(base, 7, ?EVENT_NUMBER_TRCB, ?LATENCY).

test_trcb_dots(_Config) ->
    run_test(dots, 7, ?EVENT_NUMBER_TRCB, ?LATENCY).

test_trcb_ping(_Config) ->
    run_test(ping, 2, ?EVENT_NUMBER_PING, ?LATENCY).

%% @private
run_test(Mode, NodeNumber, EventNumber, Latency) ->
    Options = [{node_number, NodeNumber},
               {trcb_exp_settings,
                [{trcb_exp_node_number, NodeNumber},
                 {trcb_exp_mode, Mode},
                 {trcb_exp_latency, Latency},
                 {trcb_exp_default_event_interval, ?EVENT_INTERVAL},
                 {trcb_exp_node_event_number, EventNumber}]}],

    trcb_exp_local_experiments_support:run_trcb(Options).
