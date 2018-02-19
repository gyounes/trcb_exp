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

-module(trcb_exp_emu_orchestration).
-author("Georges Younes <georges.r.younes@gmail.com").

-include("trcb_exp.hrl").

-behaviour(trcb_exp_orchestration).

-export([get_tasks/3,
         stop_tasks/1]).

-spec get_tasks(atom(), node_port(), boolean()) -> [node_spec()].
get_tasks(Tag, Port, _FilterByTimestamp) ->
    case Tag of
        exp ->
            [trcb_exp_util:generate_spec("10.1.1.4", Port), trcb_exp_util:generate_spec("10.1.1.5", Port)];
        synchronizer ->
            [trcb_exp_util:generate_spec("10.1.1.3", Port)];
        redis ->
            [trcb_exp_util:generate_spec("10.1.1.2", Port)]
    end.

-spec stop_tasks([atom()]) -> ok.
stop_tasks(_Tags) ->
    ok.