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

-module(trcb_exp_orchestration).
-author("Georges Younes <georges.r.younes@gmail.com").

-include("trcb_exp.hrl").

-export([get_task/3,
         get_tasks/3,
         stop_tasks/1]).

%% @doc Returns the specs of tag, given a name, a port, and
%%      filtering by timestamp if the third argument is true.
-callback get_tasks(atom(), node_port(), boolean()) -> [node_spec()].

%% @doct Stop tasks, given a list of tags
-callback stop_tasks([atom()]) -> ok.


-spec get_task(atom(), node_port(), boolean()) ->
    {ok, node_spec()} | {error, not_connected}.
get_task(Tag, Port, FilterByTimestamp) ->
    Nodes = get_tasks(Tag, Port, FilterByTimestamp),

    case Nodes of
        [] ->
            {error, not_connected};
        [Task|_] ->
            {ok, Task}
    end.

-spec get_tasks(atom(), node_port(), boolean()) -> [node_spec()].
get_tasks(Tag, Port, FilterByTimestamp) ->
    do(get_tasks, [Tag, Port, FilterByTimestamp]).

-spec stop_tasks([atom()]) -> ok.
stop_tasks(Tags) ->
    do(stop_tasks, [Tags]).

%% @private
do(Function, Args) ->
    Orchestration = trcb_exp_config:get(trcb_exp_orchestration),
    case Orchestration of
        kubernetes ->
            erlang:apply(trcb_exp_kube_orchestration, Function, Args);
        emulab ->
            erlang:apply(trcb_exp_emu_orchestration, Function, Args)
    end.
