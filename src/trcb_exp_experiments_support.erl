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

-module(trcb_exp_experiments_support).
-author("Georges Younes <georges.r.younes@gmail.com").

-include("trcb_exp.hrl").

-export([push_trcb_exp_metrics/1,
         push_lmetrics/0,
         push_ping_data/0]).

-define(LMETRICS, lmetrics).
-define(STORE, trcb_exp_metrics_store).
-define(SEP, ",").

-spec push_trcb_exp_metrics(timestamp()) -> ok.
push_trcb_exp_metrics(StartTime) ->
    TRCBVars = [trcb_exp_mode,
                trcb_exp_node_number,
                trcb_exp_default_event_interval,
                trcb_exp_node_event_number],
    TRCBConfigs = get_configs(TRCBVars),

    All = [{start_time, StartTime}]
       ++ TRCBConfigs,

    FilePath = file_path(synchronizer),
    File = encode(All),

    store(FilePath, File),
    ok.

-spec push_lmetrics() -> ok.
push_lmetrics() ->
    TimeSeries = ?LMETRICS:get_time_series(),
    Latency = ?LMETRICS:get_latency(),
    TransmissionTS = filter_by_ts_class(transmission, TimeSeries),
    MemoryTS = filter_by_ts_class(memory, TimeSeries),

    %% process transmission
    PerMessageType = lists:foldl(
        fun({Timestamp, transmission, {MessageType, Size}}, Acc0) ->
            orddict:append(MessageType, {Timestamp, Size}, Acc0)
        end,
        orddict:new(),
        TransmissionTS
    ),

    All0 = orddict:fold(
        fun(MessageType, Metrics, Acc0) ->
            lists:foldl(
                fun({Timestamp, Size}, Acc1) ->
                    V = [{ts, Timestamp},
                         {size, [Size]}],
                    orddict:append(MessageType, V, Acc1)
                end,
                Acc0,
                Metrics
            )
        end,
        orddict:new(),
        PerMessageType
    ),

    %% process memory
    All1 = lists:foldl(
        fun({Timestamp, memory, {CRDTSize, RestSize}}, Acc0) ->
            V = [{ts, Timestamp},
                 {size, [CRDTSize, RestSize]}],
            orddict:append(memory, V, Acc0)
        end,
        All0,
        MemoryTS
    ),

    %% process latency
    All2 = orddict:store(latency, Latency, All1),

    FilePath = file_path(node()),
    File = encode(All2),

    store(FilePath, File),
    ok.

-spec push_ping_data() -> ok.
push_ping_data() ->
    Log = get(log),
    ?LOG("Log is ~p ", [Log]),
    
    FilePath = file_path(node()),
    File = encode(Log),
    ?LOG("File is ~p ", [File]),

    store(FilePath, File),
    ok.

%% @private
filter_by_ts_class(Class, TS) ->
    lists:filter(
        fun({_, MClass, _}) ->
                MClass == Class
        end,
        TS
    ).

%% @private
file_path(Name) ->
    Timestamp = trcb_exp_config:get(trcb_exp_timestamp),
    Filename = str(Timestamp) ++ "/"
            ++ str(Name) ++ ".json",
    Filename.

%% @private
get_configs(Vars) ->
    lists:map(
        fun(Var) ->
            Mod = trcb_exp_config,
            {Var, Mod:get(Var)}
        end,
        Vars
    ).

%% @private
str(V) when is_atom(V) ->
    atom_to_list(V);
str(V) when is_integer(V) ->
    integer_to_list(V).

%% @private
store(FilePath, File) ->
    ok = ?STORE:put(FilePath, File).

%% @doc
encode(D) ->
    jsx:encode(D).