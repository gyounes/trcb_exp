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
                trcb_exp_drop_ratio,
                trcb_exp_latency,
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

    Memory = ?LMETRICS:get_memory(),
    lager:info("Memory is ~p", [dict:to_list(Memory)]),

    Processing = ?LMETRICS:get_processing(),
    lager:info("Processing is ~p", [dict:to_list(Processing)]),

    Transmission = ?LMETRICS:get_transmission(),
    lager:info("Transmission to list is ~p", [dict:to_list(Transmission)]),

    %% get transmission
    TransmissionDict = dict:fold(
        fun(MessageType, Metrics, Acc0) ->
            lists:foldl(
                fun({Timestamp, Size}, Acc1) ->
                    V = [{ts, Timestamp},
                         {val, Size}],
                    case orddict:find(MessageType, Acc1) of
                        {ok, L} ->
                            orddict:store(MessageType, [V|L], Acc1);
                        error ->
                            orddict:store(MessageType, [V], Acc1)
                    end
                end,
                Acc0,
                lists:reverse(Metrics)
            )
        end,
        orddict:new(),
        Transmission
    ),

    %% get processing
    ProcessingDict = dict:fold(
        fun(Type, Metrics, Acc0) ->
            lists:foldl(
                fun({Timestamp, Time}, Acc1) ->
                    V = [{ts, Timestamp},
                         {val, Time}],
                    case orddict:find(Type, Acc1) of
                        {ok, L} ->
                            orddict:store(Type, [V|L], Acc1);
                        error ->
                            orddict:store(Type, [V], Acc1)
                    end
                end,
                Acc0,
                lists:reverse(Metrics)
            )
        end,
        orddict:new(),
        Processing
    ),

    %% get memory
    MemoryDict = dict:fold(
        fun(Type, Metrics, Acc0) ->
            lists:foldl(
                fun({Timestamp, Size}, Acc1) ->
                    V = [{ts, Timestamp},
                         {val, Size}],
                    case orddict:find(Type, Acc1) of
                        {ok, L} ->
                            orddict:store(Type, [V|L], Acc1);
                        error ->
                            orddict:store(Type, [V], Acc1)
                    end
                end,
                Acc0,
                lists:reverse(Metrics)
            )
        end,
        orddict:new(),
        Memory
    ),

    All0 = orddict:store(transmission, TransmissionDict, orddict:new()),
    All1 = orddict:store(processing, ProcessingDict, All0),
    All = orddict:store(memory, MemoryDict, All1),

    FilePath = file_path(node()),

    File = encode(All),

    store(FilePath, File),

    lager:info("metrics pushed successfully"),

    ok.

-spec push_ping_data() -> ok.
push_ping_data() ->
    Log = pingserv:get_metrics(),
    ?LOG("Log is ~p ", [Log]),

    FilePath = file_path(node()),
    File = encode(Log),
    ?LOG("File is ~p ", [File]),

    store(FilePath, File),
    ok.

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