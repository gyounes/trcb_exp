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

-module(trcb_exp_sup).
-author("Georges Younes <georges.r.younes@gmail.com").

-include("trcb_exp.hrl").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type, Timeout),
        {I, {I, start_link, []}, permanent, Timeout, Type, [I]}).
-define(CHILD(I), ?CHILD(I, worker, 5000)).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    configure_peer_service(),
    {Mode, Orchestration, Synchronizer} = configure(),

    Children = trcb_exp_specs(Mode, Orchestration, Synchronizer),

    ?LOG("trcb_exp_sup initialized!"),
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.

%% @private
configure_peer_service() ->

    %% configure trcb overlay
    _Overlay = configure_var("OVERLAY",
                            trcb_exp_overlay,
                            ?DEFAULT_OVERLAY),

    PeerService = partisan_default_peer_service_manager,

    %% configure partisan manager
    partisan_config:set(partisan_peer_service_manager,
                        PeerService).

%% @private
configure() ->
    %% configure trcb mode
    Mode = configure_var("MODE",
                               trcb_exp_mode,
                               ?DEFAULT_MODE),

    %% configure node number
    configure_int("NODE_NUMBER",
                  trcb_exp_node_number,
                  1),

    %% configure node event number
    configure_int("NODE_EVENT_NUMBER",
                  trcb_exp_node_event_number,
                  30),

    %% configure default event interval
    configure_int("DEFAULT_EVENT_INTERVAL",
                  trcb_exp_default_event_interval,
                  1000),

    %% configure unique experiement timestamp
    configure_int("TIMESTAMP",
                  trcb_exp_timestamp,
                  0),

    %% configure api server
    configure_str("APISERVER",
                  trcb_exp_api_server,
                  undefined),

    %% configure auth token
    configure_str("TOKEN",
                  trcb_exp_token,
                  undefined),

    %% configure orchestration
    Orchestration = configure_var("ORCHESTRATION",
                                  trcb_exp_orchestration,
                                  undefined),

    %% configure Synchronizer master
    Synchronizer = configure_var("SYNCHRONIZER",
                        trcb_exp_synchronizer,
                        false),

    %% configure metrics store
    configure_var("METRICS_STORE",
                  trcb_exp_metrics_store,
                  undefined),

    {Mode, Orchestration, Synchronizer}.

%% @private
trcb_exp_specs(Experiment, Orchestration, Synchronizer) ->
    ExperimentSpecs = trcb_exp_experiments:get_specs(Experiment),

    OrchestrationSpecs = case Orchestration of
        undefined ->
            [];
        _ ->
            BarrierPeerServiceSpecs = [?CHILD(trcb_exp_barrier_peer_service)],
            Store = [?CHILD(trcb_exp_metrics_store)],

            SynchronizerSpecs = case Synchronizer of
                true ->
                    [?CHILD(trcb_exp_synchronizer_master)];
                false ->
                    [?CHILD(trcb_exp_synchronizer)]
            end,

            HTTPSpecs = case Synchronizer of
                true ->
                    [];
                false ->
                    [?CHILD(trcb_exp_resource)]
            end,

            BarrierPeerServiceSpecs ++ Store ++ SynchronizerSpecs ++ HTTPSpecs
    end,

    ExperimentSpecs ++ OrchestrationSpecs.

%% @private
configure_var(Env, Var, Default) ->
    To = fun(V) -> atom_to_list(V) end,
    From = fun(V) -> list_to_atom(V) end,
    configure(Env, Var, Default, To, From).

%% @private
configure_str(Env, Var, Default) ->
    F = fun(V) -> V end,
    configure(Env, Var, Default, F, F).

%% @private
configure_int(Env, Var, Default) ->
    To = fun(V) -> integer_to_list(V) end,
    From = fun(V) -> list_to_integer(V) end,
    configure(Env, Var, Default, To, From).

%% @private
configure(Env, Var, Default, To, From) ->
    Current = trcb_exp_config:get(Var, Default),
    Val = From(
        os:getenv(Env, To(Current))
    ),
    trcb_exp_config:set(Var, Val),
    Val.
