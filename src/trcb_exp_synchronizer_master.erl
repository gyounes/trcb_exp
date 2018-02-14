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

-module(trcb_exp_synchronizer_master).
-author("Georges Younes <georges.r.younes@gmail.com").

-include("trcb_exp.hrl").

-behaviour(gen_server).

%% trcb_exp_synchronizer_master callbacks
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {nodes :: list(node_spec()),
                connect_done:: ordsets:ordset(trcb_exp_node_id()),
                exp_done :: ordsets:ordset(trcb_exp_node_id()),
                metrics_done :: ordsets:ordset(trcb_exp_node_id()),
                start_time :: timestamp()}).

-define(BARRIER_PEER_SERVICE, trcb_exp_barrier_peer_service).
-define(INTERVAL, 3000).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    schedule_create_barrier(),
    ?LOG("trcb_exp_synchronizer_master initialized"),
    {ok, #state{nodes=[],
                connect_done=ordsets:new(),
                exp_done=ordsets:new(),
                metrics_done=ordsets:new(),
                start_time=0}}.

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast({connect_done, NodeName},
            #state{connect_done=ConnectDone0,
                   start_time=T0}=State) ->

    ?LOG("Received CONNECT DONE from ~p", [NodeName]),

    ConnectDone1 = ordsets:add_element(NodeName, ConnectDone0),

    T1 = case ordsets:size(ConnectDone1) == node_number() of
        true ->
            ?LOG("Everyone is CONNECT DONE. SIM GO!"),
            tell(exp_go),
            unix_timestamp();
        false ->
            T0
    end,

    {noreply, State#state{connect_done=ConnectDone1,
                          start_time=T1}};

handle_cast({exp_done, NodeName},
            #state{exp_done=ExpDone0}=State) ->

    ?LOG("Received SIM DONE from ~p", [NodeName]),

    ExpDone1 = ordsets:add_element(NodeName, ExpDone0),

    case ordsets:size(ExpDone1) == node_number() of
        true ->
            ?LOG("Everyone is SIM DONE. METRICS GO!"),
            tell(metrics_go);
        false ->
            ok
    end,

    {noreply, State#state{exp_done=ExpDone1}};

handle_cast({metrics_done, NodeName},
            #state{metrics_done=MetricsDone0,
                   start_time=StartTime}=State) ->

    ?LOG("Received METRICS DONE from ~p", [NodeName]),

    MetricsDone1 = ordsets:add_element(NodeName, MetricsDone0),

    case ordsets:size(MetricsDone1) == node_number() of
        true ->
            ?LOG("Everyone is METRICS DONE. STOP!!!"),
            trcb_exp_experiments_support:push_trcb_exp_metrics(StartTime),
            trcb_exp_orchestration:stop_tasks([trcb_exp, synchronizer]);
        false ->
            ok
    end,

    {noreply, State#state{metrics_done=MetricsDone1}};

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(create_barrier, State) ->
    Nodes = trcb_exp_orchestration:get_tasks(trcb_exp, ?BARRIER_PORT, true),

    case length(Nodes) == node_number() of
        true ->
            ok = connect(Nodes);
        false ->
            schedule_create_barrier()
    end,
    {noreply, State#state{nodes=Nodes}};

handle_info(Msg, State) ->
    lager:warning("Unhandled info message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
node_number() ->
    trcb_exp_config:get(trcb_exp_node_number).

%% @private
schedule_create_barrier() ->
    timer:send_after(?INTERVAL, create_barrier).

%% @private
connect([]) ->
    ok;
connect([Node|Rest]=All) ->
    case ?BARRIER_PEER_SERVICE:join(Node) of
        ok ->
            connect(Rest);
        Error ->
            ?LOG("Couldn't connect to ~p. Reason ~p. Will try again in ~p ms",
                 [Node, Error, ?INTERVAL]),
            timer:sleep(?INTERVAL),
            connect(All)
    end.

%% @private send to all
tell(Msg) ->
    {ok, Members} = ?BARRIER_PEER_SERVICE:members(),
    tell(Msg, without_me(Members)).

%% @private send to some
tell(Msg, Peers) ->
    lists:foreach(
        fun(Peer) ->
            ?BARRIER_PEER_SERVICE:forward_message(
               Peer,
               trcb_exp_synchronizer,
               Msg
            )
        end,
        Peers
     ).

%% @private
without_me(Members) ->
    Members -- [node()].

unix_timestamp() ->
    {Mega, Sec, _Micro} = erlang:timestamp(),
    Mega * 1000000 + Sec.