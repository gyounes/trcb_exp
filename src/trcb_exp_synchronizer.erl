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

-module(trcb_exp_synchronizer).
-author("Georges Younes <georges.r.younes@gmail.com").

-include("trcb_exp.hrl").

-behaviour(gen_server).

%% trcb_exp_synchronizer callbacks
-export([start_link/0,
         experiment_end/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {number_of_rules :: non_neg_integer()}).

-define(BARRIER_PEER_SERVICE, trcb_exp_barrier_peer_service).
-define(PEER_SERVICE, trcb_exp_peer_service).
-define(INTERVAL, 3000).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec experiment_end() -> ok.
experiment_end() ->
    gen_server:call(?MODULE, experiment_end, infinity).

%% gen_server callbacks
init([]) ->
    schedule_create_barrier(),

    ?LOG("trcb_exp_synchronizer initialized"),
    {ok, #state{number_of_rules=0}}.

handle_call(experiment_end, _From, State) ->
    tell({exp_done, node()}),
    {reply, ok, State};

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast(exp_go, State) ->
    ?LOG("Received EXP GO. Starting experiment."),
    trcb_exp_experiment_runner:start(),
    {noreply, State};

handle_cast(metrics_go, State) ->
    ?LOG("Received METRICS GO. Pushing metrics."),
    trcb_exp_experiments_support:push_lmetrics(),
    tell({metrics_done, node()}),
    {noreply, State};

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(create_barrier, State) ->
    case trcb_exp_orchestration:get_task(synchronizer, ?BARRIER_PORT, true) of
        {ok, Synchronizer} ->
            ok = connect([Synchronizer], ?BARRIER_PEER_SERVICE),
            schedule_join_peers();
        {error, not_connected} ->
            schedule_create_barrier()
    end,

    {noreply, State};

handle_info(join_peers, State) ->
    MyName = node(),
    Nodes = trcb_exp_orchestration:get_tasks(exp, ?PORT, true),
    Overlay = trcb_exp_config:get(trcb_exp_overlay),

    case length(Nodes) == node_number() of
        true ->
            %% if all nodes are connected
            ToConnect = trcb_exp_overlay:to_connect(MyName,
                                                Nodes,
                                                Overlay),
            ok = connect(ToConnect, ?PEER_SERVICE),
            tell({connect_done, node()});
        _ ->
            schedule_join_peers()
    end,
    {noreply, State};

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
schedule_join_peers() ->
    timer:send_after(?INTERVAL, join_peers).

%% @private
connect([], _) ->
    ok;
connect([Node|Rest]=All, PeerService) ->
    case PeerService:join(Node) of
        ok ->
            connect(Rest, PeerService);
        Error ->
            ?LOG("Couldn't connect to ~p. Reason ~p. Will try again in ~p ms",
                 [Node, Error, ?INTERVAL]),
            timer:sleep(?INTERVAL),
            connect(All, PeerService)
    end.

%% @private
tell(Msg) ->
    {ok, Members} = ?BARRIER_PEER_SERVICE:members(),
    lists:foreach(
        fun(Peer) ->
            ?BARRIER_PEER_SERVICE:forward_message(
               Peer,
               trcb_exp_synchronizer_master,
               Msg
            )
        end,
        without_me(Members)
     ).

%% @private
without_me(Members) ->
    Members -- [node()].
