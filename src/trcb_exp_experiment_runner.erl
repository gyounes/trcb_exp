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

-module(trcb_exp_experiment_runner).
-author("Georges Younes <georges.r.younes@gmail.com").

-include("trcb_exp.hrl").

-behaviour(gen_server).

%% trcb_exp_experiment_runner callbacks
-export([start_link/1,
         start/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {event_count :: non_neg_integer(),
                start_fun :: function(),
                event_fun :: function(),
                total_events_fun :: function(),
                check_end_fun :: function(),
                handle_info_fun :: function() | undefined}).

-define(EXPERIMENT_END_INTERVAL, 10000).

-spec start_link([function()]) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(Funs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Funs, []).

-spec start() -> ok.
start() ->
    gen_server:cast(?MODULE, start).

%% gen_server callbacks
init([StartFun, EventFun, TotalEventsFun, CheckEndFun]) ->
    ?LOG("trcb_exp_experiment_runner initialized"),
    {ok, #state{event_count=0,
                start_fun=StartFun,
                event_fun=EventFun,
                total_events_fun=TotalEventsFun,
                check_end_fun=CheckEndFun,
                handle_info_fun=undefined}};
init([StartFun, EventFun, TotalEventsFun, CheckEndFun, HandleInfoFun]) ->
    ?LOG("trcb_exp_experiment_runner handle caster initialized"),
    {ok, #state{event_count=0,
                start_fun=StartFun,
                event_fun=EventFun,
                total_events_fun=TotalEventsFun,
                check_end_fun=CheckEndFun,
                handle_info_fun=HandleInfoFun}}.

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast(start, #state{start_fun=StartFun}=State) ->
    StartFun(),
    timer:sleep(5000),
    schedule_event(),

    {noreply, State};

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(event, #state{event_count=Events0,
                          event_fun=EventFun}=State) ->
    Events = Events0 + 1,
    EventFun(Events),
    %TotalEvents = TotalEventsFun(),
    % ?LOG("Event ~p | Observed ~p | Node ~p", [Events, TotalEvents, ldb_config:id()]),
    ?LOG("Event ~p", [Events]),

    case Events == node_event_number() of
        true ->
            %% If I did all the events I should do
            schedule_experiment_end();
        false ->
            schedule_event()
    end,

    {noreply, State#state{event_count=Events}};

handle_info(experiment_end, #state{check_end_fun=CheckEndFun}=State) ->
    %TotalEvents = TotalEventsFun(),
    % ?LOG("Events observed ~p | Node ~p", [TotalEvents, ldb_config:id()]),
    % ?LOG("Events observed ~p", [TotalEvents]),

    case CheckEndFun(node_number(), node_event_number()) of
        true ->
            %% If everyone did all the events they should do
            ?LOG("All events have been observed"),
            end_experiment();
        false ->
            schedule_experiment_end()
    end,

    {noreply, State};

handle_info({delivery, A, B, C}, #state{handle_info_fun=HandleInfoFun}=State) ->
    HandleInfoFun({delivery, A, B, C}),
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
node_event_number() ->
    trcb_exp_config:get(trcb_exp_node_event_number).

get_next_poisson_distribution() ->
    Rate = 1/trcb_exp_config:get(trcb_exp_default_event_interval),
    round(-math:log(1.0 - rand:uniform())/Rate).

%% @private
schedule_event() ->
    timer:send_after(get_next_poisson_distribution(), event).

%% @private
schedule_experiment_end() ->
    timer:send_after(?EXPERIMENT_END_INTERVAL, experiment_end).

%% @private
end_experiment() ->
    case trcb_exp_config:get(trcb_exp_orchestration) of
        undefined ->
            trcb_exp_config:set(trcb_exp_experiment_end, true);
        _ ->
            trcb_exp_synchronizer:experiment_end()
    end.