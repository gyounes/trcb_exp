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

-module(trcb_exp_experiments).
-author("Georges Younes <georges.r.younes@gmail.com").

-include("trcb_exp.hrl").

-define(KEY, "events").

%% trcb_exp_experiments callbacks
-export([get_specs/1]).

%% @doc
-spec get_specs(atom()) -> [term()].
get_specs(Mode) ->
    Funs = case Mode of
        undefined ->
            [];
        dots ->
            trcb_exp(dots);
        base ->
            trcb_exp(base);
        ping ->
            ping()
    end,

    create_spec(Funs).

%% @private
create_spec(Funs) ->
    case Funs of
        [] ->
            [];
        _ ->
            [{trcb_exp_experiment_runner,
              {trcb_exp_experiment_runner, start_link, [Funs]},
              permanent, 5000, worker, [trcb_exp_experiment_runner]}]
    end.

%% @private
memory() ->
  {0, trcb:tcbmemory()}.

%% @private
trcb_exp(Mode) ->
    StartFun = fun() ->
      {ok, Members} = partisan_peer_service:members(),

      trcb:tcbfullmembership(Members),

      {InitialTag, TagUpdFun} = trcb:tcbgettagdetails(),

      %% gen_server regsters module name with pid
      %% that is why it works instead of trcb_exp_experiment_runner:self()
      trcb:tcbdelivery(trcb_exp_experiment_runner),

      put(delivery, 0),
      put(localTag, InitialTag),
      put(tagUpdFun, TagUpdFun),

      lmetrics:set_time_series_callback(fun() -> ToBeAdded = memory(), {ok, ToBeAdded} end)

    end,

    EventFun = fun(_Arg) ->
        TagUpdFun=get(tagUpdFun),
        LocalTagNew = case Mode of
          dots ->
            TagUpdFun(local, get(localTag));
          base ->
            TagUpdFun(node(), get(localTag))
        end,
        put(localTag, LocalTagNew),
        put(delivery, get(delivery) + 1),
        trcb:tcbcast(msg, LocalTagNew)
    end,

    TotalEventsFun = fun() ->
        get(delivery)
    end,

    CheckEndFun = fun(NodeNumber, NodeEventNumber) ->
        TheoTot = NodeNumber * NodeEventNumber,
        PracTotDelv = TotalEventsFun(),
        PracTotDelv == TheoTot
    end,

    HandleInfoFun = fun({delivery, A, B, _C}) ->
        TagUpdFun=get(tagUpdFun),
        put(delivery, get(delivery) + 1),
        LocalTagNew = case Mode of
          dots ->
            TagUpdFun({A, B}, get(localTag));
          base ->
            TagUpdFun(A, get(localTag))
        end,
        put(localTag, LocalTagNew)
    end,

    [StartFun,
     EventFun,
     TotalEventsFun,
     CheckEndFun,
     HandleInfoFun].

%% @private
ping() ->
    StartFun = fun() ->
      {ok, Members} = partisan_peer_service:members(),

      pingserv:fullmembership(Members),

      pingserv:setreply(trcb_exp_experiment_runner),

      put(log, orddict:new()),
      put(ctr, 0)

    end,

    EventFun = fun(_Arg) ->
        Index = get(ctr),
        put(ctr, Index+1),
        Log = get(log),
        T0=trcb_exp_util:generate_timestamp(microsecond),
        pingserv:ping(Index),
        put(log, orddict:store(Index, T0, Log))
    end,

    TotalEventsFun = fun() ->
        Log = get(log),
        orddict:size(Log)
    end,

    CheckEndFun = fun(_, NodeEventNumber) ->
      Val = NodeEventNumber == TotalEventsFun(),
      case Val of
        true ->
          Log = get(log),
          pingserv:push_metrics(Log),
          Metrics = pingserv:get_metrics(),
          ?LOG("Metrics are ~p", [Metrics]),
          Val;
        false ->
          Val
      end
    end,

    HandleInfoFun = fun(Index) ->
        T1=trcb_exp_util:generate_timestamp(microsecond),
        Log = get(log),
        T0 = orddict:fetch(Index, Log),
        put(log, orddict:store(Index, (T1-T0)/math:pow(10, 3), Log))
    end,

    [StartFun,
     EventFun,
     TotalEventsFun,
     CheckEndFun,
     HandleInfoFun].