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
  % CalcFunction = fun({ToBeAckQueue, LocalDot, DepDotList, ToBeDelvConcDots, DepGraph}) ->
  %         erts_debug:flat_size(ToBeAckQueue)
  %         + erts_debug:flat_size(LocalDot)
  %         + erts_debug:flat_size(DepDotList)
  %         + erts_debug:flat_size(ToBeDelvConcDots)
  %         + erts_debug:flat_size(DepGraph)
  %       end,

  CalcFunction = fun(L) ->
    lists:foldl(fun(X, Sum) -> erlang:byte_size(erlang:term_to_binary(X)) + Sum end, 0, L)
  end,

  {0, trcb:tcbmemory(CalcFunction)}.

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

    HandleCastFun = fun({delivery, A, B, _C}) ->
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
     HandleCastFun].

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
        T0=trcb_exp_util:generate_timestamp(microsecond, 3),
        pingserv:ping(Index),
        put(ctr, Index+1),
        Log = get(log),
        put(log, orddict:store(Index, T0, Log))
    end,

    TotalEventsFun = fun() ->
        Log = get(log),
        orddict:size(Log)
    end,

    CheckEndFun = fun(NodeEventNumber) ->
      NodeEventNumber == TotalEventsFun()
      % Val = NodeEventNumber == TotalEventsFun(),
      % case Val of
      %   true ->
      %     Log = get(log),
      %     ?LOG("Log is ~p", [Log]),
      %     Val;
      %   false ->
      %     Val
      % end
    end,

    HandleCastFun = fun(Index) ->
        T1=trcb_exp_util:generate_timestamp(microsecond, 3),
        Log = get(log),
        T0 = orddict:fetch(Index, Log),
        put(log, orddict:store(Index, (T1-T0), Log))
    end,

    [StartFun,
     EventFun,
     TotalEventsFun,
     CheckEndFun,
     HandleCastFun].