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

-module(trcb_exp_overlay).
-author("Georges Younes <georges.r.younes@gmail.com").

-include("trcb_exp.hrl").

-export([get/2,
         to_connect/3]).

-spec get(atom(), pos_integer()) -> orddict:orddict().
get(_, 1) ->
    [];
get(fullmesh, N) ->
lists:foldl(
    fun(I, Acc) ->
        Peers = lists:seq(0, I - 1) ++ lists:seq(I+1, N - 1),
        orddict:store(I, Peers, Acc)
    end,
    orddict:new(),
    lists:seq(0, N - 1)
).

%% @doc The first argument is my node spec,
%%      the second argument is a list of node specs,
%%      and the third argument is the overlay.
-spec to_connect(trcb_exp_node_id(), list(node_spec()), atom()) ->
    list(node_spec()).
to_connect(MyName, Nodes, Overlay) ->
    NodeNumber = length(Nodes),

    %% name -> node
    NameToNode = name_to_node_map(Nodes),
    %% {id -> name, id}
    {IdToName, MyId} = id_to_name_map(MyName, NameToNode),
    %% id -> [id]
    Topology = get(Overlay, NodeNumber),

    find_peers(NameToNode, IdToName, MyId, Topology).

%% @private
name_to_node_map(Nodes) ->
    lists:foldl(
        fun({Name, _, _}=Node, Acc) ->
            orddict:store(Name, Node, Acc)
        end,
        orddict:new(),
        Nodes
    ).

%% @private
id_to_name_map(MyName, NameToNode) ->
    {IdToName, MyId, _} = lists:foldl(
        fun({Name, _}, {IdToName0, MyId0, Counter0}) ->
            IdToName1 = orddict:store(Counter0, Name, IdToName0),
            MyId1 = case MyName == Name of
                true ->
                    Counter0;
                false ->
                    MyId0
            end,
            Counter1 = Counter0 + 1,
            {IdToName1, MyId1, Counter1}
        end,
        {orddict:new(), undefined, 0},
        NameToNode
    ),

    {IdToName, MyId}.

%% @private
find_peers(NameToNode, IdToName, MyId, Topology) ->
    %% [id]
    IdsToConnect = orddict:fetch(MyId, Topology),

    %% [node]
    lists:map(
        fun(PeerId) ->
            PeerName = orddict:fetch(PeerId, IdToName),
            orddict:fetch(PeerName, NameToNode)
        end,
        IdsToConnect
    ).