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

-module(trcb_exp_kube_orchestration).
-author("Georges Younes <georges.r.younes@gmail.com").

-include("trcb_exp.hrl").

-behaviour(trcb_exp_orchestration).

-export([get_tasks/3,
         stop_tasks/1]).

-spec get_tasks(atom(), node_port(), boolean()) -> [node_spec()].
get_tasks(Tag, Port, FilterByTimestamp) ->
    ?LOG("in get tasks Tag is ~p Port is ~p FilterByTimestamp is ~p ", [Tag, Port, FilterByTimestamp]),
    Path = pods_path() ++ selector(Tag, FilterByTimestamp),
    case http(get, Path) of
        {ok, Nodes} ->
            generate_nodes(Nodes, Port);
        {error, invalid} ->
            []
    end.

-spec stop_tasks([atom()]) -> ok.
stop_tasks(Tags) ->
    lists:foreach(
        fun(Tag) ->
            case Tag of
                exp ->
                    lists:foreach(
                        fun(Id) ->
                            ok = delete_task(Tag, Id)
                        end,
                        lists:seq(1, trcb_exp_config:get(trcb_exp_node_number)));
                _ ->
                    ok = delete_task(Tag, 0)
            end
        end,
        Tags
    ),
    ok.

%% @private
delete_task(Tag, Id) ->
    Path = case Tag of
            asd ->
                pods_path() ++ selector(Tag, true) ++ ",id%3D" ++ integer_to_list(Id);
            _ ->
                pods_path() ++ selector(Tag, true)
        end,

    Result = case http(get, Path) of
        {ok, _} ->
            DR = http(delete, Path),
            case DR of
                {ok, _} ->
                    ok;
                _ ->
                    error
            end;
        {error, invalid} ->
            error
    end,

    case Result of
        ok ->
            ok;
        error ->
            ?LOG("Delete failed. Trying again in 1 second"),
            timer:sleep(1000),
            delete_task(Tag, Id)
    end.

%% @private
http(Method, Path) ->
    URL = server() ++ Path,
    Headers = headers(),
    run_http(Method, {URL, Headers}).

%% @private
http(Method, Path, Body0) ->
    URL = server() ++ Path,
    Headers = headers(),
    ContentType = "application/json",
    Body1 = binary_to_list(encode(Body0)),
    run_http(Method, {URL, Headers, ContentType, Body1}).

%% @private
run_http(Method, Request) ->
    Options = [{body_format, binary}],

    case httpc:request(Method, Request, [{ssl, [{server_name_indication, disable}]}], Options) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, decode(Body)};
        {error, Reason} ->
            lager:warning("Couldn't process ~p request. Reason ~p",
                 [Method, Reason]),
            {error, invalid}
    end.

%% @private
headers() ->
    Token = trcb_exp_config:get(trcb_exp_token),
    [{"Authorization", "Bearer " ++ Token}].

%% @private
server() ->
    trcb_exp_config:get(trcb_exp_api_server).

%% @private
timestamp() ->
    integer_to_list(trcb_exp_config:get(trcb_exp_timestamp)).

%% @private
pods_path() ->
    "/api/v1/pods".

%% @private
selector(Tag, FilterByTimestamp) ->
    Selector = "?labelSelector=" ++ "tag%3D" ++ atom_to_list(Tag),

    case FilterByTimestamp of
        true ->
            Selector ++ ",timestamp%3D" ++ timestamp();
        false ->
            Selector
    end.

%% @doc
encode(D) ->
    jsx:encode(D).

%% @doc
decode(E) when is_list(E) ->
    decode(list_to_binary(E));
decode(E) when is_binary(E) ->
    Opts = [{labels, atom}, return_maps],
    jsx:decode(E, Opts).

%% @private
generate_nodes(Map, Port) ->
    Items = maps:get(items, Map),
    lists:foldl(
        fun(Item, Nodes) ->
            %% find ip
            Status = maps:get(status, Item),

            case maps:is_key(podIP, Status) of
                true ->
                    IP = binary_to_list(
                        maps:get(podIP, Status)
                    ),
                    Node = trcb_exp_util:generate_spec(IP, Port),
                    [Node | Nodes];
                false ->
                    Nodes
            end
        end,
        [],
        Items
    ).