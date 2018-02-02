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

-module(trcb_exp_metrics_store).
-author("Georges Younes <georges.r.younes@gmail.com").

-include("trcb_exp.hrl").

-export([start_link/0,
         put/2]).

%% @doc Start the store.
-callback start_link() -> {ok, pid()} | ignore | {error, term()}.

%% @doc Stores some `value()' associated with some `key()'.
-callback put(key(), value()) -> ok.

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    do(start_link, []).

-spec put(key(), value()) -> ok.
put(Key, Value) ->
    do(put, [Key, Value]).

%% @private
do(Function, Args) ->
    Store = trcb_exp_config:get(trcb_exp_metrics_store),
    case Store of
        redis ->
            erlang:apply(trcb_exp_redis_metrics_store, Function, Args)
    end.
