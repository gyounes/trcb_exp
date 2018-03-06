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

-module(trcb_exp_util).
-author("Georges Younes <georges.r.younes@gmail.com").

-include("trcb_exp.hrl").

-export([generate_spec/2,
generate_timestamp/1]).

%% @doc Given an IP string and port string
%%      genenerate the node spec.
-spec generate_spec(list(), node_port()) -> node_spec().
generate_spec(IpStr, Port) ->
    NameStr = "trcb_exp-" ++ integer_to_list(Port) ++ "@" ++ IpStr,

    ParsedName = list_to_atom(NameStr),
    {ok, ParsedIp} = inet_parse:address(IpStr),

    {ParsedName, ParsedIp, Port}.

%% private
generate_timestamp(Unit) ->
% erlang:system_time(Unit).
erlang:monotonic_time(Unit).
