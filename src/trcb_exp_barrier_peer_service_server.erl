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

-module(trcb_exp_barrier_peer_service_server).
-author("Georges Younes <georges.r.younes@gmail.com").

-include("trcb_exp.hrl").

-behaviour(gen_server).

%% trcb_exp_barrier_peer_service_server callbacks
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {listener :: gen_tcp:socket()}).

-spec start_link(node_port()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

%% gen_server callbacks
init([Port]) ->
    {ok, Listener} = gen_tcp:listen(Port, ?TCP_OPTIONS),

    prepare_accept(),

    ?LOG("trcb_exp_barrier_peer_service_server initialized"),
    {ok, #state{listener=Listener}}.

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast(accept, #state{listener=Listener}=State) ->
    {ok, Socket} = gen_tcp:accept(Listener),

    {ok, Pid} = trcb_exp_barrier_peer_service_client:start_link(Socket),
    gen_tcp:controlling_process(Socket, Pid),

    prepare_accept(),

    {noreply, State};

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    lager:warning("Unhandled info message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
prepare_accept() ->
    gen_server:cast(?MODULE, accept).
