-define(APP, trcb_exp).
-type error() :: {error, atom()}.

%% peer service
-type trcb_exp_node_id() :: node().
-type node_ip() :: inet:ip_address().
-type node_port() :: non_neg_integer().
-type node_spec() :: {trcb_exp_node_id(), node_ip(), node_port()}.
-type handler() :: term(). %% module
-type message() :: term().
-type timestamp() :: non_neg_integer().

%% defaults
-define(DEFAULT_MODE, base).
-define(DEFAULT_OVERLAY, fullmesh).
-define(DEFAULT_LATENCY, 0).


%% logging
-ifdef(debug).
-define(LOG(M), lager:info(M)).
-define(LOG(M, A), lager:info(M, A)).
-else.
-define(LOG(_M), ok).
-define(LOG(_M, _A), ok).
-endif.

% -define(LOG(M), lager:info(M)).
% -define(LOG(M, A), lager:info(M, A)).

%% barrier
-define(PORT, 6866).
-define(BARRIER_PORT, 6867).
-define(REDIS_PORT, 6379).
-define(TCP_OPTIONS, [binary, {active, true}, {packet, 4}, {keepalive, true}, {nodelay, true}]).

-define(UNIT, millisecond).

%% web config
-define(WEB_IP, "0.0.0.0").
-define(WEB_PORT, 8080).
-define(WEB_CONFIG, [{ip, ?WEB_IP},
                     {port, ?WEB_PORT}]).

%% logs
-type key() :: list().
-type value() :: binary().
