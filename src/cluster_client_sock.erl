%%  Copyright (C) 2011 - Molchanov Maxim,
%% @author Maxim Molchanov <elzor.job@gmail.com>

-module(cluster_client_sock).
-behavior(gen_server).

-export([start/0, start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([
    
    ]).

-define(TCP_OPTIONS, [binary, {packet, 0}]). %[binary, {packet, 0}, {keepalive, true}, {backlog, 30}, {active, false}, {reuseaddr, true}]

-record( state, {   
                    timer,
                    init_time=-1
                }).

%% Public API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop(Module) ->
  gen_server:call(Module, stop, infinity).

stop() ->
  stop(?MODULE).

state(Module) ->
  gen_server:call(Module, state, infinity).

state() ->
  state(?MODULE).

%% Server implementation, a.k.a.: callbacks
init([]) ->
  State = #state{
    timer     = erlang:send_after(1, self(), update_cluster_connection),
    init_time = utils:unix_timestamp()
  },
  {ok, State}.


handle_call(_Request, _From, State) ->
  say("call ~p, ~p, ~p.", [_Request, _From, State]),
  {reply, ok, State}.


handle_cast(_Msg, State) ->
  say("cast ~p, ~p.", [_Msg, State]),
  {noreply, State}.

handle_info(update_cluster_connection, State) ->
  erlang:cancel_timer(State#state.timer),
  {ok, Addr}  = config:get(master_node_addr),
  {ok, Port} = config:get(master_node_port),
  case gen_tcp:connect(Addr, Port, ?TCP_OPTIONS, 60000) of
    {ok, Sock} ->
        gen_tcp:send(Sock, term_to_binary({<<"in">>, {atom_to_binary(node(),utf8),utils:unix_timestamp()}})),
        gen_tcp:close(Sock);
    _Else->
        error_logger:error_report("Cluster master node not avaliable!")
  end,
  {noreply, State#state{timer = erlang:send_after(5000, self(), update_cluster_connection)}};


handle_info(_Info, State) ->
  say("info ~p, ~p.", [_Info, State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  say("terminate ~p, ~p", [_Reason, _State]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  say("code_change ~p, ~p, ~p", [_OldVsn, State, _Extra]),
  {ok, State}.

%% Some helper methods.

say(Format) ->
  say(Format, []).
say(Format, Data) ->
  io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).