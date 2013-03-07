%%  Copyright (C) 2011 - Molchanov Maxim,
%% @author Maxim Molchanov <elzor.job@gmail.com>

-module(cluster_srv_sock).
-export([start/0, start_link/0, loop/1]).

% echo_server specific code

start() ->
	tcp_server_wrapper:start(?MODULE, 7000, {?MODULE, loop}).

start_link() ->
	start().

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            case binary_to_term(Data, [safe]) of
                {<<"in">>, {BinNodeName, _}} ->
                    pool:attach(binary_to_atom(BinNodeName, utf8));
                _Else -> pass
            end,
            loop(Socket);
        {error, closed} ->
            ok
    end.