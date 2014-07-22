-module(server).
-export([listen/1, accept/1, loop/2, start/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
    spawn(?MODULE,listen,[Port]).

% Call echo:listen(Port) to start the service.
listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).

% Wait for incoming connections and spawn the echo loop when we get one.
accept(LSocket) ->
    io:format("running accept~n"),
    case gen_tcp:accept(LSocket) of
        {ok, Socket} -> 
            io:format("accepting client~n"),
            gen_tcp:send(Socket, <<"Welcome to the chat server!\r\n">>),
            gen_tcp:send(Socket, <<"Please start by selecting a nickname: ">>),
            {ok, Nickname} = gen_tcp:recv(Socket, 0),
            io:format("~w connected~n",[Nickname]),
            spawn(fun() -> accept(LSocket) end),
            loop(Socket, Nickname);
        Other ->
            io:format("accept returned ~w, later", [Other]),
            ok
    end.

% Echo back whatever data we receive on Socket.
loop(Socket, Nickname) ->
    inet:setopts(Socket,[{active,once}]),
    receive
        code_switch ->
            io:format("Hot loading code!~n"),
            server:loop(Socket, Nickname),
            ok;
        {tcp,Socket,<<"quit\r\n">>} ->
            gen_tcp:send(Socket, <<"Bye !">>),
            exit("Client quit");
        {tcp,Socket,<<"/nick\r\n">>} ->
            gen_tcp:send(Socket, Nickname),
            loop(Socket, Nickname); 
        {tcp,Socket,<<"Fuck off\r\n">>} ->
            gen_tcp:send(Socket, <<"No need for such language!\r\n">>),
            loop(Socket, Nickname);
        {tcp,Socket,Data} ->
            gen_tcp:send(Socket,Data),
            loop(Socket, Nickname);
        {tcp_closed,Socket} ->
            io:format("Socket ~w closed [~w]~n",[Socket,self()]),
            ok
    end.
