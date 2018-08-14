%%% @author drimtajm
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 24 Jan 2016 by drimtajm

-module(matrix_controller).

-export([start/0, stop/0]).

start() ->
    application:start(matrix_controller).

stop() ->
    matrix_controller_sup:stop().
