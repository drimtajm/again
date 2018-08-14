%% coding: utf-8
%%%-------------------------------------------------------------------
%%% @author  drimtajm
%%% @copyright (C) 2016, 
%%% @doc     Matrix contoller server
%%%
%%% @end
%%% Created : 23 Jan 2016 by drimtajm
%%%-------------------------------------------------------------------
-module(code_receiver).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(FILENAME, "../led_matrix_code.bin").
-define(TIMEOUT, infinity).

-record(state, {bindings}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, {close, undefined}, ?TIMEOUT).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    matrix_display:clear(),
    Bindings0=erl_eval:new_bindings(),
    Bindings1=erl_eval:add_binding('Visa', fun matrix_display:display/2,
				   Bindings0),
    Bindings2=erl_eval:add_binding('Animera', fun matrix_display:animate/2,
				   Bindings1),

    {ok, #state{bindings=Bindings2}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({code, Code}, _From, State) ->
    io:format("Received code.~n"),
    Reply = handle_code(Code, State),
    file:write_file(get_filename_with_path(?FILENAME), Code),
    {reply, Reply, State};
handle_call(go, From, State) ->
    io:format("Reading save file (~p)...", [?FILENAME]),
    case file:read_file(get_filename_with_path(?FILENAME)) of
	{error, enoent} ->
	    io:format("error, not found.~n"),
	    {reply, noop, State};
	{ok, Bin} ->
	    io:format("done.~n"),
	    Reply = handle_code(binary_to_list(Bin), State),
	    gen_server:reply(From, Reply),
	    {ok, NextPi} = application:get_env(matrix_controller, next_pi),
	    case NextPi of
		undefined ->
		    io:format("Finished!~n"),
		    done;
		_Else ->
		    io:format("Handing over to: ~w~n", [NextPi]),
		    RemoteReply = gen_server:call({?SERVER,
						   get_node_name(NextPi)}, go,
						  ?TIMEOUT),
		    io:format("Remote node replied: ~w~n", [RemoteReply])
	    end,
	    {noreply, State}
    end;
handle_call({close, undefined}, _From, State) ->
    io:format("Received close request.~n"),
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_code(Code, #state{bindings=Bindings}) ->
    {ok,ErlTokens,_}=erl_scan:string(Code),

    %% Now parse the tokens into the abstract form
    {ok,ErlAbsForm}=erl_parse:parse_exprs(ErlTokens),

    %% Now evaluate the string
    erl_eval:exprs(ErlAbsForm,Bindings),
    matrix_display:clear(),
    ok.

get_node_name(RemoteHost) ->
    list_to_atom(lists:concat([atom_to_list(led_server), "@",
			       atom_to_list(RemoteHost)])).

get_filename_with_path(Filename) ->
    filename:join(filename:dirname(code:which(?MODULE)), Filename).
