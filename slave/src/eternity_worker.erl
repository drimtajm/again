%%%-------------------------------------------------------------------
%%% @author drimtajm
%%% @copyright (C) 2018
%%% @doc
%%%
%%% @end
%%% Created :  9 Aug 2018 by drimtajm
%%%-------------------------------------------------------------------
-module(eternity_worker).

-behaviour(gen_fsm).

%% API
-export([start_link/1, edge_puzzler/5, example/0]).

%% gen_fsm callbacks
-export([init/1, idle/2, busy/2, idle/3, busy/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).
-define(corners_file, "../../data/corners.data").
-define(edges_file, "../../data/edges.data").
-define(inner_pieces_file, "../../data/inner_pieces.data").

-record(state, {name, corners, edges, inner_pieces, child_process, time}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name) ->
    gen_fsm:start_link({local, Name}, ?MODULE, Name, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init(Name) ->
    process_flag(trap_exit, true),
    {ok, Corners} = file:consult(?corners_file),
    {ok, Edges} = file:consult(?edges_file),
    {ok, InnerPieces} = file:consult(?inner_pieces_file),
    %% TODO: send request for registration
    %%       if unsucessful, return {stop, registration_failed}
    %% TODO: send request for dataset
    {ok, idle, #state{name = Name,
		      corners = Corners,
		      edges = Edges,
		      inner_pieces = InnerPieces,
		      child_process = no_value}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
idle(_Event, State) ->
    {next_state, idle, State}.

busy(no_result_found, State) ->
    %% TODO: send back result, request a new dataset
    io:format("No result found"),
    {next_state, idle, State#state{child_process = no_value}};
busy({result, Result}, State) ->
    %% TODO: send back result, request a new dataset
    Time2 = erlang:system_time(),
    Time1 = State#state.time,
    {PieceSubset, Solutions} = Result,
    io:format("Got result: ~w patterns~nTook: ~w ms~n",
	      [length(Solutions), ((Time2 - Time1) div 1000000)]),
    lists:foreach(fun (S) ->
			  io:format("~w~n", [S])
		  end, Solutions),
    {next_state, idle, State#state{child_process = no_value}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
idle({dataset, Dataset}, _From, State) ->
    Time = erlang:system_time(),
    ChildProcess = start_child_process(Dataset, State),
    {reply, ok, busy, State#state{child_process = ChildProcess, time = Time}};
idle(Event, _From, State) ->
    {reply, syntax_error, idle, State}.

busy(Event, _From, State) ->
    {reply, busy, busy, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(stop, StateName, State) ->
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
example() ->
    {ok, _Pid} = start_link(worker),
%%    PieceSubset = encode_piece_subset([1, 5, 7, 29, 32]),
%%    PieceSubset = encode_piece_subset([1, 5, 6, 7, 8, 11, 15, 20,
%%				       22, 24, 25, 26, 27, 28, 29]),
    PieceSubset = encode_piece_subset([1, 5, 6, 7, 8, 9, 10, 11,
				       12, 13, 14, 15, 16, 17, 18]),
    Dataset = [{piece_limit, 7}, {mission_type, puzzle_edge},
%%	       {piece_subset, {{corner, 1}, {edges, [5, 7, 29, 32]}}}],
%%	       {piece_subset, {{corner, 1}, {edges, [15, 20, 34, 41]}}}],
	       {piece_subset, PieceSubset}],
    Reply = gen_fsm:sync_send_event(worker, {dataset, Dataset}),
    io:format("Got reply: ~w~n", [Reply]),
    timer:sleep(10000),
    gen_fsm:send_all_state_event(worker, stop).

keyfind(Key, List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Value}} -> Value;
	false                 -> no_value
    end.

start_child_process(Dataset, State) ->
    PieceLimit = keyfind(piece_limit, Dataset),
    MissionType = keyfind(mission_type, Dataset),
    PieceSubset = keyfind(piece_subset, Dataset),
    case MissionType of
	puzzle_edge ->
	    {{corners, [CornerId]}, {edges, EdgeIds}, {inner_pieces, []}} = 
		decode_piece_subset(PieceSubset),
	    Corner = keyfind(CornerId, State#state.corners),
	    Edges = lists:map(fun (EdgeId) ->
				      keyfind(EdgeId, State#state.edges)
			      end, EdgeIds),
	    spawn_link(?MODULE, edge_puzzler, [State#state.name, Corner, Edges,
					       PieceLimit, PieceSubset]);
	_Else ->
	    not_implemented_yet
    end.

encode_piece_subset({{corners, Corners}, {edges, Edges},
		     {inner_pieces, InnerPieces}}) ->
    encode_piece_subset(Corners ++ Edges ++ InnerPieces);
encode_piece_subset(Pieces) ->
    lists:foldl(fun (Piece, PieceSubset) ->
			PieceSubset bor (1 bsl (256 - Piece))
		end, 0, Pieces).
    
decode_piece_subset(PieceSubset) ->
    Pieces = decode_piece_subset(PieceSubset, 256, []),
    Corners = [Piece || Piece <- Pieces, Piece =< 4],
    Edges = [Piece || Piece <- Pieces, Piece > 4, Piece =< 48],
    InnerPieces = [Piece || Piece <- Pieces, Piece > 48],
    {{corners, Corners}, {edges, Edges}, {inner_pieces, InnerPieces}}.

decode_piece_subset(_, 0, Result) ->    
    Result;
decode_piece_subset(PieceSubset, Bit, Result) ->
    NewResult = case PieceSubset band 2#1 of
		    0 -> Result;
		    1 -> [Bit | Result]
		end,
    decode_piece_subset(PieceSubset bsr 1, Bit - 1, NewResult).

edge_puzzler(FsmRef, Corner, Edges, PieceLimit, PieceSubset) ->
    case edge_search(Corner, Edges, PieceLimit) of
	no_result_found ->
	    gen_fsm:send_event(FsmRef, no_result_found);
	Result ->
	    io:format(".~n"),
	    gen_fsm:send_event(FsmRef, {result, {PieceSubset, 
						 lists:usort(Result)}})
    end.

convert_solution({Left, Right}) ->
    {list_to_binary(lists:map(fun ({_LP, MP, _RP}) -> MP end, Left)),
     list_to_binary(lists:map(fun ({_LP, MP, _RP}) -> MP end,
			      lists:reverse(Right)))}.
    
edge_search(Corner, Edges, PieceLimit) ->
    LeftMatches = match(left, Corner, Edges),
    case edge_search(Corner, Edges, LeftMatches, left,
		     PieceLimit, 0, {[], []}, []) of
	[]     -> no_result_found;
	Result -> lists:map(fun convert_solution/1, Result)
    end.

edge_search(_Corner, [], [], right, PieceLimit, PieceLimit,
	    PartResult, FinalResult) ->
    %% Complete solution, add it to the final result
    [PartResult | FinalResult];
edge_search(Corner, PiecesLeft, _Matches, left, PieceLimit, PieceLimit,
	    PartResult, FinalResult) ->
    %% Done with one direction, change direction
    RightMatches = match(right, Corner, PiecesLeft),
    edge_search(Corner, PiecesLeft, RightMatches, right, PieceLimit, 0,
		PartResult, FinalResult);
edge_search(_Corner, _PiecesLeft, [], _Direction, _PieceLimit, _PieceCount,
	    _PartResult, FinalResult) ->
    %% No more matching pieces, discard partial solution
    FinalResult;
edge_search(Corner, PiecesLeft, [First | Rest], Direction, PieceLimit,
	    PieceCount, PartResult, FinalResult) ->
    %% Somewhere in the middle, test the first match in given direction
    NewPiecesLeft = lists:delete(First, PiecesLeft),
    NewMatches = match(Direction, First, NewPiecesLeft),
    NewPartResult = insert_piece(Direction, First, PartResult),
    NewFinalResult = edge_search(Corner, NewPiecesLeft, NewMatches, Direction,
				 PieceLimit, PieceCount + 1, NewPartResult,
				 FinalResult),
    edge_search(Corner, PiecesLeft, Rest, Direction, PieceLimit, PieceCount,
		PartResult, NewFinalResult).
    

match(left, {LeftPattern, _RightPattern}, Edges) ->
    [{LeftP, InnerP, RightP} || {LeftP, InnerP, RightP} <- Edges,
				RightP == LeftPattern];
match(left, {LeftPattern, _InnerPattern, _RightPattern}, Edges) ->
    [{LeftP, InnerP, RightP} || {LeftP, InnerP, RightP} <- Edges,
				RightP == LeftPattern];
match(right, {_LeftPatter, RightPattern}, Edges) ->
    [{LeftP, InnerP, RightP} || {LeftP, InnerP, RightP} <- Edges,
				LeftP == RightPattern];
match(right, {_LeftPatter, _InnerPattern, RightPattern}, Edges) ->
    [{LeftP, InnerP, RightP} || {LeftP, InnerP, RightP} <- Edges,
				LeftP == RightPattern];
match(_, _, _) ->
    %% TODO: match inner pieces
    [].

insert_piece(left, Piece, {Left, Right}) ->
    {[Piece | Left], Right};
insert_piece(right, Piece, {Left, Right}) ->
    {Left, [Piece | Right]}.

%% [1, 5, 7, 29, 32].

    
