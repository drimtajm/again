%% coding: utf-8
%%%-------------------------------------------------------------------
%%% @author  drimtajm
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created : 24 Jan 2016 by drimtajm
%%%-------------------------------------------------------------------
-module(matrix_display).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, clear/0, display/2, animate/2]).
%% Internal exports
-export([display/3, animate/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, infinity).
-define(INTERVAL_SLOW, 50).
-define(INTERVAL_MEDIUM, 25).
-define(INTERVAL_FAST, 10).

-record(state, {spi_handle, display_type}).

-record(register_value, {register, value, add = false}).
-record(double_matrix_row, {left_value = 0,
			    right_value = 0}).
-record(colour_matrix_row, {red_value = 0,
			    green_value = 0}).

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
    gen_server:call(?SERVER, stop, ?TIMEOUT).

%% Clears a matrix display
clear() ->
    ok = gen_server:call(?SERVER, clear, ?TIMEOUT).

%% Displays points on a matrix display
display(Points, WaitTime) ->
    call_server(display, Points, WaitTime).

%% Displays points on a matrix display, one point at a time
animate(Points, WaitTime) ->
    call_server(animate, {Points, snabbt}, WaitTime).

call_server(Function, Args, WaitTimeInSeconds) ->
    Reply = gen_server:call(?SERVER, {Function, Args}, ?TIMEOUT),
    WaitTimeInMilliseconds = trunc(WaitTimeInSeconds * 1000),
    timer:sleep(WaitTimeInMilliseconds),
    io:format("display done~n"),
    Reply.

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
    {ok, DisplayType} = application:get_env(matrix_controller, display_type),
    io:format("Current display type is: ~w~n", [DisplayType]),
    {ok, Handle} = spi_interface:open_spi_bus(get_ce_pin(DisplayType)),
    ok = set_initial_register_values(Handle, DisplayType),
    clear(Handle, DisplayType),
    {ok, #state{display_type=DisplayType, spi_handle=Handle}}.

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
handle_call(clear, _From, State) ->
    Reply = clear(State#state.spi_handle, State#state.display_type),
    {reply, Reply, State};
handle_call({Function, Args}, _From,
	    #state{display_type=DisplayType, spi_handle=SpiHandle}=State) ->
    io:format("Preparing to call function: ~w...", [Function]),
    {Points, ExtraArg} = if (is_tuple(Args)) -> Args;
			    true -> {Args, undefined}
			 end,
    PointList = case DisplayType of
		    double_matrix -> Points;
		    colour_matrix ->
			[{Coordinate, get_colour(ColourIndex)}
			 || {Coordinate, ColourIndex} <- Points]
		end,
    Reply = case ExtraArg of
		undefined ->
		    ?MODULE:Function(SpiHandle, DisplayType, PointList);
		_Else ->
		    ?MODULE:Function(SpiHandle,DisplayType,PointList,ExtraArg)
	    end,
    io:format("done~n"),
    {reply, Reply, State};
handle_call(stop, _From, State) ->
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
terminate(_Reason, #state{spi_handle=SpiHandle}) ->
    ok = spi_interface:close_spi_bus(SpiHandle).

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

%% Function display takes a list of points and converts it to
%% picture data which is then displayed on the matrix.
display(SpiHandle, DisplayType, PointList) ->
    EmptyPicture = get_empty_picture(DisplayType),
    Picture = add_points_to_picture(PointList, EmptyPicture),
    display_picture(SpiHandle, Picture).

%% Function animate takes a list of points and displays the pictures
%% resulting from adding one point at a time to the previous picture,
%% starting of course with an empty picture and ending with a complete
%% picture containing all given points.
%% The order of the points is preserved and the partial pictures are
%% shown with an interval of 20 milliseconds.
animate(SpiHandle, DisplayType, PointList, Speed) ->
    EmptyPicture = get_empty_picture(DisplayType),
    lists:foldl(fun (Point, Picture0) ->
			Picture = add_points_to_picture([Point], Picture0),
			display_picture(SpiHandle, Picture),
			case Speed of
			    sakta -> timer:sleep(?INTERVAL_SLOW);
			    mittemellan -> timer:sleep(?INTERVAL_MEDIUM);
			    snabbt -> timer:sleep(?INTERVAL_FAST)
			end,
			Picture
		end, EmptyPicture, PointList).

%% Clear clears the matrix by writing zeroes to all display registers
clear(SpiHandle, colour_matrix) ->
    lists:foreach(fun (N) ->
			  spi_write(SpiHandle, (16#20 + N), 0)
		  end, lists:seq(0, 15)),
    ok;
clear(SpiHandle, double_matrix) ->
    lists:foreach(fun (N) ->
			  spi_write(SpiHandle, N, 0, 0)
		  end, lists:seq(1, 8)),
    ok.

%% This function updates a tuple in a property list through
%% an update function. The value of the element with key
%% "Key" is updated by applying function "Fun" on it.
%% If the key is not found, nothing happens to the list.
keyupdate(Key, Proplist, Fun) ->
    keyupdate(Key, Proplist, Fun, []).
keyupdate(_Key, [], _Fun, SearchedList) ->
    %% key not found, just return the original list
    lists:reverse(SearchedList);
keyupdate(Key, [{Key, Value} | Rest], Fun, SearchedList) ->
    %% key found, update the value
    lists:reverse(SearchedList) ++ [{Key, Fun(Value)} | Rest];
keyupdate(Key, [First | Rest], Fun, SearchedList) ->
    %% key not found yet, continue searching
    keyupdate(Key, Rest, Fun, [First | SearchedList]).

%% This function makes sure a colour is always one of "röd", "grön", "orange"
%% When getting "a fake unicode string", we need to convert to proper unicode
get_colour(String) when is_list(String) ->
    NewString = unicode:characters_to_list(list_to_binary(String)),
    string:to_lower(NewString);
get_colour(1) -> "röd";
get_colour(2) -> "grön";
get_colour(3) -> "orange".
    
%%---------------------------------------------------------
%%  SPI functions

%% Single register value getting/setting is straightforward.
%% When reading registers, the value should be zero and when
%% writing registers, the return value is not needed.
spi_read(Handle, Register) ->
    {ok, [_, Value]} =
	spi_interface:transfer_spi_data(Handle, [Register, 0]),
    Value.

spi_write(Handle, Register, Value) ->
    spi_interface:transfer_spi_data(Handle, [Register, Value]).

%% Chained devices need double register value setting.
%% The first register value will be clocked through to the second
%% device and the second value will end up in the first device.
%% In our case, we set the same register on both devices.
%% To set the same value for both devices, we need to add an extra
%% argument to differentiate from the function above. 
spi_write(Handle, Register, Value, same) ->
    spi_write(Handle, Register, Value, Value);
spi_write(Handle, Register, Value1, Value2) when is_integer(Register),
						 is_integer(Value1),
						 is_integer(Value2) ->
    spi_interface:transfer_spi_data(Handle, [Register, Value1,
					     Register, Value2]).

get_initial_register_values(colour_matrix) ->
    RegValues0 = [#register_value{register = Address, value = 0}
		  || Address <- lists:seq(16#20, 16#2F)],
    [#register_value{register = 1, value = 0} | RegValues0]
	++ [#register_value{register = 4, value = 1, add = true}];
get_initial_register_values(double_matrix) ->
    [#register_value{register = 16#9, value = 0},
     #register_value{register = 16#a, value = 3},
     #register_value{register = 16#b, value = 7},
     #register_value{register = 16#c, value = 1},
     #register_value{register = 16#f, value = 0}].

get_ce_pin(colour_matrix) -> 8;
get_ce_pin(double_matrix) -> 24.

%% Setting a register value for a colour matrix, single device.
%% Adding a value to a previous means setting some additional bits. 
set_register_value(Handle, colour_matrix,
		   #register_value{register = Register,
				   value = Value,
				   add = true}) ->
    Value0 = spi_read(Handle, Register),
    spi_write(Handle, Register, Value0 bor Value);
set_register_value(Handle, colour_matrix,
		   #register_value{register = Register,
				   value = Value}) ->
    spi_write(Handle, Register, Value);
%% Setting a register value for a double matrix, two devices.
set_register_value(Handle, double_matrix,
		   #register_value{register = Register,
				   value = Value}) ->
    spi_write(Handle, Register, Value, same).

%% Setting the initial register values of the device(s).
%% Do simple recursion over the data.
set_initial_register_values(Handle, DisplayType) ->
    InitialRegisterValues = get_initial_register_values(DisplayType),
    set_initial_register_values(Handle, DisplayType, InitialRegisterValues).
set_initial_register_values(_Handle, _DisplayType, []) ->
    ok;
set_initial_register_values(Handle, DisplayType, [First | Rest]) ->
    set_register_value(Handle, DisplayType, First),
    set_initial_register_values(Handle, DisplayType, Rest).


%%---------------------------------------------------------
%%  Display functions

%% This function displays a picture of lib internal format
%% on the matrix.
%% Always display a picture row by row. Since register setting
%% is fast, there will be no noticeable delay and a picture
%% will appear "all at once".
display_picture(SpiHandle, Picture) ->
    lists:foreach(fun ({RowNumber, RowData}) ->
			  display_row(SpiHandle, RowNumber, RowData)
		  end, Picture).

%% For a colour matrix, the red leds are addressed with register offset
%% 0x20 and the green leds with register offset 0x28.
display_row(SpiHandle, Row, #colour_matrix_row{red_value = RedValue,
					       green_value = GreenValue}) ->
    spi_write(SpiHandle, (16#20 + Row - 1), RedValue),
    spi_write(SpiHandle, (16#28 + Row - 1), GreenValue);
%% For a double matrix, there are two values for the same row.
%% The register address is the row number and the first value
%% addresses the right matrix, the second the left as explained above.
display_row(SpiHandle, Row, #double_matrix_row{left_value = LeftValue,
					       right_value = RightValue}) ->
    spi_write(SpiHandle, Row, RightValue, LeftValue).


%%---------------------------------------------------------
%%  Picture building functions

%% Construct an empty row for the display type.
%% The values inside the row default to zero (see record definition)
get_empty_row(double_matrix) ->
    #double_matrix_row{};
get_empty_row(colour_matrix) ->
    #colour_matrix_row{}.

%% An empty picture is a list of empty rows keyed by the row number
get_empty_picture(DisplayType) ->
    lists:map(fun (Row) ->
		      Value = get_empty_row(DisplayType),
		      {Row, Value}
	      end, lists:seq(1, 8)).

%% This function gives a byte with the nth bit set,
%% counting from 1 from the left, i.e. the bit in "column n".
%% This effectively turns a column number into a bit in a byte.
column_number_to_value(ColumnNumber) ->
    (1 bsl (8 - ColumnNumber)).

%% This function adds a bit/led to a row for a colour matrix.
%% The colour determines whether the bit is to be set in the red value,
%% the green value or both.
%% Note that the bit is given as a byte value (see the function above).
add_column_to_colour_row(#colour_matrix_row{red_value = RedValue} = Row,
			 Value, "röd") ->
    Row#colour_matrix_row{red_value = RedValue bor Value};
add_column_to_colour_row(#colour_matrix_row{green_value = GreenValue} = Row,
			 Value, "grön") ->
    Row#colour_matrix_row{green_value = GreenValue bor Value};
add_column_to_colour_row(#colour_matrix_row{red_value = RedValue,
					    green_value = GreenValue} = Row,
			 Value, "orange") ->
    Row#colour_matrix_row{red_value = RedValue bor Value,
			  green_value = GreenValue bor Value}.

%% This function adds a bit/led to a row for a double matrix.
%% Note that the bit is given as a column index and that the column index
%% for a double matrix can be between 1 and 16, determining whether the
%% bit is to be added to the left value (column 1 to 8), or to the right
%% value (column 9 to 16).
add_column_to_double_row(#double_matrix_row{left_value = LeftValue} = Row,
			 ColumnNumber) when ColumnNumber =< 8 ->
    ColumnValue = column_number_to_value(ColumnNumber),
    Row#double_matrix_row{left_value = LeftValue bor ColumnValue};
add_column_to_double_row(#double_matrix_row{right_value = RightValue} = Row,
			 ColumnNumber) ->
    ColumnValue = column_number_to_value(ColumnNumber - 8),
    Row#double_matrix_row{right_value = RightValue bor ColumnValue}.    

%% This function adds a point/led to a picture.
%% For a colour matrix, the point is a tuple containing the colour.
%% For a double matrix, it is only a coordinate.
%% Note that the coordinate is given as a unicode string that is converted
%% to a column and row index, i.e. "A1" becomes 1,1, "G8" becomes 7,8 asf.
%% The corresponding row is then updated with the helper functions above.
add_point_to_picture({Coordinate, Colour}, PictureData0) ->
    [Column, Row] = unicode:characters_to_list(Coordinate),
    ColumnNumber = string:to_lower(Column) - $a + 1,
    RowNumber = Row - $0,
    %% Now rotate the picture 90 degrees to the left.
    %% The column number will point out the row and the row number
    %% will point out the column.
    RowValue = column_number_to_value(9-RowNumber),
    keyupdate(ColumnNumber, PictureData0,
	      fun (RowData0) ->
		      add_column_to_colour_row(RowData0, RowValue, Colour)
	      end);
add_point_to_picture(Coordinate, PictureData0) ->
    [Column, Row] = unicode:characters_to_list(Coordinate),
    ColumnNumber = string:to_lower(Column) - $a + 1,
    RowNumber = Row - $0,
    keyupdate(RowNumber, PictureData0,
	      fun (RowData0) ->
		      add_column_to_double_row(RowData0, ColumnNumber)
	      end).

%% This function adds a list of points/leds to a picture, one at a time.
add_points_to_picture([], Picture0) ->
    Picture0;
add_points_to_picture(PointList, Picture0) ->
    lists:foldl(fun add_point_to_picture/2, Picture0, PointList).

%%=========================================================
