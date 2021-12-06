-module(day02).

-export([
    read_dataset/1,
    forecast_position_part1/1,
    forecast_position_part2/1,
    vector_norm/1
]).

-record(vector,
    {
        x = 0 :: integer(),
        y = 0 :: integer()
    }
).

-record(cmd,
    {
        name,
        vector
    }
).

move_reader(<<"forward ", X/binary>>) ->
    #cmd{name = forward, vector = #vector{x = binary_to_integer(X)}};
move_reader(<<"down ", X/binary>>) ->
    #cmd{name = down, vector = #vector{y = binary_to_integer(X)}};
move_reader(<<"up ", X/binary>>) ->
    #cmd{name = up, vector = #vector{y = binary_to_integer(X) * -1}}.

read_dataset(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global]),
    lists:map(fun move_reader/1, Lines).

forecast_position_part1(Moves) ->
    lists:foldl(
        fun(Move, Position) ->
            vector_add(Move#cmd.vector, Position)
        end,
        #vector{x = 0, y = 0},
        Moves
    ).

part2_folder(#cmd{name = forward, vector = Move}, {Aim, Position}) ->
    ComputedMove = Move#vector{y = Aim * Move#vector.x},
    {Aim, vector_add(ComputedMove, Position)};
part2_folder(Cmd, {Aim, Position}) ->
    {Aim + Cmd#cmd.vector#vector.y, Position}.

forecast_position_part2(Moves) ->
    {_, NextPosition} = lists:foldl(
        fun part2_folder/2,
        {0, #vector{x = 0, y = 0}},
        Moves
    ),
    NextPosition.

vector_norm(#vector{x = X, y = Y}) -> X * Y.

vector_add(A, B) ->
    #vector{
        x = A#vector.x + B#vector.x,
        y = A#vector.y + B#vector.y
    }.
