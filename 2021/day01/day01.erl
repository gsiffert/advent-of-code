-module(day01).

-export([
    read_dataset/1,
    windowing/2,
    depth_measurement_increase/1
]).

read_dataset(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global]),
    lists:map(fun binary_to_integer/1, Lines).

folder(Current, {PreviousDepth, Sum}) when Current > PreviousDepth ->
    {Current, Sum +1};
folder(Current, {_, Sum}) ->
    {Current, Sum}.

depth_measurement_increase(Measurements) ->
    {_, Times} = lists:foldl(
        fun folder/2,
        {hd(Measurements), 0},
        tl(Measurements)
    ),
    Times.

windowing(Measurements, Size) -> windowing(Measurements, Size, []).

windowing([], _Size, Acc) ->
    lists:reverse(Acc);
windowing(Measurements, Size, Acc) ->
    WindowSum = lists:sum(lists:sublist(Measurements, Size)),
    windowing(tl(Measurements), Size, [WindowSum | Acc]).
