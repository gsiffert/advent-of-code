-module(day03).

-export([
    read_dataset/1,
    gamma/1,
    epsilon/1,
    part1/2,
    part2/2,
    map_to_integer/1
]).

binary_reader(<<>>, Map, _Index) -> Map;
binary_reader(<<"1", Rest/binary>>, Map, Index) ->
    binary_reader(Rest, maps:put(Index, 1, Map), Index+1);
binary_reader(<<"0", Rest/binary>>, Map, Index) ->
    binary_reader(Rest, maps:put(Index, 0, Map), Index+1).

read_dataset(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global]),
    lists:map(fun(Line) -> binary_reader(Line, maps:new(), 0) end, Lines).

group_by_columns(Binaries) ->
    Initial = maps:map(fun(_, X) -> [X] end, hd(Binaries)),
    lists:foldl(
        fun(Bits, NewMap) ->
            maps:merge_with(fun(_, V1, V2) -> [V1 | V2] end, Bits, NewMap)
        end,
        Initial,
        tl(Binaries)
    ).

bit_counter([], OneCount, ZeroCount) -> {OneCount, ZeroCount};
bit_counter([1 | Rest], OneCount, ZeroCount) ->
    bit_counter(Rest, OneCount + 1, ZeroCount);
bit_counter([0 | Rest], OneCount, ZeroCount) ->
    bit_counter(Rest, OneCount, ZeroCount + 1).

gamma(List) ->
    {OneCount, ZeroCount} = bit_counter(List, 0, 0),
    if
        OneCount >= ZeroCount -> 1;
        true -> 0
    end.

epsilon(List) ->
    {OneCount, ZeroCount} = bit_counter(List, 0, 0),
    if
        OneCount >= ZeroCount -> 0;
        true -> 1
    end.

map_to_integer(Map) ->
    Bitstring = maps:fold(
        fun(_, Value, Acc) ->
            <<Acc/bitstring, Value:1>>
        end,
        <<>>,
        Map
    ),
    BitSize = bit_size(Bitstring),
    <<N:BitSize>> = Bitstring,
    N.

part1(Binaries, Reducer) ->
    maps:map(
        fun(_Key, Value) -> Reducer(Value) end,
        group_by_columns(Binaries)
    ).

filters([Last], _Map, _Index) -> Last;
filters(Binaries, Reducer, Index) ->
    Value = Reducer(maps:get(Index, group_by_columns(Binaries))),
    FilteredBinaries = lists:filter(
        fun(Bits) ->
            BitValue = maps:get(Index, Bits),
            if
                BitValue =:= Value -> true;
                true -> false
            end
        end,
        Binaries
    ),
    filters(FilteredBinaries, Reducer, Index+1).

part2(Binaries, Reducer) ->
    filters(Binaries, Reducer, 0).
