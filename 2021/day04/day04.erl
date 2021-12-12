-module(day04).

-export([
    read_dataset/1,
    part1/1,
    part2/1
]).

-record(game,
    {
        draws,
        boards
    }).

-record(board,
    {
        sets,
        numbers
    }
).

read_board(Lines) ->
   Array = lists:foldl(
        fun(Line, Board) ->
            Tokens = binary:split(Line, <<" ">>, [global]),
            FilteredTokens = lists:filter(fun(X) -> X =/= <<>> end, Tokens),
            Integers = lists:map(fun erlang:binary_to_integer/1, FilteredTokens),
            Row = array:from_list(Integers),
            array:set(array:sparse_size(Board), array:fix(Row), Board)
        end,
        array:new(),
        Lines
    ),
    VerticalSets = lists:map(
        fun(X) ->
            Set = lists:map(
                fun(Y) ->
                    Row = array:get(Y, Array),
                    array:get(X, Row)
                end,
                lists:seq(0, array:sparse_size(Array)-1)
            ),
            lists:sort(Set)
        end,
        lists:seq(0, array:sparse_size(Array)-1)
    ),
    Sets = array:sparse_foldl(
        fun(_Idx, Value, Acc) ->
            Set = lists:sort(array:to_list(Value)),
            [Set | Acc]
        end,
        VerticalSets,
        Array
    ),
    #board{sets = Sets, numbers = lists:umerge(Sets)}.

read_lines([], Game) -> Game;
read_lines([Line | Rest], #game{draws = undefined} = Game) ->
    Tokens = binary:split(Line, <<",">>, [global]),
    Draws = lists:map(fun erlang:binary_to_integer/1, Tokens),
    read_lines(Rest, Game#game{draws = Draws});
read_lines([<<>> | Rest], Game) ->
    {BoardLines, NextBoards} = lists:split(5, Rest),
    Board = read_board(BoardLines),
    AccBoards = [Board | Game#game.boards],
    read_lines(NextBoards, Game#game{boards = AccBoards}).

read_dataset(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global]),
    read_lines(Lines, #game{boards = []}).

draws_in_board(Draws, Board) ->
    lists:any(
        fun(Set) ->
            lists:all(fun(Number) -> sets:is_element(Number, Draws) end, Set)
        end,
        Board#board.sets
    ).

is_draws_in_board(Game, Length) ->
    {Draw, _} = lists:split(Length, Game#game.draws),
    DrawSet = sets:from_list(
        Draw,
        [{version, 2}]
    ),
    Result = lists:search(
        fun(Board) -> draws_in_board(DrawSet, Board) end,
        Game#game.boards
    ),
    case Result of
        false -> false;
        {value, B} -> {value, {Draw, B}}
    end.

sum_unmarked(Draw, Board) ->
    DrawSet = sets:from_list(
        Draw,
        [{version, 2}]
    ),
    UnmarkedNumbers = lists:filter(
        fun(Number) -> sets:is_element(Number, DrawSet) =:= false end,
        Board#board.numbers
    ),
    lists:sum(UnmarkedNumbers).

search_first_board(#game{draws = Draws}, Length, false) when Length > length(Draws) ->
    no_match;
search_first_board(Game, Length, false) ->
    Result = is_draws_in_board(Game, Length),
    search_first_board(Game, Length+1, Result);
search_first_board(_, _, Result) ->
    {value, {Draw, Board}} = Result,
    {Draw, Board}.

part1(Game) ->
    {Draw, Board} = search_first_board(Game, 5, false),
    sum_unmarked(Draw, Board) * lists:last(Draw).

part2(#game{boards = [Last]} = Game) ->
    part1(Game);
part2(Game) ->
    {_, Board} = search_first_board(Game, 5, false),
    FilteredBoards = lists:delete(Board, Game#game.boards),
    part2(Game#game{boards = FilteredBoards}).
