# Advent of Code 2022 Day 14
# https://adventofcode.com/2022/day/14
app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import cli.Stdout
import array2d.Array2D exposing [Array2D]
import array2d.Index2D exposing [Index2D]
import "example.txt" as exampleInput : Str

# Offset makes it easier to display the results
offsetCols = 300

caveDimensions = { rows: 170, cols: 700 - offsetCols }

sandSource = { row: 0, col: 500 - offsetCols }
exampleCave = initCave exampleInput

expect exampleCave |> addMaxSand |> countSand == 24
expect exampleCave |> addFloor |> addMaxSand |> countSand == 93

main =
    part1Cave = exampleCave |> addMaxSand
    part1Count = part1Cave |> countSand |> Num.toStr

    part2Cave = exampleCave |> addFloor |> addMaxSand
    part2Count = part2Cave |> countSand |> Num.toStr

    Stdout.line! "Part 1: $(part1Count)"
    Stdout.line! "$(displayCave part1Cave)\n"
    Stdout.line! "Part 2: $(part2Count)"
    Stdout.line! "$(displayCave part2Cave)\n"

Cave : Array2D [Air, Rock, Sand]

Path : List Index2D

addMaxSand : Cave -> Cave
addMaxSand = \cave ->
    when addSand cave sandSource is
        Ok sandyCave -> addMaxSand sandyCave
        Err _ -> cave

addSand : Cave, Index2D -> Result Cave [SandFellIntoTheInfiniteAbyss, Blocked]
addSand = \cave, pos ->
    when Array2D.get cave pos is
        Err OutOfBounds -> Err SandFellIntoTheInfiniteAbyss
        Ok Sand | Ok Rock -> Err Blocked
        Ok Air ->
            addSand cave (down pos)
            |> elseIfBlocked \_ -> addSand cave (downLeft pos)
            |> elseIfBlocked \_ -> addSand cave (downRight pos)
            |> elseIfBlocked \_ -> Ok (Array2D.set cave pos Sand)

elseIfBlocked = \result, thunk ->
    when result is
        Err Blocked -> thunk {}
        _ -> result

down = \{ row, col } -> { row: row + 1, col }
downLeft = \{ row, col } -> { row: row + 1, col: col - 1 }
downRight = \{ row, col } -> { row: row + 1, col: col + 1 }

countSand : Cave -> U64
countSand = \cave -> Array2D.countIf cave \material -> material == Sand

initCave : Str -> Cave
initCave = \input ->
    startState = Array2D.repeat Air caveDimensions
    input |> toPaths |> List.walk startState setRockPath

toPaths : Str -> List Path
toPaths = \input ->
    input
    |> Str.splitOn "\n"
    |> List.dropLast 1
    |> List.map \line ->
        line
        |> Str.splitOn " -> "
        |> List.map toPoint

toPoint : Str -> Index2D
toPoint = \input ->
    when Str.splitOn input "," is
        [strCol, strRow] ->
            row = strRow |> Str.toU64 |> orCrash "Expected a number"
            col = strCol |> Str.toU64 |> orCrash "Expected a number"

            { row, col: col - offsetCols }

        _ -> crash "Expected a point: $(input)"

setRockPath : Cave, Path -> Cave
setRockPath = \cave, path ->
    when path is
        [point1, point2, ..] ->
            nextCave = addRockLineSegment cave point1 point2
            nextPath = List.dropFirst path 1
            setRockPath nextCave nextPath

        _ -> cave

addRockLineSegment : Cave, Index2D, Index2D -> Cave
addRockLineSegment = \cave, point1, point2 ->
    if point1.row == point2.row && point1.col != point2.col then
        # vertical line
        row = point1.row
        startCol = Num.min point1.col point2.col
        endCol = Num.max point1.col point2.col

        { start: At startCol, end: At endCol }
        |> List.range
        |> List.walk cave \state, col -> Array2D.set state { row, col } Rock
    else if point1.row != point2.row && point1.col == point2.col then
        # horizontal line
        col = point1.col
        startRow = Num.min point1.row point2.row
        endRow = Num.max point1.row point2.row

        { start: At startRow, end: At endRow }
        |> List.range
        |> List.walk cave \state, row -> Array2D.set state { row, col } Rock
    else
        crash "Diagonal lines not supported"

addFloor : Cave -> Cave
addFloor = \cave ->
    cave
    |> Array2D.findLastIndex \material -> material == Rock
    |> Result.map \{ row: floorLevel, col: _ } ->
        point1 = { row: floorLevel + 2, col: 0 }
        point2 = { row: floorLevel + 2, col: cave |> Array2D.shape |> .cols }
        addRockLineSegment cave point1 point2
    |> orCrash "Error finding floor level"

displayCave : Cave -> Str
displayCave = \cave ->
    cave
    |> Array2D.map \material ->
        when material is
            Air -> "."
            Rock -> "#"
            Sand -> "o"
    |> joinArrayWith "" "\n"

joinArrayWith : Array2D Str, Str, Str -> Str
joinArrayWith = \array, elemSep, rowSep ->
    array
    |> Array2D.toLists
    |> List.map \row -> Str.joinWith row elemSep
    |> Str.joinWith rowSep

orCrash : Result a *, Str -> a
orCrash = \result, msg ->
    when result is
        Ok a -> a
        Err _ -> crash msg
