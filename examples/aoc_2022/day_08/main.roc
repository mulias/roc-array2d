# Advent of Code 2022 Day 8
# https://adventofcode.com/2022/day/8
app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br
}

import cli.Stdout
import parser.Parser exposing [Parser, map, between, chompWhile, sepBy1, oneOrMore]
import parser.String exposing [parseStr, string, digit]
import array2d.Array2D exposing [Array2D]
import array2d.Index2D exposing [Index2D]
import "example.txt" as exampleInput : Str

exampleTreeHeights = parseInput exampleInput

expect exampleTreeHeights |> visibleTrees |> countVisible == 21

expect exampleTreeHeights |> treeSightLines |> mostScenicTree |> scenicScore == 8

main =
    visibilities = visibleTrees exampleTreeHeights
    sightLines = treeSightLines exampleTreeHeights
    scenicTree = mostScenicTree sightLines

    Stdout.line! "Part 1: $(visibilities |> countVisible |> Num.toStr)"
    Stdout.line! "$(displayVisibilityMap visibilities)\n"
    Stdout.line! "Part 2: $(scenicTree |> scenicScore |> Num.toStr)"
    Stdout.line! "$(displayScenicTreeMap exampleTreeHeights scenicTree)\n"

HeightMap : Array2D U64

VisibilityMap : Array2D Bool

TreeSightLines : { index : Index2D, left : U64, right : U64, up : U64, down : U64 }

SightLinesMap : Array2D TreeSightLines

visibleTrees : HeightMap -> VisibilityMap
visibleTrees = \heightMap ->
    mapShape = Array2D.shape heightMap

    visibilityMap = Array2D.map heightMap \_elem -> Bool.false

    startState = { visibilityMap, maxHeight: 0 }

    setTreeVisible = \state, treeHeight, index -> {
        visibilityMap: Array2D.set state.visibilityMap index Bool.true,
        maxHeight: treeHeight,
    }

    withLeft =
        Array2D.walk
            heightMap
            startState
            { direction: Forwards, orientation: Rows }
            \state, treeHeight, index ->
                if Index2D.isRowStart index || treeHeight > state.maxHeight then
                    setTreeVisible state treeHeight index
                else
                    state

    withRight =
        Array2D.walk
            heightMap
            withLeft
            { direction: Backwards, orientation: Rows }
            \state, treeHeight, index ->
                if Index2D.isRowEnd index mapShape || treeHeight > state.maxHeight then
                    setTreeVisible state treeHeight index
                else
                    state

    withTop =
        Array2D.walk
            heightMap
            withRight
            { direction: Forwards, orientation: Cols }
            \state, treeHeight, index ->
                if Index2D.isColStart index || treeHeight > state.maxHeight then
                    setTreeVisible state treeHeight index
                else
                    state

    finalState =
        Array2D.walk
            heightMap
            withTop
            { direction: Backwards, orientation: Cols }
            \state, treeHeight, index ->
                if Index2D.isColEnd index mapShape || treeHeight > state.maxHeight then
                    setTreeVisible state treeHeight index
                else
                    state

    finalState.visibilityMap

countVisible : VisibilityMap -> U64
countVisible = \treeVisibilities -> Array2D.countIf treeVisibilities \visible -> visible

displayVisibilityMap : VisibilityMap -> Str
displayVisibilityMap = \visibilityMap ->
    visibilityMap
    |> Array2D.map \isVisible -> if isVisible then "X" else "."
    |> joinArrayWith "" "\n"

treeSightLines : HeightMap -> SightLinesMap
treeSightLines = \heightMap ->
    Array2D.mapWithIndex heightMap \treeHeight, treeIndex ->
        visibleCount = \count, otherTreeHeight, otherIndex ->
            if treeIndex == otherIndex then
                Continue (count + 1)
            else if treeHeight > otherTreeHeight then
                Continue (count + 1)
            else
                Break count

        left =
            Array2D.walkUntil
                heightMap
                0
                { direction: Backwards, orientation: Rows, start: treeIndex }
                \count, otherTree, otherIndex ->
                    if Index2D.isRowStart otherIndex then
                        Break count
                    else
                        visibleCount count otherTree otherIndex

        right =
            Array2D.walkUntil
                heightMap
                0
                { direction: Forwards, orientation: Rows, start: treeIndex }
                \count, otherTree, otherIndex ->
                    if Index2D.isRowEnd otherIndex (Array2D.shape heightMap) then
                        Break count
                    else
                        visibleCount count otherTree otherIndex

        up =
            Array2D.walkUntil
                heightMap
                0
                { direction: Backwards, orientation: Cols, start: treeIndex }
                \count, otherTree, otherIndex ->
                    if Index2D.isColStart otherIndex then
                        Break count
                    else
                        visibleCount count otherTree otherIndex

        down =
            Array2D.walkUntil
                heightMap
                0
                { direction: Forwards, orientation: Cols, start: treeIndex }
                \count, otherTree, otherIndex ->
                    if Index2D.isColEnd otherIndex (Array2D.shape heightMap) then
                        Break count
                    else
                        visibleCount count otherTree otherIndex

        { index: treeIndex, left, right, up, down }

mostScenicTree : SightLinesMap -> TreeSightLines
mostScenicTree = \trees ->
    startState =
        trees
        |> Array2D.get { row: 0, col: 0 }
        |> orCrash "Unexpected empty array"

    Array2D.walk trees startState { direction: Forwards } \state, nextTree, _index ->
        if scenicScore state < scenicScore nextTree then
            nextTree
        else
            state

scenicScore : TreeSightLines -> U64
scenicScore = \{ left, right, up, down } -> left * right * up * down

displayScenicTreeMap : HeightMap, TreeSightLines -> Str
displayScenicTreeMap = \heightMap, { index, right, left, up, down } ->
    rightSegment = List.range { start: After index.col, end: Length right }
    leftSegment = List.range { start: At (index.col - left), end: Length left }
    upSegment = List.range { start: At (index.row - up), end: Length up }
    downSegment = List.range { start: After index.row, end: Length down }

    scenicTreeMap = Array2D.map heightMap Num.toStr

    withRight = List.walk rightSegment scenicTreeMap \mapState, col ->
        Array2D.set mapState { row: index.row, col } "."

    withLeft = List.walk leftSegment withRight \mapState, col ->
        Array2D.set mapState { row: index.row, col } "."

    withUp = List.walk upSegment withLeft \mapState, row ->
        Array2D.set mapState { row, col: index.col } "."

    withAll = List.walk downSegment withUp \mapState, row ->
        Array2D.set mapState { row, col: index.col } "."

    joinArrayWith withAll "" "\n"

parseInput : Str -> Array2D U64
parseInput = \inputStr ->
    parser =
        oneOrMore digit
        |> sepBy1 (string "\n")
        |> between optionalWhitespace optionalWhitespace
        |> map \rows ->
            rows
            |> Array2D.fromExactLists
            |> orCrash "Input rows are not all the same length"

    when parseStr parser inputStr is
        Ok treeHeights -> treeHeights
        Err (ParsingFailure msg) -> crash "parsing failure '$(msg)'"
        Err (ParsingIncomplete leftover) -> crash "parsing incomplete '$(leftover)'"

isWhitespace : U8 -> Bool
isWhitespace = \char ->
    when char is
        ' ' -> Bool.true
        '\n' -> Bool.true
        '\t' -> Bool.true
        11 -> Bool.true # U+000B LINE TABULATION
        12 -> Bool.true # U+000C FORM FEED
        '\r' -> Bool.true
        _ -> Bool.false

optionalWhitespace : Parser (List U8) (List U8)
optionalWhitespace =
    chompWhile isWhitespace

orCrash : Result a *, Str -> a
orCrash = \result, msg ->
    when result is
        Ok a -> a
        Err _ -> crash msg

joinArrayWith : Array2D Str, Str, Str -> Str
joinArrayWith = \array, elemSep, rowSep ->
    array
    |> Array2D.toLists
    |> List.map \row -> Str.joinWith row elemSep
    |> Str.joinWith rowSep
