# Advent of Code 2022 Day 8
# https://adventofcode.com/2022/day/8
app "Advent2022Day08"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        cli.Stdout,
        cli.Task,
        parser.Core.{ Parser, map, between, chompWhile, sepBy1, oneOrMore },
        parser.String.{ RawStr, parseStr, string, digit },
        array2d.Array2D.{ Array2D },
        "example.txt" as exampleInput : Str,
    ]
    provides [main] to cli

exampleTreeHeights = parseInput exampleInput

expect exampleTreeHeights |> visibleTrees |> countVisible == 21

expect exampleTreeHeights |> treeSightLines |> mostScenicTree |> scenicScore == 8

main =
    visibilities = visibleTrees exampleTreeHeights
    sightLines = treeSightLines exampleTreeHeights
    scenicTree = mostScenicTree sightLines

    _ <- Stdout.line "Part 1: \(visibilities |> countVisible |> Num.toStr)" |> Task.await
    _ <- Stdout.write "\(displayVisibilityMap visibilities)\n\n" |> Task.await
    _ <- Stdout.line "Part 2: \(scenicTree |> scenicScore |> Num.toStr)" |> Task.await
    _ <- Stdout.write "\(displayScenicTreeMap exampleTreeHeights scenicTree)\n\n" |> Task.await
    Task.ok {}

HeightMap : Array2D Nat

VisibilityMap : Array2D Bool

TreeSightLines : { index : Array2D.Index, left : Nat, right : Nat, up : Nat, down : Nat }

SightLinesMap : Array2D TreeSightLines

visibleTrees : HeightMap -> VisibilityMap
visibleTrees = \heightMap ->
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
                if Array2D.isRowStart index || treeHeight > state.maxHeight then
                    setTreeVisible state treeHeight index
                else
                    state

    withRight =
        Array2D.walk
            heightMap
            withLeft
            { direction: Backwards, orientation: Rows }
            \state, treeHeight, index ->
                if Array2D.isRowEnd heightMap index || treeHeight > state.maxHeight then
                    setTreeVisible state treeHeight index
                else
                    state

    withTop =
        Array2D.walk
            heightMap
            withRight
            { direction: Forwards, orientation: Cols }
            \state, treeHeight, index ->
                if Array2D.isColStart index || treeHeight > state.maxHeight then
                    setTreeVisible state treeHeight index
                else
                    state

    finalState =
        Array2D.walk
            heightMap
            withTop
            { direction: Backwards, orientation: Cols }
            \state, treeHeight, index ->
                if Array2D.isColEnd heightMap index || treeHeight > state.maxHeight then
                    setTreeVisible state treeHeight index
                else
                    state

    finalState.visibilityMap

countVisible : VisibilityMap -> Nat
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
                    if Array2D.isRowStart otherIndex then
                        Break count
                    else
                        visibleCount count otherTree otherIndex

        right =
            Array2D.walkUntil
                heightMap
                0
                { direction: Forwards, orientation: Rows, start: treeIndex }
                \count, otherTree, otherIndex ->
                    if Array2D.isRowEnd heightMap otherIndex then
                        Break count
                    else
                        visibleCount count otherTree otherIndex

        up =
            Array2D.walkUntil
                heightMap
                0
                { direction: Backwards, orientation: Cols, start: treeIndex }
                \count, otherTree, otherIndex ->
                    if Array2D.isColStart otherIndex then
                        Break count
                    else
                        visibleCount count otherTree otherIndex

        down =
            Array2D.walkUntil
                heightMap
                0
                { direction: Forwards, orientation: Cols, start: treeIndex }
                \count, otherTree, otherIndex ->
                    if Array2D.isColEnd heightMap otherIndex then
                        Break count
                    else
                        visibleCount count otherTree otherIndex

        { index: treeIndex, left, right, up, down }

mostScenicTree : SightLinesMap -> TreeSightLines
mostScenicTree = \trees ->
    startState =
        trees
        |> Array2D.get { x: 0, y: 0 }
        |> orCrash "Unexpected empty array"

    Array2D.walk trees startState { direction: Forwards, orientation: Rows, start: { x: 0, y: 0 } } \state, nextTree, _index ->
        if scenicScore state < scenicScore nextTree then
            nextTree
        else
            state

scenicScore : TreeSightLines -> Nat
scenicScore = \{ left, right, up, down } -> left * right * up * down

displayScenicTreeMap : HeightMap, TreeSightLines -> Str
displayScenicTreeMap = \heightMap, { index, right, left, up, down } ->
    rightSegment = List.range { start: After index.y, end: Length right }
    leftSegment = List.range { start: At (index.y - left), end: Length left }
    upSegment = List.range { start: At (index.x - up), end: Length up }
    downSegment = List.range { start: After index.x, end: Length down }

    scenicTreeMap = Array2D.map heightMap Num.toStr

    withRight = List.walk rightSegment scenicTreeMap \mapState, y ->
        Array2D.set mapState { x: index.x, y } "."

    withLeft = List.walk leftSegment withRight \mapState, y ->
        Array2D.set mapState { x: index.x, y } "."

    withUp = List.walk upSegment withLeft \mapState, x ->
        Array2D.set mapState { x, y: index.y } "."

    withAll = List.walk downSegment withUp \mapState, x ->
        Array2D.set mapState { x, y: index.y } "."

    joinArrayWith withAll "" "\n"

parseInput : Str -> Array2D Nat
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
        Err (ParsingFailure msg) -> crash "parsing failure '\(msg)'"
        Err (ParsingIncomplete leftover) -> crash "parsing incomplete '\(leftover)'"

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
