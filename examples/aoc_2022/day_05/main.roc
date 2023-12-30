# Advent of Code 2022 Day 5
# https://adventofcode.com/2022/day/5
app "Advent2022Day05"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.2.0/pmAttjSPjyubNa8XiVH9D3vsDMqHahq_yz81N_tt_UU.tar.br",
    }
    imports [
        cli.Stdout,
        cli.Task,
        parser.Core.{ Parser, between, sepBy1, chompWhile, keep, skip, const, map, oneOf, buildPrimitiveParser, parsePartial, fail },
        parser.String.{ RawStr, parseStr, string, codeunit, digits, anyCodeunit },
        array2d.Array2D.{ Array2D },
        array2d.Shape2D,
        array2d.Index2D,
        "example.txt" as exampleInput : Str,
    ]
    provides [main] to cli

CraneSpec : [CrateMover9000, CrateMover9001]

Crate : Str

Stack : List Crate

StackId : Nat

Stacks : Dict StackId Stack

Step : { count : Nat, source : StackId, dest : StackId }

Input : { stacks : Stacks, steps : List Step }

expect exampleInput |> parseInput |> finalState CrateMover9000 |> topCrates == "CMZ"
expect exampleInput |> parseInput |> finalState CrateMover9001 |> topCrates == "MCD"

main =
    part1 = exampleInput |> parseInput |> finalState CrateMover9000
    part2 = exampleInput |> parseInput |> finalState CrateMover9001

    _ <- Stdout.line "Part 1: \(topCrates part1)" |> Task.await
    _ <- Stdout.write "\(displayStacks part1)\n\n" |> Task.await
    _ <- Stdout.line "Part 2: \(topCrates part2)" |> Task.await
    _ <- Stdout.write "\(displayStacks part2)\n\n" |> Task.await
    Task.ok {}

topCrates : Stacks -> Str
topCrates = \stacks ->
    stacks
    |> stacksToList
    |> List.map \stack ->
        stack
        |> List.last
        |> orCrash "Unexpected empty stack"
    |> Str.joinWith ""

finalState : Input, CraneSpec -> Stacks
finalState = \{ stacks, steps }, spec ->
    List.walk steps stacks \stacksState, step -> performStep stacksState step spec

performStep : Stacks, Step, CraneSpec -> Stacks
performStep = \stacks, step, craneSpec ->
    sourceStack = getStack stacks step.source
    destStack = getStack stacks step.dest
    splitIndex = (List.len sourceStack) - step.count

    { before: newSourceStack, others: movedCrates } = List.split sourceStack splitIndex

    newDestStack =
        when craneSpec is
            CrateMover9000 -> List.concat destStack (List.reverse movedCrates)
            CrateMover9001 -> List.concat destStack movedCrates

    stacks
    |> Dict.insert step.source newSourceStack
    |> Dict.insert step.dest newDestStack

getStack : Stacks, StackId -> Stack
getStack = \stacks, stackId ->
    stacks |> Dict.get stackId |> orCrash "can't find stack #\(Num.toStr stackId)"

displayStacks : Stacks -> Str
displayStacks = \stacks ->
    stacks
    |> stacksToList
    |> List.map \stack -> List.map stack Ok
    |> Array2D.fromLists (FitLongest (Err Empty))
    |> Array2D.rotateCounterClockwise
    |> Array2D.map \elem ->
        when elem is
            Ok crate -> "[\(crate)]"
            Err Empty -> "   "
    |> joinArrayWith " " "\n"

stacksToList : Stacks -> List Stack
stacksToList = \stacks ->
    stacks
    |> Dict.toList
    |> List.sortWith \(idA, _stackA), (idB, _stackB) -> Num.compare idA idB
    |> List.map \(_id, stack) -> stack

parseInput : Str -> Input
parseInput = \inputStr ->
    when parseStr inputParser inputStr is
        Ok input -> input
        Err (ParsingFailure msg) -> crash "parsing failure '\(msg)'"
        Err (ParsingIncomplete leftover) -> crash "parsing incomplete '\(leftover)'"

inputParser : Parser RawStr Input
inputParser =
    const (\stacks -> \steps -> { stacks, steps })
    |> keep stacksParser
    |> skip optionalWhitespace
    |> keep stepsParser
    |> skip optionalWhitespace

stacksParser : Parser RawStr Stacks
stacksParser =
    const
        (\crateCols -> \labels ->
                labels
                |> List.map2 crateCols \label, col -> (label, col)
                |> Dict.fromList
        )
    |> keep crateColsParser
    |> skip optionalWhitespace
    |> keep labelsParser

crateColsParser : Parser RawStr (List (List Crate))
crateColsParser =
    map crateRowsParser \rows ->
        rows
        |> Array2D.rotateClockwise
        |> Array2D.toLists
        |> List.map \cols ->
            List.keepOks cols \col -> col

crateRowsParser : Parser RawStr (Array2D (Result Crate [Empty]))
crateRowsParser =
    table
        (oneOf [crateParser, noCrateParser])
        (codeunit ' ')
        (codeunit '\n')

crateParser =
    const
        (\c ->
            [c]
            |> Str.fromUtf8
            |> orCrash "Error: UTF8 to String conversion issue"
            |> Ok
        )
    |> skip (codeunit '[')
    |> keep anyCodeunit
    |> skip (codeunit ']')

noCrateParser = string "   " |> map \_ -> Err Empty

labelsParser : Parser RawStr (List Nat)
labelsParser =
    digits
    |> sepBy1 optionalWhitespace
    |> between optionalWhitespace optionalWhitespace

stepsParser : Parser RawStr (List Step)
stepsParser =
    sepBy1 stepParser optionalWhitespace

stepParser : Parser RawStr Step
stepParser =
    const (\count -> \source -> \dest -> { count, source, dest })
    |> skip (string "move ")
    |> keep digits
    |> skip (string " from ")
    |> keep digits
    |> skip (string " to ")
    |> keep digits

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

optionalWhitespace : Parser RawStr RawStr
optionalWhitespace =
    chompWhile isWhitespace

table : Parser in a, Parser in *, Parser in * -> Parser in (Array2D a)
table = \elem, elemSep, rowSep ->
    elem
    |> sepBy1 elemSep
    |> sepBy1 rowSep
    |> andThen \rows ->
        when Array2D.fromExactLists rows is
            Ok array -> const array
            Err InconsistentRowLengths -> fail "table: rows do not have consistant lengths"

andThen : Parser input a, (a -> Parser input b) -> Parser input b
andThen = \firstParser, buildNextParser ->
    fun = \input ->
        { val: firstVal, input: rest } <- Result.try (parsePartial firstParser input)
        nextParser = buildNextParser firstVal

        parsePartial nextParser rest

    buildPrimitiveParser fun

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
