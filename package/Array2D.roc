interface Array2D exposes [
        Shape,
        Index,
        empty,
        init,
        initWithList,
        initWithLists,
        repeat,
        fromList,
        fromLists,
        fromExactList,
        fromExactLists,
        shape,
        size,
        isRowStart,
        isRowEnd,
        isColStart,
        isColEnd,
        set,
        get,
        update,
        swap,
        map,
        mapWithIndex,
        walk,
        walkUntil,
        toList,
        toLists,
        reshape,
        transpose,
        flipX,
        flipY,
        rotateClockwise,
        rotateCounterClockwise,
        countIf,
        findFirstIndex,
        findLastIndex,
    ] imports []

Shape : { dimX : Nat, dimY : Nat }

Index : { x : Nat, y : Nat }

Array2D a := { data : List a, shape : Shape } implements [Eq { isEq: isEq }]

empty : Array2D *
empty = @Array2D { data: [], shape: { dimX: 0, dimY: 0 } }

expect empty |> toList |> List.len == 0

repeat : a, Shape -> Array2D a
repeat = \elem, arrayShape ->
    @Array2D { data: List.repeat elem (shapeSize arrayShape), shape: arrayShape }

expect repeat Empty { dimX: 3, dimY: 2 } == @Array2D { data: [Empty, Empty, Empty, Empty, Empty, Empty], shape: { dimX: 3, dimY: 2 } }

init : Shape, (Index -> a) -> Array2D a
init = \arrayShape, fn ->
    mapWithIndex (repeat Empty arrayShape) \_elem, index -> fn index

expect
    init { dimX: 4, dimY: 2 } \{ x, y } -> (x, y)
    == @Array2D { data: [(0, 0), (0, 1), (1, 0), (1, 1), (2, 0), (2, 1), (3, 0), (3, 1)], shape: { dimX: 4, dimY: 2 } }

initWithList : List a, Shape, (Result a [Empty], Index -> b) -> Array2D b
initWithList = \list, arrayShape, fn ->
    list
    |> List.map Ok
    |> fromList (Fill (Err Empty) arrayShape)
    |> mapWithIndex fn

expect
    initWithList [1, 2, 3, 4] { dimX: 2, dimY: 3 } \elem, _ -> elem
    == @Array2D { data: [Ok 1, Ok 2, Ok 3, Ok 4, Err Empty, Err Empty], shape: { dimX: 2, dimY: 3 } }

expect
    initWithList [1, 2, 3, 4] { dimX: 2, dimY: 3 } \elem, _ -> Result.withDefault elem 0
    == @Array2D { data: [1, 2, 3, 4, 0, 0], shape: { dimX: 2, dimY: 3 } }

expect
    initWithList [1, 2, 3, 4] { dimX: 2, dimY: 3 } \elem, _ ->
        elem |> Result.map Some |> Result.withDefault Empty
    == @Array2D { data: [Some 1, Some 2, Some 3, Some 4, Empty, Empty], shape: { dimX: 2, dimY: 3 } }

initWithLists : List (List a), Shape, (Result a [Empty], Index -> b) -> Array2D b
initWithLists = \lists, arrayShape, fn ->
    lists
    |> List.map \list -> List.map list Ok
    |> fromLists (Fill (Err Empty) arrayShape)
    |> mapWithIndex fn

expect
    initWithLists [[1, 2, 3, 4], [1, 2]] { dimX: 2, dimY: 3 } \elem, _ -> elem |> Result.map Some |> Result.withDefault Empty
    == @Array2D { data: [Some 1, Some 2, Some 3, Some 1, Some 2, Empty], shape: { dimX: 2, dimY: 3 } }

fromList : List a, [Fit, Fill a Shape] -> Array2D a
fromList = \list, stratagy ->
    array = @Array2D { data: list, shape: { dimX: 1, dimY: List.len list } }
    when stratagy is
        Fit -> array
        Fill defaultElem arrayShape -> reshape array defaultElem arrayShape

expect
    fromList [1, 2, 3, 4] Fit
    == @Array2D { data: [1, 2, 3, 4], shape: { dimX: 1, dimY: 4 } }

expect
    fromList [1, 2, 3, 4, 5] (Fill 0 { dimX: 4, dimY: 2 })
    == @Array2D { data: [1, 2, 3, 4, 5, 0, 0, 0], shape: { dimX: 4, dimY: 2 } }

fromLists : List (List a), [FitShortest, FitLongest a, Fill a Shape] -> Array2D a
fromLists = \lists, stratagy ->
    when stratagy is
        FitShortest ->
            arrayShape = {
                dimX: List.len lists,
                dimY: lists |> List.map List.len |> List.min |> Result.withDefault 0,
            }
            startData = List.withCapacity (shapeSize arrayShape)
            data = List.walk lists startData \acc, list ->
                List.concat acc (List.takeFirst list arrayShape.dimY)

            @Array2D { data, shape: arrayShape }

        FitLongest defaultElem ->
            arrayShape = {
                dimX: List.len lists,
                dimY: lists |> List.map List.len |> List.max |> Result.withDefault 0,
            }
            startData = List.withCapacity (shapeSize arrayShape)
            data = List.walk lists startData \acc, list ->
                List.concat acc (resize list defaultElem arrayShape.dimY)

            @Array2D { data, shape: arrayShape }

        Fill defaultElem arrayShape ->
            paddedLists = resize lists [] arrayShape.dimX
            startData = List.withCapacity (shapeSize arrayShape)
            data = List.walk paddedLists startData \acc, list ->
                List.concat acc (resize list defaultElem arrayShape.dimY)

            @Array2D { data, shape: arrayShape }

expect
    fromLists [[1, 2, 3], [4]] FitShortest
    == @Array2D { data: [1, 4], shape: { dimX: 2, dimY: 1 } }

expect
    fromLists [[1, 2, 3], [4]] (FitLongest 0)
    == @Array2D { data: [1, 2, 3, 4, 0, 0], shape: { dimX: 2, dimY: 3 } }

expect
    fromLists [[1, 2, 3], [4]] (Fill -1 { dimX: 4, dimY: 2 })
    == @Array2D { data: [1, 2, 4, -1, -1, -1, -1, -1], shape: { dimX: 4, dimY: 2 } }

fromExactList : List a, Shape -> Result (Array2D a) [NotEnoughElements, TooManyElements]
fromExactList = \list, arrayShape ->
    if List.len list == shapeSize arrayShape then
        Ok (@Array2D { data: list, shape: arrayShape })
    else if List.len list < shapeSize arrayShape then
        Err NotEnoughElements
    else
        Err TooManyElements

expect
    fromExactList [1, 2, 3, 4, 5, 6] { dimX: 2, dimY: 3 }
    == Ok (@Array2D { data: [1, 2, 3, 4, 5, 6], shape: { dimX: 2, dimY: 3 } })

expect fromExactList [1, 2, 3, 4] { dimX: 2, dimY: 3 } == Err NotEnoughElements

expect fromExactList [1, 2, 3, 4, 5, 6, 7] { dimX: 2, dimY: 3 } == Err TooManyElements

fromExactLists : List (List a) -> Result (Array2D a) [InconsistentRowLengths]
fromExactLists = \lists ->
    first = lists |> List.first |> Result.withDefault []

    if List.all lists \list -> List.len list == List.len first then
        array = @Array2D {
            data: List.join lists,
            shape: { dimX: List.len lists, dimY: List.len first },
        }
        Ok array
    else
        Err InconsistentRowLengths

expect
    fromExactLists [[1, 2], [3, 4], [5, 6]]
    == Ok (@Array2D { data: [1, 2, 3, 4, 5, 6], shape: { dimX: 3, dimY: 2 } })

expect fromExactLists [[1, 2, 3], [3, 4], [5, 6]] == Err InconsistentRowLengths

expect fromExactLists [[1, 2], [3, 4], [5, 6, 7]] == Err InconsistentRowLengths

shape : Array2D * -> Shape
shape = \@Array2D array -> array.shape

size : Array2D * -> Nat
size = \@Array2D array -> shapeSize array.shape

isRowStart : Index -> Bool
isRowStart = \{ y } -> y == 0

isRowEnd : Array2D *, Index -> Bool
isRowEnd = \@Array2D { shape: { dimY } }, { y } -> y + 1 >= dimY

isColStart : Index -> Bool
isColStart = \{ x } -> x == 0

isColEnd : Array2D *, Index -> Bool
isColEnd = \@Array2D { shape: { dimX } }, { x } -> x + 1 >= dimX

shapeSize : Shape -> Nat
shapeSize = \{ dimX, dimY } -> dimX * dimY

set : Array2D a, Index, a -> Array2D a
set = \@Array2D array, index, elem ->
    if isInBounds array.shape index then
        @Array2D { array & data: List.set array.data (listIndexOf array.shape index) elem }
    else
        @Array2D array

expect
    repeat 0 { dimX: 3, dimY: 3 }
    |> set { x: 1, y: 1 } 9
    == @Array2D { data: [0, 0, 0, 0, 9, 0, 0, 0, 0], shape: { dimX: 3, dimY: 3 } }

expect
    repeat 0 { dimX: 3, dimY: 3 }
    |> set { x: 1, y: 4 } 9
    == @Array2D { data: [0, 0, 0, 0, 0, 0, 0, 0, 0], shape: { dimX: 3, dimY: 3 } }

get : Array2D a, Index -> Result a [OutOfBounds]
get = \@Array2D array, index ->
    if isInBounds array.shape index then
        List.get array.data (listIndexOf array.shape index)
    else
        Err OutOfBounds

update : Array2D a, Index, (a -> a) -> Array2D a
update = \@Array2D array, index, fn ->
    if isInBounds array.shape index then
        @Array2D {
            data: List.update array.data (listIndexOf array.shape index) fn,
            shape: array.shape,
        }
    else
        @Array2D array

swap : Array2D a, Index, Index -> Array2D a
swap = \@Array2D array, indexA, indexB ->
    @Array2D { array & data: List.swap array.data (listIndexOf array.shape indexA) (listIndexOf array.shape indexB) }

expect
    [[X, A, A], [A, Y, A], [A, A, A]]
    |> fromLists FitShortest
    |> swap { x: 0, y: 0 } { x: 1, y: 1 }
    |> toLists
    == [[Y, A, A], [A, X, A], [A, A, A]]

map : Array2D a, (a -> b) -> Array2D b
map = \@Array2D array, fn ->
    @Array2D { data: List.map array.data fn, shape: array.shape }

mapWithIndex : Array2D a, (a, Index -> b) -> Array2D b
mapWithIndex = \@Array2D array, fn -> @Array2D {
        data: List.mapWithIndex array.data \elem, listIndex ->
            fn elem (arrayIndexOf listIndex array.shape),
        shape: array.shape,
    }

WalkOptions : {
    direction ? [Forwards, Backwards],
    orientation ? [Rows, Cols],
    start ? Index,
}

walk : Array2D a, state, WalkOptions, (state, a, Index -> state) -> state
walk = \array, startState, options, fn ->
    { direction ? Forwards, orientation ? Rows } = options
    { start ? walkStart array direction } = options

    if direction == Forwards && orientation == Rows && start == { x: 0, y: 0 } then
        (@Array2D { data, shape: arrayShape }) = array
        List.walkWithIndex data startState \state, elem, listIndex ->
            fn state elem (arrayIndexOf listIndex arrayShape)
    else
        walkUntil array startState options \state, elem, index ->
            Continue (fn state elem index)

expect
    repeat 0 { dimX: 3, dimY: 2 }
    |> walk [] { direction: Forwards, orientation: Rows, start: { x: 0, y: 0 } } \acc, _, { x, y } -> List.append acc (x, y)
    == [(0, 0), (0, 1), (1, 0), (1, 1), (2, 0), (2, 1)]

expect
    repeat 0 { dimX: 3, dimY: 2 }
    |> walk [] { direction: Backwards, orientation: Rows, start: { x: 2, y: 1 } } \acc, _, { x, y } -> List.append acc (x, y)
    == [(2, 1), (2, 0), (1, 1), (1, 0), (0, 1), (0, 0)]

expect
    repeat 0 { dimX: 3, dimY: 2 }
    |> walk [] { direction: Forwards, orientation: Cols, start: { x: 0, y: 0 } } \acc, _, { x, y } -> List.append acc (x, y)
    == [(0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1)]

expect
    repeat 0 { dimX: 3, dimY: 2 }
    |> walk [] { direction: Backwards, orientation: Cols, start: { x: 2, y: 1 } } \acc, _, { x, y } -> List.append acc (x, y)
    == [(2, 1), (1, 1), (0, 1), (2, 0), (1, 0), (0, 0)]

walkUntil : Array2D a, state, WalkOptions, (state, a, Index -> [Continue state, Break state]) -> state
walkUntil = \array, startState, options, fn ->
    { direction ? Forwards, orientation ? Rows } = options
    { start ? walkStart array direction } = options

    when (direction, orientation) is
        (Forwards, Rows) -> walkRowsUntil array start startState fn
        (Backwards, Rows) -> walkRowsBackwardsUntil array start startState fn
        (Forwards, Cols) -> walkColsUntil array start startState fn
        (Backwards, Cols) -> walkColsBackwardsUntil array start startState fn

walkStart : Array2D a, [Forwards, Backwards] -> Index
walkStart = \@Array2D array, direction ->
    if direction == Forwards then { x: 0, y: 0 } else lastArrayIndex array.shape

walkRowsUntil : Array2D a, Index, state, (state, a, Index -> [Continue state, Break state]) -> state
walkRowsUntil = \array, startIndex, state, fn ->
    iterate array startIndex state incY fn

walkRowsBackwardsUntil : Array2D a, Index, state, (state, a, Index -> [Continue state, Break state]) -> state
walkRowsBackwardsUntil = \array, startIndex, state, fn ->
    iterate array startIndex state decY fn

walkColsUntil : Array2D a, Index, state, (state, a, Index -> [Continue state, Break state]) -> state
walkColsUntil = \array, startIndex, state, fn ->
    iterate array startIndex state incX fn

walkColsBackwardsUntil : Array2D a, Index, state, (state, a, Index -> [Continue state, Break state]) -> state
walkColsBackwardsUntil = \array, startIndex, state, fn ->
    iterate array startIndex state decX fn

iterate : Array2D a, Index, state, (Array2D a, Index -> Result Index [OutOfBounds]), (state, a, Index -> [Continue state, Break state]) -> state
iterate = \array, index, state, nextIndexFn, nextStateFn ->
    elem =
        when get array index is
            Ok e -> e
            Err OutOfBounds -> crash "Unexpected error iterating over Array2D"

    when (nextIndexFn array index, nextStateFn state elem index) is
        (Ok nextIndex, Continue nextState) -> iterate array nextIndex nextState nextIndexFn nextStateFn
        (_, Continue nextState) -> nextState
        (_, Break nextState) -> nextState

incY : Array2D *, Index -> Result Index [OutOfBounds]
incY = \array, index ->
    if isRowEnd array index then
        if isColEnd array index then
            Err OutOfBounds
        else
            Ok { x: index.x + 1, y: 0 }
    else
        Ok { x: index.x, y: index.y + 1 }

decY : Array2D *, Index -> Result Index [OutOfBounds]
decY = \@Array2D array, index ->
    if isRowStart index then
        if isColStart index then
            Err OutOfBounds
        else
            Ok { x: index.x - 1, y: array.shape.dimY - 1 }
    else
        Ok { x: index.x, y: index.y - 1 }

incX : Array2D *, Index -> Result Index [OutOfBounds]
incX = \array, index ->
    if isColEnd array index then
        if isRowEnd array index then
            Err OutOfBounds
        else
            Ok { x: 0, y: index.y + 1 }
    else
        Ok { x: index.x + 1, y: index.y }

decX : Array2D *, Index -> Result Index [OutOfBounds]
decX = \@Array2D array, index ->
    if isColStart index then
        if isRowStart index then
            Err OutOfBounds
        else
            Ok { x: array.shape.dimX - 1, y: index.y - 1 }
    else
        Ok { x: index.x - 1, y: index.y }

toList : Array2D a -> List a
toList = \@Array2D { data } -> data

expect toList (repeat Empty { dimX: 2, dimY: 2 }) == [Empty, Empty, Empty, Empty]

toLists : Array2D a -> List (List a)
toLists = \array ->
    (@Array2D { data, shape: { dimX, dimY } }) = array
    xIndicies = List.range { start: At 0, end: Before dimX }
    startState = List.withCapacity dimX

    List.walk xIndicies startState \state, xIndex ->
        startIndex = listIndexOf { dimX, dimY } { x: xIndex, y: 0 }
        row = List.sublist data { start: startIndex, len: dimY }
        List.append state row

expect toLists (repeat 0 { dimX: 2, dimY: 2 }) == [[0, 0], [0, 0]]

expect
    toLists (fromList [1, 2, 3, 4, 5] (Fill 0 { dimX: 2, dimY: 3 }))
    == [[1, 2, 3], [4, 5, 0]]

reshape : Array2D a, a, Shape -> Array2D a
reshape = \@Array2D { data }, defaultValue, newShape ->
    @Array2D { data: resize data defaultValue (shapeSize newShape), shape: newShape }

expect repeat 1 { dimX: 3, dimY: 3 } |> reshape 9 { dimX: 1, dimY: 1 } |> toLists == [[1]]

expect
    repeat 1 { dimX: 3, dimY: 3 }
    |> reshape 9 { dimX: 4, dimY: 3 }
    |> toLists
    == [[1, 1, 1], [1, 1, 1], [1, 1, 1], [9, 9, 9]]

resize : List a, a, Nat -> List a
resize = \list, defaultValue, newLen ->
    oldLen = List.len list
    if newLen < oldLen then
        List.takeFirst list newLen
    else if newLen > oldLen then
        List.concat list (List.repeat defaultValue (newLen - oldLen))
    else
        list

transpose : Array2D a -> Array2D a
transpose = \@Array2D array ->
    startAcc = @Array2D { array & shape: { dimX: array.shape.dimY, dimY: array.shape.dimX } }
    List.walkWithIndex array.data startAcc \acc, elem, listIndex ->
        newIndex =
            listIndex
            |> arrayIndexOf array.shape
            |> flipIndex array.shape Diagonal

        set acc newIndex elem

expect
    [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
    |> fromExactLists
    |> Result.map transpose
    |> Result.map toLists
    == Ok [[1, 5, 9], [2, 6, 10], [3, 7, 11], [4, 8, 12]]

flipX : Array2D a -> Array2D a
flipX = \@Array2D array ->
    startAcc = @Array2D array
    List.walkWithIndex array.data startAcc \acc, elem, listIndex ->
        newIndex =
            listIndex
            |> arrayIndexOf array.shape
            |> flipIndex array.shape X

        set acc newIndex elem

expect
    [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
    |> fromExactLists
    |> Result.map flipX
    |> Result.map toLists
    == Ok [[9, 10, 11, 12], [5, 6, 7, 8], [1, 2, 3, 4]]

flipY : Array2D a -> Array2D a
flipY = \@Array2D array ->
    startAcc = @Array2D array
    List.walkWithIndex array.data startAcc \acc, elem, listIndex ->
        newIndex =
            listIndex
            |> arrayIndexOf array.shape
            |> flipIndex array.shape X

        set acc newIndex elem

expect
    [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
    |> fromExactLists
    |> Result.map flipX
    |> Result.map toLists
    == Ok [[9, 10, 11, 12], [5, 6, 7, 8], [1, 2, 3, 4]]

rotateClockwise : Array2D a -> Array2D a
rotateClockwise = \@Array2D array ->
    newShape = { dimX: array.shape.dimY, dimY: array.shape.dimX }
    startAcc = @Array2D { array & shape: newShape }
    List.walkWithIndex array.data startAcc \acc, elem, listIndex ->
        newIndex =
            listIndex
            |> arrayIndexOf array.shape
            |> flipIndex array.shape Diagonal
            |> flipIndex newShape Y

        set acc newIndex elem

expect
    [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
    |> fromExactLists
    |> Result.map rotateClockwise
    |> Result.map toLists
    == Ok [[9, 5, 1], [10, 6, 2], [11, 7, 3], [12, 8, 4]]

rotateCounterClockwise : Array2D a -> Array2D a
rotateCounterClockwise = \@Array2D array ->
    newShape = { dimX: array.shape.dimY, dimY: array.shape.dimX }
    startAcc = @Array2D { array & shape: newShape }
    List.walkWithIndex array.data startAcc \acc, elem, listIndex ->
        newIndex =
            listIndex
            |> arrayIndexOf array.shape
            |> flipIndex array.shape Diagonal
            |> flipIndex newShape X

        set acc newIndex elem

expect
    [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
    |> fromExactLists
    |> Result.map rotateCounterClockwise
    |> Result.map toLists
    == Ok [[4, 8, 12], [3, 7, 11], [2, 6, 10], [1, 5, 9]]

countIf : Array2D a, (a -> Bool) -> Nat
countIf = \@Array2D array, fn -> List.countIf array.data fn

findFirstIndex : Array2D a, (a -> Bool) -> Result Index [NotFound]
findFirstIndex = \@Array2D array, fn ->
    array.data
    |> List.findFirstIndex fn
    |> Result.map \listIndex -> arrayIndexOf listIndex array.shape

findLastIndex : Array2D a, (a -> Bool) -> Result Index [NotFound]
findLastIndex = \@Array2D array, fn ->
    array.data
    |> List.findLastIndex fn
    |> Result.map \listIndex -> arrayIndexOf listIndex array.shape

isInBounds : Shape, Index -> Bool
isInBounds = \arrayShape, index ->
    index.x < arrayShape.dimX && index.y < arrayShape.dimY

listIndexOf : Shape, Index -> Nat
listIndexOf = \{ dimY }, { x, y } -> (x * dimY) + y

arrayIndexOf : Nat, Shape -> Index
arrayIndexOf = \index, { dimY } ->
    x = index // dimY
    y = index % dimY
    { x, y }

lastArrayIndex : Shape -> Index
lastArrayIndex = \{ dimX, dimY } -> { x: dimX - 1, y: dimY - 1 }

flipIndex : Index, Shape, [X, Y, Diagonal] -> Index
flipIndex = \{ x, y }, { dimX, dimY }, axis ->
    when axis is
        X -> { x: Num.absDiff x (dimX - 1), y }
        Y -> { x, y: Num.absDiff y (dimY - 1) }
        Diagonal -> { x: y, y: x }

expect flipIndex { x: 0, y: 0 } { dimX: 5, dimY: 1 } X == { x: 4, y: 0 }
expect flipIndex { x: 1, y: 0 } { dimX: 5, dimY: 1 } X == { x: 3, y: 0 }
expect flipIndex { x: 2, y: 0 } { dimX: 5, dimY: 1 } X == { x: 2, y: 0 }
expect flipIndex { x: 3, y: 0 } { dimX: 5, dimY: 1 } X == { x: 1, y: 0 }
expect flipIndex { x: 4, y: 0 } { dimX: 5, dimY: 1 } X == { x: 0, y: 0 }
expect flipIndex { x: 0, y: 0 } { dimX: 1, dimY: 4 } Y == { x: 0, y: 3 }
expect flipIndex { x: 0, y: 1 } { dimX: 1, dimY: 4 } Y == { x: 0, y: 2 }
expect flipIndex { x: 0, y: 2 } { dimX: 1, dimY: 4 } Y == { x: 0, y: 1 }
expect flipIndex { x: 0, y: 3 } { dimX: 1, dimY: 4 } Y == { x: 0, y: 0 }

isEq : Array2D a, Array2D a -> Bool where a implements Eq
isEq = \@Array2D a, @Array2D b -> a == b
