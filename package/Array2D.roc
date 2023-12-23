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
        hasIndex,
        isEmpty,
        isRowStart,
        isRowEnd,
        isColStart,
        isColEnd,
        set,
        get,
        update,
        replace,
        swap,
        map,
        mapWithIndex,
        walk,
        walkUntil,
        first,
        last,
        firstIndex,
        lastIndex,
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
        joinWith,
        subarray,
        mul,
    ] imports []

## 2D coordinate for indexing into an `Array2D`.
Index : { x : Nat, y : Nat }

## Dimensions of an `Array2D`. An array of shape `{ rows, cols }` will contain
## indices from `{ row: 0, col: 0 }` to `{ row: rows - 1, col: cols - 1 }`.
Shape : { rows : Nat, cols : Nat }

## Fixed size multidimensional array. Elements with the same `x` index
## component share a row, while elements with the same `y` index component
## share a column. Unlike the builtin `List` data type `Array2D` is fully
## allocated on creation, with a starting element for each valid `Index`.
Array2D a := { data : List a, shape : Shape } implements [Eq { isEq: isEq }]

## Return an empty array with 0 rows, 0 columns, and no elements.
empty : {} -> Array2D *
empty = \{} -> @Array2D { data: [], shape: { rows: 0, cols: 0 } }

expect empty {} |> toList |> List.len == 0

## Return an [identity matrix](https://en.wikipedia.org/wiki/Identity_matrix)
## with the specified number of `1`s along the diagonal.
identity : Nat -> Array2D (Num *)
identity = \ones ->
    init { rows: ones, cols: ones } \{ x, y } -> if x == y then 1 else 0

expect identity 0 |> toLists == []
expect identity 1 |> toLists == [[1]]
expect identity 2 |> toLists == [[1, 0], [0, 1]]
expect identity 3 |> toLists == [[1, 0, 0], [0, 1, 0], [0, 0, 1]]

## Create an array with the same value repeated for each element.
repeat : a, Shape -> Array2D a
repeat = \elem, arrayShape ->
    @Array2D { data: List.repeat elem (shapeSize arrayShape), shape: arrayShape }

expect repeat Empty { rows: 3, cols: 2 } == @Array2D { data: [Empty, Empty, Empty, Empty, Empty, Empty], shape: { rows: 3, cols: 2 } }

## Create an array of given `Shape`, populating elements via an init function.
init : Shape, (Index -> a) -> Array2D a
init = \arrayShape, fn ->
    mapWithIndex (repeat Empty arrayShape) \_elem, index -> fn index

expect
    init { rows: 4, cols: 2 } \{ x, y } -> (x, y)
    == @Array2D { data: [(0, 0), (0, 1), (1, 0), (1, 1), (2, 0), (2, 1), (3, 0), (3, 1)], shape: { rows: 4, cols: 2 } }

## Fill an array of the given `Shape`, populating elements via a starting list
## and an init function. If there are not enough list elements to fill the
## array then the init function is ran with `Err Empty` as the element.
initWithList : List a, Shape, (Result a [Empty], Index -> b) -> Array2D b
initWithList = \list, arrayShape, fn ->
    list
    |> List.map Ok
    |> fromList (Fill (Err Empty) arrayShape)
    |> mapWithIndex fn

expect
    initWithList [1, 2, 3, 4] { rows: 2, cols: 3 } \elem, _ -> elem
    == @Array2D { data: [Ok 1, Ok 2, Ok 3, Ok 4, Err Empty, Err Empty], shape: { rows: 2, cols: 3 } }

expect
    initWithList [1, 2, 3, 4] { rows: 2, cols: 3 } \elem, _ -> Result.withDefault elem 0
    == @Array2D { data: [1, 2, 3, 4, 0, 0], shape: { rows: 2, cols: 3 } }

expect
    initWithList [1, 2, 3, 4] { rows: 2, cols: 3 } \elem, _ ->
        elem |> Result.map Some |> Result.withDefault Empty
    == @Array2D { data: [Some 1, Some 2, Some 3, Some 4, Empty, Empty], shape: { rows: 2, cols: 3 } }

## Fill an array of the given `Shape` with elements from a list of lists, via
## an init function. Each inner list is used to populate one row of the
## `Array2D`. If there are not enough lists, or if there are not enough
## elements in a list to fill an array row, then the init function is ran with
## `Err Empty` as the element.
initWithLists : List (List a), Shape, (Result a [Empty], Index -> b) -> Array2D b
initWithLists = \lists, arrayShape, fn ->
    lists
    |> List.map \list -> List.map list Ok
    |> fromLists (Fill (Err Empty) arrayShape)
    |> mapWithIndex fn

expect
    initWithLists [[1, 2, 3, 4], [1, 2]] { rows: 2, cols: 3 } \elem, _ -> elem |> Result.map Some |> Result.withDefault Empty
    == @Array2D { data: [Some 1, Some 2, Some 3, Some 1, Some 2, Empty], shape: { rows: 2, cols: 3 } }

## Create an array from a list. The `strategy` determines how the array is
## initialized:
##
##    * `Fit` - Return an array with 1 row and enough columns to exactly
##      fitting the list elements.
##
##    * `Fill default shape` - Return an array of `shape`. If there are not
##       enough list elements to fill the array then `default` is used for all
##       remaining elements.
fromList : List a, [Fit, Fill a Shape] -> Array2D a
fromList = \list, stratagy ->
    array = @Array2D { data: list, shape: { rows: 1, cols: List.len list } }
    when stratagy is
        Fit -> array
        Fill defaultElem arrayShape -> reshape array defaultElem arrayShape

expect
    fromList [1, 2, 3, 4] Fit
    == @Array2D { data: [1, 2, 3, 4], shape: { rows: 1, cols: 4 } }

expect
    fromList [1, 2, 3, 4, 5] (Fill 0 { rows: 4, cols: 2 })
    == @Array2D { data: [1, 2, 3, 4, 5, 0, 0, 0], shape: { rows: 4, cols: 2 } }

## Create an array from a list of lists. The `strategy` determines how the
## array is initialized:
##
##    * `FitShortest` - Return an array where each list is a row. The array has
##      columns equal to the number of elements in the shortest list. Any
##      additional elements are dropped per row.
##
##    * `FitLongest default` - Return an array where each list is a row. The
##      array has columns equal to the number of elements in the longest list.
##      If there are not enough list elements to fill a row then `default` is
##      used for all remaining values.
##
##    * `Fill default shape` - Return an array of `shape`. If there are not
##      enough lists or not enough elements in a list to fill a row then
##      `default` is used for all remaining values.
fromLists : List (List a), [FitShortest, FitLongest a, Fill a Shape] -> Array2D a
fromLists = \lists, stratagy ->
    when stratagy is
        FitShortest ->
            arrayShape = {
                rows: List.len lists,
                cols: lists |> List.map List.len |> List.min |> Result.withDefault 0,
            }
            startData = List.withCapacity (shapeSize arrayShape)
            data = List.walk lists startData \acc, list ->
                List.concat acc (List.takeFirst list arrayShape.cols)

            @Array2D { data, shape: arrayShape }

        FitLongest defaultElem ->
            arrayShape = {
                rows: List.len lists,
                cols: lists |> List.map List.len |> List.max |> Result.withDefault 0,
            }
            startData = List.withCapacity (shapeSize arrayShape)
            data = List.walk lists startData \acc, list ->
                List.concat acc (resize list defaultElem arrayShape.cols)

            @Array2D { data, shape: arrayShape }

        Fill defaultElem arrayShape ->
            paddedLists = resize lists [] arrayShape.rows
            startData = List.withCapacity (shapeSize arrayShape)
            data = List.walk paddedLists startData \acc, list ->
                List.concat acc (resize list defaultElem arrayShape.cols)

            @Array2D { data, shape: arrayShape }

expect
    fromLists [[1, 2, 3], [4]] FitShortest
    == @Array2D { data: [1, 4], shape: { rows: 2, cols: 1 } }

expect
    fromLists [[1, 2, 3], [4]] (FitLongest 0)
    == @Array2D { data: [1, 2, 3, 4, 0, 0], shape: { rows: 2, cols: 3 } }

expect
    fromLists [[1, 2, 3], [4]] (Fill -1 { rows: 4, cols: 2 })
    == @Array2D { data: [1, 2, 4, -1, -1, -1, -1, -1], shape: { rows: 4, cols: 2 } }

## Create an array of given `Shape`, populating elements via a starting list.
## If the list does not have enough elements to fill the array then return `Err
## NotEnoughElements`. If the list has more elements than the size of the
## array then return `Err TooManyElements`.
fromExactList : List a, Shape -> Result (Array2D a) [NotEnoughElements, TooManyElements]
fromExactList = \list, arrayShape ->
    if List.len list == shapeSize arrayShape then
        Ok (@Array2D { data: list, shape: arrayShape })
    else if List.len list < shapeSize arrayShape then
        Err NotEnoughElements
    else
        Err TooManyElements

expect
    fromExactList [1, 2, 3, 4, 5, 6] { rows: 2, cols: 3 }
    == Ok (@Array2D { data: [1, 2, 3, 4, 5, 6], shape: { rows: 2, cols: 3 } })

expect fromExactList [1, 2, 3, 4] { rows: 2, cols: 3 } == Err NotEnoughElements

expect fromExactList [1, 2, 3, 4, 5, 6, 7] { rows: 2, cols: 3 } == Err TooManyElements

## Create an array from a list of lists. If the row lists do not all have the
## same length then return `Err InconsistentRowLengths`.
fromExactLists : List (List a) -> Result (Array2D a) [InconsistentRowLengths]
fromExactLists = \lists ->
    firstRow = lists |> List.first |> Result.withDefault []

    if List.all lists \list -> List.len list == List.len firstRow then
        array = @Array2D {
            data: List.join lists,
            shape: { rows: List.len lists, cols: List.len firstRow },
        }
        Ok array
    else
        Err InconsistentRowLengths

expect
    fromExactLists [[1, 2], [3, 4], [5, 6]]
    == Ok (@Array2D { data: [1, 2, 3, 4, 5, 6], shape: { rows: 3, cols: 2 } })

expect fromExactLists [[1, 2, 3], [3, 4], [5, 6]] == Err InconsistentRowLengths

expect fromExactLists [[1, 2], [3, 4], [5, 6, 7]] == Err InconsistentRowLengths

## Get the X/Y dimensions of an array.
shape : Array2D * -> Shape
shape = \@Array2D array -> array.shape

expect repeat 0 { rows: 3, cols: 4 } |> shape == { rows: 3, cols: 4 }
expect empty {} |> shape == { rows: 0, cols: 0 }

## Get the total number of elements in an array.
size : Array2D * -> Nat
size = \@Array2D array -> shapeSize array.shape

expect repeat 0 { rows: 3, cols: 4 } |> size == 12
expect empty {} |> size == 0

## Predicate to determine if an index is within the bounds of an array.
hasIndex : Array2D *, Index -> Bool
hasIndex = \@Array2D { shape: { rows, cols } }, index ->
    index.x < rows && index.y < cols

expect repeat 0 { rows: 3, cols: 2 } |> hasIndex { x: 0, y: 0 } == Bool.true
expect repeat 0 { rows: 3, cols: 2 } |> hasIndex { x: 2, y: 1 } == Bool.true
expect repeat 0 { rows: 3, cols: 2 } |> hasIndex { x: 0, y: 2 } == Bool.false
expect repeat 0 { rows: 3, cols: 2 } |> hasIndex { x: 3, y: 0 } == Bool.false

## Predicate to determine if an array has zero elements. An empty array will
## have at least one dimension of length 0.
isEmpty : Array2D * -> Bool
isEmpty = \@Array2D { data } -> List.isEmpty data

expect isEmpty (empty {})
expect !(repeat 0 { rows: 2, cols: 4 } |> isEmpty)

## Predicate to determine if an index is the first index in a row.
isRowStart : Index -> Bool
isRowStart = \{ y } -> y == 0

expect isRowStart { x: 0, y: 0 } == Bool.true
expect isRowStart { x: 0, y: 1 } == Bool.false

## Predicate to determine if an index is the last index in a row for a given
## array.
isRowEnd : Array2D *, Index -> Bool
isRowEnd = \@Array2D { shape: { cols } }, { y } -> y + 1 == cols

expect identity 1 |> isRowEnd { x: 0, y: 0 } == Bool.true
expect identity 3 |> isRowEnd { x: 0, y: 2 } == Bool.true
expect identity 3 |> isRowEnd { x: 0, y: 5 } == Bool.false
expect repeat X { rows: 2, cols: 3 } |> isRowEnd { x: 0, y: 1 } == Bool.false

## Predicate to determine if an index is the first index in a column.
isColStart : Index -> Bool
isColStart = \{ x } -> x == 0

expect isColStart { x: 0, y: 0 } == Bool.true
expect isColStart { x: 1, y: 0 } == Bool.false

## Predicate to determine if an index is the last index in a column for a given
## array.
isColEnd : Array2D *, Index -> Bool
isColEnd = \@Array2D { shape: { rows } }, { x } -> x + 1 == rows

expect identity 1 |> isColEnd { x: 0, y: 0 } == Bool.true
expect identity 3 |> isColEnd { x: 2, y: 0 } == Bool.true
expect identity 3 |> isColEnd { x: 5, y: 0 } == Bool.false
expect repeat X { rows: 3, cols: 2 } |> isColEnd { x: 1, y: 0 } == Bool.false

## Change the element at the given index. If the index is outside the bounds of
## the array, return the original array unmodified.
set : Array2D a, Index, a -> Array2D a
set = \@Array2D array, index, elem ->
    array
    |> .shape
    |> listIndexOf index
    |> Result.map \listIndex ->
        @Array2D { array & data: List.set array.data listIndex elem }
    |> Result.withDefault (@Array2D array)

expect
    repeat 0 { rows: 3, cols: 3 }
    |> set { x: 1, y: 1 } 9
    == @Array2D { data: [0, 0, 0, 0, 9, 0, 0, 0, 0], shape: { rows: 3, cols: 3 } }

expect
    repeat 0 { rows: 3, cols: 3 }
    |> set { x: 1, y: 4 } 9
    == @Array2D { data: [0, 0, 0, 0, 0, 0, 0, 0, 0], shape: { rows: 3, cols: 3 } }

## Attempt to retrieve the array value at a given index. If the index is not
## within the array then return `Err OutOfBounds`.
get : Array2D a, Index -> Result a [OutOfBounds]
get = \@Array2D array, index ->
    array
    |> .shape
    |> listIndexOf index
    |> Result.try \listIndex ->
        List.get array.data listIndex

expect identity 4 |> get { x: 1, y: 1 } == Ok 1
expect identity 4 |> get { x: 1, y: 2 } == Ok 0
expect identity 4 |> get { x: 4, y: 2 } == Err OutOfBounds

## Update the value at the given index with the given function.
## If the index is outside the bounds of the array, return the original
## array unmodified.
update : Array2D a, Index, (a -> a) -> Array2D a
update = \@Array2D array, index, fn ->
    array
    |> .shape
    |> listIndexOf index
    |> Result.map \listIndex ->
        @Array2D {
            data: List.update array.data listIndex fn,
            shape: array.shape,
        }
    |> Result.withDefault (@Array2D array)

expect
    repeat 0 { rows: 3, cols: 3 }
    |> update { x: 1, y: 1 } \n -> n + 1
    == @Array2D { data: [0, 0, 0, 0, 1, 0, 0, 0, 0], shape: { rows: 3, cols: 3 } }

expect
    repeat 0 { rows: 3, cols: 3 }
    |> update { x: 1, y: 4 } \n -> n + 1
    == @Array2D { data: [0, 0, 0, 0, 0, 0, 0, 0, 0], shape: { rows: 3, cols: 3 } }

## Update the value at the given index, returning the new array and the
## replaced value. If the index is outside the bounds of the array, return the
## original array unmodified and the intended replacement value as `value`.
replace : Array2D a, Index, a -> { array : Array2D a, value : a }
replace = \@Array2D array, index, newValue ->
    array
    |> .shape
    |> listIndexOf index
    |> Result.map \listIndex ->
        { list, value } = List.replace array.data listIndex newValue
        { array: @Array2D { array & data: list }, value }
    |> Result.withDefault { array: @Array2D array, value: newValue }

expect
    [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    |> fromLists FitShortest
    |> replace { x: 1, y: 2 } 0
    == {
        array: @Array2D {
            data: [1, 2, 3, 4, 5, 0, 7, 8, 9],
            shape: { rows: 3, cols: 3 },
        },
        value: 6,
    }

expect
    [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    |> fromLists FitShortest
    |> replace { x: 1, y: 10 } 0
    == {
        array: @Array2D {
            data: [1, 2, 3, 4, 5, 6, 7, 8, 9],
            shape: { rows: 3, cols: 3 },
        },
        value: 0,
    }

## Exchange elements at two array indices. If either index is out of bounds
## return the array unchanged.
swap : Array2D a, Index, Index -> Array2D a
swap = \@Array2D array, indexA, indexB ->
    result =
        listIndexA <- listIndexOf array.shape indexA |> Result.try
        listIndexB <- listIndexOf array.shape indexB |> Result.map
        @Array2D { array & data: List.swap array.data listIndexA listIndexB }

    result |> Result.withDefault (@Array2D array)

expect
    [[X, A, A], [A, Y, A], [A, A, A]]
    |> fromLists FitShortest
    |> swap { x: 0, y: 0 } { x: 1, y: 1 }
    |> toLists
    == [[Y, A, A], [A, X, A], [A, A, A]]

expect
    [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    |> fromLists FitShortest
    |> swap { x: 0, y: 0 } { x: 1, y: 3 }
    |> toLists
    == [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

## Convert each element in the array to something new via a conversion
## function. Return a new array of the converted values.
map : Array2D a, (a -> b) -> Array2D b
map = \@Array2D array, fn ->
    @Array2D { data: List.map array.data fn, shape: array.shape }

expect
    repeat 0 { rows: 2, cols: 3 }
    |> map \n -> n + 1
    == repeat 1 { rows: 2, cols: 3 }

## Similar to `Array2D.map`, also provide the index for each element.
mapWithIndex : Array2D a, (a, Index -> b) -> Array2D b
mapWithIndex = \@Array2D array, fn -> @Array2D {
        data: List.mapWithIndex array.data \elem, listIndex ->
            fn elem (arrayIndexOf listIndex array.shape),
        shape: array.shape,
    }

expect
    @Array2D { data: [1, 2, 3, 4], shape: { rows: 2, cols: 2 } }
    |> mapWithIndex \n, idx -> (n, idx)
    == @Array2D {
        data: [
            (1, { x: 0, y: 0 }),
            (2, { x: 0, y: 1 }),
            (3, { x: 1, y: 0 }),
            (4, { x: 1, y: 1 }),
        ],
        shape: { rows: 2, cols: 2 },
    }

WalkOptions a : {
    direction : [Forwards, Backwards],
    orientation ? [Rows, Cols],
    start ? Index,
}a

## Build a value using each element in the array.
##
## Starting with a given `state` value, walk through the array elements in the
## order specified by the `options` record, running a given `step` function to
## produce a new `state`.
##
## Options:
##
##    * `direction` - Required, `Forwards` to traverse elements from the first
##      index to the last index, `Backwards` to go from the last index to the
##      first index.
##
##    * `orientation` - Optional, defaults to `Rows`. Determines if elements
##      are visited in [row-major or column-major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order).
##
##    * `start` - Optional, defaults to the first index for `Forwards` and the
##      last index for `Backwards`. The index to start walking from. If the
##      `start` index is outside of the array bounds then either no elements
##      will be walked `Forwards`, or all elements will be walked `Backwards`.
walk : Array2D a, state, WalkOptions *, (state, a, Index -> state) -> state
walk = \array, startState, options, fn ->
    direction = options.direction
    { orientation ? Rows, start ? walkStart array options.direction } = options

    if direction == Forwards && orientation == Rows && start == { x: 0, y: 0 } then
        (@Array2D { data, shape: arrayShape }) = array
        List.walkWithIndex data startState \state, elem, listIndex ->
            fn state elem (arrayIndexOf listIndex arrayShape)
    else
        walkUntil array startState options \state, elem, index ->
            Continue (fn state elem index)

expect
    repeat 0 { rows: 3, cols: 2 }
    |> walk [] { direction: Forwards } \acc, _, { x, y } -> List.append acc (x, y)
    == [(0, 0), (0, 1), (1, 0), (1, 1), (2, 0), (2, 1)]

expect
    repeat 0 { rows: 3, cols: 2 }
    |> walk [] { direction: Backwards } \acc, _, { x, y } -> List.append acc (x, y)
    == [(2, 1), (2, 0), (1, 1), (1, 0), (0, 1), (0, 0)]

expect
    repeat 0 { rows: 3, cols: 2 }
    |> walk [] { direction: Forwards, orientation: Cols, start: { x: 0, y: 0 } } \acc, _, { x, y } -> List.append acc (x, y)
    == [(0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1)]

expect
    repeat 0 { rows: 3, cols: 2 }
    |> walk [] { direction: Backwards, orientation: Cols } \acc, _, { x, y } -> List.append acc (x, y)
    == [(2, 1), (1, 1), (0, 1), (2, 0), (1, 0), (0, 0)]

expect
    repeat 0 { rows: 3, cols: 2 }
    |> walk [] { direction: Forwards, start: { x: 1, y: 1 } } \acc, _, { x, y } -> List.append acc (x, y)
    == [(1, 1), (2, 0), (2, 1)]

expect
    repeat 0 { rows: 3, cols: 2 }
    |> walk [] { direction: Backwards, start: { x: 1, y: 1 } } \acc, _, { x, y } -> List.append acc (x, y)
    == [(1, 1), (1, 0), (0, 1), (0, 0)]

expect
    repeat 0 { rows: 3, cols: 2 }
    |> walk [] { direction: Forwards, start: { x: 10, y: 10 } } \acc, _, { x, y } -> List.append acc (x, y)
    == []

expect
    repeat 0 { rows: 3, cols: 2 }
    |> walk [] { direction: Backwards, start: { x: 10, y: 10 } } \acc, _, { x, y } -> List.append acc (x, y)
    == [(2, 1), (2, 0), (1, 1), (1, 0), (0, 1), (0, 0)]

## Similar to `Array2D.walk`, but the `step` function either continues walking
## with a `Continue`, or stops early with a `Break`.
walkUntil : Array2D a, state, WalkOptions *, (state, a, Index -> [Continue state, Break state]) -> state
walkUntil = \array, startState, options, fn ->
    direction = options.direction
    { orientation ? Rows, start ? walkStart array options.direction } = options

    # When walking backwards it's ok for the start index to be outside the
    # array bounds. In this case start at the last valid index.
    boundedStart =
        if direction == Backwards && !(hasIndex array start) then
            array |> lastIndex |> Result.withDefault { x: 0, y: 0 }
        else
            start

    when (direction, orientation) is
        (Forwards, Rows) -> walkRowsUntil array boundedStart startState fn
        (Backwards, Rows) -> walkRowsBackwardsUntil array boundedStart startState fn
        (Forwards, Cols) -> walkColsUntil array boundedStart startState fn
        (Backwards, Cols) -> walkColsBackwardsUntil array boundedStart startState fn

expect
    [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    |> fromLists FitShortest
    |> walkUntil 0 { direction: Forwards } \acc, elem, _index ->
        if elem == 5 then
            Break acc
        else
            Continue (acc + elem)
    == 10

expect
    [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    |> fromLists FitShortest
    |> walkUntil 0 { direction: Backwards } \acc, elem, _index ->
        if elem == 5 then
            Break acc
        else
            Continue (acc + elem)
    == 30

## Return the first element in the array, or `Err ArrayWasEmpty` if the array
## was empty.
first : Array2D a -> Result a [ArrayWasEmpty]
first = \array ->
    array
    |> firstIndex
    |> Result.try \index -> get array index
    |> Result.mapErr \_ -> ArrayWasEmpty

expect empty {} |> first == Err ArrayWasEmpty
expect @Array2D { data: [5, 2, 3, 4], shape: { rows: 2, cols: 2 } } |> first == Ok 5

## Return the last element in the array, or `Err ArrayWasEmpty` if the array
## was empty.
last : Array2D a -> Result a [ArrayWasEmpty]
last = \array ->
    array
    |> lastIndex
    |> Result.try \index -> get array index
    |> Result.mapErr \_ -> ArrayWasEmpty

expect empty {} |> last == Err ArrayWasEmpty
expect @Array2D { data: [5, 2, 3, 4], shape: { rows: 2, cols: 2 } } |> last == Ok 4

## Return the first index in the array, or `Err ArrayWasEmpty` if the array was
## empty.
firstIndex : Array2D * -> Result Index [ArrayWasEmpty]
firstIndex = \array ->
    if isEmpty array then
        Err ArrayWasEmpty
    else
        Ok { x: 0, y: 0 }

expect empty {} |> firstIndex == Err ArrayWasEmpty
expect @Array2D { data: [5, 2, 3, 4], shape: { rows: 2, cols: 2 } } |> firstIndex == Ok { x: 0, y: 0 }

## Return the last index in the array, or `Err ArrayWasEmpty` if the array was
## empty.
lastIndex : Array2D * -> Result Index [ArrayWasEmpty]
lastIndex = \array ->
    if isEmpty array then
        Err ArrayWasEmpty
    else
        { rows, cols } = shape array
        Ok { x: rows - 1, y: cols - 1 }

expect empty {} |> lastIndex == Err ArrayWasEmpty
expect identity 4 |> lastIndex == Ok { x: 3, y: 3 }

## Convert an array to a flat list of elements.
toList : Array2D a -> List a
toList = \@Array2D { data } -> data

expect toList (repeat Empty { rows: 2, cols: 2 }) == [Empty, Empty, Empty, Empty]

## Convert an array to a list of lists, where each inner list is one array row.
toLists : Array2D a -> List (List a)
toLists = \array ->
    (@Array2D { data, shape: { rows, cols } }) = array
    xIndicies = List.range { start: At 0, end: Before rows }
    startState = List.withCapacity rows

    List.walk xIndicies startState \state, xIndex ->
        startIndex =
            when listIndexOf { rows, cols } { x: xIndex, y: 0 } is
                Ok listIndex -> listIndex
                Err OutOfBounds -> crash "Index should not be out of bounds"

        row = List.sublist data { start: startIndex, len: cols }
        List.append state row

expect toLists (repeat 0 { rows: 2, cols: 2 }) == [[0, 0], [0, 0]]

expect
    toLists (fromList [1, 2, 3, 4, 5] (Fill 0 { rows: 2, cols: 3 }))
    == [[1, 2, 3], [4, 5, 0]]

## Change the shape of an array. Elements maintain [row-major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order)
## but might be wrapped onto a new row by the change in dimensions. If the new
## array size is smaller then elements at the end will be truncated. If the new
## array size is larger then new elements will be set to the given `default`
## value.
reshape : Array2D a, a, Shape -> Array2D a
reshape = \@Array2D { data }, defaultValue, newShape ->
    @Array2D { data: resize data defaultValue (shapeSize newShape), shape: newShape }

expect repeat 1 { rows: 3, cols: 3 } |> reshape 9 { rows: 1, cols: 1 } |> toLists == [[1]]

expect
    repeat 1 { rows: 3, cols: 3 }
    |> reshape 9 { rows: 4, cols: 3 }
    |> toLists
    == [[1, 1, 1], [1, 1, 1], [1, 1, 1], [9, 9, 9]]

## Take the [transpose](https://en.wikipedia.org/wiki/Transpose) of a matrix.
## This operation flips the array values along the diagonal.
transpose : Array2D a -> Array2D a
transpose = \@Array2D array ->
    startAcc = @Array2D { array & shape: { rows: array.shape.cols, cols: array.shape.rows } }
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

## Swap the position of each element so that their `x` component changes and
## `y` component stays the same. This exchanges the top row of the array with
## the bottom row, the second row with the second to last row, etc.
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

## Swap the position of each element so that their `y` component changes and
## `x` component stays the same. This exchanges the first element of each row
## with the last element of the row, the second element with the second to last
## element, etc.
flipY : Array2D a -> Array2D a
flipY = \@Array2D array ->
    startAcc = @Array2D array
    List.walkWithIndex array.data startAcc \acc, elem, listIndex ->
        newIndex =
            listIndex
            |> arrayIndexOf array.shape
            |> flipIndex array.shape Y

        set acc newIndex elem

expect
    [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
    |> fromExactLists
    |> Result.map flipY
    |> Result.map toLists
    == Ok [[4, 3, 2, 1], [8, 7, 6, 5], [12, 11, 10, 9]]

## Rotate every array element clockwise 90 degrees. This swaps the X and Y
## dimensions of the array.
rotateClockwise : Array2D a -> Array2D a
rotateClockwise = \@Array2D array ->
    newShape = { rows: array.shape.cols, cols: array.shape.rows }
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

## Rotate every array element counter-clockwise 90 degrees. This swaps the X
## and Y dimensions of the array.
rotateCounterClockwise : Array2D a -> Array2D a
rotateCounterClockwise = \@Array2D array ->
    newShape = { rows: array.shape.cols, cols: array.shape.rows }
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

## Run the given function on each element of an array and return the number of
## elements for which the function returned Bool.true.
countIf : Array2D a, (a -> Bool) -> Nat
countIf = \@Array2D array, fn -> List.countIf array.data fn

## Return the index of the first element in the array satisfying a predicate
## function. If no satisfying element is found return `Err NotFound`.
findFirstIndex : Array2D a, (a -> Bool) -> Result Index [NotFound]
findFirstIndex = \@Array2D array, fn ->
    array.data
    |> List.findFirstIndex fn
    |> Result.map \listIndex -> arrayIndexOf listIndex array.shape

## Return the index of the last element in the array satisfying a predicate
## function. If no satisfying element is found return `Err NotFound`.
findLastIndex : Array2D a, (a -> Bool) -> Result Index [NotFound]
findLastIndex = \@Array2D array, fn ->
    array.data
    |> List.findLastIndex fn
    |> Result.map \listIndex -> arrayIndexOf listIndex array.shape

## Concatenate an array of strings into a single string, interspersing each
## element with an element separator and each row with a row separator.
joinWith : Array2D Str, Str, Str -> Str
joinWith = \array, elemSep, rowSep ->
    array
    |> Array2D.toLists
    |> List.map \row -> Str.joinWith row elemSep
    |> Str.joinWith rowSep

expect
    [["a", "b", "c"], ["d", "e", "f"], ["g", "h", "j"]]
    |> fromLists FitShortest
    |> joinWith "," "|"
    == "a,b,c|d,e,f|g,h,j"

## Create a new array containing elements from a rectangular area within the
## given array. The `firstCorner` and `secondCorner` indices can specify any
## two corners of the subarray area.
subarray : Array2D a, Index, Index -> Array2D a
subarray = \array, firstCorner, secondCorner ->
    topLeft = {
        x: Num.min firstCorner.x secondCorner.x,
        y: Num.min firstCorner.y secondCorner.y,
    }

    if hasIndex array topLeft then
        limit = lastIndex array |> Result.withDefault { x: 0, y: 0 }

        bottomRight = {
            x: Num.max firstCorner.x secondCorner.x |> Num.min limit.x,
            y: Num.max firstCorner.y secondCorner.y |> Num.min limit.y,
        }

        rows = bottomRight.x - topLeft.x + 1
        cols = bottomRight.y - topLeft.y + 1

        init { rows, cols } \{ x, y } ->
            when get array { x: topLeft.x + x, y: topLeft.y + y } is
                Ok elem -> elem
                Err OutOfBounds -> crash "Unexpected error creating subarray"
    else
        @Array2D { data: [], shape: { rows: 0, cols: 0 } }

expect
    fromLists [[1, 2, 3], [4, 5, 6], [7, 8, 9]] FitShortest
    |> subarray { x: 0, y: 1 } { x: 2, y: 2 }
    |> toLists
    == [[2, 3], [5, 6], [8, 9]]

expect
    fromLists [[1, 2, 3], [4, 5, 6], [7, 8, 9]] FitShortest
    |> subarray { x: 2, y: 2 } { x: 0, y: 1 }
    |> toLists
    == [[2, 3], [5, 6], [8, 9]]

expect
    fromLists [[1, 2, 3], [4, 5, 6], [7, 8, 9]] FitShortest
    |> subarray { x: 2, y: 1 } { x: 0, y: 2 }
    |> toLists
    == [[2, 3], [5, 6], [8, 9]]

expect
    fromLists [[1, 2, 3], [4, 5, 6], [7, 8, 9]] FitShortest
    |> subarray { x: 0, y: 2 } { x: 2, y: 1 }
    |> toLists
    == [[2, 3], [5, 6], [8, 9]]

expect
    fromLists [[1, 2, 3], [4, 5, 6], [7, 8, 9]] FitShortest
    |> subarray { x: 0, y: 0 } { x: 10, y: 10 }
    |> toLists
    == [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

expect
    fromLists [[1, 2, 3], [4, 5, 6], [7, 8, 9]] FitShortest
    |> subarray { x: 0, y: 10 } { x: 10, y: 0 }
    |> toLists
    == [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

expect
    fromLists [[1, 2, 3], [4, 5, 6], [7, 8, 9]] FitShortest
    |> subarray { x: 0, y: 2 } { x: 10, y: 10 }
    |> toLists
    == [[3], [6], [9]]

expect
    fromLists [[1, 2, 3, 0], [4, 5, 6, 0], [7, 8, 9, 0]] FitShortest
    |> subarray { x: 5, y: 5 } { x: 10, y: 10 }
    |> toLists
    == []

## Perform [matrix multiplication](https://en.wikipedia.org/wiki/Matrix_multiplication)
## on two arrays. A prerequisite of this operation is that the number of columns in
## the first array must match the number of rows in the second array. If this
## doesn't hold then return `Err InnerDimensionsMismatch`.
mul : Array2D (Num a), Array2D (Num a) -> Result (Array2D (Num a)) [InnerDimensionsMismatch]
mul = \a, b ->
    shapeA = shape a
    shapeB = shape b

    if shapeA.cols == shapeB.rows then
        sharedIndices = List.range { start: At 0, end: Before shapeA.cols }

        product =
            init { rows: shapeA.rows, cols: shapeB.cols } \{ x, y } ->
                List.walk sharedIndices 0 \state, index ->
                    when (get a { x, y: index }, get b { x: index, y }) is
                        (Ok elemA, Ok elemB) -> state + (elemA * elemB)
                        (_, _) -> crash "Unexpected error multiplying arrays"

        Ok product
    else
        Err InnerDimensionsMismatch

expect
    a = fromLists [[1, 2], [3, 4]] FitShortest
    b = fromLists [[5, 6], [7, 8]] FitShortest
    c = fromLists
        [
            [(1 * 5) + (2 * 7), (1 * 6) + (2 * 8)],
            [(3 * 5) + (4 * 7), (3 * 6) + (4 * 8)],
        ]
        FitShortest
    mul a b == Ok c

expect
    a = fromLists [[1, 2, 3, 4]] FitShortest
    b = fromLists [[2, 1, 2], [4, 3, 5], [3, 7, 9], [2, 1, 7]] FitShortest
    c = fromLists [[27, 32, 67]] FitShortest
    mul a b == Ok c

expect
    a = fromLists [[1, 2, 3]] FitShortest
    b = fromLists [[2, 1, 2], [4, 3, 5], [3, 7, 9], [2, 1, 7]] FitShortest
    mul a b == Err InnerDimensionsMismatch

expect
    a = fromLists [[1, 2], [3, 4], [5, 6]] FitShortest
    mul a (identity 2) == Ok a

expect
    a = fromLists [[1, 2, 3], [4, 5, 6]] FitShortest
    mul a (identity 3) == Ok a

isEq : Array2D a, Array2D a -> Bool where a implements Eq
isEq = \@Array2D a, @Array2D b -> a == b

shapeSize : Shape -> Nat
shapeSize = \{ rows, cols } -> rows * cols

isInShape : Shape, Index -> Bool
isInShape = \arrayShape, index ->
    index.x < arrayShape.rows && index.y < arrayShape.cols

listIndexOf : Shape, Index -> Result Nat [OutOfBounds]
listIndexOf = \arrayShape, index ->
    if isInShape arrayShape index then
        Ok ((index.x * arrayShape.cols) + index.y)
    else
        Err OutOfBounds

arrayIndexOf : Nat, Shape -> Index
arrayIndexOf = \index, { cols } ->
    x = index // cols
    y = index % cols
    { x, y }

flipIndex : Index, Shape, [X, Y, Diagonal] -> Index
flipIndex = \{ x, y }, { rows, cols }, axis ->
    when axis is
        X -> { x: Num.absDiff x (rows - 1), y }
        Y -> { x, y: Num.absDiff y (cols - 1) }
        Diagonal -> { x: y, y: x }

expect flipIndex { x: 0, y: 0 } { rows: 5, cols: 1 } X == { x: 4, y: 0 }
expect flipIndex { x: 1, y: 0 } { rows: 5, cols: 1 } X == { x: 3, y: 0 }
expect flipIndex { x: 2, y: 0 } { rows: 5, cols: 1 } X == { x: 2, y: 0 }
expect flipIndex { x: 3, y: 0 } { rows: 5, cols: 1 } X == { x: 1, y: 0 }
expect flipIndex { x: 4, y: 0 } { rows: 5, cols: 1 } X == { x: 0, y: 0 }
expect flipIndex { x: 0, y: 0 } { rows: 1, cols: 4 } Y == { x: 0, y: 3 }
expect flipIndex { x: 0, y: 1 } { rows: 1, cols: 4 } Y == { x: 0, y: 2 }
expect flipIndex { x: 0, y: 2 } { rows: 1, cols: 4 } Y == { x: 0, y: 1 }
expect flipIndex { x: 0, y: 3 } { rows: 1, cols: 4 } Y == { x: 0, y: 0 }

resize : List a, a, Nat -> List a
resize = \list, defaultValue, newLen ->
    oldLen = List.len list
    if newLen < oldLen then
        List.takeFirst list newLen
    else if newLen > oldLen then
        List.concat list (List.repeat defaultValue (newLen - oldLen))
    else
        list

walkStart : Array2D a, [Forwards, Backwards] -> Index
walkStart = \array, direction ->
    when direction is
        Forwards -> { x: 0, y: 0 }
        Backwards -> array |> lastIndex |> Result.withDefault { x: 0, y: 0 }

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
    when get array index is
        Ok elem ->
            when (nextIndexFn array index, nextStateFn state elem index) is
                (Ok nextIndex, Continue nextState) ->
                    iterate array nextIndex nextState nextIndexFn nextStateFn

                (_, Continue nextState) -> nextState
                (_, Break nextState) -> nextState

        Err OutOfBounds ->
            state

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
            Ok { x: index.x - 1, y: array.shape.cols - 1 }
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
            Ok { x: array.shape.rows - 1, y: index.y - 1 }
    else
        Ok { x: index.x - 1, y: index.y }
