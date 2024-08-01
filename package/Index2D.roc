interface Index2D exposes [
        Index2D,
        add,
        sub,
        incRow,
        incCol,
        decRow,
        decCol,
        adjacentTo,
        allAdjacentTo,
        flipRow,
        flipCol,
        transpose,
        first,
        last,
        isRowStart,
        isRowEnd,
        isColStart,
        isColEnd,
    ] imports [
        Shape2D.{ Shape2D },
    ]

## Two-dimensional index for referencing elements in an `Array2D`.
Index2D : { row : U64, col : U64 }

## Add indices, returning a new index with the sum of the two `row` values
## and sum of the two `col` values as component parts. If the resulting index
## is not within the array shape then return `Err OutOfBounds`.
add : Index2D, Index2D, Shape2D -> Result Index2D [OutOfBounds]
add = \indexA, indexB, shape ->
    indexSum = {
        row: indexA.row + indexB.row,
        col: indexA.col + indexB.col,
    }

    if Shape2D.hasIndex shape indexSum then
        Ok indexSum
    else
        Err OutOfBounds

## Subtract indices, returning a new index with the difference of the two `row`
## values and difference of the two `col` values as component parts. If the
## resulting index is not within the array shape then return `Err OutOfBounds`.
sub : Index2D, Index2D, Shape2D -> Result Index2D [OutOfBounds]
sub = \indexA, indexB, shape ->
    result =
        row <- Num.subChecked indexA.row indexB.row |> Result.try
        col <- Num.subChecked indexA.col indexB.col |> Result.try

        indexDif = { row, col }

        if Shape2D.hasIndex shape indexDif then
            Ok indexDif
        else
            Err OutOfBounds

    result |> Result.mapErr \_ -> OutOfBounds

## Go to the next row index within the array shape, wrapping to the next column
## when the end of a row is reached. If the next index is outside of the array
## shape then return `Err OutOfBounds`.
incRow : Index2D, Shape2D -> Result Index2D [OutOfBounds]
incRow = \index, shape ->
    if isColEnd index shape then
        if isRowEnd index shape then
            Err OutOfBounds
        else
            Ok { row: 0, col: index.col + 1 }
    else
        Ok { row: index.row + 1, col: index.col }

## Go to the next column index within the array shape, wrapping to the next row
## when the end of a column is reached. If the next index is outside of the
## array shape then return `Err OutOfBounds`.
incCol : Index2D, Shape2D -> Result Index2D [OutOfBounds]
incCol = \index, shape ->
    if isRowEnd index shape then
        if isColEnd index shape then
            Err OutOfBounds
        else
            Ok { row: index.row + 1, col: 0 }
    else
        Ok { row: index.row, col: index.col + 1 }

## Go to the previous row index within the array shape, wrapping to the
## previous column when the start of a row is reached. If the next index is
## outside of the array shape then return `Err OutOfBounds`.
decRow : Index2D, Shape2D -> Result Index2D [OutOfBounds]
decRow = \index, shape ->
    if isColStart index then
        if isRowStart index then
            Err OutOfBounds
        else
            Ok { row: shape.rows - 1, col: index.col - 1 }
    else
        Ok { row: index.row - 1, col: index.col }

## Go to the previous column index within the array shape, wrapping to the
## previous row when the start of a column is reached. If the next index is
## outside of the array shape then return `Err OutOfBounds`.
decCol : Index2D, Shape2D -> Result Index2D [OutOfBounds]
decCol = \index, shape ->
    if isRowStart index then
        if isColStart index then
            Err OutOfBounds
        else
            Ok { row: index.row - 1, col: shape.cols - 1 }
    else
        Ok { row: index.row, col: index.col - 1 }

## Try to get an index adjacent to the current index. If the specified adjacent
## index is outside of the array shape then return `Err OutOfBounds`. Note that
## `adjacent index shape SameRow SameCol` will return `Ok index`.
adjacentTo : Index2D, Shape2D, [PrevRow, SameRow, NextRow], [PrevCol, SameCol, NextCol] -> Result Index2D [OutOfBounds]
adjacentTo = \index, shape, rowRel, colRel ->
    addIndex = {
        row: if rowRel == NextRow then 1 else 0,
        col: if colRel == NextCol then 1 else 0,
    }

    subIndex = {
        row: if rowRel == PrevRow then 1 else 0,
        col: if colRel == PrevCol then 1 else 0,
    }

    add index addIndex shape
    |> Result.try \indexWithNext ->
        sub indexWithNext subIndex shape

## List all indices adjacent to the given index and within the array shape. The
## list will contain 0 to 8 indices, and will not include the given index.
allAdjacentTo : Index2D, Shape2D -> List Index2D
allAdjacentTo = \index, shape ->
    [
        adjacentTo index shape PrevRow PrevCol,
        adjacentTo index shape SameRow PrevCol,
        adjacentTo index shape NextRow PrevCol,
        adjacentTo index shape PrevRow SameCol,
        adjacentTo index shape NextRow SameCol,
        adjacentTo index shape PrevRow NextCol,
        adjacentTo index shape SameRow NextCol,
        adjacentTo index shape NextRow NextCol,
    ]
    |> List.keepOks \r -> r

## Change the `row` component of the index, reflecting over the center row or
## rows. This swaps the first row with the last row, second row with second to
## last row, etc.
flipRow : Index2D, Shape2D -> Result Index2D [OutOfBounds]
flipRow = \index, shape ->
    if Shape2D.hasIndex shape index then
        Ok {
            row: Num.absDiff index.row (shape.rows - 1),
            col: index.col,
        }
    else
        Err OutOfBounds

expect flipRow { row: 0, col: 0 } { rows: 5, cols: 1 } == Ok { row: 4, col: 0 }
expect flipRow { row: 1, col: 0 } { rows: 5, cols: 1 } == Ok { row: 3, col: 0 }
expect flipRow { row: 2, col: 0 } { rows: 5, cols: 1 } == Ok { row: 2, col: 0 }
expect flipRow { row: 3, col: 0 } { rows: 5, cols: 1 } == Ok { row: 1, col: 0 }
expect flipRow { row: 4, col: 0 } { rows: 5, cols: 1 } == Ok { row: 0, col: 0 }

## Change the `col` component of the index, reflecting over the center column
## or columns. This swaps the first column with the last column, second column
## with second to last column, etc.
flipCol : Index2D, Shape2D -> Result Index2D [OutOfBounds]
flipCol = \index, shape ->
    if Shape2D.hasIndex shape index then
        Ok {
            row: index.row,
            col: Num.absDiff index.col (shape.cols - 1),
        }
    else
        Err OutOfBounds

expect flipCol { row: 0, col: 0 } { rows: 1, cols: 4 } == Ok { row: 0, col: 3 }
expect flipCol { row: 0, col: 1 } { rows: 1, cols: 4 } == Ok { row: 0, col: 2 }
expect flipCol { row: 0, col: 2 } { rows: 1, cols: 4 } == Ok { row: 0, col: 1 }
expect flipCol { row: 0, col: 3 } { rows: 1, cols: 4 } == Ok { row: 0, col: 0 }

## Get the [transpose](https://en.wikipedia.org/wiki/Transpose) for an index.
## Swaps the `row` and `col` index components, reflecting the index over the
## center diagonal line. This operation is independent of array shape, since
## transposing an array also changes its shape.
transpose : Index2D -> Index2D
transpose = \{ row, col } -> { row: col, col: row }

## The first index within the given array shape. If the shape is for an empty
## array then return `Err ShapeWasEmpty`.
first : Shape2D -> Result Index2D [ShapeWasEmpty]
first = \shape ->
    if Shape2D.isEmpty shape then
        Err ShapeWasEmpty
    else
        Ok { row: 0, col: 0 }

## The last index within the given array shape. If the shape is for an empty
## array then return `Err ShapeWasEmpty`.
last : Shape2D -> Result Index2D [ShapeWasEmpty]
last = \shape ->
    if Shape2D.isEmpty shape then
        Err ShapeWasEmpty
    else
        Ok { row: shape.rows - 1, col: shape.cols - 1 }

## Predicate to determine if an index is the first index in a row.
isRowStart : Index2D -> Bool
isRowStart = \{ col } -> col == 0

expect isRowStart { row: 0, col: 0 } == Bool.true
expect isRowStart { row: 0, col: 1 } == Bool.false

## Predicate to determine if an index is the first index in a column.
isColStart : Index2D -> Bool
isColStart = \{ row } -> row == 0

expect isColStart { row: 0, col: 0 }
expect isColStart { row: 1, col: 0 } |> Bool.not

## Predicate to determine if an index is the last index in a row for a given
## array shape.
isRowEnd : Index2D, Shape2D -> Bool
isRowEnd = \{ col }, { cols } -> col + 1 >= cols

expect isRowEnd { row: 0, col: 0 } { rows: 1, cols: 1 }
expect isRowEnd { row: 0, col: 2 } { rows: 3, cols: 3 }
expect isRowEnd { row: 0, col: 5 } { rows: 3, cols: 2 }
expect isRowEnd { row: 0, col: 1 } { rows: 2, cols: 3 } |> Bool.not

## Predicate to determine if an index is the last index in a column for a given
## array shape.
isColEnd : Index2D, Shape2D -> Bool
isColEnd = \{ row }, { rows } -> row + 1 >= rows

expect isColEnd { row: 0, col: 0 } { rows: 1, cols: 1 }
expect isColEnd { row: 2, col: 0 } { rows: 3, cols: 2 }
expect isColEnd { row: 5, col: 0 } { rows: 3, cols: 3 }
expect isColEnd { row: 1, col: 0 } { rows: 3, cols: 2 } |> Bool.not
