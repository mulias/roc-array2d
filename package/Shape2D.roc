interface Shape2D exposes [
        Shape2D,
        isEmpty,
        hasIndex,
        size,
    ] imports []

## Dimensions of an `Array2D` where `rows` is the total number of rows and
## `cols` is that total number of columns.
Shape2D : { rows : U64, cols : U64 }

Index2D : { row : U64, col : U64 }

## Predicate to determine if an array shape can contain indices. When the shape
## has zero rows or zero columns then there is no index that can fall within the
## shape bounds.
isEmpty : Shape2D -> Bool
isEmpty = \{ rows, cols } -> rows == 0 || cols == 0

## Predicate to determine if an index is within the bounds of an array shape.
hasIndex : Shape2D, Index2D -> Bool
hasIndex = \shape, index ->
    index.row < shape.rows && index.col < shape.cols

## Total number of elements within the array shape.
size : Shape2D -> U64
size = \{ rows, cols } -> rows * cols
