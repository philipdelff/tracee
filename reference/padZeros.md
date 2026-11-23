# Pad Numbers with Leading Zeros

Converts numeric values to character strings with leading zeros to
achieve a specified total width.

## Usage

``` r
padZeros(x, nchars)
```

## Arguments

- x:

  A numeric vector of integers to be padded with leading zeros.

- nchars:

  An integer specifying the total number of characters in the output
  string (including leading zeros).

## Value

A character vector of the same length as `x`, with each element padded
with leading zeros to reach `nchars` characters.

## Examples

``` r
# Pad single digit numbers to 3 characters
padZeros(5, 3)
#> [1] "005"
# Returns: "005"

# Pad a vector of numbers
padZeros(c(1, 10, 100), 4)
#> [1] "0001" "0010" "0100"
# Returns: c("0001", "0010", "0100")

# Useful for creating sequential file names
paste0("file_", padZeros(1:5, 3), ".txt")
#> [1] "file_001.txt" "file_002.txt" "file_003.txt" "file_004.txt" "file_005.txt"
# Returns: c("file_001.txt", "file_002.txt", ..., "file_005.txt")

# Or append to an existing file name
NMdata::fnAppend("file.txt", padZeros(1:5, 3), collapse=NULL)
#> [1] "file_001.txt" "file_002.txt" "file_003.txt" "file_004.txt" "file_005.txt"
```
