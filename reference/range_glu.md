# Calculate glucose level range

The function `range_glu` outputs the distance between minimum and
maximum glucose values per subject in a tibble object.

## Usage

``` r
range_glu(data)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

## Value

If a DataFrame object is passed, then a tibble object with two columns:
subject id and corresponding range value is returned. If a vector of
glucose values is passed, then a tibble object with just the range value
is returned. [`as.numeric()`](https://rdrr.io/r/base/numeric.html) can
be wrapped around the latter to output just a numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for the range values is returned. `NA` glucose values are
omitted from the calculation of the range.

## Examples

``` r
data(example_data_1_subject)
range_glu(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id        range
#>   <fct>     <int>
#> 1 Subject 1   210

data(example_data_5_subject)
range_glu(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id        range
#>   <fct>     <int>
#> 1 Subject 1   210
#> 2 Subject 2   310
#> 3 Subject 3   244
#> 4 Subject 4   182
#> 5 Subject 5   332
```
