# Calculate median glucose level

The function median_glu is a wrapper for the base function median().
Output is a tibble object with subject id and median values.

## Usage

``` r
median_glu(data)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding median value is returned. If a vector of
glucose values is passed, then a tibble object with just the median
value is returned. as.numeric() can be wrapped around the latter to
output just a numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for the median values is returned. NA glucose values are
omitted from the calculation of the median.

## Examples

``` r
data(example_data_1_subject)
median_glu(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id        median
#>   <fct>      <int>
#> 1 Subject 1    112

data(example_data_5_subject)
median_glu(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id        median
#>   <fct>      <dbl>
#> 1 Subject 1    112
#> 2 Subject 2    211
#> 3 Subject 3    140
#> 4 Subject 4    126
#> 5 Subject 5    164
```
