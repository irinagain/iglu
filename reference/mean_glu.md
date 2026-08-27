# Calculate mean glucose level

The function mean_glu is a wrapper for the base function mean(). Output
is a tibble object with subject id and mean values.

## Usage

``` r
mean_glu(data)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding mean value is returned. If a vector of
glucose values is passed, then a tibble object with just the mean value
is returned. as.numeric() can be wrapped around the latter to output
just a numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for the mean values is returned. NA glucose values are omitted
from the calculation of the mean.

## Examples

``` r
data(example_data_1_subject)
mean_glu(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id         mean
#>   <fct>     <dbl>
#> 1 Subject 1  124.

data(example_data_5_subject)
mean_glu(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id         mean
#>   <fct>     <dbl>
#> 1 Subject 1  124.
#> 2 Subject 2  218.
#> 3 Subject 3  154.
#> 4 Subject 4  130.
#> 5 Subject 5  175.
```
