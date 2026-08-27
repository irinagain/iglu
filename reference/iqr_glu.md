# Calculate glucose level iqr

The function iqr_glu outputs the distance between the 25th percentile
and the 25th percentile of the glucose values in a tibble object.

## Usage

``` r
iqr_glu(data)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding IQR value is returned. If a vector of
glucose values is passed, then a tibble object with just the IQR value
is returned. as.numeric() can be wrapped around the latter to output
just a numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for the IQR values is returned. NA glucose values are omitted
from the calculation of the IQR.

## Examples

``` r
data(example_data_1_subject)
iqr_glu(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id          IQR
#>   <fct>     <dbl>
#> 1 Subject 1    44

data(example_data_5_subject)
iqr_glu(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id          IQR
#>   <fct>     <dbl>
#> 1 Subject 1    44
#> 2 Subject 2    74
#> 3 Subject 3    48
#> 4 Subject 4    40
#> 5 Subject 5    77
```
