# Calculate sd glucose level

The function `sd_glu` is a wrapper for the base function
[`sd()`](https://rdrr.io/r/stats/sd.html). Output is a tibble object
with subject id and sd values.

## Usage

``` r
sd_glu(data)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding sd value is returned. If a vector of
glucose values is passed, then a tibble object with just the sd value is
returned. [`as.numeric()`](https://rdrr.io/r/base/numeric.html) can be
wrapped around the latter to output just a numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for the sd values is returned. `NA` glucose values are omitted
from the calculation of the sd.

## Examples

``` r
data(example_data_1_subject)
sd_glu(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id           SD
#>   <fct>     <dbl>
#> 1 Subject 1  33.3

data(example_data_5_subject)
sd_glu(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id           SD
#>   <fct>     <dbl>
#> 1 Subject 1  33.3
#> 2 Subject 2  52.4
#> 3 Subject 3  44.8
#> 4 Subject 4  29.1
#> 5 Subject 5  58.6
```
