# Calculate glucose level quantiles

The function `quantile_glu` is a wrapper for the base function
[`quantile()`](https://rdrr.io/r/stats/quantile.html). Output is a
tibble object with columns for subject id and each of the quantiles.

## Usage

``` r
quantile_glu(data, quantiles = c(0, 25, 50, 75, 100))
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

- quantiles:

  List of quantile values between 0 and 100.

## Value

If a DataFrame object is passed, then a tibble object with a column for
subject id and then a column for each quantile value is returned. If a
vector of glucose values is passed, then a tibble object without the
subject id is returned.
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) can be wrapped
around the latter to output a numeric vector.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for each quantile is returned. `NA` glucose values are omitted
from the calculation of the quantiles.

The values are scaled from 0-1 to 0-100 to be consistent in output with
above_percent, below_percent, and in_range_percent.

The command `quantile_glu(...) / 100` will scale each element down from
0-100 to 0-1.

## Examples

``` r
data(example_data_1_subject)

quantile_glu(example_data_1_subject)
#> # A tibble: 1 × 6
#> # Groups:   id [1]
#>   id          `0`  `25`  `50`  `75` `100`
#>   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Subject 1    66    99   112   143   276
quantile_glu(example_data_1_subject, quantiles = c(0, 33, 66, 100))
#> # A tibble: 1 × 5
#> # Groups:   id [1]
#>   id          `0`  `33`  `66` `100`
#>   <fct>     <dbl> <dbl> <dbl> <dbl>
#> 1 Subject 1    66   104   128   276

data(example_data_5_subject)

quantile_glu(example_data_5_subject)
#> # A tibble: 5 × 6
#> # Groups:   id [5]
#>   id          `0`  `25`  `50`  `75` `100`
#>   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Subject 1    66    99   112   143   276
#> 2 Subject 2    90   179   211   253   400
#> 3 Subject 3    60   125   140   173   304
#> 4 Subject 4    50   109   126   149   232
#> 5 Subject 5    66   134   164   211   398
quantile_glu(example_data_5_subject, quantiles = c(0, 10, 90, 100))
#> # A tibble: 5 × 5
#> # Groups:   id [5]
#>   id          `0`  `10`  `90` `100`
#>   <fct>     <dbl> <dbl> <dbl> <dbl>
#> 1 Subject 1    66    92  172    276
#> 2 Subject 2    90   160  293    400
#> 3 Subject 3    60   110  226.   304
#> 4 Subject 4    50    94  167    232
#> 5 Subject 5    66   104  259    398
```
