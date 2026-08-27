# Calculate summary glucose level

The function \`summary_glu\` is a wrapper for the base function
\`summary()\`. Output is a tibble object with subject id and the summary
value: Minimum, 1st Quantile, Median, Mean, 3rd Quantile and Max.

## Usage

``` r
summary_glu(data)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

## Value

If a DataFrame object is passed, then a tibble object with a column for
subject id and then a column for each summary value is returned. If a
vector of glucose values is passed, then a tibble object without the
subject id is returned. \`as.numeric()\` can be wrapped around the
latter to output a numeric vector with values in order of Min, 1st
Quantile, Median, Mean, 3rd Quantile and Max.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for each of summary values is returned. \`NA\` glucose values
are omitted from the calculation of the summary values.

## Examples

``` r
data(example_data_1_subject)
summary_glu(example_data_1_subject)
#> # A tibble: 1 × 7
#> # Groups:   id [1]
#>   id         Min. `1st Qu.` Median  Mean `3rd Qu.`  Max.
#>   <fct>     <dbl>     <dbl>  <dbl> <dbl>     <dbl> <dbl>
#> 1 Subject 1    66        99    112  124.       143   276

data(example_data_5_subject)
summary_glu(example_data_5_subject)
#> # A tibble: 5 × 7
#> # Groups:   id [5]
#>   id         Min. `1st Qu.` Median  Mean `3rd Qu.`  Max.
#>   <fct>     <dbl>     <dbl>  <dbl> <dbl>     <dbl> <dbl>
#> 1 Subject 1    66        99    112  124.       143   276
#> 2 Subject 2    90       179    211  218.       253   400
#> 3 Subject 3    60       125    140  154.       173   304
#> 4 Subject 4    50       109    126  130.       149   232
#> 5 Subject 5    66       134    164  175.       211   398
```
