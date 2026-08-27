# Calculate mean difference between glucose values obtained at the same time of day (MODD)

The function modd produces MODD values in a tibble object.

## Usage

``` r
modd(data, lag = 1, tz = "")
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl".

- lag:

  Integer indicating which lag (# days) to use. Default is 1.

- tz:

  A character string specifying the time zone to be used.
  System-specific (see
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html)), but " " is
  the current time zone, and "GMT" is UTC (Universal Time, Coordinated).
  Invalid values are most commonly treated as UTC, on some platforms
  with a warning.

## Value

A tibble object with two columns: subject id and corresponding MODD
value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for the MODD values is returned.

Missing values will be linearly interpolated when close enough to
non-missing values.

MODD is calculated by taking the mean of absolute differences between
measurements at the same time 1 day away, or more if lag parameter is
set to an integer \> 1.

## References

Service, F. J. & Nelson, R. L. (1980) Characteristics of glycemic
stability. *Diabetes care* **3** .58-62,
[doi:10.2337/diacare.3.1.58](https://doi.org/10.2337/diacare.3.1.58) .

## Examples

``` r

data(example_data_1_subject)
modd(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id         MODD
#>   <fct>     <dbl>
#> 1 Subject 1  27.8
modd(example_data_1_subject, lag = 2)
#> # A tibble: 1 × 2
#>   id         MODD
#>   <fct>     <dbl>
#> 1 Subject 1  27.7

data(example_data_5_subject)
modd(example_data_5_subject, lag = 2)
#> # A tibble: 5 × 2
#>   id         MODD
#>   <fct>     <dbl>
#> 1 Subject 1  27.7
#> 2 Subject 2  46.0
#> 3 Subject 3  47.1
#> 4 Subject 4  24.9
#> 5 Subject 5  56.0
```
