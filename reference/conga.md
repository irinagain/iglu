# Continuous Overall Net Glycemic Action (CONGA)

The function conga produces CONGA values a tibble object for any n hours
apart.

## Usage

``` r
conga(data, n = 24, tz = "")
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl".

- n:

  An integer specifying how many hours prior to an observation should be
  used in the CONGA calculation. The default value is set to n = 24
  hours

- tz:

  A character string specifying the time zone to be used.
  System-specific (see
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html)), but " " is
  the current time zone, and "GMT" is UTC (Universal Time, Coordinated).
  Invalid values are most commonly treated as UTC, on some platforms
  with a warning.

## Value

A tibble object with two columns: subject id and corresponding CONGA
value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for the CONGA values is returned.

Missing values will be linearly interpolated when close enough to
non-missing values.

CONGA is the standard deviation of the difference between glucose values
that are exactly n hours apart. CONGA is computed by taking the standard
deviation of differences in measurements separated by n hours.

## References

McDonnell et al. (2005) : A novel approach to continuous glucose
analysis utilizing glycemic variation *Diabetes Technology and
Therapeutics* **7** .253-263,
[doi:10.1089/dia.2005.7.253](https://doi.org/10.1089/dia.2005.7.253) .

## Examples

``` r

data(example_data_1_subject)
conga(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id        CONGA
#>   <fct>     <dbl>
#> 1 Subject 1  37.0

data(example_data_5_subject)
conga(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id        CONGA
#>   <fct>     <dbl>
#> 1 Subject 1  37.0
#> 2 Subject 2  60.6
#> 3 Subject 3  63.4
#> 4 Subject 4  33.5
#> 5 Subject 5  73.8
```
