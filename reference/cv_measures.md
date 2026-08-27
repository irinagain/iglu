# Calculate Coefficient of Variation subtypes

The function cv_measures produces CV subtype values in a tibble object.

## Usage

``` r
cv_measures(data, dt0 = NULL, inter_gap = 45, tz = "" )
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl". Should only
  be data for 1 subject. In case multiple subject ids are detected, a
  warning is produced and only 1st subject is used.

- dt0:

  The time frequency for interpolation in minutes, the default will
  match the CGM meter's frequency (e.g. 5 min for Dexcom).

- inter_gap:

  The maximum allowable gap (in minutes) for interpolation. The values
  will not be interpolated between the glucose measurements that are
  more than inter_gap minutes apart. The default value is 45 min.

- tz:

  A character string specifying the time zone to be used.
  System-specific (see
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html)), but " " is
  the current time zone, and "GMT" is UTC (Universal Time, Coordinated).
  Invalid values are most commonly treated as UTC, on some platforms
  with a warning.

## Value

When a data.frame object is passed, then a tibble object with three
columns: subject id and corresponding CV subtype values is returned.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for each cv subtype values is returned.

Missing values will be linearly interpolated when close enough to
non-missing values.

1.  CVmean:

    Calculated by first taking the coefficient of variation of each
    day's glucose measurements, then taking the mean of all the
    coefficient of variation. That is, for x days we compute cv_1 ...
    cv_x daily coefficient of variations and calculate \\1/x \* \sum
    \[(cv_i)\]\\

2.  CVsd:

    Calculated by first taking the coefficient of variation of each
    day's glucose measurements, then taking the standard deviation of
    all the coefficient of variations. That is, for d days we compute
    cv_1 ... cv_d daily coefficient of variations and calculate
    SD(\[cv_1, cv_2, ... cv_d\])

## References

Umpierrez, et.al. (2018) Glycemic Variability: How to Measure and Its
Clinical Implication for Type 2 Diabetes *The American Journal of
Medical Sciences* **356** .518-527,
[doi:10.1016/j.amjms.2018.09.010](https://doi.org/10.1016/j.amjms.2018.09.010)
.

## Examples

``` r

data(example_data_1_subject)
cv_measures(example_data_1_subject)
#> # A tibble: 1 × 3
#>   id        CVmean  CVsd
#>   <fct>      <dbl> <dbl>
#> 1 Subject 1   21.1  7.80

data(example_data_5_subject)
cv_measures(example_data_5_subject)
#> # A tibble: 5 × 3
#>   id        CVmean  CVsd
#>   <fct>      <dbl> <dbl>
#> 1 Subject 1   21.1  7.80
#> 2 Subject 2   17.0  6.41
#> 3 Subject 3   27.1  9.40
#> 4 Subject 4   18.4  5.70
#> 5 Subject 5   29.0  7.56
```
