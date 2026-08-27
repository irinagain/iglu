# Calculate the Rate of Change at each time point (ROC)

The function `roc` produces rate of change values in a tibble object.

## Usage

``` r
roc(data, timelag = 15, dt0 = NULL, inter_gap = 45, tz = "")
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl".

- timelag:

  Integer indicating the time period (# minutes) over which rate of
  change is calculated. Default is 15, e.g. rate of change is the change
  in glucose over the past 15 minutes divided by 15.

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

A tibble object with two columns: subject id and rate of change values

## Details

A tibble object with a column for subject id and a column for ROC values
is returned. A ROC value is returned for each time point for all the
subjects. Thus multiple rows are returned for each subject. If the rate
of change cannot be calculated, the function will return `NA` for that
point.

The glucose values are linearly interpolated over a time grid starting
at the beginning of the first day of data and ending on the last day of
data. Because of this, there may be many `NA`s at the beginning and the
end of the roc values for each subject. These `NA`s are a result of
interpolated time points that do not have recorded glucose values near
them because recording had either not yet begun for the day or had
already ended.

The ROC is calculated as \\\frac{G(t_i) - G(t\_{i-1})}{t_i - t\_{i-1}}\\
where \\G_i\\ is the Glucose measurement at time \\t_i\\ and
\\G\_{i-1}\\ is the Glucose measurement at time \\t\_{i-1}\\. The time
difference between the points, \\t_i - t\_{i-1}\\, is selectable and set
at a default of 15 minutes.

## References

Clarke et al. (2009) Statistical Tools to Analyze Continuous Glucose
Monitor Data, Diabetes *Diabetes Technology and Therapeutics* **11**
S45-S54,
[doi:10.1089/dia.2008.0138](https://doi.org/10.1089/dia.2008.0138) .

## Author

Elizabeth Chun, David Buchanan

## Examples

``` r

data(example_data_1_subject)
roc(example_data_1_subject)
#> # A tibble: 4,032 × 2
#>    id          roc
#>    <fct>     <dbl>
#>  1 Subject 1    NA
#>  2 Subject 1    NA
#>  3 Subject 1    NA
#>  4 Subject 1    NA
#>  5 Subject 1    NA
#>  6 Subject 1    NA
#>  7 Subject 1    NA
#>  8 Subject 1    NA
#>  9 Subject 1    NA
#> 10 Subject 1    NA
#> # ℹ 4,022 more rows
roc(example_data_1_subject, timelag = 10)
#> # A tibble: 4,032 × 2
#>    id          roc
#>    <fct>     <dbl>
#>  1 Subject 1    NA
#>  2 Subject 1    NA
#>  3 Subject 1    NA
#>  4 Subject 1    NA
#>  5 Subject 1    NA
#>  6 Subject 1    NA
#>  7 Subject 1    NA
#>  8 Subject 1    NA
#>  9 Subject 1    NA
#> 10 Subject 1    NA
#> # ℹ 4,022 more rows

data(example_data_5_subject)
roc(example_data_5_subject)
#> # A tibble: 18,720 × 2
#>    id          roc
#>    <fct>     <dbl>
#>  1 Subject 1    NA
#>  2 Subject 1    NA
#>  3 Subject 1    NA
#>  4 Subject 1    NA
#>  5 Subject 1    NA
#>  6 Subject 1    NA
#>  7 Subject 1    NA
#>  8 Subject 1    NA
#>  9 Subject 1    NA
#> 10 Subject 1    NA
#> # ℹ 18,710 more rows
```
