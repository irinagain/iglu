# Calculate the Mean Absolute Glucose (MAG)

The function mag calculates the Mean Absolute Glucose change (MAG) using
raw or interpolated data.

## Usage

``` r
mag(data, n = NULL, dt0 = NULL, inter_gap = 45, tz = "", max_gap = 45,
interpolate = FALSE)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl".

- n:

  Integer giving the desired interval in minutes over which to calculate
  the change in glucose when interpolate = TRUE. Default is the CGM
  meter's frequency (dt0) to measure change in every reading.

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

- max_gap:

  Numeric giving the maximum allowed time gap, in minutes, between
  consecutive glucose readings to be included in the MAG calculation
  when using raw data (interpolate = FALSE). Default is 45 minutes. If
  the time gap between two consecutive readings exceeds this threshold,
  the change in glucose between those readings will not be included in
  the MAG calculation.

- interpolate:

  Logical indicating whether MAG should be calculated using raw data or
  linearly interpolated glucose values. Default is FALSE.

## Value

A tibble object with two columns: subject id and MAG value.

## Details

A tibble object with a column for subject id and a column for MAG values
is returned.

If interpolate = TRUE, the data is linearly interpolated and MAG is
calculated as \\\frac{\|\Delta G\|}{\Delta t}\\ where \\\|\Delta G\|\\
is the sum of the absolute change in glucose over n-minute intervals
(default n = dt0) and \\\Delta t\\ is the total elapsed time (in hours).

If interpolate = FALSE, MAG is calculated as \\\frac{\|\Delta
G\|}{\Delta t}\\ where \\\|\Delta G\|\\ is the sum of absolute changes
between consecutive observed glucose readings with time gaps less than
or equal to max_gap, and \\\Delta t\\ is the total time spanned by those
valid time gaps (in hours).

This yields the Mean Absolute Glucose change (mg/dL per hour).

## References

Hermanides et al. (2010) Glucose Variability is Associated with
Intensive Care Unit Mortality, *Critical Care Medicine* **38(3)**
838-842,
[doi:10.1097/CCM.0b013e3181cc4be9](https://doi.org/10.1097/CCM.0b013e3181cc4be9)

Kohnert et al. (2013) Evaluation of the Mean Absolute Glucose Change as
a Measure of Glycemic Variability Using Continuous Glucose Monitoring
Data, *Diabetes Technol Ther.* **15(6)** 448-454,
[doi:10.1089/dia.2012.0303](https://doi.org/10.1089/dia.2012.0303)

## Author

Elizabeth Chun, Neo Kok

## Examples

``` r

data(example_data_1_subject)
mag(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id          MAG
#>   <fct>     <dbl>
#> 1 Subject 1  27.3

data(example_data_5_subject)
mag(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id          MAG
#>   <fct>     <dbl>
#> 1 Subject 1  27.3
#> 2 Subject 2  35.6
#> 3 Subject 3  37.6
#> 4 Subject 4  29.5
#> 5 Subject 5  47.7
```
