# Calculate Meal Metrics

The function meal_metrics calculates three simple glucose meal metrics

## Usage

``` r
meal_metrics(data, mealtimes, before_win = 1, after_win = 3,
recovery_win = 1, interpolate = TRUE, adjust_mealtimes = TRUE, dt0 = NULL,
inter_gap = 45, tz = "", glucose_times = FALSE)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl". Should only
  be data for 1 subject. In case multiple subject ids are detected, a
  warning is produced and only 1st subject is used.

- mealtimes:

  Either a vector of mealtimes, corresponding to data being from a
  single subject, OR a dataframe with at least 2 columns labeled id and
  mealtime. Optionally the mealtimes dataframe can include a column
  labeled meal, giving the meal type (helps to compensate for
  overlapping meals)

- before_win:

  integer specifying number of hours to extend window before meal

- after_win:

  integer specifying number of hours to extend window after meal

- recovery_win:

  interger specifying number of hours for recovery beyond after window

- interpolate:

  Boolean to indicate if CGM data should be interpolated or not. Default
  set to FALSE due to time intensive nature of interpolation. Parameters
  dt0, inter_gap, and tz will only be used if interpolate is set to
  TRUE.

- adjust_mealtimes:

  Boolean to indicate if function should attempt to align mealtimes with
  CGM data times. This is important if mealtimes and CGM data times are
  not exactly aligned, because the function will return NA's for
  mealtimes that don't match with a corresponding CGM time stamp.

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

- glucose_times:

  Boolean to indicate if underlying glucose and times should be
  returned - e.g. baseline glucose value used to calculate \\Delta G\\.
  Intended for use in plotting function

## Value

By default tibble object with 6 columns will be returned: id, time,
meal, deltag, deltat, and basereco. If glucose_times = TRUE then 5 more
columns are returned for baseline glucose (basegl), peak glucose
(peakgl), recover glucose (recovergl), peak timestamp (peaktime), and
recovery timestamp (recovertime)

## Details

A tibble object is returned with three metrics calculated for each
mealtime. The last three columns of the output correspond to the three
metrics: deltag refers to \\\Delta G\\, deltat is \\\Delta T\\, and
basereco is % Baseline recovery. If no meal column is given in the
original data, one will be automatically generated with a unique number
for each meal.

## References

Service, F. John. (2013) Glucose Variability, *Diabetes* **62(5)**:
1398-1404, [doi:10.2337/db12-1396](https://doi.org/10.2337/db12-1396)

## See also

plot_meals()

## Examples

``` r
data(example_data_hall)
data(example_meals_hall)
meal_metrics(example_data_hall, example_meals_hall)
#> No mealtimes found for subject: 1636-69-001
#> No mealtimes found for subject: 1636-69-026
#> No mealtimes found for subject: 1636-69-032
#> No mealtimes found for subject: 1636-69-090
#> No mealtimes found for subject: 1636-69-091
#> No mealtimes found for subject: 1636-69-114
#> No mealtimes found for subject: 1636-70-1005
#> No mealtimes found for subject: 1636-70-1010
#> No mealtimes found for subject: 2133-015
#> No mealtimes found for subject: 2133-017
#> No mealtimes found for subject: 2133-019
#> No mealtimes found for subject: 2133-021
#> No mealtimes found for subject: 2133-024
#> No mealtimes found for subject: 2133-027
#> No mealtimes found for subject: 2133-035
#> No mealtimes found for subject: 2133-036
#> Warning: There was 1 warning in `dplyr::reframe()`.
#> ℹ In argument: `meal_metrics_single(...)`.
#> ℹ In group 9: `id = "2133-004"`.
#> Caused by warning in `meal_metrics_single()`:
#> ! Some meals don't match with CGM readings. Mealtimes returned with NA for metric values
#> # A tibble: 25 × 6
#>    id           time                meal  deltag deltat basereco
#>    <chr>        <dttm>              <chr>  <dbl>  <dbl>    <dbl>
#>  1 1636-69-001  NA                  NA      NA       NA   NA    
#>  2 1636-69-026  NA                  NA      NA       NA   NA    
#>  3 1636-69-032  NA                  NA      NA       NA   NA    
#>  4 1636-69-090  NA                  NA      NA       NA   NA    
#>  5 1636-69-091  NA                  NA      NA       NA   NA    
#>  6 1636-69-114  NA                  NA      NA       NA   NA    
#>  7 1636-70-1005 NA                  NA      NA       NA   NA    
#>  8 1636-70-1010 NA                  NA      NA       NA   NA    
#>  9 2133-004     2016-09-23 10:10:00 CF 1    87.8     80    0.362
#> 10 2133-004     2016-09-27 09:40:00 PB 1    NA       NA   NA    
#> # ℹ 15 more rows
```
