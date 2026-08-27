# Optimized Calculations of Time Dependent iglu Metrics

The function optimized_iglu_functions optimizes the calculation of all
time dependent iglu metrics by extracting the CGMS2DayByDay calculation
and passing the result into each function.

## Usage

``` r
optimized_iglu_functions(data, dt0 = NULL, inter_gap = 45, tz = "", timelag = 15, lag = 1)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl".

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

- timelag:

  Integer indicating the time period (# minutes) over which rate of
  change is calculated. Default is 15, e.g. rate of change is the change
  in glucose over the past 15 minutes divided by 15.

- lag:

  Integer indicating which lag (# days) to use. Default is 1.

## Value

If a data.frame object is passed, then a tibble object with 1 row for
each subject and one column for each metric is returned.

## Details

Returns a tibble object with 1 row for each subject and a column for
each metric. This function includes time dependent iglu metrics only.
For metric specific information, please see the corresponding function
documentation.

## Examples

``` r
data(example_data_1_subject)
optimized_iglu_functions(example_data_1_subject)
#> # A tibble: 1 × 15
#>   id        Conga   GVP  MODD SD.Roc CV_Measures_Mean CV_Measures_SD   AUC   MAG
#>   <fct>     <dbl> <dbl> <dbl>  <dbl>            <dbl>          <dbl> <dbl> <dbl>
#> 1 Subject 1  37.0  16.8  27.8  0.620             21.1           7.80  123.  17.0
#> # ℹ 6 more variables: SDw <dbl>, SDhhmm <dbl>, SDwsh <dbl>, SDdm <dbl>,
#> #   SDb <dbl>, SDbdm <dbl>

# Pass some arguments to possibly change the defaults
optimized_iglu_functions(example_data_1_subject, dt0 = 5, inter_gap = 30)
#> # A tibble: 1 × 15
#>   id        Conga   GVP  MODD SD.Roc CV_Measures_Mean CV_Measures_SD   AUC   MAG
#>   <fct>     <dbl> <dbl> <dbl>  <dbl>            <dbl>          <dbl> <dbl> <dbl>
#> 1 Subject 1  36.3  16.9  27.4  0.621             21.0           7.81  123.  17.0
#> # ℹ 6 more variables: SDw <dbl>, SDhhmm <dbl>, SDwsh <dbl>, SDdm <dbl>,
#> #   SDb <dbl>, SDbdm <dbl>

data(example_data_5_subject)
optimized_iglu_functions(example_data_5_subject)
#> # A tibble: 5 × 15
#>   id        Conga   GVP  MODD SD.Roc CV_Measures_Mean CV_Measures_SD   AUC   MAG
#>   <fct>     <dbl> <dbl> <dbl>  <dbl>            <dbl>          <dbl> <dbl> <dbl>
#> 1 Subject 1  37.0  16.8  27.8  0.620             21.1           7.80  123.  17.0
#> 2 Subject 2  60.6  20.5  44.1  0.642             17.0           6.41  218.  18.9
#> 3 Subject 3  63.4  25.6  48.2  0.831             27.1           9.40  154.  27.1
#> 4 Subject 4  33.5  17.1  24.9  0.617             18.4           5.70  130.  16.7
#> 5 Subject 5  73.8  38.3  59.4  1.05              29.0           7.56  174.  35.3
#> # ℹ 6 more variables: SDw <dbl>, SDhhmm <dbl>, SDwsh <dbl>, SDdm <dbl>,
#> #   SDb <dbl>, SDbdm <dbl>
```
