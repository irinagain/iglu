# Calculate the standard deviation of the rate of change

The function `sd_roc` produces the standard deviation of the rate of
change values in a tibble object.

## Usage

``` r
sd_roc(data, timelag = 15, dt0 = NULL, inter_gap = 45, tz = "")
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

A tibble object with two columns: subject id and standard deviation of
the rate of change values for each subject.

## Details

A tibble object with one row for each subject, a column for subject id
and a column for the standard deviation of the rate of change.

When calculating rate of change, missing values will be linearly
interpolated when close enough to non-missing values.

Calculated by taking the standard deviation of all the ROC values for
each individual subject. NA rate of change values are omitted from the
standard deviation calculation.

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
sd_roc(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id        sd_roc
#>   <fct>      <dbl>
#> 1 Subject 1  0.620
sd_roc(example_data_1_subject, timelag = 10)
#> # A tibble: 1 × 2
#>   id        sd_roc
#>   <fct>      <dbl>
#> 1 Subject 1  0.652

data(example_data_5_subject)
sd_roc(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id        sd_roc
#>   <fct>      <dbl>
#> 1 Subject 1  0.620
#> 2 Subject 2  0.642
#> 3 Subject 3  0.831
#> 4 Subject 4  0.617
#> 5 Subject 5  1.05 
sd_roc(example_data_5_subject, timelag = 10)
#> # A tibble: 5 × 2
#>   id        sd_roc
#>   <fct>      <dbl>
#> 1 Subject 1  0.652
#> 2 Subject 2  0.704
#> 3 Subject 3  0.858
#> 4 Subject 4  0.659
#> 5 Subject 5  1.09 
```
