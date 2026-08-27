# Calculate percentage of time CGM was active

The function \`active_percent\` produces the

## Usage

``` r
active_percent(data, dt0 = NULL, tz = "",
range_type = "automatic", ndays = 14, consistent_end_date = NULL)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl".

- dt0:

  The time frequency for interpolated aligned grid in minutes, the
  default will match the CGM meter's frequency (e.g. 5 min for Dexcom).

- tz:

  **tz = "".** A character string specifying the time zone to be used.
  System-specific (see
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html)), but " " is
  the current time zone, and "GMT" is UTC (Universal Time, Coordinated).
  Invalid values are most commonly treated as UTC, on some platforms
  with a warning.

- range_type:

  **range_type = "automatic".** A character string indicating the type
  of range ('automatic' or 'manual').

- ndays:

  **ndays = 14.** An integer specifying the number of days to consider
  in the calculation.

- consistent_end_date:

  **consistent_end_date = NULL.** A Date object or NULL indicating a
  period end date to be used for every subject. Leaving this value NULL
  will result in the end date being unique to each subject.

## Value

A tibble object with five columns: subject id, corresponding
active_percent value, duration of measurement period in days, start
date, and end date.

## Details

The function \`active_percent\` produces a tibble object with values
equal to the percentage of time the CGM was active, the total number of
observed days, the start date, and the end date. For example, if a CGM's
(5 min frequency) times were 0, 5, 10, 15 and glucose values were
missing at time 5, then percentage of time the CGM was active is 75 The
output columns correspond to the subject id, the percentage of time for
which the CGM was active, the number of days of measurements, the start
date and the end date of measurements. The output rows correspond to the
subjects. The values of \`active_percent\` are always between 0

## References

Danne et al. (2017) International Consensus on Use of Continuous Glucose
Monitoring *Diabetes Care* **40** .1631-1640,
[doi:10.2337/dc17-1600](https://doi.org/10.2337/dc17-1600) .

## Author

Pratik Patel, Irina Gaynanova

## Examples

``` r

data(example_data_1_subject)

active_percent(example_data_1_subject)
#> # A tibble: 1 × 5
#>   id        active_percent ndays     start_date          end_date           
#>   <fct>              <dbl> <drtn>    <dttm>              <dttm>             
#> 1 Subject 1           79.8 12.7 days 2015-06-06 21:50:27 2015-06-19 13:59:36

data(example_data_5_subject)

active_percent(example_data_5_subject)
#> # A tibble: 5 × 5
#>   id        active_percent ndays     start_date          end_date           
#>   <fct>              <dbl> <drtn>    <dttm>              <dttm>             
#> 1 Subject 1           79.8 12.7 days 2015-06-06 21:50:27 2015-06-19 13:59:36
#> 2 Subject 2           58.9 16.7 days 2015-02-24 22:31:29 2015-03-13 14:38:01
#> 3 Subject 3           92.1  5.8 days 2015-03-10 20:36:26 2015-03-16 15:11:05
#> 4 Subject 4           98.7 12.9 days 2015-03-13 17:44:09 2015-03-26 15:01:58
#> 5 Subject 5           95.8 10.6 days 2015-02-28 22:40:06 2015-03-11 13:04:28
active_percent(example_data_5_subject, dt0 = 5, tz = 'GMT')
#> # A tibble: 5 × 5
#>   id        active_percent ndays     start_date          end_date           
#>   <fct>              <dbl> <drtn>    <dttm>              <dttm>             
#> 1 Subject 1           79.8 12.7 days 2015-06-06 21:50:27 2015-06-19 13:59:36
#> 2 Subject 2           58.9 16.7 days 2015-02-24 22:31:29 2015-03-13 14:38:01
#> 3 Subject 3           92.1  5.8 days 2015-03-10 20:36:26 2015-03-16 15:11:05
#> 4 Subject 4           98.7 12.9 days 2015-03-13 17:44:09 2015-03-26 15:01:58
#> 5 Subject 5           95.8 10.6 days 2015-02-28 22:40:06 2015-03-11 13:04:28
```
