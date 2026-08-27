# Calculate all metrics in iglu

The function all_metrics runs all of the iglu metrics, and returns the
results with one column per metric.

## Usage

``` r
all_metrics(
  data,
  dt0 = NULL,
  inter_gap = 45,
  tz = "",
  timelag = 15,
  lag = 1,
  metrics_to_include = c("all", "consensus_only")
)
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

- metrics_to_include:

  Returns all metrics computed by iglu or all on the consensus list
  (Battelino 2023)

## Value

A tibble object with 1 row per subject and one column per metric is
returned.

## Details

All iglu functions are calculated within the all_metrics function, and
the resulting tibble is returned with one row per subject and a column
for each metric. Time dependent functions are calculated together using
the function optimized_iglu_functions with two exceptions: PGS and
episodes are calculated within all_metrics because their structure does
not align with optimized_iglu_functions. Note that episodes related
outputs included in all_metrics are only average episodes per day. To
get the average duration and glucose, please use the standalone episodes
function

For metric specific information, please see the corresponding function
documentation.

## References

Battelino T, Alexander CM, Amiel SA, et al. Continuous glucose
monitoring and metrics for clinical trials: an international consensus
statement. *Lancet Diabetes Endocrinol.* **2023;11(1):**42-57.
[doi:10.1016/S2213-8587(22)00319-9](https://doi.org/10.1016/S2213-8587%2822%2900319-9)
.

\# Specify the meter frequency and change the interpolation gap to 30
min all_metrics(example_data_1_subject, dt0 = 5, inter_gap = 30)

## Examples

``` r
data(example_data_1_subject)
all_metrics(example_data_1_subject)
#> # A tibble: 1 × 62
#>   id         ADRR  COGI    CV  eA1C hypo_lv1 hypo_lv2 hypo_extended hyper_lv1
#>   <fct>     <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>         <dbl>     <dbl>
#> 1 Subject 1  15.1  93.0  26.9  5.94   0.0899        0             0      1.44
#> # ℹ 53 more variables: hyper_lv2 <dbl>, hypo_lv1_excl <dbl>,
#> #   hyper_lv1_excl <dbl>, GMI <dbl>, GRI <dbl>, GRADE <dbl>, GRADE_eugly <dbl>,
#> #   GRADE_hyper <dbl>, GRADE_hypo <dbl>, HBGI <dbl>, LBGI <dbl>,
#> #   hyper_index <dbl>, hypo_index <dbl>, IGC <dbl>, IQR <dbl>, J_index <dbl>,
#> #   M_value <dbl>, MAD <dbl>, MAGE <dbl>, active_percent <dbl>, ndays <drtn>,
#> #   start_date <dttm>, end_date <dttm>, above_140 <dbl>, above_180 <dbl>,
#> #   above_250 <dbl>, below_54 <dbl>, below_70 <dbl>, in_range_63_140 <dbl>, …
```
