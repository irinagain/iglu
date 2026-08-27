# Calculates Hypo/Hyperglycemic episodes with summary statistics

The function determines episodes or events, calculates summary
statistics, and optionally returns data with episode label columns added

## Usage

``` r
episode_calculation(
  data,
  lv1_hypo = 70,
  lv2_hypo = 54,
  lv1_hyper = 180,
  lv2_hyper = 250,
  dur_length = 15,
  end_length = 15,
  return_data = FALSE,
  dt0 = NULL,
  inter_gap = 45,
  tz = ""
)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl". Should only
  be data for 1 subject. In case multiple subject ids are detected, a
  warning is produced and only 1st subject is used.

- lv1_hypo:

  Numeric value specifying a hypoglycemia threshold for level 1

- lv2_hypo:

  Numeric value specifying a hypoglycemia threshold for level 2

- lv1_hyper:

  Numeric value specifying a hyperglycemia threshold for level 1

- lv2_hyper:

  Numeric value specifying a hyperglycemia threshold for level 2

- dur_length:

  Numeric value specifying the minimum duration in minutes to be
  considered an episode. Note dur_length should be a multiple of the
  data recording interval otherwise the function will round up to the
  nearest multiple. Default is 15 minutes to match consensus.

- end_length:

  Numeric value specifying the minimum duration in minutes of improved
  glycemia for an episode to end. Default is equal to dur_length to
  match consensus.

- return_data:

  Boolean indicating whether to also return data with episode labels.
  Defaults to FALSE which means only episode summary statistics will be
  returned

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

If return_data is FALSE, a single dataframe with columns:

- id:

  Subject id

- type:

  Type of episode - either hypoglycemia or hyperglycemia

- level:

  Level of episode - one of lv1, lv2, extended, lv1_excl

- avg_ep_per_day:

  Average number of episodes per day calculated as (total \#
  episodes)/(recording time in days (24hrs))

- avg_ep_duration:

  Average duration of episodes in minutes

- avg_ep_gl:

  Average glucose in the episode in mg/dL

- total_episodes:

  Total number of episodes in the subject's glucose trace

If return_data is TRUE, returns a list where the first entry is the
episode summary dataframe (see above) and the second entry is the input
data with episode labels added. Note the data returned here has been
interpolated using the CGMS2DayByDay() function. Mostly for use with
epicalc_profile function. Format of the second list entry is:

- id:

  Subject id

- time:

  Interpolated timestamps

- gl:

  glucose in mg/dL

- \[episode_label\]:

  One column per episode label - i.e. lv1_hypo, lv2_hypo, lv1_hyper,
  lv2_hyper, ext_hypo. 0 means not this type of episode, a positive
  integer label is assigned to each episode. Note the labels are \*not\*
  unique by subject only unique by segment

## Details

We follow the definition of episodes given in the 2023 consensus by
Battelino et al. Note we have classified lv2 as a subset of lv1 since we
find the consensus to be slightly ambiguous. For lv1 exclusive of lv2,
please see lv1_excl which summarises episodes that were exclusively lv1
and did not cross the lv2 threshold. Also note, hypo extended refers to
episodes that are \>120 consecutive minutes below lv1 hypo and ends with
at least 15 minutes of normoglycemia. For more details on each category
please see the reference below (Battelino et al 2023).

## References

Battelino et al. (2023): Continuous glucose monitoring and metrics for
clinical trials: an international consensus statement *Lancet Diabetes &
Endocrinology* **11(1)** .42-57,
[doi:10.1016/s2213-8587(22)00319-9](https://doi.org/10.1016/s2213-8587%2822%2900319-9)
.

## See also

epicalc_profile()

## Author

Elizabeth Chun, Jung Hoon Seo, Johnathan Shih

## Examples

``` r
episode_calculation(example_data_5_subject, lv1_hypo=100, lv1_hyper= 120)
#> # A tibble: 35 × 7
#>    id        type  level avg_ep_per_day avg_ep_duration avg_ep_gl total_episodes
#>    <fct>     <chr> <chr>          <dbl>           <dbl>     <dbl>          <dbl>
#>  1 Subject 1 hypo  lv1            2.88             130.      92.9             32
#>  2 Subject 1 hypo  lv2            0                  0       NA                0
#>  3 Subject 1 hypo  exte…          1.08             243.      92.1             12
#>  4 Subject 1 hyper lv1            3.42             170.     143.              38
#>  5 Subject 1 hyper lv2            0.180             30      264.               2
#>  6 Subject 1 hypo  lv1_…          2.88             130.      92.9             32
#>  7 Subject 1 hyper lv1_…          3.33             166.     141.              37
#>  8 Subject 2 hypo  lv1            0.102             50       94.6              1
#>  9 Subject 2 hypo  lv2            0                  0       NA                0
#> 10 Subject 2 hypo  exte…          0                  0       NA                0
#> # ℹ 25 more rows
```
