# Display Episode Calculation statistics for selected subject

Display Episode Calculation statistics for selected subject

## Usage

``` r
epicalc_profile(
  data,
  lv1_hypo = 70,
  lv2_hypo = 54,
  lv1_hyper = 180,
  lv2_hyper = 250,
  dur_length = 15,
  end_length = 15,
  subject = NULL,
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

- subject:

  String corresponding to subject id

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

A plot displaying (1) the statistics for the episodes and (2) the
episodes colored by level.

## See also

episode_calculation()

## Author

Johnathan Shih, Jung Hoon Seo, Elizabeth Chun

## Examples

``` r
epicalc_profile(example_data_1_subject)

```
