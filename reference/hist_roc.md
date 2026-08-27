# Plot histogram of Rate of Change values (ROC)

The function hist_roc produces a histogram plot of ROC values

## Usage

``` r
hist_roc(data, subjects = NULL, timelag = 15, dt0 = NULL, inter_gap = 45, tz = "")
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl".

- subjects:

  String or list of strings corresponding to subject names in 'id'
  column of data. Default is all subjects.

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

A histogram of ROC values per subject

## Details

For the default, a histogram is produced for each subject displaying the
ROC values colored by ROC categories defined as follows. The breaks for
the categories are: c(-Inf, -3, -2, -1, 1, 2, 3, Inf) where the glucose
is in mg/dl and the ROC values are in mg/dl/min. A ROC of -5 mg/dl/min
will thus be placed in the first category and colored accordingly.

## References

Clarke et al. (2009) Statistical Tools to Analyze Continuous Glucose
Monitor Data, Diabetes *Diabetes Technology and Therapeutics* **11**
S45-S54,
[doi:10.1089/dia.2008.0138](https://doi.org/10.1089/dia.2008.0138) .

## See also

[`plot_roc`](https://irinagain.github.io/iglu/reference/plot_roc.md) for
reference paper on ROC categories.

## Author

Elizabeth Chun, David Buchanan

## Examples

``` r

data(example_data_1_subject)
hist_roc(example_data_1_subject)


data(example_data_5_subject)
hist_roc(example_data_5_subject)

hist_roc(example_data_5_subject, subjects = 'Subject 3')

```
