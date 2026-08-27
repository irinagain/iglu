# Calculate Area Under Curve AUC

The function auc produces hourly average AUC for each subject.

## Usage

``` r
auc(data, tz="")
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

- tz:

  String value of time zone.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding hourly average AUC value is returned.

AUC is calculated for every hour using the trapezoidal rule, then hourly
average AUC is calculated for each 24 hour period, then the mean of
hourly average AUC across all 24 hour periods is returned as overall
hourly average AUC.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for hourly average AUC values is returned. NA glucose values
are omitted from the calculation of the AUC.

AUC is calculated using the formula: (dt0/60) \* ((gl\[2:length(gl)\] +
gl\[1:(length(gl)-1)\])/2), where dt0/60 is the frequency of the cgm
measurements in hours and gl are the glucose values.

This formula is based off the Trapezoidal Rule: (time\[2\]-time\[1\] \*
((glucose\[1\]+glucose\[2\])/2)).

## References

Danne et al. (2017) International Consensus on Use of Continuous Glucose
Monitoring, *Diabetes Care* **40** .1631-1640,
[doi:10.2337/dc17-1600](https://doi.org/10.2337/dc17-1600) .

## Examples

``` r
data(example_data_1_subject)
auc(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id        hourly_auc
#>   <fct>          <dbl>
#> 1 Subject 1       123.
```
