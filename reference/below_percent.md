# Calculate percentage below targeted values

The function below_percent produces a tibble object with values equal to
the percentage of glucose measurements below target values. The output
columns correspond to the subject id followed by the target values and
the output rows correspond to the subjects. The values will be between 0
(no measurements) and 100 (all measurements).

## Usage

``` r
below_percent(data, targets_below = c(54, 70))
```

## Arguments

- data:

  DataFrame with column names ("id", "time", and "gl"), or numeric
  vector of glucose values.

- targets_below:

  Numeric vector of glucose thresholds. Glucose values from data
  argument will be compared to each value in the targets_below vector.
  Default list is (54, 70).

## Value

If a data.frame object is passed, then a tibble object with a column for
subject id and then a column for each target value is returned. If a
vector of glucose values is passed, then a tibble object without the
subject id is returned. as.numeric() can be wrapped around the latter to
output a numeric vector.

## Details

A tibble object with 1 row for each subject, a column for subject id and
column for each target value is returned. NA's will be omitted from the
glucose values in calculation of percent.

## References

Rodbard (2009) Interpretation of continuous glucose monitoring data:
glycemic variability and quality of glycemic control, *Diabetes
Technology and Therapeutics* **11** .55-67,
[doi:10.1089/dia.2008.0132](https://doi.org/10.1089/dia.2008.0132) .

## See also

plot_ranges()

## Examples

``` r

data(example_data_1_subject)

below_percent(example_data_1_subject)
#> # A tibble: 1 × 3
#>   id        below_54 below_70
#>   <fct>        <dbl>    <dbl>
#> 1 Subject 1        0    0.137
below_percent(example_data_1_subject, targets_below = c(50, 100, 180))
#> # A tibble: 1 × 4
#>   id        below_100 below_180 below_50
#>   <fct>         <dbl>     <dbl>    <dbl>
#> 1 Subject 1      25.6      91.6        0

data(example_data_5_subject)

below_percent(example_data_5_subject)
#> # A tibble: 5 × 3
#>   id        below_54 below_70
#>   <fct>        <dbl>    <dbl>
#> 1 Subject 1   0         0.137
#> 2 Subject 2   0         0    
#> 3 Subject 3   0         0.326
#> 4 Subject 4   0.0546    0.273
#> 5 Subject 5   0         0.103
below_percent(example_data_5_subject, targets_below = c(80, 180))
#> # A tibble: 5 × 3
#>   id        below_180 below_80
#>   <fct>         <dbl>    <dbl>
#> 1 Subject 1      91.6    0.583
#> 2 Subject 2      25.6    0    
#> 3 Subject 3      81.1    0.848
#> 4 Subject 4      95.0    1.69 
#> 5 Subject 5      61.9    1.03 
```
