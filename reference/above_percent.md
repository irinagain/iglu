# Calculate percentage of values above target thresholds

The function `above_percent` produces a tibble object with values equal
to the percentage of glucose measurements above target values. The
output columns correspond to the subject id followed by the target
values, and the output rows correspond to the subjects. The values will
be between 0 (no measurements) and 100 (all measurements).

## Usage

``` r
above_percent(data, targets_above = c(140, 180, 250))
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

- targets_above:

  **Default: (140, 180, 250).** Numeric vector of glucose thresholds.
  Glucose values from data argument will be compared to each value in
  the targets_above vector.

## Value

If a DataFrame object is passed, then a tibble object with a column for
subject id and then a column for each target value is returned. If a
vector of glucose values is passed, then a tibble object without the
subject id is returned. Wrap \`as.numeric()\` around the latter to
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

above_percent(example_data_1_subject)
#> # A tibble: 1 × 4
#>   id        above_140 above_180 above_250
#>   <fct>         <dbl>     <dbl>     <dbl>
#> 1 Subject 1      26.1      8.20     0.377
above_percent(example_data_1_subject, targets_above = c(100, 150, 180))
#> # A tibble: 1 × 4
#>   id        above_100 above_150 above_180
#>   <fct>         <dbl>     <dbl>     <dbl>
#> 1 Subject 1      72.7      20.1      8.20

data(example_data_5_subject)

above_percent(example_data_5_subject)
#> # A tibble: 5 × 4
#>   id        above_140 above_180 above_250
#>   <fct>         <dbl>     <dbl>     <dbl>
#> 1 Subject 1      26.1      8.20     0.377
#> 2 Subject 2      96.6     73.6     26.1  
#> 3 Subject 3      49.8     18.3      5.68 
#> 4 Subject 4      32.0      4.61     0    
#> 5 Subject 5      69.8     37.8     11.3  
above_percent(example_data_5_subject, targets_above = c(70, 170))
#> # A tibble: 5 × 3
#>   id        above_170 above_70
#>   <fct>         <dbl>    <dbl>
#> 1 Subject 1     10.4      99.9
#> 2 Subject 2     82.3     100  
#> 3 Subject 3     26.5      99.6
#> 4 Subject 4      8.19     99.6
#> 5 Subject 5     44.7      99.9
```
