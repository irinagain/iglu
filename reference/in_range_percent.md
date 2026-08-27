# Calculate percentage in targeted value ranges

The function in_range_percent produces a tibble object with values equal
to the percentage of glucose measurements in ranges of target values.
The output columns correspond to subject id followed by the target value
ranges, and the rows correspond to the subjects. The values will be
between 0 (no measurements) and 100 (all measurements).

## Usage

``` r
in_range_percent(data, target_ranges = list(c(70, 180), c(63, 140)))
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

- target_ranges:

  List of target value ranges wrapped in an r 'list' structure. Default
  list of ranges is ((70, 180), (63, 140)) mg/dL, where the range
  (70, 180) is recommended to assess glycemic control for subjects with
  type 1 or type 2 diabetes, and (63, 140) is recommended for assessment
  of glycemic control during pregnancy; see Battelino et al. (2019)

## Value

If a data.frame object is passed, then a tibble object with a column for
subject id and then a column for each target value is returned. If a
vector of glucose values is passed, then a tibble object without the
subject id is returned. as.numeric() can be wrapped around the latter to
output a numeric vector.

## Details

A tibble object with 1 row for each subject, a column for subject id and
column for each range of target values is returned. NA's will be omitted
from the glucose values in calculation of percent.

in_range_percent will only work properly if the target_ranges argument
is a list of paired values in the format list(c(a1,b1), c(a2,b2), ...).
The paired values can be ordered (min, max) or (max, min). See the
Examples section for proper usage.

## References

Rodbard (2009) Interpretation of continuous glucose monitoring data:
glycemic variability and quality of glycemic control, *Diabetes
Technology and Therapeutics* **11** .55-67,
[doi:10.1089/dia.2008.0132](https://doi.org/10.1089/dia.2008.0132) .

Battelino et al. (2019) Clinical targets for continuous glucose
monitoring data interpretation: recommendations from the international
consensus on time in range. *Diabetes Care* **42**(8):1593-603,
[doi:10.2337/dci19-0028](https://doi.org/10.2337/dci19-0028)

## See also

plot_ranges()

## Examples

``` r

data(example_data_1_subject)

in_range_percent(example_data_1_subject)
#> # A tibble: 1 × 3
#>   id        in_range_63_140 in_range_70_180
#>   <fct>               <dbl>           <dbl>
#> 1 Subject 1            73.9            91.7
in_range_percent(example_data_1_subject, target_ranges = list(c(50, 100), c(200,
300), c(80, 140)))
#> # A tibble: 1 × 4
#>   id        in_range_200_300 in_range_50_100 in_range_80_140
#>   <fct>                <dbl>           <dbl>           <dbl>
#> 1 Subject 1             3.70            27.3            73.3

data(example_data_5_subject)

in_range_percent(example_data_5_subject)
#> # A tibble: 5 × 3
#>   id        in_range_63_140 in_range_70_180
#>   <fct>               <dbl>           <dbl>
#> 1 Subject 1           73.9             91.7
#> 2 Subject 2            3.36            26.4
#> 3 Subject 3           50.0             81.3
#> 4 Subject 4           67.9             95.1
#> 5 Subject 5           30.2             62.1
in_range_percent(example_data_1_subject, target_ranges = list(c(60, 120), c(140,
250)))
#> # A tibble: 1 × 3
#>   id        in_range_140_250 in_range_60_120
#>   <fct>                <dbl>           <dbl>
#> 1 Subject 1             26.3            59.6
```
