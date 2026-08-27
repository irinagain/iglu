# Percentage of GRADE score attributable to hyperglycemia

The function grade_hyper produces %GRADE hyperglycemia values in a
tibble object.

## Usage

``` r
grade_hyper(data, upper = 140)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

- upper:

  Upper bound used for hyperglycemia cutoff, in mg/dL. Default is 140.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding %GRADE hyperglycemia value is returned. If
a vector of glucose values is passed, then a tibble object with just the
%GRADE hyperglycemia value is returned. as.numeric() can be wrapped
around the latter to output just a numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for %GRADE hyperglycemia values is returned. NA glucose values
are omitted from the calculation of the %GRADE hyperglycemia values.

%GRADE hyperglycemia is determined by calculating the percentage of
GRADE score (see grade function) attributed to hyperglycemic glucose
values.

## References

Hill et al. (2007): A method for assessing quality of control from
glucose profiles *Diabetic Medicine* **24** .753-758,
[doi:10.1111/j.1464-5491.2007.02119.x](https://doi.org/10.1111/j.1464-5491.2007.02119.x)
.

## Examples

``` r

data(example_data_1_subject)
grade_hyper(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id        GRADE_hyper
#>   <fct>           <dbl>
#> 1 Subject 1        71.7
grade_hyper(example_data_1_subject, upper = 180)
#> # A tibble: 1 × 2
#>   id        GRADE_hyper
#>   <fct>           <dbl>
#> 1 Subject 1        33.1

data(example_data_5_subject)
grade_hyper(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id        GRADE_hyper
#>   <fct>           <dbl>
#> 1 Subject 1        71.7
#> 2 Subject 2        99.4
#> 3 Subject 3        79.6
#> 4 Subject 4        65.5
#> 5 Subject 5        93.3
grade_hyper(example_data_5_subject, upper = 160)
#> # A tibble: 5 × 2
#>   id        GRADE_hyper
#>   <fct>           <dbl>
#> 1 Subject 1        48.5
#> 2 Subject 2        96.3
#> 3 Subject 3        65.7
#> 4 Subject 4        39.3
#> 5 Subject 5        82.0
```
