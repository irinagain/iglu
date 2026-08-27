# Percentage of GRADE score attributable to hypoglycemia

The function grade_hypo produces %GRADE hypoglycemia values in a tibble
object.

## Usage

``` r
grade_hypo(data, lower = 80)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

- lower:

  Lower bound used for hypoglycemia cutoff, in mg/dL. Default is 80

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding %GRADE hypoglycemia value is returned. If a
vector of glucose values is passed, then a tibble object with just the
%GRADE hypoglycemia value is returned. as.numeric() can be wrapped
around the latter to output just a numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for %GRADE hypoglycemia values is returned. NA glucose values
are omitted from the calculation of the %GRADE hypoglycemia values.

%GRADE hypoglycemia is determined by calculating the percentage of GRADE
score (see grade function) attributed to hypoglycemic glucose values.

## References

Hill et al. (2007): A method for assessing quality of control from
glucose profiles *Diabetic Medicine* **24** .753-758,
[doi:10.1111/j.1464-5491.2007.02119.x](https://doi.org/10.1111/j.1464-5491.2007.02119.x)
.

## Examples

``` r

data(example_data_1_subject)
grade_hypo(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id        GRADE_hypo
#>   <fct>          <dbl>
#> 1 Subject 1      0.221
grade_hypo(example_data_1_subject, lower = 70)
#> # A tibble: 1 × 2
#>   id        GRADE_hypo
#>   <fct>          <dbl>
#> 1 Subject 1      0.118

data(example_data_5_subject)
grade_hypo(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id        GRADE_hypo
#>   <fct>          <dbl>
#> 1 Subject 1      0.221
#> 2 Subject 2      0    
#> 3 Subject 3      0.287
#> 4 Subject 4      0.702
#> 5 Subject 5      0.109
grade_hypo(example_data_5_subject, lower = 65)
#> # A tibble: 5 × 2
#>   id        GRADE_hypo
#>   <fct>          <dbl>
#> 1 Subject 1      0    
#> 2 Subject 2      0    
#> 3 Subject 3      0.191
#> 4 Subject 4      0.297
#> 5 Subject 5      0    
```
