# Percentage of GRADE score attributable to target range

The function grade_eugly produces %GRADE euglycemia values in a tibble
object.

## Usage

``` r
grade_eugly(data, lower = 70, upper = 140)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

- lower:

  Lower bound used for hypoglycemia cutoff, in mg/dL. Default is 70

- upper:

  Upper bound used for hyperglycemia cutoff, in mg/dL. Default is 140.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding %GRADE euglycemia value is returned. If a
vector of glucose values is passed, then a tibble object with just the
%GRADE euglycemia value is returned. as.numeric() can be wrapped around
the latter to output just a numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for %GRADE euglycemia values is returned. NA glucose values are
omitted from the calculation of the %GRADE euglycemia values.

%GRADE euglycemia is determined by calculating the percentage of GRADE
score (see grade function) attributed to values in the target range,
i.e. values not below hypoglycemic or above hyperglycemic cutoffs.

## References

Hill et al. (2007): A method for assessing quality of control from
glucose profiles *Diabetic Medicine* **24** .753-758,
[doi:10.1111/j.1464-5491.2007.02119.x](https://doi.org/10.1111/j.1464-5491.2007.02119.x)
.

## Examples

``` r

data(example_data_1_subject)
grade_eugly(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id        GRADE_eugly
#>   <fct>           <dbl>
#> 1 Subject 1        28.2
grade_eugly(example_data_1_subject, lower = 80, upper = 180)
#> # A tibble: 1 × 2
#>   id        GRADE_eugly
#>   <fct>           <dbl>
#> 1 Subject 1        66.7

data(example_data_5_subject)
grade_eugly(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id        GRADE_eugly
#>   <fct>           <dbl>
#> 1 Subject 1      28.2  
#> 2 Subject 2       0.645
#> 3 Subject 3      20.2  
#> 4 Subject 4      34.1  
#> 5 Subject 5       6.68 
grade_eugly(example_data_5_subject, lower = 80, upper = 160)
#> # A tibble: 5 × 2
#>   id        GRADE_eugly
#>   <fct>           <dbl>
#> 1 Subject 1       51.3 
#> 2 Subject 2        3.69
#> 3 Subject 3       34.0 
#> 4 Subject 4       60.0 
#> 5 Subject 5       17.9 
```
