# Calculate mean GRADE score

The function grade produces GRADE score values in a tibble object.

## Usage

``` r
grade(data)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding GRADE value is returned. If a vector of
glucose values is passed, then a tibble object with just the GRADE value
is returned. as.numeric() can be wrapped around the latter to output
just a numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for GRADE values is returned. NA glucose values are omitted
from the calculation of the GRADE.

GRADE score is calculated by \\1/n \* \sum \[425 \* (log(log(G_i /
18)) + .16)^2\]\\ Where \\G_i\\ is the ith Glucose measurement and n is
the total number of measurements.

## References

Hill et al. (2007): A method for assessing quality of control from
glucose profiles *Diabetic Medicine* **24** .753-758,
[doi:10.1111/j.1464-5491.2007.02119.x](https://doi.org/10.1111/j.1464-5491.2007.02119.x)
.

## Examples

``` r

data(example_data_1_subject)
grade(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id        GRADE
#>   <fct>     <dbl>
#> 1 Subject 1  3.47

data(example_data_5_subject)
grade(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id        GRADE
#>   <fct>     <dbl>
#> 1 Subject 1  3.47
#> 2 Subject 2 15.8 
#> 3 Subject 3  7.26
#> 4 Subject 4  4.16
#> 5 Subject 5 10.1 
```
