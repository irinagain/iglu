# Calculate J-index

The function j_index produces J-Index values a tibble object.

## Usage

``` r
j_index(data)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding J-Index value is returned. If a vector of
glucose values is passed, then a tibble object with just the J-Index
value is returned. as.numeric() can be wrapped around the latter to
output just a numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for J-Index values is returned. NA glucose values are omitted
from the calculation of the J-Index.

J-Index score is calculated by \\.001 \* \[mean(G) + sd(G)\]^2\\ where G
is the list of Glucose Measurements.

## References

Wojcicki (1995) "J"-index. A new proposition of the assessment of
current glucose control in diabetic patients *Hormone and Metabolic
Research* **27** .41-42,
[doi:10.1055/s-2007-979906](https://doi.org/10.1055/s-2007-979906) .

## Examples

``` r

data(example_data_1_subject)
j_index(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id        J_index
#>   <fct>       <dbl>
#> 1 Subject 1    24.6

data(example_data_5_subject)
j_index(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id        J_index
#>   <fct>       <dbl>
#> 1 Subject 1    24.6
#> 2 Subject 2    73.3
#> 3 Subject 3    39.5
#> 4 Subject 4    25.2
#> 5 Subject 5    54.4
```
