# Calculate Coefficient of Variation (CV) of glucose levels

The function cv_glu produces CV values in a tibble object.

## Usage

``` r
cv_glu(data)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding CV value is returned. If a vector of
glucose values is passed, then a tibble object with just the CV value is
returned. as.numeric() can be wrapped around the latter to output just a
numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for CV values is returned. NA glucose values are omitted from
the calculation of the CV.

CV (Coefficient of Variation) is calculated by \\100 \* sd(G) /
mean(G)\\ Where G is the list of all Glucose measurements for a subject.

## References

Rodbard (2009) Interpretation of continuous glucose monitoring data:
glycemic variability and quality of glycemic control, *Diabetes
Technology and Therapeutics* **11** .55-67,
[doi:10.1089/dia.2008.0132](https://doi.org/10.1089/dia.2008.0132) .

## Examples

``` r

data(example_data_1_subject)
cv_glu(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id           CV
#>   <fct>     <dbl>
#> 1 Subject 1  26.9

data(example_data_5_subject)
cv_glu(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id           CV
#>   <fct>     <dbl>
#> 1 Subject 1  26.9
#> 2 Subject 2  24.0
#> 3 Subject 3  29.1
#> 4 Subject 4  22.4
#> 5 Subject 5  33.5
```
