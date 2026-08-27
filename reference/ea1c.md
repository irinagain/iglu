# Calculate eA1C

The function ea1c produces eA1C values in a tibble object.

## Usage

``` r
ea1c(data)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding eA1C is returned. If a vector of glucose
values is passed, then a tibble object with just the eA1C value is
returned. as.numeric() can be wrapped around the latter to output just a
numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for eA1C values is returned. NA glucose values are omitted from
the calculation of the eA1C.

eA1C score is calculated by \\(46.7+mean(G))/28.7\\ where G is the
vector of Glucose Measurements (mg/dL).

## References

Nathan (2008) Translating the A1C assay into estimated average glucose
values *Hormone and Metabolic Research* **31** .1473-1478,
[doi:10.2337/dc08-0545](https://doi.org/10.2337/dc08-0545) .

## Author

Marielle Hicban

## Examples

``` r

data(example_data_1_subject)
ea1c(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id         eA1C
#>   <fct>     <dbl>
#> 1 Subject 1  5.94

data(example_data_5_subject)
ea1c(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id         eA1C
#>   <fct>     <dbl>
#> 1 Subject 1  5.94
#> 2 Subject 2  9.24
#> 3 Subject 3  6.99
#> 4 Subject 4  6.15
#> 5 Subject 5  7.71
```
