# Calculate GMI

The function gmi produces GMI values in a tibble object.

## Usage

``` r
gmi(data)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding GMI is returned. If a vector of glucose
values is passed, then a tibble object with just the GMI value is
returned. as.numeric() can be wrapped around the latter to output just a
numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for GMI values is returned. NA glucose values are omitted from
the calculation of the GMI.

GMI score is calculated by \\3.31 + (.02392\*mean(G))\\ where G is the
vector of Glucose Measurements (mg/dL).

## References

Bergenstal (2018) Glucose Management Indicator (GMI): A New Term for
Estimating A1C From Continuous Glucose Monitoring *Hormone and Metabolic
Research* **41** .2275-2280,
[doi:10.2337/dc18-1581](https://doi.org/10.2337/dc18-1581) .

## Examples

``` r

data(example_data_1_subject)
gmi(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id          GMI
#>   <fct>     <dbl>
#> 1 Subject 1  6.27

data(example_data_5_subject)
gmi(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id          GMI
#>   <fct>     <dbl>
#> 1 Subject 1  6.27
#> 2 Subject 2  8.54
#> 3 Subject 3  6.99
#> 4 Subject 4  6.41
#> 5 Subject 5  7.49
```
