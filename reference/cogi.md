# Calculate Continuous Glucose Monitoring Index (COGI) values

The function COGI produces cogi values in a tibble object.

## Usage

``` r
cogi(data, targets = c(70, 180), weights = c(.5,.35,.15))
```

## Arguments

- data:

  DataFrame with column names ("id", "time", and "gl"), or numeric
  vector of glucose values.

- targets:

  Numeric vector of two glucose values for threshold. Glucose values
  from data argument will be compared to each value in the targets
  vector to determine the time in range and time below range for COGI.
  The lower value will be used for determining time below range. Default
  list is (70, 180).

- weights:

  Numeric vector of three weights to be applied to time in range, time
  below range and glucose variability, respectively. The default list is
  (.5,.35,.15)

## Value

If a data.frame object is passed, then a tibble object with a column for
subject id and then a column for each target value is returned. If a
vector of glucose values is passed, then a tibble object without the
subject id is returned. as.numeric() can be wrapped around the latter to
output a numeric vector.

## Details

A tibble object with 1 row for each subject, a column for subject id and
column for each target value is returned. NA's will be omitted from the
glucose values in calculation of cogi.

## References

Leelarathna (2020) Evaluating Glucose Control With a Novel Composite
Continuous Glucose Monitoring Index, *Diabetes Technology and
Therapeutics* **14(2)** 277-284,
[doi:10.1177/1932296819838525](https://doi.org/10.1177/1932296819838525)
.

## Examples

``` r

data(example_data_1_subject)

cogi(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id         COGI
#>   <fct>     <dbl>
#> 1 Subject 1  93.0
cogi(example_data_1_subject, targets = c(50, 140), weights = c(.3,.6,.1))
#> # A tibble: 1 × 2
#>   id         COGI
#>   <fct>     <dbl>
#> 1 Subject 1  90.5

data(example_data_5_subject)

cogi(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id         COGI
#>   <fct>     <dbl>
#> 1 Subject 1  93.0
#> 2 Subject 2  57.5
#> 3 Subject 3  85.4
#> 4 Subject 4  95.1
#> 5 Subject 5  74.1
cogi(example_data_5_subject, targets = c(80, 180), weights = c(.2,.4,.4))
#> # A tibble: 5 × 2
#>   id         COGI
#>   <fct>     <dbl>
#> 1 Subject 1  89.9
#> 2 Subject 2  70.0
#> 3 Subject 3  82.0
#> 4 Subject 4  89.3
#> 5 Subject 5  71.5
```
