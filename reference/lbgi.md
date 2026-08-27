# Calculate Low Blood Glucose Index (LBGI)

The function lbgi produces LBGI values in a tibble object.

## Usage

``` r
lbgi(data)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding LBGI value is returned. If a vector of
glucose values is passed, then a tibble object with just the LBGI value
is returned. as.numeric() can be wrapped around the latter to output
just a numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for LBGI values is returned. NA glucose values are omitted from
the calculation of the LBGI.

LBGI is calculated by \\1/n \* \sum (10 \* fbg_i ^2)\\, where \\fbg_i =
min(0, 1.509 \* (log(G_i)^{1.084} - 5.381)\\, G_i is the ith Glucose
measurement for a subject, and n is the total number of measurements for
that subject.

## References

Kovatchev et al. (2006) Evaluation of a New Measure of Blood Glucose
Variability in, Diabetes *Diabetes care* **29** .2433-2438,
[doi:10.2337/dc06-1085](https://doi.org/10.2337/dc06-1085) .

## Examples

``` r

data(example_data_1_subject)
lbgi(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id         LBGI
#>   <fct>     <dbl>
#> 1 Subject 1 0.432

data(example_data_5_subject)
lbgi(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id           LBGI
#>   <fct>       <dbl>
#> 1 Subject 1 0.432  
#> 2 Subject 2 0.00464
#> 3 Subject 3 0.142  
#> 4 Subject 4 0.356  
#> 5 Subject 5 0.195  
```
