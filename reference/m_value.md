# Calculate the M-value

Calculates the M-value of Schlichtkrull et al. (1965) for each subject
in the data, where the M-value is the mean of the logarithmic
transformation of the deviation from a reference value. Produces a
tibble object with subject id and M-values.

## Usage

``` r
m_value(data, r = 90)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

- r:

  A reference value corresponding to basal glycemia in normal subjects;
  default is 90 mg/dL.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding M-value is returned. If a vector of glucose
values is passed, then a tibble object with just the M-value is
returned. as.numeric() can be wrapped around the latter to output just a
numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for the M-values is returned. NA glucose values are omitted
from the calculation of the M-value.

M-value is computed by averaging the transformed glucose values, where
each transformed value is equal to \\\|10 \* log\_{10}(glucose/r)\|^3\\,
where r is the specified reference value.

## References

Schlichtkrull J, Munck O, Jersild M. (1965) The M-value, an index of
blood-sugar control in diabetics. *Acta Medica Scandinavica* **177**
.95-102.
[doi:10.1111/j.0954-6820.1965.tb01810.x](https://doi.org/10.1111/j.0954-6820.1965.tb01810.x)
.

## Examples

``` r
data(example_data_5_subject)

m_value(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id        M_value
#>   <fct>       <dbl>
#> 1 Subject 1    7.02
#> 2 Subject 2   63.7 
#> 3 Subject 3   19.9 
#> 4 Subject 4    7.39
#> 5 Subject 5   34.8 
m_value(example_data_5_subject, r = 100)
#> # A tibble: 5 × 2
#>   id        M_value
#>   <fct>       <dbl>
#> 1 Subject 1    4.10
#> 2 Subject 2   45.4 
#> 3 Subject 3   12.8 
#> 4 Subject 4    4.04
#> 5 Subject 5   24.0 
```
