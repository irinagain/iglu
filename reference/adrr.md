# Calculate average daily risk range (ADRR)

The function \`adrr\` produces ADRR values in a tibble object.

## Usage

``` r
adrr(data)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl".

## Value

A tibble object with two columns: subject id and corresponding ADRR
value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for ADRR values is returned. \`NA\` glucose values are omitted
from the calculation of the ADRR values.

ADRR is the average sum of HBGI corresponding to the highest glucose
value and LBGI corresponding to the lowest glucose value for each day,
with the average taken over the daily sums. If there are no high glucose
or no low glucose values, then 0 will be substituted for the HBGI value
or the LBGI value, respectively, for that day.

## References

Kovatchev et al. (2006) Evaluation of a New Measure of Blood Glucose
Variability in, Diabetes *Diabetes care* **29** .2433-2438,
[doi:10.2337/dc06-1085](https://doi.org/10.2337/dc06-1085) .

## Examples

``` r

data(example_data_1_subject)
adrr(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id         ADRR
#>   <fct>     <dbl>
#> 1 Subject 1  15.1

data(example_data_5_subject)
adrr(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id         ADRR
#>   <fct>     <dbl>
#> 1 Subject 1  15.1
#> 2 Subject 2  33.9
#> 3 Subject 3  28.3
#> 4 Subject 4  13.8
#> 5 Subject 5  35.8
```
