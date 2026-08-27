# Calculate Glucose Variability Percentage (GVP)

The function mad produces GVP values in a tibble object.

## Usage

``` r
gvp(data)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl"

## Value

A tibble object with two columns: subject id and corresponding GVP
value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for GVP values is returned. NA glucose values are omitted from
the calculation of the GVP.

GVP is calculated by dividing the total length of the line of the
glucose trace by the length of a perfectly flat trace. The formula for
this is \\sqrt(diff^2+dt0^2)/(n\*dt0)\\, where diff is the change in
Glucose measurements from one reading to the next, dt0 is the time gap
between measurements and n is the number of glucose readings

## References

Peyser et al. (2017) Glycemic Variability Percentage: A Novel Method for
Assessing Glycemic Variability from Continuous Glucose Monitor Data.
*Diabetes Technol Ther* **20**(1):6–16,
[doi:10.1089/dia.2017.0187](https://doi.org/10.1089/dia.2017.0187) .

## Author

David Buchanan, Mary Martin

## Examples

``` r

data(example_data_1_subject)
gvp(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id          GVP
#>   <fct>     <dbl>
#> 1 Subject 1  16.8

data(example_data_5_subject)
gvp(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id          GVP
#>   <fct>     <dbl>
#> 1 Subject 1  16.8
#> 2 Subject 2  20.5
#> 3 Subject 3  25.6
#> 4 Subject 4  17.1
#> 5 Subject 5  38.3
```
