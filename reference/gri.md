# Calculate Glycemia Risk Index (GRI)

The function gri produces a tibble object with values equal to the
glycemia risk index (GRI) metric. The output columns are subject id and
GRI value. ' The output rows correspond to subjects.

## Usage

``` r
gri(data, tz = "")
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

- tz:

  **Default: "".** A character string specifying the time zone to be
  used. System-specific (see
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html)), but " " is
  the current time zone, and "GMT" is UTC (Universal Time, Coordinated).
  Invalid values are most commonly treated as UTC, on some platforms
  with a warning.

## Value

A tibble object with columns for subject id and GRI value. Rows
correspond to individual subjects.

## Details

A tibble object with 1 row for each subject, a column for subject id and
column for GRI is returned. The formula for GRI is as follows:

\\(3.0 × VLow) + (2.4 × Low) + (1.6 × VHigh) + (0.8 × High)\\

where VLow, Low, VHigh, and High correspond to the percent of glucose
values in the ranges \<54 mg/dL, 54-70 mg/dL, \>250 mg/dL, and 180-250
mg/dL respectively. The maximum allowed value for GRI is 100%, any
calculated values higher than 100 are capped.

## References

Klonoff et al. (2022) A Glycemia Risk Index (GRI) of Hypoglycemia and
Hyperglycemia for Continuous Glucose Monitoring Validated by Clinician
Ratings. *J Diabetes Sci Technol*
[doi:10.1177/19322968221085273](https://doi.org/10.1177/19322968221085273)
.

## Author

Elizabeth Chun

## Examples

``` r

data(example_data_1_subject)
gri(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id          GRI
#>   <fct>     <dbl>
#> 1 Subject 1  7.19

data(example_data_5_subject)
gri(example_data_5_subject, tz = 'GMT')
#> # A tibble: 5 × 2
#>   id          GRI
#>   <fct>     <dbl>
#> 1 Subject 1  7.19
#> 2 Subject 2 79.7 
#> 3 Subject 3 20.0 
#> 4 Subject 4  4.38
#> 5 Subject 5 39.5 
```
