# Calculate Index of Glycemic Control

The function igc produces IGC values in a tibble object.

## Usage

``` r
igc(data, LLTR = 80, ULTR = 140, a = 1.1, b = 2, c = 30, d = 30)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

- LLTR:

  Lower Limit of Target Range, default value is 80 mg/dL.

- ULTR:

  Upper Limit of Target Range, default value is 140 mg/dL.

- a:

  Exponent, generally in the range from 1.0 to 2.0, default value is
  1.1.

- b:

  Exponent, generally in the range from 1.0 to 2.0, default value is 2.

- c:

  Scaling factor, to display Hyperglycemia Index, Hypoglycemia Index,
  and IGC on approximately the same numerical range as measurements of
  HBGI, LBGI and GRADE, default value is 30.

- d:

  Scaling factor,to display Hyperglycemia Index, Hypoglycemia Index, and
  IGC on approximately the same numerical range as measurements of HBGI,
  LBGI and GRADE, default value is 30.

## Value

A tibble object with two columns: subject id and corresponding IGC
value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for the IGC values is returned.

IGC is calculated by taking the sum of the Hyperglycemia Index and the
Hypoglycemia index. See
[`hypo_index`](https://irinagain.github.io/iglu/reference/hypo_index.md)
and
[`hyper_index`](https://irinagain.github.io/iglu/reference/hyper_index.md).

## References

Rodbard (2009) Interpretation of continuous glucose monitoring data:
glycemic variability and quality of glycemic control, *Diabetes
Technology and Therapeutics* **11** .55-67,
[doi:10.1089/dia.2008.0132](https://doi.org/10.1089/dia.2008.0132) .

## Examples

``` r
data(example_data_1_subject)
igc(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id          IGC
#>   <fct>     <dbl>
#> 1 Subject 1 0.401
igc(example_data_1_subject, ULTR = 160)
#> # A tibble: 1 × 2
#>   id          IGC
#>   <fct>     <dbl>
#> 1 Subject 1 0.203

data(example_data_5_subject)
igc(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id          IGC
#>   <fct>     <dbl>
#> 1 Subject 1 0.401
#> 2 Subject 2 4.17 
#> 3 Subject 3 1.22 
#> 4 Subject 4 0.401
#> 5 Subject 5 2.22 
igc(example_data_5_subject, LLTR = 75, ULTR = 150)
#> # A tibble: 5 × 2
#>   id          IGC
#>   <fct>     <dbl>
#> 1 Subject 1 0.278
#> 2 Subject 2 3.64 
#> 3 Subject 3 0.965
#> 4 Subject 4 0.242
#> 5 Subject 5 1.86 
```
