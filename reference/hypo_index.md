# Calculate Hypoglycemia Index

The function hypo_index produces Hypoglycemia index values in a tibble
object.

## Usage

``` r
hypo_index(data, LLTR = 80, b = 2, d = 30)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

- LLTR:

  Lower Limit of Target Range, default value is 80 mg/dL.

- b:

  Exponent, generally in the range from 1.0 to 2.0, default value is 2.

- d:

  Scaling factor,to display Hyperglycemia Index, Hypoglycemia Index, and
  IGC on approximately the same numerical range as measurements of HBGI,
  LBGI and GRADE, default value is 30.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding Hypoglycemia Index value is returned. If a
vector of glucose values is passed, then a tibble object with just the
Hypoglycemia Index value is returned. as.numeric() can be wrapped around
the latter to output just a numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for the Hypoglycemia Index values is returned. NA glucose
values are omitted from the calculation of the Hypoglycemia Index
values.

Hypoglycemia Index is calculated by \\\frac{1}{n \cdot d} \sum
\left(ULTR - hyperBG_j \right)^b\\. Here n is the total number of
Glucose measurements (excluding NA values), and \\hypoBG_j\\ is the jth
Glucose measurement below the LLTR cutoff, b is an exponent, and d is a
scaling factor.

## References

Rodbard (2009) Interpretation of continuous glucose monitoring data:
glycemic variability and quality of glycemic control, *Diabetes
Technology and Therapeutics* **11** .55-67,
[doi:10.1089/dia.2008.0132](https://doi.org/10.1089/dia.2008.0132) .

## Examples

``` r
data(example_data_1_subject)
hypo_index(example_data_1_subject, LLTR = 60)
#> # A tibble: 1 × 2
#>   id        hypo_index
#>   <fct>          <dbl>
#> 1 Subject 1          0

data(example_data_5_subject)
hypo_index(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id        hypo_index
#>   <fct>          <dbl>
#> 1 Subject 1     0.0103
#> 2 Subject 2     0     
#> 3 Subject 3     0.0378
#> 4 Subject 4     0.0437
#> 5 Subject 5     0.0120
hypo_index(example_data_5_subject, LLTR = 70)
#> # A tibble: 5 × 2
#>   id        hypo_index
#>   <fct>          <dbl>
#> 1 Subject 1   0.000480
#> 2 Subject 2   0       
#> 3 Subject 3   0.00626 
#> 4 Subject 4   0.00978 
#> 5 Subject 5   0.000387
```
