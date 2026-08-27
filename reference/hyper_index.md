# Calculate Hyperglycemia Index

The function hyper_index produces Hyperglycemia Index values in a tibble
object.

## Usage

``` r
hyper_index(data, ULTR = 140, a = 1.1, c = 30)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

- ULTR:

  Upper Limit of Target Range, default value is 140 mg/dL.

- a:

  Exponent, generally in the range from 1.0 to 2.0, default value is
  1.1.

- c:

  Scaling factor, to display Hyperglycemia Index, Hypoglycemia Index,
  and IGC on approximately the same numerical range as measurements of
  HBGI, LBGI and GRADE, default value is 30.

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding Hyperglycemia Index value is returned. If a
vector of glucose values is passed, then a tibble object with just the
Hyperglycemia Index value is returned. as.numeric() can be wrapped
around the latter to output just a numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for the Hyperglycemia Index values is returned. NA glucose
values are omitted from the calculation of the Hyperglycemia Index
values.

Hyperglycemia Index is calculated by \\\frac{1}{n \cdot c} \sum
\left(hyperBG_j - ULTR \right)^a\\. Here n is the total number of
Glucose measurements (excluding NA values), \\hyperBG_j\\ is the jth
Glucose measurement above the ULTR cutoff, a is an exponent, and c is a
scaling factor.

## References

Rodbard (2009) Interpretation of continuous glucose monitoring data:
glycemic variability and quality of glycemic control, *Diabetes
Technology and Therapeutics* **11** .55-67,
[doi:10.1089/dia.2008.0132](https://doi.org/10.1089/dia.2008.0132) .

## Examples

``` r
data(example_data_1_subject)
hyper_index(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id        hyper_index
#>   <fct>           <dbl>
#> 1 Subject 1       0.391
hyper_index(example_data_1_subject, ULTR = 160)
#> # A tibble: 1 × 2
#>   id        hyper_index
#>   <fct>           <dbl>
#> 1 Subject 1       0.192

data(example_data_5_subject)
hyper_index(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id        hyper_index
#>   <fct>           <dbl>
#> 1 Subject 1       0.391
#> 2 Subject 2       4.17 
#> 3 Subject 3       1.18 
#> 4 Subject 4       0.358
#> 5 Subject 5       2.21 
hyper_index(example_data_5_subject, ULTR = 150)
#> # A tibble: 5 × 2
#>   id        hyper_index
#>   <fct>           <dbl>
#> 1 Subject 1       0.275
#> 2 Subject 2       3.64 
#> 3 Subject 3       0.947
#> 4 Subject 4       0.222
#> 5 Subject 5       1.86 
```
