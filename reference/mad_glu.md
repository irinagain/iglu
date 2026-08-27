# Calculate Median Absolute Deviation (MAD)

The function mad produces MAD values in a tibble object.

## Usage

``` r
mad_glu(data, constant = 1.4826)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl", or numeric
  vector of glucose values.

- constant:

  Numeric object which will be multipled by the MAD value. Defaults to
  1.4826. Reasons for this default value can be seen in the details
  section of the documentation of r's base mad method

## Value

If a data.frame object is passed, then a tibble object with two columns:
subject id and corresponding MAD value is returned. If a vector of
glucose values is passed, then a tibble object with just the MAD value
is returned. as.numeric() can be wrapped around the latter to output
just a numeric value.

## Details

A tibble object with 1 row for each subject, a column for subject id and
a column for MAD values is returned. NA glucose values are omitted from
the calculation of the MAD.

MAD is calculated by taking the median of the difference of the glucose
readings from their median and multiplying it by a scaling factor
\\1.4826\*median(\|gl-median(gl)\|)\\, where gl is the list of Glucose
measurements.

## Author

David Buchanan, Marielle Hicban

## Examples

``` r

data(example_data_1_subject)
mad_glu(example_data_1_subject)
#> # A tibble: 1 × 2
#>   id          MAD
#>   <fct>     <dbl>
#> 1 Subject 1  25.2

data(example_data_5_subject)
mad_glu(example_data_5_subject)
#> # A tibble: 5 × 2
#>   id          MAD
#>   <fct>     <dbl>
#> 1 Subject 1  25.2
#> 2 Subject 2  53.4
#> 3 Subject 3  31.1
#> 4 Subject 4  29.7
#> 5 Subject 5  53.4
```
