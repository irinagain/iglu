# Plot Time in Ranges as a bar plot

The function plot_ranges produces a barplot showing the percent of time
in glucose ranges.

## Usage

``` r
plot_ranges(data)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl". Should only
  be data for 1 subject. In case multiple subject ids are detected, a
  warning is produced and only 1st subject is used.

## Value

Single subject bar chart showing percent in different glucose ranges.

## Details

Only a single subject's data may be used. There are four ranges: very
low (below 54 mg/dL), low (54-69 mg/dL), target range (70-180 mg/dL),
high (181-250 mg/dL), and very high (above 250 mg/dL). This plot is
meant to be used as part of the Ambulatory Glucose Profile (AGP)

## References

Johnson et al. (2019) Utilizing the Ambulatory Glucose Profile to
Standardize and Implement Continuous Glucose Monitoring in Clinical
Practice, *Diabetes Technology and Therapeutics* **21:S2** S2-17-S2-25,
[doi:10.1089/dia.2019.0034](https://doi.org/10.1089/dia.2019.0034) .

## Author

Elizabeth Chun

## Examples

``` r

data(example_data_1_subject)
plot_ranges(example_data_1_subject)

```
