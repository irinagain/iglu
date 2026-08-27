# Calculate metrics for the Ambulatory Glucose Profile (AGP)

The function \`agp_metrics\` runs the following functions and combines
them into a tibble object: \`active_percent\`, \`mean_glu\`, \`gmi\`,
\`cv_glu\`, \`below_percent\`, \`in_range_percent\`, \`above_percent\`.

## Usage

``` r
agp_metrics(data, shinyformat = FALSE, tz = '')
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl".

- shinyformat:

  **Default: FALSE.** Boolean indicating whether the output should be
  formatted for the single subject AGP page in shiny.

- tz:

  **Default: "".** A character string specifying the time zone to be
  used. System-specific (see
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html)), but " " is
  the current time zone, and "GMT" is UTC (Universal Time, Coordinated).
  Invalid values are most commonly treated as UTC, on some platforms
  with a warning.

## Value

By default, a tibble object with 1 row for each subject, and 13 columns
is returned: a column for subject id, a column for start date, a column
for end date, a column for number of days, a column for active_percent,
a column for Mean value, a column for gmi value, a column for cv value,
a column for below_54 value, a column for below_70 value, a column for
in_range_70_180 value, a column for above_180 value, a column for
above_250 value,

## Details

The function uses recommended cutoffs of 54, 70, 180, and 250 mg/dL for
calculation.

If \`shinyformat == FALSE\` (default), returns a tibble object with 1
row for each subject, and 12 columns: a column for subject id (\`id\`),
a column for start date (\`start_date\`), a column for end date
(\`end_date\`), a column for number of days (\`ndays\`), a column for
active percent (\`active_percent\`), a column for mean value (\`mean\`),
a column for GMI value (\`GMI\`), a column for CV value (\`CV\`), a
column for a column for a column for a column for a column for

If \`shinyformat == TRUE\`, a tibble with 2 columns: metric and value,
is returned. This output is used when generating the single subject AGP
shiny page.

## References

Johnson et al. (2019) Utilizing the Ambulatory Glucose Profile to
Standardize and Implement Continuous Glucose Monitoring in Clinical
Practice, *Diabetes Technology and Therapeutics* **21:S2** S2-17-S2-25,
[doi:10.1089/dia.2019.0034](https://doi.org/10.1089/dia.2019.0034) .

## Examples

``` r
data(example_data_1_subject)
agp_metrics(example_data_1_subject)
#> # A tibble: 1 × 13
#>   id    start_date          end_date            ndays active_percent  mean   GMI
#>   <fct> <dttm>              <dttm>              <drt>          <dbl> <dbl> <dbl>
#> 1 Subj… 2015-06-06 21:50:27 2015-06-19 13:59:36 12.7…           79.8  124.  6.27
#> # ℹ 6 more variables: CV <dbl>, below_54 <dbl>, below_70 <dbl>,
#> #   in_range_70_180 <dbl>, above_180 <dbl>, above_250 <dbl>
```
