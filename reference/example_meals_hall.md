# Example mealtimes data from Hall et al. (2018)

Example of mealtimes data format for meal_metrics function, corresponds
to example_data_hall data.

## Usage

``` r
example_meals_hall
```

## Format

A data.frame with 9 rows and 3 columns, which are:

- id:

  identifier of subject

- meal:

  meal type identifier

- mealtime:

  time of meal

## Details

There are 3 types of meals available: Cereal Flakes (CF), Peanut Butter
Sandwich (PB), and Protein Bar (Bar). The number after the abbreviation
refers to the replication number for the original study. For more
details on nutritional differences, please see the original study
reference.

This dataset should be used along with example_data_hall to calculate
meal_metrics.

## References

Hall et al. (2018) : Glucotypes reveal new patterns of glucose
dysregulation *Plos Biology* **16** (7): 3:e2005143
[doi:10.1371/journal.pbio.2005143](https://doi.org/10.1371/journal.pbio.2005143)
.
