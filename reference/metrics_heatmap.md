# Create a heatmap of metric values by subject based on hierarchical clustering order

Create a heatmap of metric values by subject based on hierarchical
clustering order

## Usage

``` r
metrics_heatmap(
  data = NULL,
  metrics = NULL,
  metric_cluster = 6,
  clustering_method = "complete",
  clustering_distance_metrics = "correlation",
  clustering_distance_subjects = "correlation",
  tz = ""
)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl".

- metrics:

  precalculated metric values, with first column corresponding to
  subject id. If 'NULL', the metrics are calculated from supplied 'data'
  using
  [`all_metrics`](https://irinagain.github.io/iglu/reference/all_metrics.md)

- metric_cluster:

  number of visual metric clusters, default value is 6

- clustering_method:

  the agglomeration method for hierarchical clustering, accepts same
  values as [`hclust`](https://rdrr.io/r/stats/hclust.html), default
  value is 'complete'

- clustering_distance_metrics:

  the distance measure for metrics clustering, accepts same values as
  [`dist`](https://rdrr.io/r/stats/dist.html), default value is
  'correlation' distance

- clustering_distance_subjects:

  the distance measure for subjects clustering, accepts same values as
  [`dist`](https://rdrr.io/r/stats/dist.html), default value is
  'correlation' distance

- tz:

  **Default: "".** A character string specifying the time zone to be
  used. System-specific (see
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html)), but " " is
  the current time zone, and "GMT" is UTC (Universal Time, Coordinated).
  Invalid values are most commonly treated as UTC, on some platforms
  with a warning.

## Value

A heatmap of metrics by subjects generated via
[`pheatmap`](https://rdrr.io/pkg/pheatmap/man/pheatmap.html)

## Examples

``` r
# Using pre-calculated sd metrics only rather than default (all metrics)
mecs = sd_measures(example_data_5_subject)
metrics_heatmap(metrics = mecs)
```
