#' Create a heatmap of metric values by subject based on hierarchical clustering order
#'
#' @inheritParams roc
#' @param metrics precalculated metric values, with first column corresponding to subject id. If 'NULL', the metrics are calculated from supplied 'data' using \code{\link{all_metrics}}
#' @param metric_cluster number of visual metric clusters, default value is 6
#' @param clustering_method the agglomeration method for hierarchical clustering, accepts same values as \code{\link{hclust}}, default value is 'complete'
#' @param clustering_distance_metrics the distance measure for metrics clustering, accepts same values as \code{\link{dist}}, default value is 'correlation' distance
#' @param clustering_distance_subjects the distance measure for subjects clustering, accepts same values as \code{\link{dist}}, default value is 'correlation' distance
#'
#' @return A heatmap of metrics by subjects generated via \code{\link{pheatmap}}
#' @export
#'
#' @examples
#' # Using pre-calculated sd metrics only rather than default (all metrics)
#' mecs = sd_measures(example_data_5_subject)
#' metrics_heatmap(metrics = mecs)
metrics_heatmap <- function(data = NULL, metrics = NULL, metric_cluster = 6, clustering_method = "complete",
                            clustering_distance_metrics = "correlation",
                            clustering_distance_subjects = "correlation"){
  # Check whether pre-calculated metrics are provided
  mecs = metrics
  if(is.null(mecs)){
    if(is.null(data)){
      stop("Either CGM data or precalculated metrics must be provided")
    }else{
      data = check_data_columns(data, time_check=TRUE)
      # No metrics are provided, calculate all by default
      mecs = all_metrics(data)
    }
  }

  # Check that at least 2 subjects are provided
  nids = nrow(mecs)
  if (nids < 4){
    stop("Function requires at least 4 subjects to construct the heatmap")
  }
  nmetrics = ncol(mecs) - 1
  if (nmetrics < 4){
    stop("Function requires at least 4 metrics to construct the heatmap")
  }

 return(metrics_heatmap_pure(mecs, metric_cluster, clustering_method,
                             clustering_distance_metrics, clustering_distance_subjects))
}


# Same as metrics_heatmap
metrics_heatmap_pure <- function(mecs, metric_cluster = 6, clustering_method = "complete",
                            clustering_distance_metrics = "correlation",
                            clustering_distance_subjects = "correlation"){

  # Remove columns that have id, active_percent, ndays, start_date, end_date
  metric_names = names(mecs)
  mecs_mat = mecs[, !(metric_names %in% c("id", "active_percent", "ndays", "start_date", "end_date"))]
  # Check that the remaining columns are numeric
  isNumeric = sapply(mecs_mat, is.numeric)
  if (sum(!isNumeric) > 0){
    warning("Some supplied metrics are non-numeric, only subset of metrics with numeric values is used.")
    mecs_mat = as.matrix(mecs_mat[,isNumeric])
  }else{
    mecs_mat = as.matrix(mecs_mat)
  }

  # Get subject names and name rows accordingly
  rownames(mecs_mat) = as.character(mecs$id)

  # Get metric names
  metric_names = colnames(mecs_mat)

  # Check if any metric has zero variability - remove if it does
  sd_metrics = apply(mecs_mat, 2, sd)
  if (any(sd_metrics == 0)){
    # Remove any metric that has no variability across subjects
    whichzero = metric_names[sd_metrics == 0]
    warning(paste("There is no variability in metric", whichzero, "across subjects. This metric is not used in the heatmap."))
    mecs_mat = mecs_mat[ , sd_metrics > 0]
    metric_names = metric_names[sd_metrics > 0]
  }
  # Do centering and scaling of all metrics before drawing the heatmap
  mecs_mat_scale = scale(mecs_mat)

  # Rename the metrics to make the plots nices
  metric_names[metric_names == "adrr"]="ADRR"
  metric_names[metric_names == "hourly_auc"]="AUC"
  metric_names[metric_names == "conga"]="CONGA"
  metric_names[metric_names == "cv"]="CV"
  metric_names[metric_names == "ea1c"]="eA1C"
  metric_names[metric_names == "gmi"]="GMI"
  metric_names[metric_names == "grade_eugly"]="GRADE eugly"
  metric_names[metric_names == "grade_hyper"]="GRADE hyper"
  metric_names[metric_names == "grade_hypo"]="GRADE hypo"
  metric_names[metric_names == "grade"]="GRADE"
  metric_names[metric_names == "gvp"]="GVP"
  metric_names[metric_names == "hbgi"]="HBGI"
  metric_names[metric_names == "lbgi"]="LBGI"
  metric_names[metric_names == "hyper_index"]="Hyper Index"
  metric_names[metric_names == "hypo_index"]="Hypo Index"
  metric_names[metric_names == "above_140"]="% above 140"
  metric_names[metric_names == "above_180"]="% above 180"
  metric_names[metric_names == "above_250"]="% above 250"
  metric_names[metric_names == "below_54"]="% below 54"
  metric_names[metric_names == "below_70"]="% below 70"
  metric_names[metric_names == "range"]="Range"
  metric_names[metric_names == "iqr"]="IQR"
  metric_names[metric_names == "igc"]="IGC"
  metric_names[metric_names == "j_index"]="J-index"
  metric_names[metric_names == "m_value"]="M-value"
  metric_names[metric_names == "mage"]="MAGE"
  metric_names[metric_names == "1st Qu."]="1st quartile"
  metric_names[metric_names == "3rd Qu."]="3rd quartile"
  metric_names[metric_names == "in_range_63_140"]="% in 63-140"
  metric_names[metric_names == "in_range_70_180"]="% in 70-180"
  metric_names[metric_names == "modd"]="MODD"
  metric_names[metric_names == "sd"]="SDt"
  metric_names[metric_names == "sd_roc"]="SD ROC"
  metric_names[metric_names == "Min."]="Min"
  metric_names[metric_names == "Max."]="Max"
  metric_names[metric_names == "mag"]="MAG"
  metric_names[metric_names == "SdW"]="SDw"
  metric_names[metric_names == "SdHHMM"]="SDhh:mm"
  metric_names[metric_names == "SdWSH"]="SDws h"
  metric_names[metric_names == "SdDM"]="SDdm"
  metric_names[metric_names == "SdB"]="SDb"
  metric_names[metric_names == "SdBDM"]="SDb // dm"
  metric_names[metric_names == "CV_Measures_SD"]="CVsd"
  metric_names[metric_names == "CV_Measures_Mean"]="CVmean"
  colnames(mecs_mat_scale) = metric_names

  # Do heatmap
  p = pheatmap::pheatmap(t(mecs_mat_scale), cutree_rows = metric_cluster,
                         clustering_distance_rows = clustering_distance_metrics,
                         clustering_distance_cols = clustering_distance_subjects,
                         clustering_method = clustering_method, angle_col = 0)
  return(p)
}
