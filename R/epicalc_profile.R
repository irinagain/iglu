#' Display Episode Calculation statistics for selected subject
#' @name epicalc_profile
#'
#' @inheritParams episode_calculation
#' @param subject String corresponding to subject id
#'
#' @return A plot displaying (1) the statistics for the episodes and (2) the episodes colored by level.
#'
#' @export
#'
#' @author Johnathan Shih, Jung Hoon Seo, Elizabeth Chun
#'
#' @seealso episode_calculation()
#'
#' @examples
#' epicalc_profile(example_data_1_subject)
#'

epicalc_profile <- function(data,lv1_hypo=70,lv2_hypo=54,lv1_hyper=180,lv2_hyper=250,
                            dur_length = 15, subject = NULL, dt0 = NULL, inter_gap = 45, tz = ""){

  #Clean up Global environment
  id = num_levels = NULL
  rm(list = c("id", "num_levels"))

  if (!is.null(subject)){
    data = data[data$id == subject, ]
  }

  #Checking for more than 1 subject
  ns = length(unique(data$id))
  if (ns > 1){
    subject = unique(data$id)[1]
    warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
    data = data[data$id == subject, ]
  }


  #Calling episode_calculation for data
  episodes = episode_calculation(data, lv1_hypo, lv2_hypo, lv1_hyper, lv2_hyper,
                                 return_data = TRUE, dur_length, dt0, inter_gap, tz)

  ep_summary = episodes[[1]]
  ep_data = episodes[[2]]

  #Creating table 1(t1) -------------------------------------
  tableStat = data.frame("Hypoglycemia/Hyperglycemia episode metrics")
  tableStat[1, 1] = ""
  tableStat[1, 2] = "Hypoglycemia"
  tableStat[1, 3] = "Hypoglycemia"
  tableStat[1, 4] = "Hypoglycemia"
  tableStat[1, 5] = "Hyperglycemia"
  tableStat[1, 6] = "Hyperglycemia"

  tableStat[2, 1] = ""
  tableStat[2, 2] = "Level 1"
  tableStat[2, 3] = "Level 2"
  tableStat[2, 4] = "Extended"
  tableStat[2, 5] = "Level 1"
  tableStat[2, 6] = "Level 2"

  tableStat[3, 1] = "Thresholds"
  tableStat[3, 2] = paste0("<", as.character(lv1_hypo), " mg/dL")
  tableStat[3, 3] = paste0("<", as.character(lv2_hypo), " mg/dL")
  tableStat[3, 4] = paste0("<", as.character(lv1_hypo), " mg/dL")
  tableStat[3, 5] = paste0(">", as.character(lv1_hyper), " mg/dL")
  tableStat[3, 6] = paste0(">", as.character(lv2_hyper), " mg/dL")

  tableStat[4, 1] = "Avg Episodes/Day"
  tableStat[4, 2] = as.character(format(round(ep_summary$avg_ep_per_day[1], 2), nsmall = 2))
  tableStat[4, 3] = as.character(format(round(ep_summary$avg_ep_per_day[2], 2), nsmall = 2))
  tableStat[4, 4] = as.character(format(round(ep_summary$avg_ep_per_day[3], 2), nsmall = 2))
  tableStat[4, 5] = as.character(format(round(ep_summary$avg_ep_per_day[4], 2), nsmall = 2))
  tableStat[4, 6] = as.character(format(round(ep_summary$avg_ep_per_day[5], 2), nsmall = 2))

  tableStat[5, 1] = "Mean duration"
  tableStat[5, 2] = paste0(as.character(format(round(ep_summary$avg_ep_duration[1], 2), nsmall = 2)), " min")
  tableStat[5, 3] = paste0(as.character(format(round(ep_summary$avg_ep_duration[2], 2), nsmall = 2)), " min")
  tableStat[5, 4] = paste0(as.character(format(round(ep_summary$avg_ep_duration[3], 2), nsmall = 2)), " min")
  tableStat[5, 5] = paste0(as.character(format(round(ep_summary$avg_ep_duration[4], 2), nsmall = 2)), " min")
  tableStat[5, 6] = paste0(as.character(format(round(ep_summary$avg_ep_duration[5], 2), nsmall = 2)), " min")

  tableStat[6, 1] = "Mean glucose"
  tableStat[6, 2] = paste0(as.character(format(round(ep_summary$avg_ep_gl[1], 2), nsmall = 2)), " mg/dl")
  tableStat[6, 3] = paste0(as.character(format(round(ep_summary$avg_ep_gl[2], 2), nsmall = 2)), " mg/d")
  tableStat[6, 4] = paste0(as.character(format(round(ep_summary$avg_ep_gl[3], 2), nsmall = 2)), " mg/d")
  tableStat[6, 5] = paste0(as.character(format(round(ep_summary$avg_ep_gl[4], 2), nsmall = 2)), " mg/d")
  tableStat[6, 6] = paste0(as.character(format(round(ep_summary$avg_ep_gl[5], 2), nsmall = 2)), " mg/d")

  #Styling the table
  mytheme <- gridExtra::ttheme_minimal(base_size = 10, padding = unit(c(4,2),"mm"))
  t1 <- gridExtra::tableGrob(tableStat, rows = NULL, cols = NULL, theme = mytheme )

  #Adding border(t1)
  t1 <- gtable::gtable_add_grob(t1,
                                grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 5)),
                                t = 1, b = 6, l = 1, r = 6)
  #Adding dotted separator(t1)
  separators <- replicate(ncol(t1) - 2,
                          grid::segmentsGrob(x1 = unit(0, "npc"), gp=grid::gpar(lty=2)),
                          simplify=FALSE)

  t1 <- gtable::gtable_add_grob(t1, grobs = separators,
                                t = 2, b = nrow(t1), l = seq_len(ncol(t1)-2)+2)
  padding <- unit(0.5,"line")

  #Adding title and footnote(t1)
  title <- grid::textGrob("Episode Metrics",gp=grid::gpar(fontsize=18), x=0, hjust=0)
  footnote <- grid::textGrob(paste0("An episode is >= ", dur_length, " continuous minutes"), x=1, hjust=1,
                             gp=grid::gpar( fontface="italic", fontsize = 8))

  padding <- unit(0.5,"line")
  t1 <- gtable::gtable_add_rows(t1,
                                heights = grid::grobHeight(title) + padding,
                                pos = 0)
  t1 <- gtable::gtable_add_rows(t1,
                                heights = grid::grobHeight(footnote)+ padding)
  t1 <- gtable::gtable_add_grob(t1, list(title, footnote),
                                t=c(1, nrow(t1)), l=c(1,2),
                                r=ncol(t1))


  # Creating overall plot(p1) ---------------------------------

  # recode since lv2 is a subset of lv1
  labels = c("lv1_hypo", "lv2_hypo", "lv1_hyper", "lv2_hyper")
  plot_data = ep_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      num_levels = sum(c(lv1_hypo != 0, lv2_hypo != 0, lv1_hyper != 0, lv2_hyper != 0)),
      class = ifelse(
        # either no types - normal, one type - keep nonzero, subset - choose lv2
        num_levels == 0, "Normal",
        ifelse(num_levels == 1,
               labels[which(c(lv1_hypo != 0, lv2_hypo != 0, lv1_hyper != 0, lv2_hyper != 0))],
               c("lv2_hypo", "lv2_hyper")[which(c(lv2_hypo != 0, lv2_hyper != 0))])
      ),
      class = factor(class, levels = c("lv2_hypo", "lv1_hypo", "Normal", "lv1_hyper", "lv2_hyper"))
    )

  # match plot ranges colors (AGP)
  colors <- c("#F9B500", "#F9F000", "#48BA3C", "#F92D00", "#8E1B1B")
  p1 = ggplot(plot_data) +
    geom_point(aes(time, gl, color = class)) +
    scale_color_manual(values = colors, drop = FALSE,
                       labels = c("lv2_hypo", "lv1_hypo", "Normal", "lv1_hyper", "lv2_hyper")) +
    ggplot2::scale_x_datetime(name = 'Date') +
    ggplot2::scale_y_continuous(name = 'Blood Glucose')


  #adding all figures together ---------------------------

  pFinal = (

    wrap_elements(t1) + plot_layout()) / p1

  pFinal


  # }#end Function
}

