#' Display Episode Calculation statistics for selected subject
#' @name epicalc_profile
#'
#' @usage
#' epicalc_profile(data, lv1_hypo=100.0, lv2_hypo = 70, lv1_hyper= 120.0, lv2_hyper = 160, color_scheme = "Color Scheme 1")
#'
#' @param data data
#'
#' @param lv1_hypo hypo_thres
#'
#' @param lv2_hypo hypo_thres
#'
#' @param lv1_hyper lv1_hyper
#'
#' @param lv2_hyper lv2_hyper
#'
#' @param color_scheme color_scheme
#'
#' @return A plot displaying the varying glucose levels (mg/dL) of the subject in a day as well as the statistics for the episodes.
#'
#' @export
#'
#' @import
#' gtable
#' grid
#' patchwork
#' gridExtra
#' @author Johnathan Shih, Jung Hoon Seo
#'
#' @examples
#' epicalc_profile(example_data_1_subject)
#'

epicalc_profile <- function(data, lv1_hypo=100.0, lv2_hypo = 70, lv1_hyper= 120.0, lv2_hyper = 160, color_scheme = "Color Scheme 1"){

  #Importing the data
  subject = unique(data$id)
  data_ip = CGMS2DayByDay(data, dt0 = 5)


  gl_ip = data_ip[[1]]
  test = data_ip$actual_dates
  day_label = as.character(test)

  ns = length(subject)
  if (ns > 1){
    subject = subject[1]
    warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
    data = data %>% dplyr::filter(id == subject)
  }

  epicalc = episode_calculation(data, lv1_hypo, lv2_hypo, lv1_hyper, lv2_hyper)

  gl_ip.t = t(gl_ip)
  #Checking for multiple subjects
  subject = unique(data$id)

  #Creating table 1(t1) -------------------------------------
  tableStat = data.frame("Hypoglycemia/Hyperglycemia episode metrics")
  tableStat[1, 1] = ""
  tableStat[1, 2] = "Hypoglycemia"
  tableStat[1, 3] = "Hypoglycemia"
  tableStat[1, 4] = "Hyperglycemia"
  tableStat[1, 5] = "Hyperglycemia"

  tableStat[2, 1] = ""
  tableStat[2, 2] = "Level 2"
  tableStat[2, 3] = "Level 1"
  tableStat[2, 4] = "Level 1"
  tableStat[2, 5] = "Level 2"

  tableStat[3, 1] = "Thresholds"
  tableStat[3, 2] = paste0("<", as.character(lv2_hypo), " mg/dL")
  tableStat[3, 3] = paste0("<", as.character(lv1_hypo), " mg/dL")
  tableStat[3, 4] = paste0(">", as.character(lv1_hyper), " mg/dL")
  tableStat[3, 5] = paste0(">", as.character(lv2_hyper), " mg/dL")

  tableStat[4, 1] = "(Mean)Episodes/Day"
  tableStat[4, 2] = as.character(format(round(epicalc[1,]$Hypo_ep, 2), nsmall = 2))
  tableStat[4, 3] = as.character(format(round(epicalc[1,]$Hyper_ep, 2), nsmall = 2))
  tableStat[4, 4] = as.character(format(round(epicalc[2,]$Hypo_ep, 2), nsmall = 2))
  tableStat[4, 5] = as.character(format(round(epicalc[2,]$Hyper_ep, 2), nsmall = 2))

  tableStat[5, 1] = "Mean duration"
  tableStat[5, 2] = paste0(as.character(format(round(epicalc[1,]$hypo_duration, 2), nsmall = 2)), " min")
  tableStat[5, 3] = paste0(as.character(format(round(epicalc[1,]$hyper_duration, 2), nsmall = 2)), " min")
  tableStat[5, 4] = paste0(as.character(format(round(epicalc[2,]$hypo_duration,, 2), nsmall = 2)), " min")
  tableStat[5, 5] = paste0(as.character(format(round(epicalc[2,]$hyper_duration, 2), nsmall = 2)), " min")

  tableStat[6, 1] = "Avg min (per day)"
  tableStat[6, 2] = paste0(as.character(format(round(epicalc[1,]$hypo_min_avg, 2), nsmall = 2)), " min")
  tableStat[6, 3] = paste0(as.character(format(round(epicalc[1,]$hyper_min_avg, 2), nsmall = 2)), " min")
  tableStat[6, 4] = paste0(as.character(format(round(epicalc[2,]$hypo_min_avg, 2), nsmall = 2)), " min")
  tableStat[6, 5] = paste0(as.character(format(round(epicalc[2,]$hyper_min_avg, 2), nsmall = 2)), " min")

  mytheme <- gridExtra::ttheme_minimal(base_size = 10, padding = unit(c(4,2),"mm"))

  t1 <- gridExtra::tableGrob(tableStat, rows = NULL, cols = NULL, theme = mytheme )

  #Adding border(t1)
  t1 <- gtable_add_grob(t1,
                        grobs = rectGrob(gp = gpar(fill = NA, lwd = 5)),
                        t = 1, b = 6, l = 1, r = 5)

  #Adding dotted separator(t1)
  separators <- replicate(ncol(t1) - 2,
                          segmentsGrob(x1 = unit(0, "npc"), gp=gpar(lty=2)),
                          simplify=FALSE)

  t1 <- gtable_add_grob(t1, grobs = separators,
                        t = 2, b = nrow(t1), l = seq_len(ncol(t1)-2)+2)
  padding <- unit(0.5,"line")

  #Adding title and footnote(t1)
  title <- textGrob("Episode Metrics",gp=gpar(fontsize=18), x=0, hjust=0)
  footnote <- textGrob("An episode is >= 15 continuous minutes", x=1, hjust=1,

                       gp=gpar( fontface="italic", fontsize = 8))

  padding <- unit(0.5,"line")
  t1 <- gtable_add_rows(t1,
                        heights = grobHeight(title) + padding,
                        pos = 0)
  t1 <- gtable_add_rows(t1,
                        heights = grobHeight(footnote)+ padding)
  t1 <- gtable_add_grob(t1, list(title, footnote),
                        t=c(1, nrow(t1)), l=c(1,2),
                        r=ncol(t1))

  # Creating overall plot(p1) ---------------------------------

  df = as.data.frame(as.table(gl_ip.t))
  gl_data.t = df$Freq
  gl_data.t = as.data.frame(gl_data.t)
  x = cbind(1:length(gl_ip.t))
  x = data.frame(x)

  final = cbind(x, gl_data.t)
  #Defining thresholds
  final$col <- cut(final$gl_data,
                   breaks = c(-Inf, lv2_hypo,lv1_hypo, lv1_hyper, lv2_hyper, Inf),
                   labels = c("Hypoglycemia(Level 2)","Hypoglycemia(Level 1)", "Normal", "Hyperglycemia(Level 1)","Hyperglycemia(Level 2)" ))

  # Select the color scheme
  color_dots <- c()
  if (color_scheme == "Color Scheme 1"){
    hypo_col2 <- '#8E1B1B'
    hypo_col1 <- '#F92D00'
    norm_col <- '#48BA3C'
    hyper_col1 <- '#F9F000'
    hyper_col2 <-'#F9B500'
    color_dots <- c(hypo_col2, hypo_col1, norm_col, hyper_col1 , hyper_col2)
    color_dashed <- c(hypo_col2, hypo_col1, norm_col, hyper_col1 , hyper_col2)
  }
  else if (color_scheme == "Color Scheme 2"){
    #dplyr::mutate(range = factor(range, levels = c("very_high", 'high', 'target', 'low', 'very_low')))
    hypo_col2 <- 'blue'
    hypo_col1 <- 'lightblue'
    norm_col <- 'white'
    hyper_col1 <- 'red'
    hyper_col2 <-'darkred'
    color_dots <- c(hypo_col2, hypo_col1, norm_col, hyper_col1 , hyper_col2)
    color_dashed <- c(hypo_col2, hypo_col1, norm_col, hyper_col1 , hyper_col2)
 }
  else if (color_scheme == "Color Scheme 3"){
    hypo_col2 <- 'darkred'
    hypo_col1 <- 'red'
    norm_col <- 'green'
    hyper_col1 <- 'orange'
    hyper_col2 <-'darkorange'
    color_dots <- c(hypo_col2, hypo_col1, norm_col, hyper_col1 , hyper_col2)
    color_dashed <- c(hypo_col2, hypo_col1, norm_col, hyper_col1 , hyper_col2)
  }


  time_break <- c(0, 288,576,864,1152,1440,1728,2016,2304,2592,2880,3168,3456,3744)
  # day_label <- c("Day 1", "Day 2","Day 3", "Day 4","Day 5", "Day 6","Day 7", "Day 8","Day 9", "Day 10","Day 11", "Day 12", "Day 13", "Day 14")

  #Creating plot
  p1 <- ggplot(final, aes(x = x, y = gl_data.t)) + geom_point(aes(color = col), size = 1, na.rm=TRUE) +
    scale_size_area() +
    xlab("Time") +
    ylab("Glucose level [mg/dL]") +
    # ggtitle("") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_hline(yintercept = lv1_hypo, size = 0.3,linetype = 'dashed', color = color_dashed[2]) +
    geom_hline(yintercept = lv1_hyper, size = 0.3,linetype = 'dashed', color = color_dashed[4]) +
    geom_hline(yintercept = lv2_hypo,size = 0.3, linetype = 'dashed', color = color_dashed[1]) +
    geom_hline(yintercept = lv2_hyper,size = 0.3, linetype = 'dashed', color = color_dashed[5]) +
    scale_color_manual(values=color_dots) +
    scale_x_continuous(breaks = time_break, label = day_label)+
    theme_bw() +
    theme(axis.text.x = element_text(size = 7, angle = 90), plot.title = element_text(hjust = 0.5), legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "grey90"), axis.line = element_line(colour = "black"))

  #adding all figures together ---------------------------

  pFinal = (wrap_elements(t1) + plot_layout()) / p1

  pFinal

  # }#end Function
}

