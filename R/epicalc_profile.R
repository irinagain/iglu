#' Display Episode Calculation statistics for selected subject
#' @name epicalc_profile
#'
#' @usage
#' epicalc_profile(data, maxd = 14, inter_gap = 45, dt0 = NULL, tz = "", daily = TRUE)
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
#
library(ggplot2)
library(grid)
library(patchwork)
library(gridExtra)
library(gtable)


epicalc_profile <- function(data, hypo_thres=100, hyper_thres= 120.0, color_scheme = c("blue-red", "red-orange")){

  #Importing the data
  subject = unique(data$id)
  data_ip = CGMS2DayByDay(data, dt0 = 5)

  gl_ip = data_ip[[1]][1:14,]
  test = data_ip$actual_dates
  day_label = as.character(test)


  epicalc = episode_calculation(data, hypo_thres, hyper_thres)
  gl_ip.t = t(gl_ip)
  #Checking for multiple subjects
  subject = unique(data$id)
  ns = length(subject)
  if (ns > 1){
    subject = subject[1]
    warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
    data = data %>% dplyr::filter(id == subject)
  }
  print("part 1")
  #Creating table 1(t1) -------------------------------------
  tableStat = data.frame("Hypoglycemia/Hyperglycemia episode metrics")
  tableStat[1, 1] = ""
  tableStat[1, 2] = "Hypoglycemia"
  tableStat[1, 3] = "Hyperglycemia"
  tableStat[2, 1] = "Thresholds"
  tableStat[2, 2] = paste0("<", as.character(hypo_thres), " mg/dL")
  tableStat[2, 3] = paste0(">", as.character(hyper_thres), " mg/dL")
  tableStat[3, 1] = "(Mean)Episodes/Day"
  tableStat[3, 2] = as.character(format(round(epicalc$Hypo_ep, 2), nsmall = 2))
  tableStat[3, 3] = as.character(format(round(epicalc$Hyper_ep, 2), nsmall = 2))
  tableStat[4, 1] = "Mean duration"
  tableStat[4, 2] = paste0(as.character(format(round(epicalc$hypo_duration, 2), nsmall = 2)), " min")
  tableStat[4, 3] = paste0(as.character(format(round(epicalc$hyper_duration, 2), nsmall = 2)), " min")
  tableStat[5, 1] = "Average episode glucose level"
  tableStat[5, 2] = paste0(as.character(format(round(epicalc$Hypo_mean, 2), nsmall = 2)), " mg/dL")
  tableStat[5, 3] = paste0(as.character(format(round(epicalc$Hyper_mean, 2), nsmall = 2)), " mg/dL")

  mytheme <- gridExtra::ttheme_minimal(base_size = 10, padding = unit(c(4,2),"mm"))
  t1 <- gridExtra::tableGrob(tableStat, rows = NULL, cols = NULL, theme = mytheme )

  #Adding border(t1)
  t1 <- gtable_add_grob(t1,
                        grobs = rectGrob(gp = gpar(fill = NA, lwd = 3)),
                        t = 1, b = 5, l = 1, r = 3)
  #Adding dotted separator(t1)
  separators <- replicate(ncol(t1) - 2,
                          segmentsGrob(x1 = unit(0, "npc"), gp=gpar(lty=2)),
                          simplify=FALSE)
  t1 <- gtable_add_grob(t1, grobs = separators,
                        t = 2, b = nrow(t1), l = seq_len(ncol(t1)-2)+2)

  print("part 2")
  #Adding title and footnote(t1)
  title <- textGrob("",gp=gpar(fontsize=16))
  footnote <- textGrob("An episode is >= 15 continuous minutes", x=0, hjust=0,
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
                breaks = c(-Inf, hypo_thres-0.0001, hyper_thres+0.0001, Inf),
                labels = c("Hypoglycemia", "Normal", "Hyperglycemia"))

  #plot colors, default is blue-red
    hypo_col <- 'blue'
    norm_col <- 'white'
    hyper_col <- 'red'


  #if(color_scheme == "red-orange"){
  #  hypo_col <- 'red'
  #  norm_col <- 'green'
  #  hyper_col <- 'orange'
  #}

  time_break <- c(0, 288,576,864,1152,1440,1728,2016,2304,2592,2880,3168,3456,3744)
  # day_label <- c("Day 1", "Day 2","Day 3", "Day 4","Day 5", "Day 6","Day 7", "Day 8","Day 9", "Day 10","Day 11", "Day 12", "Day 13", "Day 14")

  #Creating plot
  p1 <- ggplot(final, aes(x = x, y = gl_data.t)) + geom_point(aes(color = col), size = 1, na.rm=TRUE) +
  scale_size_area() +
  xlab("Time") +
  ylab("Glucose level [mg/dL]") +
  ggtitle("Overall Glucose levels [mg/dL]") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = hypo_thres, linetype = 'dashed', color = 'red') +
  geom_hline(yintercept = hyper_thres, linetype = 'dashed', color = 'red') +
  scale_color_manual(values=c(hypo_col, norm_col, hyper_col)) +
  scale_x_continuous(breaks = time_break, label = day_label)+
  theme_bw() +
  theme(axis.text.x = element_text(size = 7, angle = 90), plot.title = element_text(hjust = 0.5), legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey90"), axis.line = element_line(colour = "black"))


  #Creating percentage table (t2) ---------------------------
  tableStat2 = data.frame("")

  tableStat2[1, 1] = "Low Alert"
  tableStat2[1, 2] = "In Target Range"
  tableStat2[1, 3] = "High Alert"
  tableStat2[2, 1] = paste0("<", as.character(hypo_thres), " mg/dL")
  tableStat2[2, 2] = paste0(as.character(hypo_thres), " - ", as.character(hypo_thres), " mg/dL")
  tableStat2[2, 3] = paste0(">", as.character(hyper_thres), " mg/dL")
  tableStat2[3, 1] = paste0(as.character(format(round(epicalc$low_alert), nsmall = 2)), "%")
  tableStat2[3, 2] = paste0(as.character(format(round(epicalc$target_range, 2), nsmall = 2)), "%")
  tableStat2[3, 3] = paste0(as.character(format(round(epicalc$high_alert, 2), nsmall = 2)), "%")

  mytheme <- gridExtra::ttheme_minimal(
    #Bolding the percentage
    core=list(
      fg_params=list(fontface=c(rep("plain", 2), "bold.italic"))
    )
  )

  t2 <- gridExtra::tableGrob(tableStat2, rows = NULL, cols = NULL, theme = mytheme )
  #Adding separators
  separators <- replicate(ncol(t2) - 2,
                          segmentsGrob(x1 = unit(0, "npc"), gp=gpar(lty=2)),
                          simplify=FALSE)
  t2 <- gtable_add_grob(t2, grobs = separators,
                        t = 1, b = nrow(t2), l = seq_len(ncol(t2)-2)+1)
  t2 <- gtable_add_grob(t2, grobs = separators,
                        t = 1, b = nrow(t2), l = seq_len(ncol(t2)-2)+2)
  #Adding borders
  t2 <- gtable_add_grob(t2,
                        grobs = rectGrob(gp = gpar(fill = NA, lwd = 3)),
                        t = 1, b = 3, l = 1, r = 3)
  #Adding title and footnote
  title <- textGrob("",gp=gpar(fontsize=20))
  footnote <- textGrob("Percentage of daily time in range", x=0, hjust=0,
                       gp=gpar( fontface="italic", fontsize = 8))
  print("part 3")
  padding <- unit(0.5,"line")
  t2 <- gtable_add_rows(t2,
                        heights = grobHeight(title) + padding,
                        pos = 0)
  t2 <- gtable_add_rows(t2,
                        heights = grobHeight(footnote)+ padding)
  t2 <- gtable_add_grob(t2, list(title, footnote),
                        t=c(1, nrow(t2)), l=c(1,2),
                        r=ncol(t2))

  #Daily plots(p3) ----------------------------------------

  p3 = plot_daily(data, maxd = 14, LLTR = hypo_thres, ULTR = hyper_thres)

  #adding all figures together ---------------------------

  pFinal = (

   wrap_elements(t1) +  wrap_elements(t2) + plot_layout()) / p1 / p3

  pFinal


  # }#end Function
}
