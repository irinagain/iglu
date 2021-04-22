
epicalc_profile <- function(data, hypo_thres=90.0, hyper_thres= 120.0){

  #Importing the data
  subject = unique(data$id)
  data_ip = CGMS2DayByDay(data, dt0 = 5)
  gl_by_id_ip = data_ip[[1]][2,]
  epicalc = episode_calculation(data)

  #Checking for multiple subjects
  subject = unique(data$id)
  ns = length(subject)
  if (ns > 1){
    subject = subject[1]
    warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
    data = data %>% dplyr::filter(id == subject)
  }

  #Creating table 1(t1)
  tableStat = data.frame("Hypoglycemia/Hyperglycemia episode count")
  tableStat[1, 1] = ""
  tableStat[1, 2] = "Hypoglycemia"
  tableStat[1, 3] = "Hyperglycemia"
  tableStat[2, 1] = "Thresholds"
  tableStat[2, 2] = paste0("<", as.character(hypo_thres), " mg/dL")
  tableStat[2, 3] = paste0(">", as.character(hyper_thres), " mg/dL")
  tableStat[3, 1] = "(Mean)Episodes/Day"
  tableStat[3, 2] = as.character(epicalc$Hypo_ep)
  tableStat[3, 3] = as.character(epicalc$Hyper_ep)
  tableStat[4, 1] = "Mean duration"
  tableStat[4, 2] = paste0(as.character(epicalc$hypo_duration), " min")
  tableStat[4, 3] = paste0(as.character(epicalc$hyper_duration), " min")
  tableStat[5, 1] = "Average Glucose level"
  tableStat[5, 2] = paste0(as.character(round(epicalc$Hypo_mean),1), "mg/dL")
  tableStat[5, 3] = paste0(as.character(round(epicalc$Hyper_mean),1), "mg/dL")

  mytheme <- gridExtra::ttheme_minimal()
  t1 <- tableGrob(tableStat, rows = NULL, cols = NULL, theme = mytheme )
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



  # Creating daily plot(p1)

  df = data.frame(gl_by_id_ip)
  x = cbind(1:length(gl_by_id_ip))
  #Defining thresholds
  df$col <- cut(df$gl_by_id_ip,
                #Correct the < and <=
                breaks = c(-Inf, hypo_thres-0.0001, hyper_thres+0.0001, Inf),
                labels = c("Hypoglycemia", "Normal", "Hyperglycemia"))
  #Creating plot
  p1 <- ggplot(df, aes(x, y=gl_by_id_ip)) + geom_point(aes(color = col)) +
    scale_size_area() +
    xlab("Time") +
    ylab("Glucose level") +
    ggtitle("Glucose level in a day") + theme(plot.title = element_text(hjust = 0.5)) +
    geom_hline(yintercept = hypo_thres, linetype = 'dashed', color = 'red') +
    geom_hline(yintercept = hyper_thres, linetype = 'dashed', color = 'red') +
    scale_colour_discrete("") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))


  #Creating percentage table (t2)
  tableStat2 = data.frame("")
  tableStat2[1, 1] = "Low Alert"
  tableStat2[1, 2] = "In Target Range"
  tableStat2[1, 3] = "High Alert"
  tableStat2[2, 1] = paste0("<", as.character(hypo_thres), " mg/dL")
  tableStat2[2, 2] = paste0(as.character(hypo_thres), " - ", as.character(hypo_thres), " mg/dL")
  tableStat2[2, 3] = paste0(">", as.character(hyper_thres), " mg/dL")
  tableStat2[3, 1] = paste0(as.character(format(round(epicalc$low_alert, 2), nsmall = 2)), "%")
  tableStat2[3, 2] = paste0(as.character(format(round(epicalc$target_range, 2), nsmall = 2)), "%")
  tableStat2[3, 3] = paste0(as.character(format(round(epicalc$high_alert, 2), nsmall = 2)), "%")

  mytheme <- gridExtra::ttheme_minimal(
  #Bolding the percentage
    core=list(
      fg_params=list(fontface=c(rep("plain", 2), "bold.italic"))
   )
  )

  t2 <- tableGrob(tableStat2, rows = NULL, cols = NULL, theme = mytheme )
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

  padding <- unit(0.5,"line")
  t2 <- gtable_add_rows(t2,
                        heights = grobHeight(title) + padding,
                        pos = 0)
  t2 <- gtable_add_rows(t2,
                        heights = grobHeight(footnote)+ padding)
  t2 <- gtable_add_grob(t2, list(title, footnote),
                        t=c(1, nrow(t2)), l=c(1,2),
                        r=ncol(t2))


  #adding all figures together

  pFinal = (

    wrap_elements(t1) + wrap_elements(t2) + plot_layout()) / p1




  pFinal


  # }#end Function
}
