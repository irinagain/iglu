# Script to reproduce plot
# Author: Nathaniel J. Fernandes, Lizzie Chun
# Date: Sep. 17, 2023

# All data can be found in the "data/" folder of this repository. See the Readme for more info on origin & preprocessing.

# These datasets come from a publicly available CGM dataset. Both the link to the original
# download site and preprocessor scripts can be found through our Awesome-CGM GitHub
# repository: https://github.com/irinagain/Awesome-CGM. These datasets are already processed
# via the preprocessor script.

# USER AGREEMENT: You consent to the following user agreement by using this data.
#     "Use of the T1D Exchange publicly-available data requires that you include the following attribution and disclaimer in any publication, presentation or communication resulting from use of these data:
#     The source of the data is the T1D Exchange, but the analyses, content and conclusions presented herein are solely the responsibility of the authors and have not been reviewed or approved by the T1D Exchange.
#     In addition, the T1D Exchange should be notified via email (publicdatasetuse@t1dexchange.org) when a manuscript (send title) or abstract (send title and name of meeting) reports T1D Exchange data or analyses of the data.
#     Please provide notification at the time of submission and again at time of acceptance."

if (!require("readxl")) install.packages("readxl")
if (!require("iglu")) install.packages("iglu")
if (!require("dplyr")) install.packages("dplyr")

library("readxl")
library("iglu")
library("dplyr")

manual_calc <- read_excel("data/manual_calculations.xlsx")
dataset_filenames = unique(manual_calc$dataset_filename)

for (filename in dataset_filenames) {
  dataset = read.csv(paste('data/', filename, sep=""), header=TRUE)
  rows = dplyr::filter(manual_calc, dataset_filename == filename)
  
  figure.path = "data/raw_cgm_plots/"
  pdf(file = paste0(figure.path, rows[1, ]$dataset, ".pdf"), width=10, height=5, onefile=TRUE)
  
  for (i in 1:nrow(rows)) {
    data = dataset[rows[i,]$start:rows[i,]$end, ]
    date = as.Date(data$time[1])
    manual = rows[i, ]$manual
    subject = rows[i, ]$subject
    
    p = plot_glu(data) + ggtitle(paste0("Manual MAGE: ", manual, " (", subject, ", ", date, ")"))
    print(p) # adds the ggplot `p` to pdf as a new page
  }
  
  dev.off()
}