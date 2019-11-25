# set the working directory
setwd(
  "D:/Data/Dropbox/Study/10.Semester/Mensch-Computer Interaktion/mci-af/_StudyResults"
)
.libPaths("C:/Users/tobia/AppData/Local/Temp/RtmpwLoWdE/downloaded_packages")

#### Variables
##################
detailed <- FALSE
plot <- TRUE
file <- c("resync_clean.csv")

#### Basic import
##################

# read in the csv file:
df_results <- read.csv(file[[1]], sep=";", header = TRUE) 	# read in the first line as column labels with header=TRUE

#### Actual analysis
##################

## libs
library(pastecs)
library(car) # used for levene + mauchly
library(coin) # used for wilcoxon
library(reshape)
library(pgirmess) # used for kruskalmc
library(ggplot2)
library(scales)
library(tidyr)
library(matrixStats)


cat("Remaining data samples\n")

print(length(df_results[, 1]))

cat("\nSound group distribution\n")
print(table(df_results["FadeSeconds"]))

###### Correlations

print("Searching for correlations...")

for (group in c(5, 20)) {
  print(paste("Analyzing group", group))
  
  d_t <- df_results[df_results$FadeSeconds == group, ]
  for (sam in c("SAM_pleasure_pre", "SAM_arousal_pre", "SAM_dominance_pre")) {
    df_cor = data.frame("v1" = d_t["RSME"],
                        "v2" = d_t[sam])
    print(cor.test(df_cor[, "RSME"], df_cor[, sam], method = "kendall"))
  }
  rm(sam)
}
rm(d_t)
rm(df_cor)

#### Complete approach
df_t <-
  data.frame(
    "rsme" = df_results$RSME,
    "ordE" = df_results$orderingMis,
    "matE" = df_results$matchingMis,
    "couE" = df_results$countingMis,
    "fg" = df_results$FadeSeconds
  )
stat.desc(df_t)
shapiro.test(df_t[, "rsme"])
leveneTest(df_t[, "rsme"] ~ factor(fg, c(5, 20)), data = df_t)
kruskal.test(df_t[, "rsme"] ~ factor(fg, c(5, 20)), data = df_t)

print("Analyzing error rates")
for (group in c(5, 20)) {
  print(paste("Analyzing group", group))
  
  d_t <- df_t[df_t$fg == group, ]
  
  df_cor = data.frame("v1" = d_t$rsme,
                      "v2" = d_t$ordE)
  print(cor.test(df_cor$v1, df_cor$v2, method = "kendall"))
  #print(stat.desc(d_t))
}
#rm(d_t)
rm(df_cor)

##################################
##################################
##########     PLOT   ############
##################################
##################################
##################################

if (plot) {
  #### SAM
  
  df_t <- df_results[df_results$FadeSeconds != 0,]
  df_plot <- data.frame(
    "Pleasure" = df_t$SAM_pleasure_pre,
    "Arousal" = df_t$SAM_arousal_pre,
    "Dominance" = df_t$SAM_dominance_pre,
    "group" = df_t$FadeSeconds,
    "moment" = "pre"
  )
  df_plot$moment <-
    factor(df_plot$moment, c("pre", "post"))
  df_plot$group <-
    factor(df_plot$group, c(5, 20))
  df_plot <-
    gather(df_plot, sam, val, Pleasure:Dominance, factor_key = TRUE)
  bp <-
    ggplot(data = df_plot, aes(x = sam, y = val)) +
    geom_boxplot(aes(fill = group), position = position_dodge(1)) +
    #coord_cartesian(ylim = c(0, 9)) +
    labs(x = "Sam Dimension", y = "Reaction") +
    guides(fill = guide_legend(title = paste("Fade Group (", df_plot$moment, ")", sep="")))
  bp + scale_fill_manual(values = c("#941c20", "#4A5745", "#009269"))
  rm(df_t)
}
