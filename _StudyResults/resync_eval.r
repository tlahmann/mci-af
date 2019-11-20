# set the working directory
setwd(
  "D:\\Data\\study\\12.Semester\\mci-af\\_StudyResults"
)
.libPaths("C:/Users/tobia/Documents/R/win-library/3.6")

#### Variables
##################
plot <- FALSE
file <- c("resync_complete.csv", "resync2_clean.csv")

#### Basic import
##################

# read in the csv file:
df_results <-
  read.csv(file[[1]], header = TRUE, sep = ";", stringsAsFactors = FALSE) 	# read in the first line as column labels with header=TRUE

library(plyr)

# print to console the results of ls() call:
print("Data objects:")
print(ls()) 							# ls(): which data objects currently exist in this workspace
print("Columns of the result set:")
print(names(df_results)) 					# names(): which column names exist in this dataframe?

# number of participants (all rows, column no.1)
print("number of data samples:")
print(length(df_results[, 1]))

# number of columns/variables
print("number of variables:")
print(length(df_results))

#### Actual analysis
##################

## libs
library(pastecs)
# library(reshape)
library(ggplot2)
library(scales)
library(tidyr)
# library(matrixStats)

# print(table(df_results["gender"]))
options(scipen=100)
options(digits=2)
print(stat.desc(df_results["RSME"]))
print(stat.desc(df_results["orderingMis"]))
print(stat.desc(df_results["matchingMis"]))
print(stat.desc(df_results["countingMis"]))

print(stat.desc(df_results["orderingTime"]))
print(stat.desc(df_results["matchingTime"]))
print(stat.desc(df_results["countingTime"]))

print(stat.desc(df_results[df_results["AlarmDuration"] != -1, "AlarmDuration"]))

for (i in c(5, 20, -1)) {
  for (j in c(
    "o1", "o2", "o3", "o4", "o5", "o6", "o7", "o8", "o9", "o10",
    "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10",
    "c1", "c2", "c3"
    )) {
    tmp <- stat.desc(df_results[df_results["FadeSeconds"] == i, j])
    sts <- c(tmp["min"], tmp["max"], tmp["range"], tmp["median"], tmp["mean"], tmp["std.dev"])
    # print(i + j)
    print(sts)
  }
}
rm(i)
rm(j)
rm(tmp)
rm(sts)

##################################
##################################
##########     PLOT   ############
##################################
##################################
##################################

if (plot) {
  blank_theme <- theme_minimal()+
    theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    # panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=40, face="bold")
    )

  df2 <- data.frame(group=rep(c("5", "20", "-1"), each=10),
                  task=factor(c("o1", "o2", "o3", "o4", "o5","o6", "o7", "o8", "o9", "o10")),
                  val=c(
                    median(df_results[df_results$FadeSeconds == 5, "o1"]), 
                    median(df_results[df_results$FadeSeconds == 5, "o2"]),
                    median(df_results[df_results$FadeSeconds == 5, "o3"]), 
                    median(df_results[df_results$FadeSeconds == 5, "o4"]),
                    median(df_results[df_results$FadeSeconds == 5, "o5"]), 
                    median(df_results[df_results$FadeSeconds == 5, "o6"]),
                    median(df_results[df_results$FadeSeconds == 5, "o7"]), 
                    median(df_results[df_results$FadeSeconds == 5, "o8"]),
                    median(df_results[df_results$FadeSeconds == 5, "o9"]), 
                    median(df_results[df_results$FadeSeconds == 5, "o10"]),
                    median(df_results[df_results$FadeSeconds == 20, "o1"]), 
                    median(df_results[df_results$FadeSeconds == 20, "o2"]),
                    median(df_results[df_results$FadeSeconds == 20, "o3"]), 
                    median(df_results[df_results$FadeSeconds == 20, "o4"]),
                    median(df_results[df_results$FadeSeconds == 20, "o5"]), 
                    median(df_results[df_results$FadeSeconds == 20, "o6"]),
                    median(df_results[df_results$FadeSeconds == 20, "o7"]), 
                    median(df_results[df_results$FadeSeconds == 20, "o8"]),
                    median(df_results[df_results$FadeSeconds == 20, "o9"]), 
                    median(df_results[df_results$FadeSeconds == 20, "o10"]),
                    median(df_results[df_results$FadeSeconds == -1, "o1"]), 
                    median(df_results[df_results$FadeSeconds == -1, "o2"]),
                    median(df_results[df_results$FadeSeconds == -1, "o3"]), 
                    median(df_results[df_results$FadeSeconds == -1, "o4"]),
                    median(df_results[df_results$FadeSeconds == -1, "o5"]), 
                    median(df_results[df_results$FadeSeconds == -1, "o6"]),
                    median(df_results[df_results$FadeSeconds == -1, "o7"]), 
                    median(df_results[df_results$FadeSeconds == -1, "o8"]),
                    median(df_results[df_results$FadeSeconds == -1, "o9"]), 
                    median(df_results[df_results$FadeSeconds == -1, "o10"])
                    )
                  )
  df2$task <- factor(df2$task,levels = c("o1", "o2", "o3", "o4", "o5","o6", "o7", "o8", "o9", "o10"))
  bp <- ggplot(df2, aes(x=task, y=val, group=group)) +
    geom_line(aes(color=group))+
    geom_point(aes(color=group)) +
    blank_theme +
    #coord_cartesian(ylim = c(0, 9)) +
    labs(x = "Sam Dimension", y = "Reaction") +
    guides(fill = guide_legend(title = "Fade duration")) +
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#ffffbf"))
  ggsave("timeTask1.pdf", bp, NULL, NULL, 1, 7, 5)


  #### SAM
  df_plot <- data.frame(
    "Pleasure" = df_results$SAM_pleasure_pre,
    "Arousal" = df_results$SAM_arousal_pre,
    "Dominance" = df_results$SAM_dominance_pre,
    "group" = df_results$FadeSeconds
  )
  df_plot$group <-
    factor(df_plot$group, c("20", "5", "-1"))
  df_plot <-
    gather(df_plot, sam, val, Pleasure:Dominance, factor_key = TRUE)
  bp <-
    ggplot(data = df_plot, aes(x = sam, y = val)) +
    geom_boxplot(aes(fill = group), position = position_dodge(1)) +
    blank_theme +
    #coord_cartesian(ylim = c(0, 9)) +
    labs(x = "Sam Dimension", y = "Reaction") +
    guides(fill = guide_legend(title = "Fade duration")) +
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#ffffbf"))
  ggsave("SAMpre.pdf", bp, NULL, NULL, 1, 7, 5)

  df_plot <- data.frame(
    "Pleasure" = df_results$SAM_pleasure_post,
    "Arousal" = df_results$SAM_arousal_post,
    "Dominance" = df_results$SAM_dominance_post,
    "group" = df_results$FadeSeconds
  )
  df_plot$group <-
    factor(df_plot$group, c("20", "5", "-1"))
  df_plot <-
    gather(df_plot, sam, val, Pleasure:Dominance, factor_key = TRUE)
  bp <-
    ggplot(data = df_plot, aes(x = sam, y = val)) +
    geom_boxplot(aes(fill = group), position = position_dodge(1)) +
    blank_theme +
    #coord_cartesian(ylim = c(0, 9)) +
    labs(x = "Sam Dimension", y = "Reaction") +
    guides(fill = guide_legend(title = "Fade duration")) +
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#ffffbf"))
  ggsave("SAMpost.pdf", bp, NULL, NULL, 1, 7, 5)

  ## Ordering mistakes histogram
  df_plot <- data.frame(
    group = df_results["FadeSeconds"],
    value = df_results["orderingMis"]
  )
  # Basic histogram
  p <- ggplot(df_plot, aes(x=orderingMis, fill=factor(FadeSeconds), color=factor(FadeSeconds)))  + 
    geom_histogram(binwidth=1, position="dodge") +
    theme_minimal() +
    # geom_density(alpha=.2, fill="#FF6666") +
    labs(x = "Mistakes", y = "Count")
  ggsave("orderingMisHist.pdf", p, NULL, NULL, 1, 7, 5)

  ## Matching mistakes histogram
  df_plot <- data.frame(
    group = df_results["FadeSeconds"],
    value = df_results["matchingMis"]
  )
  # Basic histogram
  p <- ggplot(df_plot, aes(x=matchingMis, fill=factor(FadeSeconds), color=factor(FadeSeconds)))  + 
    geom_histogram(binwidth=1, position="dodge") +
    theme_minimal() +
    # geom_density(alpha=.2, fill="#FF6666") +
    labs(x = "Mistakes", y = "Count")
  ggsave("matchingMisHist.pdf", p, NULL, NULL, 1, 7, 5)

  ## Counting mistakes histogram
  df_plot <- data.frame(
    group = df_results["FadeSeconds"],
    value = df_results["countingMis"]
  )
  # Basic histogram
  p <- ggplot(df_plot, aes(x=countingMis, fill=factor(FadeSeconds), color=factor(FadeSeconds)))  + 
    geom_histogram(binwidth=1, position="dodge") +
    theme_minimal() +
    # geom_density(alpha=.2, fill="#FF6666") +
    labs(x = "Mistakes", y = "Count")
  ggsave("countingMisHist.pdf", p, NULL, NULL, 1, 7, 5)

  ## Ordering time histogram
  df_plot <- data.frame(
    group = df_results["FadeSeconds"],
    value = df_results["orderingTime"]
  )
  # Basic histogram
  p <- ggplot(df_plot, aes(x=orderingTime, fill=factor(FadeSeconds), color=factor(FadeSeconds)))  + 
    geom_histogram(binwidth=2000, position="dodge") +
    theme_minimal() +
    # geom_density(alpha=.2, fill="#FF6666") +
    labs(x = "Time (ms)", y = "Count")
  ggsave("orderingTimeHist.pdf", p, NULL, NULL, 1, 7, 5)

  ## Matching time histogram
  df_plot <- data.frame(
    group = df_results["FadeSeconds"],
    value = df_results["matchingTime"]
  )
  # Basic histogram
  p <- ggplot(df_plot, aes(x=matchingTime, fill=factor(FadeSeconds), color=factor(FadeSeconds)))  + 
    geom_histogram(binwidth=2000, position="dodge") +
    theme_minimal() +
    # geom_density(alpha=.2, fill="#FF6666") +
    labs(x = "Time (ms)", y = "Count")
  ggsave("matchingTimeHist.pdf", p, NULL, NULL, 1, 7, 5)

  ## Counting time histogram
  df_plot <- data.frame(
    group = df_results["FadeSeconds"],
    value = df_results["countingTime"]
  )
  # Basic histogram
  p <- ggplot(df_plot, aes(x=countingTime, fill=factor(FadeSeconds), color=factor(FadeSeconds)))  + 
    geom_histogram(binwidth=2000, position="dodge") +
    theme_minimal() +
    # geom_density(alpha=.2, fill="#FF6666") +
    labs(x = "Time (ms)", y = "Count")
  ggsave("countingTimeHist.pdf", p, NULL, NULL, 1, 7, 5)


  ####### Alarm duration
  ## Counting time histogram
  dt <- df_results[df_results["AlarmDuration"] > 0,]
  df_plot <- data.frame(
    group = dt["Participant_ID"],
    value = dt["AlarmDuration"]
  )
  # Basic histogram
  p <- ggplot(df_plot, aes(x=AlarmDuration)) + 
    geom_histogram(binwidth=0.5, colour="black", fill="#ffffbf") +
    blank_theme +
    labs(x = "Count", y = "Time (ms)") +
    guides(fill = guide_legend(title = "Alarm Duration"))
  ggsave("alarmDurationHist.pdf", p, NULL, NULL, 1, 7, 5)
  rm(dt)

  rm(df_plot)
  rm(bp)
}
