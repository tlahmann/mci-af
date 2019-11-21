# set the working directory
setwd(
  "D:\\Data\\study\\12.Semester\\mci-af\\_StudyResults"
)
.libPaths("C:/Users/tobia/Documents/R/win-library/3.6")

#### Variables
##################
plot <- FALSE
details <- FALSE
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


for (j in c("orderingMis", "matchingMis", "countingMis")) {
  print(j)
  for (i in c(5, 20, -1)) {
    print(i)
    # print(stat.desc(df_results[df_results["FadeSeconds"] == i, j]))
    print(table(df_results[df_results["FadeSeconds"] == i, j]))

  }
}
print(stat.desc(df_results[df_results["FadeSeconds"] == i, "orderingTime"]))
print(stat.desc(df_results[df_results["FadeSeconds"] == i, "matchingTime"]))
print(stat.desc(df_results[df_results["FadeSeconds"] == i, "countingTime"]))

print(stat.desc(df_results[df_results["AlarmDuration"] != -1, "AlarmDuration"]))

if(details){
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
}

##################################
##################################
##########     PLOT   ############
##################################
##################################
##################################
blank_theme <- theme_minimal()+
    theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    # panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=40, face="bold")
    )

if (plot) {
  # Change all occurances of numbers as groups to strings
  df_results["FadeSeconds"] <- data.frame(lapply(df_results["FadeSeconds"], function(x) {
      gsub("-1", "Alarm", x)
  }))
  df_results["FadeSeconds"] <- data.frame(lapply(df_results["FadeSeconds"], function(x) {
      gsub("20", "Fade 20", x)
  }))
  df_results["FadeSeconds"] <- data.frame(lapply(df_results["FadeSeconds"], function(x) {
      gsub("5", "Fade 5", x)
  }))

  df2 <- data.frame(group=rep(c("Fade 5", "Fade 20", "Alarm"), each=10),
    task=factor(c("o1", "o2", "o3", "o4", "o5","o6", "o7", "o8", "o9", "o10")),
    val=c(
      median(df_results[df_results$FadeSeconds == "Fade 5", "o1"]), 
      median(df_results[df_results$FadeSeconds == "Fade 5", "o2"]),
      median(df_results[df_results$FadeSeconds == "Fade 5", "o3"]), 
      median(df_results[df_results$FadeSeconds == "Fade 5", "o4"]),
      median(df_results[df_results$FadeSeconds == "Fade 5", "o5"]), 
      median(df_results[df_results$FadeSeconds == "Fade 5", "o6"]),
      median(df_results[df_results$FadeSeconds == "Fade 5", "o7"]), 
      median(df_results[df_results$FadeSeconds == "Fade 5", "o8"]),
      median(df_results[df_results$FadeSeconds == "Fade 5", "o9"]), 
      median(df_results[df_results$FadeSeconds == "Fade 5", "o10"]),
      median(df_results[df_results$FadeSeconds == "Fade 20", "o1"]), 
      median(df_results[df_results$FadeSeconds == "Fade 20", "o2"]),
      median(df_results[df_results$FadeSeconds == "Fade 20", "o3"]), 
      median(df_results[df_results$FadeSeconds == "Fade 20", "o4"]),
      median(df_results[df_results$FadeSeconds == "Fade 20", "o5"]), 
      median(df_results[df_results$FadeSeconds == "Fade 20", "o6"]),
      median(df_results[df_results$FadeSeconds == "Fade 20", "o7"]), 
      median(df_results[df_results$FadeSeconds == "Fade 20", "o8"]),
      median(df_results[df_results$FadeSeconds == "Fade 20", "o9"]), 
      median(df_results[df_results$FadeSeconds == "Fade 20", "o10"]),
      median(df_results[df_results$FadeSeconds == "Alarm", "o1"]), 
      median(df_results[df_results$FadeSeconds == "Alarm", "o2"]),
      median(df_results[df_results$FadeSeconds == "Alarm", "o3"]), 
      median(df_results[df_results$FadeSeconds == "Alarm", "o4"]),
      median(df_results[df_results$FadeSeconds == "Alarm", "o5"]), 
      median(df_results[df_results$FadeSeconds == "Alarm", "o6"]),
      median(df_results[df_results$FadeSeconds == "Alarm", "o7"]), 
      median(df_results[df_results$FadeSeconds == "Alarm", "o8"]),
      median(df_results[df_results$FadeSeconds == "Alarm", "o9"]), 
      median(df_results[df_results$FadeSeconds == "Alarm", "o10"])
      )
    )
  df2$task <- factor(df2$task,levels = c("o1", "o2", "o3", "o4", "o5","o6", "o7", "o8", "o9", "o10"))
  bp <- ggplot(df2, aes(x=task, y=val, group=group)) +
    geom_line(aes(color=group))+
    geom_point(aes(color=group)) +
    theme_minimal() +
    labs(y = "Time (ms)", x = "Subtask", title="Ordering subtask times", fill = "Group", color="Group") +
    guides(fill = guide_legend(title = "Group")) +
    scale_color_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))+
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))
  ggsave("timeTask1.pdf", bp, NULL, NULL, 1, 7, 5)

  df2 <- data.frame(group=rep(c("Fade 5", "Fade 20", "Alarm"), each=10),
    task=factor(c("m1", "m2", "m3", "m4", "m5","m6", "m7", "m8", "m9", "m10")),
    val=c(
      median(df_results[df_results$FadeSeconds == "Fade 5", "m1"]), 
      median(df_results[df_results$FadeSeconds == "Fade 5", "m2"]),
      median(df_results[df_results$FadeSeconds == "Fade 5", "m3"]), 
      median(df_results[df_results$FadeSeconds == "Fade 5", "m4"]),
      median(df_results[df_results$FadeSeconds == "Fade 5", "m5"]), 
      median(df_results[df_results$FadeSeconds == "Fade 5", "m6"]),
      median(df_results[df_results$FadeSeconds == "Fade 5", "m7"]), 
      median(df_results[df_results$FadeSeconds == "Fade 5", "m8"]),
      median(df_results[df_results$FadeSeconds == "Fade 5", "m9"]), 
      median(df_results[df_results$FadeSeconds == "Fade 5", "m10"]),
      median(df_results[df_results$FadeSeconds == "Fade 20", "m1"]), 
      median(df_results[df_results$FadeSeconds == "Fade 20", "m2"]),
      median(df_results[df_results$FadeSeconds == "Fade 20", "m3"]), 
      median(df_results[df_results$FadeSeconds == "Fade 20", "m4"]),
      median(df_results[df_results$FadeSeconds == "Fade 20", "m5"]), 
      median(df_results[df_results$FadeSeconds == "Fade 20", "m6"]),
      median(df_results[df_results$FadeSeconds == "Fade 20", "m7"]), 
      median(df_results[df_results$FadeSeconds == "Fade 20", "m8"]),
      median(df_results[df_results$FadeSeconds == "Fade 20", "m9"]), 
      median(df_results[df_results$FadeSeconds == "Fade 20", "m10"]),
      median(df_results[df_results$FadeSeconds == "Alarm", "m1"]), 
      median(df_results[df_results$FadeSeconds == "Alarm", "m2"]),
      median(df_results[df_results$FadeSeconds == "Alarm", "m3"]), 
      median(df_results[df_results$FadeSeconds == "Alarm", "m4"]),
      median(df_results[df_results$FadeSeconds == "Alarm", "m5"]), 
      median(df_results[df_results$FadeSeconds == "Alarm", "m6"]),
      median(df_results[df_results$FadeSeconds == "Alarm", "m7"]), 
      median(df_results[df_results$FadeSeconds == "Alarm", "m8"]),
      median(df_results[df_results$FadeSeconds == "Alarm", "m9"]), 
      median(df_results[df_results$FadeSeconds == "Alarm", "m10"])
      )
    )
  df2$task <- factor(df2$task,levels = c("m1", "m2", "m3", "m4", "m5","m6", "m7", "m8", "m9", "m10"))
  bp <- ggplot(df2, aes(x=task, y=val, group=group)) +
    geom_line(aes(color=group))+
    geom_point(aes(color=group)) +
    theme_minimal() +
    labs(y = "Time (ms)", x = "Subtask", title="Matching subtask times", fill = "Group", color="Group") +
    guides(fill = guide_legend(title = "Group")) +
    scale_color_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))+
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))
  ggsave("timeTask2.pdf", bp, NULL, NULL, 1, 7, 5)

  df2 <- data.frame(group=rep(c("Fade 5", "Fade 20", "Alarm"), each=3),
    task=factor(c("c1", "c2", "c3")),
    val=c(
      median(df_results[df_results$FadeSeconds == "Fade 5", "c1"]), 
      median(df_results[df_results$FadeSeconds == "Fade 5", "c2"]),
      median(df_results[df_results$FadeSeconds == "Fade 5", "c3"]),
      median(df_results[df_results$FadeSeconds == "Fade 20", "c1"]), 
      median(df_results[df_results$FadeSeconds == "Fade 20", "c2"]),
      median(df_results[df_results$FadeSeconds == "Fade 20", "c3"]),
      median(df_results[df_results$FadeSeconds == "Alarm", "c1"]), 
      median(df_results[df_results$FadeSeconds == "Alarm", "c2"]),
      median(df_results[df_results$FadeSeconds == "Alarm", "c3"])
      )
    )
  df2$task <- factor(df2$task,levels = c("c1", "c2", "c3"))
  bp <- ggplot(df2, aes(x=task, y=val, group=group)) +
    geom_line(aes(color=group))+
    geom_point(aes(color=group)) +
    theme_minimal() +
    labs(y = "Time (ms)", x = "Subtask", title="Counting subtask times", fill = "Group", color="Group") +
    guides(fill = guide_legend(title = "Group")) +
    scale_color_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))+
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))
  ggsave("timeTask3.pdf", bp, NULL, NULL, 1, 7, 5)

  #### SAM
  df_plot <- data.frame(
    "Pleasure" = df_results$SAM_pleasure_pre,
    "Arousal" = df_results$SAM_arousal_pre,
    "Dominance" = df_results$SAM_dominance_pre,
    "group" = df_results$FadeSeconds
  )
  df_plot$group <-
    factor(df_plot$group, c("Fade 5", "Fade 20", "Alarm"))
  df_plot <-
    gather(df_plot, sam, val, Pleasure:Dominance, factor_key = TRUE)
  bp <-
    ggplot(data = df_plot, aes(x = sam, y = val)) +
    geom_boxplot(aes(fill = group), position = position_dodge(1)) +
    blank_theme +
    #coord_cartesian(ylim = c(0, 9)) +
    labs(x = "Sam Dimension", y = "Reaction") +
    guides(fill = guide_legend(title = "Group")) +
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))
  ggsave("SAMpre.pdf", bp, NULL, NULL, 1, 7, 5)

  df_plot <- data.frame(
    "Pleasure" = df_results$SAM_pleasure_post,
    "Arousal" = df_results$SAM_arousal_post,
    "Dominance" = df_results$SAM_dominance_post,
    "group" = df_results$FadeSeconds
  )
  df_plot$group <-
    factor(df_plot$group, c("Fade 5", "Fade 20", "Alarm"))
  df_plot <-
    gather(df_plot, sam, val, Pleasure:Dominance, factor_key = TRUE)
  bp <-
    ggplot(data = df_plot, aes(x = sam, y = val)) +
    geom_boxplot(aes(fill = group), position = position_dodge(1)) +
    blank_theme +
    #coord_cartesian(ylim = c(0, 9)) +
    labs(x = "Sam Dimension", y = "Reaction") +
    guides(fill = guide_legend(title = "Group")) +
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))
  ggsave("SAMpost.pdf", bp, NULL, NULL, 1, 7, 5)

  ## Ordering mistakes histogram
  df_plot <- data.frame(
    group = df_results["FadeSeconds"],
    value = df_results["orderingMis"]
  )
  # Basic histogram
  p <- ggplot(df_plot, aes(x=orderingMis, fill=FadeSeconds, color=FadeSeconds))  + 
    geom_histogram(color="grey", binwidth=1, position="dodge") +
    theme_minimal() +
    labs(x = "Mistakes", y = "Count", title="Ordering mistakes", fill = "Group", color="Group") +
    scale_color_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))+
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))
  ggsave("orderingMisHist.pdf", p, NULL, NULL, 1, 7, 5)

  ## Matching mistakes histogram
  df_plot <- data.frame(
    group = df_results["FadeSeconds"],
    value = df_results["matchingMis"]
  )
  # Basic histogram
  p <- ggplot(df_plot, aes(x=matchingMis, fill=FadeSeconds, color=FadeSeconds))  + 
    geom_histogram(color="grey", binwidth=1, position="dodge") +
    theme_minimal() +
    labs(x = "Mistakes", y = "Count", title="Matching mistakes", fill = "Group", color="Group") +
    scale_color_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))+
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))
  ggsave("matchingMisHist.pdf", p, NULL, NULL, 1, 7, 5)

  ## Counting mistakes histogram
  df_plot <- data.frame(
    group = df_results["FadeSeconds"],
    value = df_results["countingMis"]
  )
  # Basic histogram
  p <- ggplot(df_plot, aes(x=countingMis, fill=FadeSeconds, color=FadeSeconds)) + 
    geom_histogram(color="grey", binwidth=1, position="dodge") +
    theme_minimal() +
    labs(x = "Mistakes", y = "Count", title="Counting mistakes", fill = "Group", color="Group") +
    scale_color_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))+
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))
  ggsave("countingMisHist.pdf", p, NULL, NULL, 1, 7, 5)

  ## Ordering time histogram
  df_plot <- data.frame(
    group = df_results["FadeSeconds"],
    value = df_results["orderingTime"]
  )
  # Basic histogram
  p <- ggplot(df_plot, aes(x=orderingTime, fill=FadeSeconds, color=FadeSeconds)) + 
    geom_histogram(color="grey", binwidth=2000, position="dodge") +
    theme_minimal() +
    labs(x = "Time (ms)", y = "Count", title="Ordering time", fill = "Group", color="Group") +
    scale_color_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))+
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))
  ggsave("orderingTimeHist.pdf", p, NULL, NULL, 1, 7, 5)

  ## Matching time histogram
  df_plot <- data.frame(
    group = df_results["FadeSeconds"],
    value = df_results["matchingTime"]
  )
  # Basic histogram
  p <- ggplot(df_plot, aes(x=matchingTime, fill=FadeSeconds, color=FadeSeconds)) + 
    geom_histogram(color="grey", binwidth=2000, position="dodge") +
    theme_minimal() +
    labs(x = "Time (ms)", y = "Count", title="Matching time", fill = "Group", color="Group") +
    scale_color_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))+
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))
  ggsave("matchingTimeHist.pdf", p, NULL, NULL, 1, 7, 5)

  ## Counting time histogram
  df_plot <- data.frame(
    group = df_results["FadeSeconds"],
    value = df_results["countingTime"]
  )
  # Basic histogram
  p <- ggplot(df_plot, aes(x=countingTime, fill=FadeSeconds, color=FadeSeconds)) + 
    geom_histogram(color="grey", binwidth=2000, position="dodge") +
    theme_minimal() +
    labs(x = "Time (ms)", y = "Count", title="Counting time", fill = "Group", color="Group") +
    scale_color_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))+
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))
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
    geom_histogram(binwidth=0.5, colour="black", fill="#fc8d59") +
    theme_minimal() +
    labs(x = "Time (ms)", y = "Count", title="Alarm duration", fill = "Group") +
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))
  ggsave("alarmDurationHist.pdf", p, NULL, NULL, 1, 7, 5)
  rm(dt)

  rm(df_plot)
  rm(bp)
}
