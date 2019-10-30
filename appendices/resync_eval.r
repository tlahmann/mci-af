# set the working directory
setwd(
  "D:\\Data\\study\\12.Semester\\mci-af\\appendices"
)
.libPaths("C:/Users/tobia/Documents/R/win-library/3.6")

#### Variables
##################
plot <- TRUE
file <- c("resync_clean.csv", "resync2_clean.csv")

#### Basic import
##################

# read in the csv file:
df_results <-
  read.csv(file[[1]], header = TRUE, sep = ";", stringsAsFactors = FALSE) 	# read in the first line as column labels with header=TRUE
df_results2 <-
  read.csv(file[[2]], header = TRUE, sep = ";", stringsAsFactors = FALSE) 	# read in the first line as column labels with header=TRUE

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
library(plyr)
# library(pastecs)
# library(reshape)
library(ggplot2)
library(scales)
library(tidyr)
# library(matrixStats)

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


#### SAM
df_plot <- rbind.fill(
  data.frame(
    "Pleasure" = df_results$SAM_pleasure_pre,
    "Arousal" = df_results$SAM_arousal_pre,
    "Dominance" = df_results$SAM_dominance_pre,
    "group" = df_results$FadeSeconds
  ), data.frame(
    "Pleasure" = df_results2$SAM_pleasure_pre,
    "Arousal" = df_results2$SAM_arousal_pre,
    "Dominance" = df_results2$SAM_dominance_pre,
    "group" = df_results2$FadeSeconds
  )
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

df_plot <- rbind.fill(
  data.frame(
    "Pleasure" = df_results$SAM_pleasure_post,
    "Arousal" = df_results$SAM_arousal_post,
    "Dominance" = df_results$SAM_dominance_post,
    "group" = df_results$FadeSeconds
  ), data.frame(
    "Pleasure" = df_results2$SAM_pleasure_post,
    "Arousal" = df_results2$SAM_arousal_post,
    "Dominance" = df_results2$SAM_dominance_post,
    "group" = df_results2$FadeSeconds
  )
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

rm(df_plot)
rm(bp)
