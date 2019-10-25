# set the working directory
setwd(
  "D:\\Data\\Dropbox\\Study\\mci-af\\appendices"
)
.libPaths("C:/Users/Tobias/Documents/R/win-library/3.4")

#### Variables
##################
plot <- TRUE
file <- c("results-survey512856.csv", "results-survey382784.csv", "results-surveyCombined.csv")

#### Basic import
##################

# read in the csv file:
df_results <-
  read.csv(file[[3]], header = TRUE) 	# read in the first line as column labels with header=TRUE
# df2 <-
  # read.csv(file[[2]], header = TRUE) 	# read in the first line as column labels with header=TRUE

# print("Columns of the result set:")
# print(names(df1)) 
# print("Columns of the result set:")
# print(names(df2)) 

# library(plyr)

# df_results <- rbind.fill(d1, d2)
# rm(df1)
# rm(df2)

# remove unnecessary columns:
for (i in c("Response.ID", "Date.submitted", "Last.page", "Start.language", "What.is.the.highest.degree.or.level.of.school.you.have.completed.Â.If.currently.enrolled..highest.degree.received...Other.")) {
  df_results[[i]] <- NULL
}
rm(i)

# write cleaned data
# write.csv(df_results, file[[3]])

##### Rename data columns
names(df_results)[names(df_results) == "Please.enter.your.participant.id"] <- "participantId"
names(df_results)[names(df_results) == "Please.enter.your.age.in.years"] <- "age"
names(df_results)[names(df_results) == "Please.select.your.gender"] <- "gender"
names(df_results)[names(df_results) == "What.subject..if.any..did.you.study.or.are.you.currently.studying."] <- "Current studies"
names(df_results)[names(df_results) == "What.is.the.highest.degree.or.level.of.school.you.have.completed.Â.If.currently.enrolled..highest.degree.received."] <- "highestDegree"
names(df_results)[names(df_results) == "How.much.experience.do.you.have.with.virtual.reality..VR..and.augmented.reality..AR....VR."] <- "experienceVR"
names(df_results)[names(df_results) == "How.much.experience.do.you.have.with.virtual.reality..VR..and.augmented.reality..AR....AR."] <- "experienceAR"

# Change data selection
df_results$gender <- as.character(df_results$gender)
df_results$gender[df_results$gender == "A1"] <- "female"
df_results$gender[df_results$gender == "A2"] <- "male"
df_results$gender[df_results$gender == "A3"] <- "diverse"

df_results$experienceVR <- as.character(df_results$experienceVR)
df_results$experienceVR[df_results$experienceVR == "A1"] <- "[A1] No experience at all"
df_results$experienceVR[df_results$experienceVR == "A2"] <- "[A2] Almost no experience"
df_results$experienceVR[df_results$experienceVR == "A3"] <- "[A3] Less than average experience"
df_results$experienceVR[df_results$experienceVR == "A4"] <- "[A4] Some experience"
df_results$experienceVR[df_results$experienceVR == "A5"] <- "[A5] More than average experience"
df_results$experienceVR[df_results$experienceVR == "A6"] <- "[A6] Experienced"
df_results$experienceVR[df_results$experienceVR == "A7"] <- "[A7] Very highly experienced"
df_results$experienceAR <- as.character(df_results$experienceAR)
df_results$experienceAR[df_results$experienceAR == "A1"] <- "[A1] No experience at all"
df_results$experienceAR[df_results$experienceAR == "A2"] <- "[A2] Almost no experience"
df_results$experienceAR[df_results$experienceAR == "A3"] <- "[A3] Less than average experience"
df_results$experienceAR[df_results$experienceAR == "A4"] <- "[A4] Some experience"
df_results$experienceAR[df_results$experienceAR == "A5"] <- "[A5] More than average experience"
df_results$experienceAR[df_results$experienceAR == "A6"] <- "[A6] Experienced"
df_results$experienceAR[df_results$experienceAR == "A7"] <- "[A7] Very highly experienced"

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
library(reshape)
library(ggplot2)
library(scales)
library(tidyr)
library(matrixStats)

print(table(df_results["gender"]))
print(stat.desc(df_results["age"]))

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
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=20, face="bold")
    )
    
  ### Plot Gender
  df_plot <- data.frame(
    group = c("male", "female"),
    value = c(
      table(df_results["gender"])[[2]],
      table(df_results["gender"])[[1]]
    )
  )
  bp <- ggplot(df_plot, aes(x = "", y = value, fill = group)) +
    geom_bar(width = 1, stat = "identity")
  pie <- bp + coord_polar("y", start = 0) +
    labs(x = "", y = "") +
    guides(fill = guide_legend(title = "Gender")) +
    theme(text = element_text(size = 20))
  pie +
      geom_text(aes(y = value / 3 + c(0, cumsum(value)[-length(value)]),
                      label = percent(value / length(df_results$gender))), size = 6) +
    scale_fill_manual(values = c("#d73027", "#4575b4", "#ffffbf"))


  ### Plot VR experience
  t <- table(df_results["experienceVR"])
  g <- c("[A1] No experience at all", "[A2] Almost no experience", "[A3] Less than average experience", "[A4] Some experience", "[A5] More than average experience", "[A6] Experienced", "[A7] Very highly experienced")
  v <- c(t[g[1]], t[g[2]], t[g[3]], t[g[4]], t[g[5]], t[g[6]], t[g[7]])
  v[is.na(v)] <- 0
  v <- as.numeric(as.character(v))
  df_plot <- data.frame(
    group = g,
    value = v
  )
  bp <- ggplot(df_plot, aes(x = "", y = value, fill = group)) +
    geom_bar(width = 1, stat = "identity")
  pie <- bp + coord_polar("y", start = 0) +
    blank_theme +
    labs(x = "", y = "") +
    guides(fill = guide_legend(title = "Experience (VR)")) +
    theme(axis.text.x=element_blank())
  pie + scale_fill_manual(values = c("#d73027", "#fc8d59", "#fee090", "#ffffbf", "#e0f3f8", "#91bfdb", "#4575b4"))

  ### Plot AR experience
  t <- table(df_results["experienceAR"])
  g <- c("[A1] No experience at all", "[A2] Almost no experience", "[A3] Less than average experience", "[A4] Some experience", "[A5] More than average experience", "[A6] Experienced", "[A7] Very highly experienced")
  v <- c(t[g[1]], t[g[2]], t[g[3]], t[g[4]], t[g[5]], t[g[6]], t[g[7]])
  v[is.na(v)] <- 0
  v <- as.numeric(as.character(v))
  df_plot <- data.frame(
    group = g,
    value = v
  )
  bp <- ggplot(df_plot, aes(x = "", y = value, fill = group)) +
    geom_bar(width = 1, stat = "identity")
  pie <- bp + coord_polar("y", start = 0) +
    blank_theme +
    labs(x = "", y = "") +
    guides(fill = guide_legend(title = "Experience (AR)")) +
    theme(axis.text.x=element_blank()) 
  pie + scale_fill_manual(values = c("#d73027", "#fc8d59", "#fee090", "#ffffbf", "#e0f3f8", "#91bfdb", "#4575b4"))

  # # "Collection Count", "Sam Pleasure", "Sam Arousal", "Sam Dominance"
  # df_plot <-
  #   data.frame("var" = df_results$approachHaltedTimeAvg,
  #              "group" = df_results$soundGroup)
  # df_plot$group <-
  #   factor(df_plot$group, c("LVHA", "HVHA", "HVLA"))
  # # Change  automatically color by groups
  # bp <-
  #   ggplot(df_plot, aes(x = group, y = var, fill = group)) +
  #   geom_boxplot() +
  #   labs(x = "Sound Group", y = "Seconds Stopped") +
  #   guides(fill = guide_legend(title = "Sound Group"))
  # bp + scale_fill_manual(values = c("#941c20", "#4A5745", "#009269"))
  # # bp + theme_classic()

  ## Plot Age
  df_plot <- data.frame("var" = df_results$age,
                        "group" = "Player")
  # Change automatically color by groups
  bp <-
    ggplot(df_plot, aes(x = group, y = var, fill = group)) +
    geom_boxplot() +
    labs(x = "", y = "Age") +
    theme(legend.position = "none", text = element_text(size = 20))
  bp + scale_fill_manual(values = c("#009269", "#4A5745", "#941c20")) + coord_flip()
  
  rm(df_plot)
  rm(bp)
  rm(pie)
}
