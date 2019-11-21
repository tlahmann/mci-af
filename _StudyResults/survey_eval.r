# set the working directory
setwd(
  "D:\\Data\\study\\12.Semester\\mci-af\\_StudyResults"
)
.libPaths("C:/Users/tobia/Documents/R/win-library/3.6")

#### Variables
##################
plot <- FALSE
file <- c("results-survey512856.csv", "results-survey382784.csv", "results-surveyCombined.csv")

#### Basic import
##################

# read in the csv file:
df_results <-
  read.csv(file[[3]], header = TRUE, stringsAsFactors = FALSE) 	# read in the first line as column labels with header=TRUE
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
names(df_results)[names(df_results) == "To.what.extent.do.you.agree.with.the.following.statements...I.felt.tired.before.the.experiment.."] <- "tiredBefore"
names(df_results)[names(df_results) == "To.what.extent.do.you.agree.with.the.following.statements...I.felt.tired.after.the.experiment.."] <- "tiredAfter"
names(df_results)[names(df_results) == "To.what.extent.do.you.agree.with.the.following.statements...I.felt.comfortable.trying.to.sleep.with.a.VR.head.mounted.display.."] <- "comfortableHeadset"
names(df_results)[names(df_results) == "To.what.extent.do.you.agree.with.the.following.statements...I.felt.more.awake.after.solving.the.task.."] <- "awakeAfterSolving"
names(df_results)[names(df_results) == "To.what.extent.do.you.agree.with.the.following.statements...The.transition.from.sleeping.resting.to.solving.tasks.was.easy.for.me.."] <- "transitionEasy"
names(df_results)[names(df_results) == "To.what.extent.do.you.agree.with.the.following.statements...I.can.imagine.being.woken.up.like.in.the.experiment.to.prepare.for.dangerous.situations..e.g..take.the.control.of.a.car..."] <- "imagineWakingUp"
names(df_results)[names(df_results) == "To.what.extent.do.you.agree.with.the.following.statements...I.can.imagine.wearing.a.head.mounted.display.permanently..if.they.become.tiny.and.comfortable.."] <- "permanentWearing"
names(df_results)[names(df_results) == "To.what.extent.do.you.agree.with.the.following.statements...I.usually.sleep.with.a.sleep.mask.on.."] <- "sleepingMask"
names(df_results)[names(df_results) == "To.what.extent.do.you.agree.with.the.following.statements...I.have.experience.with.meditation.."] <- "meditation"
names(df_results)[names(df_results) == "To.what.extent.do.you.agree.with.the.following.statements...I.didn.t.feel.that.comfortable.trying.to.sleep.with.a.VR.head.mounted.display.."] <- "notComfortableHeadset"
names(df_results)[names(df_results) == "To.what.extent.do.you.agree.with.the.following.statements...I.felt.awake.before.the.experiment.."] <- "awakeBefore"
names(df_results)[names(df_results) == "To.what.extent.do.you.agree.with.the.following.statements...The.transition.from.sleeping.resting.to.solving.tasks.was.hard.for.me.."] <- "transitionHard"

# Change data selection
df_results$gender <- as.character(df_results$gender)
df_results$gender[df_results$gender == "A1"] <- "[A1] female"
df_results$gender[df_results$gender == "A2"] <- "[A2] male"
df_results$gender[df_results$gender == "A3"] <- "[A3] diverse"

# df_results$experienceAR[df_results$experienceAR == "A1"] <- 1
# df_results$experienceAR[df_results$experienceAR == "A2"] <- 2
# df_results$experienceAR[df_results$experienceAR == "A3"] <- 3
# df_results$experienceAR[df_results$experienceAR == "A4"] <- 4
# df_results$experienceAR[df_results$experienceAR == "A5"] <- 5
# df_results$experienceAR[df_results$experienceAR == "A6"] <- 6
# df_results$experienceAR[df_results$experienceAR == "A7"] <- 7
# df_results$experienceAR <- as.numeric(df_results$experienceAR)
# print(stat.desc(df_results["experienceAR"]))

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

for (i in c("tiredBefore", "tiredAfter", "comfortableHeadset", "awakeAfterSolving", "transitionEasy", "imagineWakingUp", "permanentWearing", "sleepingMask", "meditation", "notComfortableHeadset", "awakeBefore", "transitionHard")) {
  tmp <- df_results[i]
  # print(a[a == "A1"])
  tmp[tmp == "A1"] <- "[A1] Strongly Agree"
  tmp[tmp == "A2"] <- "[A2] Agree"
  tmp[tmp == "A3"] <- "[A3] Somewhat agree"
  tmp[tmp == "A4"] <- "[A4] Neither agree nor disagree"
  tmp[tmp == "A5"] <- "[A5] Somewhat disagree"
  tmp[tmp == "A6"] <- "[A6] Disagree"
  tmp[tmp == "A7"] <- "[A7] Strongly disagree"
  df_results[i] <- tmp
}
rm(i)
rm(tmp)

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
    # panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=40, face="bold")
    )
    
  ### Plot Gender
  t <- table(df_results["gender"])
  g <- c("[A2] male", "[A1] female", "[A3] divers")
  v <- c(t[g[1]], t[g[2]], t[g[3]])
  v[is.na(v)] <- 0
  v <- as.numeric(as.character(v))
  df_plot <- data.frame(
    group = g,
    value = v
  )
  bp <- ggplot(df_plot, aes(x = "", y = value, fill = group)) +
    geom_bar(width = 1, stat = "identity")
  bp <- bp + coord_polar("y", start = 0) +
    blank_theme +
    labs(x = "", y = "") +
    guides(fill = guide_legend(title = "Gender")) +
    theme(plot.title = element_text(size = 12, face = "bold"),
      legend.title=element_text(size=20), 
      legend.text=element_text(size=16),
      axis.text.x=element_blank()) +
    scale_fill_manual(values = c("#3288bd", "#d53e4f", "#fc8d59"))
  ggsave("gender.pdf", bp, NULL, NULL, 1, 7, 5)

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
    theme(plot.title = element_text(size = 12, face = "bold"),
      legend.title=element_text(size=20), 
      legend.text=element_text(size=16),
      axis.text.x=element_blank()) +
    scale_fill_manual(values = c("#d53e4f", "#fc8d59", "#fee08b", "#ffffbf", "#e6f598", "#99d594", "#3288bd"))
  ggsave("expVr.pdf", pie, NULL, NULL, 1, 7, 5)

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
    theme(plot.title = element_text(size = 12, face = "bold"),
      legend.title=element_text(size=20), 
      legend.text=element_text(size=16),
      axis.text.x=element_blank()) +
    scale_fill_manual(values = c("#d53e4f", "#fc8d59", "#fee08b", "#ffffbf", "#e6f598", "#99d594", "#3288bd"))
  ggsave("expAr.pdf", pie, NULL, NULL, 1, 7, 5)

  ## Plot Age
  df_plot <- data.frame("var" = df_results$age,
                        "group" = "Player")
  # Change automatically color by groups
  bp <-
    # theme_classic() +
    ggplot(df_plot, aes(x = group, y = var, fill = group)) +
    geom_boxplot() +
    blank_theme+
    labs(x = "", y = "Age") +
    theme(legend.position = "none", text = element_text(size = 20)) +
    scale_fill_manual(values = c("#fc8d59")) + coord_flip()
  ggsave("age.pdf", bp, NULL, NULL, 1, 7, 5)
  

  for (i in c("tiredBefore", "tiredAfter", "comfortableHeadset", "awakeAfterSolving", "transitionEasy", "imagineWakingUp", "permanentWearing", "sleepingMask", "meditation", "notComfortableHeadset", "awakeBefore", "transitionHard")) {
    t <- table(df_results[i])
    g <- c("[A1] Strongly Agree", "[A2] Agree", "[A3] Somewhat agree", "[A4] Neither agree nor disagree", "[A5] Somewhat disagree", "[A6] Disagree", "[A7] Strongly disagree")
    v <- c(t[g[1]], t[g[2]], t[g[3]], t[g[4]], t[g[5]], t[g[6]], t[g[7]])
    v[is.na(v)] <- 0
    v <- as.numeric(as.character(v))
    df_plot <- data.frame(group = g, value = v)
    p<-ggplot(
      data=df_plot, 
      aes(x=group, y=value, fill=group)) +
      geom_bar(stat="identity",
      show.legend = FALSE)+
      blank_theme+
      labs(x = "Agreement", y = "Count")+
      scale_fill_manual(values = c("#d53e4f", "#fc8d59", "#fee08b", "#ffffbf", "#e6f598", "#99d594", "#3288bd"))
    ggsave(paste(i, ".pdf", sep=""), p, NULL, NULL, 1, 12, 5)
  }
  rm(i)

  rm(df_plot)
  rm(bp)
  rm(pie)
  rm(p)
}
