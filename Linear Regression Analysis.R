###############################################################################
#AUTHOR: Rachel C. Rice
#DATE: November 16, 2021
#PURPOSE: Perform linear regression analysis on sipper data
#In essence, does number of counts logged by the sipper predict mL drunk?
#In addition, we will use three-way ANOVA to investigate differences in counts.
###############################################################################
 
library(readxl)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(rstatix)
library(WebPower)
stringsAsFactors = FALSE

#DATA IMPORT AND MANIPULATION INTO USEFUL FRAMES.
#NAMELY, WE NEED TO CONSTRUCT DATA FRAMES BOTH SEPARATING AND COMBINING L AND R SIDE VALUES FOR BULK ANALYSIS.
#WANT TO DO THE SAME FOR ETOH DATA AND WATER DATA PRETENDING IT WAS LIKE ETOH.

#Read in Bulk fluid data
fluid_data_m1 <- read_excel(path = "Automated_Drinking_Male_Cohort_1_Continuous_Access_09.2021.xlsx", sheet="LinReg Data",range = "LinReg Data!B3:J254", col_names = c("Day", "Mouse", "Age", "Leftg", "Leftg/kg", "Rightg", "Rightg/kg", "LeftCounts", "RightCounts"))
fluid_data_m1[fluid_data_m1<=0] <- NA
fluid_data_m2 <- read_excel(path = "Automated_Drinking_Male_Cohort_2_Continuous_Access_10.2021.xlsx", sheet="LinReg Data",range = "LinReg Data!B3:J254", col_names = c("Day", "Mouse", "Age", "Leftg", "Leftg/kg", "Rightg", "Rightg/kg", "LeftCounts", "RightCounts"))
fluid_data_m2[fluid_data_m2<=0] <- NA
fluid_data_f <- read_excel(path = "Automated_Drinking_Female_Cohort_Continuous_Access_11.2021.xlsx", sheet="LinReg Data",range = "LinReg Data!B3:J506", col_names = c("Day", "Mouse", "Age", "Leftg", "Leftg/kg", "Rightg", "Rightg/kg", "LeftCounts", "RightCounts"))
fluid_data_f[fluid_data_f<=0] <- NA


#Make a left data frame for each cohort with bulk data (ignoring EtOH and water)
fluid_data_left_frame_m1 <- data.frame()
for (row in 1:nrow(fluid_data_m1)) {
  fluid_data_left_frame_m1[row,"Day"] <- fluid_data_m1[row,"Day"]
  fluid_data_left_frame_m1[row,"Mouse"] <- fluid_data_m1[row,"Mouse"]
  fluid_data_left_frame_m1[row,"Age"] <- fluid_data_m1[row,"Age"]
  fluid_data_left_frame_m1[row, "Sex"] <- "M"
  fluid_data_left_frame_m1[row,"Left g"] <- fluid_data_m1[row,"Leftg"]
  fluid_data_left_frame_m1[row,"Left g/kg"] <- fluid_data_m1[row,"Leftg/kg"]
  fluid_data_left_frame_m1[row,"Left Counts"] <- fluid_data_m1[row,"LeftCounts"]

}

fluid_data_left_frame_m2 <- data.frame()
for (row in 1:nrow(fluid_data_m2)) {
  fluid_data_left_frame_m2[row,"Day"] <- fluid_data_m2[row,"Day"]
  fluid_data_left_frame_m2[row,"Mouse"] <- fluid_data_m2[row,"Mouse"]
  fluid_data_left_frame_m2[row,"Age"] <- fluid_data_m2[row,"Age"]
  fluid_data_left_frame_m2[row, "Sex"] <- "M"
  fluid_data_left_frame_m2[row,"Left g"] <- fluid_data_m2[row,"Leftg"]
  fluid_data_left_frame_m2[row,"Left g/kg"] <- fluid_data_m2[row,"Leftg/kg"]
  fluid_data_left_frame_m2[row,"Left Counts"] <- fluid_data_m2[row,"LeftCounts"]
  
}

fluid_data_left_frame_f <- data.frame()
for (row in 1:nrow(fluid_data_f)) {
  fluid_data_left_frame_f[row,"Day"] <- fluid_data_f[row,"Day"]
  fluid_data_left_frame_f[row,"Mouse"] <- fluid_data_f[row,"Mouse"]
  fluid_data_left_frame_f[row,"Age"] <- fluid_data_f[row,"Age"]
  fluid_data_left_frame_f[row, "Sex"] <- "F"
  fluid_data_left_frame_f[row,"Left g"] <- fluid_data_f[row,"Leftg"]
  fluid_data_left_frame_f[row,"Left g/kg"] <- fluid_data_f[row,"Leftg/kg"]
  fluid_data_left_frame_f[row,"Left Counts"] <- fluid_data_f[row,"LeftCounts"]
  
}

#Make a right data frame for each cohort
fluid_data_right_frame_m1 <- data.frame()
for (row in 1:nrow(fluid_data_m1)) {
  fluid_data_right_frame_m1[row,"Day"] <- fluid_data_m1[row,"Day"]
  fluid_data_right_frame_m1[row,"Mouse"] <- fluid_data_m1[row,"Mouse"]
  fluid_data_right_frame_m1[row,"Age"] <- fluid_data_m1[row,"Age"]
  fluid_data_right_frame_m1[row,"Sex"] <- "M"
  fluid_data_right_frame_m1[row,"Right g"] <- fluid_data_m1[row,"Rightg"]
  fluid_data_right_frame_m1[row,"Right g/kg"] <- fluid_data_m1[row,"Rightg/kg"]
  fluid_data_right_frame_m1[row,"Right Counts"] <- fluid_data_m1[row,"RightCounts"]
}

fluid_data_right_frame_m2 <- data.frame()
for (row in 1:nrow(fluid_data_m2)) {
  fluid_data_right_frame_m2[row,"Day"] <- fluid_data_m2[row,"Day"]
  fluid_data_right_frame_m2[row,"Mouse"] <- fluid_data_m2[row,"Mouse"]
  fluid_data_right_frame_m2[row,"Age"] <- fluid_data_m2[row,"Age"]
  fluid_data_right_frame_m2[row,"Sex"] <- "M"
  fluid_data_right_frame_m2[row,"Right g"] <- fluid_data_m2[row,"Rightg"]
  fluid_data_right_frame_m2[row,"Right g/kg"] <- fluid_data_m2[row,"Rightg/kg"]
  fluid_data_right_frame_m2[row,"Right Counts"] <- fluid_data_m2[row,"RightCounts"]
}

fluid_data_right_frame_f <- data.frame()
for (row in 1:nrow(fluid_data_f)) {
  fluid_data_right_frame_f[row,"Day"] <- fluid_data_f[row,"Day"]
  fluid_data_right_frame_f[row,"Mouse"] <- fluid_data_f[row,"Mouse"]
  fluid_data_right_frame_f[row,"Age"] <- fluid_data_f[row,"Age"]
  fluid_data_right_frame_f[row,"Sex"] <- "F"
  fluid_data_right_frame_f[row,"Right g"] <- fluid_data_f[row,"Rightg"]
  fluid_data_right_frame_f[row,"Right g/kg"] <- fluid_data_f[row,"Rightg/kg"]
  fluid_data_right_frame_f[row,"Right Counts"] <- fluid_data_f[row,"RightCounts"]
}

#Combine left and right data frame for each cohort for bulk analysis and for all sexes

fluid_data_left_frame_m1_combine <- fluid_data_left_frame_m1
colnames(fluid_data_left_frame_m1_combine) <- c("Day", "Mouse", "Age", "Sex", "g", "gkg", "Counts")
fluid_data_left_frame_m1_combine$Side <- "Left"
fluid_data_left_frame_m2_combine <- fluid_data_left_frame_m2
colnames(fluid_data_left_frame_m2_combine) <- c("Day", "Mouse", "Age", "Sex", "g", "gkg", "Counts")
fluid_data_left_frame_m2_combine$Side <- "Left"
fluid_data_left_frame_f_combine <- fluid_data_left_frame_f
colnames(fluid_data_left_frame_f_combine) <- c("Day", "Mouse", "Age", "Sex", "g", "gkg", "Counts")
fluid_data_left_frame_f_combine$Side <- "Left"
fluid_data_right_frame_m1_combine <- fluid_data_right_frame_m1
colnames(fluid_data_right_frame_m1_combine) <- c("Day", "Mouse", "Age", "Sex", "g", "gkg", "Counts")
fluid_data_right_frame_m1_combine$Side <- "Right"
fluid_data_right_frame_m2_combine <- fluid_data_right_frame_m2
colnames(fluid_data_right_frame_m2_combine) <- c("Day", "Mouse", "Age", "Sex", "g", "gkg", "Counts")
fluid_data_right_frame_m2_combine$Side <- "Right"
fluid_data_right_frame_f_combine <- fluid_data_right_frame_f
colnames(fluid_data_right_frame_f_combine) <- c("Day", "Mouse", "Age", "Sex", "g", "gkg", "Counts")
fluid_data_right_frame_f_combine$Side <- "Right"

fluid_data_LandR_m1 <- bind_rows(fluid_data_left_frame_m1_combine, fluid_data_right_frame_m1_combine)
fluid_data_LandR_m2 <- bind_rows(fluid_data_left_frame_m2_combine ,fluid_data_right_frame_m2_combine)
fluid_data_L_m <- bind_rows(fluid_data_left_frame_m1_combine, fluid_data_left_frame_m2_combine)
fluid_data_R_m <- bind_rows(fluid_data_right_frame_m1_combine, fluid_data_right_frame_m2_combine)
fluid_data_LandR_m <- bind_rows(fluid_data_LandR_m1, fluid_data_LandR_m2)
fluid_data_LandR_f <- bind_rows(fluid_data_left_frame_f_combine, fluid_data_right_frame_f_combine)
fluid_data_LandR_all <- bind_rows(fluid_data_LandR_m, fluid_data_LandR_f)
fluid_data_L_all <- bind_rows(fluid_data_L_m, fluid_data_left_frame_f_combine)
fluid_data_R_all <- bind_rows(fluid_data_R_m, fluid_data_right_frame_f_combine)


#READ IN ETOH AND REPEAT THE PROCESS FOR ETOH, H2O, AND COMBINED. 
EtOH_data_import_m1 <- read_excel(path = "Automated_Drinking_Male_Cohort_1_Continuous_Access_09.2021.xlsx", sheet="LinReg Data",range = "LinReg Data!M3:U128", col_names = c("Day", "Mouse", "Age", "EtOH g", "EtOH g/kg", "EtOH Counts", "H2O g", "H2O g/kg", "H2O Counts"))
EtOH_data_import_m1[EtOH_data_import_m1<=0] <- NA
EtOH_data_import_m2 <- read_excel(path = "Automated_Drinking_Male_Cohort_2_Continuous_Access_10.2021.xlsx", sheet="LinReg Data",range = "LinReg Data!M3:U128", col_names = c("Day", "Mouse", "Age", "EtOH g", "EtOH g/kg", "EtOH Counts", "H2O g", "H2O g/kg", "H2O Counts"))
EtOH_data_import_m2[EtOH_data_import_m2<=0] <- NA
EtOH_data_import_f <-read_excel(path = "Automated_Drinking_Female_Cohort_Continuous_Access_11.2021.xlsx", sheet="LinReg Data",range = "LinReg Data!M3:U254", col_names = c("Day", "Mouse", "Age", "EtOH g", "EtOH g/kg", "EtOH Counts", "H2O g", "H2O g/kg", "H2O Counts"))
EtOH_data_import_f[EtOH_data_import_f<=0] <- NA

#Make a left data frame for each cohort with bulk data (ignoring EtOH and water)
EtOH_data_m1 <- data.frame()
for (row in 1:nrow(EtOH_data_import_m1)) {
  EtOH_data_m1[row,"Day"] <- EtOH_data_import_m1[row,"Day"]
  EtOH_data_m1[row,"Mouse"] <- EtOH_data_import_m1[row,"Mouse"]
  EtOH_data_m1[row,"Age"] <- EtOH_data_import_m1[row,"Age"]
  EtOH_data_m1[row, "Sex"] <- "M"
  EtOH_data_m1[row,"EtOH g"] <- EtOH_data_import_m1[row,"EtOH g"]
  EtOH_data_m1[row,"EtOH g/kg"] <- EtOH_data_import_m1[row,"EtOH g/kg"]
  EtOH_data_m1[row,"EtOH Counts"] <- EtOH_data_import_m1[row,"EtOH Counts"]
  
}

EtOH_data_m2 <- data.frame()
for (row in 1:nrow(EtOH_data_import_m2)) {
  EtOH_data_m2[row,"Day"] <- EtOH_data_import_m2[row,"Day"]
  EtOH_data_m2[row,"Mouse"] <- EtOH_data_import_m2[row,"Mouse"]
  EtOH_data_m2[row,"Age"] <- EtOH_data_import_m2[row,"Age"]
  EtOH_data_m2[row, "Sex"] <- "M"
  EtOH_data_m2[row,"EtOH g"] <- EtOH_data_import_m2[row,"EtOH g"]
  EtOH_data_m2[row,"EtOH g/kg"] <- EtOH_data_import_m2[row,"EtOH g/kg"]
  EtOH_data_m2[row,"EtOH Counts"] <- EtOH_data_import_m2[row,"EtOH Counts"]
  
}

EtOH_data_f <- data.frame()
for (row in 1:nrow(EtOH_data_import_f)) {
  EtOH_data_f[row,"Day"] <- EtOH_data_import_f[row,"Day"]
  EtOH_data_f[row,"Mouse"] <- EtOH_data_import_f[row,"Mouse"]
  EtOH_data_f[row,"Age"] <- EtOH_data_import_f[row,"Age"]
  EtOH_data_f[row, "Sex"] <- "F"
  EtOH_data_f[row,"EtOH g"] <- EtOH_data_import_f[row,"EtOH g"]
  EtOH_data_f[row,"EtOH g/kg"] <- EtOH_data_import_f[row,"EtOH g/kg"]
  EtOH_data_f[row,"EtOH Counts"] <- EtOH_data_import_f[row,"EtOH Counts"]
  
}

#Make an H2O data frame for each cohort
H2O_data_m1 <- data.frame()
for (row in 1:nrow(EtOH_data_import_m1)) {
  H2O_data_m1[row,"Day"] <- EtOH_data_import_m1[row,"Day"]
  H2O_data_m1[row,"Mouse"] <- EtOH_data_import_m1[row,"Mouse"]
  H2O_data_m1[row,"Age"] <- EtOH_data_import_m1[row,"Age"]
  H2O_data_m1[row, "Sex"] <- "M"
  H2O_data_m1[row,"H2O g"] <- EtOH_data_import_m1[row,"H2O g"]
  H2O_data_m1[row,"H2O g/kg"] <- EtOH_data_import_m1[row,"H2O g/kg"]
  H2O_data_m1[row,"H2O Counts"] <- EtOH_data_import_m1[row,"H2O Counts"]
  
}

H2O_data_m2 <- data.frame()
for (row in 1:nrow(EtOH_data_import_m2)) {
  H2O_data_m2[row,"Day"] <- EtOH_data_import_m2[row,"Day"]
  H2O_data_m2[row,"Mouse"] <- EtOH_data_import_m2[row,"Mouse"]
  H2O_data_m2[row,"Age"] <- EtOH_data_import_m2[row,"Age"]
  H2O_data_m2[row, "Sex"] <- "M"
  H2O_data_m2[row,"H2O g"] <- EtOH_data_import_m2[row,"H2O g"]
  H2O_data_m2[row,"H2O g/kg"] <- EtOH_data_import_m2[row,"H2O g/kg"]
  H2O_data_m2[row,"H2O Counts"] <- EtOH_data_import_m2[row,"H2O Counts"]
  
}

H2O_data_f <- data.frame()
for (row in 1:nrow(EtOH_data_import_f)) {
  H2O_data_f[row,"Day"] <- EtOH_data_import_m2[row,"Day"]
  H2O_data_f[row,"Mouse"] <- EtOH_data_import_m2[row,"Mouse"]
  H2O_data_f[row,"Age"] <- EtOH_data_import_m2[row,"Age"]
  H2O_data_f[row, "Sex"] <- "F"
  H2O_data_f[row,"H2O g"] <- EtOH_data_import_m2[row,"H2O g"]
  H2O_data_f[row,"H2O g/kg"] <- EtOH_data_import_m2[row,"H2O g/kg"]
  H2O_data_f[row,"H2O Counts"] <- EtOH_data_import_m2[row,"H2O Counts"]

}

#Combine EtOH and H2O data frame for each cohort for bulk analysis and for all sexes

EtOH_data_m1_combine <- EtOH_data_m1
colnames(EtOH_data_m1_combine) <- c("Day", "Mouse", "Age", "Sex", "g", "gkg", "Counts")
EtOH_data_m2_combine <- EtOH_data_m2
colnames(EtOH_data_m2_combine) <- c("Day", "Mouse", "Age", "Sex", "g", "gkg", "Counts")
EtOH_data_f_combine <- EtOH_data_f
colnames(EtOH_data_f_combine) <- c("Day", "Mouse", "Age", "Sex", "g", "gkg", "Counts")
H2O_data_m1_combine <- H2O_data_m1
colnames(H2O_data_m1_combine) <- c("Day", "Mouse", "Age", "Sex", "g", "gkg", "Counts")
H2O_data_m2_combine <- H2O_data_m2
colnames(H2O_data_m2_combine) <- c("Day", "Mouse", "Age", "Sex", "g", "gkg", "Counts")
H2O_data_f_combine <- H2O_data_f
colnames(H2O_data_f_combine) <- c("Day", "Mouse", "Age", "Sex", "g", "gkg", "Counts")

EtOH_m <- bind_rows(EtOH_data_m1_combine, EtOH_data_m2_combine)
EtOH_f <- EtOH_data_f_combine
H2O_m <- bind_rows(H2O_data_m1_combine, H2O_data_m2_combine)
H2O_f <-H2O_data_f_combine

EtOH_all <- bind_rows(EtOH_m, EtOH_f)
H2O_all <- bind_rows(H2O_m, H2O_f)


EtOH_and_H2O_m1 <- bind_rows(EtOH_data_m1_combine, H2O_data_m1_combine)
EtOH_and_H2O_m2 <- bind_rows(EtOH_data_m2_combine, H2O_data_m2_combine)
EtOH_and_H2O_m <- bind_rows(EtOH_and_H2O_m1, EtOH_and_H2O_m2)
EtOH_and_H2O_f <- bind_rows(EtOH_data_f_combine, H2O_data_f_combine)
EtOH_and_H2O_all <- bind_rows(fluid_data_LandR_m, fluid_data_LandR_f)

#NOW WE PLOT

total_fluid_regression <- ggplot(fluid_data_LandR_all, aes(x=Counts, y=g)) +
  geom_point(aes(color = Age, shape = Sex)) + 
  scale_color_manual(values=c("#3E067D","#F70065","#277f80")) +
  geom_smooth(method = "lm") +
  ggtitle("Total Fluid Data") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Sipper Counts") + ylab("Total g")

EtOH_all$Age <- factor(EtOH_all$Age, levels = c("3 wk", "6 wk", "18 wk"))
EtOH_gkg_plot <- ggplot(EtOH_all, aes(x=Counts, y=gkg)) +
  geom_point(aes(color = Age, shape = Sex)) + 
  scale_color_manual(values=c("#3E067D","#F70065","#277f80")) +
  geom_smooth(method = "lm", aes(linetype = Sex)) +
  ggtitle("Total g/kg EtOH") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Counts") + ylab("Total g/kg EtOH") 

H2O_all$Age <- factor(H2O_all$Age, levels = c("3 wk", "6 wk", "18 wk"))
H2O_gkg_plot <- ggplot(H2O_all, aes(x=Counts, y=gkg)) +
  geom_point(aes(color = Age, shape = Sex)) + 
  scale_color_manual(values=c("#3E067D","#F70065","#277f80")) +
  geom_smooth(method = "lm", aes(linetype = Sex)) +
  ggtitle("Total g/kg H2O (Ethanol Group)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Counts") + ylab("Total g/kg H2O") 

################################################################################
#2/22/22 redoing EtOH graph, splitting up by age/sex

#Male
EtOH_m_gkg_plot <- ggplot(EtOH_m, aes(x=Counts, y=gkg)) +
  geom_point(color = "#619CFF") + 
  geom_smooth(method = "lm", color = "black") +
  ggtitle("Males") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size = 15)) +
  xlab("Counts") + ylab("g/kg EtOH") +
  stat_cor(method = "pearson", label.y = 17, size = 5) + 
  stat_regline_equation(label.x = 1500, label.y = 17, size = 5)


#Female
EtOH_f_gkg_plot <- ggplot(EtOH_f, aes(x=Counts, y=gkg)) +
  geom_point(color = "#F8766D") + 
  geom_smooth(method = "lm", color = "black") +
  ggtitle("Females") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size = 15)) +
  xlab("Counts") + ylab("g/kg EtOH") +
  stat_cor(method = "pearson", label.y = 23, size = 5) + 
  stat_regline_equation(label.x = 1800, label.y = 23, size = 5)

#3 wk (male and female)
EtOH_3_gkg_plot <- ggplot(EtOH_all[EtOH_all$Age == "3 wk", ], aes(x=Counts, y=gkg)) +
  geom_point(color = "#3E067D") + 
  geom_smooth(method = "lm", color = "black") +
  ggtitle("3 wk") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size = 15)) +
  xlab("Counts") + ylab("g/kg EtOH") +
  stat_cor(method = "pearson", label.y = 25, size = 5) + 
  stat_regline_equation(label.x = 1800, label.y = 25, size = 5)

#6 wk (male and female)
EtOH_6_gkg_plot <- ggplot(EtOH_all[EtOH_all$Age == "6 wk", ], aes(x=Counts, y=gkg)) +
  geom_point(color = "#F70065") + 
  geom_smooth(method = "lm", color = "black") +
  ggtitle("6 wk") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size = 15)) +
  xlab("Counts") + ylab("g/kg EtOH") +
  stat_cor(method = "pearson", label.y = 19, size = 5) + 
  stat_regline_equation(label.x = 1500, label.y = 19, size = 5)

#18 wk (male and female)
EtOH_18_gkg_plot <- ggplot(EtOH_all[EtOH_all$Age == "18 wk", ], aes(x=Counts, y=gkg)) +
  geom_point(color = "#277f80") + 
  geom_smooth(method = "lm", color = "black") +
  ggtitle("18 wk") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size = 15)) +
  xlab("Counts") + ylab("g/kg EtOH") +
  stat_cor(method = "pearson", label.y = 19, size = 5) + 
  stat_regline_equation(label.x = 1800, label.y = 19, size = 5)


##TRY FOR WATER AS WELL JUST TO SEE
#Male
EtOH_m_gkg_plot <- ggplot(EtOH_m, aes(x=Counts, y=gkg)) +
  geom_point(color = "#619CFF") + 
  geom_smooth(method = "lm", color = "black") +
  ggtitle("Males") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size = 15)) +
  xlab("Counts") + ylab("g/kg EtOH") +
  stat_cor(method = "pearson", label.y = 17, size = 5) + 
  stat_regline_equation(label.x = 1500, label.y = 17, size = 5)


#Female
H2O_f_gkg_plot <- ggplot(H2O_f, aes(x=Counts, y=gkg)) +
  geom_point(color = "#F8766D") + 
  geom_smooth(method = "lm", color = "black") +
  ggtitle("Females") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size = 15)) +
  xlab("Counts") + ylab("g/kg H2O") +
  stat_cor(method = "pearson", label.y = 23, size = 5) + 
  stat_regline_equation(label.x = 1800, label.y = 23, size = 5)

#3 wk (male and female)
H2O_3_gkg_plot <- ggplot(H2O_all[H2O_all$Age == "3 wk", ], aes(x=Counts, y=gkg)) +
  geom_point(color = "#3E067D") + 
  geom_smooth(method = "lm", color = "black") +
  ggtitle("3 wk") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size = 15)) +
  xlab("Counts") + ylab("g/kg H2O") +
  stat_cor(method = "pearson", label.y = 25, size = 5) + 
  stat_regline_equation(label.x = 1800, label.y = 25, size = 5)

#6 wk (male and female)
H2O_6_gkg_plot <- ggplot(H2O_all[H2O_all$Age == "6 wk", ], aes(x=Counts, y=gkg)) +
  geom_point(color = "#F70065") + 
  geom_smooth(method = "lm", color = "black") +
  ggtitle("6 wk") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size = 15)) +
  xlab("Counts") + ylab("g/kg H2O") +
  stat_cor(method = "pearson", label.y = 19, size = 5) + 
  stat_regline_equation(label.x = 1500, label.y = 19, size = 5)

#18 wk (male and female)
H2O_18_gkg_plot <- ggplot(H2O_all[H2O_all$Age == "18 wk", ], aes(x=Counts, y=gkg)) +
  geom_point(color = "#F70065") + 
  geom_smooth(method = "lm", color = "black") +
  ggtitle("18 wk") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size = 15)) +
  xlab("Counts") + ylab("g/kg H2O") +
  stat_cor(method = "pearson", label.y = 19, size = 5) + 
  stat_regline_equation(label.x = 1800, label.y = 19, size = 5)


################################################################################

##3 Way ANOVA and corresponding plot to determine differences in daily counts

EtOH_ANOVA <- EtOH_all
EtOH_ANOVA$Fluid <- "EtOH"
H2O_ANOVA <- H2O_all
H2O_ANOVA$Fluid <- "H2O"
ANOVA_df <- na.omit(rbind(EtOH_ANOVA, H2O_ANOVA))

three_way_ANOVA <- aov(Counts ~ Sex * Fluid * Age, ANOVA_df)
three_way_Tukey <- TukeyHSD(three_way_ANOVA)
effect_size_eta <- eta_squared(three_way_ANOVA)
effect_size_Cohens_f <- sqrt((effect_size_eta)/(1-effect_size_eta))
#Cohen's d is 2*f
effect_size_Cohens_d <- 2*effect_size_Cohens_f


#Use partial eta squared since measurements could be used multiple times between groups?
power_analysis_sex <- wp.kanova(n = 742, ndf = 1, f = 0.17998106, ng = 7, alpha = 0.05)
power_analysis_fluid <- wp.kanova(n = 742, ndf = 1, f = 0.04846826, ng = 7,  alpha = 0.05)
power_analysis_age <-  wp.kanova(n = 742, ndf = 2, f = 0.03956414, ng = 7, alpha = 0.05)
power_analysis_sex_fluid <- wp.kanova(n = 742, ndf = 2, f = 0.08780343, ng = 7, alpha = 0.05)
power_analysis_sex_age <- wp.kanova(n = 742, ndf = 2, f = 0.07110003, ng = 7, alpha = 0.05)
power_analysis_fluid_age <- wp.kanova(n = 742, ndf = 2, f = 0.05670352, ng = 7, alpha = 0.05)
power_analysis_sex_fluid_age <- wp.kanova(n = 742, ndf = 2, f = 0.10925466, ng = 7, alpha = 0.05)

dodge <- position_dodge(width = 0.5)
all_plot <- ggplot(ANOVA_df, aes(x = Fluid, y = Counts)) + 
  geom_violin(width = 0.5, aes(fill = Sex), position = dodge) + 
  geom_boxplot(width = 0.1, aes(x = Fluid, y = Counts, color = Sex), position = dodge) +
  scale_color_manual(values = c("black", "black")) +
  xlab("Fluid Type") + ylab("Total Daily Counts") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + facet_wrap(~Age)

#GET AVERAGE COUNTS FOR EACH ANIMAL OVER ALL THE DAYS AND REDO ABOVE ANALYSES.
#Easier to do this in excel, was having issue with for loop.
#Debug for loop below later, here is print to excel.
write_excel_csv(na.omit(ANOVA_df), file = "Average ANOVA.csv")

##BUGGY CODE BELOW, FIX
#easiest to split sexes since numbers repeat across sexes.

male_ANOVA_df <- na.omit(ANOVA_df[ANOVA_df$Sex=="M",])
female_ANOVA_df <- na.omit(ANOVA_df[ANOVA_df$Sex=="F",])

##why am I missing data once I get to the ANOVA???? What's wrong with my loops?

#MALES

#output df for for loop
averaged_male_df <- as.data.frame(matrix(nrow = 1, ncol = 5))
colnames(averaged_male_df) <- c("Mouse", "Age", "Sex", "Fluid", "Avg_Counts")
males <- unique(sort(male_ANOVA_df[,"Mouse"]))
male_index <- 1

for (i in 1:length(males)) {
  print(males[i])
  current_mouse_df <- male_ANOVA_df[male_ANOVA_df$Mouse == males[i], ]
  print(current_mouse_df)
  
  mouse_EtOH_df <- as.data.frame(matrix(nrow = 1, ncol = 5))
  colnames(mouse_EtOH_df) <- c("Mouse", "Age", "Sex", "Fluid", "Avg_Counts")
  mouse_EtOH_df$Mouse <- males[i]
  mouse_EtOH_df$Age <- current_mouse_df[current_mouse_df$Mouse == males[i], "Age"][1]
  mouse_EtOH_df$Sex <- "M"
  mouse_EtOH_df$Fluid <- "EtOH"
  mouse_EtOH_df$Avg_Counts <- mean(current_mouse_df[current_mouse_df$Fluid == "EtOH", "Counts"])
  print(mouse_EtOH_df)
  
  mouse_H2O_df <- data.frame(matrix(nrow = 1, ncol = 5))
  colnames(mouse_H2O_df) <- c("Mouse", "Age", "Sex", "Fluid", "Avg_Counts")
  mouse_H2O_df$Mouse <- males[i]
  mouse_H2O_df$Age <- current_mouse_df[current_mouse_df$Mouse == males[i], "Age"][1]
  mouse_H2O_df$Sex <- "M"
  mouse_H2O_df$Fluid <- "H2O"
  mouse_H2O_df$Avg_Counts <- mean(current_mouse_df[current_mouse_df$Fluid == "H2O", "Counts"])
  
  mouse_df <- rbind(mouse_EtOH_df, mouse_H2O_df)
  
  
  averaged_male_df <- rbind(mouse_df, averaged_male_df)
}

averaged_male_df <- na.omit(averaged_male_df)

#FEMALES

#output df for for loop
averaged_female_df <- as.data.frame(matrix(nrow = 1, ncol = 5))
colnames(averaged_female_df) <- c("Mouse", "Age", "Sex", "Fluid", "Avg_Counts")
females <- unique(sort(female_ANOVA_df[,"Mouse"]))

for (i in 1:length(females)) {
  print(females[i])
  current_mouse_df <- female_ANOVA_df[female_ANOVA_df$Mouse == females[i], ]
  print(current_mouse_df)
  
  mouse_EtOH_df <- as.data.frame(matrix(nrow = 1, ncol = 5))
  colnames(mouse_EtOH_df) <- c("Mouse", "Age", "Sex", "Fluid", "Avg_Counts")
  mouse_EtOH_df$Mouse <- females[i]
  mouse_EtOH_df$Age <- current_mouse_df[current_mouse_df$Mouse == females[i], "Age"][1]
  mouse_EtOH_df$Sex <- "F"
  mouse_EtOH_df$Fluid <- "EtOH"
  mouse_EtOH_df$Avg_Counts <- mean(current_mouse_df[current_mouse_df$Fluid == "EtOH", "ACounts"])
  print(mouse_EtOH_df)
  
  mouse_H2O_df <- data.frame(matrix(nrow = 1, ncol = 5))
  colnames(mouse_H2O_df) <- c("Mouse", "Age", "Sex", "Fluid", "Avg_Counts")
  mouse_H2O_df$Mouse <- females[i]
  mouse_H2O_df$Age <- current_mouse_df[current_mouse_df$Mouse == females[i], "Age"][1]
  mouse_H2O_df$Sex <- "F"
  mouse_H2O_df$Fluid <- "H2O"
  mouse_H2O_df$Avg_Counts <- mean(current_mouse_df[current_mouse_df$Fluid == "H2O", "Avg_Counts"])
  
  mouse_df <- rbind(mouse_EtOH_df, mouse_H2O_df)
  
  
  averaged_female_df <- rbind(mouse_df, averaged_female_df)
}

averaged_female_df <- na.omit(averaged_female_df)

averaged_ANOVA_df <- rbind(averaged_male_df, averaged_female_df)

#Redo ANOVA with averaged counts for each mouse

three_way_ANOVA2 <- aov(Avg_Counts ~ Sex * Fluid * Age, averaged_ANOVA_df)
#Results are not significant for any of the factors.
#Not worth continuing. 

dodge2 <- position_dodge(width = 0.5)
all_plot2 <- ggplot(averaged_ANOVA_df, aes(x = Fluid, y = Avg_Counts)) + 
  geom_violin(width = 0.5, aes(fill = Sex), position = dodge) + 
  geom_boxplot(width = 0.1, aes(x = Fluid, y = Avg_Counts, color = Sex), position = dodge) +
  scale_color_manual(values = c("black", "black")) +
  xlab("Fluid Type") + ylab("Total Daily Counts") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + facet_wrap(~Age)


# Correlations for average total ethanol counts for ethanol mice, wk 2 vs wk 2

#wk 1 days 1-7
#wk 2 days 8-14

#MALES

male_EtOH_mice <- unique(sort(EtOH_all[EtOH_all$Sex == "M", "Mouse"]))
index_count <- 1
two_week_corr_df_m <- data.frame(matrix(ncol = 5))
colnames(two_week_corr_df_m) <- c("Mouse", "Age", "Sex", 
                               "Week 1 Average EtOH Counts", 
                               "Week 2 Average EtOH Counts")

for (i in 1:length(male_EtOH_mice)) {
  current_mouse_df <- as.data.frame(matrix(ncol = 5))
  colnames(current_mouse_df) <- c("Mouse", "Age", "Sex", 
                                  "Week 1 Average EtOH Counts", 
                                  "Week 2 Average EtOH Counts")
  all_mouse_lines <- EtOH_all[EtOH_all$Mouse == male_EtOH_mice[index_count],]
  current_mouse_df$Mouse <- male_EtOH_mice[index_count]
  current_mouse_df$Age <- all_mouse_lines[1, "Age"]
  current_mouse_df$Sex <- "M"
  week_1 <- na.omit(all_mouse_lines[all_mouse_lines$Day <= 7,])
  week_2 <- na.omit(all_mouse_lines[all_mouse_lines$Day >= 8,])
  current_mouse_df$"Week 1 Average EtOH Counts" <- mean(week_1$Counts)
  current_mouse_df$"Week 2 Average EtOH Counts" <- mean(week_2$Counts)
  two_week_corr_df_m <- na.omit(rbind(current_mouse_df, two_week_corr_df_m))
  index_count <- index_count + 1
  
}
  

 #FEMALES

female_EtOH_mice <- unique(sort(EtOH_all[EtOH_all$Sex == "F", "Mouse"]))
index_count2 <- 1
two_week_corr_df_f <- data.frame(matrix(ncol = 5))
colnames(two_week_corr_df_f) <- c("Mouse", "Age", "Sex", 
                                  "Week 1 Average EtOH Counts", 
                                  "Week 2 Average EtOH Counts")

for (i in 1:length(female_EtOH_mice)) {
  current_mouse_df <- as.data.frame(matrix(ncol = 5))
  colnames(current_mouse_df) <- c("Mouse", "Age", "Sex", 
                                  "Week 1 Average EtOH Counts", 
                                  "Week 2 Average EtOH Counts")
  all_mouse_lines <- EtOH_all[EtOH_all$Mouse == female_EtOH_mice[index_count2],]
  current_mouse_df$Mouse <- female_EtOH_mice[index_count2]
  current_mouse_df$Age <- all_mouse_lines[1, "Age"]
  current_mouse_df$Sex <- "F"
  week_1 <- na.omit(all_mouse_lines[all_mouse_lines$Day <= 7,])
  week_2 <- na.omit(all_mouse_lines[all_mouse_lines$Day >= 8,])
  current_mouse_df$"Week 1 Average EtOH Counts" <- mean(week_1$Counts)
  current_mouse_df$"Week 2 Average EtOH Counts" <- mean(week_2$Counts)
  two_week_corr_df_f <- na.omit(rbind(current_mouse_df, two_week_corr_df_f))
  index_count2 <- index_count2 + 1
  
}


week2vs1corrplot_m <-  ggplot(two_week_corr_df_m, 
                              aes(x=two_week_corr_df_m[,"Week 1 Average EtOH Counts"], 
                                  y=two_week_corr_df_m[,"Week 2 Average EtOH Counts"])) +
  geom_point(aes(color = Age)) + 
  scale_color_manual(values=c("#3E067D","#F70065","#277f80")) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Male Week 2 vs Week 1 Count Correlations") +
  xlab("Week 1 Average EtOH Counts") +
  ylab("Week 2 Average EtOH Counts") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 20)) +
  stat_cor(method = "pearson", label.y = 2500) +
  stat_regline_equation(label.y = 2500, label.x = 1300)

week2vs1corrplot_f <-  ggplot(two_week_corr_df_f, 
                              aes(x=two_week_corr_df_f[,"Week 1 Average EtOH Counts"], 
                                  y=two_week_corr_df_f[,"Week 2 Average EtOH Counts"])) +
  geom_point(aes(color = Age)) + 
  scale_color_manual(values=c("#3E067D","#F70065","#277f80")) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Female Week 2 vs Week 1 Count Correlations") +
  xlab("Week 1 Average EtOH Counts") +
  ylab("Week 2 Average EtOH Counts") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = 
          element_line(colour = "black"), text = element_text(size = 20)) +
  stat_cor(method = "pearson", label.y = 3500) +
  stat_regline_equation(label.y = 3500, label.x = 1300)

################################################################################
#NONLINEAR REGRESSION
circadian_df <- read.csv("Nonlinear Regression.csv")
circadian_df$SEM <- circadian_df$SD/sqrt(circadian_df$N)
colnames(circadian_df) <- c("Hour", "Age", "Sex", "Mean", "SD", "N", "SEM")
circadian_df$Age <- factor(circadian_df$Age, levels = c("3 wk", "6 wk", "18 wk"))


##FINDING MAX AND MIN Y FOR GEOM SMOOTH
#HELP FROM HERE: https://community.rstudio.com/t/getting-maximum-y-value-from-a-trendline/109848/2
# Nest the data and fit smoothers
dd <- circadian_df[circadian_df$Sex == "M",] %>% 
  group_nest(Age) %>% 
  mutate(
    lm = map(data, ~ lm(Mean ~ poly(Hour,3), .x)),
    lm = map(lm, predict)
  ) %>% 
  unnest(c(lm, data))

# Extract max value for each group
dd_max <- dd %>% 
  group_by(Age) %>% 
  slice_max(lm)

dd_min <- dd %>% 
  group_by(Age) %>% 
  slice_min(lm)

dd_min

nonlin_reg_plot_M <- ggplot(circadian_df[circadian_df$Sex == "M",], 
                            aes(x = Hour, y = Mean, color = Age)) +
  geom_rect(aes(xmin=1, xmax=13, ymin=0, ymax=125, fill=I("darkgrey"), alpha=I(0.1), color=NULL)) +
  scale_color_manual(values = c("#3E067D","#F70065","#277f80")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 1),
        text = element_text(size = 19, color = "black"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black")) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) + 
  ylab("Mean EtOH Counts/60 min") + xlab(element_blank()) +
  scale_x_continuous(expand = c(0,0), breaks = 1:25,
                     labels = c("10:00AM", "11:00AM","12:00PM","1:00PM", 
                                "2:00PM", "3:00PM","4:00PM", "5:00PM", 
                                "6:00PM", "7:00PM", "8:00PM", "9:00PM",
                                "10:00PM", "11:00PM", "12:00AM", "1:00AM", 
                                "2:00AM", "3:00AM", "4:00AM", "5:00AM", 
                                "6:00AM", "7:00AM", "8:00AM", "9:00AM", 
                                "9:59AM")) +
  scale_y_continuous(expand = c(0,0)) +
  geom_point(data = dd_max, aes(y = 120, color = Age), shape = 8, size = 2, stroke = 1.1) +
  geom_point(data = dd_min, aes(y = 120, color = Age), shape = 8, size = 2, stroke = 1.1, position = position_dodge(0.2))


dd1 <- circadian_df[circadian_df$Sex == "F",] %>% 
  group_nest(Age) %>% 
  mutate(
    lm1 = map(data, ~ lm(Mean ~ poly(Hour,3), .x)),
    lm1 = map(lm1, predict)
  ) %>% 
  unnest(c(lm1, data))

# Extract max value for each group
dd_max1 <- dd1 %>% 
  group_by(Age) %>% 
  slice_max(lm1)

dd_min1 <- dd1 %>% 
  group_by(Age) %>% 
  slice_min(lm1)

dd_min1


nonlin_reg_plot_F <- ggplot(circadian_df[circadian_df$Sex == "F",], 
                            aes(x = Hour, y = Mean, color = Age)) +
  geom_rect(aes(xmin=1, xmax=13, ymin=0, ymax=125, fill=I("darkgrey"), alpha=I(0.1), color=NULL)) +
  scale_color_manual(values = c("#3E067D","#F70065","#277f80")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 1),
        text = element_text(size = 19), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black")) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) + 
  ylab("Mean EtOH Counts/60 min") + xlab(element_blank()) +
  scale_x_continuous(expand = c(0,0), breaks = 1:25,
                     labels = c("10:00AM", "11:00AM","12:00PM","1:00PM", 
                                "2:00PM", "3:00PM","4:00PM", "5:00PM", 
                                "6:00PM", "7:00PM", "8:00PM", "9:00PM",
                                "10:00PM", "11:00PM", "12:00AM", "1:00AM", 
                                "2:00AM", "3:00AM", "4:00AM", "5:00AM", 
                                "6:00AM", "7:00AM", "8:00AM", "9:00AM", 
                                "9:59AM")) +
  scale_y_continuous(expand = c(0,0)) +
  geom_point(data = dd_max1, aes(y = 120, color = Age), shape = 8, size = 2, stroke = 1.1, position = position_dodge(width = -1)) +
  geom_point(data = dd_min1, aes(y = 120, color = Age), shape = 8, size = 2, stroke = 1.1, position = position_dodge(width = .2))
  

getnormallegend <- ggplot(circadian_df[circadian_df$Sex == "F",], 
                                               aes(x = Hour, y = Mean, color = Age)) +
  geom_line(size = 1.1) + scale_color_manual(values = c("#3E067D","#F70065","#277f80")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                     panel.background = element_blank(), 
                                                                                     axis.line = element_line(colour = "black", size = 1),
                                                                                     text = element_text(size = 19), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
                                                                                     axis.text.y = element_text(color = "black"))
  
                                                                                     