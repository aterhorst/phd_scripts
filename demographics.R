#####################################################
#                                                   #
#                R script to generate               #
#         rose diagrams showing demographics        #
#               Version 2016-10-01                  #
#                                                   #
#####################################################

# Load requisite libraries.

library(ggplot2)
library(plyr)
library(reshape2)
library(ggthemes)
library(showtext)

# Set working directory.

setwd("~/ownCloud/Innovation Network Analysis/Quantitative Data") # MacBook
setwd("d:/Andrew/ownCloud/Innovation Network Analysis/Quantitative Data") # Home PC
# setwd("c:/Users/ter053/ownCloud/Innovation Network Analysis/Quantitative Data") # work PC


# Load categorical data generated using export_to_MPNet.R.

hf_cat <- na.omit(read.table("Case 1/MPNet Data/categorical_data.txt", sep = "", header = T))
amr_cat <- na.omit(read.table("Case 2/MPNet Data/categorical_data.txt", sep = "", header = T))
gihh_cat <- na.omit(read.table("Case 3/MPNet Data/categorical_data.txt", sep = "", header = T))

# load continuous data generated using export_to_MPNet.R

hf_cont <- na.omit(read.table("Case 1/MPNet Data/continuous_data.txt", sep = "", header = T))
amr_cont <- na.omit(read.table("Case 2/MPNet Data/continuous_data.txt", sep = "", header = T))
gihh_cont <- na.omit(read.table("Case 3/MPNet Data/continuous_data.txt", sep = "", header = T))

# merge continuous data

hf_cont$case <- 1
amr_cont$case <-2
gihh_cont$case <-3

cont <- rbind(hf_cont,amr_cont,gihh_cont)
cont <- cont[,c(1:3,19)]

x <- ddply(cont,"case", summarise, 
           N = length(age),
           mean.age = mean(age),
           mean.experience = mean(work.experience),
           mean.tenure = mean(current.job.tenure),
           sd.age = sd(age),
           sd.experience = sd(work.experience),
           sd.tenure = sd(current.job.tenure),
           range.age = max(age) - min(age),
           range.experience = max(work.experience) - min(work.experience),
           range.tenure = max(current.job.tenure) - min(current.job.tenure))




# compute frequency - education

hf <- count(hf_cont,"education.level")
hf$case <- 1

amr <- count(amr_cont,"education.level")
amr$case <- 2

gihh <- count(gihh_cont,"education.level")
gihh$case <- 3

ed_rose <- rbind(hf,amr,gihh)

# compute frequency - field

hf_f <- count(hf_cat,"broad.education.field")
hf_f$case <- 1

amr_f <- count(amr_cat,"broad.education.field")
amr_f$case <- 2

gihh_f <- count(gihh_cat,"broad.education.field")
gihh_f$case <- 3

field_rose <- rbind(hf_f,amr_f,gihh_f)

# specify labels

case_id <- c("1" = "Case 1", "2" = "Case 2", "3" = "Case 3")

ed_level <- c("Secondary Education","Certificate Level","Diploma/Advanced Diploma",
              "Bachelors Degree","Graduate Certificate/Diploma", 
              "Masters Degree","Doctoral Degree")

ed_field <- c("Natural & Physical Sciences", "Information Technology", "Engineering & Related Technologies",
              "Architecture & Building", "Agricultural, Environmental & Related Studies",
              "Health","Education", "Management & Commerce", "Society & Culture", "Creative Arts",
              "Food, Hospitality & Personal Services", "Mixed Field Programmes")

# rose diagrams

# Plot 1


svg(file = "ed_level_rose.svg", width = 6, height = 4)

ggplot(ed_rose, aes(factor(education.level), freq, fill = factor(education.level))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") + 
  geom_text(aes(label=freq), nudge_y = 0.7, size = 3) +
  coord_polar() +
  scale_y_continuous(trans = "sqrt", breaks = c(1,4,9,16,25,36)) +  
  theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank()
        # 
        # legend.text=element_text(size=8),
        # legend.text = element_text(size = 8),
        # legend.title = element_text(size = 10)
  ) +
  facet_wrap(~ case, labeller = as_labeller(case_id)) +
  guides(fill=guide_legend(ncol=2)) +
  scale_fill_discrete(name="EDUCATION LEVEL",
                      breaks=c(2:8),
                      labels= ed_level)
dev.off()

# Plot 2

svg(file = "ed_field_rose.svg", width = 6, height = 5)

ggplot(field_rose, aes(factor(broad.education.field), freq, fill = factor(broad.education.field))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") + 
  geom_text(aes(label=freq), nudge_y = 1, size = 3) +
  coord_polar() +
  scale_y_continuous(breaks = c(4,8,12,16)) +
  theme_fivethirtyeight() +
  # theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank()
        # legend.text = element_text(size = 8),
        # legend.title = element_text(size = 10)
  ) +
  facet_wrap(~ case, labeller = as_labeller(case_id)) +
  guides(fill=guide_legend(ncol=2)) +
  scale_fill_discrete(name="EDUCATION FIELD",
                      breaks=c(1:12),
                      labels= ed_field)

dev.off()

# Plot 3.

svg(file = "age_experience.svg", width = 6, height = 6)

cont$case <- as.factor(cont$case)

dat.m <- melt(cont,id.vars = 'case', measure.vars=c('age','work.experience','current.job.tenure'))

p <- ggplot(dat.m) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(size = 10), 
        strip.text.x = element_text(size=12)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  ylab('Years') +
  geom_boxplot(aes(x=factor(variable), y = value, fill = variable), width = 0.35, outlier.colour = NA) +
  geom_jitter(alpha=0.2, aes(x=factor(variable), y = value), color= "black",position = position_jitter(width = .2))+
  facet_wrap(~case, labeller = as_labeller(case_id)) +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70)) +
  scale_x_discrete(name = "", labels = c("Age", "Work \nExperience", "Job \nTenure")) +
  theme(legend.position = "none")
p                      

dev.off()