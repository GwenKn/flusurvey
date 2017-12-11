# Analysis plan (data and plots)

### Admin
# libraries
library('flusurvey')
library('ggplot2')
library('cowplot')
library('binom')
library('magrittr')
library("lubridate")
library("dplyr")
library('scales')
library('reshape')

# locations
home <- "~/Dropbox/Flusurvey/"
plots <- "~/Dropbox/Flusurvey/plots/"
data <- "~/Dropbox/Flusurvey/data/"

### Data 
setwd(data)
bt <- readRDS("bouts.rds") #dt <- extract_data("flusurvey_raw_2010_2017.rds", surveys=c("background", "symptom"))

### How many were prescribed antibiotics? 
dt %>% .$medication.antibiotic %>% table # 3598 entries

### Edit dt to a table with those with antibiotic info
# %>% = "pipe" = do something with the object beforehand
antibiotics_orig <- dt %>% 
  dplyr::filter(!is.na(medication.antibiotic)) %>%
  dplyr::filter(!is.na(agegroup)) 

# Have similar number by year?
antibiotics_season <- antibiotics_orig%>%
  group_by(season) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n())
anti_binom <-binom.confint(antibiotics_season$prescribed, antibiotics_season$n,method="wilson")
antibiotics_season %<>%left_join(anti_binom)

g<-ggplot(antibiotics_season,aes(x=season, y=n,color=season))+geom_point()+
  scale_y_continuous("Number of records", limits=c(0,max(1000+antibiotics_season$n)))+guides(color=FALSE)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

h<-ggplot(antibiotics_season,aes(x=season, y=prescribed,color=season))+geom_point()+
  scale_y_continuous("Number of prescriptions", limits=c(0,max(100+antibiotics_season$prescribed)))+guides(color=FALSE)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

j<-ggplot(antibiotics_season,aes(x=season, y=mean, ymin=lower, ymax=upper,color=season))+geom_point()+geom_errorbar()+
  expand_limits(y=0)+scale_y_continuous("Prescription rate", label=percent,limits=c(0,0.03)) +guides(color=FALSE)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggdraw() + draw_plot(g, 0, 0, 0.3, 1) + draw_plot(h, 0.33, 0, 0.3, 1) + draw_plot(j, 0.66, 0, 0.3, 1) 
setwd(plots)
ggsave("compare_seasons.pdf")

# What is the rate for individual patients? 


