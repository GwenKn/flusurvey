## Code from Seb on antibiotic use 

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
dt <- extract_data("flusurvey_raw_2010_2017.rds", surveys=c("background", "symptom"))

### How many with antibiotic info? 
dt %>% .$medication.antibiotic %>% table # 3598 entries

### variables
c<-colnames(dt)
grep("children",c,ignore.case=TRUE,value=TRUE) # search for entries of interest

### Edit dt to a table with those with antibiotic info
# %>% = "pipe" = do something with the object beforehand
antibiotics_orig <- dt %>% 
  dplyr::filter(!is.na(medication.antibiotic)) %>%
  dplyr::filter(!is.na(agegroup)) 

# group by season and agegroup
antibiotics_age_season <- antibiotics_orig%>%
  group_by(agegroup,season) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By season", region = "All",visit="All",child="All",vaccine.this.year="All", gender = "All") # new variable = all 'by season'

# group by agegroup
antibiotics_age <- antibiotics_orig%>%
  group_by(agegroup) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="Overall",season="Overall", region = "All",visit="All",child="All",vaccine.this.year="All", gender = "All") # new variable = all 'by season'

# group by region - a lot na? m9999999 = ? 
antibiotics_region <- antibiotics_orig%>%
  dplyr::filter(!is.na(region)) %>%
  dplyr::filter(region!="m99999999") %>%
  group_by(region) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By season",season="Overall", agegroup = "Overall",visit="All",child="All",vaccine.this.year="All", gender = "All")

# vaccine.this.year (and since start?)
antibiotics_vxthis <- antibiotics_orig%>%
  dplyr::filter(!is.na(vaccine.this.year)) %>%
  group_by(vaccine.this.year) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By season",season="Overall", agegroup = "Overall", region="All",visit="All",child="All", gender = "All")

# group by contact with medical service - are these the correct variables? 
antibiotics_sub <- antibiotics_orig[,c("medication.antibiotic","visit.medical.service.gp","visit.medical.service.hospital","visit.medical.service.other","visit.medical.service.no","visit.medical.service.ae")]
antibiotics_visit <- melt(antibiotics_sub,id.vars = "medication.antibiotic",variable_name = "visit")%>%
  group_by(visit) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By season",season="Overall", agegroup = "Overall", region="All",child="All",vaccine.this.year="All", gender = "All")

# education - entry which is a ranking of how high education you have? 

# health score - link to how bad someone feels?

# living with children / frequent.contact.children
antibiotics_sub <- antibiotics_orig[,c("medication.antibiotic","living.with.children","frequent.contact.children")]
antibiotics_child <- melt(antibiotics_sub,id.vars = "medication.antibiotic",variable_name = "child") %>%
  group_by(child) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By season",season="Overall", agegroup = "Overall", region="All", visit="All",vaccine.this.year="All", gender = "All")

# Gender
antibiotics_gender <- antibiotics_orig%>%
  group_by(gender) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="Overall",season="Overall", region = "All",visit="All",child="All",vaccine.this.year="All",agegroup="Overall") # new variable = all 'by season'


## Bind to plot together
antibiotics1 <- rbind(antibiotics_age,antibiotics_age_season)
antibiotics <- rbind(antibiotics_age,antibiotics_age_season, antibiotics_gender,
                      antibiotics_region,antibiotics_visit,antibiotics_vxthis,antibiotics_child)

anti_binom <-
  binom.confint(antibiotics$prescribed, antibiotics$n,
                method="wilson")
anti_binom1 <-
  binom.confint(antibiotics1$prescribed, antibiotics1$n,
                method="wilson")


antibiotics %<>%left_join(anti_binom)
antibiotics1 %<>%left_join(anti_binom1)

p <- ggplot(antibiotics1 %>%
            mutate(lower=ifelse(season == "Overall", lower, NA_real_),
                   upper=ifelse(season == "Overall", upper, NA_real_)),
            aes(x=agegroup, y=mean, ymin=lower, ymax=upper,
                             color=season, group=season)) +
  geom_point()+
  geom_errorbar()+
  geom_line()+
  expand_limits(y=0)+
  scale_y_continuous("Prescription rate", label=percent) +
  scale_x_discrete("Age group") +
  facet_wrap(~type) +
  scale_color_brewer(palette="Dark2")
p
setwd(plots)
ggsave("antibiotic_prescription_rate.pdf", p)

#### Plots

# Region
p <- ggplot(antibiotics %>% dplyr::filter(region != "All") %>%
              mutate(lower=ifelse(season == "Overall", lower, NA_real_),
                     upper=ifelse(season == "Overall", upper, NA_real_)),
            aes(x=region, y=mean, ymin=lower, ymax=upper,
                color=region)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Prescription rate", label=percent) +
  scale_x_discrete("Region") + theme(axis.text.x = element_text(angle = 50, hjust = 1))
p

# Gender
p <- ggplot(antibiotics %>% dplyr::filter(gender != "All") %>%
              mutate(lower=ifelse(season == "Overall", lower, NA_real_),
                     upper=ifelse(season == "Overall", upper, NA_real_)),
            aes(x=gender, y=mean, ymin=lower, ymax=upper,
                color=gender)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Prescription rate", label=percent) +
  scale_x_discrete("Gender") + theme(axis.text.x = element_text(angle = 50, hjust = 1))
p

# Visit - wrong categories?
p <- ggplot(antibiotics %>% dplyr::filter(visit != "All") %>%
              mutate(lower=ifelse(season == "Overall", lower, NA_real_),
                     upper=ifelse(season == "Overall", upper, NA_real_)),
            aes(x=visit, y=mean, ymin=lower, ymax=upper,
                color=region)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Prescription rate", label=percent) +
  scale_x_discrete("Visit") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

# Vaccine
p <- ggplot(antibiotics %>% dplyr::filter(vaccine.this.year != "All") %>%
              mutate(lower=ifelse(season == "Overall", lower, NA_real_),
                     upper=ifelse(season == "Overall", upper, NA_real_)),
            aes(x=vaccine.this.year, y=mean, ymin=lower, ymax=upper,
                color=region)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Prescription rate", label=percent) +
  scale_x_discrete("Vaccine this year") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

# Child contact - wrong? 
p <- ggplot(antibiotics %>% dplyr::filter(child != "All") %>%
              mutate(lower=ifelse(season == "Overall", lower, NA_real_),
                     upper=ifelse(season == "Overall", upper, NA_real_)),
            aes(x=child, y=mean, ymin=lower, ymax=upper,
                color=region)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Prescription rate", label=percent) +
  scale_x_discrete("Child contact") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p





## Episodes - Seb has code? 