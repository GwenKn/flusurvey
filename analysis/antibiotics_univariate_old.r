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
library('data.table') # for rbindlist

# locations
home <- "~/Dropbox/Flusurvey/"
plots <- "~/Dropbox/Flusurvey/plots/"
data <- "~/Dropbox/Flusurvey/data/"

### Data 
setwd(data)
# Original data - all people. Not split into bouts of illness
# dt <- extract_data("flusurvey_raw_2010_2017.rds", surveys=c("background", "symptom"))
### Doesn't work? bt <- bouts_of_illness(dt); bt2 <- rbindlist(bt,fill = TRUE)

# Bouts of illness data - use this as 
bt <- readRDS("bouts.rds")

### How many ...
# .. bouts
dim(bt) # 28332
# .. patients
length(unique(bt[,1])) # 3656
# .. with antibiotic info? 
#dt %>% .$medication.antibiotic %>% table # 2830? (3598) entries in original data
bt %>% .$medication.antibiotic %>% table # 1176

### variables
c<-colnames(dt)
grep("education",c,ignore.case=TRUE,value=TRUE) # search for entries of interest
ll <- grep("visit",c,ignore.case=TRUE,value=TRUE) # search for entries of interest

### Edit dt to a table with those with antibiotic info
# %>% = "pipe" = do something with the object beforehand
antibiotics_orig <- dt %>% 
  dplyr::filter(!is.na(medication.antibiotic))

# group by season and agegroup
antibiotics_age_season <- antibiotics_orig%>%
  dplyr::filter(!is.na(agegroup)) %>%
  group_by(agegroup,season) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By season", region = "All",visit="All",living.with.children="All",vaccine.this.year="All", gender = "All",frequent.contact.children = "All") # new variable = all 'by season'

# group by agegroup
antibiotics_age <- antibiotics_orig%>%
  group_by(agegroup) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="Overall",season="Overall", region = "All",visit="All",living.with.children="All",vaccine.this.year="All", gender = "All",frequent.contact.children = "All") # new variable = all 'by season'

# group by region - a lot na? m9999999 = ? 
antibiotics_region <- antibiotics_orig%>%
  dplyr::filter(!is.na(region)) %>%
  dplyr::filter(region!="m99999999") %>%
  group_by(region) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By season",season="Overall", agegroup = "Overall",visit="All",living.with.children="All",vaccine.this.year="All", gender = "All",frequent.contact.children = "All")

# vaccine.this.year (and since start?)
antibiotics_vxthis <- antibiotics_orig%>%
  dplyr::filter(!is.na(vaccine.this.year)) %>%
  group_by(vaccine.this.year) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By season",season="Overall", agegroup = "Overall", region="All",visit="All",living.with.children="All", gender = "All",frequent.contact.children = "All")

## group by contact with medical service - are these the correct variables? 
# how many got antibiotics and we know their contact with Healthcare
table(antibiotics_orig$medication.antibiotic)
antibiotics_get <- antibiotics_orig %>% dplyr::filter(medication.antibiotic!="f") 
tt <- as.data.frame(table(antibiotics_get$visit.medical.service.no))
tt$Freq[4] / (tt$Freq[3] + tt$Freq[4])
# with visit data
antibiotics_get_visit <- antibiotics_get %>% dplyr::filter(visit.medical.service.no!="t") 
antibiotics_get_visit <- antibiotics_get_visit[,c("sid","visit.medical.service.gp","visit.medical.service.hospital","visit.medical.service.other","visit.medical.service.no","visit.medical.service.ae")]
antibiotics_get_visitm <- melt(antibiotics_get_visit,id.vars = "sid",variable_name = "visit")
table(antibiotics_get_visitm[,2:3]) # summary but not of overlapping
# how many with multiple? 
muu <- 0; 
for(i in 1:length(antibiotics_get_visit[,1])){if(length(which(antibiotics_get_visit[i,] == "t"))>1){muu = muu + 1}}
muu  # 337 with more than 1, 39 with more than 2, 5 more than 3  
# exclude or include 1 variable = mult
# summarise
antibiotics_get_visit$mult = 0; antibiotics_get_visit$mult2 = 0;  antibiotics_get_visit$mult3 = 0; antibiotics_get_visit$mult4 = 0;  
for(i in 1:length(antibiotics_get_visit[,1])){
  if(length(which(antibiotics_get_visit[i,] == "t"))>1){
    # if multiple
    antibiotics_get_visit$mult[i] = 1
    if(length(which(antibiotics_get_visit[i,] == "t"))>3){ # if more than 3
      antibiotics_get_visit$mult4[i]=1
    }else{ # if 2 or 3
      if(length(which(antibiotics_get_visit[i,] == "t"))>2){antibiotics_get_visit$mult3[i]=1}
      else{antibiotics_get_visit$mult2[i]=1}
    }
  }
}
mm <- melt(antibiotics_get_visit[which(antibiotics_get_visit$mult==0),1:6],id.vars = "sid",variable_name = "visit")
tt<-table(mm[,2:3])


rowSums(antibiotics_get_visit[,-1])



summarise_all(antibiotics_get_visit,funs(n_distinct(.)))
antibiotics_get_visit%>% group_by %>% tally()

%>%
  dplyr::filter(visit.medical.service.no!="f") %>%
  table

antibiotics_abx_visit <- antibiotics_orig %>% 
  dplyr::filter(!is.na(medication.antibiotic)) %>%
  dplyr::filter(!is.na(agegroup)) 



antibiotics_sub <- antibiotics_orig[,c("medication.antibiotic","visit.medical.service.gp","visit.medical.service.hospital","visit.medical.service.other","visit.medical.service.no","visit.medical.service.ae")]
antibiotics_sub <- melt(antibiotics_sub,id.vars = "medication.antibiotic",variable_name = "visit")
cast(medication.antibiotic ~ visit   , data = antibiotics_sub, fun = length)
antibiotics_visit <- antibiotics_sub %>%
  dplyr::filter(!is.na(visit)) %>%
  group_by(visit) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By season",season="Overall", agegroup = "Overall", region="All",living.with.children="All",vaccine.this.year="All", gender = "All",frequent.contact.children = "All")


antibiotics_sub <- antibiotics_orig[,c("medication.antibiotic","region")]
antibiotics_region <- antibiotics_sub%>%
  dplyr::filter(!is.na(region)) %>%
  dplyr::filter(region!="m99999999") %>%
  group_by(region) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By season",season="Overall", agegroup = "Overall",visit="All",living.with.children="All",vaccine.this.year="All", gender = "All",frequent.contact.children = "All")


# education - entry which is a ranking of how high education you have? 
# main.activity instead => group? 


# "health.score" - link to how bad someone feels?
# 0 = worse. 100 = best. Minimum during episode in bt. If Inf then no entry. 
# baseline (median when no symptoms) as well
# relative (take difference between health and baseline?) or actual? [check any guidelines? scale of health scores] 
# is there a correlation? 

# "ili" = formed from symptoms. 
# yes / no have ili and so could work out if with ili more likely to get abx


# living with children / frequent.contact.children
antibiotics_child <- antibiotics_orig %>%
  group_by(living.with.children) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By season",season="Overall", agegroup = "Overall", region="All", visit="All",vaccine.this.year="All", gender = "All",frequent.contact.children = "All")

# frequent.contact.children (is likely to be regular contact with > 10)
antibiotics_freqchild <- antibiotics_orig %>%
  group_by(frequent.contact.children) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By season",season="Overall", agegroup = "Overall", region="All", visit="All",living.with.children="All",vaccine.this.year="All", gender = "All",frequent.contact.children = "All")


# Gender
antibiotics_gender <- antibiotics_orig%>%
  group_by(gender) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="Overall",season="Overall", region = "All",visit="All",living.with.children="All",vaccine.this.year="All",agegroup="Overall",frequent.contact.children = "All") # new variable = all 'by season'

###*******************************************************
## Bind to plot together
antibiotics1 <- rbind(antibiotics_age,antibiotics_age_season)
antibiotics <- rbind(antibiotics_age,antibiotics_age_season, antibiotics_gender,
                     antibiotics_region,antibiotics_visit,antibiotics_vxthis,antibiotics_child, antibiotics_freqchild)

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
ggsave("age_antibiotic_prescription_rate.pdf", p)

#### Plots

# Region
rr <- antibiotics %>% dplyr::filter(region != "All")
xlabs <- paste(rr$region," (N=",rr$n,")",sep="")
p <- ggplot(rr %>%
              mutate(lower=ifelse(season == "Overall", lower, NA_real_),
                     upper=ifelse(season == "Overall", upper, NA_real_)),
            aes(x=region, y=mean, ymin=lower, ymax=upper,
                color=region)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Prescription rate", label=percent) +
  scale_x_discrete("Region", labels = xlabs) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
setwd(plots)
ggsave("Region_results.pdf")

# Gender
p <- ggplot(antibiotics %>% dplyr::filter(gender != "All") %>%
              mutate(lower=ifelse(season == "Overall", lower, NA_real_),
                     upper=ifelse(season == "Overall", upper, NA_real_)),
            aes(x=gender, y=mean, ymin=lower, ymax=upper,
                color=gender)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Prescription rate", label=percent,lim=c(0,0.02)) +
  scale_x_discrete("Gender") + theme(axis.text.x = element_text(angle = 50, hjust = 1))
p
setwd(plots)
ggsave("Gender_results.pdf")

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
                color=vaccine.this.year)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Prescription rate", label=percent) +
  scale_x_discrete("Vaccine this year") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
setwd(plots)
ggsave("vaccine.pdf")

# Child contact - wrong? 
p <- ggplot(antibiotics %>% dplyr::filter(living.with.children != "All") %>%
              mutate(lower=ifelse(season == "Overall", lower, NA_real_),
                     upper=ifelse(season == "Overall", upper, NA_real_)),
            aes(x=living.with.children, y=mean, ymin=lower, ymax=upper,
                color=living.with.children)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Prescription rate", label=percent) + guides(colour = FALSE)+
  scale_x_discrete("Living with children", labels = c("no", "yes")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
setwd(plots)
ggsave("live_with_children.pdf")

# Freq child contact

p <- ggplot(antibiotics %>% dplyr::filter(vaccine.this.year != "All") %>%
              mutate(lower=ifelse(season == "Overall", lower, NA_real_),
                     upper=ifelse(season == "Overall", upper, NA_real_)),
            aes(x=vaccine.this.year, y=mean, ymin=lower, ymax=upper,
                color=vaccine.this.year)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Prescription rate", label=percent) +
  scale_x_discrete("Vaccine this year") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

p <- ggplot(antibiotics %>% dplyr::filter(frequent.contact.children != "All") %>%
              mutate(lower=ifelse(season == "Overall", lower, NA_real_),
                     upper=ifelse(season == "Overall", upper, NA_real_)),
            aes(x=frequent.contact.children, y=mean, ymin=lower, ymax=upper,
                color=frequent.contact.children)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Prescription rate", label=percent) + guides(colour = FALSE)+
  scale_x_discrete("Frequent contact with children", labels = c("no", "yes")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
setwd(plots)
ggsave("freq_contact_children.pdf")


## Episodes - Seb has code? 
# what proportion of episodes got abx? 