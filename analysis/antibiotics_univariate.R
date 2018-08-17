#### Univariate analysis
# Gives plots for supplementary file
# Based on code from Seb on antibiotic use 

### Admin
# libraries
library('flusurvey')
library('ggplot2')
library('cowplot')
library('binom')
library('magrittr')
library("lubridate")
library("plyr")
library("dplyr")
library('scales')
library('reshape')
library('data.table') # for rbindlist
theme_set(theme_bw(base_size=24))

# locations
home <- "~/Documents/Flusurvey/"
plots <- "~/Documents/Flusurvey/plots/"
data <- "~/Documents/Flusurvey/data/"

### Data 
setwd(data)
# Original data - all people. Not split into bouts of illness
# dt <- extract_data("flusurvey_raw_2010_2017.rds", surveys=c("background", "symptom"))
### Doesn't work? bt <- bouts_of_illness(dt); bt2 <- rbindlist(bt,fill = TRUE)

# Bouts of illness data - use this as
#bt <- readRDS("bouts_20180130.rds")
bt <- readRDS("bouts_20180209.rds") %>% filter(no.symptoms=="f")
nrow(bt) # 28332?

#bt <- readRDS("bouts_20180130.rds")
# bt <- readRDS("bouts.rds")


###******** CLEANING - if keep in multivariate then need to have all the data ********#######
# visit: Only keep those who have ask that they clicked one box for the visit question
btt <- bt %>% dplyr::filter(visit.medical.service.no == "t" | visit.medical.service.gp == "t" | visit.medical.service.hospital == "t" | visit.medical.service.ae == "t" | visit.medical.service.other == "t" | visit.medical.service.appointment == "t")
#       removes 380 now 338

# health score: 
btt %<>% mutate(min.health.score = if_else(is.finite(min.health.score), min.health.score, NA_integer_))
#       Makes NA if didn't input a health score
w<-unique(c(which(btt$min.health.score > 100),which(btt$health.score > 100),which(btt$baseline.health.score > 100)))
btt <- btt[-w,]
length(w) #       removes 13 now 15
# vaccine.this.year: 
w<-which(btt$vaccine.this.year == "dont_know")
btt <- btt[-w,] 
length(w) # removes 46 
# ili.fever 
w<-which(is.na(btt$ili.fever))
btt <- btt[-w,] 
length(w) # removes 30 now 101
# age
w<-which(is.na(btt$age))
btt <- btt[-w,] 
length(w) # removes 30 now 277...
# Islands
w<-c(which(btt$region == "channel_islands"), which(btt$region == "isle_of_man"))
btt <- btt[-w,] 
risl <- length(w) # 41?
# Save it
saveRDS(btt, "btt_abx.rds")

###***** How many ... ********#######
# .. bouts
dim(btt) # 27874 bigger now 36654
btt$id = seq(1,dim(btt)[1],1) # add in bt id
# .. participants
length(unique(btt[,"participant_id"])) # 3654  now 3664 
# .. episodes per participant
h<-hist(btt$participant_id,breaks = seq(1,3700,1))
max(h$counts)
min(h$counts)
mean(h$counts)
var(h$counts)
# .. with antibiotic info? 
#dt %>% .$medication.antibiotic %>% table # 2830? (3598) entries in original data
btt %>% .$medication.antibiotic %>% table # 1163 now 1897
1897 / 34757 # 1163/27946 # 5.45% 

### variables
c<-colnames(btt)
grep("smoke",c,ignore.case=TRUE,value=TRUE) # search for entries of interest
ll <- grep("smoke",c,ignore.case=TRUE,value=TRUE) # search for entries of interest

### Edit dt to a table with those with antibiotic info
# %>% = "pipe" = do something with the object beforehand
antibiotics_orig <- btt #%>% 
  #dplyr::filter(!is.na(medication.antibiotic)) # NO NA IN ANYMOREactually non-NA in bt

# Have similar number by year?
antibiotics_season <- antibiotics_orig%>%
  group_by(season) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n())
anti_binom <-binom.confint(antibiotics_season$prescribed, antibiotics_season$n,method="wilson")
antibiotics_season %<>%left_join(anti_binom)

transform(antibiotics_season, season = as.numeric(season))

g<-ggplot(antibiotics_season,aes(x=season, y=n,color=factor(season)))+geom_point(size=2)+scale_x_discrete("Season")+
  scale_y_continuous("Number of episodes", limits=c(0,max(1000+antibiotics_season$n)))+guides(color=FALSE)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

h<-ggplot(antibiotics_season,aes(x=season, y=prescribed,color=factor(season)))+geom_point(size=2)+scale_x_discrete("Season")+
  scale_y_continuous("Number of prescriptions", limits=c(0,max(100+antibiotics_season$prescribed)))+guides(color=FALSE)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

j<-ggplot(antibiotics_season,aes(x=season, y=mean, ymin=lower, ymax=upper,color=factor(season)))+geom_point(size=2)+geom_errorbar()+scale_x_discrete("Season")+
  expand_limits(y=0)+scale_y_continuous("Antibiotic usage rate", label=percent) +guides(color=FALSE)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggdraw() + draw_plot(g, 0, 0, 0.3, 1) + draw_plot(h, 0.33, 0, 0.3, 1) + draw_plot(j, 0.66, 0, 0.3, 1) 
setwd(plots)
ggsave("compare_seasons.pdf",width = 12, height = 5)

## MEAN
mean(antibiotics_season$mean)
mm <- mean(sum(antibiotics_season$prescribed)/sum(antibiotics_season$n))
#** Do some participants always take antibiotics? Is the Antibiotic usage rate per episode higher for some people? 
antibiotics_participant <- antibiotics_orig%>%
  group_by(participant_id) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n(), rate = prescribed / n)

ggplot(antibiotics_participant, aes(x=n, y=rate)) + geom_point() + scale_x_continuous("Number of prescriptions") +
  scale_y_continuous("Antibiotic usage rate\n(number of prescriptions\nper episode per person)")
ggsave("compare_participants.pdf",width = 12, height = 5)

antibiotics_participants_rates <- antibiotics_participant%>%
  group_by(prescribed) %>%
  summarise(nparticipants=length(unique(participant_id)), n=n())
antibiotics_participants_rates$total_prescrip = antibiotics_participants_rates$prescribed*antibiotics_participants_rates$n
# 2 people contribute 8 and 9 prescriptions
# 37% (711/1897) of prescriptions are from a single prescription per person 

#** What is the rate of prescribing by what a person thinks they have? 
# don't think have answers to this question (What do you think is causing your symptoms?) in btt
antibiotics_think <- antibiotics_orig%>%
  group_by(what.do.you.think) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n(), rate = prescribed / n)
anti_binom <-binom.confint(antibiotics_think$prescribed, antibiotics_think$n,method="wilson")
antibiotics_think %<>%left_join(anti_binom)

ggplot(antibiotics_think,aes(x=what.do.you.think, y=mean, ymin=lower, ymax=upper,color=factor(what.do.you.think)))+geom_point(size=2)+geom_errorbar()+scale_x_discrete("Season")+
  expand_limits(y=0)+scale_y_continuous("Antibiotic usage rate", label=percent) +guides(color=FALSE)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

antibiotics_think$nlabs <- paste(antibiotics_think$what.do.you.think," \n(N=",antibiotics_think$n,")",sep="")

ggplot(antibiotics_think,aes(x=nlabs, y=mean, ymin=lower, ymax=upper,color=factor(nlabs)))+geom_point(size=2)+geom_errorbar()+
  scale_x_discrete("'What do you think you have?'") + geom_hline(yintercept = mean(sum(antibiotics_season$prescribed)/sum(antibiotics_season$n)),lty="dashed")+
  expand_limits(y=0)+scale_y_continuous("Antibiotic usage rate", label=percent) +guides(color=FALSE)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("compare_bywhatyouthink.pdf",width = 12, height = 5)

#** Is there a pattern by region? 
antibiotics_region_cases <- antibiotics_orig%>%
  dplyr::filter(!is.na(region)) %>%
  dplyr::filter(region!="m99999999") %>%
  group_by(region)  %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n(), rate = prescribed / n, 
            pat_no = length(unique(participant_id)), case_rate = n / pat_no / 100)

anti_binom <-binom.confint(antibiotics_region_cases$prescribed, antibiotics_region_cases$n,method="wilson")
antibiotics_region_cases %<>%left_join(anti_binom)
ggplot(antibiotics_region_cases,aes(x=region, y=mean, ymin=lower, ymax=upper,color=factor(region)))+geom_point(size=2)+geom_errorbar()+scale_x_discrete("Region")+
  expand_limits(y=0)+scale_y_continuous("Antibiotic usage rate", label=percent) +guides(color=FALSE)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_point(aes(y = case_rate),shape = 4)

ggplot(antibiotics_region_cases,aes(x=mean, y = case_rate)) + geom_point()
### Doesn't seem to be a correlation between more episodes per participant and antibiotic usage

################### *** Data grouping for univariate analysis ######
###*** group by season and agegroup
antibiotics_age_season <- antibiotics_orig%>%
  dplyr::filter(!is.na(agegroup)) %>%
  group_by(agegroup,season) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By season&age", region = "All", vaccine.this.year="All", gender = "All",frequent.contact.children = "All", highest.education = "All", ili = "All",which.visit="All", ili.fever = "All",main.activity = "All", norisk = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All") # new variable = all 'by season'

###*** group by agegroup
antibiotics_age <- antibiotics_orig%>%
  group_by(agegroup) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By agegroup",season="Overall", region = "All", vaccine.this.year="All", gender = "All",frequent.contact.children = "All", highest.education = "All", ili = "All",which.visit="All", ili.fever = "All",main.activity = "All", norisk = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All") # new variable = all 'by season'

###*** group by region - a lot na? m9999999 = ? 
antibiotics_region <- antibiotics_orig%>%
  dplyr::filter(!is.na(region)) %>%
  dplyr::filter(region!="m99999999") %>%
  group_by(region) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By region",season="Overall", agegroup = "Overall", vaccine.this.year="All", gender = "All",frequent.contact.children = "All", highest.education = "All", ili = "All",which.visit="All", ili.fever = "All",main.activity = "All", norisk = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All")

###*** vaccine.this.year (and since start?)
antibiotics_vxthis <- antibiotics_orig%>%
  dplyr::filter(!is.na(vaccine.this.year)) %>%
  group_by(vaccine.this.year) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By vx status",season="Overall", agegroup = "Overall", region="All",  gender = "All",frequent.contact.children = "All", highest.education = "All", ili = "All",which.visit="All", ili.fever = "All",main.activity = "All", norisk = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All")

###*** group by vaccine and agegroup
antibiotics_vxthis_age <- antibiotics_orig%>%
  dplyr::filter(!is.na(agegroup)) %>%
  group_by(agegroup,vaccine.this.year) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By vx&age", season="Overall", region = "All", gender = "All",frequent.contact.children = "All", highest.education = "All", ili = "All",which.visit="All", ili.fever = "All",main.activity = "All", norisk = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All") # new variable = all 'by season'

###*** group by contact with medical service - are these the correct variables? 
# with visit data
# cleaned so that they clicked one box for this question
antibiotics_visit <- antibiotics_orig
## how many with multiple? # not just those with antibiotic prescriptions
antibiotics_visit <- antibiotics_visit[,c("id","visit.medical.service.gp","visit.medical.service.hospital",
                                          "visit.medical.service.other","visit.medical.service.ae","visit.medical.service.appointment")]
# to count
#muu <- 0;
#for(i in 1:length(antibiotics_visit[,1])){if(length(which(antibiotics_visit[i,] == "t"))>2){muu = muu + 1; print(c(i,length(which(antibiotics_visit[i,] == "t")),which(antibiotics_visit[i,] == "t")))}}#if(antibiotics_visit[i,6] == "t"){print(c(i,length(which(antibiotics_visit[i,] == "t")),which(antibiotics_visit[i,] == "t")))}}}
#muu # of all: 2342 with at least 1, 144 with more than 1, 14 with more than 2, 0 more than 3
# of those that get antibiotics: 77 with more than 1, 7 with more than 2, 0 more than 3  
# exclude? or include 1 variable = mult
# summarise
mult = matrix(0,1,length(antibiotics_visit[,1])) #antibiotics_get_visit$mult2 = 0;  antibiotics_get_visit$mult3 = 0; antibiotics_get_visit$mult4 = 0;  
for(i in 1:length(antibiotics_visit[,1])){
  if(length(which(antibiotics_visit[i,] == "t"))>1){ # change to 1 or 2
    # if multiple
    mult[i] <- 1 # change to 1 or 2
    #print(i)
  } 
  if(length(which(antibiotics_visit[i,] == "t"))>2){ # change to 1 or 2
    # if multiple
    mult[i] <- 2 # change to 1 or 2
    #print(i)
  } 
}

# New variable to represent who visit
antibiotics_orig$which.visit <- 0 # only want those with 1 visit first
w1<-intersect(which(mult==0),which(antibiotics_orig[,"visit.medical.service.gp"]=="t"))
w2<-intersect(which(mult==0),which(antibiotics_orig[,"visit.medical.service.hospital"]=="t"))
w3<-intersect(which(mult==0),which(antibiotics_orig[,"visit.medical.service.ae"]=="t"))
w4<-intersect(which(mult==0),which(antibiotics_orig[,"visit.medical.service.other"]=="t"))
w5<-intersect(which(mult==0),which(antibiotics_orig[,"visit.medical.service.appointment"]=="t"))
antibiotics_orig[w1,"which.visit"] = 1 # gp
antibiotics_orig[w2,"which.visit"] = 2 # hosp
antibiotics_orig[w3,"which.visit"] = 3 # ae
antibiotics_orig[w4,"which.visit"] = 4 # other
antibiotics_orig[w5,"which.visit"] = 5 # appointment
# multiple
w6<-intersect(intersect(which(mult==1),which(antibiotics_orig[,"visit.medical.service.gp"]=="t")),which(antibiotics_orig[,"visit.medical.service.hospital"]=="t"))
w7<-intersect(intersect(which(mult==1),which(antibiotics_orig[,"visit.medical.service.gp"]=="t")),    which(antibiotics_orig[,"visit.medical.service.ae"]=="t"))
w8<-intersect(intersect(which(mult==1),which(antibiotics_orig[,"visit.medical.service.gp"]=="t")),    which(antibiotics_orig[,"visit.medical.service.other"]=="t"))
w9<-intersect(intersect(which(mult==1),which(antibiotics_orig[,"visit.medical.service.hospital"]=="t")),    which(antibiotics_orig[,"visit.medical.service.ae"]=="t"))
w10<-intersect(intersect(which(mult==1),which(antibiotics_orig[,"visit.medical.service.hospital"]=="t")),    which(antibiotics_orig[,"visit.medical.service.other"]=="t"))
antibiotics_orig[w6,"which.visit"] = 6 # gp + hops
antibiotics_orig[w7,"which.visit"] = 7 # gp + ae
antibiotics_orig[w8,"which.visit"] = 8 # gp + other
antibiotics_orig[w9,"which.visit"] = 9 # hosp + ae
antibiotics_orig[w10,"which.visit"] = 10 # hosp + other

w11<-intersect(intersect(intersect(which(mult==2),which(antibiotics_orig[,"visit.medical.service.gp"]=="t")),which(antibiotics_orig[,"visit.medical.service.hospital"]=="t")), which(antibiotics_orig[,"visit.medical.service.ae"]=="t"))
w12<-intersect(intersect(intersect(which(mult==2),which(antibiotics_orig[,"visit.medical.service.gp"]=="t")),which(antibiotics_orig[,"visit.medical.service.hospital"]=="t")), which(antibiotics_orig[,"visit.medical.service.other"]=="t"))
w13<-intersect(intersect(intersect(which(mult==2),which(antibiotics_orig[,"visit.medical.service.hospital"]=="t")),which(antibiotics_orig[,"visit.medical.service.ae"]=="t")),    which(antibiotics_orig[,"visit.medical.service.other"]=="t"))
w14<-intersect(intersect(intersect(which(mult==2),which(antibiotics_orig[,"visit.medical.service.gp"]=="t")),which(antibiotics_orig[,"visit.medical.service.other"]=="t")), which(antibiotics_orig[,"visit.medical.service.ae"]=="t"))
antibiotics_orig[w11,"which.visit"] = 11 # gp + hospital + ae
antibiotics_orig[w12,"which.visit"] = 12 # gp + hospital + other
antibiotics_orig[w13,"which.visit"] = 13 # hospital + ae + other
antibiotics_orig[w14,"which.visit"] = 14 # gp + other + ae

length(which(antibiotics_orig$which.visit>0)) # should be 2342

### group by visit
antibiotics_visit <- antibiotics_orig%>%
  group_by(which.visit) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By visit", season="Overall",agegroup = "Overall",region = "All",vaccine.this.year="All", gender = "All",frequent.contact.children = "All", highest.education = "All", ili = "All", ili.fever = "All",main.activity = "All", norisk = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All") # new variable = all 'by season'

### group by visit yn
antibiotics_visityn <- antibiotics_orig%>%
  group_by(visit.medical.service.no) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By visityn", season="Overall",agegroup = "Overall",region = "All",vaccine.this.year="All", gender = "All",frequent.contact.children = "All", highest.education = "All", ili = "All", ili.fever = "All",main.activity = "All", norisk = "All",which.visit = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All") # new variable = all 'by season'

### group by visit yn & age
antibiotics_visityn_age <- antibiotics_orig%>%
  group_by(visit.medical.service.no,agegroup) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By visityn", season="Overall",region = "All",vaccine.this.year="All", gender = "All",frequent.contact.children = "All", highest.education = "All", ili = "All", ili.fever = "All",main.activity = "All", norisk = "All",which.visit = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All") # new variable = all 'by season'

###*** education - entry which is a ranking of how high education you have? 
length(which(antibiotics_orig$highest.education!="NA")) # 8160 have info = 28.8%
antibiotics_h.edu <- antibiotics_orig %>%
  group_by(highest.education) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By education",season="Overall", agegroup = "Overall", region="All", vaccine.this.year="All", gender = "All",frequent.contact.children = "All",  ili = "All",which.visit="All", ili.fever = "All",main.activity = "All", norisk = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All")

###*** main.activity 
antibiotics_main_activity <- antibiotics_orig %>%
  group_by(main.activity) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By main activity",season="Overall", agegroup = "Overall", region="All", vaccine.this.year="All", gender = "All",frequent.contact.children = "All", highest.education = "All", ili = "All",which.visit="All", ili.fever = "All", norisk = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All")

###*** "ili" = formed from symptoms. 
# yes / no have ili and so could work out if with ili more likely to get abx
antibiotics_ili <- antibiotics_orig %>%
  group_by(ili) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By ili",season="Overall", agegroup = "Overall", region="All",  vaccine.this.year="All", gender = "All",frequent.contact.children = "All", highest.education = "All",which.visit = "All", ili.fever = "All",main.activity = "All", norisk = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All")

# ili.fever better tracks flu season
antibiotics_ili_fever <- antibiotics_orig %>%
  group_by(ili.fever) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By ili.fever",season="Overall", agegroup = "Overall", region="All",  vaccine.this.year="All", gender = "All",frequent.contact.children = "All", highest.education = "All",ili = "All",which.visit = "All",main.activity = "All", norisk = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All")

###*** frequent.contact.children (is likely to be regular contact with > 10)
antibiotics_freqchild <- antibiotics_orig %>%
  group_by(frequent.contact.children) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By freq contact children",season="Overall", agegroup = "Overall", region="All",  vaccine.this.year="All", gender = "All", highest.education = "All", ili = "All",which.visit="All", ili.fever = "All",main.activity = "All", norisk = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All")

###*** Gender
antibiotics_gender <- antibiotics_orig%>%
  group_by(gender) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By gender",season="Overall", region = "All", vaccine.this.year="All",agegroup="Overall",frequent.contact.children = "All", highest.education = "All", ili = "All",which.visit="All", ili.fever = "All",main.activity = "All", norisk = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All") # new variable = all 'by season'

###*** group by gender and agegroup
antibiotics_gender_age <- antibiotics_orig%>%
  dplyr::filter(!is.na(agegroup)) %>%
  group_by(agegroup,gender) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By gender&age", season="Overall", region = "All", vaccine.this.year="All",frequent.contact.children = "All", highest.education = "All", ili = "All",which.visit="All", ili.fever = "All",main.activity = "All", norisk = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All") # new variable = all 'by season'
###*** health score groupings
# cut by normalised / baseline / mean for this bout
# normalised - ignore? bad to divide
antibiotics_hs <- antibiotics_orig 
antibiotics_hs$hs <- (antibiotics_orig$min.health.score - antibiotics_orig$baseline.health.score)/antibiotics_orig$baseline.health.score
#antibiotics_hs$hs <- (antibiotics_orig$min.health.score - antibiotics_orig$baseline.health.score)
antibiotics_hs <- antibiotics_hs %>% dplyr::filter(!is.na(hs)) # remove any with NA = 12657! 
antibiotics_hs <- antibiotics_hs %>% dplyr::filter(!is.infinite(hs)) # remove any INF = 2... 
antibiotics_hs <- antibiotics_hs %>% dplyr::filter(!(hs > 0)) # remove any greater than 0...?

antibiotics_hs$cut.hs <- cut(antibiotics_hs$hs, breaks = 5)
antibiotics_hs$cut.hs_base <- cut(antibiotics_hs$baseline.health.score, breaks = 5)
antibiotics_hs$cut.hs_score <- cut(antibiotics_hs$min.health.score, breaks = 5)
antibiotics_hs1 <- antibiotics_hs%>%
  group_by(cut.hs) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By risk",season="Overall", agegroup = "Overall", region="All", vaccine.this.year="All", gender = "All",agegroup="Overall",frequent.contact.children = "All", highest.education = "All", ili = "All",which.visit="All", ili.fever = "All",main.activity = "All",visit.medical.service.no = "All",norisk="All",cut.hs_score = "All", cut.hs_base = "All")

antibiotics_hs2 <- antibiotics_hs%>%
  group_by(cut.hs_base) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By risk",season="Overall", agegroup = "Overall", region="All", vaccine.this.year="All", gender = "All",agegroup="Overall",frequent.contact.children = "All", highest.education = "All", ili = "All",which.visit="All", ili.fever = "All",main.activity = "All",visit.medical.service.no = "All",norisk="All",cut.hs="All",cut.hs_score = "All")

antibiotics_hs3 <- antibiotics_hs%>%
  group_by(cut.hs_score) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By risk",season="Overall", agegroup = "Overall", region="All", vaccine.this.year="All", gender = "All",agegroup="Overall",frequent.contact.children = "All", highest.education = "All", ili = "All",which.visit="All", ili.fever = "All",main.activity = "All",visit.medical.service.no = "All",norisk="All",cut.hs="All",cut.hs_base = "All")


###*** risk - underlying health condition
antibiotics_risk <- antibiotics_orig%>%
  group_by(norisk) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By risk",season="Overall", agegroup = "Overall", region="All", vaccine.this.year="All", gender = "All",agegroup="Overall",frequent.contact.children = "All", highest.education = "All", ili = "All",which.visit="All", ili.fever = "All",main.activity = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All")

###*** smoke TO DO
antibiotics_smoke <- antibiotics_orig%>%
  group_by(smoke) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By risk",season="Overall", agegroup = "Overall", region="All", vaccine.this.year="All", gender = "All",agegroup="Overall",frequent.contact.children = "All", highest.education = "All", ili = "All",which.visit="All", ili.fever = "All",main.activity = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All")

###*** SYMPTOMS TO DO 
# HOW PICK FROM... all separate column names MELT? 
antibiotics_symp <- antibiotics_orig[,c("medication.antibiotic","no.symptoms","fever","chills","blocked.runny.nose","sneezing",
"sore.throat","cough","shortness.breath","headache","muscle.and.or.joint.pain","chest.pain",
"tired","loss.appetite","phlegm","eye.irritation","nausea","vomiting","diarrhoea","stomach.ache","other.symptoms")]
antibiotics_symp <- melt(antibiotics_symp,id.vars = "medication.antibiotic")
antibiotics_symp %>%
  group_by(value) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup

antibiotics_smoke <- antibiotics_orig%>%
  group_by(smoke) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  mutate(type="By risk",season="Overall", agegroup = "Overall", region="All", vaccine.this.year="All", gender = "All",agegroup="Overall",frequent.contact.children = "All", highest.education = "All", ili = "All",which.visit="All", ili.fever = "All",main.activity = "All",visit.medical.service.no = "All",cut.hs="All",cut.hs_score = "All", cut.hs_base = "All")




###***************************************************************************************************************************
## Bind to plot together
antibiotics1 <- rbind(antibiotics_age,antibiotics_age_season)
antibiotics <- rbind(antibiotics_age_season, antibiotics_age, antibiotics_region,
                     antibiotics_vxthis,antibiotics_vxthis_age,
                     antibiotics_h.edu,antibiotics_main_activity,
                     antibiotics_visit,antibiotics_visityn,
                     antibiotics_ili,antibiotics_ili_fever,
                     antibiotics_freqchild,
                     antibiotics_gender,antibiotics_gender_age,
                     antibiotics_hs1,antibiotics_hs2,antibiotics_hs3, antibiotics_risk)
anti_binom  <- binom.confint(antibiotics$prescribed, antibiotics$n,method="wilson")
anti_binom1 <- binom.confint(antibiotics1$prescribed, antibiotics1$n,method="wilson")

antibiotics  <- cbind(antibiotics, anti_binom[,c("mean","lower","upper")])
#antibiotics %<>%left_join(anti_binom)
antibiotics1 <- cbind(antibiotics1, anti_binom1[,c("mean","lower","upper")])

setwd(data)
saveRDS(antibiotics, "antibiotics.rds")
saveRDS(antibiotics1, "antibiotics1.rds")

#### Plots ***************************************************************************************************************************
## Antibiotics 1
# Age and season
p <- ggplot(antibiotics1 %>%
              mutate(lower=ifelse(season == "Overall", lower, NA_real_), # mutate is remove the error_bar out of the each season by age plot
                     upper=ifelse(season == "Overall", upper, NA_real_)),
            aes(x=agegroup, y=mean, ymin=lower, ymax=upper,
                color=season, group=season)) +
  geom_point()+
  geom_errorbar()+
  geom_line()+
  expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) +
  scale_x_discrete("Age group") +
  facet_wrap(~type) +
  scale_color_brewer(palette="Dark2")
p
setwd(plots)
ggsave("age_season_antibiotic_prescription_rate.pdf", p, width = 12, height = 5)

# Age and visit
anti_binom  <- binom.confint(antibiotics_visityn_age$prescribed, antibiotics_visityn_age$n,method="wilson")
antibiotics_visityn_age1  <- cbind(antibiotics_visityn_age, anti_binom[,c("mean","lower","upper")])
#
p <- ggplot(antibiotics_visityn_age1 %>%
              mutate(lower=ifelse(season == "Overall", lower, NA_real_), # mutate is remove the error_bar out of the each season by age plot
                     upper=ifelse(season == "Overall", upper, NA_real_)),
            aes(x=agegroup, y=mean, ymin=lower, ymax=upper,
                color=visit.medical.service.no, group=visit.medical.service.no)) +
  geom_point()+
  geom_errorbar()+
  geom_line()+
  expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) +
  scale_x_discrete("Age group") +
  facet_wrap(~type) +
  scale_color_brewer(palette="Dark2")
p
setwd(plots)
ggsave("age_visit_antibiotic_prescription_rate.pdf", p, width = 12, height = 5)

#### Plots ***************************************************************************************************************************
### Region
rr <- antibiotics %>% dplyr::filter(region != "All")
xlabs <- paste(rr$region," (N=",rr$n,")",sep="")
rr2<-rr
rr2$region <- xlabs
p <- ggplot(rr2,aes(x=region, y=mean, ymin=lower, ymax=upper,color=region)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) + ggtitle("By region") + 
  scale_x_discrete("Region") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
setwd(plots)

ggsave("Region_results.pdf",width = 12, height = 10)

w<-which(rr2$n < 100)
p <- ggplot(rr2[-w,],aes(x=region, y=mean, ymin=lower, ymax=upper,color=region)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) + ggtitle("By region (N > 100)")+
  scale_x_discrete("Region") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+guides(colour=FALSE)
p
setwd(plots)
ggsave("Region_results_(bigcount).pdf",width = 12, height = 10)

p <- ggplot(rr2[-w,],aes(x=region, y=mean, ymin=lower, ymax=upper,color=region)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) +
  scale_x_discrete("Region") + theme(axis.text.x = element_text(angle = 60, hjust = 1))+guides(colour=FALSE)
p
setwd(plots)
ggsave("Region_results_(bigcount_nolabel).pdf",width = 15, height = 10)

### Gender
p <- ggplot(antibiotics %>% dplyr::filter(type == "By gender"),
            aes(x=gender, y=mean, ymin=lower, ymax=upper,color=factor(gender))) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) +
  scale_x_discrete("Gender",labels = c("Female","Male")) +guides(colour=FALSE)
p
ggsave("Gender_results.pdf",width = 12, height = 5)

p <- ggplot(antibiotics %>% dplyr::filter(type == "By gender&age"),
            aes(x=agegroup, y=mean, ymin=lower, ymax=upper,color=factor(gender))) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) +
  scale_x_discrete("Age") + scale_color_discrete("Gender")
p
ggsave("Gender_age_results.pdf",width = 12, height = 5)

### Visit TO CHECK X LABELS!
#( 1 = gp,2 # hosp,3 # ae,4 # other,5 # gp + hops,6 # gp + ae,7 # gp + other,8 # hosp + ae,9 # hosp + ae)
aa<-antibiotics %>% dplyr::filter(type == "By visit")
aa$which.visit <- as.numeric(aa$which.visit)
visit_names <- c("None*","GP*","Hospital*","A&E","Other*","Appointment","GP+Hospital","GP+A&E","GP+Other","Hospital+A&E","Hospital+Other",
                 "GP+Hospital+A&E","GP+Hospital+Other","Hospital+A&E+Other","GP+Other+A&E")
p <- ggplot(aa,aes(x=which.visit, y=mean, ymin=lower, ymax=upper)) +
  geom_point()+geom_errorbar(width=0.3)+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_x_continuous("Visit",breaks=seq(0,14,1),labels = visit_names) +
  geom_vline(xintercept = c(0.5,5.5,10.5), size = 1,linetype=2)
p
ggsave("Visit_results.pdf",width = 12, height = 5)

w<-which(aa$n>100)
p <- ggplot(aa[w,],aes(x=which.visit, y=mean, ymin=lower, ymax=upper)) +
  geom_point()+geom_errorbar(width=0.3)+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent)  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_x_continuous("Visit",breaks=seq(0,14,1)[w],labels = visit_names[w]) +
  geom_vline(xintercept = c(0.5,5.5,10.5), size = 1,linetype=2)
p
ggsave("Visit_results_(bigcount).pdf",width = 12, height = 5)

# visit y/n
p <- ggplot(antibiotics %>% dplyr::filter(type == "By visityn"),
            aes(x=visit.medical.service.no, y=mean, ymin=lower, ymax=upper,colour=visit.medical.service.no)) +
  geom_point()+geom_errorbar(width=0.3)+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent)  + guides(colour=FALSE)+
  scale_x_discrete("Visit",labels = c("Visited","No visit"))
p
ggsave("Visit_results_yn.pdf",width = 12, height = 5)
## what proportion of those who didn't visit got an antibiotics? 
aa_v <- antibiotics %>% dplyr::filter(type == "By visityn")
# these got antibiotics despite no visit:
aa_v[which(aa_v$visit.medical.service.no=="t"),"prescribed"] / sum(aa_v$prescribed)


### Vaccine
p <- ggplot(antibiotics %>% dplyr::filter(type == "By vx status"), 
            aes(x=vaccine.this.year, y=mean, ymin=lower, ymax=upper,
                color=vaccine.this.year)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) + guides(colour=FALSE) +
  scale_x_discrete("Vaccine this year", labels = c("No","Yes")) 
p
setwd(plots)
ggsave("vaccine.pdf",width = 12, height = 8)

### Vaccine & Age
aa<-antibiotics %>% dplyr::filter(type == "By vx&age") %>% dplyr::filter(vaccine.this.year != "dont_know")
pa <- ggplot(aa,aes(x=agegroup, y=mean, ymin=lower, ymax=upper,color=vaccine.this.year)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) + scale_colour_discrete("Vaccine\nthis\nyear",labels=c("No","Yes")) + 
  scale_x_discrete("Age" ) 
pa
setwd(plots)
ggsave("vaccine_age_(noyesonly).pdf",width = 12, height = 8)


### Freq child contact
p <- ggplot(antibiotics %>% dplyr::filter(frequent.contact.children != "All"),
            aes(x=frequent.contact.children, y=mean, ymin=lower, ymax=upper,
                color=frequent.contact.children)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) + guides(colour = FALSE)+
  scale_x_discrete("Frequent contact with children", labels = c("No", "Yes")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
setwd(plots)
ggsave("freq_contact_children.pdf",width = 12, height = 5)

### Education
p <- ggplot(antibiotics %>% dplyr::filter(highest.education != "All"),
            aes(x=highest.education, y=mean, ymin=lower, ymax=upper,
                color=highest.education)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) + guides(colour = FALSE)+
  scale_x_discrete("Highest education",limits=c("no.education","education.gcse","education.alevels","education.bsc","education.msc",
                                                "education.stillin")) + theme(axis.text.x = element_text(angle = 60, hjust = 1))
p
setwd(plots)
ggsave("highest_education.pdf",width = 12, height = 8)

### Main activity 
# all > 100 so leave all in
aa<-antibiotics %>% dplyr::filter(main.activity != "All")
p <- ggplot(aa,aes(x=main.activity, y=mean, ymin=lower, ymax=upper,
                color=main.activity)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) + guides(colour = FALSE)+
  scale_x_discrete("Main activity") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
p
setwd(plots)
ggsave("main_activity.pdf",width = 12, height = 8)

# correlation with age? TO DO
# need? 
aa<-antibiotics %>% dplyr::filter(type == "By main.activity" | type == "By agegroup")
aa <- antibiotics %>% dplyr::group_by(main.activity, agegroup)
p <- ggplot(aa,aes(x=main.activity, y=mean, ymin=lower, ymax=upper,
                   color=agegroup, group = agegroup)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) + guides(colour = FALSE)+
  scale_x_discrete("Main activity") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
p


### ILI
p <- ggplot(antibiotics %>% dplyr::filter(ili != "All"),
            aes(x=ili, y=mean, ymin=lower, ymax=upper,
                color=ili)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) + guides(colour = FALSE)+
  scale_x_discrete("ILI (ECDC standards)",labels = c("No", "Yes")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
ggsave("ili.pdf",width = 12, height = 8)

p <- ggplot(antibiotics %>% dplyr::filter(ili.fever != "All"),
            aes(x=ili.fever, y=mean, ymin=lower, ymax=upper,color=ili.fever)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) + guides(colour = FALSE)+
  scale_x_discrete("ILI & fever (ECDC standards)",labels = c("No", "Yes")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
ggsave("ili_fever.pdf",width = 12, height = 8)
prop.test(table(antibiotics_orig$medication.antibiotic, antibiotics_orig$ili.fever), correct=FALSE) 

###*** "health.score" - link to how bad someone feels?
# 0 = worse. 100 = best. Minimum during episode in bt. If Inf then no entry. 
# baseline (median when no symptoms) as well
# relative (take difference between health and baseline?) or actual? [check any guidelines? scale of health scores] 
# is there a correlation? 

antibiotics_hs <- antibiotics_orig 
antibiotics_hs$hs <- (antibiotics_orig$min.health.score - antibiotics_orig$baseline.health.score)/antibiotics_orig$baseline.health.score
antibiotics_hs <- antibiotics_hs %>% dplyr::filter(!is.na(hs)) # remove any with NA = 12657! 
antibiotics_hs <- antibiotics_hs %>% dplyr::filter(!is.infinite(hs)) # remove any INF = 2... 

antibiotics_hs <- antibiotics_hs %>% dplyr::filter(hs<0) # health score min < baseline
dahs <- ddply(antibiotics_hs,"medication.antibiotic",summarise,n=length(hs),meanhs = mean(hs),minhs = min(hs), maxhs = max(hs))

p <- ggplot(dahs,aes(x=medication.antibiotic, y=meanhs, ymin=minhs, ymax=maxhs,color=medication.antibiotic)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Normalised healthscore [(min-base)/base]") + guides(colour = FALSE)+
  scale_x_discrete("Antibiotic prescription",labels = c("No", "Yes"))
p
ggsave("health_score_yn_range.pdf",width = 12, height = 8)

p <- ggplot(antibiotics_hs, aes(x=1, y = hs,colour=factor(medication.antibiotic)))+geom_point()+
  geom_jitter() + scale_color_discrete("Abx?") +
  scale_y_continuous("Normalised healthscore\n[(min-base)/base]") 
p
ggsave("health_score_yn_all.pdf",width = 12, height = 8)

p <- ggplot(antibiotics_hs, aes(x=factor(medication.antibiotic), y = health.score,colour=factor(medication.antibiotic)))+geom_point()+
  geom_jitter() + scale_color_discrete("Abx?") +
  scale_y_continuous("Episode healthscore") 
p

p <- ggplot(antibiotics_hs, aes(x=factor(medication.antibiotic), y = min.health.score,colour=factor(medication.antibiotic)))+ geom_violin()+
 scale_color_discrete("Abx?") + scale_x_discrete("Antibiotic?", labels = c("No", "Yes")) + geom_boxplot(width=0.1) + 
  scale_y_continuous("Episode health score") + scale_color_discrete(guide = "none")
p
ggsave("health_score_yn_violin.pdf",width = 12, height = 8)

p1 <- ggplot(antibiotics %>% dplyr::filter(cut.hs != "All"),
            aes(x=cut.hs, y=mean, ymin=lower, ymax=upper,color=cut.hs)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+ggtitle("(Min. episode\n- Baseline)/Baseline")+scale_x_discrete("Health score")+
  scale_y_continuous("Antibiotic usage rate", label=percent) + guides(colour = FALSE)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
p2 <- ggplot(antibiotics %>% dplyr::filter(cut.hs_score != "All"),
             aes(x=cut.hs_score, y=mean, ymin=lower, ymax=upper,color=cut.hs_score)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+ggtitle("Minimum\nepisode HS")+scale_x_discrete("Health score")+
  scale_y_continuous("Antibiotic usage rate", label=percent) + guides(colour = FALSE)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
p3 <- ggplot(antibiotics %>% dplyr::filter(cut.hs_base != "All"),
             aes(x=cut.hs_base, y=mean, ymin=lower, ymax=upper,color=cut.hs_base)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+ggtitle("Baseline HS")+scale_x_discrete("Health score")+
  scale_y_continuous("Antibiotic usage rate", label=percent) + guides(colour = FALSE)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggdraw() + draw_plot(p2, 0, 0, 0.3, 1) + draw_plot(p3, 0.33, 0, 0.3, 1) + draw_plot(p1, 0.66, 0, 0.3, 1) 
setwd(plots)
ggsave("health_score_cut.pdf",width = 12, height = 8)

# Underlying health == risk
p <- ggplot(antibiotics %>% dplyr::filter(norisk != "All"),
            aes(x=norisk, y=mean, ymin=lower, ymax=upper,color=norisk)) +
  geom_point()+geom_errorbar()+expand_limits(y=0)+
  scale_y_continuous("Antibiotic usage rate", label=percent) + guides(colour = FALSE)+
  scale_x_discrete("Risk", labels = c("Yes","No")) 
p
ggsave("risk.pdf",width = 12, height = 8)

