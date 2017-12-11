## Multivariate analysis
# Based on Seb's "contact_model.R"

## libraries
library('rethinking')
library('flusurvey')
library('dplyr')
library('magrittr')
library('lubridate')
library('data.table')
library('xtable')

## locations
home <- "~/Documents/Flusurvey/"
plots <- "~/Documents/Flusurvey/plots/"
data <- "~/Documents/Flusurvey/data/"
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## Function from Seb
# Changes somethig with multiple levels to this var y/n e.g. "main.occupation"
categorical_to_single <- function(dt, var) {
  categories <- levels(dt[, get(var)])[-1]
  for (category in categories) {
    dt[, paste(var, category, sep="_") :=
         as.integer(get(var) == category)]
  }
  return(dt)
}

### data
# Bouts of illness data - for antibiotics, cleaned in antibiotics_univariate
setwd(data)
bt <- readRDS("btt_abx.rds")
bt <- data.table(bt)

# models load
setwd(home)
setwd("mvar_models")
rr<-readRDS("multivariate_models.rds")
ma.1   <- rr$ma.1
ma.1.1 <- rr$ma.1.1
ma.2   <- rr$ma.2
ma.2.1 <- rr$ma.2.1
ma.2.2 <- rr$ma.2.2
ma.3   <- rr$ma.3


## Analysis
# Include: 
# \item Age (will be regularised) [age] x 
# \item If have ILI and fever [ili.fever] x
# \item Vaccine status [vaccine.this.year] x
# \item Whether they visited a medical service or not [Visit.medical.service.no] x
# \item Gender [gender] x
# \item Frequent contact with children [frequent.contact.children]
# \item Underlying health risk [norisk] x NOTE that this is 1 for true = no risk. So 0 for yes I have a risk

## remove "." from names
colnames(bt) <- gsub("\\.", "_", colnames(bt))

# Set up data
btd <- bt %>%
  categorical_to_single("main_activity") %>%
  select(participant_id, medication_antibiotic, age, 
         ili_fever, vaccine_this_year, 
         visit_medical_service_no,
         starts_with("main_activity_"),
         gender, 
         frequent_contact_children, norisk
  ) %>%
  mutate(participant_id=as.integer(participant_id)) %>%
  dplyr::filter(!is.na(age)) %>%
  mutate(abx = as.integer(medication_antibiotic) -1,
         age=(age-mean(age))/sd(age), ## regularise
         vaccine_this_year = as.integer(vaccine_this_year) - 1, 
         visit_medical_service_no = as.integer(visit_medical_service_no) - 1,
         gender = as.integer(gender)-1,
         frequent_contact_children = as.integer(frequent_contact_children) - 1,
         norisk = as.integer(norisk)-1)

# check
length(which(is.na(btd))) # should be 0
dim(btd)

#####*** Output for paper ####
### How many ...
# .. bouts
dim(btd) # 27673
# .. participants
length(unique(btd[,"participant_id"])) # 3650
# .. with antibiotic info? 
btd %>% .$medication_antibiotic %>% table # 1151
1151/27673
# .. which visit medical service
w<-which(btd$medication_antibiotic == "t")
table(btd[w,"visit_medical_service_no"])
188/(188+963)
table(btd$visit_medical_service_no)
w<-which(btd$visit_medical_service_no == "0")
table(btd[w,"medication_antibiotic"])
963/(1344+963)
# .. episodes per participant
h<-hist(btd$participant_id,breaks = seq(1,5000,1))
max(h$counts)
min(h$counts)
mean(h$counts)
sqrt(var(h$counts))


################### *** Model 1: population characteristics: age and gender ################################################################################################################
ma.1 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a + b*gender + c*age + d*age*gender,
    c(a,b,c,d) ~ dnorm(0,10)
  ),
  data=btd, chains=1, iter = 4000, warmup = 1000 
)

precis(ma.1)
summary(ma.1)
plot(ma.1)
pairs(ma.1)

# need to account for age in gender results? 
ma.1.1 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a + b*gender + c*age,
    c(a,b,c) ~ dnorm(0,10)
  ),
  data=btd, chains=1, iter = 4000, warmup = 1000  
)

precis(ma.1.1)
summary(ma.1.1)
plot(ma.1.1)
pairs(ma.1.1)

compare(ma.1, ma.1.1)

################### *** Model 2: all ################################################################################################################
ma.2 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a + b*gender + c*age + d*age*gender +
      ea * ili_fever + f * vaccine_this_year + g * frequent_contact_children + 
      h * norisk + ia * visit_medical_service_no + 
      j * main_activity_paid_employment_part_time +
      k * main_activity_self_employed +
      l * main_activity_school +
      m * main_activity_home_maker +
      n * main_activity_unemployed +
      oa * main_activity_long_term_leave +
      p * main_activity_retired +
      q * main_activity_other,
    c(a,b,c,d,ea,f,g,h,ia,j,k,l,m,n,oa,p,q) ~ dnorm(0,10)
  ),
  data=btd, chains=2, cores=2 
)

precis(ma.2)
summary(ma.2)
plot(ma.2)
pairs(ma.2)

# remove age/gender link (as shown by ma1.1 better than ma.1)
ma.2.1 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a + b*gender + c*age +
      ea * ili_fever + f * vaccine_this_year + g * frequent_contact_children + 
      h * norisk + ia * visit_medical_service_no + 
      j * main_activity_paid_employment_part_time +
      k * main_activity_self_employed +
      l * main_activity_school +
      m * main_activity_home_maker +
      n * main_activity_unemployed +
      oa * main_activity_long_term_leave +
      p * main_activity_retired +
      q * main_activity_other,
    c(a,b,c,ea,f,g,h,ia,j,k,l,m,n,oa,p,q) ~ dnorm(0,10)
  ),
  data=btd, chains=2, cores=2 
)

precis(ma.2.1)
summary(ma.2.1)
plot(ma.2.1)
pairs(ma.2.1)

# and remove main activity - correlates with age
ma.2.2 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a + b*gender + c*age +
      ea * ili_fever + f * vaccine_this_year + g * frequent_contact_children + 
      h * norisk + ia * visit_medical_service_no,
    c(a,b,c,ea,f,g,h,ia) ~ dnorm(0,10)
  ),
  data=btd, chains=1,iter = 4000, warmup = 1000  
)


precis(ma.2.2)
summary(ma.2.2)
plot(ma.2.2)
pairs(ma.2.2)

compare(ma.2,ma.2.1, ma.2.2) ### COMPARE
plot(compare(ma.2, ma.2.1, ma.2.2))

################### *** Model 3: control for medical visit ################################################################################################################
# As medical visit was such a big driver, include this with intercept
ma.3 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a_v[visit_medical_service_no] + b*gender + c*age +
      ea * ili_fever + f * vaccine_this_year + g * frequent_contact_children + 
      h * norisk + ia * visit_medical_service_no,
    a_v[visit_medical_service_no] ~ dnorm(a,sigma_v),
    c(a,b,c,ea,f,g,h,ia) ~ dnorm(0,10),
    sigma_v ~ dcauchy(0,10)
  ),
  data=btd, chains=1,iter = 4000, warmup = 1000  
)

precis(ma.3)
summary(ma.3)
plot(ma.3)
pairs(ma.3)

# remove ia -> ma.3 double counts? 
ma.3.1 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a_v[visit_medical_service_no] + b*gender + c*age +
      ea * ili_fever + f * vaccine_this_year + g * frequent_contact_children + 
      h * norisk,
    a_v[visit_medical_service_no] ~ dnorm(a,sigma_v),
    c(a,b,c,ea,f,g,h) ~ dnorm(0,40),
    sigma_v ~ dcauchy(0,40)
  ),
  data=btd, chains=1, cores=1 
)

precis(ma.3.1)
summary(ma.3.1)
plot(ma.3.1)
pairs(ma.3.1)

###*** Compare ########################################################
compare(ma.1, ma.2) # complete weight to ma.2
compare(ma.1, ma.2, ma.1.1, ma.2.1, ma.2.2) # 69% to ma.2.2, 23 to ma.2.1, 8 to ma.2
compare(ma.1, ma.2, ma.1.1, ma.2.1, ma.2.2,ma.3)
compare(ma.1, ma.2, ma.1.1, ma.2.1, ma.2.2,ma.3, ma.3.1)

plot(coeftab(ma.1, ma.2, ma.1.1, ma.2.1, ma.2.2,ma.3))
plot(coeftab(ma.2.2, ma.3)) # accounting for visit in the intercept has little impact 
# on the estimates for the other parameters

###** for paper
compare(ma.1.1, ma.1, ma.2.2, ma.3)
plot(coeftab(ma.2.2, ma.3))

p2.2 <- precis(ma.2.2)$output


coeftab(ma.2.2, ma.3)
precis(ma.2.2)
mean2.2 <-  c(round(logistic(c(-0.13, 0.07,0.11,0.6, -0.1, 0.27,-0.43,-4.49)),2), "NA")
lower2.2 <- c(round(logistic(c(-0.28,-0.06,0.05,0.37,-0.23,0.11,-0.57,-4.63)),2), "NA")
upper2.2 <- c(round(logistic(c( 0.02, 0.2, 0.17,0.85, 0.04,0.42,-0.28,-4.36)),2), "NA")

precis(ma.3)
mean3 <-  round(logistic(c( 2.02, 0.07,0.11,0.6, -0.1, 0.27,-0.43,-2.82, 5.98)),2)
lower3 <- round(logistic(c(-9.25,-0.06,0.05,0.37,-0.22,0.11,-0.60,-9.61, 0.04)),2)
upper3 <- round(logistic(c(11.84, 0.19,0.17,0.84, 0.03,0.43,-0.31, 5.01, 12.52)),2)

para_v <- c("$a$","$\beta_1$","$\beta_2$","$\beta_3$","$\beta_4$","$\beta_5$","$\beta_6$","$\beta_7$","$sigma_v$") # won't let me put "\" in sigma...!
descrip <- c("Intercept","Coefficient of age","Coefficient of gender","Coefficient of ILI fever"
,"Coefficient of vaccine this year","Coefficient of frequent contact with children"
,"Coefficient of underlying risk","Coefficient of visit to medical service","Variance in intercept ($a$)")
tbl <- cbind(para_v,descrip,mean2.2, lower2.2, upper2.2, mean3, lower3, upper3)

xftbl <- xtable(tbl)
print(xftbl, booktabs = TRUE, file="est_para.txt")


###*** Save ########################################################
setwd(home)
setwd("mvar_models")
saveRDS(list(ma.1 = ma.1,
             ma.2 = ma.2,
             ma.1.1 = ma.1.1,
             ma.2.1 = ma.2.1,
             ma.2.2 = ma.2.2,
             ma.3 = ma.3,
             ma.3.1 = ma.3.1),"multivariate_models.rds")


###*** Plot #####
### Generate samples
post <- extract.samples(ma.2.2) # 2000
postd<-data.frame(matrix(unlist(post), nrow=3000, byrow=F))
colnames(postd)<-names(post)
colnames(postd) <- descrip <- c("Intercept","Coeff. age","Coeff. gender","Coeff. ILI fever","Coeff. vx","Coeff. children","Coeff. risk","Coeff. visit")
postdm <- melt(postd)
# convert 
postdm$value <- logistic(postdm$value)
setwd(plots)

ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.05) +
  facet_wrap(~variable, scales = 'free') + geom_vline(xintercept=0)
ggsave("logistic_posteriors_sep3.pdf")

ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.005,size = 1) + geom_vline(xintercept=0) + scale_color_manual(values = cbPalette)
ggsave("logistic_posteriors_tog3.pdf")

### Generate samples
post <- extract.samples(ma.3) # 2000
postd<-data.frame(matrix(unlist(post), nrow=3000, byrow=F))
colnames(postd)<-names(post)
colnames(postd) <- descrip <- c("Intercept","Coeff. age","Coeff. gender","Coeff. ILI fever","Coeff. vx","Coeff. children","Coeff. risk","Coeff. visit")
postdm <- melt(postd)
# convert 
postdm$value <- logistic(postdm$value)
setwd(plots)

ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.05) +
  facet_wrap(~variable, scales = 'free') + geom_vline(xintercept=0)
ggsave("logistic_posteriors_sep.pdf")

ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.005,size = 1) + geom_vline(xintercept=0) + scale_color_manual(values = cbPalette)
ggsave("logistic_posteriors_tog.pdf")

# Plot extracts
post_1ky <- extract.samples(ma.2.2)
xx <- sample(btd$abx,length(post_1ky$a), replace = TRUE)
post_1ky$output <- post_1ky$a + post_1ky$b * btd$gender[1:2000] + post_1ky$c * btd$age[1:2000] + post_1ky$ea * btd$ili_fever[1:2000] + post_1ky$f * btd$vaccine_this_year[1:2000] + post_1ky$g * btd$frequent_contact_children[1:2000] + post_1ky$h * btd$norisk[1:2000] + 
  post_1ky$ia * btd$visit_medical_service_no[1:2000]

plot(btd$abx)
points(xx,col="red")


### ensemble
ee<-ensemble(ma.2.2,ma.3,data = btd) # adds in weight
mu <- apply(ee$link, 2, mean)
mu.PI <- apply(ee$link, 2, PI)
plot(btd[,"abx"],mu,ylim = c(0,1))

dd <- as.data.frame(cbind(mu,btd$abx))
colnames(dd) <- c("mu","abx")
p <- ggplot(dd, aes(x=abx, y=mu, group = abx)) + geom_violin() + scale_y_continuous("Mean",limits = c(0,1)) + 
  scale_x_continuous("Antibiotic taken\nthis episode", breaks = c(0,1), labels = c("0 (No)","1 (Yes)"))
p  
setwd(plots)
ggsave("violin_plot_fit_weight.pdf")

# Individual models 
mu2.2 <- link(ma.2.2)
mu3 <- link(ma.3)

# bind together
dd <- as.data.frame(cbind(btd$abx,mu,colMeans(mu2.2),colMeans(mu3)))
colnames(dd) <- c("abx","Joint","Model 3","Model 4")
ddm <- melt(dd, id.vars = "abx")
p <- ggplot(ddm, aes(x=abx, y=value, group = abx)) + geom_violin() + facet_wrap(~variable) + 
  scale_y_continuous("Mean",limits = c(0,1)) + 
  scale_x_continuous("Antibiotic taken\nthis episode", breaks = c(0,1), labels = c("0 (No)","1 (Yes)"))
p  
setwd(plots)
ggsave("violin_plot_fit_separate.pdf")


############################*** OLD *****************************************************************************************************########################################################
# mock
m2 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a + bp*gender,
    a ~ dnorm(0,10),
    bp ~ dnorm(0,10)
  ),
  data=btd, chains=2, cores=2 
)

precis(m2)
summary(m2)
plot(m2)
pairs(m2)


## include all in model
m_variate_model <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a +
      ba * age +
      bg * gender +
      bf * ili_fever + 
      bv * vaccine_this_year + 
      bc * frequent_contact_children + 
      brn * norisk + 
      bvmn * visit_medical_service_no + 
      bm1 * main_activity_paid_employment_part_time +
      bm2 * main_activity_self_employed +
      bm3 * main_activity_school +
      bm4 * main_activity_home_maker +
      bm5 * main_activity_unemployed +
      bm6 * main_activity_long_term_leave +
      bm7 * main_activity_retired +
      bm8 * main_activity_other,
    a ~ dnorm(2.5, 1),
    ba ~ dnorm(0, 1),
    bg ~ dnorm(0, 1),
    bf ~ dnorm(0, 1),
    bv ~ dnorm(0, 1),
    bc ~ dnorm(0, 1),
    brn ~ dnorm(0, 1),
    bvmn ~ dnorm(0, 1),
    bm1 ~ dnorm(0, 1),
    bm2 ~ dnorm(0, 1),
    bm3 ~ dnorm(0, 1),
    bm4 ~ dnorm(0, 1),
    bm5 ~ dnorm(0, 1),
    bm6 ~ dnorm(0, 1),
    bm7 ~ dnorm(0, 1),
    bm8 ~ dnorm(0, 1)
  ), data=btd, chains=2, cores=2
)

precis(m_variate_model) # prior (0,1) = all much changed
precis(m_variate_model,corr=TRUE) # why NAs? 
summary(m_variate_model)
plot(m_variate_model)
pairs(m_variate_model)
dashboard(m_variate_model)

#postcheck(m_variate_model)

# outcome is binomial. Yes / No. What posterior can be plotted? 
# 

### Generate samples
post <- extract.samples(m_variate_model) # 2000
postd<-data.frame(matrix(unlist(post), nrow=2000, byrow=F))
colnames(postd)<-names(post)
# check
#head(postd$a)
#head(post$a)
postdm <- melt(postd)
ggplot(postdm,aes(x=))

ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.05) + geom_vline(xintercept=0)
ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.05) + facet_wrap(~variable) + geom_vline(xintercept=0)
ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.05) +
      facet_wrap(~variable) + scale_x_continuous(lim=c(-1,1)) + geom_vline(xintercept=0)


## Include individual variation?
# individual important within episode? 
m_variate_individual_model <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a[participant_id] +
      ba * age +
      bg * gender +
      bf * ili_fever + 
      bv * vaccine_this_year + 
      bc * frequent_contact_children + 
      brn * norisk + 
      bvmn * visit_medical_service_no + 
      bm1 * main_activity_paid_employment_part_time +
      bm2 * main_activity_self_employed +
      bm3 * main_activity_school +
      bm4 * main_activity_home_maker +
      bm5 * main_activity_unemployed +
      bm6 * main_activity_long_term_leave +
      bm7 * main_activity_retired +
      bm8 * main_activity_other,
    a ~ dnorm(2.5, 1),
    ba ~ dnorm(0, 1),
    bg ~ dnorm(0, 1),
    bf ~ dnorm(0, 1),
    bv ~ dnorm(0, 1),
    bc ~ dnorm(0, 1),
    brn ~ dnorm(0, 1),
    bvmn ~ dnorm(0, 1),
    bm1 ~ dnorm(0, 1),
    bm2 ~ dnorm(0, 1),
    bm3 ~ dnorm(0, 1),
    bm4 ~ dnorm(0, 1),
    bm5 ~ dnorm(0, 1),
    bm6 ~ dnorm(0, 1),
    bm7 ~ dnorm(0, 1),
    bm8 ~ dnorm(0, 1)
  ), data=btd,  start=list(a=rep(-0.1, dim(btd)[1])), chains=2, cores=2
)

precis(m_variate_individual_model) # prior (0,1) = all much changed
precis(m_variate_individual_model,corr=TRUE) # why NAs? 
summary(m_variate_individual_model)
plot(m_variate_individual_model)
pairs(m_variate_individual_model)
dashboard(m_variate_individual_model)

### Save models
setwd(home)
setwd("mvar_models")
saveRDS(list(mvariate     = m_variate_model,
             mvariate_ind = m_variate_individual_model),
        "1st_1_2_models.rds")


### Generate samples
post <- extract.samples(m_variate_individual_model) # 2000
postd<-data.frame(matrix(unlist(post), nrow=2000, byrow=F))
colnames(postd)<-names(post)
# check
#head(postd$a)
#head(post$a)
postdm <- melt(postd)
ggplot(postdm,aes(x=))

ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.05) + geom_vline(xintercept=0)
ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.05) + facet_wrap(~variable) + geom_vline(xintercept=0)
ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.05) +
  facet_wrap(~variable) + scale_x_continuous(lim=c(-1,1)) + geom_vline(xintercept=0)

#### Remove some elements 
m1 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a +
      ba * age +
      bg * gender +
      bf * ili_fever + 
      bv * vaccine_this_year + 
      bc * frequent_contact_children + 
      brn * norisk + 
      bvmn * visit_medical_service_no,
    a ~ dnorm(2.5, 1),
    ba ~ dnorm(0, 1),
    bg ~ dnorm(0, 1),
    bf ~ dnorm(0, 1),
    bv ~ dnorm(0, 1),
    bc ~ dnorm(0, 1),
    brn ~ dnorm(0, 1),
    bvmn ~ dnorm(0, 1)
  ), data=btd, chains=2, cores=2
)

m2 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a +
      ba * age +
      bg * gender +
      bf * ili_fever + 
      bv * vaccine_this_year + 
      bc * frequent_contact_children + 
      brn * norisk,
    a ~ dnorm(2.5, 1),
    ba ~ dnorm(0, 1),
    bg ~ dnorm(0, 1),
    bf ~ dnorm(0, 1),
    bv ~ dnorm(0, 1),
    bc ~ dnorm(0, 1),
    brn ~ dnorm(0, 1)
  ), data=btd, chains=2, cores=2
)

m3 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a +
      ba * age +
      bg * gender +
      bf * ili_fever + 
      bv * vaccine_this_year + 
      bc * frequent_contact_children,
    a ~ dnorm(2.5, 1),
    ba ~ dnorm(0, 1),
    bg ~ dnorm(0, 1),
    bf ~ dnorm(0, 1),
    bv ~ dnorm(0, 1),
    bc ~ dnorm(0, 1),
    brn ~ dnorm(0, 1)
  ), data=btd, chains=2, cores=2
)

m4 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a +
      ba * age +
      bg * gender +
      bf * ili_fever + 
      bv * vaccine_this_year + 
      bc * frequent_contact_children + 
      brn * norisk + 
      bm1 * main_activity_paid_employment_part_time +
      bm2 * main_activity_self_employed +
      bm3 * main_activity_school +
      bm4 * main_activity_home_maker +
      bm5 * main_activity_unemployed +
      bm6 * main_activity_long_term_leave +
      bm7 * main_activity_retired +
      bm8 * main_activity_other,
    a ~ dnorm(2.5, 1),
    ba ~ dnorm(0, 1),
    bg ~ dnorm(0, 1),
    bf ~ dnorm(0, 1),
    bv ~ dnorm(0, 1),
    bc ~ dnorm(0, 1),
    brn ~ dnorm(0, 1),
    bvmn ~ dnorm(0, 1), 
    bm1 ~ dnorm(0, 1),
    bm2 ~ dnorm(0, 1),
    bm3 ~ dnorm(0, 1),
    bm4 ~ dnorm(0, 1),
    bm5 ~ dnorm(0, 1),
    bm6 ~ dnorm(0, 1),
    bm7 ~ dnorm(0, 1),
    bm8 ~ dnorm(0, 1)
  ), data=btd, chains=2, cores=2
)

m5 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a +
      ba * age +
      bg * gender +
      bf * ili_fever + 
      bv * vaccine_this_year,
    a ~ dnorm(2.5, 1),
    ba ~ dnorm(0, 1),
    bg ~ dnorm(0, 1),
    bf ~ dnorm(0, 1),
    bv ~ dnorm(0, 1),
    bc ~ dnorm(0, 1),
    brn ~ dnorm(0, 1)
  ), data=btd, chains=2, cores=2
)

m6 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a +
      ba * age +
      bg * gender +
      bf * ili_fever,
    a ~ dnorm(2.5, 1),
    ba ~ dnorm(0, 1),
    bg ~ dnorm(0, 1),
    bf ~ dnorm(0, 1),
    bv ~ dnorm(0, 1),
    bc ~ dnorm(0, 1),
    brn ~ dnorm(0, 1)
  ), data=btd, chains=2, cores=2
)

m7 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a +
      ba * age +
      bg * gender,
    a ~ dnorm(2.5, 1),
    ba ~ dnorm(0, 1),
    bg ~ dnorm(0, 1),
    bf ~ dnorm(0, 1),
    bv ~ dnorm(0, 1),
    bc ~ dnorm(0, 1),
    brn ~ dnorm(0, 1)
  ), data=btd, chains=2, cores=2
)

m8 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a +
      ba * age,
    a ~ dnorm(2.5, 1),
    ba ~ dnorm(0, 1),
    bg ~ dnorm(0, 1),
    bf ~ dnorm(0, 1),
    bv ~ dnorm(0, 1),
    bc ~ dnorm(0, 1),
    brn ~ dnorm(0, 1)
  ), data=btd, chains=2, cores=2
)

## compare
# m_variate_model (all), m1 (no main activity), m2 (no main + visit_medic), m3 (no main + visit_medic + norisk)
# m3 (no visit_medic only), m5 (no main + visit_medic + norisk + frequent_contact_children) + ... + m8 (only intercept + age)
compare(m1,m2,m3,m5,m6,m7,m8,m_variate_model)
plot(compare(m1,m2,m3,m4,m5,m6,m7,m8,m_variate_model))