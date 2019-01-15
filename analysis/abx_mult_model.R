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
home <- "~/Documents/flusurvey/"
plots <- "~/Documents/flusurvey/plots/"
data <- "~/Dropbox/Flusurvey/data/"
mvmodels <- "~/Dropbox/Flusurvey/mvar_models/"
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

# # models load
# setwd(mvmodels)
# rr<-readRDS("multivariate_models.rds")
# ma.1   <- rr$ma.1 # MODEL 2
# ma.1.1 <- rr$ma.1.1 # MODEL 1
# ma.2   <- rr$ma.2
# ma.2.1 <- rr$ma.2.1
# ma.2.2 <- rr$ma.2.2
# ma.3   <- rr$ma.3 # MODEL 4
# ma.3.1 <- rr$ma.3.1
# ma.4   <- rr$ma.4


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
         visit_or_contact,
         visit_medical_service_no,
         starts_with("main_activity_"),
         gender, 
         frequent_contact_children, frequent_contact_elderly,norisk
  ) %>%
  mutate(participant_id=as.integer(participant_id)) %>%
  dplyr::filter(!is.na(age)) %>%
  mutate(abx = as.integer(medication_antibiotic) -1,
         age=(age-mean(age))/sd(age), ## regularise
         vaccine_this_year = as.integer(vaccine_this_year) - 1, 
         ili_fever = as.integer(ili_fever) - 1,
         visit_medical_service_no = as.integer(visit_medical_service_no) - 1,
         visit_or_contact = as.integer(visit_or_contact),
         gender = as.integer(gender)-1,
         frequent_contact_children = as.integer(frequent_contact_children) - 1,
         frequent_contact_elderly = as.integer(frequent_contact_elderly) - 1,
         norisk = as.integer(norisk)-1)

# check
length(which(is.na(btd))) # should be 0
dim(btd)

# participant_id needs to be in order for vector indexing for individual intercept model  
uu <- unique(btd$participant_id)
btd$participant_id_order <- 0
for(i in 1:length(uu)){
  w<-which(btd$participant_id == uu[i])
  btd[w,"participant_id_order"] <- i
}
btd$participant_id <- btd$participant_id_order
btd$participant_id_order <- NULL

## visit or contact needs to be 0 1 not 1 0
w<-which(btd$visit_or_contact == 0)
btd[w,"visit_or_contact"] = 2

w<-which(btd$visit_or_contact == 1)
btd[w,"visit_or_contact"] = 0

w<-which(btd$visit_or_contact == 2)
btd[w,"visit_or_contact"] = 1

# ################### *** Model 1: population characteristics: age and gender ################################################################################################################
# ma.1 <- map2stan(
#   alist(
#     abx ~ dbinom(1,theta),
#     logit(theta) <- a + b*gender + c*age,
#     c(a,b,c) ~ dnorm(0,10)
#   ),
#   data=btd, chains=4, cores = 4, iter = 6000, warmup = 1000 
# )
# 
# precis(ma.1)
# summary(ma.1)
# plot(ma.1)
# pairs(ma.1)
# 
# ################### *** Model 2: population characteristics: age and gender, interplay ################################################################################################################
# ma.2 <- map2stan( 
#   alist(
#     abx ~ dbinom(1,theta),
#     logit(theta) <- a + b*gender + c*age + j*age*gender,
#     c(a,b,c,j) ~ dnorm(0,10)
#   ),
#   data=btd, chains=4, cores = 4, iter = 6000, warmup = 1000 
# )
# 
# precis(ma.2)
# summary(ma.2)
# plot(ma.2)
# pairs(ma.2)
# 
# ##### COMPARE
# compare(ma.1, ma.2)

################### *** Model 3 ################################################################################################################
ma.3 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a + b*gender + c*age  +
      d * ili_fever + ea * vaccine_this_year + f * frequent_contact_children + 
      g * norisk + h * visit_or_contact + 
      ia * frequent_contact_elderly,
    c(a,b,c,d,ea,f,g,h,ia) ~ dnorm(0,10)
  ),
  data=btd, chains=4, cores=4, iter = 6000, warmup = 1000, control=list(adapt_delta=0.90)
)

precis(ma.3)
summary(ma.3)
plot(ma.3)
pairs(ma.3)




################### *** Model 4: control for medical visit ################################################################################################################
# As medical visit was such a big driver, include this with intercept
# remove h -> double counts? 



ma.4 <- map2stan(  ##### MODEL 4 #####
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a_v[visit_or_contact] + b*gender + c*age +
      d * ili_fever + ea * vaccine_this_year + f * frequent_contact_children + 
      g * norisk +  ia * frequent_contact_elderly,
    a_v[visit_or_contact] ~ dnorm(a,sigma_v),
    c(a,b,c,d,ea,f,g,ia) ~ dnorm(0,10), # WAS 40 = too high
    sigma_v ~ dcauchy(0,40)
  ),
  data=btd, chains=4, cores=4, iter = 6000, warmup = 1000, control=list(adapt_delta=0.90)
)


# ma.3 <- map2stan(
#   alist(
#     abx ~ dbinom(1,theta),
#     logit(theta) <- a_v[visit_medical_service_no] + b*gender + c*age +
#       ea * ili_fever + f * vaccine_this_year + g * frequent_contact_children + 
#       h * norisk + ia * visit_medical_service_no,
#     a_v[visit_medical_service_no] ~ dnorm(a,sigma_v),
#     c(a,b,c,ea,f,g,h,ia) ~ dnorm(0,10),
#     sigma_v ~ dcauchy(0,10)
#   ),
#   data=btd, chains=1,iter = 4000, warmup = 1000  
# )


precis(ma.4)
summary(ma.4)
plot(ma.4)
pairs(ma.4)


###*** Compare ########################################################
#compare(ma.1, ma.2) # MORE weight to ma.1
compare(ma.3, ma.4) # 

plot(coeftab(ma.3, ma.4)) # accounting for visit in the intercept has little impact 

###*** Save ########################################################
setwd(mvmodels)
saveRDS(list(ma.3 = ma.3,
             ma.4 = ma.4),"multivariate_models_paper.rds")


###*** POSTERIOR #####################
post <- extract.samples(ma.3)
dens(post$a, xlim = c(-2,2), ylim = c(0,15))
dens(post$b,col = rangi2, add = TRUE)
dens(post$c,col = "red", add = TRUE)
precis(ma.3, depth = 2)

# ### Generate samples MA1
# post <- extract.samples(ma.1) # 2000
# postd<-data.frame(matrix(unlist(post), nrow=20000, byrow=F))
# colnames(postd)<-names(post)
# colnames(postd) <- descrip <- c("Intercept","Coeff. gender","Coeff. age")
# postdm <- melt(postd)
# # convert 
# #postdm$value <- logistic(postdm$value)
# 
# ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.05) +
#   facet_wrap(~variable, scales = 'free') + geom_vline(xintercept=0)
# ggsave("logistic_posteriors_sep_ma.1.pdf")
# 
# ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.005,size = 1) + geom_vline(xintercept=0) + scale_color_manual(values = cbPalette)
# ggsave("logistic_posteriors_tog_ma.1.pdf", width = 10, height = 8)
# 
# 
# ### Generate samples MA2
# post <- extract.samples(ma.2) # 2000
# postd<-data.frame(matrix(unlist(post), nrow=20000, byrow=F))
# colnames(postd)<-names(post)
# colnames(postd) <- descrip <- c("Intercept","Coeff. gender","Coeff. age","Coeff. age&gender")
# postdm <- melt(postd)
# # convert 
# postdm$value <- logistic(postdm$value)
# 
# ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.05) +
#   facet_wrap(~variable, scales = 'free') + geom_vline(xintercept=0)
# ggsave("logistic_posteriors_sep_ma.2.pdf")
# 
# ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.005,size = 1) + geom_vline(xintercept=0) + scale_color_manual(values = cbPalette)
# ggsave("logistic_posteriors_tog_ma.2.pdf", width = 10, height = 8)

### Generate samples MA3
post <- extract.samples(ma.3) # 20000
postd<-data.frame(matrix(unlist(post), nrow=20000, byrow=F))
colnames(postd)<-names(post)
colnames(postd) <- descrip <- c("Intercept","Coeff. gender","Coeff. age",
                                "Coeff. ILI fever","Coeff. vx","Coeff. children","Coeff. risk",
                                "Coeff. visit", "Coeff.elderly")

postd2 <- postd[,c("Coeff. gender","Coeff. age",
                                "Coeff. ILI fever","Coeff. vx","Coeff. children","Coeff. risk",
                                "Coeff. visit", "Coeff.elderly")]
postdm <- melt(postd)
postdm2 <- melt(postd2)
# convert 
#postdm$value <- logistic(postdm$value)

ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.05) +
  facet_wrap(~variable, scales = 'free') + geom_vline(xintercept=0)
ggsave("logistic_posteriors_sep_ma.3.pdf")

ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.005,size = 1) + 
  geom_vline(xintercept=0) + scale_color_manual(values = c(cbPalette,"black")) + 
  scale_x_continuous(lim = c(-5,5))
ggsave("logistic_posteriors_tog_ma.3.pdf", width = 10, height = 8)

ggplot(postdm2, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.005,size = 1) + 
  geom_vline(xintercept=0) + scale_color_manual(values = c(cbPalette,"black")) + 
  scale_x_continuous(lim = c(-5,5))
ggsave("logistic_posteriors_tog_no_intercept_ma.3.pdf", width = 10, height = 8)

### Generate samples MA4
post <- extract.samples(ma.4) # 2000
postd<-data.frame(matrix(unlist(post), nrow=20000, byrow=F))
colnames(postd)<-names(post)
colnames(postd) <- descrip <- c("Intercept_visit_YN","Intercept_visit_NY","mean intercept",
                                "Coeff. gender","Coeff. age",
                                "Coeff. ILI fever","Coeff. vx","Coeff. children","Coeff. risk","Coeff.elderly", 
                                "sigma intercept")
postd2 <- postd[,c("Coeff. gender","Coeff. age",
                                "Coeff. ILI fever","Coeff. vx",
                   "Coeff. children","Coeff. risk","Coeff.elderly")]

postdm <- melt(postd)
postdm2 <- melt(postd2)
# convert 
#postdm$value <- logistic(postdm$value)

ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.05) +
  facet_wrap(~variable, scales = 'free') + geom_vline(xintercept=0)
ggsave("logistic_posteriors_sep_ma.4.pdf")

ggplot(postdm, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.005,size = 1) + 
  geom_vline(xintercept=0) + scale_color_manual(values = c("red","cyan",cbPalette[1:7],"black", "pink")) + 
  scale_x_continuous(lim = c(-10,10))
ggsave("logistic_posteriors_tog_ma.4.pdf", width = 10, height = 8)

ggplot(postdm2, aes(value, colour = variable)) + geom_freqpoly(binwidth = 0.005,size = 1) + 
  geom_vline(xintercept=0) + scale_color_manual(values = c(cbPalette[c(1:6,8)],"black", "pink")) + 
  scale_x_continuous(lim = c(-5,5))
ggsave("logistic_posteriors_tog_no_intercept_ma.4.pdf", width = 10, height = 8)


#########################################################################################################
##############################################################################################################
#########################################################################################################
##### *** INFORMED PRIORS *** ######
#########################################################################################################
#########################################################################################################
#########################################################################################################

################### *** Model 3 ################################################################################################################
ma.3p <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a + b*gender + c*age  +
      d * ili_fever + ea * vaccine_this_year + f * frequent_contact_children + 
      g * norisk + h * visit_or_contact + 
      ia * frequent_contact_elderly,
    c(b,c,f,ia) ~ dnorm(1,10), # Gender = Female is one
    c(g,h) ~ dnorm(-1,10), # Norisk = 1, didn't visit = 1
    c(a,d,ea) ~ dnorm(0,10)
  ),
  data=btd, chains=4, cores=4, iter = 6000, warmup = 1000, control=list(adapt_delta=0.90)
)

precis(ma.3p)
summary(ma.3p)
plot(ma.3p)
pairs(ma.3p)


################### *** Model 4: control for medical visit ################################################################################################################
# As medical visit was such a big driver, include this with intercept
# remove h -> double counts? 
ma.4p <- map2stan(  ##### MODEL 4 #####
                   alist(
                     abx ~ dbinom(1,theta),
                     logit(theta) <- a_v[visit_or_contact] + b*gender + c*age +
                       d * ili_fever + ea * vaccine_this_year + f * frequent_contact_children + 
                       g * norisk +  ia * frequent_contact_elderly,
                     a_v[visit_or_contact] ~ dnorm(a,sigma_v),
                     c(b,c,d,ea,f,g,ia) ~ dnorm(0,10),
                     c(b,c,f,ia) ~ dnorm(1,10), # Gender = Female is one
                     c(a,g) ~ dnorm(-1,10), # Norisk = 1, didn't visit = 1
                     c(a,d,ea) ~ dnorm(0,10),
                     sigma_v ~ dcauchy(0,40)
                   ),
                   data=btd, chains=4, cores=4, iter = 6000, warmup = 1000, control=list(adapt_delta=0.90)
)

precis(ma.4p)
summary(ma.4p)
plot(ma.4p)
pairs(ma.4p)


###*** Compare ########################################################
compare(ma.1, ma.2, ma.3, ma.4, ma.3p, ma.4p) # 
compare(ma.1, ma.2, ma.3, ma.4)
compare(ma.1, ma.2)


###*** Save ########################################################
setwd(mvmodels)
saveRDS(list(ma.3p = ma.3p,
             ma.4p = ma.4p),"multivariate_models_priors_paper.rds")

##### READ IN ######

setwd(mvmodels)
mm <- readRDS("multivariate_models_paper.rds")
ma.1 = mm$ma.1;
ma.2 = mm$ma.2;
ma.3 = mm$ma.3;
ma.4 = mm$ma.4;

mm <- readRDS("multivariate_models_priors_paper.rds")
ma.3p = mm$ma.3p;
ma.4p = mm$ma.4p;

###*** Compare ########################################################
compare(ma.1, ma.2, ma.3, ma.4, ma.3p, ma.4p) # 
compare(ma.1, ma.2, ma.3, ma.4)
compare(ma.1, ma.2)
compare(ma.3, ma.4)
compare(ma.3p, ma.4p)

cc <- compare(ma.1, ma.2, ma.3, ma.4, ma.3p, ma.4p)

plot(cc, SE=TRUE, dSE = TRUE)

Model1 <- ma.3
Model2 <- ma.4

cc <- coeftab(Model1, Model2)
pdf("model_compare_para.pdf")
plot(cc)
dev.off()

###*** Remove low level coefficients #######
post <- extract.samples(ma.3)
coefs_mean <- abs(c(mean(post$a), mean(post$b), mean(post$c), mean(post$d), mean(post$ea), 
                    mean(post$f), mean(post$g),mean(post$h),mean(post$ia)))
names(coefs_mean) <- c("a","b","c","d","ea","f","g","h","ia")
coefs_mean[order(coefs_mean)]

## => remove c first, < 0.2 smallest

ma.3.a1 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a + b*gender + 
      d * ili_fever + ea * vaccine_this_year + f * frequent_contact_children + 
      g * norisk + h * visit_or_contact + ia * frequent_contact_elderly,
    c(a,b,d,ea,f,g,h,ia) ~ dnorm(0,10)
  ),
  data=btd, chains=4, cores=4, iter = 6000, warmup = 1000, control=list(adapt_delta=0.90)
  #data=btd, chains=2, cores=4, iter = 2000, warmup = 1000, control=list(adapt_delta=0.90)
)

precis(ma.3.a1)
summary(ma.3.a1)
plot(ma.3.a1)
pairs(ma.3.a1)

## => remove c and ia, < 0.2 and v similar

ma.3.a2 <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a + b*gender + 
      d * ili_fever + ea * vaccine_this_year + f * frequent_contact_children + 
      g * norisk + h * visit_or_contact,
    c(a,b,d,ea,f,g,h) ~ dnorm(0,10)
  ),
  data=btd, chains=4, cores=4, iter = 6000, warmup = 1000, control=list(adapt_delta=0.90)
  #data=btd, chains=2, cores=4, iter = 2000, warmup = 1000, control=list(adapt_delta=0.90)
)

precis(ma.3.a2)
summary(ma.3.a2)
plot(ma.3.a2)
pairs(ma.3.a2)

## remove b next 

ma.3.b <- map2stan(
  alist(
    abx ~ dbinom(1,theta),
    logit(theta) <- a + d * ili_fever + ea * vaccine_this_year + f * frequent_contact_children + 
      g * norisk + h * visit_or_contact,
    c(a,d,ea,f,g,h) ~ dnorm(0,10)
  ),
 data=btd, chains=4, cores=4, iter = 6000, warmup = 1000, control=list(adapt_delta=0.90)
 #data=btd, chains=2, cores=4, iter = 2000, warmup = 1000, control=list(adapt_delta=0.90)
)

precis(ma.3.b)
summary(ma.3.b)
plot(ma.3.b)
pairs(ma.3.b)

###*** Save ########################################################
setwd(mvmodels)
saveRDS(list(ma.3.a1 = ma.3.a1,
             ma.3.a2 = ma.3.a2,
             ma.3.b = ma.3.b),"multivariate_models_paper_remove.rds")

## Compare 
compare(ma.3, ma.3.a1, ma.3.a2, ma.3.b)


