antibioticsv <- rbind(antibiotics_age_season, antibiotics_age, antibiotics_region,
                      antibiotics_vxthis,antibiotics_vxthis_age,
                      antibiotics_h.edu,antibiotics_main_activity,
                      antibiotics_visit,antibiotics_ili,antibiotics_ili_fever,
                      antibiotics_freqchild,
                      antibiotics_gender,antibiotics_gender_age,
                      antibiotics_risk)
antibioticsv <- antibioticsv[,-1]

antibiotics <- rbind(antibiotics_age_season, antibiotics_age, antibiotics_region,
                     antibiotics_vxthis,antibiotics_vxthis_age,
                     antibiotics_h.edu,antibiotics_main_activity,
                     antibiotics_ili,antibiotics_ili_fever,
                     antibiotics_freqchild,
                     antibiotics_gender,antibiotics_gender_age,
                     antibiotics_risk)

anti_binom  <- binom.confint(antibiotics$prescribed, antibiotics$n,method="wilson")
anti_binomv  <- binom.confint(antibioticsv$prescribed, antibioticsv$n,method="wilson")

anew <- left_join(antibiotics, anti_binom)
anewv <- left_join(antibioticsv, anti_binomv)
dim(anewv)
dim(antibioticsv)
dim(anti_binomv)


#########
library(rethinking)
data(chimpanzees)

# don't want any variables with NAs
d <- list( 
  pulled_left = chimpanzees$pulled_left ,
  prosoc_left = chimpanzees$prosoc_left ,
  condition = chimpanzees$condition ,
  actor = as.integer( chimpanzees$actor ) ,
  blockid = as.integer( chimpanzees$block )
)

# RStan fit
m2 <- map2stan(
  alist(
    pulled_left ~ dbinom(1,theta),
    logit(theta) <- a + bp*prosoc_left + bpc*condition*prosoc_left ,
    a ~ dnorm(0,10),
    bp ~ dnorm(0,10),
    bpc ~ dnorm(0,10)
  ) ,
  data=d, chains=2, cores=1 )

precis(m2)
summary(m2)
plot(m2)
pairs(m2)
