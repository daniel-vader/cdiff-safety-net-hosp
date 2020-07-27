# Model relationship between C diff infection and 
# geographic/demographic factors 

# Depends on data output from cdiff_maps.R

# Author: Daniel Vader

# Load packages
library(robustlmm)
library(lme4)
library(tidyr)
library(boot)

################################################################################
### Modeling
################################################################################

cdiff <- readRDS("cdiff_glmer_dat.rds")

# Median OR function
bootMOR <- function(model) {
  return(exp(.6745*sqrt(2*getME(model,"theta")^2))) #exp(0.95*getME(model,"theta")))
}

# bootstrap 95% CI around median OR
mor95 <- function(model){
  boot_model <- bootMer(model, bootMOR, nsim=1000, parallel="multicore", ncpus=4)
  b <- boot::boot.ci(boot_model, type="norm", index=1)
  return(b)
}


### Empty model
m1 <- glmer(cdiff ~ (1 | Tract), family=binomial(), data=cdiff)

bootMOR(m1) # get median OR for random effects
mor95(m1) # bootstrap 95% CI around median OR


### Model with random intercept and zip-level sdi
m2 <- glmer(cdiff ~ NDI + (1 | Tract), family=binomial(), data=cdiff)

# Get ORs with CI for fixed effects
tidy(m2,conf.int=TRUE,exponentiate=TRUE,effects="fixed")

# Get ICC and MOR
bootMOR(m2)
mor95(m2) # bootstrap 95% ci for mor


### Model with race / ethnicity, zip level sdi, random intercept
m3 <- glmer(cdiff ~ reth + NDI + (1 | Tract), family=binomial(), data=cdiff)

# Get ORs with CI for fixed effects
tidy(m3,conf.int=TRUE,exponentiate=TRUE,effects="fixed")

m3.sd <- m3.sum$varcor$Tract %>% attr("stddev")

# Compute intraclass correlation and median odds ratio
bootMOR(m3)
mor95(m3) # bootstrap 95% ci for mor


### Model with random intercept, zip-level sdi, race / ethnicity, gender, 
  # insurance type, and referal type
m4 <- glmer(cdiff ~ reth + insur2 + refer2 + NDI + (1 | Tract), 
            family=binomial(), data=cdiff)

# Get ORs with CI for fixed effects
tidy(m4,conf.int=TRUE,exponentiate=TRUE,effects="fixed")

# Compute intraclass correlation and median odds ratio
bootMOR(m4)
mor95(m4) # bootstrap 95% ci for mor


### Model with random intercept, zip-level sdi, race / ethnicity, gender, insurance type,
#  referal type, prior abx, and prior proton pump inhibitor use
m5 <- glmer(cdiff ~ reth + insur2 + refer2 + NDI + prior_abx + prior_proton + 
              (1 | Tract), 
            family=binomial(), data=cdiff)

# Get ORs with CI for fixed effects
tidy(m5,conf.int=TRUE,exponentiate=TRUE,effects="fixed")

# Compute intraclass correlation and median odds ratio
bootMOR(m5)
mor95(m5) # bootstrap 95% ci for mor


################################################################################
### Table 1
################################################################################
library(tidyr)
library(sf)

cdiff <- readRDS("cdiff_glmer_dat.rds") %>% as.data.frame()

tabvars <- c("reth", "insur2", "refer2", "prior_abx", "prior_proton")

for(i in 1:length(tabvars)){
  t <- table(cdiff[,tabvars[i]], cdiff[,"cdiff"]) # get freqs
  p <- chisq.test(t) # get p-val
  tp <- t %>% prop.table(2) %>% as.data.frame.matrix() # get percentages
  t <- t %>% as.data.frame.matrix()
  t$total <- t[,1] + t[,2]
  t$total_per <- t$total/sum(t$total) %>% as.vector()
  t$p <- p$p.value
  
  # bind freqs, percents, and p-values in order
  tpp <- cbind(t[1], tp[1], t[2], tp[2], t[3], t[4], t[5])
  
  if(i == 1){
    freqtab <- tpp
  }else{
    freqtab <- rbind(freqtab, tpp)
  }
}

freqtab <- freqtab %>% as.matrix() 

# Add continuous variables
tabvars_c  <- c("NDI", "age")
for(i in 1:length(tabvars_c)){
  m1 <- mean(cdiff[cdiff$cdiff == 0,tabvars_c[i]])
  m2 <- mean(cdiff[cdiff$cdiff == 1,tabvars_c[i]])
  m3 <- mean(cdiff[,tabvars_c[i]])
  sd1 <- sd(cdiff[cdiff$cdiff == 0,tabvars_c[i]])
  sd2 <- sd(cdiff[cdiff$cdiff == 1,tabvars_c[i]])
  sd3 <- sd(cdiff[,tabvars_c[i]])
  tt <- t.test(cdiff[,tabvars_c[i]] ~ cdiff$cdiff)
  p <- tt$p.value
  tpp <- cbind(m1, sd1, m2, sd2, m3, sd3, p)
  freqtab <- rbind(freqtab, tpp)
  rownames(freqtab)[nrow(freqtab)] <- tabvars_c[i]
}

freqtab <- freqtab %>% as.data.frame()
colnames(freqtab) <- c("control_n_mean", "control_per_sd", "case_n_mean",
                       "case_per_sd", "total_n_mean", "total_per_sd", "p")


write.csv(freqtab, "freqtab.csv")

