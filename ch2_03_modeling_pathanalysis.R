####################################################################################################
## Author: Julia Morris
## Description: Performs path analyses for Chapter 2.
## Output:  Models for Chapter 2.
####################################################################################################

#load packages
library(data.table)
library(Hmisc)
library(ppcor)

#set directories
code_dir <- "C:/Users/julia/Dropbox/HRS/code_dir/"
data_dir <- "C:/Users/julia/Dropbox/HRS/data_dir/"
output_dir <- "C:/Users/julia/Dropbox/HRS/output_dir/"

#load data
hrs <- fread(paste0(data_dir, "randhrs_pslbq_imputed_ch2.csv"))
hrs <- hrs[, V1 := NULL]

#make difference variables
hrs[, cesd_diff := rcesdml_11 - rcesdml_10]
hrs[, drink_diff := drinkswk_11 - drinkswk_10]
hrs[, bmi_diff := rbmi_11 - rbmi_10]

###Path Analysis
#subset data by sex and rate
mwhite <- hrs[female==0&raceblack==0]
femwhite <- hrs[female==1&raceblack==0]
mblack <- hrs[female==0&raceblack==1]
femblack <- hrs[female==1&raceblack==1]


###White Males
##pcor
mwhite <- mwhite[,c("rec_sle_11","care_trans_11","sup_neg_all_11","rbmi_11","drinkswk_11","cesd_diff","hwealthlog_11","raedyrs","age_100_11",
                    "rcesdml_11","drinkswk_11","rbmi_11", "rawtsamp"),with=FALSE]
#rcorr(as.matrix(mwhite),type="pearson")

###
#route 1a - alcohol
mwhite1a <- (glm(drinkswk_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, mwhite, weights = rawtsamp, family = negative.binomial(theta = 1)))
#route 1b - bmi
mwhite1b <- (glm(rbmi_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, mwhite, weights = rawtsamp, family = gaussian(link = "identity")))
#route 2 - option 1 - rcesdml with only alcohol and bmi
mwhite1 <- glm(rcesdml_11 ~ drinkswk_11 + rbmi_11, mwhite, weights = rawtsamp, family = negative.binomial(theta = 1))
#route 2 - option 2 - full saturation model
mwhite2 <- glm(rcesdml_11 ~ drinkswk_11 + rbmi_11 + care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, mwhite, weights = rawtsamp, family = negative.binomial(theta = 1))
#bootstrap the indirect effect - option 1
J = 10
N = dim(mwhite)[1]
results.a = numeric(J)
results.b = numeric(J)
for(i in 1:J) {
  Cases = sample(1:N, N, replace=T)
  tempdf = mwhite[Cases,]
  a = coef(glm(drinkswk_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  b = coef(glm(rbmi_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = gaussian(link = "identity")))[2]
  c = coef(glm(rcesdml_11 ~ drinkswk_11 + rbmi_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  results.a[i] = a * c
  results.b[i] = b * c
}
#summary(results.a)
#summary(results.b)
round(quantile(results.a,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)
round(quantile(results.b,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)
#bootstrap the indirect effect - option 2
J = 10
N = dim(mwhite)[1]
results.a = numeric(J)
results.b = numeric(J)
for(i in 1:J) {
  Cases = sample(1:N, N, replace=T)
  tempdf = mwhite[Cases,]
  a = coef(glm(drinkswk_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  b = coef(glm(rbmi_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = gaussian(link = "identity")))[2]
  c = coef(glm(rcesdml_11 ~ drinkswk_11 + rbmi_11 + care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  results.a[i] = a * c
  results.b[i] = b * c
}
#summary(results.a)
#summary(results.b)
round(quantile(results.a,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)
round(quantile(results.b,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)


###White Females
##pcor
femwhite <- femwhite[,c("rec_sle_11","care_trans_11","sup_neg_all_11","rbmi_11","drinkswk_11","cesd_diff","hwealthlog_11","raedyrs","age_100_11",
                    "rcesdml_11","drinkswk_11","rbmi_11", "rawtsamp"),with=FALSE]
#rcorr(as.matrix(femwhite),type="pearson")

###
#route 1a - alcohol
femwhite1a <- (glm(drinkswk_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, femwhite, weights = rawtsamp, family = negative.binomial(theta = 1)))
#route 1b - bmi
femwhite1b <- (glm(rbmi_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, femwhite, weights = rawtsamp, family = gaussian(link = "identity")))
#route 2 - option 1 - rcesdml with only alcohol and bmi
femwhite1 <- (glm(rcesdml_11 ~ drinkswk_11 + rbmi_11, femwhite, weights = rawtsamp, family = negative.binomial(theta = 1)))
#route 2 - option 2 - full saturation model
femwhite2 <- (glm(rcesdml_11 ~ drinkswk_11 + rbmi_11 + care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, femwhite, weights = rawtsamp, family = negative.binomial(theta = 1)))
#bootstrap the indirect effect - option 1
J = 10
N = dim(femwhite)[1]
results.a = numeric(J)
results.b = numeric(J)
for(i in 1:J) {
  Cases = sample(1:N, N, replace=T)
  tempdf = femwhite[Cases,]
  a = coef(glm(drinkswk_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  b = coef(glm(rbmi_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = gaussian(link = "identity")))[2]
  c = coef(glm(rcesdml_11 ~ drinkswk_11 + rbmi_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  results.a[i] = a * c
  results.b[i] = b * c
}
#summary(results.a)
#summary(results.b)
round(quantile(results.a,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)
round(quantile(results.b,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)
#bootstrap the indirect effect - option 2
J = 10
N = dim(femwhite)[1]
results.a = numeric(J)
results.b = numeric(J)
for(i in 1:J) {
  Cases = sample(1:N, N, replace=T)
  tempdf = femwhite[Cases,]
  a = coef(glm(drinkswk_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  b = coef(glm(rbmi_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = gaussian(link = "identity")))[2]
  c = coef(glm(rcesdml_11 ~ drinkswk_11 + rbmi_11 + care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  results.a[i] = a * c
  results.b[i] = b * c
}
#summary(results.a)
#summary(results.b)
round(quantile(results.a,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)
round(quantile(results.b,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)


###Black Males
##pcor
mblack <- mblack[,c("rec_sle_11","care_trans_11","sup_neg_all_11","rbmi_11","drinkswk_11","cesd_diff","hwealthlog_11","raedyrs","age_100_11",
                    "rcesdml_11","drinkswk_11","rbmi_11", "rawtsamp"),with=FALSE]
#rcorr(as.matrix(mblack),type="pearson")

###
#route 1a - alcohol
mblack1a <- (glm(drinkswk_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, mblack, weights = rawtsamp, family = negative.binomial(theta = 1)))
#route 1b - bmi
mblack1b <- (glm(rbmi_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, mblack, weights = rawtsamp, family = gaussian(link = "identity")))
#route 2 - option 1 - rcesdml with only alcohol and bmi
mblack1 <- (glm(rcesdml_11 ~ drinkswk_11 + rbmi_11, mblack, weights = rawtsamp, family = negative.binomial(theta = 1)))
#route 2 - option 2 - full saturation model
mblack2 <- (glm(rcesdml_11 ~ drinkswk_11 + rbmi_11 + care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, mblack, weights = rawtsamp, family = negative.binomial(theta = 1)))
#bootstrap the indirect effect - option 1
J = 10
N = dim(mblack)[1]
results.a = numeric(J)
results.b = numeric(J)
for(i in 1:J) {
  Cases = sample(1:N, N, replace=T)
  tempdf = mblack[Cases,]
  a = coef(glm(drinkswk_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  b = coef(glm(rbmi_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = gaussian(link = "identity")))[2]
  c = coef(glm(rcesdml_11 ~ drinkswk_11 + rbmi_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  results.a[i] = a * c
  results.b[i] = b * c
}
#summary(results.a)
#summary(results.b)
round(quantile(results.a,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)
round(quantile(results.b,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)
#bootstrap the indirect effect - option 2
J = 10
N = dim(mblack)[1]
results.a = numeric(J)
results.b = numeric(J)
for(i in 1:J) {
  Cases = sample(1:N, N, replace=T)
  tempdf = mblack[Cases,]
  a = coef(glm(drinkswk_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  b = coef(glm(rbmi_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = gaussian(link = "identity")))[2]
  c = coef(glm(rcesdml_11 ~ drinkswk_11 + rbmi_11 + care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  results.a[i] = a * c
  results.b[i] = b * c
}
#summary(results.a)
#summary(results.b)
round(quantile(results.a,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)
round(quantile(results.b,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)


###Black Females
##pcor
femblack <- femblack[,c("rec_sle_11","care_trans_11","sup_neg_all_11","rbmi_11","drinkswk_11","cesd_diff","hwealthlog_11","raedyrs","age_100_11",
                    "rcesdml_11","drinkswk_11","rbmi_11", "rawtsamp"),with=FALSE]
#rcorr(as.matrix(femblack),type="pearson")

###
#route 1a - alcohol
femblack1a <- (glm(drinkswk_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, femblack, weights = rawtsamp, family = negative.binomial(theta = 1)))
#route 1b - bmi
femblack1b <- (glm(rbmi_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, femblack, weights = rawtsamp, family = gaussian(link = "identity")))
#route 2 - option 1 - rcesdml with only alcohol and bmi
femblack1 <- (glm(rcesdml_11 ~ drinkswk_11 + rbmi_11, femblack, weights = rawtsamp, family = negative.binomial(theta = 1)))
#route 2 - option 2 - full saturation model
femblack2 <- (glm(rcesdml_11 ~ drinkswk_11 + rbmi_11 + care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, femblack, weights = rawtsamp, family = negative.binomial(theta = 1)))
#bootstrap the indirect effect - option 1
J = 10
N = dim(femblack)[1]
results.a = numeric(J)
results.b = numeric(J)
for(i in 1:J) {
  Cases = sample(1:N, N, replace=T)
  tempdf = femblack[Cases,]
  a = coef(glm(drinkswk_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  b = coef(glm(rbmi_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = gaussian(link = "identity")))[2]
  c = coef(glm(rcesdml_11 ~ drinkswk_11 + rbmi_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  results.a[i] = a * c
  results.b[i] = b * c
}
#summary(results.a)
#summary(results.b)
round(quantile(results.a,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)
round(quantile(results.b,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)
#bootstrap the indirect effect - option 2
J = 10
N = dim(femblack)[1]
results.a = numeric(J)
results.b = numeric(J)
for(i in 1:J) {
  Cases = sample(1:N, N, replace=T)
  tempdf = femblack[Cases,]
  a = coef(glm(drinkswk_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  b = coef(glm(rbmi_11 ~ care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = gaussian(link = "identity")))[2]
  c = coef(glm(rcesdml_11 ~ drinkswk_11 + rbmi_11 + care_trans_11 + sup_neg_all_11 + hwealthlog_11 + raedyrs + age_100_11, tempdf, weights = rawtsamp, family = negative.binomial(theta = 1)))[2]
  results.a[i] = a * c
  results.b[i] = b * c
}
#summary(results.a)
#summary(results.b)
round(quantile(results.a,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)
round(quantile(results.b,c(0.0005,0.005,0.025,0.5,.975,.995,.9995)),3)


##store the coefficients, so I can create a .csv of output
coefs.mwhite1a <- data.frame(coef(summary(mwhite1a))) 
coefs.mwhite1b <- data.frame(coef(summary(mwhite1b))) 
coefs.mwhite1 <- data.frame(coef(summary(mwhite1))) 
coefs.mwhite2 <- data.frame(coef(summary(mwhite2))) 
coefs.femwhite1a <- data.frame(coef(summary(femwhite1a))) 
coefs.femwhite1b <- data.frame(coef(summary(femwhite1b))) 
coefs.femwhite1 <- data.frame(coef(summary(femwhite1))) 
coefs.femwhite2 <- data.frame(coef(summary(femwhite2))) 
coefs.mblack1a <- data.frame(coef(summary(mblack1a))) 
coefs.mblack1b <- data.frame(coef(summary(mblack1b))) 
coefs.mblack1 <- data.frame(coef(summary(mblack1))) 
coefs.mblack2 <- data.frame(coef(summary(mblack2))) 
coefs.femblack1a <- data.frame(coef(summary(femblack1a))) 
coefs.femblack1b <- data.frame(coef(summary(femblack1b))) 
coefs.femblack1 <- data.frame(coef(summary(femblack1))) 
coefs.femblack2 <- data.frame(coef(summary(femblack2))) 

#create tables of coefficients by model
#model 1a
res1a <- cbind(coefs.mwhite1a, coefs.femwhite1a, coefs.mblack1a, coefs.femblack1a)
#model 1b
res1b <- cbind(coefs.mwhite1b, coefs.femwhite1b, coefs.mblack1b, coefs.femblack1b)
#model 1
res1 <- cbind(coefs.mwhite1, coefs.femwhite1, coefs.mblack1, coefs.femblack1)
#model 2
res2 <- cbind(coefs.mwhite2, coefs.femwhite2, coefs.mblack2, coefs.femblack2)
#rbind
resall <- rbind(res1a, res1b, res1, res2)

#white to .csv
write.csv(resall, paste0(output_dir, "ch2_resall.csv"))

##testing differences in the saturated models across groups
#split datasets to create pairs; allows me to test interaction terms within split datasets to determine
# if path analyses vary by race and sex
hrsmale <- hrs[female==0]
hrsfemale <- hrs[female==1]
hrswhite <- hrs[raceblack==0]
hrsblack <- hrs[raceblack==1]

#run the models with the interaction terms
white_bysex <- glm(rcesdml_11 ~ drinkswk_11*female + rbmi_11*female + 
                    care_trans_11*female + sup_neg_all_11*female + 
                    hwealthlog_11*female + raedyrs*female + age_100_11*female, 
                  hrswhite, weights = rawtsamp, family = negative.binomial(theta = 1))
black_bysex <- glm(rcesdml_11 ~ drinkswk_11*female + rbmi_11*female + 
                     care_trans_11*female + sup_neg_all_11*female + 
                     hwealthlog_11*female + raedyrs*female + age_100_11*female, 
                   hrsblack, weights = rawtsamp, family = negative.binomial(theta = 1))
male_byrace <- glm(rcesdml_11 ~ drinkswk_11*raceblack + rbmi_11*raceblack + 
                  care_trans_11*raceblack + sup_neg_all_11*raceblack + 
                  hwealthlog_11*raceblack + raedyrs*raceblack + age_100_11*raceblack, 
                hrsmale, weights = rawtsamp, family = negative.binomial(theta = 1))
female_byrace <- glm(rcesdml_11 ~ drinkswk_11*raceblack + rbmi_11*raceblack + 
                      care_trans_11*raceblack + sup_neg_all_11*raceblack + 
                      hwealthlog_11*raceblack + raedyrs*raceblack + age_100_11*raceblack, 
                    hrsfemale, weights = rawtsamp, family = negative.binomial(theta = 1))

