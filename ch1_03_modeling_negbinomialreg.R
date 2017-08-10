####################################################################################################
## Author: Julia Morris
## Description: Performs poisson regression analyses for Chapter 1.
## Output:  Models for Chapter 1.
## Note: Adaptable code for rootogram and observed vs. expected plots at end.
####################################################################################################


#load packages
library(data.table)
library(plyr)
library(MASS)
library(countreg)
library(magrittr)

#set directories
code_dir <- "C:/Users/julia/Dropbox/HRS/code_dir/"
data_dir <- "C:/Users/julia/Dropbox/HRS/data_dir/"
output_dir <- "C:/Users/julia/Dropbox/HRS/output_dir/"

#load data
hrs <- fread(paste0(data_dir, "randhrs_pslbq_imputed_ch1.csv"))
hrs <- hrs[, V1 := NULL]

#subset to wave 11 for analyses
hrs <- hrs[wave==11]

#create interaction terms for models
#functional limitations and retirement transition
hrs[, funcXret := func_lim * ret_trans]
#functional limitations and care transition
hrs[, funcXcare := func_lim * care_trans]

#negative social support and retirement transition
hrs[, supnegXret := sup_neg_all * ret_trans]
#negative social support and care transition
hrs[, supnegXcare := sup_neg_all * care_trans]

#positive social support and retirement transition
hrs[, supposXret := sup_pos_all * ret_trans]
#positive social support and care transition
hrs[, supposXcare := sup_pos_all * care_trans]


#split poisson regression models

#model 1a: transition variables: retirement transition
#break up by sex and race, then fit the model to each piece
glm1a <- dlply(hrs, c("female","raceblack"),
             function(hrs)
               glm.nb(rcesdml ~ 
                    age_100 +
                    hwealthlog +
                    hinclog +
                    raedyrs +
                    ret_trans 
                  , data = hrs, weights = rawtsamp))
#apply coef to eah model and return a df
glm1a_coef <- ldply(glm1a, coef)
#print summary
l_ply(glm1a, summary, .print = T)

#model 1b: transition variables: care transition
#break up by sex and race, then fit the model to each piece
glm1b <- dlply(hrs, c("female","raceblack"),
              function(hrs)
                glm.nb(rcesdml ~ 
                     age_100 +
                     hwealthlog +
                     hinclog +
                     raedyrs +
                     care_trans 
                   , data = hrs, weights = rawtsamp))
#apply coef to eah model and return a df
glm1b_coef <- ldply(glm1b, coef)
#print summary
l_ply(glm1b, summary, .print = T)

#model 1c: transition variables: retirement transition AND care transition
#break up by sex and race, then fit the model to each piece
glm1c <- dlply(hrs, c("female","raceblack"),
              function(hrs)
                glm.nb(rcesdml ~ 
                     age_100 +
                     hwealthlog +
                     hinclog +
                     raedyrs +
                     care_trans + 
                     ret_trans 
                   , data = hrs, weights = rawtsamp))
#apply coef to eah model and return a df
glm1c_coef <- ldply(glm1c, coef)
#print summary
l_ply(glm1c, summary, .print = T)

#model 2a: social support variables: positive social support
#break up by sex and race, then fit the model to each piece
glm2a <- dlply(hrs, c("female","raceblack"),
              function(hrs)
                glm.nb(rcesdml ~ 
                     age_100 +
                     hwealthlog +
                     hinclog +
                     raedyrs +
                     sup_pos_all 
                   , data = hrs, weights = rawtsamp))
#apply coef to eah model and return a df
glm2a_coef <- ldply(glm2a, coef)
#print summary
l_ply(glm2a, summary, .print = T)

#model 2b: social support variables: negative social support
#break up by race, then fit the model to each piece
glm2b <- dlply(hrs, c("female","raceblack"),
              function(hrs)
                glm.nb(rcesdml ~ 
                     age_100 +
                     hwealthlog +
                     hinclog +
                     raedyrs +
                     sup_neg_all 
                   , data = hrs, weights = rawtsamp))
#apply coef to eah model and return a df
glm2b_coef <- ldply(glm2b, coef)
#print summary
l_ply(glm2b, summary, .print = T)

#model 2c: social support variables: positive social support AND negative social support
#break up by race, then fit the model to each piece
glm2c <- dlply(hrs, c("female","raceblack"),
              function(hrs)
                glm.nb(rcesdml ~ 
                     age_100 +
                     hwealthlog +
                     hinclog +
                     raedyrs +
                     sup_neg_all +
                     sup_pos_all 
                     , data = hrs, weights = rawtsamp))
#apply coef to eah model and return a df
glm2c_coef <- ldply(glm2c, coef)
#print summary
l_ply(glm2c, summary, .print = T)

#model 3a: transition variables: retirement transition AND retirement status
#break up by sex and race, then fit the model to each piece
glm3a <- dlply(hrs, c("female","raceblack"),
               function(hrs)
                 glm.nb(rcesdml ~ 
                       age_100 +
                       hwealthlog +
                       hinclog +
                       raedyrs +
                       ret_trans +
                       rretired
                     , data = hrs, weights = rawtsamp))
#apply coef to eah model and return a df
glm3a_coef <- ldply(glm3a, coef)
#print summary
l_ply(glm3a, summary, .print = T)

#model 3b: transition variables: care transition AND care status
#break up by sex and race, then fit the model to each piece
glm3b <- dlply(hrs, c("female","raceblack"),
               function(hrs)
                 glm.nb(rcesdml ~ 
                       age_100 +
                       hwealthlog +
                       hinclog +
                       raedyrs +
                       care_trans +
                       care_any
                     , data = hrs, weights = rawtsamp))
#apply coef to eah model and return a df
glm3b_coef <- ldply(glm3b, coef)
#print summary
l_ply(glm3b, summary, .print = T)

#model 3c: transition variables: retirement transition and status AND care transition and status
#break up by sex and race, then fit the model to each piece
glm3c <- dlply(hrs, c("female","raceblack"),
               function(hrs)
                 glm.nb(rcesdml ~ 
                       age_100 +
                       hwealthlog +
                       hinclog +
                       raedyrs +
                       care_trans + 
                       ret_trans +
                       care_any +
                       rretired
                     , data = hrs, weights = rawtsamp))
#apply coef to eah model and return a df
glm3c_coef <- ldply(glm3c, coef)
#print summary
l_ply(glm3c, summary, .print = T)


#model 4a: social support / LT interactions: positive social support & care transition
#break up by sex and race, then fit the model to each piece
glm4a <- dlply(hrs, c("female","raceblack"),
              function(hrs)
                glm.nb(rcesdml ~ 
                     age_100 +
                     hwealthlog +
                     hinclog +
                     raedyrs +
                     care_trans + 
                     sup_pos_all +
                     supposXcare 
                   , data = hrs, weights = rawtsamp))
#apply coef to eah model and return a df
glm4a_coef <- ldply(glm4a, coef)
#print summary
l_ply(glm4a, summary, .print = T)

#model 4b: social support / LT interactions: negative social support & care transition
#break up by race, then fit the model to each piece
glm4b <- dlply(hrs, c("female","raceblack"),
              function(hrs)
                glm.nb(rcesdml ~ 
                     age_100 +
                     hwealthlog +
                     hinclog +
                     raedyrs +
                     care_trans + 
                     sup_neg_all +
                     supnegXcare 
                   , data = hrs, weights = rawtsamp))
#apply coef to eah model and return a df
glm4b_coef <- ldply(glm4b, coef)
#print summary
l_ply(glm4b, summary, .print = T)

#model 4c: social support / LT interactions: positive and negative social support and care transition
#break up by race, then fit the model to each piece
glm4c <- dlply(hrs, c("female","raceblack"),
              function(hrs)
                glm.nb(rcesdml ~ 
                     age_100 +
                     hwealthlog +
                     hinclog +
                     raedyrs +
                     care_trans + 
                     sup_neg_all +
                     sup_pos_all +
                     supnegXcare +
                     supposXcare 
                   , data = hrs, weights = rawtsamp))
#apply coef to eah model and return a df
glm4c_coef <- ldply(glm4c, coef)
#print summary
l_ply(glm4c, summary, .print = T)


#model 4d: social support / LT interactions: positive social support & retirement transition
#break up by sex and race, then fit the model to each piece
glm4d <- dlply(hrs, c("female","raceblack"),
              function(hrs)
                glm.nb(rcesdml ~ 
                     age_100 +
                     hwealthlog +
                     hinclog +
                     raedyrs +
                     ret_trans +
                     sup_pos_all +
                     supposXret 
                   , data = hrs, weights = rawtsamp))
#apply coef to eah model and return a df
glm4d_coef <- ldply(glm4d, coef)
#print summary
l_ply(glm4d, summary, .print = T)

#model 4e: social support / LT interactions: negative social support & retirement transition
#break up by race, then fit the model to each piece
glm4e <- dlply(hrs, c("female","raceblack"),
              function(hrs)
                glm.nb(rcesdml ~ 
                     age_100 +
                     hwealthlog +
                     hinclog +
                     raedyrs +
                     ret_trans +
                     sup_neg_all +
                     supnegXret
                   , data = hrs, weights = rawtsamp))
#apply coef to eah model and return a df
glm4e_coef <- ldply(glm4e, coef)
#print summary
l_ply(glm4e, summary, .print = T)

#model 4f: social support / LT interactions: positive and negative social support & retirement transition
#break up by race, then fit the model to each piece
glm4f <- dlply(hrs, c("female","raceblack"),
              function(hrs)
                glm.nb(rcesdml ~ 
                     age_100 +
                     hwealthlog +
                     hinclog +
                     raedyrs +
                     ret_trans +
                     sup_neg_all +
                     sup_pos_all +
                     supnegXret +
                     supposXret 
                   , data = hrs, weights = rawtsamp))
#apply coef to eah model and return a df
glm4f_coef <- ldply(glm4f, coef)
#print summary
l_ply(glm4f, summary, .print = T)


###Model Fit Assessment
##Rootograms
#Selected model: Model 3c
#all respondents
glm3c <- glm.nb(rcesdml ~ age_100 + hwealthlog + hinclog + raedyrs + care_trans + 
                 ret_trans + care_any + rretired, 
               data = hrs, weights = rawtsamp)
countreg::rootogram(glm3c)

#white males
glm3cmw <- glm.nb(rcesdml ~ age_100 + hwealthlog + hinclog + raedyrs + care_trans + 
                   ret_trans + care_any + rretired, 
                 data = hrs[female==0&raceblack==0], weights = rawtsamp)
countreg::rootogram(glm3cmw)

#white females
glm3cfemw <- glm.nb(rcesdml ~ age_100 + hwealthlog + hinclog + raedyrs + care_trans + 
                     ret_trans + care_any + rretired,  
                   data = hrs[female==1&raceblack==0], weights = rawtsamp)
countreg::rootogram(glm3cfemw)

#black males
glm3cbw <- glm.nb(rcesdml ~ age_100 + hwealthlog + hinclog + raedyrs + care_trans + 
                   ret_trans + care_any + rretired, 
                 data = hrs[female==0&raceblack==1], weights = rawtsamp)
countreg::rootogram(glm3cbw)

#black females
glm3cfemb <- glm.nb(rcesdml ~ age_100 + hwealthlog + hinclog + raedyrs + care_trans + 
                     ret_trans + care_any + rretired, 
                   data = hrs[female==1&raceblack==1], weights = rawtsamp)
countreg::rootogram(glm3cfemb)

#histrogram of observed veruss expected counts
op <- par(mfrow=c(1,2))
set.seed(98056)

#white males
hrs$rcesdml %>% `[`(hrs$female==0&hrs$raceblack==0) %>% 
  table() %>% barplot(main = "Observed White Males")
rnbinom(n = 1999, size = glm3c$theta, mu = sum(coef(glm3c))) %>% 
  table() %>%  barplot(main = "Simulated White Males")

#white females
hrs$rcesdml %>% `[`(hrs$female==1&hrs$raceblack==0) %>% 
  table() %>% barplot(main = "Observed White Females")
rnbinom(n = 1999, size = glm3c$theta, mu = sum(coef(glm3c))) %>% 
  table() %>%  barplot(main = "Simulated White Females")

#black males
hrs$rcesdml %>% `[`(hrs$female==0&hrs$raceblack==1) %>% 
  table() %>% barplot(main = "Observed Black Males")
rnbinom(n = 172, size = glm3c$theta, mu = sum(coef(glm3c))) %>% 
  table() %>%  barplot(main = "Simulated Black Males")

#black females
hrs$rcesdml %>% `[`(hrs$female==1&hrs$raceblack==1) %>% 
  table() %>% barplot(main = "Observed Black Females")
rnbinom(n = 455, size = glm3c$theta, mu = sum(coef(glm3c))) %>% 
  table() %>%  barplot(main = "Simulated Black Females")
par(op)
