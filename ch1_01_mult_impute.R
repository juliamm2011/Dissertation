####################################################################################################
## Author: Julia Morris
## Description: Performs multiple imputation for HRS sample data, for variables used in Chapter 1.
## Output:  .csv of imputed HRS data for analyses.
####################################################################################################


#load packages
library(data.table)
library(Amelia)

#set directories
code_dir <- "C:/Users/julia/Dropbox/HRS/code_dir/"
data_dir <- "C:/Users/julia/Dropbox/HRS/data_dir/"
output_dir <- "C:/Users/julia/Dropbox/HRS/output_dir/"

#load data
hrs <- fread(paste0(data_dir, "randhrs_pslbq_recoded.csv"))
hrs <- hrs[, V1 := NULL]

#ch 1: psychosocial
hrs <- hrs[pslqb==1 & wave>=10]

#select vars
ids <- c("hhid", "pn")
cols <- c(ids,"rawtsamp","care_gc","care_par","chr_sle",
          "drinkswk","female","func_lim","siadlza",
          "hatota","hitot","priv_ins",
          "raceblack","racohbyr","raedyrs",
          "rageyb","rbmi","rcesdml","rconds",
          "rec_sle","rfairpoorsrh","rretired", "rmarpar",
          "care_sp","sup_neg_all","sup_pos_all","wave")
hrs <- hrs[, (cols), with=FALSE]

#perform imputation (m=5)
ordvars <- c("care_gc","care_par","drinkswk","func_lim","siadlza","raedyrs","rcesdml","rconds","rfairpoorsrh")
nomvars <- c("female","priv_ins","raceblack","racohbyr","rretired","rmarpar")
a_out <- amelia(hrs, m=5, idvars=ids, ts = "wave", ords=ordvars, noms=nomvars, polytime = 1,
                bound = rbind(c(3, 0, 1)), c(12, 0, Inf))

##average the result of the 5 imputations in order to create one data.table
#save each imputation as a separate data.table
a1 <- a_out$imputations[[1]]
a2 <- a_out$imputations[[2]]
a3 <- a_out$imputations[[3]]
a4 <- a_out$imputations[[4]]
a5 <- a_out$imputations[[5]]

#add row identifier variable to allow for row averaging
vec <- c(1:nrow(a1))
a1 <- cbind(a1,vec)
a2 <- cbind(a2,vec)
a3 <- cbind(a3,vec)
a4 <- cbind(a4,vec)
a5 <- cbind(a5,vec)

#rbind them together
hrsimp <- rbind(a1,a2,a3,a4,a5)
#average by row identifier
hrsimp <- hrsimp[, lapply(.SD, mean, na.rm=TRUE), by="vec"]
#remove row identifier
hrsimp <- hrsimp[, vec := NULL]

#round binary variables to 0 or 1, turn negative values to 0
cols <- c("care_gc","care_par","care_sp","rretired","priv_ins","rmarpar")
hrsimp <- hrsimp[, (cols) := round(.SD, 0), .SDcols=cols]

#perform recodes that need to happen after imputation (re: variable conversions & transitions)

#care-taker for any of the three groups
hrsimp[, care_any := rowSums(.SD, na.rm=T), .SDcols=c("care_gc","care_par","care_sp")]
hrsimp <- hrsimp[care_any>=1, care_any := 1]

#generate categorical var from H11ATOTA: does not have negative wealth (debt) (0), has debt (1)
#***Note: this variable is not particularly informative, but could think about
#***subsetting data based upon this criteria, and perhaps logging wealth variable for those
#***with positive wealth
#set negative values to 0 before logging
hrsimp[hatota>0, hwealthlog := hatota]
hrsimp[hatota<=0, hwealthlog := 0.0000001]
hrsimp[, hwealthlog := lapply(.SD, function(x) log(x)), .SDcols="hwealthlog"]

##create weekly drinking measure
#set negative values to 0 before logging
#hrsimp[drinkswk>0, drinkswklog := drinkswk]
#hrsimp[drinkswk<=0, drinkswklog := 0.0000001]
#hrsimp[, drinkswklog := lapply(.SD, function(x) log(x)), .SDcols="drinkswklog"]

#H11ITOT:W11 Income: Total HHold / R+Sp only
#set negative values to 0 before logging
hrsimp[hitot>0, hinclog := hitot]
hrsimp[hitot<=0, hinclog := 0.0000001]
hrsimp[, hinclog := lapply(.SD, function(x) log(x)), .SDcols="hinclog"]

#RAGEYB: Respondent's Age 
#hrsimp[, .N, by="rageyb"]
hrsimp[, age_100 := lapply(.SD, function(x) x/100), .SDcols="rageyb"]

#Transitions
#aggregate by those variables
ids <- c("hhid", "pn")
newdt <- hrsimp[, c(ids,"wave", "care_gc","care_par","care_sp","rretired"), with=FALSE]
newdt <- newdt[, lapply(.SD, sum, na.rm=T), by=ids]
#drop cases where the respondent is not in both waves
newdt <- newdt[wave==21]
#transition occur when someone moves between 1 and 0, so will have a sum of 1; else recode to 0
#Care
newdt[, care_trans := 0]
newdt[care_gc==1 | care_par==1 | care_sp==1, care_trans := 1]
#Paid Labor
newdt[, ret_trans := 0]
newdt[rretired==1, ret_trans := 1]

#Merge back in
#recode wave to 11
newdt[wave==21, wave := 11]
#subset to only id's and new variables
newdt <- newdt[, c(ids,"wave","care_trans","ret_trans"), with=FALSE]
hrsimp2 <- Reduce(function(x, y) {merge(x, y, by=c(ids,"wave"), all = TRUE)}, list(hrsimp,newdt))

#write to .csv
write.csv(hrsimp2, paste0(data_dir, "randhrs_pslbq_imputed_ch1.csv"))


