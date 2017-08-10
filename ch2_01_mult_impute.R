####################################################################################################
## Author: Julia Morris
## Description: Performs multiple imputation for HRS sample data, for variables used in Chapter 2.
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
hrs9 <- fread(paste0(data_dir, "randhrs_pslbq_recoded.csv"))
hrs9 <- hrs9[, V1 := NULL]
hrs1011 <- fread(paste0(data_dir, "randhrs_pslbq_imputed_ch1.csv"))
hrs1011 <- hrs1011[, V1 := NULL]

#only impute data needed from wave 9, rbind to imputed waves 10 & 11
hrs9 <- hrs9[wave==9]
#merge in psychosocial questionnaire id to wave 9
hrstemp <- hrs1011[wave==10]
hrstemp <- hrstemp[, c("hhid","pn"), with=FALSE]
hrstemp[, pslbq := 1]
hrs9 <- Reduce(function(x, y) {merge(x, y, by=c("hhid", "pn"), all.x = TRUE)}, list(hrs9, hrstemp))
#keep only psylbq respondents
hrs9 <- hrs9[pslbq==1]

#select vars
ids <- c("hhid", "pn")
cols <- c(ids, "rawtsamp",
          "drinkswk","female","func_lim","siadlza",
          "hatota","hitot","priv_ins",
          "raceblack","racohbyr","raedyrs",
          "rageyb","rbmi","rcesdml","rconds",
          "rfairpoorsrh","rretired", "rmarpar")
hrs9 <- hrs9[, (cols), with=FALSE]

#perform imputation
ordvars <- c("drinkswk","func_lim","siadlza","raedyrs","rcesdml","rconds","rfairpoorsrh")
nomvars <- c("female","priv_ins","raceblack","racohbyr","rretired","rmarpar")
a_out <- amelia(hrs9, m=5, idvars=ids, ords=ordvars, noms=nomvars,
                bound = rbind(c(3, 0, 1)), c(9, 0, Inf))

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
cols <- c("rretired","priv_ins","rmarpar")
hrsimp <- hrsimp[, (cols) := round(.SD, 0), .SDcols=cols]

#perform recodes that need to happen after imputation (re: variable conversions & transitions)

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

#add wave identifier
hrsimp[, wave := 9]

#rbind to waves 10 & 11
l = list(hrs1011, hrsimp)
hrsbind <- rbindlist(l, use.names=TRUE, fill=TRUE)

#cast wide
hrswide <- dcast(hrsbind, hhid + pn ~ wave, value.var=c("rawtsamp","female","raceblack","racohbyr","raedyrs",
                            "chr_sle","drinkswk","func_lim","siadlza",
                            "hatota","hitot","priv_ins","rageyb","rbmi",
                            "rcesdml","rconds","rec_sle","rfairpoorsrh","rretired",
                            "rmarpar","sup_neg_all","sup_pos_all","hwealthlog",
                            "hinclog","age_100", "care_trans","ret_trans",
                            "care_gc","care_par","care_sp","care_any"))

#average identifier variables across waves, in case some changed during imputation
hrswide[, rawtsamp := rowMeans(.SD, na.rm = TRUE), .SDcols = c("rawtsamp_9", "rawtsamp_10", "rawtsamp_11")]
hrswide[, female := rowMeans(.SD, na.rm = TRUE), .SDcols = c("female_9", "female_10", "female_11")]
hrswide[, raceblack := rowMeans(.SD, na.rm = TRUE), .SDcols = c("raceblack_9", "raceblack_10", "raceblack_11")]
hrswide[, racohbyr := rowMeans(.SD, na.rm = TRUE), .SDcols = c("racohbyr_9", "racohbyr_10", "racohbyr_11")]
hrswide[, raedyrs := rowMeans(.SD, na.rm = TRUE), .SDcols = c("raedyrs_9", "raedyrs_10", "raedyrs_11")]
#round to full digits
cols <- c("female","raceblack","racohbyr","raedyrs")
hrswide <- hrswide[, (cols) := round(.SD, 0), .SDcols=cols]
#drop wave-specific variables
cols <- c("rawtsamp_9", "rawtsamp_10", "rawtsamp_11",
             "female_9", "female_10", "female_11",
             "raceblack_9", "raceblack_10", "raceblack_11",
             "racohbyr_9", "racohbyr_10", "racohbyr_11",
             "raedyrs_9", "raedyrs_10", "raedyrs_11")
hrstemp <- hrswide[, !(cols), with=FALSE]

#write to .csv
write.csv(hrstemp, paste0(data_dir, "randhrs_pslbq_imputed_ch2.csv"))


