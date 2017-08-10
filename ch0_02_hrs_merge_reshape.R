####################################################################################################
## Author: Julia Morris
## Description: Merge psychosocial leave-behind questionnaire, Section E and Section F files with
##              HRS formatted file. Melt final final.
## Output:  Long merged HRS file.
## NOTE: 
####################################################################################################

rm(list=ls())

#load libraries
library(data.table)
library(splitstackshape)
library(plyr)

#set directories
code_dir <- "C:/Users/julia/Dropbox/HRS/code_dir/"
raw_data_dir <- "C:/Users/julia/Dropbox/HRS/raw_data_dir/"
data_dir <- "C:/Users/julia/Dropbox/HRS/data_dir/"
output_dir <- "C:/Users/julia/Dropbox/HRS/output_dir/"

#load data files
p10 <- fread(paste0(raw_data_dir, "/H10LB_R.csv")) #wave 10
e10 <- fread(paste0(raw_data_dir, "/H10E_H.csv")) #wave 10
f10 <- fread(paste0(raw_data_dir, "/H10F_R.csv")) #wave 10
p12 <- fread(paste0(raw_data_dir, "/H12LB_R.csv")) #wave 11
e12 <- fread(paste0(raw_data_dir, "/H12E_H.csv")) #wave 11
f12 <- fread(paste0(raw_data_dir, "/H12F_R.csv")) #wave 11

#remove non-numeric characters
p10 <- p10[, lapply(.SD, as.numeric)]
e10 <- e10[, lapply(.SD, as.numeric)]
f10 <- f10[, lapply(.SD, as.numeric)]
p12 <- p12[, lapply(.SD, as.numeric)]
e12 <- e12[, lapply(.SD, as.numeric)]
f12 <- f12[, lapply(.SD, as.numeric)]

#select variables to keep: p10
vars_keep_p10 <- c("HHID", "PN", "MSUBHH","MCSR","MFAMR","MFINR","MLB004","MLB007","MLB011",
                   "MLB015","MLB006","MLB010","MLB014","MLB018","MLB009A","MLB009B",
                   "MLB009C","MLB013A","MLB013B","MLB013C","MLB017A","MLB017B","MLB017C",
                   "MLB005A","MLB005B","MLB005C","MLB005D","MLB005E","MLB005F","MLB005G",
                   "MLB008A","MLB008B","MLB008C","MLB008D","MLB008E","MLB008F","MLB008G",
                   "MLB012A","MLB012B","MLB012C","MLB012D","MLB012E","MLB012F","MLB012G",
                   "MLB016A","MLB016B","MLB016C","MLB016D","MLB016E","MLB016F","MLB016G",
                   "MLB020A","MLB020B","MLB020C","MLB020E","MLB020D","MLB020F","MLB020G",
                   "MLB020H","MLB020I","MLB020J","MLB020K","MLB023A","MLB023B","MLB023C",
                   "MLB023D","MLB023E","MLB029B1","MLB029B3","MLB029B7","MLB029B8",
                   "MLB029B2","MLB029B4","MLB029B5","MLB029B6","MLB030A","MLB030B",
                   "MLB030C","MLB030D","MLB030E","MLB030F","MLB031M1","MLB031M2","MLB031M3",
                   "MLB031M4","MLB031M5","MLB031M6","MLB031M7","MLB031M8","MLB031M9",
                   "MLB031M10","MLB031M11","MLB036A","MLB036B","MLB036C","MLB036D",
                   "MLB036E","MLB036F","MLB036G","MLB037A","MLB037B","MLB037C","MLB037D",
                   "MLB037E","MLB037F","MLB037G","MLB038A","MLB038B","MLB038C","MLB038D",
                   "MLB038E","MLB038F","MLB039A","MLB039B","MLB039C","MLB039D","MLB039E",
                   "MLB039F","MLB040","MLB040A_A","MLB040A_B","MLB040A_C","MLB040A_D",
                   "MLB040A_E","MLB040A_F","MLB040A_G","MLB040A_H","MLB045","MLB046A",
                   "MLB046B","MLB047A","MLB047B","MLB047C","MLB047D")
#subset columns
p10 <- p10[, vars_keep_p10, with=FALSE]

#select variables to keep: p12
vars_keep_p12 <- c("HHID", "PN", "MSUBHH","NCSR","NFAMR","NFINR","NLB004","NLB007","NLB011",
                 "NLB015","NLB006","NLB010","NLB014","NLB018","NLB009A","NLB009B",
                 "NLB009C","NLB013A","NLB013B","NLB013C","NLB017A","NLB017B","NLB017C",
                 "NLB005A","NLB005B","NLB005C","NLB005D","NLB005E","NLB005F","NLB005G",
                 "NLB008A","NLB008B","NLB008C","NLB008D","NLB008E","NLB008F","NLB008G",
                 "NLB012A","NLB012B","NLB012C","NLB012D","NLB012E","NLB012F","NLB012G",
                 "NLB016A","NLB016B","NLB016C","NLB016D","NLB016E","NLB016F","NLB016G",
                 "NLB020A","NLB020B","NLB020C","NLB020E","NLB020D","NLB020F","NLB020G",
                 "NLB020H","NLB020I","NLB020J","NLB020K","NLB023A","NLB023B","NLB023C",
                 "NLB023D","NLB023E","NLB029B1","NLB029B3","NLB029B7","NLB029B8",
                 "NLB029B2","NLB029B4","NLB029B5","NLB029B6","NLB030A","NLB030B",
                 "NLB030C","NLB030D","NLB030E","NLB030F","NLB031M1","NLB031M2","NLB031M3",
                 "NLB031M4","NLB031M5","NLB031M6","NLB031M7","NLB031M8","NLB031M9",
                 "NLB031M10","NLB031M11","NLB036A","NLB036B","NLB036C","NLB036D",
                 "NLB036E","NLB036F","NLB036G","NLB037A","NLB037B","NLB037C","NLB037D",
                 "NLB037E","NLB037F","NLB037G","NLB038A","NLB038B","NLB038C","NLB038D",
                 "NLB038E","NLB038F","NLB039A","NLB039B","NLB039C","NLB039D","NLB039E",
                 "NLB039F","NLB040","NLB040A_A","NLB040A_B","NLB040A_C","NLB040A_D",
                 "NLB040A_E","NLB040A_F","NLB040A_G","NLB040A_H","NLB079","NLB080A",
                 "NLB080B","NLB081A","NLB081B","NLB081C","NLB081D")
#subset columns
p12 <- p12[, vars_keep_p12, with=FALSE]

#remove underscores from all names
setnames(p10, gsub("_", "", names(p10)))
setnames(p12, gsub("_", "", names(p12)))

#add wave identifer to each variable
setnames(p10, paste0(names(p10), sep = "_", 10))
setnames(p12, paste0(names(p12), sep = "_", 11))

#remove wave identifier from respondent id vars
setnames(p10, c("HHID_10", "PN_10"), c("hhid", "pn"))
setnames(p12, c("HHID_11", "PN_11"), c("hhid", "pn"))

#make p10 and p12 names match
setnames(p10, c("MCSR_10", "MFAMR_10", "MFINR_10"), c("NCSR_10", "NFAMR_10", "NINR_10"))
setnames(p10, gsub("MLB", "NLB", names(p10)))
#perceived work ability
setnames(p10, c("NLB047A_10", "NLB047B_10", "NLB047C_10", "NLB047D_10"), 
         c("workaba_10", "workabb_10", "workabc_10", "workabd_10"))
setnames(p12, c("NLB081A_11", "NLB081B_11", "NLB081C_11", "NLB081D_11"), 
         c("workaba_11", "workabb_11", "workabc_11", "workabd_11"))
#job lock
setnames(p10, c("NLB046A_10", "NLB046B_10"), c("jlocka_10", "jlockb_10"))
setnames(p12, c("NLB080A_11", "NLB080B_11"), c("jlocka_11", "jlockb_11"))
#currently working
setnames(p10, c("NLB045_10"), c("curwork_10"))
setnames(p12, c("NLB079_11"), c("curwork_11"))

#add psychosocial identifier variable for easy sub-setting later
p10[, pslqb_10 := 1]
p12[, pslqb_11 := 1]

#select variables to keep, subset and rename: e12
vars_keep_e12 <- c("HHID", "NE060")
e12 <- e12[, vars_keep_e12, with=FALSE]
setnames(e12, c("HHID", "NE060"), c("hhid", "NE060_11"))

#select variables to keep, subset and rename: e10
vars_keep_e10 <- c("HHID", "ME060")
e10 <- e10[, vars_keep_e10, with=FALSE]
setnames(e10, c("HHID", "ME060"), c("hhid", "NE060_10"))

#select variables to keep, subset and rename: f12
vars_keep_f12 <- c("HHID", "PN", "NF119")
f12 <- f12[, vars_keep_f12, with=FALSE]
setnames(f12, c("HHID", "PN", "NF119"), c("hhid", "pn", "NF119_11"))

#select variables to keep, subset and rename: f10
vars_keep_f10 <- c("HHID", "PN", "MF119")
f10 <- f10[, vars_keep_f10, with=FALSE]
setnames(f10, c("HHID", "PN", "MF119"), c("hhid", "pn", "NF119_10"))

##merge into hrs (wide) data
#load data
hrs <- fread(paste0(data_dir, "randhrs_formatted.csv"))
hrs <- hrs[, V1 := NULL]

#select variables to keep
vars_static <- c("hhidpn", "hhid", "pn", "ragender", "raracem", "raeduc", "raedyrs", "rawtsamp", "raestrat", "rahhidpn", "racohbyr")
vars_vary <- c("rhigov", "rlifein", "rhenum", "siadlza", 
               "rsmokev", "rsmoken", "rdrink", "rdrinkd", "rdrinkn", "rbmi", "rageyb", "rmstat",
               "rshlt", "rdepres", "reffort", "rsleepr", "rwhappy", "renlife", "rfsad", "rgoing",
               "rconds", "rconde", "rsayret", "rretsat", "hitot", "hatota",
               "rlgmusa", "rmobila", "rfinea", "rcovr", "rcovs", "riwstat")

#Add suffixes
numvec <- rep(3:11, times=length(vars_vary))
vars_analysis_vary <- paste0(vars_vary, "_", numvec)
vars_keep <- c(vars_static, vars_analysis_vary)

#subset columns
hrs <- hrs[, vars_keep, with=FALSE]

#merge
hrs_merged <- Reduce(function(x, y) {merge(x, y, by=c("hhid", "pn"), all = TRUE)}, list(hrs, p10, p12, f10, f12))
hrs_merged <- Reduce(function(x, y) {merge(x, y, by=c("hhid"), all.x = TRUE)}, list(hrs_merged, e10, e12))

#write to .csv
#write.csv(hrs_merged, paste0(data_dir, "randhrs_pslbq_merged.csv"))

#select variables to keep
vars_static <- c("hhidpn", "hhid", "pn", "ragender", "raracem", "raeduc", "raedyrs", "rawtsamp", 
                 "raestrat", "rahhidpn", "racohbyr")
vars_vary <- c("rhigov", "rlifein", "rhenum", "siadlza", 
               "rsmokev", "rsmoken", "rdrink", "rdrinkd", "rdrinkn", "rbmi", "rageyb", "rmstat",
               "rshlt", "rdepres", "reffort", "rsleepr", "rwhappy", "renlife", "rfsad", "rgoing",
               "rconds", "rconde", "rsayret", "rretsat", "hitot", "hatota",
               "rlgmusa", "rmobila", "rfinea", "rcovr", "rcovs", "riwstat")
vars_vary_p10 <- names(p10)[grep("_", names(p10))]
vars_vary_e10 <- names(e10)[grep("_", names(e10))]
vars_vary_f10 <- names(f10)[grep("_", names(f10))]
vars_vary_p12 <- names(p12)[grep("_", names(p12))]
vars_vary_e12 <- names(e12)[grep("_", names(e12))]
vars_vary_f12 <- names(f12)[grep("_", names(f12))]

#Add suffixes
numvec <- rep(3:11, times=length(vars_vary))
vars_analysis_vary <- paste0(vars_vary, "_", numvec)
vars_keep <- c(vars_static, vars_analysis_vary, vars_vary_p10, vars_vary_e10, vars_vary_f10,
               vars_vary_p12, vars_vary_e12, vars_vary_f12)

#subset columns
hrs_merged <- hrs_merged[, vars_keep, with=FALSE]

#reshape from wide to long
#determine id vars (no wave assignment) from wave-specific vars
idvars <- names(hrs_merged)
#varyingCols <- grep("_", names(hrs_merged))

#melt long (idvars = first 11 variables)
hrs_long <- melt(hrs_merged, id=idvars[1:11])
#split variable into Variable + Wave, and rename
hrs_long <- cSplit(hrs_long, splitCols = c("variable"), sep = "_")
setnames(hrs_long, c("variable_1", "variable_2"), c("variable", "wave"))
#cast
hrs_long <- dcast(hrs_long, hhidpn+hhid+pn+ragender+raracem+raedyrs+raeduc+rawtsamp+raestrat+rahhidpn+racohbyr+wave ~ variable, 
                  value.var = "value", fun.aggregate=mean)

#write to .csv
write.csv(hrs_long, paste0(data_dir, "randhrs_pslbq_long.csv"))
