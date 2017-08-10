####################################################################################################
## Author: Julia Morris
## Description: Basic formatting for raw RAND HRS data.
## Output: .csv with RAND HRS data, which needs reshaping and recoding.
## NOTE: Run FIRST. 
####################################################################################################

#load packages
library(data.table)

#set directories
code_dir <- "C:/Users/julia/Dropbox/HRS/code_dir/"
raw_data_dir <- "C:/Users/julia/Dropbox/HRS/raw_data_dir/"
data_dir <- "C:/Users/julia/Dropbox/HRS/data_dir/"
output_dir <- "C:/Users/julia/Dropbox/HRS/output_dir/"

#load data
hrs <- read.csv(paste0(raw_data_dir, "randhrs.csv"), stringsAsFactors = FALSE)
hrs <- as.data.table(hrs)

#remove non-numeric characters
hrs_wide <- hrs[, lapply(.SD, as.character)]
hrs_wide <- hrs_wide[, lapply(.SD, as.numeric)]

#drop hhidpn numbers with too few digits (data errors)
hrs_wide <- hrs_wide[hhidpn>=10000000]

#keep only selected cohorts
#1=AHEAD;2=CODA;3=HRS;0=ELSE
hrs_wide <- hrs_wide[racohbyr>=1 & racohbyr<=3]

#drop if spouses (not respondents) 
hrs_wide <- hrs_wide[!which(hrs_wide[, s1hhidpn > 0])]

#remove underscores from all names
setnames(hrs_wide, gsub("_", "", names(hrs_wide)))

#keep only waves 3-11 variables (since won't be using waves 1 or 2)
#don't drop 1 and 2, because it will drop 11
hrs_wide <- subset(hrs_wide, select = grep("ra|hh|pn|r11|r10|r9|r8|r7|r6|r5|r4|r3|h11|h10|h9|h8|h7|h6|h5|h4|h3|s11|s10|s9|s8|s7|s6|s5|s4|s3", names(hrs_wide)))

#Add suffixes
numvec <- as.factor(c(3:11))
hrs_wide <- as.data.frame(hrs_wide)
for (num in numvec){
  names(hrs_wide)[names(hrs_wide) %in% names(hrs_wide[grep(num, names(hrs_wide))])] <- 
    paste(names(hrs_wide)[names(hrs_wide) %in% names(hrs_wide[grep(num, names(hrs_wide))])], num, sep = "_")
}
hrs_wide <- as.data.table(hrs_wide)

#drop prefixes
setnames(hrs_wide, gsub(("r11|r10|r9|r8|r7|r6|r5|r4|r3"), "r", names(hrs_wide)))
setnames(hrs_wide, gsub(("h11|h10|h9|h8|h7|h6|h5|h4|h3"), "h", names(hrs_wide)))
setnames(hrs_wide, gsub(("s11|s10|s9|s8|s7|s6|s5|s4|s3"), "s", names(hrs_wide)))

#save formatted .csv
write.csv(hrs_wide, paste0(data_dir, "randhrs_formatted.csv"))