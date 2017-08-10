####################################################################################################
## Author: Julia Morris
## Description: HRS variable recodes. 
## Output:  Recoded, long, merged HRS file.
## NOTE: Code annotations describe HRS survey coding structure.
####################################################################################################


#load packages
library(data.table)

#set directories
code_dir <- "C:/Users/julia/Dropbox/HRS/code_dir/"
data_dir <- "C:/Users/julia/Dropbox/HRS/data_dir/"
output_dir <- "C:/Users/julia/Dropbox/HRS/output_dir/"

#load data
hrs <- fread(paste0(data_dir, "randhrs_pslbq_long.csv"))
hrs <- hrs[, V1 := NULL]

#remove non-numeric characters
hrs <- hrs[, lapply(.SD, as.character)]
hrs <- hrs[, lapply(.SD, as.numeric)]

#change NA's to 0 for pslqb identifier variable
hrs[is.na(hrs[,pslqb]), pslqb := 0]

###Fixed demographic characteristics
#RAGENDER #GENDER 
#0 male; 1 female
setkey(hrs, ragender)
hrs[.(1), female := 0]; hrs[.(2), female := 1]
#subset to drop missing cases
hrs <- hrs[female==0|female==1]

#RARACEM #Race
#generate race dummy variables
setkey(hrs, raracem)
hrs[.(c(2, 3)), racewhite := 0]; hrs[.(1), racewhite := 1]
hrs[.(c(1, 3)), raceblack := 0]; hrs[.(2), raceblack := 1]
hrs[.(c(1, 2)), raceother := 0]; hrs[.(3), raceother := 1]

#subset by race (only white/black)
hrs <- hrs[raracem==1|raracem==2]

#RAEDUC:R EDUCATION
#generate some college variable from RAEDUC:R EDUCATION
##some college + (1), did not attend college (0)
setkey(hrs, raeduc)
hrs[.(c(1:3)), college := 0]; hrs[.(c(4, 5)), college := 1]

#RAGEYB: Respondent's Age 
#hrs[, .N, by="rageyb"]
###AFTER IMPUTATION###hrs[, age_100 := lapply(.SD, function(x) x/100), .SDcols="rageyb"]

#married/partnered (1); separated/divorced/widowed/never married (0)
##from R11MSTAT
setkey(hrs, rmstat)
hrs[.(c(1:3)), rmarpar := 0]; hrs[.(c(4:8)), rmarpar := 1]

#generate binary SRH variable from R11SHLT: FAIR OR POOR SELF-RATED HEALTH
##(1) fair or poor health, (0) good to excellent health
setkey(hrs, rshlt)
hrs[.(c(1:3)), rfairpoorsrh := 0]; hrs[.(c(4, 5)), rfairpoorsrh := 1]

#create cohort dummy variables
#1=AHEAD;2=CODA;3=HRS;0=ELSE
setkey(hrs, racohbyr)
hrs[.(1), coh_ahead := 1]; hrs[.(c(2,3)), coh_ahead := 0]
hrs[.(2), coh_coda := 1]; hrs[.(c(1,3)), coh_coda := 0]
hrs[.(3), coh_hrs := 1]; hrs[.(c(1,2)), coh_hrs := 0]

#R11CESD:W11 CESD score
#measures within cesd score variable
#R11DEPRES:W11 CESD: Felt depressed
#R11EFFORT:W11 CESD: Everything an effort
#R11SLEEPR:W11 CESD: Sleep was restless
#R11WHAPPY:W11 CESD: Was happy
##Reverse code
hrs[, rwhappyrev := abs(rwhappy-1)]
#R11FLONE:W11 CESD: Felt lonely
#R11ENLIFE:W11 CESD: Enjoyed life
##Reverse code
hrs[, renliferev := abs(renlife-1)]
#R11FSAD:W11 CESD: Felt sad
#R11GOING:W11 CESD: Could not get going

#generate cesd score WITHOUT loneliness
hrs[, rcesdml := rowSums(.SD, na.rm = TRUE), .SDcols = c("rdepres", "reffort", "rsleepr", "rwhappyrev", 
                                                         "renliferev", "rfsad", "rgoing")]
#riwstat
#1 responded and alive
#4 no response; alive
#5 no response; died this wave
#6 no response; died previous wave
#7 no response; dropped from sample
setkey(hrs, riwstat)
hrs[.(1), responded := 1]; hrs[.(0), responded := 0]; hrs[.(c(4:9)), responded := 0]
hrs[.(c(5,6)), died := 1]; hrs[.(c(0,1,4,7)), died := 0]
hrs[.(c(5)), died_thiswave := 1]; hrs[.(c(0,1,4,6,7)), died_thiswave := 0]
#create sum of responses
propDT <- hrs[, list(resp_sum = sum(responded, na.rm=TRUE),
                     died_ever = sum(died, na.rm=TRUE)), by=c("hhid","pn")]
#recode died_ever
propDT[died_ever>=1, died_ever := 1]
#merge back into data.table
hrs <- Reduce(function(x, y) {merge(x, y, by=c("hhid", "pn"), all.x = TRUE)}, list(hrs, propDT))
#create proportion variable
hrs[, resp_prp := lapply(.SD, function(x) x/(length(unique(hrs[,wave])))), .SDcols="resp_sum"]

#R11DRINKD:W11 R # days/week drinks
hrs <- hrs[rdrinkd == 99, rdrinkd := NA]
#0   0.0 or doesnt drink
#99   99.all day
#hrs[, .N, by="rdrinkd"]

#R11DRINKN:W11 R # drinks/day when drinks
hrs <- hrs[rdrinkn == 99, rdrinkn := NA]
#0   0.0 or doesnt drink
#99   99.all day
#hrs[, .N, by="rdrinkn"]

##create weekly drinking measure
hrs[, drinkswk := rdrink * rdrinkd * rdrinkn]
####AFTER IMPUTATION###hrs[, drinkswklog := lapply(.SD, function(x) log(x)), .SDcols="drinkswk"]

#Not retired (0); Retired (completely or partly) (1)
setkey(hrs, rsayret)
hrs[.(0), rretired := 0]; hrs[.(c(1, 2)), rretired := 1]

#H11ITOT:W11 Income: Total HHold / R+Sp only
####AFTER IMPUTATION###hrs[, hinclog := lapply(.SD, function(x) log(x)), .SDcols="hitot"]

#R11COVR:W11 R covered by R empl plan
#0   0.no
#1   1.yes
#R11COVS:W11 R covered by S empl plan
#0   0.no
#1   1.yes
hrs[rcovr==0, priv_ins := 0]
hrs[rcovs==0, priv_ins := 0]
hrs[rcovr==1 | rcovs==1, priv_ins := 1]

###Functional Limitations
##Mobility (RwMOBILA): The five tasks included in the mobility index are walking several blocks, walking one block,
#walking across the room, climbing several flights of stairs and climbing one flight of stairs. Note that this index is missing
#for AHEAD entry cohort Respondents and their spouses in Wave 2, because one or more of the elements in the index is not
#available in Wave 2A.
##Large Muscle (RwLGMUSA): The four tasks included in the large muscle index are sitting for two hours, getting up
#from a chair, stooping or kneeling or crouching, and pushing or pulling a large object. Note that this index is missing for
#AHEAD entry cohort Respondents and their spouses in Wave 2, because one or more of the elements in the index is not
#available in Wave 2A.
##Fine Motor Skills (RwFINEA): The three tasks included in this index are: picking up a dime, eating, and dressing.
hrs[, func_lim := rowSums(.SD, na.rm = TRUE), .SDcols = c("rmobila", "rlgmusa", "rfinea")]

#Spouse Some Diff-IADLs
#Instrumental Activities of Daily Living (RwIADLA, RwLMCOGA, RwIADLZA): Not all waves
###ask the same Instrumental Activities of Daily Living (IADL) tasks. 
###SwIADLZA includes using a telephone, taking medication, handling money, shopping, preparing meals.
#recode to be binary for any/none
setkey(hrs, siadlza)
hrs[.(0), care_sp := 0]; hrs[.(1:5), care_sp := 1]

#generate categorical var from H11ATOTA: does not have negative wealth (debt) (0), has debt (1)
#***Note: this variable is not particularly informative, but could think about
#***subsetting data based upon this criteria, and perhaps logging wealth variable for those
#***with positive wealth
###AFTER IMPUTATION###hrs[hatota>0, hwealthlog := hatota]
###AFTER IMPUTATION###hrs[hatota<=0, hwealthlog := 0]
###AFTER IMPUTATION###hrs[, hwealthlog := lapply(.SD, function(x) log(x)), .SDcols="hwealthlog"]

##Perceived Ability to Work
#How many points would you give your CURRENT ABILITY TO WORK? (0 means that you cannot 
#currently work at all; 10 means your work ability is currently at its lifetime best))
#CODING: 11 point, continuous measure (0-10). Scaling: Sum the items to create an index of work ability.
hrs[, work_able := rowSums(.SD, na.rm = TRUE), .SDcols = c("workaba", "workabb", "workabc", "workabd")]

###PSLBQ

##Contact with social network
#Nine questions assess the extent to which respondents are in contact with their social networks (excluding spouses). 
#Similar questions refer to contact with children (Q 9a-c), other family (Q 13a-c), and friends (Q17a-c).
#(On average, how often do you do each of the following? Please check the answer which shows how you feel about each statement.)
#a=Meet up (include both arranged and chance meetings). b=Speak on the phone. c=Write or email.
#1=Three or more times a week, 2=Once or twice a week, 3=Once or twice a month, 4=Every few months, 5=Once or twice a year, 
#6=Less than once a year or never
#Scaling: Reverse code all items. Depending on your research question, average or sum across items for each specific relation 
#category or across all relation categories for a measure of overall contact with the social network. Set the final score to 
#missing if there is more than one item with missing values.
#reverse coding: NLB009A, NLB009B, NLB009C, NLB013A, NLB013B, NLB013C, NLB017A, NLB017B, NLB017C
cols_rev <- c("NLB009A","NLB009B","NLB009C","NLB013A","NLB013B","NLB013C","NLB017A","NLB017B","NLB017C")
hrs[, (cols_rev) := lapply(.SD, function(x) abs(x-7)), .SDcols=cols_rev]

##Perceived social support (relationship quality)
#Four sets of 7 items (Q5, Q8, Q12, Q16) examine the perceived support that respondents receive from their spouses (Q5), 
#children (Q8), family (Q12), and friends (Q16). For each relationship category there are 3 positively worded items (items a-c) 
#and 4 negatively worded items (items d- g). Some researchers use these items as indicators of perceived relationship quality 
#rather than support.
#Positive Social Support (items a-c). Negative Social Support (items d-g). Coding: 1=A lot, 2=Some, 3=A little, 4=Not at all.
#Scaling: Reverse code all items. Create an index of positive social support and an index of negative social support for each 
#relationship category by averaging the scores within each dimension [positive (a-c) and negative (d-g)]. Set the final score to 
#missing if there is more than one item with missing values for the positive social support scale, or more than two items with 
#missing values for the negative social support scale.
#reverse code: NLB005A, NLB005B, NLB005C, NLB005D, NLB005E, NLB005F, NLB005G
#reverse code: NLB008A, NLB008B, NLB008C, NLB008D, NLB008E, NLB008F, NLB008G
#reverse code: NLB012A, NLB012B, NLB012C, NLB012D, NLB012E, NLB012F, NLB012G
#reverse code: NLB016A, NLB016B, NLB016C, NLB016D, NLB016E, NLB016F, NLB016G
cols_rev <- c("NLB005A","NLB005B","NLB005C","NLB005D","NLB005E","NLB005F","NLB005G",
              "NLB008A","NLB008B","NLB008C","NLB008D","NLB008E","NLB008F","NLB008G",
              "NLB012A","NLB012B","NLB012C","NLB012D","NLB012E","NLB012F","NLB012G",
              "NLB016A","NLB016B","NLB016C","NLB016D","NLB016E","NLB016F","NLB016G")
hrs[, (cols_rev) := lapply(.SD, function(x) abs(x-5)), .SDcols=cols_rev]

#calculate averages
#spouse
hrs[, sup_pos_spouse := rowMeans(.SD, na.rm=T), .SDcols=c("NLB005A","NLB005B","NLB005C")]
hrs[, sup_neg_spouse := rowMeans(.SD, na.rm=T), .SDcols=c("NLB005D","NLB005E","NLB005F","NLB005G")]
#children
hrs[, sup_pos_child := rowMeans(.SD, na.rm=T), .SDcols=c("NLB008A","NLB008B","NLB008C")]
hrs[, sup_neg_child := rowMeans(.SD, na.rm=T), .SDcols=c("NLB008D","NLB008E","NLB008F","NLB008G")]
#family
hrs[, sup_pos_fam := rowMeans(.SD, na.rm=T), .SDcols=c("NLB012A","NLB012B","NLB012C")]
hrs[, sup_neg_fam := rowMeans(.SD, na.rm=T), .SDcols=c("NLB012D","NLB012E","NLB012F","NLB012G")]
#friends
hrs[, sup_pos_fri := rowMeans(.SD, na.rm=T), .SDcols=c("NLB016A","NLB016B","NLB016C")]
hrs[, sup_neg_fri := rowMeans(.SD, na.rm=T), .SDcols=c("NLB016D","NLB016E","NLB016F","NLB016G")]
#total
hrs[, sup_pos_all := rowMeans(.SD, na.rm=T), .SDcols=c("sup_pos_spouse","sup_pos_child",
                                                       "sup_pos_fam", "sup_pos_fri")]
hrs[, sup_neg_all := rowMeans(.SD, na.rm=T), .SDcols=c("sup_neg_spouse","sup_neg_child",
                                                       "sup_neg_fam", "sup_neg_fri")]

##loneliness (2006, 2008, & 2010)
#Create an index of loneliness by reverse-coding items 20a, 20b, 20c, and
#20e and averaging the scores across all 11 items. Set the final score to
#missing if there is more than five items with missing values.
#coding: 1=Often, 2=Some of the time, 3=Hardly ever or never
#reverse code NLB020A, NLB020B, NLB020C, NLB020E
cols_rev <- c("NLB020A","NLB020B","NLB020C","NLB020E")
hrs[, (cols_rev) := lapply(.SD, function(x) abs(x-4)), .SDcols=cols_rev]
#normal coding NLB020D, NLB020F, NLB020G, NLB020H, NLB020I, NLB020J, NLB020K

##Self-perceptions of Aging (Satisfaction with Aging) (2008 & 2010) 
#[MAY BE IMPORTANT FOR STORY ABOUT LATE-LIFE AS DIFF THAN EARLY LIFE]
#Create a rating of aging satisfaction by reverse coding items Q29 b1, b3, b7, and b8 
#and averaging the scores across all 8 items. Set the final score to missing if there are 
#more than four items with missing values.
#coding: 1=Strongly disagree, 2=Somewhat disagree, 3=Slightly disagree, 4=Slightly agree, 5=Somewhat agree, 6=Strongly agree
#reverse code NLB029B1, NLB029B3, NLB029B7, NLB029B8
cols_rev <- c("NLB029B1","NLB029B3","NLB029B7","NLB029B8")
hrs[, (cols_rev) := lapply(.SD, function(x) abs(x-7)), .SDcols=cols_rev]
#normal coding NLB029B2, NLB029B4, NLB029B5, NLB029B6

##everyday discrimination (2006, 2008, & 2010)
#Create an index of discrimination by reverse-coding all items and averaging the scores across all six items. 
#Set the final score to missing if there are more than three items with missing values.
#1=Almost every day, 2=At least once a week, 3=A few times a month, 4=A few times a year, 5=Less than once a year, 6=Never
#reverse code: NLB030A, NLB030B, NLB030C, NLB030D, NLB030E, NLB030F
cols_rev <- c("NLB030A","NLB030B","NLB030C","NLB030D","NLB030E","NLB030F")
hrs[, (cols_rev) := lapply(.SD, function(x) abs(x-7)), .SDcols=cols_rev]

##Recent Stressful Life Events (2006, 2008, & 2010)
#coding: 1=Yes, 5=No. Scaling: An index can be created by summing the number of positive responses to the items.
setkey(hrs, NLB038A)
hrs[.(1), rec_slea := 1]; hrs[.(5), rec_slea := 0]
setkey(hrs, NLB038B)
hrs[.(1), rec_sleb := 1]; hrs[.(5), rec_sleb := 0]
setkey(hrs, NLB038C)
hrs[.(1), rec_slec := 1]; hrs[.(5), rec_slec := 0]
setkey(hrs, NLB038D)
hrs[.(1), rec_sled := 1]; hrs[.(5), rec_sled := 0]
setkey(hrs, NLB038E)
hrs[.(1), rec_slee := 1]; hrs[.(5), rec_slee := 0]
setkey(hrs, NLB038F)
hrs[.(1), rec_slef := 1]; hrs[.(5), rec_slef := 0]
#NLB038A, NLB038B, NLB038C, NLB038D, NLB038E, NLB038F
hrs[, rec_sle := rowSums(.SD, na.rm = TRUE), .SDcols = c("rec_slea", "rec_sleb", "rec_slec", 
                                                         "rec_sled", "rec_slee", "rec_slef")]

##Satisfaction with Domains of Life (2008 & 2010)
#Coding: 1=Completely satisfied, 2=Very satisfied, 3=Somewhat satisfied, 4=Not very satisfied, 5=Not at all satisfied
#Scaling: Reverse score each item so that a higher score corresponds to more satisfaction in each domain (housing Q39a, city Q39b, 
#nonwork Q39c, family life Q39d, financial situation Q39e, health Q39f, overall life satisfaction Q39g).
#reverse coding: NLB039A, NLB039B, NLB039C, NLB039D, NLB039E, NLB039F
cols_rev <- c("NLB039A","NLB039B","NLB039C","NLB039D","NLB039E","NLB039F")
hrs[, (cols_rev) := lapply(.SD, function(x) abs(x-6)), .SDcols=cols_rev]

##Ongoing Chronic Stressors (Q. 40 in 2006; not included in 2008) These 8 items were also in the 2004 Pilot SAQ.
#Q40a Ongoing health problems (in yourself)
#Q40b Ongoing physical or emotional problems (in spouse or child)
#Q40c Ongoing problems with alcohol or drug use in family member
#Q40d Ongoing difficulties at work
#Q40e Ongoing financial strain
#Q40f Ongoing housing problems
#Q40g Ongoing problems in a close relationship
#Q40h Helping at least one sick, limited, or frail family member or friend on a regular basis
#Coding: 1=No, didn't happen, 2=Yes, but not upsetting, 3=Yes, somewhat upsetting, 4=Yes, very upsetting
#Scaling: The measure can be scored by calculating a simple unweighted sum of all ongoing problems.
#NLB040A_A, NLB040A_B, NLB040A_C, NLB040A_D, NLB040A_E, NLB040A_F, NLB040A_G, NLB040A_H
hrs[, chr_sle := rowSums(.SD, na.rm = TRUE), .SDcols = c("NLB040AA", "NLB040AB", "NLB040AC", 
                                                         "NLB040AD", "NLB040AE", "NLB040AF",
                                                         "NLB040AG", "NLB040AH")]

##Currently Working (2006, 2008, 2010)
#[APPEARS TO BE DIFFERENT QUESTION IN 2012 - LOOK INTO THIS; NLB045 IS NOT THE SAME IN 2012]
#coding: 1=Yes, 5=no
#MLB045
#2012: NLB079
hrs[curwork == 5, curwork := 0]

##Job Lock (2008 & 2010)
#coding: 1=Yes, 5=No
#job lock a = money; job lock b = insurance
hrs[jlocka == 5, jlocka := 0]
hrs[jlockb == 5, jlockb := 0]
hrs[jlocka == 0, job_lock := 0]
hrs[jlockb == 0, job_lock := 0]
hrs[jlocka == 1 | jlockb ==1, job_lock := 1]

##mastery (2006, 2008, & 2010)
#Create an index of Mastery by averaging the scores across items Q23a-Q23e. Set the final
#score to missing if there are more than three items with missing values.
#coding: 1=Strongly disagree, 2=Somewhat disagree, 3=Slightly disagree, 4=Slightly agree, 5=Somewhat agree, 6=Strongly agree
#NLB023A, NLB023B, NLB023C, NLB023D, NLB023E
hrs[, mastery := rowMeans(.SD,na.rm=T), .SDcols=c("NLB023A","NLB023B","NLB023C", "NLB023D", "NLB023E")]

####SECTIONS E AND F
#parent care
setkey(hrs, NF119)
hrs[.(1), care_par := 1]; hrs[.(5), care_par := 0]
#grandchild care
setkey(hrs, NE060)
hrs[.(1), care_gc := 1]; hrs[.(5), care_gc := 0]

#write to .csv
#write.csv(hrs, paste0(data_dir, "randhrs_pslbq_recoded.csv"))
write.csv(hrs, paste0(data_dir, "randhrs_pslbq_recoded_apr2017.csv")) #updated to include "died_thiswave"