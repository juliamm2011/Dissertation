####################################################################################################
## Author: Julia Morris
## Description: Creates Table 1, histograms, and correlation plots for Chapter 1. 
## Output:  Table 1, histograms, correlation plots.
####################################################################################################

#load packages
library(data.table)
library(tableone)
library(PerformanceAnalytics)
library(Hmisc)

#set directories
code_dir <- "C:/Users/julia/Dropbox/HRS/code_dir/"
data_dir <- "C:/Users/julia/Dropbox/HRS/data_dir/"
output_dir <- "C:/Users/julia/Dropbox/HRS/output_dir/"

#load data
hrs <- fread(paste0(data_dir, "randhrs_pslbq_imputed_ch1.csv"))
hrs <- hrs[, V1 := NULL]

#subset to wave 11 for table
hrs <- hrs[wave==11]

#create binary ces-d outcome using 3 as the cut-off.
hrs[rcesdml>=3, cdep3 := 1]; hrs[rcesdml<3, cdep3 := 0]

#create binary ces-d outcome using 4 as the cut-off.
hrs[rcesdml>=4, cdep4 := 1]; hrs[rcesdml<4, cdep4 := 0]

#create new variable for facet_plot labels
hrs[raceblack==0&female==0, groupvar := "1 White Male"]
hrs[raceblack==0&female==1, groupvar := "2 White Female"]
hrs[raceblack==1&female==0, groupvar := "3 Black Male"]
hrs[raceblack==1&female==1, groupvar := "4 Black Female"]

#histogram of ces-d depressive symptoms
hist(hrs[, rcesdml], breaks=8, main="CES-D Depressive Symptoms, 2012", xlab="Number of Reported Symptoms")
#histogram of ces-d outcome: facet plot
p <- qplot(rcesdml, data = hrs, geom = "histogram", binwidth=1, 
           main="CES-D Depressive Symptoms, 2012", 
           xlab="Number of Reported Symptoms",
           ylab="Frequency")
p + facet_wrap(~ groupvar, scales = 'free_y') + theme_bw()

#histogram with percentages
p <- ggplot(hrs, aes(x=rcesdml)) + geom_histogram(aes(y=..count../sum(..count..)), binwidth=1) + 
                 ggtitle("CES-D Depressive Symptoms, 2012") +
                   xlab("Number of Reported Symptoms") + ylab("Percent")
p + facet_wrap(~ groupvar, scales = 'free_y') + theme_bw()

#histograms of CES-D depressive symptoms, stratified by Sex and Race
ggplot(hrs[groupvar=="1 White Male"], aes(x=rcesdml)) +
  geom_histogram(aes(y=..count../sum(..count..)), binwidth=1) + 
  ylim(0,0.58) + theme_bw() +
  ggtitle("White Males") +
  xlab("Number of Reported CES-D Depressive Symptoms, 2012") + ylab("Percent")
ggplot(hrs[groupvar=="2 White Female"], aes(x=rcesdml)) +
  geom_histogram(aes(y=..count../sum(..count..)), binwidth=1) + 
  ylim(0,0.58) + theme_bw() +
  ggtitle("White Females") +
  xlab("Number of Reported CES-D Depressive Symptoms, 2012") + ylab("Percent")
ggplot(hrs[groupvar=="3 Black Male"], aes(x=rcesdml)) +
  geom_histogram(aes(y=..count../sum(..count..)), binwidth=1) + 
  ylim(0,0.58) + theme_bw() +
  ggtitle("Black Males") +
  xlab("Number of Reported CES-D Depressive Symptoms, 2012") + ylab("Percent")
ggplot(hrs[groupvar=="4 Black Female"], aes(x=rcesdml)) +
  geom_histogram(aes(y=..count../sum(..count..)), binwidth=1) + 
  ylim(0,0.58) + theme_bw() +
  ggtitle("Black Females") +
  xlab("Number of Reported CES-D Depressive Symptoms, 2012") + ylab("Percent")

#create Table 1; library(tableone)
#Create a variable list which we want in Table 1
listVars <- c("rec_sle","chr_sle","drinkswk","func_lim",
              "hatota","hitot","raedyrs","rageyb","rbmi",
              "rcesdml","rconds","sup_neg_all","sup_pos_all",
              "racohbyr","rfairpoorsrh","care_gc","care_par",
              "care_sp","care_any","priv_ins","rmarpar","rretired",
              "care_trans","ret_trans","female","raceblack","cdep4")
#Define categorical variables
catVars <- c("racohbyr","rfairpoorsrh","care_gc","care_par","care_sp","care_any","priv_ins","rmarpar","rretired",
             "care_trans","ret_trans","female","raceblack","cdep4")
#Total Population
table1a <- CreateTableOne(vars = listVars, data = hrs, factorVars = catVars)
#export Table 1 to Microsoft Word; library(ReporteRs); library(magrittr)
table1a <- print(table1a)

#Stratified by Sex and Race
#Remove Sex and Race from list of vars
listVars <- c("rec_sle","chr_sle","drinkswk","func_lim",
              "hatota","hitot","raedyrs","rageyb","rbmi",
              "rcesdml","rconds","sup_neg_all","sup_pos_all",
              "racohbyr","rfairpoorsrh","care_gc","care_par",
              "care_sp","care_any","priv_ins","rmarpar","rretired",
              "care_trans","ret_trans","cdep4")
catVars <- c("racohbyr","rfairpoorsrh","care_gc","care_par","care_sp","care_any","priv_ins","rmarpar","rretired",
             "care_trans","ret_trans","cdep4")
stratVars <- c("female","raceblack")
table1b <- CreateTableOne(listVars, data = hrs, factorVars = catVars, strata = stratVars)
table1b <- print(table1b)

#write to .csv
write.csv(table1b, paste0(output_dir, "ch1_table1.csv"))

###correlation matrices

##correlation matrix with p-values function
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}
## create function to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}
##flatten the table
flattenSquareMatrix(cor.prob(hrs_sub))
##plot the data
chart.Correlation(hrs_sub)

#correlation matrix
rcorr(as.matrix(hrs_sub),type="pearson")

#split correlation matrices by sub-group

#subset data by sex and race (white male, white female, black male, black female)
hrs_wm_sub <- hrs[raceblack==0&female==0, c("age_100","hwealthlog","hinclog","raedyrs",
                                            "sup_neg_all","sup_pos_all",
                                            "care_trans","ret_trans","rcesdml"), with=FALSE]
hrs_wf_sub <- hrs[raceblack==0&female==1, c("age_100","hwealthlog","hinclog","raedyrs",
                                            "sup_neg_all","sup_pos_all",
                                            "care_trans","ret_trans","rcesdml"), with=FALSE]
hrs_bm_sub <- hrs[raceblack==1&female==0, c("age_100","hwealthlog","hinclog","raedyrs",
                                            "sup_neg_all","sup_pos_all",
                                            "care_trans","ret_trans","rcesdml"), with=FALSE]
hrs_bf_sub <- hrs[raceblack==1&female==1, c("age_100","hwealthlog","hinclog","raedyrs",
                                            "sup_neg_all","sup_pos_all",
                                            "care_trans","ret_trans","rcesdml"), with=FALSE]
#flatten the tables
flattenSquareMatrix(cor.prob(hrs_wm_sub))
#plot the data
chart.Correlation(hrs_wm_sub)

#flatten the tables
flattenSquareMatrix(cor.prob(hrs_wf_sub))
#plot the data
chart.Correlation(hrs_wf_sub)

#flatten the tables
flattenSquareMatrix(cor.prob(hrs_bm_sub))
#plot the data
chart.Correlation(hrs_bm_sub)

#flatten the tables
flattenSquareMatrix(cor.prob(hrs_bf_sub))
#plot the data
chart.Correlation(hrs_bf_sub)

#correlation matrix
rcorr(as.matrix(hrs_wm_sub),type="pearson")
