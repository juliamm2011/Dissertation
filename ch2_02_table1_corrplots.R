####################################################################################################
## Author: Julia Morris
## Description: Creates Table 1 and correlation plots for Chapter 2. 
## Output:  Table 1, correlation plots.
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
hrs <- fread(paste0(data_dir, "randhrs_pslbq_imputed_ch2.csv"))
hrs <- hrs[, V1 := NULL]

#make difference variables
hrs[, cesd_diff := rcesdml_11 - rcesdml_10]
hrs[, drink_diff := drinkswk_11 - drinkswk_10]
hrs[, bmi_diff := rbmi_11 - rbmi_10]

#create Table 1; library(tableone)
#Create a variable list which we want in Table 1
listVars <- c("chr_sle_11","drinkswk_11","drink_diff",
              "func_lim_11","rbmi_11","bmi_diff",
              "hatota_11","hitot_11","raedyrs","rageyb_11",
              "rcesdml_11","cesd_diff",
              "rconds_11","sup_neg_all_11","sup_pos_all_11",
              "racohbyr","rfairpoorsrh_11","priv_ins_11",
              "rmarpar_11","rretired_11","rec_sle_11",
              "care_trans_11","female","raceblack")
#Define categorical variables
catVars <- c("racohbyr","rfairpoorsrh_11","priv_ins_11","rmarpar_11","rretired_11",
             "care_trans_11","female","raceblack")
#Total Population
table1a <- CreateTableOne(vars = listVars, data = hrs, factorVars = catVars)
#export Table 1 to Microsoft Word; library(ReporteRs); library(magrittr)
table1a <- print(table1a)

#Stratified by Sex and Race
#Remove Sex and Race from list of vars
listVars <- c("drinkswk_11",
              "func_lim_11","rbmi_11",
              "hatota_11","hitot_11","raedyrs","rageyb_11",
              "rcesdml_11",
              "rconds_11","sup_neg_all_11","sup_pos_all_11",
              "racohbyr","rretired_11",
              "care_trans_11")
#Define categorical variables
catVars <- c("racohbyr","rretired_11",
             "care_trans_11")
stratVars <- c("female","raceblack")
table1b <- CreateTableOne(listVars, data = hrs, factorVars = catVars, strata = stratVars)
table1b <- print(table1b)

#write to .csv
write.csv(table1b, paste0(output_dir, "ch2_table1.csv"))

###correlation matrices
#subset data
hrs_sub <- hrs[, c("chr_sle_11","care_trans_11","sup_neg_all_11",
                   "rbmi_11","drinkswk_11","cesd_diff",
                   "rcesdml_11","drink_diff","bmi_diff","rec_sle_11",
                   "female","raceblack"), with=FALSE]
#set up functions
## correlation matrix with p-values
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
#flatten the table
flattenSquareMatrix(cor.prob(hrs_sub))
#plot the data
chart.Correlation(hrs_sub)

#correlation matrix
rcorr(as.matrix(hrs_sub),type="pearson")

