####################################################################################################
## Author: Julia Morris
## Description: Creates ropeladder plot for Chapter 1 negative binomial regression models.
## Output: Ropeladder plot.
## NOTE: Models and scenarios can be adjusted. Current code includes Model 3c; 0.5 SDs.
####################################################################################################

#load packages
library(data.table)
library(tile)
library(simcf)
library(MASS)

#set directories
code_dir <- "C:/Users/julia/Dropbox/HRS/code_dir/"
data_dir <- "C:/Users/julia/Dropbox/HRS/data_dir/"
output_dir <- "C:/Users/julia/Dropbox/HRS/output_dir/"

#load data
hrs <- fread(paste0(data_dir, "randhrs_pslbq_imputed_ch1.csv"))
hrs <- hrs[, V1 := NULL]

#subset to wave 11 for analyses
hrs <- hrs[wave==11]

#specify model -- model 3c
model <- (rcesdml ~ rageyb + hwealthlog + hinclog + raedyrs + 
            ret_trans + care_trans + rretired + care_any)

# Estimate white males
wm1.res <- glm.nb(model, data = hrs[which(hrs$female==0&hrs$raceblack==0),], weights = rawtsamp)
wm1.pe <- wm1.res$coefficients        # point estimates
wm1.vc <- vcov(wm1.res)               # var-cov matrix

# Estimate white females
wf1.res <- glm.nb(model, data = hrs[which(hrs$female==1&hrs$raceblack==0),], weights = rawtsamp)
wf1.pe <- wf1.res$coefficients        # point estimates
wf1.vc <- vcov(wf1.res)               # var-cov matrix

# Estimate black males
bm1.res <- glm.nb(model, data = hrs[which(hrs$female==0&hrs$raceblack==1),], weights = rawtsamp)
bm1.pe <- bm1.res$coefficients         # point estimates
bm1.vc <- vcov(bm1.res)                # var-cov matrix

# Estimate black females
bf1.res <- glm.nb(model, data = hrs[which(hrs$female==1&hrs$raceblack==1),], weights = rawtsamp)
bf1.pe <- bf1.res$coefficients         # point estimates
bf1.vc <- vcov(bf1.res)                # var-cov matrix

# Initialize 7 different scenarios to mean values of covariates
xscen <- cfMake(model, data=hrs, nscen=4)

# Configure scenario 1:  Raise age by 1/2 sd

# Configure scenario 4:  Raise prob of retirement by 1/2 sd
xscen <- cfName(xscen, "Paid Labor Trans +0.5 sd", scen=1)
xscen <- cfChange(xscen, "ret_trans", 
                  x = mean(hrs$ret_trans,na.rm=T) + 0.5*sd(hrs$ret_trans,na.rm=T), 
                  scen=1)

# Configure scenario 5:  Raise prob of caregiver by 1/2 sd
xscen <- cfName(xscen, "Caregiver Trans +0.5 sd", scen=2)
xscen <- cfChange(xscen, "care_trans",  
                  x = mean(hrs$care_trans,na.rm=T) + 0.5*sd(hrs$care_trans,na.rm=T), 
                  scen=2)

xscen <- cfName(xscen, "Retired +0.5 sd", scen=3)
xscen <- cfChange(xscen, "rretired", 
                  x = mean(hrs$rretired,na.rm=T) + 0.5*sd(hrs$rretired,na.rm=T), 
                  scen=3)

xscen <- cfName(xscen, "Caregiver +0.5 sd", scen=4)
xscen <- cfChange(xscen, "care_any", 
                  x = mean(hrs$care_any,na.rm=T) + 0.5*sd(hrs$care_any,na.rm=T), 
                  scen=4)


# Simulate conditional expectations for these counterfactuals
sims <- 10000

# White male simulations
simbetas.wm <- mvrnorm(sims, wm1.pe, wm1.vc)   
wm1.qoi <- loglinsimfd(xscen, simbetas.wm, ci=0.95)

# White female simulations
simbetas.wf <- mvrnorm(sims, wf1.pe, wf1.vc)      
wf1.qoi <- loglinsimfd(xscen, simbetas.wf, ci=0.95)

# Black male simulations
simbetas.bm <- mvrnorm(sims, bm1.pe, bm1.vc)     
bm1.qoi <- loglinsimfd(xscen, simbetas.bm, ci=0.95)

# Black female simulations
simbetas.bf <- mvrnorm(sims, bf1.pe, bf1.vc)      
bf1.qoi <- loglinsimfd(xscen, simbetas.bf, ci=0.95)

# Create ropeladder traces of first differences from each model
trace1 <- ropeladder(x=wm1.qoi$pe,
                     lower=wm1.qoi$lower,
                     upper=wm1.qoi$upper,
                     labels=row.names(xscen$x),
                     plot=1
)

trace2 <- ropeladder(x=wf1.qoi$pe,
                     lower=wf1.qoi$lower,
                     upper=wf1.qoi$upper,
                     plot=2
)

trace3 <- ropeladder(x=bm1.qoi$pe,
                     lower=bm1.qoi$lower,
                     upper=bm1.qoi$upper,                   
                     plot=3
)

trace4 <- ropeladder(x=bf1.qoi$pe,
                     lower=bf1.qoi$lower,
                     upper=bf1.qoi$upper,
                     plot=4
)

rug1 <- rugTile(x = hrs$rcesdml - mean(hrs$rcesdml,na.rm=T),
                plot = 1:4
)

vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "solid",
                      plot = 1:4
)

# Make version of these traces for a single plot
singlerug1 <- rugTile(x = hrs$rcesdml - mean(hrs$rcesdml,na.rm=T),
                      plot = 1
)

singlevertmark <- linesTile(x = c(0,0),
                            y = c(0,1),
                            lty = "solid",
                            plot = 1
)

# Revise traces to place on same plot
trace1$plot <- trace2$plot <- trace3$plot <- trace4$plot <- 1
vertmark$plot <- 1

# Revise traces to make symbols different
trace1$pch <- 19
trace2$pch <- 15
trace3$pch <- 17
trace4$pch <- 23

# Add sublabels to each trace
trace1$sublabels <- "White Males"
trace2$sublabels <- "White Females"
trace3$sublabels <- "Black Males"
trace4$sublabels <- "Black Females"

# Widen space between entries to make labels visible
trace1$entryheight <- 0.45

# Shift sublabels to left side of plot to avoid overlap
trace1$sublabelsX <- 0.07
trace2$sublabelsX <- 0.07
trace3$sublabelsX <- 0.07
trace4$sublabelsX <- 0.07

# Add boxes around the results for each covariate 
# when traces are plotted to the same graph 
# (could add to any of the traces)
trace1$shadowrow <- TRUE

# Collect in matrix form all first differences and confidence 
# intervals across models (columns) and covariates (rows)
allPE <- cbind(wm1.qoi$pe, wf1.qoi$pe, bm1.qoi$pe, bf1.qoi$pe)
allLOWER <- cbind(wm1.qoi$lower, 
                  wf1.qoi$lower, 
                  bm1.qoi$lower, 
                  bf1.qoi$lower)
allUPPER <- cbind(wm1.qoi$upper, 
                  wf1.qoi$upper, 
                  bm1.qoi$upper,  
                  bf1.qoi$upper)

# Create a trace for each covariate of the 
# different models' estimates
# (Save these traces in a vector of traces; 
#  note double bracket indexing)

collectedtraces <- vector("list", nrow(allPE))

for (i in 1: nrow(allPE)) {
  collectedtraces[[i]] <- ropeladder(x = allPE[i,],
                                     lower = allLOWER[i,],
                                     upper = allUPPER[i,],
                                     shadowbox = TRUE,
                                     plot = i
  )
}

# Add ropeladder labels to first and fifth plots
# (The first ropeladder in each row of plots; 
#  note double bracket indexing)
collectedtraces[[1]]$labels <- 
  collectedtraces[[3]]$labels <- 
  c("White Males", "White Females", 
    "Black Males", "Black Females")

# Revise vertical mark to plot on all seven plots
vertmark$plot <- 1:4

tile(collectedtraces, vertmark,
     RxC = c(2,2),
     limits = c(-0.5,0.5),
     width = list(spacer=4),
     output = list(file="C:/Users/julia/Dropbox/HRS/output_dir/ropeladder_3c.pdf", width=20),       
     xaxis = list(at = c(-0.5,-0.25,0,0.25,0.5)),   
     xaxistitle = list(labels="E(CES-D)"),
     topaxis= list(at = mean(hrs$rcesdml,na.rm=T)*c(0.75, 1,1.25) 
                   - mean(hrs$rcesdml,na.rm=T),
                   labels = c(".75x","1x","1.25x"),
                   add = rep(TRUE,4)),
     topaxistitle = list(labels="E(CES-D) / average"),
     plottitle = list(labels1 = "Pr(Paid Labor Trans)",
                      labels2 = "Pr(Caregiving Trans)",
                      labels3 = "Pr(Retired)",
                      labels4 = "Pr(Caregiver)"),
     gridlines=list(type="top"))
