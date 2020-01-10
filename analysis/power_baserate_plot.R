#########################################################
#####                                               #####
##### Script 4: Calculate and plot power and the    #####
#####           rate of true hypotheses for the     #####
#####           observed positive result rates in   #####
#####           the absence of bias                 #####
#####                                               #####
#####-----------------------------------------------#####
#####                                               #####
##### Content:                                      #####
#####  * 1: Setup                                   #####
#####  * 2: Functions to calculate power and rate   #####
#####       of true hypotheses                      #####
#####  * 3: Plot functions with observed positive   #####
#####       result rates as input                   #####
#####                                               #####
##### Note:                                         #####
#####   RR = Registered Report                      #####
#####   SR = standard report                        #####
#####   PRR = positive result rate                  #####
#####                                               #####
#########################################################

##==============================================================================##
## 1. Setup: load packages and data 
##==============================================================================##

# The ggplot2 package is needed to create the plot. 
# Install the package, if needed, by uncommenting the following line:
# install.packages("ggplot2") 

# Load packages:
library(ggplot2)

# Load data: Creating the plot requires input from the main analysis script
# (quantitative_analyses.R). In the RMarkdown file for the manuscript, this
# script is already loaded when the code below is run. The respective variables
# are *not* hard-coded or recalculated here to avoid errors or inconsistencies
# if any changes to the data or main analysis script should be made in the future.
# Therefore, if you wish to run this script outside of the RMarkdown environment 
# of the manuscript, you first need to load the main analysis script (and with it,
# the data). Adjust the code to load the script if necessary.

# source("quantitative_analyses.R")


##==============================================================================##
## 2. Functions to calculate power and rate of true hypotheses 
##    Inferring the proportion of true hypotheses studied in the literature
##    and statistical power based on the observed proportion of 
##    positive results
##==============================================================================##

# The functions calculated below are based on the following equation:
# PPR = a*(1-t) + (1-b)*t, with
# PRR: the positive result rate (the proportion of positive results in the literature),
# t: the proportion of true hypotheses in the literature,
# a: alpha (the probability of a positive result when testing a false hypothesis), and
# 1-b: power (the probability of a positive result when testing a true hypothesis)
# 
# Solving for 1-b gives
# 1-b = (PRR - a + a*t)/t
# As a function:
pwrfun <- function(t, a, PRR){(PRR -a + a*t)/t}

# We will assume a fixed alpha level of 5% for all of the following:
a <- 0.05

##==============================================================================##
## 3. Plot functions with observed positive result rates as input
##    The main comparison of interest is *original* standard reports versus
##    *original* Registered Reports (based on the suspicion that replication
##    RRs might have a lower base rate of true hypotheses), but we will plot
##    the values for all RRs (original and replications) as well because it
##    may be of general interest.
##    We thus have 3 groups:
##    * original SRs (n = 148)
##    * original RRs (n = 30)
##    * all RRs (n = 71)
##==============================================================================##

## 3.1 Create a dataframe for each group and fill it with calculated power
##     and baserate values

# Create dataframe for original SRs
pwrprior.origSR <- data.frame(t=c(1:1000)/1000) # Enter values for t from .001 - 1
pwrprior.origSR$group <- "original SRs (PRR = .9595, n = 148)" # group label (later used for plot)

# Calculate values for power (1-b) for all values of t and:
# - for the lower end of the 95% CI for the positive result rate
# - for the positive result rate estimate
# - for the upper end of the 95% CI for the positive result rate
pwrprior.origSR$lower <- pwrfun(t = pwrprior.origSR$t, a = a, PRR = min(SR.orig.binom$conf.int))
pwrprior.origSR$estimate <- pwrfun(t = pwrprior.origSR$t, a = a, PRR = prop.orig.support.SR)
pwrprior.origSR$upper <- pwrfun(t = pwrprior.origSR$t, a = a, PRR = max(SR.orig.binom$conf.int))   


# Create dataframe for original RRs
pwrprior.origRR <- data.frame(t=c(1:1000)/1000) # Enter values for t from .001 - 1
pwrprior.origRR$group <- "original RRs (PRR = .5, n = 30)" # group label (later used for plot)

# Calculate values for power (1-b) for all values of t and:
# - for the lower end of the 95% CI for the positive result rate
# - for the positive result rate estimate
# - for the upper end of the 95% CI for the positive result rate
pwrprior.origRR$lower <- pwrfun(t = pwrprior.origRR$t, a = a, PRR = min(RR.orig.binom$conf.int))
pwrprior.origRR$estimate <- pwrfun(t = pwrprior.origRR$t, a = a, PRR = prop.orig.support.RR)
pwrprior.origRR$upper <- pwrfun(t = pwrprior.origRR$t, a = a, PRR = max(RR.orig.binom$conf.int))   


# Create dataframe for all RRs
pwrprior.allRRs <-  data.frame(t=c(1:1000)/1000) # Enter values for t from .001 - 1
pwrprior.allRRs$group <- "all RRs (PRR = .4366, N = 71)" # group label (later used for plot)

# Calculate values for power (1-b) for all values of t and:
# - for the lower end of the 95% CI for the positive result rate
# - for the positive result rate estimate
# - for the upper end of the 95% CI for the positive result rate
pwrprior.allRRs$lower <- pwrfun(t = pwrprior.allRRs$t, a = a, PRR = min(RR.binom$conf.int))
pwrprior.allRRs$estimate <- pwrfun(t = pwrprior.allRRs$t, a = a, PRR = prop.support.RR)
pwrprior.allRRs$upper <- pwrfun(t = pwrprior.allRRs$t, a = a, PRR = max(RR.binom$conf.int))   


## 3.2 prepare the plot

# Combine all three dataframes into one:
pwrprior.df <- rbind(pwrprior.allRRs, pwrprior.origRR, pwrprior.origSR)

# Truncate power at 1 (the function does not know that power can't
# exceed 100% and it causes problems during plotting)
pwrprior.df$lower[pwrprior.df$lower > 1 & pwrprior.df$upper > 1] <- NA
pwrprior.df$lower[pwrprior.df$lower > 1] <- 1
pwrprior.df$upper[pwrprior.df$upper > 1] <- 1

# Turn the group labels into factors and make sure they're listed in the correct order
pwrprior.df$group <- factor(pwrprior.df$group, 
                            levels = c("original SRs (PRR = .9595, n = 148)",
                                       "original RRs (PRR = .5, n = 30)", 
                                       "all RRs (PRR = .4366, N = 71)"))

## 3.3 Create the plot
power.baserate.plot <- ggplot(pwrprior.df, aes(x = t, y = estimate, colour = group, 
                                               fill = group, linetype = group)) +
                              geom_line(aes(y = estimate), size = 1) + 
                              geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .42, linetype = "blank") +
                              scale_x_continuous(name="proportion of true hypotheses", 
                                                 limits=c(0.25, 1), 
                                                 breaks = c(seq(0.3, 1, 0.1)),
                                                 expand = c(0, 0)) +
                              scale_y_continuous(name="statistical power", 
                                                 limits=c(0.25, 1), 
                                                 breaks = c(seq(0.3, 1, 0.1)),
                                                 expand = c(0, 0)) +
                              scale_colour_manual(values = c("#0073C0", "#ED443F", "#2D132C"), name = NULL) +
                              scale_fill_manual(values = c("#0073C0", "#ED443F", "#2D132C"), name = NULL) +
                              scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = NULL) +
                              theme_bw()