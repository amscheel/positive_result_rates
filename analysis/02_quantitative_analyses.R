#########################################################
#####                                               #####
##### Script 2: Quantitative analyses               #####
#####           (confirmatory and exploratory)      #####
#####                                               #####
#####-----------------------------------------------#####
#####                                               #####
##### Content:                                      #####
#####  * 1: Setup                                   #####
#####  * 2: Main confirmatory analysis:             #####
#####       positive result rate in Registered      #####
#####       Reports compared to standard reports    #####
#####  * 3: Exploratory analysis:                   #####
#####       positive result rate in replication     #####
#####       studies compared to original studies    #####
#####  * 4: Exploratory analysis:                   #####
#####       positive result rate in standard        #####
#####       reports compared to Fanelli (2010)      #####
#####  * 5: Robustness check:                       #####
#####       Run main analysis without replacement   #####
#####       standard reports                        #####
#####                                               #####
##### Note:                                         #####
#####   RR = Registered Report                      #####
#####   SR = standard report                        #####
#####                                               #####
#########################################################

##==============================================================================##
## 1. Setup: load packages and data 
##==============================================================================##

# Four packages are needed: TOSTER to calculate equivalence tests, ggplot2 for
# creating the plot for Figure 2 in the manuscript, papaja for Table 1 in 
# the manuscript, and here to locate the data file when loading it. If you  
# want to run these analyses outside of our R project, you may need to adjust
# the command to load the data, and you can do this without making use of the
# here package. papaja was not yet on CRAN when this was written and has to 
# be installed from Github, using the devtools package. Its role in this script
# is very minor though -- all analyses can be reproduced without papaja.

# Install packages if needed by uncommenting the respective line(s):
# install.packages("here")  # install here
# install.packages("TOSTER")  # install TOSTER
# install.packages("ggplot2")  # install ggplot2
# install.packages("devtools")  # install devtools to install papaja
# devtools::install_github("crsh/papaja")  # install papaja

# Load packages:
library("TOSTER")
library("ggplot2")
library("papaja")
library("here")

# load the dataset from the RDS file created with codebook
alldata <- readRDS(here("raw_data", "positive_results_in_registered_reports_codebook.rds"))
# remove excluded papers
included <- alldata[alldata$include_in_analysis==1,]


##==============================================================================##
## 2. Main confirmatory analysis:
##    Compare the proportion of SRs with supported hypotheses (i.e., positive 
##    results) to the proportion of RRs with supported hypotheses
##==============================================================================##

## 2.1 Calculate group sizes and positive result rates in both groups

# Calculate the number of included SRs (= 152)
n.SR <- length(which(included$is_RR == 0))
# Calculate the number of included RRs (= 71)
n.RR <- length(which(included$is_RR == 1))

# Calculate the number of SRs with support (= 146)
n.support.SR <- length(which(included$is_RR == 0 & included$support_binary == 1))
# Calculate the number of RRs with support (= 31)
n.support.RR <- length(which(included$is_RR == 1 & included$support_binary == 1))

# Positive result rate of SRs (proportion of SRs with support) = .9605
prop.support.SR <- n.support.SR/n.SR
# Positive result rate of RRs (proportion of RRs with support) = .4366
prop.support.RR <- n.support.RR/n.RR


## 2.2 Proportions test to test if RRs have a *lower* positive
##     result rate than SRs (i.e., one-sided test)

proptestresult <- prop.test(c(n.support.SR, n.support.RR), 
                            c(n.SR, n.RR), alternative = "greater")

# Calculate 95% confidence intervals for the positive result rate in SRs and RRs
SR.binom <- binom.test(x = n.support.SR, n = n.SR)
RR.binom <- binom.test(x = n.support.RR, n = n.RR)

min(SR.binom$conf.int) # lower end of CI for SRs
max(SR.binom$conf.int) # upper end of CI for SRs
min(RR.binom$conf.int) # lower end of CI for RRs
max(RR.binom$conf.int) # upper end of CI for RRs


## 2.3 Equivalence test to test if the difference is larger than 6%
##
##     Equivalence bounds/smallest effect size of interest (SESOI):
##     As preregistered, we used the difference between the positive result
##     rate in Psychology and in the lowest-scoring 'soft' science in 
##     Fanelli (2010) as our SESOI. The lowest-scoring 'soft' science
##     was General Social Sciences with a positive result rate of 85.5%
##     (this number is not explicitly given in the text, so we extracted it
##     from Figure 1 using WebPlotDigitizer). The positive result rate in 
##     Psychology was 91.5%.
##     Hence our SESOI is 91.5%-85.5% = 6%:

SESOI <- abs(91.5 - 85.5)/100

# Run equivalence test using the TOSTER function TOSTtwo.prop:
tostresult <- TOSTtwo.prop(prop1 = prop.support.SR, 
                           prop2 = prop.support.RR, 
                           n1 = n.SR, 
                           n2 = n.RR, 
                           low_eqbound = -SESOI, 
                           high_eqbound = SESOI,
                           alpha = 0.05,
                           verbose = FALSE)  # set to TRUE if you want to see output printed


## 2.4 Create a plot for the main analysis (Figure 2 in the manuscript)

# Create a data frame to use for plotting the main result
mainplot.df <- data.frame(is_RR = c("SR", "SR", "RR", "RR"), 
                          support = c("supported", "not supported", 
                                      "supported", "not supported"),
                          value = c(prop.support.SR*100, (1-prop.support.SR)*100, 
                                    prop.support.RR*100, (1-prop.support.RR)*100),
                          lower = c(min(SR.binom$conf.int)*100, NA,
                                    min(RR.binom$conf.int)*100, NA),
                          upper = c(max(SR.binom$conf.int)*100, NA,
                                    max(RR.binom$conf.int)*100, NA))

# Reorder the group levels so that SRs come first, RRs second
mainplot.df$is_RR <- factor(mainplot.df$is_RR, levels = c("SR", "RR"))

# Plot the main result (positive result rate in SRs vs RRs)
mainplot <- ggplot(mainplot.df, aes(x = is_RR, y = value, fill = support)) +
                    geom_bar(stat = "identity", 
                             width = 0.5) +
                    scale_fill_manual(values= c("#bcbddc", "#756bb1"),
                                      name="first hypothesis") +
                    annotate("text", label = paste("N =", n.SR), 
                             x = 1, y = 105, size = 5) +
                    annotate("text", label = paste("N =", n.RR), 
                             x = 2, y = 105, size = 5) +
                    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.05, size = 0.5) +
                    scale_x_discrete(breaks = waiver(), 
                                     labels = c("SR" = "Standard\nReports",
                                                "RR" = "Registered\nReports"), 
                                     name = NULL)+ 
                    scale_y_continuous(lim=c(0,110), breaks = c(seq(0, 100, 10)),
                                       minor_breaks = c(seq(0, 100, 5)),
                                       name = "% of papers", expand = c(0, 0))+
                    theme_minimal(base_size = 20)+
                    theme(panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          legend.margin = margin(0,0,0,-5.5)) +
                    coord_fixed(ratio=0.03)


##==============================================================================##
## 3. Exploratory analysis:
##    Compare the positive result rate of replication studies (i.e., studies 
##    that were classified as direct replications of previously published work)
##    to the positive result rate of original studies
##==============================================================================##

## 3.1 Calculate group sizes and positive result rates in both groups 
##     (replication versus original studies)

# total number of replication studies = 45
n.rep.total <- length(which(included$is_replication == 1))

# number of replication SRs = 4
n.rep.SR <- length(which(included$is_RR == 0 & 
                           included$is_replication == 1))
# proportion of replications among all SRs = 2.63%
prop.rep.SR <- n.rep.SR/n.SR

# number of replication RRs = 41
n.rep.RR <- length(which(included$is_RR == 1 & 
                           included$is_replication == 1))
# proportion of replications among all RRs = 57.75%
prop.rep.RR <- n.rep.RR/n.RR

# Positive result rate of *replication* SRs = 1
prop.rep.support.SR <- length(which(included$is_RR == 0 & 
                                      included$support_binary == 1 &
                                      included$is_replication == 1))/n.rep.SR

# Positive result rate of *original* SRs = .9595
prop.orig.support.SR <- length(which(included$is_RR == 0 & 
                                       included$support_binary == 1 &
                                       included$is_replication == 0))/(n.SR - n.rep.SR)

# Positive result rate of *replication* RRs = .3602
prop.rep.support.RR <- length(which(included$is_RR == 1 & 
                                      included$support_binary == 1 &
                                      included$is_replication == 1))/n.rep.RR

# Positive result rate of *original* RRs = .5
prop.orig.support.RR <- length(which(included$is_RR == 1 & 
                                       included$support_binary == 1 &
                                       included$is_replication == 0))/(n.RR - n.rep.RR)


## 3.2 Proportions test to test if *original* RRs still have a lower 
##     positive result rate than *original* SRs (i.e., one-sided test)

origproptest <- prop.test(c(prop.orig.support.SR*(n.SR-n.rep.SR), 
                            prop.orig.support.RR*(n.RR-n.rep.RR)), 
                          c(n.SR-n.rep.SR, n.RR-n.rep.RR), alternative = "greater")

# Calculate 95% confidence intervals for the positive result rate in
# original vs replication SRs and RRs
SR.orig.binom <- binom.test(x = prop.orig.support.SR*(n.SR-n.rep.SR), n = n.SR-n.rep.SR)
SR.rep.binom <- binom.test(x = prop.rep.support.SR*n.rep.SR, n = n.rep.SR)
RR.orig.binom <- binom.test(x = prop.orig.support.RR*(n.RR-n.rep.RR), n = n.RR-n.rep.RR)
RR.rep.binom <- binom.test(x = prop.rep.support.RR*n.rep.RR, n = n.rep.RR)

min(SR.orig.binom$conf.int) # lower end of CI for original SRs
max(SR.orig.binom$conf.int) # upper end of CI for original SRs
min(SR.rep.binom$conf.int)  # lower end of CI for replication SRs
max(SR.rep.binom$conf.int)  # upper end of CI for replication SRs
min(RR.orig.binom$conf.int) # lower end of CI for original RRs
max(RR.orig.binom$conf.int) # upper end of CI for original RRs
min(RR.rep.binom$conf.int)  # lower end of CI for replication RRs
max(RR.rep.binom$conf.int)  # upper end of CI for replication RRs


## 3.3 Equivalence test to test if the difference is larger than 6% 
##     (using the same SESOI as before, see 2.4)

## Run equivalence test using the TOSTER function TOSTtwo.prop:
reptost <- TOSTtwo.prop(prop1 = prop.orig.support.SR, 
                        prop2 = prop.orig.support.RR, 
                        n1 = n.SR-n.rep.SR, 
                        n2 = n.RR-n.rep.RR, 
                        low_eqbound = -SESOI, 
                        high_eqbound = SESOI,
                        alpha = 0.05,
                        verbose = FALSE)  # set to TRUE if you want to see output printed

## 3.4 Create a table showing the positive result rate for replications vs
##     original studies in SRs and RRs (Table 1 in the manuscript)
rep.table <- data.frame(group = c("SRs", "RRs"),
                        n.original = c(n.SR-n.rep.SR, n.RR-n.rep.RR),
                        pos.original = c(prop.orig.support.SR*(n.SR-n.rep.SR), 
                                         prop.orig.support.RR*(n.RR-n.rep.RR)),
                        percent.pos.original = c(prop.orig.support.SR*100,
                                                 prop.orig.support.RR*100), 
                        CI.original = c(paste(printnum(min(SR.orig.binom$conf.int)*100),
                                              printnum(max(SR.orig.binom$conf.int)*100), 
                                              sep = "; "),
                                        paste(printnum(min(RR.orig.binom$conf.int)*100),
                                              printnum(max(RR.orig.binom$conf.int)*100), 
                                              sep = "; ")),
                        n.rep = c(n.rep.SR, n.rep.RR),
                        pos.rep = c(prop.rep.support.SR*n.rep.SR, prop.rep.support.RR*n.rep.RR),
                        percent.pos.rep = c(prop.rep.support.SR*100, prop.rep.support.RR*100), 
                        CI.rep = c(paste(printnum(min(SR.rep.binom$conf.int)*100),
                                         printnum(max(SR.rep.binom$conf.int)*100), sep = "; "),
                                   paste(printnum(min(RR.rep.binom$conf.int)*100),
                                         printnum(max(RR.rep.binom$conf.int)*100), sep = "; ")))



##==============================================================================##
## 4. Exploratory analysis:
##    Compare the positive result rate of SRs to Fanelli (2010)
##==============================================================================##

# Number of Psychology papers in Fanelli (2010, from Fig. 1)
n.Fanelli <- 141

# Fanelli (2010) reports the positive result rate for Psychiatry&Psychology as
# 91.5% (p. 3). This is likely a rounded number since 141*0.915 = 129.015. For
# a more precise analysis, we assume that 129 out of 141 papers in Fanelli's 
# sample had positive results, giving a positive result rate of 129/141 = 0.9148936.
# In the following, we'll use this more precise number (129/141) in place of the
# 91.5% reported in the paper.
prop.support.Fanelli <- 129/141


## 4.1 Proportions test to test if the positive result rate of Psychology
##     papers in Fanelli (2010) is different from SRs in our study (two-sided test)

Fanelliproptest <- prop.test(c(prop.support.Fanelli*n.Fanelli,
                               prop.support.SR*n.SR),
                             c(n.Fanelli, n.SR), alternative = "two.sided")


## 4.2 Equivalence test to test if the difference is larger than +/-6%
##     (using the same SESOI as before, see 2.4)

# Run equivalence test using the TOSTER function TOSTtwo.prop:
Fanellitost <- TOSTtwo.prop(prop1 = prop.support.Fanelli, 
                            prop2 = prop.support.SR, 
                            n1 = n.Fanelli, 
                            n2 = n.SR, 
                            low_eqbound = -SESOI, 
                            high_eqbound = SESOI,
                            alpha = 0.05,
                            verbose = FALSE)  # set to TRUE if you want to see output printed


##==============================================================================##
## 5. Robustness check: Main confirmatory analysis without replacement SRs
##
##    The decision to replace excluded SRs from the initial sample of 150 
##    papers had not been preregistered (and eventually led to oversampling
##    as we discovered that 2 SRs excluded in the first round should 
##    actually be included, meaning that we replaced too many papers).
##    Here we repeat the main analysis from section 2 with the only
##    difference that SRs added after the first coding round are excluded
##    (i.e., analysing 144 SRs rather than 152).
##==============================================================================##

## 5.1 Calculate group size and positive result rates for SRs

# Calculate the number of included SRs (= 144)
n.SR.round1 <- length(which(included$is_RR == 0 & included$coding_round == 1))

# Calculate the number of SRs with support (= 138)
n.support.SR.round1 <- length(which(included$is_RR == 0
                                    & included$coding_round == 1
                                    & included$support_binary == 1))

# Positive result rate of SRs (proportion of SRs with support) = .9583
prop.support.SR.round1 <- n.support.SR.round1/n.SR.round1


## 5.2 Proportions test to test if RRs have a *lower* positive
##     result rate than SRs (i.e., one-sided test)

proptestresult.robcheck <- prop.test(c(n.support.SR.round1, n.support.RR), 
                                     c(n.SR.round1, n.RR), alternative = "greater")

# Calculate 95% confidence intervals for the positive result rate in SRs
SR.binom.round1 <- binom.test(x = n.support.SR.round1, n = n.SR.round1)

min(SR.binom.round1$conf.int) # lower end of CI for SRs
max(SR.binom.round1$conf.int) # upper end of CI for SRs



## 5.3 Equivalence test to test if the difference is larger than 6% 
##     (using the same SESOI as before, see 2.4)

# Run equivalence test using the TOSTER function TOSTtwo.prop:
tostresult.robcheck <- TOSTtwo.prop(prop1 = prop.support.SR.round1, 
                                    prop2 = prop.support.RR, 
                                    n1 = n.SR.round1, 
                                    n2 = n.RR, 
                                    low_eqbound = -SESOI, 
                                    high_eqbound = SESOI,
                                    alpha = 0.05,
                                    verbose = FALSE)  # set to TRUE if you want to see output printed

