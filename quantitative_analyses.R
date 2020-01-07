library("TOSTER")
library("ggplot2")
library("dplyr")
library("stringr")
library("reshape2")


#####################################################################################################
#### CHUNK 1
#####################################################################################################

#source("analysis.R")
# load the dataset from RDS created with codebook
alldata <- readRDS("positive_results_in_registered_reports_codebook.rds")
# alldata <- readRDS("C:/Users/20176208/surfdrive/Documents/_projects/_positiveresults/_data/positive_results_in_registered_reports_codebook.rds")
# alldata <- readRDS("~/surfdrive/Documents/_projects/_positiveresults/_data/positive_results_in_registered_reports_codebook.rds")

# remove excluded papers
included <- alldata[alldata$include_in_analysis==1,]

## 3. Run main preregistered analysis: the proportion of SRs with supported
## hypothesis compared to the proportion of RRs with supported hypothesis

# number of included SRs = 152
n.SR <- length(which(included$is_RR == 0))
# number of included RRs = 71
n.RR <- length(which(included$is_RR == 1))

# number of SRs with support = 146
n.support.SR <- length(which(included$is_RR == 0 & included$support_binary == 1))
# number of RRs with support = 31
n.support.RR <- length(which(included$is_RR == 1 & included$support_binary == 1))

# Positive result rate of SRs (proportion of SRs with support) = .9605
prop.support.SR <- n.support.SR/n.SR
# Positive result rate of RRs (proportion of RRs with support) = .4366
prop.support.RR <- n.support.RR/n.RR

## 3.1 Proportions test to test if RRs have a *lower* positive
## result rate than SRs (i.e., one-sided test)

proptestresult <- prop.test(c(n.support.SR, n.support.RR), c(n.SR, n.RR), alternative = "greater")


## 3.2 Equivalence test to test if the difference is larger than 6%
# Equivalence bounds/smallest effect size of interest (SESOI):
# As preregistered, we used the difference between the positive result
# rate in Psychology and in the lowest-scoring 'soft' science in 
# Fanelli (2010) as our SESOI. The lowest-scoring 'soft' science
# was General Social Sciences with a positive result rate of 85.5%
# (this number is not explicitly given in the text, so we extracted it
# from Figure 1 using WebPlotDigitizer). The positive result rate in 
# Psychology was 91.5%.
# Hence our SESOI is 91.5%-85.5% = 6%:

SESOI <- abs(91.5 - 85.5)/100

## Run proportions test and equivalence test using the TOSTER function
## TOSTtwo.prop:
tostresult <- TOSTtwo.prop(prop1 = prop.support.SR, 
                           prop2 = prop.support.RR, 
                           n1 = n.SR, 
                           n2 = n.RR, 
                           low_eqbound = -SESOI, 
                           high_eqbound = SESOI,
                           alpha = 0.05)

## Calculate confidence intervals for SR and RR PRR separately
SR.binom <- binom.test(x = prop.support.SR*n.SR, n = n.SR)
RR.binom <- binom.test(x = prop.support.RR*n.RR, n = n.RR)

#####################################################################################################

#####################################################################################################
#### CHUNK 2
#####################################################################################################

## 5. Create a plot for the main analysis

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

# Plot the main result (positive result rate in SRs vs RRs)
mainplot <- ggplot(mainplot.df, aes(x = is_RR, y = value, fill = support)) +
                    geom_bar(stat = "identity", 
                             width = 0.5) +
                    scale_fill_manual(values= c("#bcbddc", "#756bb1"),
                                      name="first hypothesis") +
                    annotate("text", label = "n = 152", x = 1, y = 105) +
                    annotate("text", label = "n = 71", x = 2, y = 105) +
                    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.05, size = 0.5) +
                    scale_x_discrete(breaks = waiver(), 
                                     labels = c("SR" = "Standard\nReports (SRs)",
                                                "RR" = "Registered\nReports (RRs)"), name = NULL)+ 
                    scale_y_continuous(lim=c(0,110), breaks = c(seq(0, 100, 10)),
                                       minor_breaks = c(seq(0, 100, 5)),
                                       name = "% of papers", expand = c(0, 0))+
                    theme_minimal()+
                    theme(panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          legend.margin = margin(0,0,0,-5))
#####################################################################################################

#####################################################################################################
#### CHUNK 3
#####################################################################################################
## 4. Run exploratory analysis: the proportion of replication 
## studies with supported hypothesis compared to the proportion 
## of original studies with supported hypothesis

# total number of replication studies = 46
n.rep.total <- length(which(included$is_replication == 1))

# number of replication SRs = 4
n.rep.SR <- length(which(included$is_RR == 0 & 
                           included$is_replication == 1))
# proportion of replications among all SRs = 2.63%
prop.rep.SR <- n.rep.SR/n.SR

# number of replication RRs = 42
n.rep.RR <- length(which(included$is_RR == 1 & 
                           included$is_replication == 1))
# proportion of replications among all RRs = 59.15%
prop.rep.RR <- n.rep.RR/n.RR

# Positive result rate of *replication* SRs = 1
prop.rep.support.SR <- length(which(included$is_RR == 0 & 
                                      included$support_binary == 1 &
                                      included$is_replication == 1))/n.rep.SR
# Positive result rate of *original* SRs = .9595
prop.orig.support.SR <- length(which(included$is_RR == 0 & 
                                       included$support_binary == 1 &
                                       included$is_replication == 0))/(n.SR - n.rep.SR)

# Positive result rate of *replication* RRs = .4048
prop.rep.support.RR <- length(which(included$is_RR == 1 & 
                                      included$support_binary == 1 &
                                      included$is_replication == 1))/n.rep.RR
# Positive result rate of *original* RRs = .4828
prop.orig.support.RR <- length(which(included$is_RR == 1 & 
                                       included$support_binary == 1 &
                                       included$is_replication == 0))/(n.RR - n.rep.RR)



## Calculate confidence intervals for original vs 
## replication SRs and RRs
SR.orig.binom <- binom.test(x = prop.orig.support.SR*(n.SR-n.rep.SR), n = n.SR-n.rep.SR)
SR.rep.binom <- binom.test(x = prop.rep.support.SR*n.rep.SR, n = n.rep.SR)
RR.orig.binom <- binom.test(x = prop.orig.support.RR*(n.RR-n.rep.RR), n = n.RR-n.rep.RR)
RR.rep.binom <- binom.test(x = prop.rep.support.RR*n.rep.RR, n = n.rep.RR)

## 4.1 Proportions test to test if *original* RRs still have a lower 
## positive result rate than *original* SRs (i.e., one-sided test)
repproptest <- prop.test(c(prop.orig.support.SR*(n.SR-n.rep.SR), 
                           prop.orig.support.RR*(n.RR-n.rep.RR)), 
                         c(n.SR-n.rep.SR, n.RR-n.rep.RR), alternative = "greater")

## 4.2 Equivalence test to test if the difference is larger than 6%
# Equivalence bounds/smallest effect size of interest (SESOI):
# same as for main hypothesis test (6%)

SESOI <- abs(91.5 - 85.5)/100

## Run proportions test and equivalence test using the TOSTER function
## TOSTtwo.prop:
reptost <- TOSTtwo.prop(prop1 = prop.orig.support.SR, 
                        prop2 = prop.orig.support.RR, 
                        n1 = n.SR-n.rep.SR, 
                        n2 = n.RR-n.rep.RR, 
                        low_eqbound = -SESOI, 
                        high_eqbound = SESOI,
                        alpha = 0.05)

## 4.3 Create a table showing the positive result rate for replications vs
## original studies in RRs vs SRs
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
#####################################################################################################

#####################################################################################################
#### CHUNK 4
#####################################################################################################
## 5. Run exploratory analysis: compare SRs to Fanelli (2010)

# Number of Psychology papers in Fanelli (2010, from Fig. 1)
n.Fanelli <- 141
# Fanelli (2010) reports the positive result rate for Psychiatry&Psychology as
# 91.5% (p. 3). This is likely a rounded number since 141*0.915 = 129.015. For
# a more precise analysis, we assume that 129 out of 141 papers in Fanelli's 
# sample had positive results, giving a positive result rate of 129/141 = 0.9148936.
# In the following, we'll use this more precise number (129/141) in place of the
# 91.5% reported in the paper.
prop.support.Fanelli <- 129/141

## 5.1 Proportions test to test if the positive result rate of Psychology
## papers in Fanelli (2010) is different from SRs in our study

Fanelliproptest <- prop.test(c(prop.support.Fanelli*n.Fanelli,
                               prop.support.SR*n.SR),
                             c(n.Fanelli, n.SR), alternative = "two.sided")

## 5.2 Equivalence test to test if the difference is larger than +/-6%
# Equivalence bounds/smallest effect size of interest (SESOI):
# same as for main hypothesis test (6%)

SESOI <- abs(91.5 - 85.5)/100

## Run proportions test and equivalence test using the TOSTER function
## TOSTtwo.prop:
Fanellitost <- TOSTtwo.prop(prop1 = prop.support.Fanelli, 
                            prop2 = prop.support.SR, 
                            n1 = n.Fanelli, 
                            n2 = n.SR, 
                            low_eqbound = -SESOI, 
                            high_eqbound = SESOI,
                            alpha = 0.05)
#####################################################################################################

#####################################################################################################
#### CHUNK 5
#####################################################################################################
## 6. Analyse the phrases used to introduce hypotheses in RRs

## 6.1 Check how many RRs would have been discovered with the search
## phrase "test* the hypothes*"

# Create a new variable that indicates if the *title* contains 
# the phrase "test* the hypothes*" (TRUE vs FALSE)
included$searchphrase.in.title <- sapply(included$title, grepl, 
                                         pattern = "test\\w* the hypothes")
# Create a new variable that indicates if the *keywords* contain 
# the phrase "test* the hypothes*" (TRUE vs FALSE)
included$searchphrase.in.keywords <- sapply(included$keywords, grepl, 
                                            pattern = "test\\w* the hypothes")
# Create a new variable that indicates if the *abstract* contains 
# the phrase "test* the hypothes*" (TRUE vs FALSE)
included$searchphrase.in.abstract <- sapply(included$abstract, grepl, 
                                            pattern = "test\\w* the hypothes")

# Calculate the number of RRs that contain the search phrase in their abstract,
# title, or keywords: 2 RRs
testthehypothes.RR <- length(which(included$is_RR == 1 & 
                                     (included$searchphrase.in.abstract == TRUE |
                                        included$searchphrase.in.keywords == TRUE |
                                        included$searchphrase.in.title == TRUE)))
#####################################################################################################

