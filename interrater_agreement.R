# load the dataset from RDS created with codebook
alldata <- readRDS("positive_results_in_registered_reports_codebook.rds")

# remove excluded papers
included <- alldata[alldata$include_in_analysis==1,]


##==============================================================================##
## 1. Calculate inter-rater agreement (between MS and AS) for main 
## variable (support_binary)

# number of papers coded by MS: 221 out of 223 = 99.10%
support.coded.MS <- sum(included$coded_by_MS)
# number of papers coded by AS: 70 out of 223 = 31.39%
support.coded.AS <- sum(included$coded_by_AS)

# number of RRs coded by AS: 23 out of 71 = 32.4%
sum(included$coded_by_AS & included$is_RR)/sum(included$is_RR)
# number of CRs coded by AS: 47 out of 152 = 30.9%
sum(included$coded_by_AS & !included$is_RR)/sum(!included$is_RR)

# number of papers double coded: 68 out of 223 = 30.49%
support.doublecoded <- length(which(included$coded_by_MS == 1 & 
                                      included$coded_by_AS ==1))

# number of 'minor' disagreements, i.e. disagreement between 
# full and partial support (no consequences for final analysis):
# 15 out of 68 cases = 22.06%
support.disagreement.minor <- length(which(included$support_orig_MS == "support" & 
                                             included$support_orig_AS == 
                                             "partial support")) +
  length(which(included$support_orig_MS == 
                 "partial support" &
                 included$support_orig_AS == "support"))

# Number of 'major' disagreements, i.e. disagreement between
# no support and either full or partial support (consequential for
# final analysis)

# First create new variables where both full and partial 
# support are re-coded as 1, no support as 0
included$support_major_MS <- NA
included$support_major_AS <- NA

included[which(included$support_orig_MS == "support" | 
                 included$support_orig_MS == "partial support"), 
         "support_major_MS"] <- 1
included[which(included$support_orig_AS == "support" | 
                 included$support_orig_AS == "partial support"),
         "support_major_AS"] <- 1
included[which(included$support_orig_MS == "no support"), "support_major_MS"] <- 0
included[which(included$support_orig_AS == "no support"), "support_major_AS"] <- 0

# Now calculate number and % of major disagreements: 3 out of 52 = 5.77%
support.disagreement.major <- length(which(included$support_major_MS != 
                                             included$support_major_AS))
support.disagreement.major/length(which(!is.na(included$support_major_MS) & 
                                          !is.na(included$support_major_AS)))


# Calculate the observed agreement (p.o.major): number of cases in  
# which MS & AS agree, divided by the number of double coded cases
p.o.major <- length(which(included$support_major_MS == included$support_major_AS))/
  length(which(!is.na(included$support_major_MS) & 
                 !is.na(included$support_major_AS)))

# Calculate the expected probability for 'support' (p.major.support): 
# proportion of # 'support' judgments by MS times proportion of 
# 'support' judgments by AS
p.major.support <- (length(which(included$support_major_MS == 1))/
                      length(which(!is.na(included$support_major_MS)))) *
  (length(which(included$support_major_AS == 1))/
     length(which(!is.na(included$support_major_AS))))

# Calculate the expected probability for 'no support' (p.major.nosupport): 
# proportion of # 'no support' judgments by MS times proportion of 
# 'no support' judgments by AS
p.major.nosupport <- (length(which(included$support_major_MS == 0))/
                        length(which(!is.na(included$support_major_MS)))) *
  (length(which(included$support_major_AS == 0))/
     length(which(!is.na(included$support_major_AS))))

# Calculate the expected probability of 'major' agreement between MS & AS (p.e.major)
p.e.major <- p.major.support + p.major.nosupport

# Calculate Cohen's kappa for major agreement
kappa.support <- (p.o.major - p.e.major)/(1 - p.e.major)

# Cohen's kappa for major agreement (i.e. any support vs no support, 
# ignoring missing/not codeable and treating full and partial support 
# as the same) is kappa.support = 0.805
# Interpretation of magnitude: 'substantial agreement' according to 
# Landis & Koch (1977); 'excellent' according to Fleiss (1981)


##==============================================================================##
## 2. Calculate inter-rater agreement (between AS and DL) for 
## new replication status variable

# number of papers coded by AS: 223 out of 223 papers = 100%
rep.coded.AS <- length(which(!is.na(included$is_replication_new_AS))) 

# number of papers coded by DL: 131 out of 223 papers = 58.74%
rep.coded.DL <- length(which(!is.na(included$is_replication_new_DL)))

# number of RRs coded by DL: 32 out of 71 = 45.1%
rep.RR.coded.DL <- length(which(!is.na(included$is_replication_new_DL) & included$is_RR == 1))
rep.RR.coded.DL/sum(included$is_RR)

# number of CRs coded by DL: 99 out of 152 = 65.1%
rep.SR.coded.DL <- length(which(!is.na(included$is_replication_new_DL) & included$is_RR == 0))
rep.SR.coded.DL/sum(!included$is_RR)

# number of disagreements: 5 out of 131 = 3.82%
rep.disagreements <- length(which((included$is_replication_new_AS != 
                                     included$is_replication_new_DL) & 
                                    !is.na(included$is_replication_new_DL)))


# Calculate the observed agreement (p.o.replication): number of cases in  
# which AS & DL agree, divided by the number of double coded cases
p.o.replication <- length(which(included$is_replication_new_AS == 
                                  included$is_replication_new_DL))/
  length(which(!is.na(included$is_replication_new_AS) & 
                 !is.na(included$is_replication_new_DL)))

# Calculate the expected probability for 'is replication' (p.e.isrep): 
# proportion of # 'is replication' judgments by AS times the proportion 
# of 'is replication' judgments by DL
p.e.isrep <- (length(which(included$is_replication_new_AS == 1))/
                length(which(!is.na(included$is_replication_new_AS)))) *
  (length(which(included$is_replication_new_DL == 1))/
     length(which(!is.na(included$is_replication_new_DL))))

# Calculate the expected probability for 'not a repliction' (p.e.norep): 
# proportion of 'not a replication' judgments by AS times the proportion 
# of 'not a replication' judgments by DL
p.e.norep <- (length(which(included$is_replication_new_AS == 0))/
                length(which(!is.na(included$is_replication_new_AS)))) *
  (length(which(included$is_replication_new_DL == 0))/
     length(which(!is.na(included$is_replication_new_DL))))

# Calculate the expected probability of agreement between AS & DL (p.e.rep)
p.e.rep <- p.e.isrep + p.e.norep

# Calculate Cohen's kappa for agreement
kappa.rep <- (p.o.replication - p.e.rep)/(1 - p.e.rep)

# Cohen's kappa for agreement between AS & DL on whether a hypothesis
# is a direct replication of previous work is kappa.rep = 0.879
# Interpretation of magnitude: 'almost perfect agreement' according to 
# Landis & Koch (1977); 'excellent' according to Fleiss (1981)
