# clear R workspace
rm(list = ls())
# clear the console
shell("cls")

# install required packages if they are not already installed:
if(!require(TOSTER)){
  install.packages('TOSTER')
}
if(!require(readxl)){
  install.packages('readxl')
}
# the gmodels package is currently not used
if (!require(gmodels)) {
  install.packages('gmodels')
}

# RR vs CR analysis

# load libraries
library(readxl)
library(gmodels)
library(TOSTER)

# load the dataset from excel
table <- read_excel("D:/Git Software Projects/positive_result_rates/Data/dataset_analysis_double_coding - 25_01_2019.xlsx")


# ** RECODING **
# some variables are stored as string, some need to be recoded
attach(table)
class(id)
class(include_in_analysis)
class(is_RR)
class(hypothesis_full_text_as)
class(hypothesis_full_text_ms) # character
class(finding_conclusion_full_text_as)
class(finding_conclusion_full_text_ms) # character
class(full_text_available_as)
class(full_text_available_ms)
class(is_original_work_combined)
class(is_original_work_as)
class(is_original_work_ms) # character
class(is_replication_combined)
class(is_replication_as)
class(is_replication_ms) # character
detach(table)


# final version of [has_support]
# add has support variable
table$has_support <- NA
table$has_support[table$support_combined == "support"] <- 1
table$has_support[table$support_combined == "partial support"] <- 1
table$has_support[table$support_combined == "no support"] <- 0
table(table$has_support,table$support_combined)

# final version of [is_rr]
# note: unlike the Stata script, type conversion is not needed.
table$is_rr <- table$is_RR
table(table$is_RR, table$is_rr)

# intermediate: first look at demographics of RRs and CRs
table(table$has_support,table$is_rr)

# final version of [full_text_available]
table$full_text_available <- NA
table$full_text_available[table$full_text_available_as == 0] <- 0
table$full_text_available[table$full_text_available_ms == 0] <- 0
table$full_text_available[table$full_text_available_as == 1] <- 1
table$full_text_available[table$full_text_available_ms == 1] <- 1
# [full_text_available_comb_man] is a manual attempt of the code above
table(table$full_text_available,table$full_text_available_comb_man)

# intermediate: find out which coding was chosen for disagreeing cases:
# these cases use Anne's coding when there was disagreement.
table$uses_as_coding <- NA
table$uses_as_coding[table$support_combined == table$support_as] = 1
table$uses_as_coding[table$support_combined == table$support_ms] = 0
table$uses_as_coding[table$support_ms == table$support_as] = 2
table(table$uses_as_coding)
# codebook:
# * 1: entry coded using AS coding
# * 2: entry coded using MS coding
# * 3: both AS and MS have the same coding

# final version of [hypothesis_from_full_text]
# note: [hypothesis_full_text_ms] is a character variable,
# [hypothesis_full_text_as] is a numeric variable
table$hypothesis_from_full_text <- NA
table$hypothesis_from_full_text[table$hypothesis_full_text_ms == "1"] <- 1
table$hypothesis_from_full_text[table$hypothesis_full_text_ms == "0"] <- 0
table$hypothesis_from_full_text[table$hypothesis_full_text_as == 1 & table$uses_as_coding == 1] <- 1
table$hypothesis_from_full_text[table$hypothesis_full_text_as == 0 & table$uses_as_coding == 1] <- 0
table$hypothesis_from_full_text[table$uses_as_coding == 2 & table$hypothesis_full_text_as == 0] <- 0
table$hypothesis_from_full_text[table$uses_as_coding == 2 & table$hypothesis_full_text_ms == "0"] <- 0
table(table$hypothesis_from_full_text, table$uses_as_coding)

# final version of [finding_conclusion_full_text]
# note: [finding_conclusion_ms] is a character variable,
# [finding_conclusion_as] is a numeric variable.
table$finding_conclusion_full_text <- NA
table$finding_conclusion_full_text[table$finding_conclusion_full_text_ms == "1"] <-1
table$finding_conclusion_full_text[table$finding_conclusion_full_text_ms == "0"] <-0
table$finding_conclusion_full_text[table$finding_conclusion_full_text_as == 1 & table$uses_as_coding == 1] <-1
table$finding_conclusion_full_text[table$finding_conclusion_full_text_as == 0 & table$uses_as_coding == 1] <-0
table$finding_conclusion_full_text[table$uses_as_coding == 2 & table$finding_conclusion_full_text_as == 0] <-0
table$finding_conclusion_full_text[table$uses_as_coding == 2 & table$finding_conclusion_full_text_ms == "0"] <-0

# final version of [is_replication]
# note: [is_replication_combined] already is a numeric variable
table$is_replication <- table$is_replication_combined
table(table$is_replication, table$is_replication_combined)

# final version of [is_original_work]
# note: [is_original_work_combined] already is a numeric variable
table$is_original_work <- table$is_original_work_combined
table(table$is_original_work, table$is_original_work_combined)

# final version of [include_in_analysis]
# [include_in_analysis] already is a numeric variable,
# unlike the Stata script no changes (i.e. type conversion) have to be made.

# the final dataset will have the variables:
# * id
# * is_rr
# * has_support
# * hypothesis_from_full_text
# * finding_conclusion_full_text
# * is_replication
# * is_original_work
# * include_in_analysis


# ** CHECK FOR CODING ERRORS **
# 1. it should hold that [hypothesis_from_full_text = 0 AND 
#    finding_conclusion_full_text = 0] if [full_text_available] = 0
table$coding_error_check1 <- FALSE
table$coding_error_check1[(table$hypothesis_from_full_text == 1 | table$finding_conclusion_full_text == 1) & table$full_text_available == 0] <- TRUE
table(table$coding_error_check1)

# 2. papers can be [is_replication] or [is_original_work] or both, 
#    but not neither.
table$coding_error_check2 <- FALSE
table$coding_error_check2[table$is_replication == 0 & table$is_original_work == 0] <- TRUE
table(table$coding_error_check2)


# ** CREATING NEW VARIABLES FROM EXISTING VARIABLES FOR ANALYSIS **
# account for replications and extensions - papers that are both a replication and original work (loose definition)
table$replication_and_extension <- NA
table$replication_and_extension[table$is_replication == 1 & table$is_original_work == 1] <- 1
table$replication_and_extension[table$is_replication == 0 | table$is_original_work == 0] <- 0
table(table$replication_and_extension)


# ** DESCRIPTIVE STATISTICS **
#  * what percentage in a group has support?
#  * what percentage in a group is a replication?
#  * what percentage in a group is original work?
#  * what percentage in a group is a replication and extension?
#  * in what percentage in a group did we find the hypothesis in the full text and not in the abstract?
#  * in what percentage in a group did we find the finding or conclusion in the full text and not in the abstract?

# Demographics statistics of RRs
attach(table)
# RR - include in analysis
table(include_in_analysis[is_rr == 1])

# RR - has support
table(has_support[is_rr == 1 & include_in_analysis == 1])
mean (has_support[is_rr == 1 & include_in_analysis == 1])

# RR - is replication
table(is_replication[is_rr == 1 & include_in_analysis == 1])
mean (is_replication[is_rr == 1 & include_in_analysis == 1])

# RR - is original work
table(is_original_work[is_rr == 1 & include_in_analysis == 1])
mean (is_original_work[is_rr == 1 & include_in_analysis == 1])

# RR - is replication and extension
table(replication_and_extension[is_rr == 1 & include_in_analysis == 1])
mean (replication_and_extension[is_rr == 1 & include_in_analysis == 1])

# RR - hypothesis from full text
table(hypothesis_from_full_text[is_rr == 1 & include_in_analysis == 1])
mean (hypothesis_from_full_text[is_rr == 1 & include_in_analysis == 1])

# RR - finding/conclusion from full text
table(finding_conclusion_full_text[is_rr == 1 & include_in_analysis == 1])
mean (finding_conclusion_full_text[is_rr == 1 & include_in_analysis == 1])


# Demographics statistics of CRs
# CR - include in analysis
table(include_in_analysis[is_rr == 0])

# CR - has support
table(has_support[is_rr == 0 & include_in_analysis == 1])
mean (has_support[is_rr == 0 & include_in_analysis == 1])

# CR - is replication
table(is_replication[is_rr == 0 & include_in_analysis == 1])
mean (is_replication[is_rr == 0 & include_in_analysis == 1])

# CR - is original work
table(is_original_work[is_rr == 0 & include_in_analysis == 1])
mean (is_original_work[is_rr == 0 & include_in_analysis == 1])

# CR - is replication and extension
table(replication_and_extension[is_rr == 0 & include_in_analysis == 1])
mean (replication_and_extension[is_rr == 0 & include_in_analysis == 1])

# CR - hypothesis from full text
table(hypothesis_from_full_text[is_rr == 0 & include_in_analysis == 1])
mean (hypothesis_from_full_text[is_rr == 0 & include_in_analysis == 1])

# CR - finding/conclusion from full text
table(finding_conclusion_full_text[is_rr == 0 & include_in_analysis == 1])
mean (finding_conclusion_full_text[is_rr == 0 & include_in_analysis == 1])
detach(table)


# ** PREREGISTERED - PERFORM PROPORTION TESTS **
# if all cell counts are > 5, use chi2 results, else use exact results.
# for both tests holds:
# H0: there is no statistically significant difference in [has_support] between [is_rr]
attach(table)
proportion_test <- table(has_support[include_in_analysis == 1], is_rr[include_in_analysis == 1])
proportion_test
chisq.test(proportion_test)
# This Chi2 value does match the result in Stata.
chisq.test(proportion_test, correct = FALSE)
# The Chi2 test in Stata does not appear to make use of Yates' continuity correction.
# When this correction is turned off the R result matches the Stata result.
fisher.test(proportion_test)
detach(table)
# The chi2 and exact only answer if the dependent variable is different or not.
# the H1 states that positive result rate is lower in rr than cr:
# if the chi2 or exact (when appropriate) has p < alpha AND
# the mean of rr is lower than the mean of cr (using previous sum commands)
# then H1 is supported


# ** PREREGISTERED - PERFORM EQUIVALENCE TEST (TOST) **
attach(table)
# group 1 = RR
prop_rr <- mean(has_support[is_rr == 1 & include_in_analysis == 1])
n_rr  <- length(has_support[is_rr == 1 & include_in_analysis == 1])

# group 2 = CR
prop_cr <- mean(has_support[is_rr == 0 & include_in_analysis == 1])
n_cr  <- length(has_support[is_rr == 0 & include_in_analysis == 1])
detach(table)

# equivalence bounds
# in Fanelli: 
# * positive result rate psychology = 91.5%
# * positive result rate general social sciences = 85.5% 
#   (lowest prr in soft sciences)
# 
# absolute difference  = 6
high_eq_bound <- abs(91.5 - 85.5)/100
low_eq_bound <- -1 * high_eq_bound

# perform TOST procedure:
TOSTtwo.prop(prop1=prop_rr, prop2=prop_cr, n1=n_rr, n2=n_cr, low_eqbound=low_eq_bound, high_eqbound=high_eq_bound,alpha=0.05)

# *** Exploratory Analyses ***
# ** PREREGISTERED - INTRODUCTION OF HYPOTHESES **
#  WoS uses only the abstract to find papers, meaning that hypotheses 
# introduced in the full text cannot be found by search engines that 
# can only use the abstract (and not the full text).
# note: this analyses only contains the RRs
attach(table)
# descriptive - how many did we code (abstract + full text)
all_hyp_introductions_ms <- table(hypothesis_introduction_ms[is_rr == 1 & include_in_analysis == 1 & hypothesis_introduction_ms != "" & !is.na(hypothesis_introduction_ms) & hypothesis_introduction_ms != "missing"])
all_hyp_introductions_as <- table(hypothesis_introduction_as[is_rr == 1 & include_in_analysis == 1 & hypothesis_introduction_as != "" & !is.na(hypothesis_introduction_as)])
# Amount of hypothesis introductions in RRs coded by MS
summary(all_hyp_introductions_ms)
# Amount of hypothesis introductions in RRs coded by AS
summary(all_hyp_introductions_as)

# in abstract - can be found by search engines
abstract_hyp_introductions_ms <- list(hypothesis_introduction_ms[hypothesis_full_text_ms == "0" & is_rr == 1 & include_in_analysis == 1 & hypothesis_introduction_ms != "" & !is.na(hypothesis_introduction_ms) & hypothesis_introduction_ms != "missing"])
abstract_hyp_introductions_as <- list(hypothesis_introduction_as[hypothesis_full_text_as ==  0  & is_rr == 1 & include_in_analysis == 1 & hypothesis_introduction_as != "" & !is.na(hypothesis_introduction_as)])
# print results
# Hypothesis introductions in RRs that occur in the abstract (coded by MS)
abstract_hyp_introductions_ms
# Hypothesis introductions in RRs that occur in the abstract (coded by AS)
abstract_hyp_introductions_as

# in full text - cannot be found by search engines
full_text_hyp_introductions_ms <- list(hypothesis_introduction_ms[hypothesis_full_text_ms == "1" & is_rr == 1 & include_in_analysis == 1 & hypothesis_introduction_ms != "" & !is.na(hypothesis_introduction_ms) & hypothesis_introduction_ms != "missing"])
full_text_hyp_introductions_as <- list(hypothesis_introduction_as[hypothesis_full_text_as ==  1  & is_rr == 1 & include_in_analysis == 1 & hypothesis_introduction_as != "" & !is.na(hypothesis_introduction_as)])
# print results
# Hypothesis introductions in RRs that occur in the full text (coded by MS)
full_text_hyp_introductions_ms
# Hypothesis introductions in RRs that occur in the full text (coded by AS)
full_text_hyp_introductions_as

# extra - are sentences unique to replications?
# in abstract:
abstract_replication_introductions_ms <- list(hypothesis_introduction_ms[is_rr == 1 & include_in_analysis == 1 & hypothesis_full_text_ms == "0" & is_replication == 1 & hypothesis_introduction_ms != "" & !is.na(hypothesis_introduction_ms) & hypothesis_introduction_ms != "missing"])
abstract_replication_introductions_as <- list(hypothesis_introduction_as[is_rr == 1 & include_in_analysis == 1 & hypothesis_full_text_as ==  0  & is_replication == 1 & hypothesis_introduction_as != "" & !is.na(hypothesis_introduction_as)])
# print results
# Hypothesis introductions in "replication" RRs that occur in the abstract (coded by MS)
abstract_replication_introductions_ms
# Hypothesis introductions in "replication" RRs that occur in the abstract (coded by AS)
abstract_replication_introductions_as

# in full text
full_text_replication_introductions_ms <- list(hypothesis_introduction_ms[is_rr == 1 & include_in_analysis == 1 & hypothesis_full_text_ms == "1" & is_replication == 1 & hypothesis_introduction_ms != "" & !is.na(hypothesis_introduction_ms) & hypothesis_introduction_ms != "missing"])
full_text_replication_introductions_as <- list(hypothesis_introduction_as[is_rr == 1 & include_in_analysis == 1 & hypothesis_full_text_as ==  1  & is_replication == 1 & hypothesis_introduction_as != "" & !is.na(hypothesis_introduction_as)])
# print results
# Hypothesis introductions in "replication" RRs that occur in the full text (coded by MS)
full_text_replication_introductions_ms
# Hypothesis introductions in "replication" RRs that occur in the full text (coded by AS)
full_text_replication_introductions_as
detach(table)

# ** NOT PREREGISTERED **
# Difference in prr in RRs: replication vs original work
# look out for replications and extensions!
table$rr_type3 <- NA
# Originally this variable was called prr_exp_rr_rep_vs_orig
table$rr_type3[table$is_rr == 1 & table$is_replication == 1 & table$is_original_work == 0 & table$replication_and_extension == 0] <- 1
table$rr_type3[table$is_rr == 1 & table$replication_and_extension == 1] <- 2
table$rr_type3[table$is_rr == 1 & table$is_replication == 0 & table$is_original_work == 1 & table$replication_and_extension == 0] <- 3
# codebook rr_type3
# 1 = replications
# 2 = replication_and_extensions
# 3 = original works

attach(table)
# Proportion tests with three categories
proportion_test_type3 <- table(has_support[is_rr == 1 & include_in_analysis == 1], rr_type3[is_rr == 1 & include_in_analysis == 1])
proportion_test_type3
chisq.test(proportion_test_type3)
chisq.test(proportion_test_type3, correct = FALSE)
fisher.test(proportion_test_type3)
detach(table)

# Proportion test with two categories
table$rr_fully_original <- NA
# Originally this variable was called fully_original
# originally [is_rr] and [include_in_analysis] variables were only used when creating the tables in Stata
# In this version of the analysis script these variables are also used to determine the value of [rr_fully_original]
table$rr_fully_original[table$is_rr == 1 & table$is_original_work == 1 & table$is_replication == 0 & table$include_in_analysis == 1] <- 1
table$rr_fully_original[table$is_rr == 1 & table$is_replication == 1 & table$include_in_analysis == 1] <- 0

# descriptive stats:
attach(table)
summary(rr_fully_original)

proportion_test_fully_original <- table(has_support[is_rr == 1 & include_in_analysis == 1], rr_fully_original[is_rr == 1 & include_in_analysis == 1])
proportion_test_fully_original
chisq.test(proportion_test_fully_original)
chisq.test(proportion_test_fully_original, correct = FALSE)
fisher.test(proportion_test_fully_original)
detach(table)

# Possible improvements:
# For this script:
# * add value labels for categorical variables (e.g. [has support])
# * better formatting of tables
#
# In general:
# * use numeric missing values instead of the string "missing", 
#   as numeric variables that have such a missing value are considered to be 'character'variables by R.