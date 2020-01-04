clear all
cls
set more off

/*
RR vs CR analysis
by Mitchell Schijen
for course 0BEPP0 at TU/e
*/

/** load the dataset **/
cd "C:\Users\mrmjs\OneDrive\Documenten\Technische Universiteit Eindhoven\Y4S1 - 0BEPP0 Bachelor End Project\Main Research\Analysis\Analysis scripts\Stata"
//use combined_dataset_interim
use combined_dataset_rechecked_2019_01_25

/** check the dataset **/
codebook,compact

/** recoding **/
/* all variables are stored as strings, some need to be recoded*/
// final version of [id]
destring id, replace

// final version of [has_support]
gen has_support = .
codebook support_combined
replace has_support = 1 if support_combined == "support"
replace has_support = 1 if support_combined == "partial support"
replace has_support = 0 if support_combined == "no support"

// final version of [is_rr]
gen is_rr = .
codebook is_RR
replace is_rr = 1 if is_RR == "1"
replace is_rr = 0 if is_RR == "0"
drop is_RR

// intermediate: first look at demographics of RRs and CRs
tab has_support if is_rr == 1
tab support_combined if is_rr == 1
tab has_support if is_rr == 0
tab support_combined if is_rr == 0

// final version of [full_text_available]
gen full_text_available = .
codebook full_text_available_as
codebook full_text_available_ms
replace full_text_available = 0 if full_text_available_as == "0"
replace full_text_available = 0 if full_text_available_ms == "0"
replace full_text_available = 1 if full_text_available_as == "1"
replace full_text_available = 1 if full_text_available_ms == "1"
tab full_text_available
tab full_text_available_comb_man

// intermediate: find out which coding was chosen for disagreeing cases:
* these cases use Anne's coding when there was disagreement.
gen uses_as_coding = .
replace uses_as_coding = 1 if support_combined == support_as
replace uses_as_coding = 0 if support_combined == support_ms
replace uses_as_coding = 2 if support_ms == support_as // both ms and as have the same coding

// final version of [hypothesis_from_full_text]
gen hypothesis_from_full_text = .
replace hypothesis_from_full_text = 1 if hypothesis_full_text_ms == "1"
replace hypothesis_from_full_text = 0 if hypothesis_full_text_ms == "0"
replace hypothesis_from_full_text = 1 if hypothesis_full_text_as == "1" & uses_as_coding == 1
replace hypothesis_from_full_text = 0 if hypothesis_full_text_as == "0" & uses_as_coding == 1
replace hypothesis_from_full_text = 0 if uses_as_coding == 2 & hypothesis_full_text_as == "0"
replace hypothesis_from_full_text = 0 if uses_as_coding == 2 & hypothesis_full_text_ms == "0" 

// final version of [finding_conclusion_full_text]
gen finding_conclusion_full_text = .
replace finding_conclusion_full_text = 1 if finding_conclusion_full_text_ms == "1"
replace finding_conclusion_full_text = 0 if finding_conclusion_full_text_ms == "0"
replace finding_conclusion_full_text = 1 if finding_conclusion_full_text_as == "1" & uses_as_coding == 1
replace finding_conclusion_full_text = 0 if finding_conclusion_full_text_as == "0" & uses_as_coding == 1
replace finding_conclusion_full_text = 0 if uses_as_coding == 2 & finding_conclusion_full_text_as == "0"
replace finding_conclusion_full_text = 0 if uses_as_coding == 2 & finding_conclusion_full_text_ms == "0"

// final version of [is_replication]
gen is_replication = .
codebook is_replication_combined
replace is_replication = 1 if is_replication_combined == "1"
replace is_replication = 0 if is_replication_combined == "0"

// final version of [is_original_work]
gen is_original_work = .
codebook is_original_work_combined
replace is_original_work = 1 if is_original_work_combined == "1"
replace is_original_work = 0 if is_original_work_combined == "0"

// final version of [include_in_analysis]
codebook include_in_analysis
destring include_in_analysis, replace
codebook include_in_analysis


/*
The final dataset will have the variables:
* id - ready
* is_rr - ready
* has_support - ready
* hypothesis_from_full_text - ready
* finding_conclusion_full_text - ready
* is_replication - ready
* is_original_work - ready
* include_in_analysis
*/

// check for coding errors
// 1. it should hold that [hypothesis_from_full_text = 0 AND finding_conclusion_full_text = 0] if [full_text_available] = 0
tab id if (hypothesis_from_full_text == 1 | finding_conclusion_full_text == 1) & full_text_available == 0

// 2. papers can be [is_replication] or [is_original_work] or both, but not neither
tab id if (is_replication == 0 & is_original_work == 0)


/** Create new variables from existing variables for analysis **/
// account for replications and extensions - papers that are both a replication and original work
gen replication_and_extension = .
replace replication_and_extension = 1 if is_replication == 1 & is_original_work == 1
replace replication_and_extension = 0 if is_replication == 0 | is_original_work == 0
tab id if replication_and_extension == 1

/** Descriptive statistics **/

// Differences in coding between AS and MS
// in RRs
list include_in_analysis support_as support_ms if support_as != support_ms & is_rr == 1 & support_as != ""

// in CRs
list include_in_analysis support_as support_ms if support_as != support_ms & is_rr == 0 & support_as != ""


/*
* what percentage in a group has support?
* what percentage in a group is a replication?
* what percentage in a group is original work?
* what percentage in a group is a replication and extension?
* in what percentage in a group did we find the hypothesis in the full text and not in the abstract?
* in what percentage in a group did we find the finding or conclusion in the full text and not in the abstract?
*/

//'demographic' stats of RRs
tab include_in_analysis          if is_rr == 1
tab has_support                  if is_rr == 1 & include_in_analysis == 1
tab is_replication               if is_rr == 1 & include_in_analysis == 1
tab is_original_work             if is_rr == 1 & include_in_analysis == 1
tab replication_and_extension    if is_rr == 1 & include_in_analysis == 1
tab hypothesis_from_full_text    if is_rr == 1 & include_in_analysis == 1
tab finding_conclusion_full_text if is_rr == 1 & include_in_analysis == 1

//'demographic' stats of CRs
tab include_in_analysis          if is_rr == 0
tab has_support                  if is_rr == 0 & include_in_analysis == 1
tab is_replication               if is_rr == 0 & include_in_analysis == 1
tab is_original_work             if is_rr == 0 & include_in_analysis == 1
tab replication_and_extension    if is_rr == 0 & include_in_analysis == 1
tab hypothesis_from_full_text    if is_rr == 0 & include_in_analysis == 1
tab finding_conclusion_full_text if is_rr == 0 & include_in_analysis == 1


/** Preregistered - Perform proportion tests **/
tab has_support is_rr if include_in_analysis == 1

/*
if all cell counts are > 5, use chi2 results, else use exact results.
for both tests holds:
H0: there is no statistically significant difference in [has_support] between [is_rr]
*/
// Chi2 results:
tab has_support is_rr  if include_in_analysis == 1, chi2

// Fisher's Exact results:
tab has_support is_rr if include_in_analysis == 1, exact

/*
The chi2 and exact only answer if the dependent variable is different or not.
the H1 states that positive result rate is lower in rr than cr:
* if the chi2 or exact (when appropriate) has p < alpha AND
* the mean of rr is lower than the mean of cr (using previous sum commands
* then H1 is supported
*/

/** Preregistered - Perform equivalence test (TOST) **/
// worst case scenario: use stata to grab all the variables for the TOST command in R
// or use "tost" library in Stata

// GENERAL
// TOSTER command: TOSTtwo.prop
// group1 = RR
// group2 = CR

// GROUP 1
sum has_support                  if is_rr == 1 & include_in_analysis == 1
// proportion group 1 [prop1] = .4264706
// sample size group 1 [n1] = 68
// if the difference in means is in the desired direction RR < CR, p = 0.5*p

// GROUP 2
sum has_support                  if is_rr == 0 & include_in_analysis == 1
// proportion group 2 [prop2] = .9577465
// sample size group 2 [n2] = 142

// EQUIVALENCE BOUNDS
/* in Fanelli:
   * positive result rate psychology = 91.5%
   * positive result rate general social sciences = 85.5% (lowest prr in soft sciences)
	* absolute difference  = 6
*/
di abs(91.5 - 85.5)
x
/*
Run in R
library(TOSTER)
TOSTtwo.prop(prop1=.4264706, prop2=.9577465,n1=68,n2=142,low_eqbound=-0.06,high_eqbound=0.06,alpha=0.05)
*/

/** Exploratory Analyses **/
// Preregistered - INTRODUCTION OF HYPOTHESES
/* 
WoS uses only the abstract to find papers, meaning that hypotheses introduced
in the full text cannot be found by search engines that can only use the abstract
(and not the full text).
*/ 

// descriptive - how many did we code
tab hypothesis_introduction_ms if is_rr == 1 & include_in_analysis == 1 & hypothesis_introduction_ms != ""
tab hypothesis_introduction_as if is_rr == 1 & include_in_analysis == 1 & hypothesis_introduction_as != ""

// In RRs
// in abstract - could be found by search engines
list hypothesis_introduction_ms if is_rr == 1 & include_in_analysis == 1 & hypothesis_full_text_ms == "0"
list hypothesis_introduction_as if is_rr == 1 & include_in_analysis == 1 & hypothesis_full_text_as == "0" & hypothesis_introduction_as != ""

// in full text - cannot be found by search engines
list hypothesis_introduction_ms if is_rr == 1 & include_in_analysis == 1 & hypothesis_full_text_ms == "1"
list hypothesis_introduction_as if is_rr == 1 & include_in_analysis == 1 & hypothesis_full_text_as == "1" & hypothesis_introduction_as != ""

// extra - are sentences unique to replications?
// in abstract
list hypothesis_introduction_ms if is_rr == 1 & include_in_analysis == 1 & hypothesis_full_text_ms == "0" & is_replication == 1
list hypothesis_introduction_as if is_rr == 1 & include_in_analysis == 1 & hypothesis_full_text_as == "0" & hypothesis_introduction_as != "" & is_replication == 1

// in full text
list hypothesis_introduction_ms if is_rr == 1 & include_in_analysis == 1 & hypothesis_full_text_ms == "1" & is_replication == 1
list hypothesis_introduction_as if is_rr == 1 & include_in_analysis == 1 & hypothesis_full_text_as == "1" & hypothesis_introduction_as != "" & is_replication == 1



// Not Preregistered - difference in prr in RRs: replication vs original work
// look out for replications and extensions!
gen prr_exp_rr_rep_vs_orig = .
replace prr_exp_rr_rep_vs_orig = 1 if is_replication == 1 & is_original_work == 0 & is_rr == 1 & replication_and_extension == 0
tab replication_and_extension if is_rr == 1
replace prr_exp_rr_rep_vs_orig = 2 if is_rr == 1 & replication_and_extension == 1
replace prr_exp_rr_rep_vs_orig = 3 if is_replication == 0 & is_original_work == 1 & is_rr == 1 & replication_and_extension == 0
/*
codebook prr_exp_rr_rep_vs_orig
* 1 = replications
* 2 = replication_and_extensions
* 3 = original works
*/

// proportion test:
sum has_support if prr_exp_rr_rep_vs_orig == 1 & is_rr == 1 & include_in_analysis == 1
sum has_support if prr_exp_rr_rep_vs_orig == 2 & is_rr == 1 & include_in_analysis == 1
sum has_support if prr_exp_rr_rep_vs_orig == 3 & is_rr == 1 & include_in_analysis == 1
tab has_support prr_exp_rr_rep_vs_orig if is_rr == 1 & include_in_analysis == 1, chi2
tab has_support prr_exp_rr_rep_vs_orig if is_rr == 1 & include_in_analysis == 1, exact

// with two categories
gen fully_original = .
replace fully_original = 1 if is_original_work == 1 & is_replication == 0
replace fully_original = 0 if is_replication == 1

sum has_support if fully_original == 1 & is_rr == 1 & include_in_analysis == 1
sum has_support if fully_original == 0 & is_rr == 1 & include_in_analysis == 1

tab has_support fully_original if is_rr == 1 & include_in_analysis == 1, chi2
tab has_support fully_original if is_rr == 1 & include_in_analysis == 1, exact

x
