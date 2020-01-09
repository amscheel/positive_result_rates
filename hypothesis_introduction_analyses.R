#########################################################
#####                                               #####
##### Script 3: Qualitative analysis:               #####
#####           Analysing the phrases used to       #####
#####           introduce hypotheses in Registered  #####
#####           Reports                             #####
#####                                               #####
#####-----------------------------------------------#####
#####                                               #####
##### Content:                                      #####
#####  * 1: Setup                                   #####
#####  * 2: Calculate how many Registered Reports   #####
#####       would have been detected with the       #####
#####       search phrase "test* the hypothes*"     #####
#####  * 3: Analyse the hypothesis introduction     #####
#####       phrases used in Registered Reports      #####
#####       and find clusters (identify the five    #####
#####       most frequent "core words")             #####
#####  * 4: Create two tables listing all unique    #####
#####       hypothesis introduction phrases in      #####
#####       original and replication Registered     #####
#####       Reports, respectively                   #####
#####       !!CAUTION!! Extremely long and ugly     #####
#####       code (ca. 750 lines)                    #####
#####                                               #####
##### Note:                                         #####
#####   RR = Registered Report                      #####
#####                                               #####
#########################################################

##==============================================================================##
## 1. Setup: load packages and data 
##==============================================================================##

# Two packages are needed: stringr reshape2. Install them, if needed, by 
# uncommenting the respective line(s):
# install.packages("stringr")  # install stringr
# install.packages("reshape2")  # install reshape2

# Load packages:
library("stringr")
library("reshape2")

# load the dataset from the RDS file created with codebook
alldata <- readRDS("positive_results_in_registered_reports_codebook.rds")
# remove excluded papers
included <- alldata[alldata$include_in_analysis==1,]


##==============================================================================##
## 2. Calculate how many RRs would have been discovered with the search
##    phrase "test* the hypothes*" (i.e., when searching titles, abstracts,
##    and keywords)
##==============================================================================##

# Create a new variable that indicates if the *title* contains 
# the phrase "test* the hypothes*" (TRUE vs FALSE)
included$searchphrase.in.title <- sapply(included$title, grepl, 
                                         pattern = "test\\w* the hypothes")

# Create a new variable that indicates if the *abstract* contains 
# the phrase "test* the hypothes*" (TRUE vs FALSE)
included$searchphrase.in.abstract <- sapply(included$abstract, grepl, 
                                            pattern = "test\\w* the hypothes")

# Create a new variable that indicates if the *keywords* contain 
# the phrase "test* the hypothes*" (TRUE vs FALSE)
included$searchphrase.in.keywords <- sapply(included$keywords, grepl, 
                                            pattern = "test\\w* the hypothes")

# Calculate the number of RRs that contain the search phrase in their abstract,
# title, or keywords: 2 RRs
testthehypothes.RR <- length(which(included$is_RR == 1 & 
                                     (included$searchphrase.in.abstract == TRUE |
                                      included$searchphrase.in.keywords == TRUE |
                                      included$searchphrase.in.title == TRUE)))


##==============================================================================##
## 3. Analyse the hypothesis introduction phrases used in RRs
##    and identify clusters
##==============================================================================##

# Create a new dataframe containing only RRs and relevant variables
hypintros <- included[which(included$is_RR==1), c("id",
                                                  "support_binary",
                                                  "is_replication",
                                                  "hyp_from_fulltext",
                                                  "RR_hyp_intro_abstract", 
                                                  "RR_hyp_intro_fulltext")]
hypintros <- as.data.frame(hypintros)


## 3.1 Count the number of phrases per paper and break multiple phrases
##     up into separate strings/variables

# Count the number of phrases that were coded for each paper:
# First, create a variable counting the number of phrases coded from the abstract
hypintros$abstract.count <- ifelse(is.na(str_count(
                                hypintros$RR_hyp_intro_abstract, ";")), 0, 
                                str_count(hypintros$RR_hyp_intro_abstract, ";") + 1)

# Second, create a variable counting the number of phrases coded from the full text
hypintros$fulltext.count <- ifelse(is.na(str_count(
                                hypintros$RR_hyp_intro_fulltext, ";")), 0, 
                                str_count(hypintros$RR_hyp_intro_fulltext, ";") + 1)

# If a paper has more than one phrase, they are currently all coded as one 
# string (phrases are separated with ";" but part of the same variable). 
# Now we'll break the phrases apart and put every phrase into a separate 
# variable. The maximum number of phrases coded from the abstract is 2, so
# we will create two variables for phrases from the abstract. The maximum
# number of phrases coded from the full text is 3, so we will create three
# variables for phrases from the full text. If a paper has less than 2 phrases
# from the abstract and/or less than 3 phrases from the full text, the remaining
# variables will become NA.
hypintros$introabstract.1 <- sapply(strsplit(hypintros$RR_hyp_intro_abstract, "; "), "[", 1)
hypintros$introabstract.2 <- sapply(strsplit(hypintros$RR_hyp_intro_abstract, "; "), "[", 2)
hypintros$introfulltext.1 <- sapply(strsplit(hypintros$RR_hyp_intro_fulltext, "; "), "[", 1)
hypintros$introfulltext.2 <- sapply(strsplit(hypintros$RR_hyp_intro_fulltext, "; "), "[", 2)
hypintros$introfulltext.3 <- sapply(strsplit(hypintros$RR_hyp_intro_fulltext, "; "), "[", 3)

# Determine how many RRs only had hypothesis intros coded from the full text
# and none from the abstract
n.hypintro.fulltext.only <- length(which(hypintros$abstract.count == 0 
                                         & hypintros$fulltext.count>0))

##------------------------------------------------------------------------------##
## 3.2 Analyse the contents of all used phrases and identify the most 
##     frequent words (/word stems)

# Create a new dataframe with all hypothesis introduction phrases in long format
intros.long <- melt(hypintros, 
                    id.vars = c("id", "is_replication", "hyp_from_fulltext", 
                                "abstract.count", "fulltext.count"), 
                    measure.vars = c("introabstract.1", "introabstract.2", 
                                     "introfulltext.1", "introfulltext.2", 
                                     "introfulltext.3"),
                    variable.name = "origin", value.name = "phrase", na.rm = TRUE)

# Simplify the "origin" variable ("abstract" vs "full text")
intros.long$origin <- ifelse(sapply(intros.long$origin, grepl, 
                                    pattern = "abstract"), "abstract", "fulltext")

# Clean up: some phrases had "\n" at the end, remove that
intros.long$phrase <- sapply(strsplit(intros.long$phrase, "\n"), "[", 1)

# Find all unique *words* across all hypothesis intro phrases:
# First, put all unique *phrases* into a vector
uniquephrases <- unique(intros.long$phrase)

# Next, break up all unique phrases into words (based on the spaces between words,
# meaning that non-words like "..." may be treated as words too) and put all 
# unique *words* into a vector:
uniquewords <- unique(unlist(sapply(strsplit(uniquephrases, " "), unique)))

# Sort the words in the vector alphabetically to make manual inspection easier
uniquewords <- uniquewords[order(uniquewords)]

# The vector uniquewords contains 65 items. After manual inspection, we
# reduced this number to 36 unique word *stems*: E.g., "aim" is the word stem
# for the two items "aim" and "aimed". Similarly, "hypothes" is the word stem 
# for the items "(Hypothesis", "Hypotheses", "hypotheses", "hypothesis", 
# "Hypothesis:", "hypothesize", "hypothesized".

# List of word stems identified from uniquewords:
wordstems <- c("H1", "hypothes", "accord", "aim", 
               "and", "assum", "at", "attempt",
               "conduct", "critic", "establish", "examin",
               "expect", "experiment", "had", "if",
               "investigat", "of", "perform", "predicat",
               "predict", "present", "regist", "replicat",
               "report", "reproduc", "sought", "suggest",
               "test", "that", "the", "then", 
               "to", "was", "we", "whether")

# The vector wordstems contains words that are not sufficiently meaningful
# by themselves and would not be helpful in a literature search, such as
# "to", "we", "and". In a second manual inspection, we excluded XXX such
# words: "and", "at", "had", "if", "of", "sought", "that", "the", "then", 
# "to", "was", "we" and "whether" were dropped, leaving the following list 
# of 24 "cleaned" word stems. To avoid confusion, we will call these 
# word stems "core words:
corewords <- c("accord", "aim", "assum", "attempt", 
               "conduct", "critic", "establish", "examin", 
               "expect", "experiment", "H1", "hypothes", 
               "investigat", "perform", "predicat", "predict", 
               "present", "regist", "replicat", "report", 
               "reproduc", "suggest", "test")

# Now we count how often each core word occurs across *all* hypothesis 
#introduction phrases:
# First, create a data frame with all core words
wordoccurrences.df <- data.frame(coreword = corewords, count = NA)

# Then calculate how often each core word occurs in all hypothesis
# introduction phrases and put the number into the dataframe
for (i in 1:length(corewords)) {
  wordoccurrences.df$count[i] <- sum(sapply(grepl(corewords[i], 
                                                  intros.long$phrase, 
                                                  ignore.case = TRUE), sum))
}

# Finally, sort the dataframe by core-word frequency
wordoccurrences.df <- wordoccurrences.df[order(-wordoccurrences.df$count),]

# The five most frequent core words are "hypothes" (34 occurrences), 
# "replicat" (24), "test" (20), "examin" (8), and "predict" (8).


##------------------------------------------------------------------------------##
## 3.3 Analysing the performance of a weaker version of Fanelli's search
##     term "test* the hypothes*":
##     Given that "hypothes" and "test" seem to be very common amont the 
##     hypothesis introduction phrases, maybe Fanelli wasn't so far off 
##     after all, but his full search string was too strict? 

# Calculate how often "test*" and "hypothes*" co-occur: Only 8 times!
test.and.hypothes <- sum(grepl("test", intros.long$phrase) & 
                           grepl("hypothes", intros.long$phrase, 
                                 ignore.case = TRUE))

# Calculate how many hyp intros contain either "test*" OR "hypothes*": 46
test.or.hypothes <- sum(grepl("test", intros.long$phrase) | 
                          grepl("hypothes", intros.long$phrase,
                                ignore.case = TRUE))


##------------------------------------------------------------------------------##
## 3.4 Determine how many RRs would have been found by using the five most
##     frequent core words (hypothes, replicat, test, examin, predict) as 
##     search terms and searching paper titles, abstracts, and keywords

# Create new variables that indicate if title, abstract, or keywords contain "hypothes"  
included$hypothes.in.title <- sapply(included$title, grepl, 
                                     pattern = "hypothes", ignore.case = TRUE)
included$hypothes.in.keywords <- sapply(included$keywords, grepl, 
                                        pattern = "hypothes", ignore.case = TRUE)
included$hypothes.in.abstract <- sapply(included$abstract, grepl, 
                                        pattern = "hypothes", ignore.case = TRUE)
included$hypothes.occurs <- ifelse(included$hypothes.in.title == TRUE |
                                     included$hypothes.in.keywords == TRUE |
                                     included$hypothes.in.abstract == TRUE, 1, 0)

# Create new variables that indicate if title, abstract, or keywords contain "test"  
included$test.in.title <- sapply(included$title, grepl, 
                                 pattern = "test", ignore.case = TRUE)
included$test.in.keywords <- sapply(included$keywords, grepl, 
                                    pattern = "test", ignore.case = TRUE)
included$test.in.abstract <- sapply(included$abstract, grepl, 
                                    pattern = "test", ignore.case = TRUE)
included$test.occurs <- ifelse(included$test.in.title == TRUE |
                                 included$test.in.keywords == TRUE |
                                 included$test.in.abstract == TRUE, 1, 0)

# Create new variables that indicate if title, abstract, or keywords contain "replicat"  
included$replicat.in.title <- sapply(included$title, grepl, 
                                     pattern = "replicat", ignore.case = TRUE)
included$replicat.in.keywords <- sapply(included$keywords, grepl, 
                                        pattern = "replicat", ignore.case = TRUE)
included$replicat.in.abstract <- sapply(included$abstract, grepl, 
                                        pattern = "replicat", ignore.case = TRUE)
included$replicat.occurs <- ifelse(included$replicat.in.title == TRUE |
                                     included$replicat.in.keywords == TRUE |
                                     included$replicat.in.abstract == TRUE, 1, 0)

# Create new variables that indicate if title, abstract, or keywords contain "examin"  
included$examin.in.title <- sapply(included$title, grepl, 
                                    pattern = "examin", ignore.case = TRUE)
included$examin.in.keywords <- sapply(included$keywords, grepl, 
                                       pattern = "examin", ignore.case = TRUE)
included$examin.in.abstract <- sapply(included$abstract, grepl, 
                                       pattern = "examin", ignore.case = TRUE)
included$examin.occurs <- ifelse(included$examin.in.title == TRUE |
                                    included$examin.in.keywords == TRUE |
                                    included$examin.in.abstract == TRUE, 1, 0)

# Create new variables that indicate if title, abstract, or keywords contain "predict"  
included$predict.in.title <- sapply(included$title, grepl, 
                                    pattern = "predict", ignore.case = TRUE)
included$predict.in.keywords <- sapply(included$keywords, grepl, 
                                       pattern = "predict", ignore.case = TRUE)
included$predict.in.abstract <- sapply(included$abstract, grepl, 
                                       pattern = "predict", ignore.case = TRUE)
included$predict.occurs <- ifelse(included$predict.in.title == TRUE |
                                    included$predict.in.keywords == TRUE |
                                    included$predict.in.abstract == TRUE, 1, 0)


# Calculate how many RRs contain *any* of the five core words in their title,
# abstract, or keywords: 69/71
n.hypintros.fivewords.RR <- sum((included$hypothes.occurs|
                                   included$test.occurs|
                                   included$replicat.occurs|
                                   included$predict.occurs|
                                   included$examin.occurs) & included$is_RR == 1)



##==============================================================================##
## 4. Create two tables listing all unique hypothesis introduction phrases in 
##    original and replication Registered Reports, respectively, grouped
##    by the five most frequent core words (tables 2 and 3 in the manuscript)
##
##    !!CAUTION!! Extremely long and ugly code (ca. 750 lines) 
##                (Note: this code is only used to create Table 2 and Table 3
##                in the manuscript, not for any other analysis)
##==============================================================================##

## 4.1 Setup

# Create 5 new variables that, for each hypothesis introduction phrase,
# indicate if the phrase contains the core word "hypothes", "replicat",
# "test", "examin", and "predict", respectively
intros.long$hypothes <- sapply(intros.long$phrase, grepl, pattern = "hypothes", ignore.case = TRUE)
intros.long$replicat <- sapply(intros.long$phrase, grepl, pattern = "replicat", ignore.case = TRUE)
intros.long$test <- sapply(intros.long$phrase, grepl, pattern = "test", ignore.case = TRUE)
intros.long$examin <- sapply(intros.long$phrase, grepl, pattern = "examin", ignore.case = TRUE)
intros.long$predict <- sapply(intros.long$phrase, grepl, pattern = "predict", ignore.case = TRUE)

# Because any paper can have more than one hypothesis introduction phrase,
# each table will have different entries that come from the same paper, which
# should be noted in the table note.
# For Table 2:
#Determine how many *original* RRs have more than 1 hypothesis introduction phrase: 11
length(which((hypintros$abstract.count+hypintros$fulltext.count)>1 
             & hypintros$is_replication==0))

# For Table 3:
# Determine how many *replication* RRs have more than 1 hypothesis introduction phrase: 10
length(which((hypintros$abstract.count+hypintros$fulltext.count)>1 
             & hypintros$is_replication==1))


## HERE COME THE TABLES OF DEATH -- EXPAND AT YOUR OWN RISK ##
##############################################################
## 4.2 Create Table 2: 
##     Listing all unique hypothesis introduction phrases 
##     in *ORIGINAL RRs* and how often they occur, grouped
##     by the five most frequent core words

### Each core word gets a separate section in the table. If
### two or more core words co-occur, they get a separate
### section. The table for *original* RRs will have 7 sections:
# 1: hypothes*
# 2: hypothes*, test*
# 3: test*
# 4: test*, predict*
# 5: examin*
# 6: predict*
# 7: (other)

##*****************************************#
## Section 1/7: hypothes*
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 0 &
                                                intros.long$hypothes == TRUE &
                                                intros.long$test == FALSE &
                                                intros.long$predict == FALSE &
                                                intros.long$examin == FALSE)])


auxrow <- c("hypothes*", "", 
            length(which(intros.long$is_replication == 0 &
                           intros.long$origin == "abstract" &
                           intros.long$hypothes == TRUE &
                           intros.long$test == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 0 &
                           intros.long$origin == "fulltext" &
                           intros.long$hypothes == TRUE &
                           intros.long$test == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 0 &
                           intros.long$hypothes == TRUE &
                           intros.long$test == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)))

auxtable <- data.frame(core = rep(NA, length(phraselist)), phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 0))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 0))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 0))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}

phrasetable.orig <- rbind(auxrow, auxtable)

##*****************************************#
## Section 2/7: hypothes*, test*
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 0 &
                                                intros.long$hypothes == TRUE &
                                                intros.long$test == TRUE &
                                                intros.long$replicat == FALSE &
                                                intros.long$predict == FALSE &
                                                intros.long$examin == FALSE)])

auxrow <- c("hypothes*, test*", "", 
            length(which(intros.long$is_replication == 0 &
                           intros.long$origin == "abstract" &
                           intros.long$hypothes == TRUE &
                           intros.long$test == TRUE &
                           intros.long$replicat == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 0 &
                           intros.long$origin == "fulltext" &
                           intros.long$hypothes == TRUE &
                           intros.long$test == TRUE &
                           intros.long$replicat == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 0 &
                           intros.long$hypothes == TRUE &
                           intros.long$test == TRUE &
                           intros.long$replicat == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)))

auxtable <- data.frame(core = rep("", length(phraselist)), phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 0))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 0))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 0))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}

phrasetable.orig <- rbind(phrasetable.orig, auxrow, auxtable)

##*****************************************#
## Section 3/7: test*
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 0 &
                                                intros.long$hypothes == FALSE &
                                                intros.long$test == TRUE &
                                                intros.long$predict == FALSE &
                                                intros.long$examin == FALSE)])
auxrow <- c("test*", "", 
            length(which(intros.long$is_replication == 0 &
                           intros.long$origin == "abstract" &
                           intros.long$hypothes == FALSE &
                           intros.long$test == TRUE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 0 &
                           intros.long$origin == "fulltext" &
                           intros.long$hypothes == FALSE &
                           intros.long$test == TRUE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 0 &
                           intros.long$hypothes == FALSE &
                           intros.long$test == TRUE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)))

auxtable <- data.frame(core = rep("", length(phraselist)), phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 0))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 0))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 0))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}
phrasetable.orig <- rbind(phrasetable.orig, auxrow, auxtable)

##*****************************************#
## Section 4/7: test*, predict*
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 0 &
                                                intros.long$hypothes == FALSE &
                                                intros.long$test == TRUE &
                                                intros.long$predict == TRUE &
                                                intros.long$examin == FALSE)])

auxtable <- data.frame(core = "test*, predict*", phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 0))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 0))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 0))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}
phrasetable.orig <- rbind(phrasetable.orig, auxtable)

##*****************************************#
## Section 5/7: examin*
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 0 &
                                                intros.long$hypothes == FALSE &
                                                intros.long$test == FALSE &
                                                intros.long$predict == FALSE &
                                                intros.long$examin == TRUE)])
auxrow <- c("examin*", "", 
            length(which(intros.long$is_replication == 0 &
                           intros.long$origin == "abstract" &
                           intros.long$hypothes == FALSE &
                           intros.long$test == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == TRUE)),
            length(which(intros.long$is_replication == 0 &
                           intros.long$origin == "fulltext" &
                           intros.long$hypothes == FALSE &
                           intros.long$test == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == TRUE)),
            length(which(intros.long$is_replication == 0 &
                           intros.long$hypothes == FALSE &
                           intros.long$test == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == TRUE)))

auxtable <- data.frame(core = rep("", length(phraselist)), phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 0))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 0))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 0))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}
phrasetable.orig <- rbind(phrasetable.orig, auxrow, auxtable)


##*****************************************#
## Section 6/7: predict*
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 0 &
                                                intros.long$hypothes == FALSE &
                                                intros.long$test == FALSE &
                                                intros.long$predict == TRUE &
                                                intros.long$examin == FALSE)])
auxrow <- c("predict*", "", 
            length(which(intros.long$is_replication == 0 &
                           intros.long$origin == "abstract" &
                           intros.long$hypothes == FALSE &
                           intros.long$test == FALSE &
                           intros.long$predict == TRUE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 0 &
                           intros.long$origin == "fulltext" &
                           intros.long$hypothes == FALSE &
                           intros.long$test == FALSE &
                           intros.long$predict == TRUE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 0 &
                           intros.long$hypothes == FALSE &
                           intros.long$test == FALSE &
                           intros.long$predict == TRUE &
                           intros.long$examin == FALSE)))

auxtable <- data.frame(core = rep("", length(phraselist)), phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 0))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 0))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 0))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}
phrasetable.orig <- rbind(phrasetable.orig, auxrow, auxtable)


##*****************************************#
## Section 7/7: (other)
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 0 &
                                                intros.long$hypothes == FALSE &
                                                intros.long$test == FALSE &
                                                intros.long$predict == FALSE &
                                                intros.long$examin == FALSE)])
auxrow <- c("(other)", "", 
            length(which(intros.long$is_replication == 0 &
                           intros.long$origin == "abstract" &
                           intros.long$hypothes == FALSE &
                           intros.long$test == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 0 &
                           intros.long$origin == "fulltext" &
                           intros.long$hypothes == FALSE &
                           intros.long$test == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 0 &
                           intros.long$hypothes == FALSE &
                           intros.long$test == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)))

auxtable <- data.frame(core = rep("", length(phraselist)), phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 0))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 0))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 0))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}
phrasetable.orig <- rbind(phrasetable.orig, auxrow, auxtable)

phrasetable.orig$core[which(is.na(phrasetable.orig$core))] <- ""


##############################################################
## 4.3 Create Table 3: 
##     Listing all unique hypothesis introduction phrases 
##     in *REPLICATION RRs* and how often they occur, grouped
##     by the five most frequent core words

### Each core word gets a separate section in the table. If
### two or more core words co-occur, they get a separate
### section. The table for *replication* RRs will have 10 sections:

# 1: hypothes*
# 2: hypothes*, test*
# 3: hypothes*, examin*
# 4: hypothes*, predict*
# 5: replicat*
# 6: replicat*, examin*
# 7: test*
# 8: examin*
# 9: predict*
#10: (other)


##*****************************************#
## Section 1/10: hypothes*
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 1 &
                                                intros.long$hypothes == TRUE &
                                                intros.long$test == FALSE &
                                                intros.long$replicat == FALSE &
                                                intros.long$predict == FALSE &
                                                intros.long$examin == FALSE)])

auxrow <- c("hypothes*", "", 
            length(which(intros.long$is_replication == 1 &
                           intros.long$origin == "abstract" &
                           intros.long$hypothes == TRUE &
                           intros.long$test == FALSE &
                           intros.long$replicat == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 1 &
                           intros.long$origin == "fulltext" &
                           intros.long$hypothes == TRUE &
                           intros.long$test == FALSE &
                           intros.long$replicat == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 1 &
                           intros.long$hypothes == TRUE &
                           intros.long$test == FALSE &
                           intros.long$replicat == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)))

auxtable <- data.frame(core = rep(NA, length(phraselist)), phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 1))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 1))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 1))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}

phrasetable.rep <- rbind(auxrow, auxtable)

##*****************************************#
## Section 2/10: hypothes*, test*
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 1 &
                                                intros.long$hypothes == TRUE &
                                                intros.long$test == TRUE &
                                                intros.long$replicat == FALSE &
                                                intros.long$predict == FALSE &
                                                intros.long$examin == FALSE)])

auxrow <- c("hypothes*, test*", "", 
            length(which(intros.long$is_replication == 1 &
                           intros.long$origin == "abstract" &
                           intros.long$hypothes == TRUE &
                           intros.long$test == TRUE &
                           intros.long$replicat == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 1 &
                           intros.long$origin == "fulltext" &
                           intros.long$hypothes == TRUE &
                           intros.long$test == TRUE &
                           intros.long$replicat == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 1 &
                           intros.long$hypothes == TRUE &
                           intros.long$test == TRUE &
                           intros.long$replicat == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)))

auxtable <- data.frame(core = rep("", length(phraselist)), phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 1))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 1))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 1))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}

phrasetable.rep <- rbind(phrasetable.rep, auxrow, auxtable)

##*****************************************#
## Section 3/10: hypothes*, examin*
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 1 &
                                                intros.long$hypothes == TRUE &
                                                intros.long$test == FALSE &
                                                intros.long$replicat == FALSE &
                                                intros.long$predict == FALSE &
                                                intros.long$examin == TRUE)])

auxtable <- data.frame(core = "hypothes*, examin*", phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 1))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 1))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 1))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}

phrasetable.rep <- rbind(phrasetable.rep, auxtable)

##*****************************************#
## Section 4/10: hypothes*, predict*
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 1 &
                                                intros.long$hypothes == TRUE &
                                                intros.long$test == FALSE &
                                                intros.long$replicat == FALSE &
                                                intros.long$predict == TRUE &
                                                intros.long$examin == FALSE)])

auxtable <- data.frame(core = "hypothes*, predict*", phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 1))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 1))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 1))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}

phrasetable.rep <- rbind(phrasetable.rep, auxtable)

##*****************************************#
## Section 5/10: replicat*
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 1 &
                                                intros.long$hypothes == FALSE &
                                                intros.long$test == FALSE &
                                                intros.long$replicat == TRUE &
                                                intros.long$predict == FALSE &
                                                intros.long$examin == FALSE)])

auxrow <- c("replicat*", "", 
            length(which(intros.long$is_replication == 1 &
                           intros.long$origin == "abstract" &
                           intros.long$hypothes == FALSE &
                           intros.long$test == FALSE &
                           intros.long$replicat == TRUE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 1 &
                           intros.long$origin == "fulltext" &
                           intros.long$hypothes == FALSE &
                           intros.long$test == FALSE &
                           intros.long$replicat == TRUE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 1 &
                           intros.long$hypothes == FALSE &
                           intros.long$test == FALSE &
                           intros.long$replicat == TRUE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)))

auxtable <- data.frame(core = rep("", length(phraselist)), phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 1))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 1))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 1))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}

phrasetable.rep <- rbind(phrasetable.rep, auxrow, auxtable)

##*****************************************#
## Section 6/10: replicat*, examin*
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 1 &
                                                intros.long$hypothes == FALSE &
                                                intros.long$test == FALSE &
                                                intros.long$replicat == TRUE &
                                                intros.long$predict == FALSE &
                                                intros.long$examin == TRUE)])

auxtable <- data.frame(core = "replicat*, examin*", phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 1))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 1))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 1))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}

phrasetable.rep <- rbind(phrasetable.rep, auxtable)

##*****************************************#
## Section 7/10: test*
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 1 &
                                                intros.long$hypothes == FALSE &
                                                intros.long$test == TRUE &
                                                intros.long$replicat == FALSE &
                                                intros.long$predict == FALSE &
                                                intros.long$examin == FALSE)])

auxrow <- c("test*", "", 
            length(which(intros.long$is_replication == 1 &
                           intros.long$origin == "abstract" &
                           intros.long$hypothes == FALSE &
                           intros.long$test == TRUE &
                           intros.long$replicat == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 1 &
                           intros.long$origin == "fulltext" &
                           intros.long$hypothes == FALSE &
                           intros.long$test == TRUE &
                           intros.long$replicat == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 1 &
                           intros.long$hypothes == FALSE &
                           intros.long$test == TRUE &
                           intros.long$replicat == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)))

auxtable <- data.frame(core = rep("", length(phraselist)), phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 1))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 1))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 1))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}

phrasetable.rep <- rbind(phrasetable.rep, auxrow, auxtable)

##*****************************************#
## Section 8/10: examin*
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 1 &
                                                intros.long$hypothes == FALSE &
                                                intros.long$test == FALSE &
                                                intros.long$replicat == FALSE &
                                                intros.long$predict == FALSE &
                                                intros.long$examin == TRUE)])

auxtable <- data.frame(core = "examin*", phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 1))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 1))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 1))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}

phrasetable.rep <- rbind(phrasetable.rep, auxtable)

##*****************************************#
## Section 9/10: predict*
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 1 &
                                                intros.long$hypothes == FALSE &
                                                intros.long$test == FALSE &
                                                intros.long$replicat == FALSE &
                                                intros.long$predict == TRUE &
                                                intros.long$examin == FALSE)])

auxtable <- data.frame(core = "predict*", phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 1))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 1))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 1))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}

phrasetable.rep <- rbind(phrasetable.rep, auxtable)

##*****************************************#
## Section 10/10: (other)
##*****************************************#
phraselist <- unique(intros.long$phrase[which(intros.long$is_replication == 1 &
                                                intros.long$hypothes == FALSE &
                                                intros.long$test == FALSE &
                                                intros.long$replicat == FALSE &
                                                intros.long$predict == FALSE &
                                                intros.long$examin == FALSE)])

auxrow <- c("(other)", "", 
            length(which(intros.long$is_replication == 1 &
                           intros.long$origin == "abstract" &
                           intros.long$hypothes == FALSE &
                           intros.long$test == FALSE &
                           intros.long$replicat == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 1 &
                           intros.long$origin == "fulltext" &
                           intros.long$hypothes == FALSE &
                           intros.long$test == FALSE &
                           intros.long$replicat == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)),
            length(which(intros.long$is_replication == 1 &
                           intros.long$hypothes == FALSE &
                           intros.long$test == FALSE &
                           intros.long$replicat == FALSE &
                           intros.long$predict == FALSE &
                           intros.long$examin == FALSE)))

auxtable <- data.frame(core = rep("", length(phraselist)), phrase = NA, 
                       count.abstract = NA, count.fulltext = NA, count.total = NA)

for (i in 1:length(phraselist)) {
  auxtable$phrase[i] <- phraselist[i]
  auxtable$count.abstract[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "abstract"
                                             & intros.long$is_replication == 1))
  auxtable$count.fulltext[i] <- length(which(intros.long$phrase == phraselist[i]
                                             & intros.long$origin == "fulltext"
                                             & intros.long$is_replication == 1))
  auxtable$count.total[i] <- length(which(intros.long$phrase == phraselist[i]
                                          & intros.long$is_replication == 1))
  
  auxtable <- auxtable[order(auxtable$phrase),]
}
phrasetable.rep <- rbind(phrasetable.rep, auxrow, auxtable)

phrasetable.rep$core[which(is.na(phrasetable.rep$core))] <- ""

phrasetable.rep$auxcol1 <- ""
phrasetable.rep$auxcol2 <- ""

phrasetable.rep <- phrasetable.rep[, c(7, 1, 2, 3, 4, 5, 6)]
