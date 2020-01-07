library("dplyr")
library("stringr")
library("reshape2")



#source("analysis.R")
# load the dataset from RDS created with codebook
alldata <- readRDS("positive_results_in_registered_reports_codebook.rds")

# remove excluded papers
included <- alldata[alldata$include_in_analysis==1,]

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

## 6.2 Analyse the introduction phrases and create clusters of five "core words"


hypintros <- included[which(included$is_RR==1), c("id",
                                                  "support_binary",
                                                  "is_replication",
                                                  "hyp_from_fulltext",
                                                  "RR_hyp_intro_abstract", 
                                                  "RR_hyp_intro_fulltext")]

hypintros <- as.data.frame(hypintros)

hypintros$abstract.count <- ifelse(is.na(str_count(hypintros$RR_hyp_intro_abstract, ";")), 0, 
                                   str_count(hypintros$RR_hyp_intro_abstract, ";") + 1)
hypintros$fulltext.count <- ifelse(is.na(str_count(hypintros$RR_hyp_intro_fulltext, ";")), 0, 
                                   str_count(hypintros$RR_hyp_intro_fulltext, ";") + 1)

hypintros$introabstract.1 <- sapply(strsplit(hypintros$RR_hyp_intro_abstract, "; "), "[", 1)
hypintros$introabstract.2 <- sapply(strsplit(hypintros$RR_hyp_intro_abstract, "; "), "[", 2)
hypintros$introfulltext.1 <- sapply(strsplit(hypintros$RR_hyp_intro_fulltext, "; "), "[", 1)
hypintros$introfulltext.2 <- sapply(strsplit(hypintros$RR_hyp_intro_fulltext, "; "), "[", 2)
hypintros$introfulltext.3 <- sapply(strsplit(hypintros$RR_hyp_intro_fulltext, "; "), "[", 3)

# Determine how many replication RRs have more than 1 hypothesis introduction phrase: 10
length(which((hypintros$abstract.count+hypintros$fulltext.count)>1 & hypintros$is_replication==1))

# Determine how many original RRs have more than 1 hypothesis introduction phrase: 11
length(which((hypintros$abstract.count+hypintros$fulltext.count)>1 & hypintros$is_replication==0))

# Determine how many RRs had only hypothesis intros coded from the full text
n.hypintro.fulltext.only <- length(which(hypintros$abstract.count == 0 & hypintros$fulltext.count>0))


##==============================================================================##
## 6.3
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

# Create new variables that indicate if title, abstract, or keywords contain "examine"  
included$examine.in.title <- sapply(included$title, grepl, 
                                    pattern = "examine", ignore.case = TRUE)
included$examine.in.keywords <- sapply(included$keywords, grepl, 
                                       pattern = "examine", ignore.case = TRUE)
included$examine.in.abstract <- sapply(included$abstract, grepl, 
                                       pattern = "examine", ignore.case = TRUE)
included$examine.occurs <- ifelse(included$examine.in.title == TRUE |
                                    included$examine.in.keywords == TRUE |
                                    included$examine.in.abstract == TRUE, 1, 0)

sum(included$hypothes.occurs & included$is_RR == 0)
sum(included$test.occurs & included$is_RR == 0)
sum(included$replicat.occurs & included$is_RR == 0)
sum(included$predict.occurs & included$is_RR == 0)
sum(included$examine.occurs & included$is_RR == 0)

n.hypintros.fivewords.RR <- sum((included$hypothes.occurs|
                                 included$test.occurs|
                                 included$replicat.occurs|
                                 included$predict.occurs|
                                 included$examine.occurs) & included$is_RR == 1)



###########################################################################################

intros.long <- melt(hypintros, 
                    id.vars = c("id", "is_replication", "hyp_from_fulltext", 
                                "abstract.count", "fulltext.count"), 
                    measure.vars = c("introabstract.1", "introabstract.2", 
                                     "introfulltext.1", "introfulltext.2", "introfulltext.3"),
                    variable.name = "origin", value.name = "phrase", na.rm = TRUE)

intros.long$origin <- ifelse(sapply(intros.long$origin, grepl, pattern = "abstract"), "abstract", "fulltext")

# clean up: some phrases had "\n" at the end, remove that
intros.long$phrase <- sapply(strsplit(intros.long$phrase, "\n"), "[", 1)

intros.long$test <- sapply(intros.long$phrase, grepl, pattern = "test")
intros.long$hypothes <- sapply(intros.long$phrase, grepl, pattern = "hypothes", ignore.case = TRUE)
intros.long$replicat <- sapply(intros.long$phrase, grepl, pattern = "replicat")
intros.long$predict <- sapply(intros.long$phrase, grepl, pattern = "predict")
intros.long$examin <- sapply(intros.long$phrase, grepl, pattern = "examin")

uniquephrases <- unique(intros.long$phrase)
uniquewords <- unique(unlist(sapply(strsplit(uniquephrases, " "), unique)))
uniquewords <- uniquewords[order(uniquewords)]

wordstems.cleaned <- c("according", "aim", "assume", "attempt", 
                       "conduct", "critically", "establish", "examine", 
                       "expect", "experiment", "H1", "hypothes", 
                       "if",  "investigate", "performed", "predication", 
                       "predict", "present", "registered", "replicat", 
                       "report", "reproduce", "sought", "suggests", 
                       "test", "then")

wordoccurrences.df <- data.frame(word = wordstems.cleaned, count = NA)
for (i in 1:length(wordstems.cleaned)) {
  wordoccurrences.df$count[i] <- sum(sapply(grepl(wordstems.cleaned[i], 
                                                  intros.long$phrase, 
                                                  ignore.case = TRUE), sum))
}
wordoccurrences.df <- wordoccurrences.df[order(-wordoccurrences.df$count),]

# number of times "test*" and "hypothes*" co-occur
test.and.hypothes <- sum(grepl("test", intros.long$phrase) & grepl("hypothes", 
                                                                   intros.long$phrase,
                                                                   ignore.case = TRUE))

# number of hyp intros that contain either "test*" OR "hypothes*"
test.or.hypothes <- sum(grepl("test", intros.long$phrase) | grepl("hypothes", 
                                                                  intros.long$phrase,
                                                                  ignore.case = TRUE))

## HERE COME THE TABLES OF DEATH -- EXPAND AT YOUR OWN RISK ##
#######################################################################################
### MAKE TABLE FOR ORIGINAL HYPOTHESIS INTROS ###
### Original hypothesis intros have 7 categories:
# 1: hypothes*, test*
# 2: hypothes*
# 3: test*, predict*
# 4: test*
# 5: predict*
# 6: examin*
# 7: (other)

##*****************************************#
## PART 1: hypothes*, test*
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
## PART 2: hypothes*
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
## PART 3: test*
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
## PART 4: test*, predict*
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
## PART 5: predict*
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
## PART 6: examin*
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
## PART 7: (other)
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
## THAT'S IT THE TABLE WITH ORIGINAL HYP INTROS IS DONE, SORRY FOR THE UGLY CODE ##

#######################################################################################
### MAKE TABLE FOR *REPLICATION* HYPOTHESIS INTROS ###
### Replication hypothesis intros have 10 categories:
# 1: hypothes*, test*
# 2: hypothes*, predict*
# 3: hypothes*, examin*
# 4: hypothes*
# 5: test*
# 6: replicat*
# 7: replicat*, examine*
# 8: predict*
# 9: examin*
#10: (other)

##*****************************************#
## PART 1: hypothes*, test*
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
## PART 2: hypothes*, predict*
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
## PART 3: hypothes*, examin*
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
## PART 4: hypothes*
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
## PART 5: test*
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
## PART 6: replicat*
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
## PART 7: replicat*, examine*
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
## PART 8: predict*
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
## PART 9: examin*
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
## PART 10: (other)
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





