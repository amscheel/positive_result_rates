# Load all 1919 search results
allSRs <- read.csv("SR_search_results_WoS_20190107.csv")

# Draw a random sample of 150 papers and put them into a new dataframe
set.seed(20190120)
x <- sample(c(1:1919), 150)
SRsample <- allSRs[x, ]

# Sample 8 additional papers and put them into a new dataframe
set.seed(20190120)
replacement <- sample(c(1:1919), 158)[151:158]
SRreplacement <- allSRs[replacement, ]

# Sample 1 additional paper and put it into a new dataframe
set.seed(20190120)
replacement2 <- sample(c(1:1919), 159)[159]
SRreplacement2 <- allSRs[replacement2, ]

