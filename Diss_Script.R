#Install Packages
install.packages(c("ggplot2","e1071",
                   "caret", "quanteda",
                   "irlba"))

# Load up csv data and explore in RStudio
experi.data <- read.csv("Experiment.csv", stringsAsFactors = FALSE)
View(experi.data)

#Rename headings and admire handiwork
experi.data <- experi.data[, 1:2]
names(experi.data) <- c("Positive", "Negative")
View(experi.data)

#Before starting any work, check for missing data
length(which(!complete.cases(experi.data)))

#Next, let's determine the text length of each comment
#This is done by adding a new feature for the length of each message
experi.data$TL.positive <- nchar(experi.data$Positive, allowNA = TRUE)
experi.data$TL.negative <- nchar(experi.data$Negative, allowNA = TRUE)
summary(experi.data$TL.positive)
summary(experi.data$TL.negative)

#Load up quanteda to get started with text analytics
library(quanteda)
help(package = "quanteda")

#Tokenize comments - could be grounds for some feature engineering
experi.positive.tokens <- tokens(experi.data$Positive, what = "word",
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, split_hyphens = TRUE)

experi.negative.tokens <- tokens(experi.data$Negative, what = "word",
                              remove_numbers = TRUE, remove_punct = TRUE,
                              remove_symbols = TRUE, split_hyphens = TRUE)
#Lowercase tokens
experi.positive.tokens <- tokens_tolower(experi.positive.tokens)
experi.negative.tokens <- tokens_tolower(experi.negative.tokens)

#Removing Stopwords
experi.positive.tokens <- tokens_select(experi.positive.tokens, stopwords(),
                              selection = "remove")

experi.negative.tokens <- tokens_select(experi.negative.tokens, stopwords(),
                              selection = "remove")

#Removal of contextual (descriptive) stopwords
experi.positive.tokens <- tokens_select(experi.positive.tokens,
                                        c("very", "much", "just", "absolutely",
                                          "really", "fairly", "quite"),
                                        selection = "remove")

experi.negative.tokens <- tokens_select(experi.negative.tokens,
                                        c("very", "much", "just", "absolutely",
                                          "really", "fairly", "quite"),
                                        selection = "remove")
#Produce DFM
experi.positive.dfm <- dfm(experi.positive.tokens, tolower = FALSE)
experi.negative.dfm <- dfm(experi.negative.tokens, tolower = FALSE)

#Produce matrices
experi.positive.matrix <- as.matrix(experi.positive.dfm)
experi.positive.matrix <- t(experi.positive.matrix)
View(experi.positive.matrix)
dim(experi.positive.matrix)

experi.negative.matrix <- as.matrix(experi.negative.dfm)
experi.negative.matrix <- t(experi.negative.matrix)
View(experi.negative.matrix)
dim(experi.negative.matrix)

# Sort by decreasing value of frequency

experi.positive.dtm <- sort(rowSums(experi.positive.matrix), decreasing=TRUE)
experi.positive.dtm.df <- data.frame(word = names(experi.positive.dtm),
                                     freq=experi.positive.dtm)

experi.negative.dtm <- sort(rowSums(experi.negative.matrix), decreasing=TRUE)
experi.negative.dtm.df <- data.frame(word = names(experi.negative.dtm),
                                     freq=experi.negative.dtm)


# Display the top 10 most frequent terms
head(experi.positive.dtm.df, 10)
head(experi.negative.dtm.df, 10)

#Barplots
barplot(experi.positive.dtm.df[1:5,]$freq, las = 2,
        names.arg = experi.positive.dtm.df[1:5,]$word,
        col ="green", main ="Top 5 Most Frequent Positive Words\n (2020-2021)",
        ylab = "Word Frequencies")

barplot(experi.negative.dtm.df[1:5,]$freq, las = 2,
        names.arg = experi.negative.dtm.df[1:5,]$word,
        col ="purple", main ="Top 5 Most Frequent Negative Words\n (2020-2021)",
        ylab = "Word Frequencies")