#Loading Necessary Libraries to R from above packages.

library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)
library(lattice)
library(textdata)
library(scales)
library(syuzhet)
library(readxl)

#Loading and viewing datasets

inf6029_y1 <- read_excel("Anonymised Student Feedback - Text Comments-20230824T083146Z-001/Anonymised Student Feedback - Text Comments/AY2020-2021/Copy of INF6029 AY2020-2021.xlsx")
inf6029_y2 <- read_excel("Anonymised Student Feedback - Text Comments-20230824T083146Z-001/Anonymised Student Feedback - Text Comments/AY2021-2022/Copy of INF6029 AY2021-2022.xlsx")
inf6029_y3 <- read_excel("Anonymised Student Feedback - Text Comments-20230824T083146Z-001/Anonymised Student Feedback - Text Comments/AY2022-2023/INF6029 AY2022-2023.xlsx")
View(inf6029_y1)
View(inf6029_y2)
View(inf6029_y3)

#Rename headings and admire handiwork
inf6029_y1 <- inf6029_y1[, 1:2]
names(inf6029_y1) <- c("Positive", "Negative")

inf6029_y2 <- inf6029_y2[, 1:2]
names(inf6029_y2) <- c("Positive", "Negative")

inf6029_y3 <- inf6029_y3[, 1:2]
names(inf6029_y3) <- c("Positive", "Negative")

#Tokenization
y1_tidy_dataset_pos = inf6029_y1.pos %>% unnest_tokens(word, Positive)
y1_tidy_dataset_neg = inf6029_y1.neg %>% unnest_tokens(word, Negative)

y2_tidy_dataset_pos = inf6029_y2.pos %>% unnest_tokens(word, Positive)
y2_tidy_dataset_neg = inf6029_y2.neg %>% unnest_tokens(word, Negative)

y3_tidy_dataset_pos = inf6029_y3.pos %>% unnest_tokens(word, Positive)
y3_tidy_dataset_neg = inf6029_y3.neg %>% unnest_tokens(word, Negative)

#Remove stopwords and arrange in descending order
##Positive
y1_tidy_dataset_pos2 = y1_tidy_dataset_pos %>% anti_join(stop_words)
y1_tidy_dataset_pos2 %>% count(word) %>% arrange(desc(n))

y2_tidy_dataset_pos2 = y2_tidy_dataset_pos %>% anti_join(stop_words)
y2_tidy_dataset_pos2 %>% count(word) %>% arrange(desc(n))

y3_tidy_dataset_pos2 = y3_tidy_dataset_pos %>% anti_join(stop_words)
y3_tidy_dataset_pos2 %>% count(word) %>% arrange(desc(n))

##Negative
y1_tidy_dataset_neg2 = y1_tidy_dataset_neg %>% anti_join(stop_words)
y1_tidy_dataset_neg2 %>% count(word) %>% arrange(desc(n))

y2_tidy_dataset_neg2 = y2_tidy_dataset_neg %>% anti_join(stop_words)
y2_tidy_dataset_neg2 %>% count(word) %>% arrange(desc(n))

y3_tidy_dataset_neg2 = y3_tidy_dataset_neg %>% anti_join(stop_words)
y3_tidy_dataset_neg2 %>% count(word) %>% arrange(desc(n))

#Removing numeric variables, new lines, tabs, and spaces
patterndigits = '\\b[0-9]+\\b'

##Positive
###Y1
y1_tidy_dataset_pos2$Positive = y1_tidy_dataset_pos2$word %>%
  str_replace_all("8", '')

y1_tidy_dataset_pos2$Positive = y1_tidy_dataset_pos2$Positive %>%
  str_replace_all("it’s", '')

y1_tidy_dataset_pos2$Positive = y1_tidy_dataset_pos2$Positive %>%
  str_replace_all("i’m", '')

y1_tidy_dataset_pos3 = y1_tidy_dataset_pos2[2, drop = FALSE]
y1_tidy_dataset_pos3 <- y1_tidy_dataset_pos3[!(y1_tidy_dataset_pos3$Positive == ''),]

###Y2
y2_tidy_dataset_pos2$Positive = y2_tidy_dataset_pos2$Positive %>%
  str_replace_all("1", '')

y2_tidy_dataset_pos3 = y2_tidy_dataset_pos2[2, drop = FALSE]
y2_tidy_dataset_pos3 <- y2_tidy_dataset_pos3[!(y2_tidy_dataset_pos3$Positive == ''),]

###Y3
y3_tidy_dataset_pos2$Positive = y3_tidy_dataset_pos2$Positive %>%
  str_replace_all("it’s", '')

y3_tidy_dataset_pos2$Positive = y3_tidy_dataset_pos2$Positive %>%
  str_replace_all("can’t", '')

y3_tidy_dataset_pos3 = y3_tidy_dataset_pos2[2, drop = FALSE]
y3_tidy_dataset_pos3 <- y3_tidy_dataset_pos3[!(y3_tidy_dataset_pos3$Positive == ''),]

##Negative
###Y1
y1_tidy_dataset_neg2$word = y1_tidy_dataset_neg2$word %>%
  str_replace_all("don’t", '')

y1_tidy_dataset_neg2$word = y1_tidy_dataset_neg2$word %>%
  str_replace_all("wasn’t", '')

y1_tidy_dataset_neg3 = filter(y1_tidy_dataset_neg2,!(word == ''))

###Y2
y2_tidy_dataset_neg3 = filter(y2_tidy_dataset_neg2,!(word == ''))

###Y3
y3_tidy_dataset_neg2$word = y3_tidy_dataset_neg2$word %>%
  str_replace_all("don’t", '')

y3_tidy_dataset_neg2$word = y3_tidy_dataset_neg2$word %>%
  str_replace_all("it’s", '')

y3_tidy_dataset_neg2$word = y3_tidy_dataset_neg2$word %>%
  str_replace_all("could’ve", '')

y3_tidy_dataset_neg2$word = y3_tidy_dataset_neg2$word %>%
  str_replace_all("na", '')

y3_tidy_dataset_neg3 = filter(y3_tidy_dataset_neg2,!(word == ''))
y3_tidy_dataset_neg3 = select(y3_tidy_dataset_neg3, word)

#Wordcloud generation
##Generate term frequency tables
###Positive
y1_pos_freq <- y1_tidy_dataset_pos3 %>% count(Positive, sort = TRUE)
y2_pos_freq <- y2_tidy_dataset_pos3 %>% count(Positive, sort = TRUE)
y3_pos_freq <- y3_tidy_dataset_pos3 %>% count(Positive, sort = TRUE)

###Negative
y1_neg_freq <- y1_tidy_dataset_neg3 %>% count(word, sort = TRUE)
y2_neg_freq <- y2_tidy_dataset_neg3 %>% count(word, sort = TRUE)
y3_neg_freq <- y3_tidy_dataset_neg3 %>% count(word, sort = TRUE)

##Wordclouding
###Positive

####Y1
wordcloud(words = y1_pos_freq$Positive, freq = y1_pos_freq$n, min.freq = 1,
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"),
          scale = c(2.5, 0.1))

####Y2
wordcloud(words = y2_pos_freq$Positive, freq = y2_pos_freq$n, min.freq = 1,
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"),
          scale = c(2.5, 0.1))

####Y3
wordcloud(words = y3_pos_freq$Positive, freq = y3_pos_freq$n, min.freq = 1,
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"),
          scale = c(2.5, 0.1))


###Negative

####Y1
wordcloud(words = y1_neg_freq$word, freq = y1_neg_freq$n, min.freq = 1,
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"),
          scale = c(2.5, 0.1))

####Y2
wordcloud(words = y2_neg_freq$word, freq = y2_neg_freq$n, min.freq = 1,
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"),
          scale = c(2.5, 0.1))

####Y3
wordcloud(words = y3_neg_freq$word, freq = y3_neg_freq$n, min.freq = 1,
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"),
          scale = c(2.5, 0.1))