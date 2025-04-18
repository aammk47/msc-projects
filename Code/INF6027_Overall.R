#Loading and viewing datasets
library(readxl)
inf6027_y1 <- read_excel("Anonymised Student Feedback - Text Comments-20230824T083146Z-001/Anonymised Student Feedback - Text Comments/AY2020-2021/Copy of INF6027 AY2020-2021.xlsx")
inf6027_y2 <- read_excel("Anonymised Student Feedback - Text Comments-20230824T083146Z-001/Anonymised Student Feedback - Text Comments/AY2021-2022/Copy of INF6027 AY2021-2022.xlsx")
inf6027_y3 <- read_excel("Anonymised Student Feedback - Text Comments-20230824T083146Z-001/Anonymised Student Feedback - Text Comments/AY2022-2023/INF6027 AY2022-2023.xlsx")
View(inf6027_y1)
View(inf6027_y2)
View(inf6027_y3)

#Rename headings and admire handiwork
inf6027_y1 <- inf6027_y1[, 1:2]
names(inf6027_y1) <- c("Positive", "Negative")

inf6027_y2 <- inf6027_y2[, 1:2]
names(inf6027_y2) <- c("Positive", "Negative")

inf6027_y3 <- inf6027_y3[, 1:2]
names(inf6027_y3) <- c("Positive", "Negative")

#Column slicing
##Y1
inf6027_y1.pos <- select(inf6027_y1, Positive)
inf6027_y1.neg <- select(inf6027_y1, Negative)

##Y2
inf6027_y2.pos <- select(inf6027_y2, Positive)
inf6027_y2.neg <- select(inf6027_y2, Negative)

##Y3
inf6027_y3.pos <- select(inf6027_y3, Positive)
inf6027_y3.neg <- select(inf6027_y3, Negative)

#Tokenization
y1_tidy_dataset_pos = inf6027_y1.pos %>% unnest_tokens(word, Positive)
y1_tidy_dataset_neg = inf6027_y1.neg %>% unnest_tokens(word, Negative)

y2_tidy_dataset_pos = inf6027_y2.pos %>% unnest_tokens(word, Positive)
y2_tidy_dataset_neg = inf6027_y2.neg %>% unnest_tokens(word, Negative)

y3_tidy_dataset_pos = inf6027_y3.pos %>% unnest_tokens(word, Positive)
y3_tidy_dataset_neg = inf6027_y3.neg %>% unnest_tokens(word, Negative)

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

#Removal of stopwords missed by the code
##Positive
###Y1
y1_tidy_dataset_pos2$word = y1_tidy_dataset_pos2$word %>%
  str_replace_all("um", '')

y1_tidy_dataset_pos3 <- y1_tidy_dataset_pos2[!(y1_tidy_dataset_pos2$word == ''),]

###Y2
y2_tidy_dataset_pos2$word = y2_tidy_dataset_pos2$word %>%
  str_replace_all("dont", '')

y2_tidy_dataset_pos3 <- y2_tidy_dataset_pos2[!(y2_tidy_dataset_pos2$word == ''),]

###Y3
y3_tidy_dataset_pos2$word = y3_tidy_dataset_pos2$word %>%
  str_replace_all("10", '')

y3_tidy_dataset_pos2$word = y3_tidy_dataset_pos2$word %>%
  str_replace_all("i’m", '')

y3_tidy_dataset_pos3 <- y3_tidy_dataset_pos2[!(y3_tidy_dataset_pos2$word == ''),]

##Negative
###Y1
y1_tidy_dataset_neg2$word = y1_tidy_dataset_neg2$word %>%
  str_replace_all("till", '')

y1_tidy_dataset_neg2$word = y1_tidy_dataset_neg2$word %>%
  str_replace_all("4", '')

y1_tidy_dataset_neg2$word = y1_tidy_dataset_neg2$word %>%
  str_replace_all("wasn’t", '')

y1_tidy_dataset_neg2$word = y1_tidy_dataset_neg2$word %>%
  str_replace_all("wouldn’t", '')

y1_tidy_dataset_neg2$word = y1_tidy_dataset_neg2$word %>%
  str_replace_all("bit", '')

y1_tidy_dataset_neg3 <- na.omit(y1_tidy_dataset_neg2)
y1_tidy_dataset_neg3 <- y1_tidy_dataset_neg3[!(y1_tidy_dataset_neg3$word == ''),]

###Y2
y2_tidy_dataset_neg3 <- na.omit(y2_tidy_dataset_neg2)

###Y3
y3_tidy_dataset_neg2$word = y3_tidy_dataset_neg2$word %>%
  str_replace_all("e.g", '')

y3_tidy_dataset_neg2$word = y3_tidy_dataset_neg2$word %>%
  str_replace_all("would've", '')

y3_tidy_dataset_neg2$word = y3_tidy_dataset_neg2$word %>%
  str_replace_all("don’t", '')

y3_tidy_dataset_neg2$word = y3_tidy_dataset_neg2$word %>%
  str_replace_all("10", '')

y3_tidy_dataset_neg2$word = y3_tidy_dataset_neg2$word %>%
  str_replace_all("hadn’t", '')

y3_tidy_dataset_neg2$word = y3_tidy_dataset_neg2$word %>%
  str_replace_all("4", '')

y3_tidy_dataset_neg2$word = y3_tidy_dataset_neg2$word %>%
  str_replace_all("20", '')

y3_tidy_dataset_neg3 <- na.omit(y3_tidy_dataset_neg2)
y3_tidy_dataset_neg3 <- y3_tidy_dataset_neg3[!(y3_tidy_dataset_neg3$word == ''),]

#Wordcloud generation
##Generate term frequency tables
###Positive
y1_pos_freq <- y1_tidy_dataset_pos3 %>% count(word, sort = TRUE)
y2_pos_freq <- y2_tidy_dataset_pos3 %>% count(word, sort = TRUE)
y3_pos_freq <- y3_tidy_dataset_pos3 %>% count(word, sort = TRUE)

###Negative
y1_neg_freq <- y1_tidy_dataset_neg3 %>% count(word, sort = TRUE)
y2_neg_freq <- y2_tidy_dataset_neg3 %>% count(word, sort = TRUE)
y3_neg_freq <- y3_tidy_dataset_neg3 %>% count(word, sort = TRUE)

##Wordclouding
###Positive

####Y1
wordcloud(words = y1_pos_freq$word, freq = y1_pos_freq$n, min.freq = 1,
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"),
          scale = c(2.5, 0.1))

####Y2
wordcloud(words = y2_pos_freq$word, freq = y2_pos_freq$n, min.freq = 1,
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"),
          scale = c(2.5, 0.1))

####Y3
wordcloud(words = y3_pos_freq$word, freq = y3_pos_freq$n, min.freq = 1,
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
