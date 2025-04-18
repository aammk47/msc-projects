#Loading and viewing datasets
inf6032_y1 <- read_excel("Anonymised Student Feedback - Text Comments-20230824T083146Z-001/Anonymised Student Feedback - Text Comments/AY2020-2021/Copy of INF6032 AY2020-2021.xlsx")
inf6032_y2 <- read_excel("Anonymised Student Feedback - Text Comments-20230824T083146Z-001/Anonymised Student Feedback - Text Comments/AY2021-2022/Copy of INF6032 AY2021-2022.xlsx")

View(inf6032_y1)
View(inf6032_y2)

#Rename headings and admire handiwork
inf6032_y1 <- inf6032_y1[, 1:2]
names(inf6032_y1) <- c("Positive", "Negative")

inf6032_y2 <- inf6032_y2[, 1:2]
names(inf6032_y2) <- c("Positive", "Negative")

#Column slicing
##Y1
inf6032_y1.pos <- select(inf6032_y1, Positive)
inf6032_y1.neg <- select(inf6032_y1, Negative)

##Y2
inf6032_y2.pos <- select(inf6032_y2, Positive)
inf6032_y2.neg <- select(inf6032_y2, Negative)

#Tokenization
y1_tidy_dataset_pos = inf6032_y1.pos %>% unnest_tokens(word, Positive)
y1_tidy_dataset_neg = inf6032_y1.neg %>% unnest_tokens(word, Negative)

y2_tidy_dataset_pos = inf6032_y2.pos %>% unnest_tokens(word, Positive)
y2_tidy_dataset_neg = inf6032_y2.neg %>% unnest_tokens(word, Negative)

#Remove stopwords and arrange in descending order
##Positive
y1_tidy_dataset_pos2 = y1_tidy_dataset_pos %>% anti_join(stop_words)
y1_tidy_dataset_pos2 %>% count(word) %>% arrange(desc(n))

y2_tidy_dataset_pos2 = y2_tidy_dataset_pos %>% anti_join(stop_words)
y2_tidy_dataset_pos2 %>% count(word) %>% arrange(desc(n))

##Negative
y1_tidy_dataset_neg2 = y1_tidy_dataset_neg %>% anti_join(stop_words)
y1_tidy_dataset_neg2 %>% count(word) %>% arrange(desc(n))

y2_tidy_dataset_neg2 = y2_tidy_dataset_neg %>% anti_join(stop_words)
y2_tidy_dataset_neg2 %>% count(word) %>% arrange(desc(n))

#Removing numeric variables, new lines, tabs, and spaces
patterndigits = '\\b[0-9]+\\b'

#Removal of stopwords missed by the code
##Positive
###Y1
y1_tidy_dataset_pos3 <- y1_tidy_dataset_pos2[!(y1_tidy_dataset_pos2$word == ''),]

###Y2
y2_tidy_dataset_pos2$word = y2_tidy_dataset_pos2$word %>%
  str_replace_all("e.g", '')

y2_tidy_dataset_pos2$word = y2_tidy_dataset_pos2$word %>%
  str_replace_all("lots", '')

y2_tidy_dataset_pos2$word = y2_tidy_dataset_pos2$word %>%
  str_replace_all("didn’t", '')

y2_tidy_dataset_pos3 <- na.omit(y2_tidy_dataset_pos2)
y2_tidy_dataset_pos3 <- y2_tidy_dataset_pos3[!(y2_tidy_dataset_pos3$word == ''),]

##Negative
###Y1
y1_tidy_dataset_neg2$word = y1_tidy_dataset_neg2$word %>%
  str_replace_all("e.g", '')

y1_tidy_dataset_neg2$word = y1_tidy_dataset_neg2$word %>%
  str_replace_all("19", '')

y1_tidy_dataset_neg3 <- na.omit(y2_tidy_dataset_pos2)
y1_tidy_dataset_neg3 <- y1_tidy_dataset_neg3[!(y1_tidy_dataset_neg3$word == ''),]

###Y2
y2_tidy_dataset_neg2$word = y2_tidy_dataset_neg2$word %>%
  str_replace_all("40", '')

y2_tidy_dataset_neg2$word = y2_tidy_dataset_neg2$word %>%
  str_replace_all("1", '')

y2_tidy_dataset_neg2$word = y2_tidy_dataset_neg2$word %>%
  str_replace_all("don’t", '')

y2_tidy_dataset_neg2$word = y2_tidy_dataset_neg2$word %>%
  str_replace_all("na", '')

y2_tidy_dataset_neg2$word = y2_tidy_dataset_neg2$word %>%
  str_replace_all("2", '')

y2_tidy_dataset_neg2$word = y2_tidy_dataset_neg2$word %>%
  str_replace_all("3", '')

y2_tidy_dataset_neg2$word = y2_tidy_dataset_neg2$word %>%
  str_replace_all("12", '')

y2_tidy_dataset_neg2$word = y2_tidy_dataset_neg2$word %>%
  str_replace_all("ein", 'in')

y2_tidy_dataset_neg2$word = y2_tidy_dataset_neg2$word %>%
  str_replace_all("bit", '')

y2_tidy_dataset_neg3 <- na.omit(y2_tidy_dataset_neg2)
y2_tidy_dataset_neg3 <- y2_tidy_dataset_neg3[!(y2_tidy_dataset_neg3$word == ''),]

#Wordcloud generation
##Generate term frequency tables
###Positive
y1_pos_freq <- y1_tidy_dataset_pos3 %>% count(word, sort = TRUE)
y2_pos_freq <- y2_tidy_dataset_pos3 %>% count(word, sort = TRUE)

###Negative
y1_neg_freq <- y1_tidy_dataset_neg3 %>% count(word, sort = TRUE)
y2_neg_freq <- y2_tidy_dataset_neg3 %>% count(word, sort = TRUE)

##Wordclouding
###Positive

####Y1
wordcloud(words = y1_pos_freq$word, freq = y1_pos_freq$n, min.freq = 1,
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"),
          scale = c(1.5, 0.1))

####Y2
wordcloud(words = y2_pos_freq$word, freq = y2_pos_freq$n, min.freq = 1,
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"),
          scale = c(1.5, 0.2))

###Negative

####Y1
wordcloud(words = y1_neg_freq$word, freq = y1_neg_freq$n, min.freq = 1,
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"),
          scale = c(1.5, 0.1))

####Y2
wordcloud(words = y2_neg_freq$word, freq = y2_neg_freq$n, min.freq = 1,
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"),
          scale = c(1.5, 0.1))
