library(lubridate)
library(tidyverse)



attorneys <- read_csv(here::here("Data", "attorneys.csv"))
attorney_time_entries <- read_csv(here::here("Data", "attorneytimeentries.csv"))
categories <- read_csv(here::here("Data", "categories.csv"))
clients <- read_csv(here::here("Data", "clients.csv"))
questionposts <- read_csv(here::here("Data", "questionposts.csv"))
questions <- read_csv(here::here("Data", "questions.csv"))
statesites <- read_csv(here::here("Data", "statesites.csv"))
subcategories <- read_csv(here::here("Data", "subcategories.csv"))

state_categories <- data.frame(State = questions$StateAbbr, 
                               AskedByClientUno = questions$AskedByClientUno,
                               Question = questions$QuestionUno, 
                               CategoryUno = questions$CategoryUno,
                               Category = questions$Category,
                               SubcategoryUno = questions$SubcategoryUno,
                               Subcategory = questions$Subcategory)

g1 <- ggplot(state_categories, aes(x = Category)) + 
  geom_bar(aes(fill = Category)) +
  facet_wrap(vars(State), scales = "free") +
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "")

efficiency <- questions %>%
  summarise(StateAbbr,
            QuestionUno,
            CategoryUno,
            Category,
            AskedByClientUno,
            AskedOnUtc,
            TakenOnUtc,
            ClosedOnUtc
            )

efficiency <- efficiency[efficiency$TakenOnUtc != "NULL",]
efficiency <- efficiency[efficiency$ClosedOnUtc != "NULL", ]

efficiency$ResponseTime <- int_length(interval(ymd_hms(efficiency$AskedOnUtc), ymd_hms(efficiency$TakenOnUtc)))/3600
efficiency$ClosureTime <- int_length(interval(ymd_hms(efficiency$TakenOnUtc), ymd_hms(efficiency$ClosedOnUtc)))/3600                                      

g2 <- efficiency %>% 
  group_by(Category) %>% 
  summarise(ClosureTimeAvg = mean(ClosureTime)) %>% 
  ggplot(aes(x = Category, y = ClosureTimeAvg)) + 
  geom_col(aes(fill = Category)) +
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "")

g3 <- efficiency %>% 
  group_by(Category) %>% 
  summarise(ResponseTimeAvg = mean(ResponseTime)) %>% 
  ggplot(aes(x = Category, y = ResponseTimeAvg)) + 
  geom_col(aes(fill = Category)) +
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "")

# From closure graph: obviously discrepancy with closure in juvenile cases
# also: education cases have halved the time for closure compared to juvenile???
# is there a difference between the way juvenile and the way education cases are treated?

questionposts <- questionposts[str_length(questionposts$StateAbbr)==2,]
questionposts$Id <- as.numeric(questionposts$Id)
questionposts <- questionposts[,-c(6:409)]

questions_categories <- data.frame(QuestionUno = questions$QuestionUno, Category = questions$Category)
posts_categories <- left_join(questionposts, questions_categories, by = "QuestionUno")

library(tidytext)
library(wordcloud)
questions_edu <- posts_categories %>%
  filter(Category == "Education")
tidy_edu <- questions_edu %>%
  unnest_tokens(word, PostText)
tidy_edu %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
library(reshape2)
tidy_edu_sent <- tidy_edu %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
tidy_edu_cloud <- tidy_edu_sent %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
g5 <- ggplot(tidy_edu_sent, aes(x = sentiment, y = n)) +
  geom_col(aes(fill = sentiment))

questions_juv <- posts_categories %>%
  filter(Category == "Juvenile")
tidy_juv <- questions_juv %>%
  unnest_tokens(word, PostText)
tidy_juv %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
tidy_juv_sent <- tidy_juv %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
tidy_juv_cloud <- tidy_juv_sent %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
g6 <- ggplot(tidy_juv_sent, aes(x = sentiment, y = n)) +
  geom_col(aes(fill = sentiment))

# weirdly enough: juvenile cases have more positive language than education?
# wonder if positive language encourages more thorough pro bono, leading to
# longer time to close? 

wordChartJuv <- tidy_juv_sent %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

wordChartEdu <- tidy_edu_sent %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

wordChartJuv / wordChartEdu


## focus in on why response time to education and health and disability might take a while

questions_had <- posts_categories %>%
  filter(Category == "Health and Disability")
tidy_had <- questions_had %>%
  unnest_tokens(word, PostText)
tidy_had %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
tidy_had_sent <- tidy_had %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
tidy_had_cloud <- tidy_had_sent %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
g5 <- ggplot(tidy_had_sent, aes(x = sentiment, y = n)) +
  geom_col(aes(fill = sentiment))

g5 / g6

questions_cfq <- posts_categories %>%
  filter(Category == "Consumer Financial Questions")
tidy_cfq <- questions_cfq %>%
  unnest_tokens(word, PostText)
tidy_cfq %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
tidy_cfq_sent <- tidy_cfq %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
tidy_cfq_cloud <- tidy_cfq_sent %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
g5 <- ggplot(tidy_cfq_sent, aes(x = sentiment, y = n)) +
  geom_col(aes(fill = sentiment))

g2 / g3


## look into language used in clients actual question (not responses) to see if that affects response

posts_categories_wordcount <- posts_categories %>%
  group_by(QuestionUno) %>%
  summarise(QuestionUno,
            Category,
            WordCount = sum(str_length(PostText))) %>%
  distinct()

cats_wordcount <- posts_categories_wordcount %>%
  group_by(Category) %>%
  summarise(WordCount = mean(WordCount, na.rm = TRUE))

g7 <- ggplot(cats_wordcount[!is.na(cats_wordcount$Category),], aes(x = Category, y = WordCount)) +
  geom_col(aes(fill = Category))

## juvenile has low word count but also highest closure time???? alex suggests use wordcloud and def

# install package sylcount
library(sylcount)

doc_counts(questionposts$PostText)

readability <- questionposts %>%
  bind_cols(doc_counts(questionposts$PostText)) %>%
  left_join(questions_categories, by = "QuestionUno")

words_cat <- readability %>%
  group_by(Category) %>%
  summarise(words = mean(words)) %>% 
  ggplot(aes(x = Category, y = words)) +
  geom_col(aes(fill = Category))

sents_cat <- readability %>%
  group_by(Category) %>%
  summarise(sents = mean(sents)) %>% 
  ggplot(aes(x = Category, y = sents)) +
  geom_col(aes(fill = Category))

sylls_cat <- readability %>%
  group_by(Category) %>%
  summarise(sylls_per_word = mean(sylls/words, na.rm = TRUE)) %>% 
  ggplot(aes(x = Category, y = sylls_per_word)) +
  geom_col(aes(fill = Category))

polys_cat <- readability %>%
  group_by(Category) %>%
  summarise(polys_per_word = mean(polys/words, na.rm = TRUE)) %>% 
  ggplot(aes(x = Category, y = polys_per_word)) +
  geom_col(aes(fill = Category))

library(patchwork)

(words_cat + sents_cat) / (sylls_cat + polys_cat)


readability2 <- questionposts %>%
  slice_head(n = 20) %>%
  bind_cols(readability(questionposts$PostText)) %>%
  left_join(questions_categories, by = "QuestionUno")

questionposts %>%
  slice_head(n = 20) %>%
  readability(PostText)


parsing <- problems(questionposts)
fixed_str <- parsing %>% 
  group_by(row) %>%
  summarise(string = str_flatten(actual, collapse = ", "))




