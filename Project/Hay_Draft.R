# Ordering

# ___ Is what we believe you guys need to do

# we started by looking at response and closure time across categories

# READING DATA
library(tidyverse)
library(patchwork)
library(sylcount)
library(lubridate)
library(syuzhet)
library(tidytext)
library(tidyr)


attorneys <- read_csv(here::here("Data", "attorneys.csv"))
attorney_time_entries <- read_csv(here::here("Data", "attorneytimeentries.csv"))
categories <- read_csv(here::here("Data", "categories.csv"))
clients <- read_csv(here::here("Data", "clients.csv"))
questionposts <- read_csv(here::here("Data", "questionposts.csv"))
questionposts <- questionposts[str_length(questionposts$StateAbbr)==2,]
questionposts$Id <- as.numeric(questionposts$Id)
questionposts <- questionposts[,-c(6:409)]
questions <- read_csv(here::here("Data", "questions.csv"))
statesites <- read_csv(here::here("Data", "statesites.csv"))
subcategories <- read_csv(here::here("Data", "subcategories.csv"))

# replace with case load
#Long monthly trends
# question_category <- data.frame(AskedOnUtc = questions$AskedOnUtc, Category = questions$Category)
# question_category <- question_category %>%
#   mutate(YearAsked = year(AskedOnUtc),
#          MonthAsked = month(AskedOnUtc))
# 
# p7 <- ggplot(question_category, aes(x = as.factor(YearAsked))) +
#   geom_bar() +
#   facet_wrap(~Category, scales = "free") +
#   theme(axis.text.x = element_text(angle = 90))
# p7 # increasing questions over the years over all areas

#monthly by year
# Months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
# p8 <- ggplot(question_category[question_category$YearAsked != 2022,], aes(x = MonthAsked, fill = as.factor(YearAsked))) +
#   geom_bar() +
#   facet_wrap(~Category, scales = "free") +
#   scale_x_continuous(breaks = c(1:12), labels = Months) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   guides(fill=guide_legend(title="Year"))
# p8 # education has large leap in cases as soon as school goes back for August/September
# what effect could this be having on education cases?

# replace above possibly with this:
Questions_Asked_When = question_category %>%
  count(Category, MonthAsked, YearAsked) %>%
  filter(YearAsked != 2022)
p10 <- ggplot(Questions_Asked_When, aes(x = MonthAsked, y = n, color = as.factor(YearAsked))) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  scale_x_continuous(breaks = c(1:12), labels = Months)+ 
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(~Category, scale = "free_y")  + 
  guides(color=guide_legend(title="Year"))
p9

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

g1 <- efficiency %>% 
  group_by(Category) %>% 
  summarise(ClosureTimeAvg = mean(ClosureTime)) %>% 
  ggplot(aes(x = Category, y = ClosureTimeAvg)) + 
  geom_col(aes(fill = Category)) +
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "")

g2 <- efficiency %>% 
  group_by(Category) %>% 
  summarise(ResponseTimeAvg = mean(ResponseTime)) %>% 
  ggplot(aes(x = Category, y = ResponseTimeAvg)) + 
  geom_col(aes(fill = Category)) +
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "")

g3 <- efficiency %>%
  filter(Category == "Education") %>%
  mutate(ResponseTimeAvg = mean(ResponseTime),
         MonthAsked = as.factor(month(TakenOnUtc))) %>%
  ggplot(aes(x = MonthAsked, y = ResponseTimeAvg)) +
  geom_col(aes(fill = MonthAsked))

(g1 / g2) | g3

# education cases close quickly, but possibly due to heaps of questions take a long response time
# response is generally shorter over Summer, but as soon as school goes back response time take forever

# what type of questions are being asked that may delay response?
educationposts <- questions %>%
  left_join(questionposts, by = "QuestionUno") %>%
  select(QuestionUno, PostText, AskedOnUtc, Category) %>%
  filter(Category == "Education") %>%
  mutate(Month = month(AskedOnUtc, label = TRUE))

tidy_eduposts <- educationposts %>%
  unnest_tokens(word, PostText)
cleaned_eduposts <- tidy_eduposts %>%
  anti_join(get_stopwords()) %>%
  arrange(by_group = Month)

bing <- get_sentiments("bing")

edu_sentiment <- cleaned_eduposts %>%
  inner_join(bing) %>%
  count(Month, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(Month, sentiment)) +
  geom_bar(aes(fill = Month), stat = "identity", show.legend = FALSE)

# responses and all are way more negative in October than positive (from bing)

# What type of cases are being brought in to education in October?

Education_Category = categories %>% 
  filter(Category == "Education") %>%
  right_join(subcategories, by = c("CategoryUno" = "CategoryUno")) %>%
  filter(Category != "NULL")
unique(Education_Category$Subcategory)
SpecialEducation <- c("Special Education", "Special Education (IEPs, 504 Plans, Behavioral Intervention Plans)", "Special Education/Learning Disabilities"  )
Time_Education_Questions <- questions %>% 
  filter(Category == "Education") %>%
  mutate(YearAsked = year(AskedOnUtc)) %>% 
  mutate(MonthAsked = month(AskedOnUtc, label = TRUE))

Time_Education_Questions$Subcategory <- ifelse(Time_Education_Questions$Subcategory == "Education/School Discipline/Special Education" |
       Time_Education_Questions$Subcategory == "Special Education" |
       Time_Education_Questions$Subcategory == "Special Education (IEPs, 504 Plans, Behavioral Intervention Plans)" |
       Time_Education_Questions$Subcategory == "Special Education/Learning Disabilities", "Special Education",  Time_Education_Questions$Subcategory)
Time_Education_Questions$Subcategory <- ifelse(Time_Education_Questions$Subcategory == "Education" |
                                                 Time_Education_Questions$Subcategory == "Education Law Cases" |
                                                 Time_Education_Questions$Subcategory == "School or Special Education" |
                                                 Time_Education_Questions$Subcategory == "School/Education" |
                                                 Time_Education_Questions$Subcategory == "School/Juvenile", "School/Education/SpecialEd",  Time_Education_Questions$Subcategory)
Time_Education_Questions$Subcategory <- ifelse(Time_Education_Questions$Subcategory == "School Discipline" |
                                                 Time_Education_Questions$Subcategory == "School Discipline (Short-term suspensions, Long-term suspensions, Expulsions)" |
                                                 Time_Education_Questions$Subcategory == "School Discipline (including Expulsion and Suspension)", "School Discipline",  Time_Education_Questions$Subcategory)

Edu_Subcats <- Time_Education_Questions %>%
  group_by(MonthAsked) %>%
  count(Subcategory)

g4 <- ggplot(Edu_Subcats, aes(x = MonthAsked, y = n)) +
  geom_col(aes(fill = MonthAsked)) +
  facet_wrap(~Subcategory)

# need more volunteers with expertise in school discipline and special education around August, September and October
# would advise hiring in the quieter months (Summer), and training so they are ready for these conversations. 

SchoolDis <- Time_Education_Questions %>%
  filter(Category == "Education") %>%
  filter(Subcategory == "School Discipline") %>%
  left_join(efficiency, by = "QuestionUno")
SchoolDis <- SchoolDis[SchoolDis$TakenOnUtc.x != "NULL",]
g5 <- SchoolDis %>% 
  mutate(ResponseTimeAvg = mean(ResponseTime),
         MonthAsked = month(TakenOnUtc.x, label = T)) %>%
  ggplot(aes(x = MonthAsked, y = ResponseTimeAvg)) +
  geom_col(aes(fill = MonthAsked))  # response rate takes longer in November (overworked)

SpecialEd <- Time_Education_Questions %>%
  filter(Category == "Education") %>%
  filter(Subcategory == "Special Education") %>%
  left_join(efficiency, by = "QuestionUno")
SpecialEd <- SpecialEd[SpecialEd$TakenOnUtc.x != "NULL",] %>%
  mutate(ResponseTimeAvg = mean(ResponseTime, na.rm = T),
         MonthAsked = month(TakenOnUtc.x, label = T))
g6 <- ggplot(SpecialEd, aes(x = MonthAsked, y = ResponseTimeAvg)) +
  geom_col(aes(fill = MonthAsked))

SchoolGen <- Time_Education_Questions %>%
  filter(Category == "Education") %>%
  filter(Subcategory == "School/Education/SpecialEd") %>%
  left_join(efficiency, by = "QuestionUno")
SchoolGen <- SchoolGen[SchoolGen$TakenOnUtc.x != "NULL",]
g7 <- SchoolGen %>% 
  mutate(ResponseTimeAvg = mean(ResponseTime),
         MonthAsked = month(TakenOnUtc.x, label = T)) %>%
  ggplot(aes(x = MonthAsked, y = ResponseTimeAvg)) +
  geom_col(aes(fill = MonthAsked))

g5 | g6 | g7

g8 <- Time_Education_Questions %>%
  filter(Category == "Education") %>%
  filter(Subcategory != "NULL") %>%
  left_join(efficiency, by = "QuestionUno") %>%
  filter(TakenOnUtc.x != "NULL") %>%
  mutate(ResponseTimeAvg = mean(ResponseTime, na.rm = T),
         MonthAsked = month(TakenOnUtc.x, label = T)) %>%
  ggplot(aes(x = MonthAsked, y = ResponseTimeAvg)) +
  geom_col(aes(fill = MonthAsked)) +
  facet_wrap(~Subcategory)
