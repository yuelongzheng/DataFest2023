#loading packages
library(tidyverse)
library(patchwork)
library(sylcount)
library(lubridate)
library(syuzhet)
library(tidytext)
library(tidyr)
library(tidytext)
library(wordcloud)


#reading data
questionposts <- read_csv(here::here("Data", "questionposts.csv"))
questionposts <- questionposts[str_length(questionposts$StateAbbr)==2,]
questionposts$Id <- as.numeric(questionposts$Id)
questionposts <- questionposts[,-c(6:409)]
questions <- read_csv(here::here("Data", "questions.csv"))
subcategories <- read_csv(here::here("Data", "subcategories.csv"))
categories <- read_csv(here::here("Data", "categories.csv"))

# CASELOAD
question_category <- data.frame(AskedOnUtc = questions$AskedOnUtc, Category = questions$Category)
question_category <- question_category %>%
  mutate(YearAsked = year(AskedOnUtc),
         MonthAsked = month(AskedOnUtc)) %>%
  count(Category, MonthAsked, YearAsked)
p1 <- ggplot(question_category, aes(x = MonthAsked, y = n, color = as.factor(YearAsked))) +
  geom_point() +
  geom_smooth(se = F) +
  scale_x_continuous(breaks = c(1:12))+
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Category, scale = "free_y")  +
  guides(color=guide_legend(title="Year"))
p1

# RESPONSE AND CLOSURE TIME OF EDUCATION

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

#Closure time spikes by month for education
p2 <- efficiency %>%
  filter(Category == "Education") %>%
  mutate(MonthAsked = month(TakenOnUtc, label = T)) %>%
  group_by(MonthAsked) %>%
  summarise(ClosureTimeAvg = mean(ClosureTime)) %>%
  ggplot(aes(x = MonthAsked, y = ClosureTimeAvg)) +
  geom_col(aes(fill = MonthAsked))

#Response time spikes by month for education
p3 <- efficiency %>%
  filter(Category == "Education") %>%
  mutate(MonthAsked = month(TakenOnUtc, label = T)) %>%
  group_by(MonthAsked) %>%
  summarise(ResponseTimeAvg = mean(ResponseTime, na.rm = T)) %>%
  ggplot(aes(x = MonthAsked, y = ResponseTimeAvg)) +
  geom_col(aes(fill = MonthAsked))

#Response time spikes by month for all categories
p4 <- efficiency %>%
  group_by(Category) %>%
  mutate(MonthAsked = as.factor(month(TakenOnUtc))) %>%
  group_by(MonthAsked) %>%
  summarise(ResponseTimeAvg = mean(ResponseTime, na.rm = T), Category) %>%
  ggplot(aes(x = MonthAsked, y = ResponseTimeAvg)) +
  geom_col(aes(fill = MonthAsked)) + 
  facet_wrap(~Category)

# Subcategory surges

Time_Education_Questions <- questions %>% 
  filter(Category == "Education") %>%
  mutate(YearAsked = year(AskedOnUtc)) %>% 
  mutate(MonthAsked = month(AskedOnUtc, label = TRUE))

Time_Education_Questions$Subcategory <- ifelse(Time_Education_Questions$Subcategory == "Education/School Discipline/Special Education" |
                                                 Time_Education_Questions$Subcategory == "Special Education" |
                                                 Time_Education_Questions$Subcategory == "School or Special Education" |
                                                 Time_Education_Questions$Subcategory == "Special Education (IEPs, 504 Plans, Behavioral Intervention Plans)" |
                                                 Time_Education_Questions$Subcategory == "Special Education/Learning Disabilities", "Special Education",  Time_Education_Questions$Subcategory)
Time_Education_Questions$Subcategory <- ifelse(Time_Education_Questions$Subcategory == "Education" |
                                                 Time_Education_Questions$Subcategory == "Education Law Cases" |
                                                 Time_Education_Questions$Subcategory == "School/Education" |
                                                 Time_Education_Questions$Subcategory == "School/Juvenile", "School/Education",  Time_Education_Questions$Subcategory)
Time_Education_Questions$Subcategory <- ifelse(Time_Education_Questions$Subcategory == "School Discipline" |
                                                 Time_Education_Questions$Subcategory == "School Discipline (Short-term suspensions, Long-term suspensions, Expulsions)" |
                                                 Time_Education_Questions$Subcategory == "School Discipline (including Expulsion and Suspension)", "School Discipline",  Time_Education_Questions$Subcategory)

Edu_Subcats <- Time_Education_Questions %>%
  group_by(MonthAsked) %>%
  count(Subcategory)

p5 <- ggplot(Edu_Subcats, aes(x = MonthAsked, y = n)) +
  geom_col(aes(fill = MonthAsked)) +
  facet_wrap(~Subcategory)

# g8 <- Time_Education_Questions %>%
#   filter(Category == "Education") %>%
#   filter(Subcategory != "NULL") %>%
#   left_join(efficiency, by = "QuestionUno") %>%
#   filter(TakenOnUtc.x != "NULL") %>%
#   mutate(MonthAsked = month(TakenOnUtc.x, label = T)) %>%
#   group_by(MonthAsked, Subcategory) %>%
#   summarise(ResponseTimeAvg = mean(ResponseTime, na.rm = T)) %>%
#   ggplot(aes(x = MonthAsked, y = ResponseTimeAvg)) +
#   geom_col(aes(fill = MonthAsked)) +
#   facet_wrap(~Subcategory)

# word analysis of suspension and expulsion in word analysis:
initialquestions <- questionposts %>%
  group_by(QuestionUno) %>%
  summarise(initialquestion = first(PostText)) %>%
  inner_join(questions_categories, by = "QuestionUno")

initialquestionsedu <- initialquestions %>%
  filter(Category == "Education")

suspensions_count <- sum(str_detect(initialquestionsedu$initialquestion, "suspen"), na.rm = T)
expulsions_count <- sum(str_detect(initialquestionsedu$initialquestion, "expel"), na.rm = T) + sum(str_detect(questionposts$PostText, "expul"), na.rm = T)



table(initialquestions$Category)

