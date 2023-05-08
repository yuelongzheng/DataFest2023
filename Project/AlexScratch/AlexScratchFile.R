library(tidyverse)
library(ggplot2)
library(patchwork)
library(lubridate)

attorneys <- read_csv("Data/attorneys.csv")
attorneyTimeEntries <- read_csv("Data/attorneytimeentries.csv")
categories <- read_csv("Data/categories.csv")
clients <- read_csv("Data/clients.csv")
questionPosts <- read_csv("Data/questionposts.csv")
questions <- read_csv("Data/questions.csv")
questionsNA <- read_csv("Data/questions.csv", na = "NULL")
stateSites <- read_csv("Data/statesites.csv")
subCategories <- read_csv("Data/subcategories.csv")

#Hayley stuff

df <- data.frame(Categories = questions$Category, Subcategories = questions$Subcategory)
df2 <- data.frame(table(df))
df2$Freq <- ifelse(df2$Freq == 0, NA, df2$Freq)
df2 <- df2[complete.cases(df2),]

state_categories <- data.frame(State = questions$StateAbbr, 
                               AskedByClientUno = questions$AskedByClientUno,
                               Question = questions$QuestionUno, 
                               CategoryUno = questions$CategoryUno,
                               Category = questions$Category,
                               SubcategoryUno = questions$SubcategoryUno,
                               Subcategory = questions$Subcategory)

table(state_categories$State, state_categories$Category)

g1 <- ggplot(state_categories, aes(x = Category)) + 
  geom_bar(aes(fill = Category)) +
  facet_wrap(vars(State), scales = "free") +
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "")

efficiency <- questionsNA %>%
  summarise(StateAbbr,
            QuestionUno,
            CategoryUno,
            Category,
            AskedByClientUno,
            AskedOnUtc,
            TakenOnUtc,
            ClosedOnUtc,
            LegalDeadline
            )

efficiency <- efficiency[efficiency$TakenOnUtc != "NULL",]
efficiency <- efficiency[efficiency$ClosedOnUtc != "NULL", ]
efficiency <- efficiency[efficiency$LegalDeadline != "NULL", ]
efficiency$LegalDeadline <- efficiency$LegalDeadline %>% 
  replace_na()

efficiency$ResponseTime <- int_length(interval(ymd_hms(efficiency$AskedOnUtc), ymd_hms(efficiency$TakenOnUtc)))/3600
efficiency$ClosureTime <- int_length(interval(ymd_hms(efficiency$TakenOnUtc), ymd_hms(efficiency$ClosedOnUtc)))/3600                                      
efficiency$Urgency <- 1 / int_length(interval(ymd_hms(efficiency$AskedOnUtc), ymd_hms(efficiency$LegalDeadline)))/3600


g2 <- ggplot(efficiency, aes(x = Category, y = ResponseTime)) +
  geom_col(aes(fill = Category)) +
  scale_x_discrete(labels = NULL, breaks = NULL) + 
  labs(x = "")

g3 <- ggplot(efficiency, aes(x = Category, y = ClosureTime)) +
  geom_col(aes(fill = Category)) +
  scale_x_discrete(labels = NULL, breaks = NULL) + 
  labs(x = "")

#testing averages
g4 <- efficiency %>% 
  group_by(Category) %>% 
  summarise(ClosureTimeAvg = mean(ClosureTime, na.rm = TRUE)) %>% 
  ggplot(aes(x = Category, y = ClosureTimeAvg)) + 
  geom_col()

g5 <- efficiency %>% 
  group_by(Category) %>% 
  summarise(ResponseTimeAvg = mean(ResponseTime, na.rm = TRUE)) %>% 
  ggplot(aes(x = Category, y = ResponseTimeAvg)) + 
  geom_col()

g6 <- efficiency %>% 
  group_by(Category) %>% 
  summarise(UrgencyAvg = mean(Urgency, na.rm = TRUE)) %>% 
  ggplot(aes(x = Category, y = UrgencyAvg)) + 
  geom_col()

effSummary <- efficiency %>% 
  group_by(Category) %>% 
  summarise(NumberOfCases = n(), 
            UrgencyAvg = mean(Urgency, na.rm = TRUE),
            ClosureTimeAvg = mean(ClosureTime, na.rm = TRUE),
            ResponseTimeAvg = mean(ResponseTime, na.rm = TRUE)
            ) 

g7 <- effSummary %>% 
  ggplot(aes(x = ResponseTimeAvg, y = UrgencyAvg, colour = Category, size = NumberOfCases)) + 
  geom_point()

# exploring average income, median income and max income in each state
qWithIncome <- questions %>%
  slice_head(n = 20) %>%
  mutate(
    income = clients[clients$ClientUno == AskedByClientUno,]$AnnualIncome[1]
  )

stateSites %>% 
  mutate(
    medianIncome = 
  )


#Long monthly trends
question_category = data.frame(questions$AskedOnUtc, questions$Category)
question_category = mutate(question_category, YearAsked = 
                             year(question_category$questions.AskedOnUtc))
question_category = mutate(question_category, MonthAsked = 
                             month(question_category$questions.AskedOnUtc))

p7 <- ggplot(question_category, aes(x = YearAsked)) + geom_bar() + facet_wrap(~questions.Category, scales = "free") + theme(axis.text.x = element_text(angle = 90))
p7
#monthly by year
Months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
p8 <- ggplot(question_category, aes(x = MonthAsked, fill = as.factor(YearAsked))) + geom_bar() + facet_wrap(~questions.Category, scales = "free") + scale_x_continuous(breaks = c(1:12), labels = Months) + theme(axis.text.x = element_text(angle = 90)) + guides(fill=guide_legend(title="Year"))
p8

#my addition: monthly by category
p9 <- question_category %>% 
  ggplot(aes(x = MonthAsked, fill = as.factor(questions.Category))) + 
  geom_bar() + 
  facet_wrap(~YearAsked, scales = "free") +
  scale_x_continuous(breaks = c(1:12), labels = Months) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  guides(fill = guide_legend(title = "Year"))

#mine: category freq facet by month
p10 <- question_category %>% 
  ggplot(aes(x = MonthAsked)) + 
  geom_bar() + 
  facet_wrap(~questions.Category, scales = "free") +
  scale_x_continuous(breaks = c(1:12), labels = Months) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  guides(fill = guide_legend(title = "Year"))
  
p11 <- question_category %>% 
  filter(questions.Category == "Juvenile") %>% 
  ggplot(aes(x = MonthAsked)) + 
  geom_bar() + 
  scale_x_continuous(breaks = c(1:12), labels = Months) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  guides(fill = guide_legend(title = "Year"))

#^Hire people in august bc response time colossal

question_category$ResponseTime = efficiency$ResponseTime
question_category$ClosureTime = efficiency$ClosureTime

#Closure time spikes by month for juvenile
p12 <- question_category %>% 
  filter(questions.Category == "Juvenile") %>% 
  ggplot(aes(x = MonthAsked, y = ClosureTime)) + 
  geom_col() + 
  scale_x_continuous(breaks = c(1:12), labels = Months) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  guides(fill = guide_legend(title = "Year"))
#^Big spike in closure time in feb and may

#Closure time spikes by month for all cats
p13 <- question_category %>% 
  ggplot(aes(x = MonthAsked, y = ClosureTime)) + 
  geom_col() + 
  facet_wrap(~questions.Category, scales = "free") +
  scale_x_continuous(breaks = c(1:12), labels = Months) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  guides(fill = guide_legend(title = "Year"))

#Closure time spikes by month for all cats
p14 <- question_category %>% 
  ggplot(aes(x = MonthAsked, y = ResponseTime)) + 
  geom_col() + 
  facet_wrap(~questions.Category, scales = "free") +
  scale_x_continuous(breaks = c(1:12), labels = Months) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  guides(fill = guide_legend(title = "Year"))

#looking at how many unique AttorneyUnos are answering each category
questions %>%
  group_by(Category) %>%
  summarise(
    UniqueAttorneyCount = length(unique(TakenByAttorneyUno)),
    CaseCount = n()
  )
  


#look at attorney workload by category
Caseload <- questions %>% 
  group_by(TakenByAttorneyUno, Category) %>%
  summarise(
    CaseCount = n()
  ) %>% 
  group_by(Category) %>%
  summarise(
    AvgCaseCount = mean(CaseCount)
  )

effSummary$AvgCaseCount <- Caseload$AvgCaseCount





point1 <- effSummary %>% 
  ggplot(aes(x = ResponseTimeAvg, y = UrgencyAvg, colour = Category, size = AvgCaseCount)) + 
  geom_point()

point2 <- effSummary %>% 
  ggplot(aes(x = ResponseTimeAvg, y = ClosureTimeAvg, colour = Category, size = AvgCaseCount)) + 
  geom_point()

point3 <- effSummary %>% 
  ggplot(aes(x = ResponseTimeAvg, y = , colour = Category, size = AvgCaseCount)) + 
  geom_point()


#add in the urgency column to question_category
question_category$Urgency <- efficiency$Urgency
