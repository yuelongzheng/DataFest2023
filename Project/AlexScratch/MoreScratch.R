#Load data (Transforms "NULL" to properly formatted NA values)
questionsNA <- read_csv("Data/questions.csv", na = "NULL")

#Calculate measures of efficiency
efficiency <- questions %>%
  mutate(
    ResponseTime = int_length(interval(ymd_hms(AskedOnUtc), ymd_hms(TakenOnUtc)))/3600,
    ClosureTime = int_length(interval(ymd_hms(TakenOnUtc), ymd_hms(ClosedOnUtc)))/3600,                                     
    Urgency = 1 / int_length(interval(ymd_hms(AskedOnUtc), ymd_hms(LegalDeadline)))/3600,
    Urgency = ifelse(Urgency < 0, NA, Urgency),
    YearAsked = year(AskedOnUtc),
    MonthAsked = month(AskedOnUtc)
  )

#Case load count by month for each category
p1 <- efficiency %>% 
  ggplot(aes(x = MonthAsked)) + 
  geom_bar() + 
  facet_wrap(~Category, scales = "free") +
  scale_x_continuous(breaks = c(1:12), labels = Months) + 
  #Format
  theme(axis.text.x = element_text(angle = 90)) + 
  guides(fill = guide_legend(title = "Year"))

#Urgency by month for each category
p6 <- efficiency %>% 
  ggplot(aes(x = MonthAsked, y = Urgency)) + 
  geom_col() + 
  facet_wrap(~Category, scales = "free") +
  #Format
  scale_x_continuous(breaks = c(1:12), labels = Months) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  guides(fill = guide_legend(title = "Year"))

#Closure time spikes by month for all cats
p2 <- efficiency %>% 
  ggplot(aes(x = MonthAsked, y = ClosureTime)) + 
  geom_col() + 
  facet_wrap(~Category, scales = "free") +
  #Format
  scale_x_continuous(breaks = c(1:12), labels = Months) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  guides(fill = guide_legend(title = "Year"))

#Response time spikes by month for all cats
p3 <- efficiency %>% 
  ggplot(aes(x = MonthAsked, y = ResponseTime)) + 
  geom_col() + 
  facet_wrap(~Category, scales = "free") +
  #Format
  scale_x_continuous(breaks = c(1:12), labels = Months) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  guides(fill = guide_legend(title = "Year"))

#demonstrate longest closure time by category is juvenile
p4 <- efficiency %>% 
  group_by(Category) %>% 
  summarise(ClosureTimeAvg = mean(ClosureTime, na.rm = TRUE)) %>% 
  ggplot(aes(x = Category, y = ClosureTimeAvg)) + 
  geom_col()

#demonstrate longest response time by category is education
p5 <- efficiency %>% 
  group_by(Category) %>% 
  summarise(ResponseTimeAvg = mean(ResponseTime, na.rm = TRUE)) %>% 
  ggplot(aes(x = Category, y = ResponseTimeAvg)) + 
  geom_col()

#quick inclusion: cases by month by cat, point w smooth
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