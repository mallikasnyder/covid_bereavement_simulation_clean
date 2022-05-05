require(tidyverse)

stmf2 <- fread("https://www.mortality.org/Public/STMF/Outputs/stmf.csv")
names(stmf2) <- tolower(names(stmf2))

last_week <- stmf2 %>%
  filter(year == 2021 & sex == "f") %>%
  group_by(countrycode) %>%
  summarize(last_week = max(week)) 

last_week %>%
  ggplot() + geom_histogram(aes(x = last_week), binwidth = 1) +
  labs(x = "Last week in 2021 in data", y = "Count")


View(last_week %>%
  arrange(-last_week))

