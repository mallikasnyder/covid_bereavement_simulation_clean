require(tidyverse)

gkp <- read_csv("~/covid_simulation/Output/compare.csv") %>%
  dplyr::select(c(kin, age, gkp_covid, gkp_other)) %>%
  mutate(kintype = kin, kin = NULL)

socsim <- read_csv("~/covid_simulation/Data/SWE_sim_female_counts_v2.csv") %>%
  dplyr::select(c(kintype, age, mean_kin_post_covid, mean_kin_post_other)) %>%
  mutate(socsim_covid = mean_kin_post_covid,
         socsim_other = mean_kin_post_other,
         mean_kin_post_covid = NULL,
         mean_kin_post_other = NULL)

compare <- merge(gkp, socsim)
  

compare %>%
  filter(kintype != "ggmothers") %>%
  pivot_longer(cols = c("socsim_covid", "socsim_other", 
                        "gkp_covid", "gkp_other"),
               names_to = c("source", "scenario"),
               names_sep = "_",
               values_to = "value") %>%
  mutate(kintype = if_else(kintype == "gmothers", "grandmothers", 
                       if_else(kintype == "mother", "Mothers", kintype)),
         kintype = str_to_title(kintype), 
         scenario = if_else(scenario == "other", 
                            "Counterfactual", "Covid-19"),
         source = if_else(source == "gkp",
                          "Formal", "Microsimulation")) %>%
  ggplot() + geom_bar(aes(x = age, y = value, 
                          fill = scenario, 
                          alpha = source), 
                     stat = "identity",
                     position = "dodge", color = "gray") + 
  facet_wrap(~kintype, nrow = 2) + 
    labs(x = "Age of Ego", 
         y = "Mean number of kin in July 2020",
         fill = "Scenario", 
         alpha = "Method",
         title = "Between-Methods Comparison of July 2020 Kin Counts") + 
  theme_bw()