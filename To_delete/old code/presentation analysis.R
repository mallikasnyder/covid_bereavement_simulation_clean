setwd("~/covid_simulation/")

#Source functions and packages
source("~/covid_simulation/Kin_death/01_load_functions.R")
source('~/covid_simulation/Kin_death/functions_bereavement.R')

#Directory stems
dir_stem <- c("covid", "other")

#Keep countries (for partial results)
country_partial <- c("United_Kingdom", "Germany", "France", "Sweden", "United_States_of_America")

#Find details for all simulations
list_countries <- lapply(dir_stem, function(y) {
  scenario <- y
  filepath <- paste0("~/90days/", y, "RESULTS/", "SocCovid-mallikasnyder/")
  countries <- grep(list.files(filepath), pattern = paste0(country_partial, collapse = "|"), value = T)
  countryvec <- lapply(countries, function(x) {
    simnames <- grep(pattern = ".opop", 
                     list.files(paste0(filepath, x, "/")), 
                     invert = T, value = T)
    scenarionames = rep(scenario, length(simnames))
    countrynames = rep(gsub(":Medium.sup", "", x), length(simnames))
    
    output = list(sim = simnames, scenario = scenarionames, country = countrynames)
    return(output)
  })
}
)

#Flatten this list
flat_countries <- do.call(c, do.call(c, list_countries))

#Create vectors for all sims, all countries, and all scenarios
allsims <- unlist(flat_countries[which(names(flat_countries) == "sim")], use.names = F)
allcountries <- unlist(flat_countries[which(names(flat_countries) == "country")], use.names = F) 
allscenarios <- unlist(flat_countries[which(names(flat_countries) == "scenario")], use.names = F) 

allfiles <- paste0("~/covid_simulation/Data/sim_results_new/data", allsims, ".RData")

salvaged <- lapply(1:length(allfiles), function(x) {
  try(load(file = allfiles[x]))
  output <- list(kin_bm = data$kin_bm, kin_bb = data$kin_bb, death_rates = data$death_rates)
  return(output)
})


flattened <- do.call(c, salvaged)

data_bm <- bind_rows(flattened[which(names(flattened) == "kin_bm")])
data_bb <- bind_rows(flattened[which(names(flattened) == "kin_bb")])

#Re-adding country and scenario names, and filtering
data_bm <- data_bm %>%
  ungroup() %>%
  mutate(country = allcountries[match(sim.id, allsims)],
         scenario = allscenarios[match(sim.id, allsims)]) %>%
  group_by(country, scenario) %>%
  mutate(group = as.integer(factor(sim.id))) %>%
  ungroup() %>%
  filter(group <= 50)

data_bb <- data_bb %>%
  ungroup() %>%
  mutate(country = allcountries[match(sim.id, allsims)],
         scenario = allscenarios[match(sim.id, allsims)]) %>%
  group_by(country, scenario) %>%
  mutate(group = as.integer(factor(sim.id))) %>%
  ungroup() %>%
  filter(group <= 50)

data_death_rates <- bind_rows(flattened[which(names(flattened) == "death_rates")])

data_death_rates <- data_death_rates %>%
  ungroup() %>%
  mutate(country = allcountries[match(sim.id, allsims)],
         scenario = allscenarios[match(sim.id, allsims)]) %>%
  group_by(country, scenario) %>%
  mutate(group = as.integer(factor(sim.id))) %>%
  ungroup() %>%
  filter(group <= 50)

presentation <- list(death_rates = data_death_rates, kin_bb = data_bb, kin_bm = data_bm)

#Save these datasets
save(presentation, 
     file = "~/covid_simulation/Output/presentation_data.RData" )

#Changes in death rates

data_death_rates %>%
  mutate(age2 = as.numeric(as.factor(age))) %>%
  filter(age2 <=5) %>%
  mutate(fem2 = if_else(fem == 1, "Female", "Male"),
         country = if_else(country == "United_Kingdom", "UK", 
                           if_else(country =="United_States_of_America", "USA", country)),
         scenario = if_else(scenario == "covid", "Covid-19", "Other")) %>%
  group_by(country, scenario, fem2, age) %>%
  ggplot() + geom_boxplot(aes(x = age, y = value, color = scenario)) +
  facet_grid(rows = vars(country), cols = vars(fem2)) + 
  labs(x = "Age group", y = "Mean ASMR (per 1000)", color = "Scenario", 
       title = "Mean Death Rates across Simulations")

ggsave(file = "~/covid_simulation/Output/death_rates.png")

#Average number of kin

data_bm %>%
  separate(category, into = c("age", "fem"), sep = "f") %>%
  mutate(fem2 = if_else(fem == 1, "Female", "Male"),
         country = gsub("_", " ", country),
         scenario = if_else(scenario == "covid", "Covid-19", "Other"),
         kintype = if_else(kintype == "gparents", "Grandparents", 
                  if_else(kintype == "parents", "Parents",
                  if_else(kintype == "nuclear", "Nuclear",
                  if_else(kintype == "extended", "Extended", kintype))))) %>%
  filter(scenario == "Covid-19", kintype %in% c("Nuclear", "Extended", "Grandparents", "Parents")) %>%
  mutate(kintype = ordered(as.factor(kintype), 
                           levels = c("Nuclear", "Extended", "Grandparents", "Parents"))) %>%
  group_by(country, kintype, age) %>%
  ggplot() + geom_boxplot(aes(x = age, y = mean, color = country)) +
  facet_grid(rows = vars(kintype)) + 
  labs(x = "Age Group", y = "Mean Number of Kin", color = "Country",
       title = "Bereavement Multiplier for Covid-19 (Both Sexes)")

ggsave(file = "~/covid_simulation/Output/bereavement_multiplier.png")


#Burden of bereavement differences

data_bb2 <- data_bb %>%
  separate(category, into = c("age", "sex"), sep = "f") %>%
  mutate(sex = if_else(sex == "1", "Female", "Male"),
         country = if_else(country == "United_States_of_America", "USA", 
                                  if_else(country == "United_Kingdom", "UK", country)),
         country = gsub("_", " ", country),
         scenario = if_else(scenario == "covid", "COVID-19", "Counterfactual"),
         kintype = if_else(kintype == "gparents", "Grandparents", 
                    if_else(kintype == "gunclesaunts", "Grand-Aunts/Uncles", 
                    if_else(kintype == "parents", "Parents",
                    if_else(kintype == "unclesaunts", "Aunts/Uncles", 
                    if_else(kintype == "siblings", "Siblings", 
                    if_else(kintype == "firstcousins", "First Cousins", 
                    if_else(kintype == "children", "Children",
                    if_else(kintype == "spouse", "Spouse",
                    if_else(kintype == "parentsofspouse", "Parents-in-Law", 
                    if_else(kintype == "nuclear", "Nuclear",
                    if_else(kintype == "extended", "Extended", kintype))))))))))),
        difference = mean_post - mean_pre,
        difference_relative = 1- (mean_post/mean_pre),
        difference_relative = ifelse(is.nan(difference_relative), 0, difference_relative))


data_bb2 %>% 
  filter(kintype %in% c("Nuclear", "Extended", "Grandparents", "Parents"), country == "USA") %>%
  mutate(kintype = ordered(as.factor(kintype), 
                           levels = c("Nuclear", "Extended", "Grandparents", "Parents"))) %>% 
  group_by(scenario, age, kintype) %>%
  ggplot() + 
  geom_boxplot(
    aes(x = age, y = difference_relative, color = scenario), outlier.shape = NA) +
  facet_wrap(~kintype) +
  labs(x = "Age Group", y = "Reduction Kin (%)", 
       color = "Scenario", title = "Reduction in Kin Alive Pre and Post Covid-19 (%)") +
  coord_cartesian(ylim = c(0, .25))
  
ggsave(file = "~/covid_simulation/Output/burden_of_bereavement_usa.png")


#Finally, make the STMF adjustment factors graph
load("~/covid_simulation/STMF_factor_latest_sim.RData")

factors <- covidfactor[which(attr(covidfactor, "country") %in% country_partial)]
ages <- attr(covidfactor, "age")[which(attr(covidfactor, "country") %in% country_partial)]
sexes <- attr(covidfactor, "sex")[which(attr(covidfactor, "country") %in% country_partial)]
months <- attr(covidfactor, "month")[which(attr(covidfactor, "country") %in% country_partial)]
countries <- attr(covidfactor, "country")[which(attr(covidfactor, "country") %in% country_partial)]

stmf <- tibble(mx_ratio = factors, age = ages, sex = sexes, month = months, country = countries)

stmf2 <- stmf %>%
  mutate(country = gsub("_", " ", country),
         age_int = cut(age, breaks = c(0, 14, 64, 74, 84, Inf), 
                       labels = c("0-14", "15-64", "65-74", "75-84", "85+")))
stmf2 %>%
  group_by(country, month, sex, age_int) %>%
  summarize(mx_ratio = mean(mx_ratio)) %>%
  ggplot() + geom_line(aes(x = month, y = mx_ratio, color = country)) +
  facet_grid(rows = vars(age_int), cols = vars(sex)) +
  labs(x = "Month", y = "Ratio of Observed to Expected mx Values", color = "Country",
       title = "Excess Mortality Adjustment Factors")

ggsave(file = "~/covid_simulation/Output/adjustment_factors_presentation.png")



