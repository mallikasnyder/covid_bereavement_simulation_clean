#Setting directories and loading functions
setwd("~/covid_bereavement_simulation_clean/")

#Source functions and packages
source("~/covid_bereavement_simulation_clean/Kin_death/01_load_functions.R")
source('~/covid_bereavement_simulation_clean/Kin_death/functions_bereavement.R')

#List all files in the directory
allfiles <- grep(list.files("~/covid_bereavement_simulation_clean/Data/Sweden_female_data", full.names = T), pattern = "female", value = T )[-1]

#Load all files and put them into a single object
final_data <- lapply(1:length(allfiles), function(x) {
  try(load(file = allfiles[x]))
  output <- countdata
  return(output)
})

#flat_data <- do.call(c, final_data)
#female_kin <- bind_rows(flat_data[which(names(flat_data) == "kin_bb")])
female_kin <- bind_rows(final_data)

kin_counts_female <- female_kin %>%
  mutate(age = gsub("f1", "", category)) %>%
  group_by(scenario, kintype, age) %>%
  summarize(mean_kin_pre = mean(mean_pre, na.rm = T),
            mean_kin_post = mean(mean_post, na.rm = T),
            sd_kin_pre = sd(mean_pre, na.rm = T),
            sd_kin_post = sd(mean_post, na.rm = T),
            ci_upper_kin_post = mean_kin_post + qnorm(0.975)*sd_kin_post,
            ci_lower_kin_post = mean_kin_post - qnorm(0.975)*sd_kin_post) %>%
  pivot_wider(id_cols = c("kintype", "age"), names_from = "scenario", 
              values_from = c("mean_kin_pre", "mean_kin_post", 
                              "sd_kin_pre", "sd_kin_post", 
                              "ci_upper_kin_post", "ci_lower_kin_post")) %>%
  mutate(abs_diff_pre = mean_kin_pre_covid - mean_kin_pre_other, 
         rel_diff_pre = (mean_kin_pre_covid - mean_kin_pre_other)/mean_kin_pre_other,
         abs_diff_post = mean_kin_post_covid - mean_kin_post_other, 
         rel_diff_post = (mean_kin_post_covid - mean_kin_post_other)/mean_kin_post_other)

write_csv(kin_counts_female, "~/covid_bereavement_simulation_clean/Data/SWE_sim_female_counts_final.csv")

