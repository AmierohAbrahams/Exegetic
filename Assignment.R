# Loading libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggrepel)

# Loading the data found online 
Smoking_prev <- read_csv("IHME_GBD_2015_SMOKING_PREVALENCE_1980_2015_Y2017M04D05.CSV")

# Exploring the dataset
summary(Smoking_prev)
names(Smoking_prev)
unique(Smoking_prev$location_name) #225 countries + 'Global' + 5 levels of 'SDI'
unique(Smoking_prev$year_id)

#age_group_id '22' == age_group "All Ages"
#age_group_id '27' == age_group "Age-standardized"

# Next Steps:
# The dataset has means for every year (for both sexes),
# Work out the PERCENTAGE-POINT rate of change over time (i.e. 1990 - 2015),
# Plot the females change against the males, for every country 


# Editing the dataset to only have the useful variables and observations
smok_prev_comp <- Smoking_prev %>% 
  filter(age_group_id == 27, year_id %in% c(1990, 2015), metric == "Percent", sex != "Both") %>% 
  select("location_name", "sex", "year_id", "mean")


#Separate the males and females, to calculate percentage change separately
# then re-join them using location as common variable
smok_prev_males <- smok_prev_comp %>% 
  filter(sex == "Male") %>% 
  mutate(change = mean - lag(mean, default = first(mean))) %>% 
  mutate(perc_change = change*100) %>%
  filter(year_id == 2015) %>% 
  select(location_name, perc_change) %>% 
  rename(male_change = perc_change)
  
smok_prev_females <- smok_prev_comp %>% 
  filter(sex == "Female") %>% 
  mutate(change = mean - lag(mean, default = first(mean))) %>%
  mutate(perc_change = change*100) %>% 
  filter(year_id == 2015) %>% 
  select(location_name, perc_change) %>% 
  rename(female_change = perc_change)


# Rejoin our datasets and work out which quadrant each observation falls in
smok_prev_final <- left_join(smok_prev_males, smok_prev_females) %>% 
  mutate(quadrant = case_when(male_change > 0 & female_change > 0 ~ "Q1",
                              male_change < 0 & female_change > 0 ~ "Q2",
                              male_change < 0 & female_change < 0 ~ "Q3",
                              male_change > 0 & female_change < 0 ~ "Q4"))


# Now, create the list of country names that are displayed on the final plot
# South Korea, Turkey, Belarus, Portugal, Greece, Bulgaria, Russia, Kuwait, Timor-Leste, 
# Chile, Macedonia, Cyprus, Indonesia, Azerbijan, Montenegro, Saudi Arabia, Tonga, India, 
# Japan, China, France, Britain, United States, Nepal, Brazil, Sweden, Canada, Norway, Iceland, Denmark

smok_prev_final_names <- smok_prev_final %>% 
  mutate(label = if_else(location_name %in% c("South Korea", "Turkey", "Belarus", "Portugal", "Greece", "Bulgaria", 
                                              "Russia", "Kuwait", "Timor-Leste", "Chile", "Macedonia", "Cyprus", "Indonesia", "Azerbijan",
                                              "Montenegro", "Saudi Arabia", "Tonga", "India", "Japan", "China", "France", "Britain", "United States",
                                              "Nepal", "Brazil", "Sweden", "Canada", "Norway", "Iceland", "Denmark"),"yes","no"))

# Finally, create the plot
ggplot(smok_prev_final_names, aes(male_change, female_change, colour = quadrant)) +
  geom_point(size = 4, alpha = 0.8) + 
  scale_color_manual(values = c("darkred", "steelblue3", "lightskyblue1", "tomato2")) + 
  geom_hline(yintercept = 0, size = 0.7) +
  geom_vline(xintercept = 0, size = 0.7) +
  geom_text_repel(aes(label = if_else(label == "yes",location_name,"")), col = "black", size = 3) +
  theme_bw() +
  labs(x = expression(italic("Change in male rate")), y = expression(italic("Change in female rate")), title = "What a drag", subtitle = "Daily smoking prevalence, 1990-2015, percentage-point change") +
  theme(panel.border = element_blank(), legend.position = "none", panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"), axis.ticks = element_blank()) + 
  scale_x_continuous(breaks = seq(from = -25, to = 10, by = 5)) + 
  scale_y_continuous(breaks = seq(from = -20, to = 10, by = 5)) +
  annotate("text", label = "Female increase\nmale decrease", x = -21, y = 8, size = 3.5, col = "steelblue3", fontface = 2) +
  annotate("text", label = "Female decrease\nmale decrease", x = -21, y = -15, size = 3.5, col = "lightskyblue1", fontface = 2) +
  annotate("text", label = "Female decrease\nmale increase", x = 7, y = -17, size = 3.5, col = "tomato2", fontface = 2) +
  annotate("text", label = "Female increase\nmale increase", x = 7, y = 7, size = 3.5, col = "darkred", fontface = 2)


# Questions

# What was the most challenging part of this exercise?
# Working out how to calculate rate of change. By Obtaining more experience with the lag() function,
# in order to perform a calculation between two different observations.

# What took you the longest?
#Figuring out which data were useful. The dataset had variables that didn't affect the plot, so selecting only useful columns took some time to figure out. 

# What did you enjoy the most?
# Creating the plot. I typed out the code for creating a ggplot without any hesitation and errors and saw that my plotted
# points looked like those on the original figure. In truth, that was a very satisfying moment.

# What did you enjoy the least?
# Looking at the original data. It was quite daunting viewing the data at a glance, and the graph I 
# had to recreate  with it. In hindsight, though, once I was able to systematically break down the assignment into steps, I found the task to be very simple.   
