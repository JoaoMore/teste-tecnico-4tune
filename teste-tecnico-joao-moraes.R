# Pacotes e leitura de dados ------------------------------------------------------
library(tidyverse)
library(ggthemes)
theme_set(theme_minimal() + 
            theme(legend.position = 'bottom', 
                  legend.text = element_text(size = 12),
                  axis.text = element_text(size = 11, colour = 'gray40'), 
                  axis.title = element_text(size = 12),
                  plot.title = element_text(size = 16, face = 'bold'),
                  plot.subtitle = element_text(size = 12, color = 'gray30')))

retire <- read_csv('data/data.csv')
retire$...1 <- NULL

# Functions ---------------------------------------------------------------

income_ranges <- function(ranges, breaks = 2) {
  
  # This function creates a vector os ranges that will be used to define the 
  # saver's match rate for each individual based on phase-out ranges
  
  range = (ranges[2] - ranges[1])/breaks
  
  breaks <- c(0, seq(ranges[1], ranges[2], by = range), Inf)
  
  return(breaks)
  
}

calculate_match_capital <- function(initial_age, initial_income, match, annual_contribuition) {
  
  # This function calculates the total amount that someone will have saved over the years of working based on 
  # their age when they started working, starting salary, annual retirement account contribution, and percentage 
  # of contribution the government will match based on their salary and income tax filing type
  
  # inflation, fixed
  inflation = 4.4/100
  
  # remaining years to retire, based on current age and retirement age 
  remaining_years <- 65 - initial_age
  
  # index to adjust initial income by inflation over the years
  i <- 0:remaining_years
  
  adjusted_income <- initial_income*((1+inflation)^(i))
  
  # annual contributions based on adjusted income and annual_contribution
  contribuitions <- adjusted_income*annual_contribuition
  
  # additional savings per year based on saver's match percentage and annual contribution to 
  # retirement accounts, if the annual contribution exceeds 10K usd, then the saving saver's 
  # match will be capped at 10k x saver's match percentage
  
  savings <- ifelse(contribuitions<10000, contribuitions*match, match*10000)
  
  # Since the saver's match program will only be implemented in 2027, there are no values for the first 7 years
  savings[1:7] <- 0
  
  # total amount of savings
  return(sum(savings))
  
}

# additional info ---------------------------------------------------------
age_cohoorts <- c(35,40,45,50,55,60,65)

# Phase-out ranges for saver's match

single <- c(20500, 35500)
married <- c(41000, 71000)
head_household <- c(30750, 53250)

#  ---------------------------------------------------------------------

# Creating new variable that will be used to calculate matching contribution
retire <- retire %>% 
  mutate(family_kind_declaration = case_when(
    family_kind == 1 & marital_status == 1 ~ 'couple', #married filling jointly
    family_kind %in% c(2,3) & marital_status == 1 ~ 'head_household', #head of household 
    TRUE ~ 'single' #singles
  ))

# Matching contribution ratings. These ratings were based on saver's credit ratings
labs <-  c(50, 20, 10, 0)

# Matching contribution ratings calculation, based on income and IRS filling type
retire <- retire %>% 
  mutate(annual_savings = income * annual_contribution) %>% 
  mutate(match_percent = case_when(
    family_kind_declaration == 'single' ~ cut(income, breaks = income_ranges(single), labels = labs),
    family_kind_declaration == 'couple' ~ cut(income, breaks = income_ranges(married), labels = labs),
    family_kind_declaration == 'head_household' ~ cut(income, breaks = income_ranges(head_household), labels = labs)
  ),
  match_percent = (match_percent %>% paste() %>% as.numeric())/100)

# Calculating new savings due to saver's match

new_savings <- pmap(list(retire$initial_age,
                         retire$income,
                         retire$match_percent, 
                         retire$annual_contribution),
                    calculate_match_capital) %>% 
  unlist()

retire$additional_savings <- new_savings

retire <- retire %>% 
  mutate(new_accumulated_capital = accumulated_capital + additional_savings)

# Age cohoorts ------------------------------------------------------------

retire <- retire %>% 
  mutate(age_cohoort = cut(initial_age, breaks = age_cohoorts, right = F,
                           labels = c('[35,39)', '[40,44)', '[45,49)',
                                      '[50,54)', '[55,59)', '[60,64)')))


# Binary variable indicating whether someone afforded retirement or not ------

retire <- retire %>% 
  mutate(did_retire = ifelse(new_accumulated_capital >= 0, 1, 0))

# Retirement Savings Shortfall --------------------------------------------

# The Retirement Savings Shortfall is defined as:
#   Sum of the accumulate of all the persons who had deficits / Total number of persons

shortfall <- retire %>% 
  group_by(age_cohoort, race, did_retire) %>% 
  summarise(total_deficit = sum(new_accumulated_capital),
            n = sum(weight)) %>% 
  mutate(total_deficit = ifelse(did_retire == 1, 0, total_deficit)) %>% 
  summarise(total_deficit = sum(total_deficit),
            n_people = sum(n)) %>% 
  ungroup() %>% 
  mutate(shortfall = total_deficit/n_people)

shortfall_plot <- shortfall %>% 
  ggplot(aes(age_cohoort, -shortfall, fill = factor(race))) +
  geom_col(position = 'dodge', color = 'white') +
  labs(title = 'Retirement Savings Shortfall, by age cohoort and race', 
       subtitle = 'In 4 of 5 age groups, Hispanics have the highest retirement savings shortfall',
       fill = 'Race', x = 'Age cohoort', y = 'Shortfall ($/person)') +
  scale_fill_manual(labels = c('White', 'Black', 'Hispanic', 'Other'),
                      values = c('#264253', '#2A9D8F', '#F4A261', '#E76F71')) +
  scale_y_continuous(labels = scales::number_format(prefix = '-')) 

shortfall_plot

# Retirement Readiness Rating ---------------------------------------------

# The Retirement Readiness Rating is defined as:
#   Number of persons who were able to afford its retirement / Total number of persons

# readiness in old scenario
readiness_before <- retire %>% 
  mutate(retired = ifelse(accumulated_capital >= 0, 1, 0)) %>% 
  arrange(age_cohoort) %>% 
  select(age_cohoort, retired, weight) %>% 
  mutate(afforded_before = ifelse(retired == 1, weight, 0)) %>% 
  group_by(age_cohoort) %>% 
  summarise(afforded_before = sum(afforded_before),
            n_people = sum(weight)) %>% 
  mutate(readiness_1 = afforded_before/n_people) %>% 
  select(age_cohoort, readiness_1)


# readiness in new_scenario
readiness_new <- retire %>% 
  arrange(age_cohoort) %>% 
  select(age_cohoort, did_retire, weight) %>% 
  mutate(afforded = ifelse(did_retire == 1, weight, 0)) %>% 
  group_by(age_cohoort) %>% 
  summarise(afforded = sum(afforded),
            n_people = sum(weight)) %>% 
  mutate(readiness_2 = afforded/n_people) %>% 
  select(age_cohoort, readiness_2)

readiness_total <- left_join(readiness_before, readiness_new)

readiness_total <- readiness_total %>% 
  mutate(impact = 100*(readiness_2-readiness_1)/readiness_1)

readiness_plot <- readiness_total %>% 
  ggplot(aes(age_cohoort, impact)) + 
  geom_col(fill = 'royalblue4', color = 'white') +
  geom_text(aes(label = scales::percent(impact, scale = 1)), 
            vjust = -1, fontface = 'bold', color = 'gray30') +
  labs(title = 'Impact on Retirement Readiness Rating, by age cohoorts',
       subtitle = 'Percentage difference from scenario without Saver\'s Match',
       x = 'Age Cohoort', y = 'Impact') +
  scale_y_continuous(labels = scales::number_format(suffix = '%'), limits = c(0,8))


# New scenario dataset ----------------------------------------------------

new_scenario <- retire %>% 
  select(initial_age, weight, family_kind, race, marital_status, accumulated_capital, 
         annual_contribution, income, new_accumulated_capital)

write_csv(new_scenario, file = 'output/new_scenario.csv')

# Saving plots -----------------------------------------------------------

ggsave(filename = 'output/shortfall.png', plot = shortfall_plot, 
       width = 9, height = 5, scale = 1.5, bg = 'white')

ggsave(filename = 'output/readiness.png', plot = readiness_plot, 
       width = 9, height = 5, scale = 1.5, bg = 'white')
