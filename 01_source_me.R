# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
# libraries----------------
library(tidyverse)
library(vroom)
library(here)
# constants---------------
last_full_year <- 2021
previous_year <- last_full_year - 1
# functions---------------
total_employment_year <- function(tbbl, year) {
  tbbl %>%
    filter(
      syear %in% year,
      is.na(agegrp),
      is.na(sex)
    ) %>%
    mutate(count = round(count, -2)) %>%
    pivot_wider(
      names_from = syear,
      values_from = count,
      names_prefix = "total_employment_"
    )
}

age_percentages <- function(ages, prefix) {
  agg_emp_naics %>%
    filter(
      syear %in% c((last_full_year - 5), last_full_year),
      is.na(sex),
      agegrp %in% c(ages, NA)
    ) %>%
    group_by(syear, in_age_group = !is.na(agegrp), aggregate_industry) %>%
    summarize(count = sum(count)) %>%
    pivot_wider(names_from = in_age_group, values_from = count, names_prefix = "in_age_group_") %>%
    mutate(percent_in_age_group = scales::percent(in_age_group_TRUE / in_age_group_FALSE, accuracy = 1)) %>%
    ungroup() %>%
    select(syear, aggregate_industry, percent_in_age_group) %>%
    pivot_wider(names_from = syear, values_from = percent_in_age_group, names_prefix = prefix)
}

load_clean_aggregate <- function(pat) {
  temp <- vroom(here("data", list.files(here("data"), pattern = pat))) %>%
    mutate(naics = as.numeric(NAICS_5)) %>%
    select(-NAICS_5) %>%
    wrapR::clean_tbbl()%>%
    full_join(mapping) %>%
    select(-naics) %>%
    group_by(across(c(-count))) %>%
    summarize(count = sum(count) / 12) %>%
    filter(!is.na(aggregate_industry))
}

percentage <- function(tbbl, var, value, quoted_value){
  tbbl%>%
    filter(syear %in% c(last_full_year, (last_full_year-5)))%>%
    pivot_wider(names_from = {{  var  }}, values_from = count)%>%
    mutate(percent=scales::percent({{  value  }}/`NA`, accuracy = .1))%>% #for RTRA data NA indicates the aggregate of all levels.
    select(syear, aggregate_industry, percent)%>%
    pivot_wider(names_from = syear, values_from = percent, names_prefix= paste0(quoted_value,"_"))
}

# load data--------------------------------
mapping <- vroom(here("data", "four_digit_naics_to_agg_industry.csv")) %>%
  bind_rows(tibble(naics = NA, aggregate_industry = "total,_all_industries"))

agg_emp_naics <- load_clean_aggregate(pat = "EMP_NAICS") %>%
  mutate(sex = factor(sex, levels = c(1, 2), labels = c("male", "female")))

ftpt <- load_clean_aggregate(pat = "ftpt")
cow <- load_clean_aggregate(pat = "COW")
permtemp <- load_clean_aggregate(pat = "permtemp")
size <- load_clean_aggregate(pat = "size")
status <- load_clean_aggregate(pat = "lfsstat")
hrlywages <- vroom(here("data", list.files(here("data"), pattern = "HrlyWages")))

#process the data---------------------
industry_overview <- total_employment_year(agg_emp_naics, (last_full_year - 1):last_full_year) %>%
  rename(
    current_employment = contains(as.character(last_full_year)),
    previous_employment = contains(as.character(previous_year))
  ) %>%
  mutate(
    yoy_change = scales::comma(current_employment - previous_employment),
    yoy_growth = scales::percent(current_employment / previous_employment - 1, accuracy = .1)
  ) %>%
  ungroup() %>%
  select(aggregate_industry, yoy_growth, yoy_change, current_employment)

industry_gender <- agg_emp_naics %>%
  filter(
    syear == last_full_year,
    is.na(agegrp),
    !is.na(sex)
  ) %>%
  pivot_wider(names_from = sex, values_from = count) %>%
  mutate(
    men = scales::percent(male / (male + female), accuracy = 1),
    women = scales::percent(female / (male + female), accuracy = 1)
  ) %>%
  ungroup() %>%
  select(aggregate_industry, men, women)

industry_young <- age_percentages("between15and24", "percent_young_")
industry_old <- age_percentages(c("between55and64", "65andover"), "percent_old_")
industry_part_time <- percentage(ftpt, ftpt, part_time, "part-time")
industry_self_employed <- percentage(cow, class, self_employed, "self_employed")
industry_private <- percentage(cow, class, private_employe, "private_sector")
industry_temporary <- percentage(permtemp, temp, temporary, "temporary")
industry_size <- percentage(size, size, less_than_20_employees, "small")
industry_unemployment <- percentage(status, lf_stat, unemployed, "unemployed")

industry_gender_wages <- hrlywages%>%
  mutate(naics = as.numeric(NAICS_5)) %>%
  select(-NAICS_5) %>%
  wrapR::clean_tbbl()%>%
  full_join(mapping) %>%
  select(-naics) %>%
  group_by(syear, gender, aggregate_industry) %>%
  summarize(average_wage = scales::dollar(weighted.mean(hrlyearn_num_mean, w=hrlyearn_num_count, na.rm=TRUE), accuracy = .01)) %>%
  filter(!is.na(aggregate_industry),
         syear %in% c(last_full_year, (last_full_year-5)),
         !is.na(gender)
         )%>%
  pivot_wider(names_from = gender, values_from = average_wage)%>%
  pivot_wider(names_from = syear, values_from = c("female","male"), names_prefix = "average_wage_")%>%
  select(aggregate_industry, starts_with("male"), everything())


