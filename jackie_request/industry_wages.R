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
library(conflicted)
library(janitor)
conflicts_prefer(dplyr::filter)
# constants---------------
last_full_year <- 2023

# load data--------------------------------
mapping <- vroom(here("mapping_files", "four_digit_naics_to_agg_industry.csv")) %>%
  bind_rows(tibble(naics = NA, aggregate_industry = "total,_all_industries"))

hrlywages <- vroom(here("jackie_request", list.files(here("jackie_request"), pattern = "HrlyWages_2125")))

hrlywages%>%
  filter(is.na(NAICS_5) | NAICS_5!="missi")%>%
  mutate(naics = as.numeric(NAICS_5)) %>%
  select(-NAICS_5)%>%
  left_join(mapping)%>%
  select(-naics) %>%
  clean_names()|>
  group_by(syear, aggregate_industry) %>%
  summarize(average_wage = round(weighted.mean(hrlyearn_num_mean, w=hrlyearn_num_count, na.rm=TRUE), digits=2))|>
  filter(syear==last_full_year)|>
  mutate(aggregate_industry=str_replace_all(aggregate_industry, "_", " "),
         aggregate_industry=str_to_title(aggregate_industry))|>
  write_csv(here("jackie_request","industry_wages.csv"))






