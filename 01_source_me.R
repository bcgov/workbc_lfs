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
library(XLConnect)
# constants---------------
last_full_year <- 2021
previous_year <- last_full_year - 1
# functions---------------
source(here("R","functions.R"))
# load data--------------------------------
mapping <- vroom(here("mapping_files", "four_digit_naics_to_agg_industry.csv")) %>%
  bind_rows(tibble(naics = NA, aggregate_industry = "total,_all_industries"))
gvs_mapping <- vroom(here("mapping_files", "naics_to_gvs.csv")) #goods vs services

agg_emp_naics <- load_clean_aggregate(pat = "EMP_NAICS") %>%
  mutate(sex = factor(sex, levels = c(1, 2), labels = c("male", "female")))
ftpt <- load_clean_aggregate(pat = "empftpt_naics")
cow <- load_clean_aggregate(pat = "COW")
permtemp <- load_clean_aggregate(pat = "permtemp")
size <- load_clean_aggregate(pat = "size")
status <- load_clean_aggregate(pat = "lfsstat")
region <- load_clean_aggregate(pat = "EMP_REGION")

hrlywages <- vroom(here("data", list.files(here("data"), pattern = "HrlyWages")))
youthwages <- vroom(here("data", list.files(here("data"), pattern = "wages_youth")))
reg_stat <- vroom(here("data", list.files(here("data"), pattern = "EMP_REG_STAT")))
reg_ft <- vroom(here("data", list.files(here("data"), pattern = "ftpt_region")))
region_gvs <- vroom(here("data", list.files(here("data"), pattern = "EMP_REGION")))

#industry profiles---------------------
industry_overview <- total_employment_year(agg_emp_naics, (last_full_year - 1):last_full_year) %>%
  rename(
    current_employment = contains(as.character(last_full_year)),
    previous_employment = contains(as.character(previous_year))
  ) %>%
  mutate(
    yoy_change = scales::comma(current_employment - previous_employment),
    yoy_growth = scales::percent(current_employment / previous_employment - 1, accuracy = .1),
    current_employment =scales::comma(current_employment)
  ) %>%
  ungroup() %>%
  select(aggregate_industry, yoy_growth, yoy_change, current_employment)

industry_overview <- agg_emp_naics %>%
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
  select(aggregate_industry, men, women)%>%
  full_join(industry_overview)

industry_unemployment <- status%>%
  filter(syear %in% c(last_full_year, (last_full_year-5)))%>%
  pivot_wider(names_from = lf_stat, values_from = count)%>%
  mutate(percent=scales::percent(unemployed/(employed+unemployed), accuracy = .1))%>%
  select(syear, aggregate_industry, percent)%>%
  pivot_wider(names_from = syear, values_from = percent, names_prefix= paste0("unemployment","_"))
colnames(industry_unemployment) <- str_replace(colnames(industry_unemployment), as.character(last_full_year), "current")
colnames(industry_unemployment) <- str_replace(colnames(industry_unemployment), as.character(last_full_year-5), "past")

industry_overview <- full_join(industry_overview, age_percentages("between15and24", "percent_young_"))%>%
full_join(age_percentages(c("between55and64", "65andover"), "percent_old_"))%>%
full_join(percentage(ftpt, ftpt, part_time, "part-time"))%>%
full_join(percentage(cow, class, self_employed, "self_employed"))%>%
full_join(percentage(cow, class, private_employe, "private_sector"))%>%
full_join(percentage(permtemp, temp, temporary, "temporary"))%>%
full_join(percentage(size, size, less_than_20_employees, "small"))%>%
full_join(industry_unemployment)

industry_wages <- hrlywages%>%
  filter(is.na(NAICS_5) | NAICS_5!="missi")%>%
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

colnames(industry_wages) <- str_replace(colnames(industry_wages), as.character(last_full_year), "current")
colnames(industry_wages) <- str_replace(colnames(industry_wages), as.character(last_full_year-5), "past")

industry_overview <- industry_wages%>%
  full_join(industry_overview)

industry_youth_wages <- youthwages%>%
  filter(is.na(NAICS_5) | NAICS_5!="missi")%>%
  mutate(naics = as.numeric(NAICS_5))%>%
  select(-NAICS_5) %>%
  wrapR::clean_tbbl()%>%
  full_join(mapping) %>%
  select(-naics)%>%
  filter(is.na(age),
         is.na(gender))%>%
  group_by(syear, aggregate_industry) %>%
  summarize(average_wage = scales::dollar(weighted.mean(hrlyearn_num_mean, w=hrlyearn_num_count, na.rm=TRUE), accuracy = .01)) %>%
  filter(!is.na(aggregate_industry),
         syear %in% c(last_full_year, (last_full_year-5))
  )%>%
  pivot_wider(names_from = syear, values_from = average_wage, names_prefix = "youth_wages_")

colnames(industry_youth_wages) <- str_replace(colnames(industry_youth_wages), as.character(last_full_year), "current")
colnames(industry_youth_wages) <- str_replace(colnames(industry_youth_wages), as.character(last_full_year-5), "past")

industry_overview <- industry_youth_wages%>%
  full_join(industry_overview)

industry_overview <- region%>%
  filter(syear==last_full_year)%>%
  pivot_wider(names_from = region, values_from = count)%>%
  mutate(north_coast_nechako=north_coast+nechako, .after="lower_mainland_southwest")%>%
  ungroup()%>%
  select(-syear)%>%
  mutate(across(where(is.numeric), ~ scales::percent(.x/`NA`, accuracy = .1)))%>%
  select(-`NA`, -north_coast, -nechako)%>%
  full_join(industry_overview)

industry_cleaned <- industry_overview%>%
  select(aggregate_industry, yoy_growth, yoy_change, current_employment, men, women, percent_young_past, percent_young_current,
         percent_old_past, percent_old_current, `part-time_past`, `part-time_current`, self_employed_past, self_employed_current,
         temporary_past, temporary_current, small_past, small_current, private_sector_past, private_sector_current, unemployment_past,
         unemployment_current, male_average_wage_past, male_average_wage_current, female_average_wage_past, female_average_wage_current,
         youth_wages_past, youth_wages_current, cariboo, kootenay, lower_mainland_southwest, north_coast_nechako, northeast,
         thompson_okanagan, vancouver_island_and_coast)%>%
  wrapR::camel_to_title()%>%
  slice(16,1:15,17:19) #hacky way to put total (16th row) at top.

#regional profiles-----------------------------------
regional_population <- cansim::get_cansim("17-10-0137-01")%>%
  filter(!str_detect(`Age group`, "to"),
        !str_detect(`Age group`, "65 years and older"),
        !str_detect(`Age group`, "age"))%>%
  mutate(age= parse_number(as.character(`Age group`)))%>%
  select(-`Age group`)%>%
  janitor::clean_names()%>%
  filter(grepl('British Columbia', geo),
         ref_date==max(ref_date),
         sex=="Both sexes")%>%
  select(region=geo, age, value)%>%
  mutate(region=word(region, 1, sep = ","),
         region=case_when(region=="North Coast"~"North Coast and Nechako",
                                   region=="Nechako"~"North Coast and Nechako",
                                   TRUE~region))%>%
  group_by(region, age)%>%
  summarize(value=sum(value))%>%
  mutate(age_group = cut(age,
                      breaks = c(0, 15, 25, 55, 65, 200),
                      include.lowest = T,
                      right = F))%>%
  select(-age)%>%
  group_by(age_group, region)%>%
  summarize(value=sum(value))%>%
  pivot_wider(names_from = age_group, values_from = value, names_prefix = "in_age_group_")%>%
  janitor::adorn_percentages()%>%
  mutate(across(where(is.numeric), ~scales::percent(.x, accuracy = 1)))%>%
  wrapR::clean_tbbl()

#regional full-time rates----------------
regional_full_time <-reg_ft%>%
  wrapR::clean_tbbl()%>%
  filter(syear==last_full_year,
         is.na(naics_5))%>%
  mutate(region=case_when(is.na(region)~"british_columbia",
                          region=="north_coast"~"north_coast_and_nechako",
                          region=="nechako"~"north_coast_and_nechako",
                          TRUE~region))%>%
  group_by(syear, naics_5, region, ftpt_main)%>%
  summarize(count=sum(count)/12)%>%
  ungroup()%>%
  select(-naics_5, -syear)%>%
  pivot_wider(names_from = ftpt_main, values_from = count)%>%
  mutate(percent_full_time=scales::percent(full_time/`NA`, accuracy = 1),
         full_time=scales::comma(full_time, accuracy = 100)
         )%>%
  select(region, percent_full_time, full_time)

#regional unemployment rates-------------
regional_unemployment <- reg_stat%>%
  janitor::clean_names()%>%
  filter(syear<=last_full_year,
         is.na(naics_5),
         !is.na(syear))%>%
  select(-naics_5)%>%
  mutate(region=case_when(is.na(region)~"British Columbia",
                          region=="North Coast"~"North Coast and Nechako",
                            region=="Nechako"~"North Coast and Nechako",
                            TRUE~region))%>%
  group_by(syear, lf_stat, region)%>%
  summarize(count=sum(count))%>%
  pivot_wider(names_from = lf_stat, values_from = count)%>%
  mutate(unemployment_rate=Unemployed/(Employed+Unemployed))%>%
  select(syear, region, unemployment_rate)%>%
  wrapR::clean_tbbl()

urate_summaries <- regional_unemployment%>%
  group_by(region)%>%
  summarise(low=scales::percent(min(unemployment_rate), accuracy = .1),
            high=scales::percent(max(unemployment_rate), accuracy = .1),
            ave=scales::percent(mean(unemployment_rate), accuracy = .1),
            )

regional_unemployment <- regional_unemployment%>%
  mutate(unemployment_rate=scales::percent(unemployment_rate, accuracy = .1))%>%
  pivot_wider(names_from = syear, values_from = unemployment_rate, names_prefix = "urate_")%>%
  full_join(urate_summaries)

regional_profile_1 <- full_join(regional_population, regional_full_time)%>%
  full_join(regional_unemployment)%>%
  wrapR::camel_to_title()


# size of industry within a region---------------
regional_employment_by_industry <- region%>%
  filter(syear==last_full_year,
         aggregate_industry!="total,_all_industries")%>%
  mutate(region=case_when(is.na(region)~"british_columbia",
                          region=="north_coast"~"north_coast_and_nechako",
                          region=="nechako"~"north_coast_and_nechako",
                          TRUE~region))%>%
  group_by(region, aggregate_industry)%>%
  summarize(count=sum(count))%>%
  pivot_wider(names_from = aggregate_industry, values_from = count)%>%
  arrange(region)

regional_profile_2 <- regional_employment_by_industry%>%
  janitor::adorn_percentages("row")%>%
  mutate(across(where(is.numeric), ~scales::percent(.x, accuracy = .1)))%>%
  wrapR::camel_to_title()
colnames(regional_profile_2) <- str_to_title(str_replace_all(colnames(regional_profile_2), "_"," "))

# long_rbi <- regional_employment_by_industry%>%
#   pivot_longer(cols=-region, names_to = "industry", values_to = "employment")
#
# long_rbi_percent <- regional_profile_2%>%
#   pivot_longer(cols=-region, names_to = "industry", values_to = "percent of employment in region")
#
# for_meaghan <- full_join(long_rbi, long_rbi_percent)%>%
#   mutate(employment=scales::comma(employment, accuracy = 100))%>%
#   wrapR::camel_to_title()
# for_meaghan%>%
#   write_csv(here("out", "for_meaghan.csv"))

#size of region within an industry-------------------
regional_by_region <- region%>%
  filter(syear==last_full_year,
         !is.na(region),
         aggregate_industry!="total,_all_industries"
         )%>%
  mutate(region=case_when(region=="north_coast"~"north_coast_and_nechako",
                                   region=="nechako"~"north_coast_and_nechako",
                                   TRUE~region))%>%
  group_by(region, aggregate_industry)%>%
  summarize(count=sum(count))%>%
  pivot_wider(names_from = aggregate_industry, values_from = count)%>%
  janitor::adorn_percentages("col")%>%
  mutate(across(where(is.numeric), ~scales::percent(.x, accuracy = .1)))

#goods vs services--------------------
regional_goods_vs_services <- region_gvs%>%
  filter(is.na(NAICS_5) | NAICS_5!="missi")%>%
  mutate(naics=as.numeric(NAICS_5))%>%
  wrapR::clean_tbbl()%>%
  mutate(region=case_when(is.na(region)~"british_columbia",
                         region=="north_coast"~"north_coast_and_nechako",
                         region=="nechako"~"north_coast_and_nechako",
                         TRUE~region))%>%
  select(-naics_5)%>%
  full_join(gvs_mapping)%>%
  filter(syear==last_full_year)%>%
  group_by(region, goods_vs_services)%>%
  summarise(count=sum(count)/12)%>%
  pivot_wider(names_from = goods_vs_services, values_from = count)%>%
  mutate(percent_goods=scales::percent(goods/`NA`,accuracy = 1),
         percent_services=scales::percent(services/`NA`, accuracy = 1))%>%
  select(region, percent_goods, percent_services)

regional_profile_3 <- full_join(regional_by_region, regional_goods_vs_services)%>%
  arrange(region)%>%
  wrapR::camel_to_title()
colnames(regional_profile_3) <- str_to_title(str_replace_all(colnames(regional_profile_3), "_"," "))


#save data---------------------

wb <- loadWorkbook(here("output_template", "LFS Data Sheet.xlsx"))
write_workbook(industry_cleaned, "Industry Profiles", 5, 1, FALSE)
write_workbook(regional_profile_1, "Regional Profiles", 6, 1, FALSE)
write_workbook(regional_profile_2, "Regional Profiles", 19, 1, TRUE)
write_workbook(regional_profile_3, "Regional Profiles", 33, 1, TRUE)

saveWorkbook(wb, here(
  "out",
  paste0(last_full_year, "_LFS_data_sheet.xlsx")
))


