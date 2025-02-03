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
library(conflicted)
library(janitor)
conflicts_prefer(dplyr::filter)
# constants---------------
last_full_year <- 2024
previous_year <- last_full_year - 1
# functions---------------
source(here("R","functions.R"))
# load data--------------------------------
all_mapping <- readxl::read_excel(here("mapping_files", "industry_mapping_2025.xlsx"))

mapping <- all_mapping|>
  select(naics_5, aggregate_industry)
gvs_mapping <- all_mapping|>
  select(naics_5, goods_vs_services)

#DEAL WITH SEX/GENDER MISMATCH (sex pre 2020, gender post)

hrlywages_1620 <- vroom(here("data", list.files(here("data"), pattern = "HrlyWages_1620")))
old_column_names <- colnames(hrlywages_1620)
hrlywages_2125 <- vroom(here("data", list.files(here("data"), pattern = "HrlyWages_2125")))
colnames(hrlywages_2125) <- old_column_names
hrlywages <- bind_rows(hrlywages_1620, hrlywages_2125)%>%
  clean_names()|>
  mutate(sex = factor(sex, levels = c(1, 2), labels = c("male", "female")))

youthwages_1620 <- vroom(here("data", list.files(here("data"), pattern = "wages_youth1620")))
old_column_names <- colnames(youthwages_1620)
youthwages_2125 <- vroom(here("data", list.files(here("data"), pattern = "wages_youth2125")))
colnames(youthwages_2125) <- old_column_names
youthwages <- bind_rows(youthwages_1620, youthwages_2125)%>%
  clean_names()|>
  mutate(sex = factor(sex, levels = c(1, 2), labels = c("male", "female")))

emp_naics_1620 <- vroom(here("data", list.files(here("data"), pattern = "EMP_NAICS_1620")))
old_column_names <- colnames(emp_naics_1620)
emp_naics_2125 <- vroom(here("data", list.files(here("data"), pattern = "EMP_NAICS_2125")))
colnames(emp_naics_2125) <- old_column_names

agg_emp_naics <- bind_rows(emp_naics_1620, emp_naics_2125)|>
  clean_names()|>
  left_join(mapping, by=c("naics_5"="naics_5"))%>%
  select(-naics_5) %>%
  group_by(syear, agegrp, sex, aggregate_industry)%>%
  summarize(count = sum(count) / 12)%>%
  mutate(sex = factor(sex, levels = c(1, 2), labels = c("male", "female")))

ftpt <- load_clean_aggregate(pat = "empftpt_naics")
cow <- load_clean_aggregate(pat = "COW")
permtemp <- load_clean_aggregate(pat = "permtemp")
size <- load_clean_aggregate(pat = "size")
status <- load_clean_aggregate(pat = "lfsstat")
region <- load_clean_aggregate(pat = "EMP_REGION")

reg_stat <- vroom(here("data", list.files(here("data"), pattern = "EMP_REG_STAT")))
reg_ft <- vroom(here("data", list.files(here("data"), pattern = "ftpt_region")))
region_gvs <- vroom(here("data", list.files(here("data"), pattern = "EMP_REGION")))

#industry profiles---------------------
industry_overview <- total_employment_year(agg_emp_naics, last_full_year)%>%
  rename(
    current_employment = contains(as.character(last_full_year)),
    previous_employment = contains(as.character(previous_year))
  ) %>%
  mutate(
    yoy_change = round(current_employment - previous_employment),
    yoy_growth = round(100*(current_employment / previous_employment - 1), digits = 1),
    current_employment =round(current_employment)
  ) %>%
  ungroup() %>%
  select(aggregate_industry, yoy_growth, yoy_change, current_employment)

industry_overview<- agg_emp_naics %>%
  filter(
    syear == last_full_year,
    is.na(agegrp),
    !is.na(sex)
  ) %>%
  pivot_wider(names_from = sex, values_from = count) %>%
  mutate(
    men = round(100* male / (male + female), digits=1),
    women = round(100* female / (male + female), digits=1)
  ) %>%
  ungroup() %>%
  select(aggregate_industry, men, women)%>%
  full_join(industry_overview)

industry_unemployment <- status%>%
  filter(syear %in% c(last_full_year, (last_full_year-5)))%>%
  pivot_wider(names_from = lf_stat, values_from = count)%>%
  mutate(percent=round(100*unemployed/(employed+unemployed), digits = 1))%>%
  select(syear, aggregate_industry, percent)%>%
  pivot_wider(names_from = syear, values_from = percent, names_prefix= paste0("unemployment","_"))
colnames(industry_unemployment) <- str_replace(colnames(industry_unemployment), as.character(last_full_year), "current")
colnames(industry_unemployment) <- str_replace(colnames(industry_unemployment), as.character(last_full_year-5), "past")

industry_overview <- full_join(industry_overview, age_percentages("Between15and24", "percent_young_"))%>%
full_join(age_percentages(c("Between55and64", "65andover"), "percent_old_"))%>%
full_join(percentage(ftpt, ftpt, part_time, "part_time"))%>%
full_join(percentage(cow, class, self_employed, "self_employed"))%>%
full_join(percentage(cow, class, private_employe, "private_sector"))%>%
full_join(percentage(permtemp, temp, temporary, "temporary"))%>%
full_join(percentage(size, size, less_than_20_employees, "small"))%>%
full_join(industry_unemployment)

industry_wages <- hrlywages%>%
  full_join(mapping)%>%
  group_by(syear, sex, aggregate_industry) %>%
  summarize(average_wage = round(weighted.mean(hrlyearn_num_mean, w=hrlyearn_num_count, na.rm=TRUE), digits=2)) %>%
  filter(!is.na(aggregate_industry),
         syear %in% c(last_full_year, (last_full_year-5)),
         !is.na(sex)
         )%>%
  pivot_wider(names_from = sex, values_from = average_wage)%>%
  pivot_wider(names_from = syear, values_from = c("female","male"), names_prefix = "average_wage_")%>%
  select(aggregate_industry, starts_with("male"), everything())

colnames(industry_wages) <- str_replace(colnames(industry_wages), as.character(last_full_year), "current")
colnames(industry_wages) <- str_replace(colnames(industry_wages), as.character(last_full_year-5), "past")

industry_overview <- industry_wages%>%
  full_join(industry_overview)

industry_youth_wages <- youthwages%>%
  full_join(mapping) %>%
  filter(is.na(age),
         is.na(sex))%>%
  group_by(syear, aggregate_industry) %>%
  summarize(average_wage = round(weighted.mean(hrlyearn_num_mean, w=hrlyearn_num_count, na.rm=TRUE), digits = 2)) %>%
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
  mutate(across(where(is.numeric), ~ round(100*.x/`NA`, digits=1)))%>%
  select(-`NA`, -north_coast, -nechako)%>%
  full_join(industry_overview)


#add in redundant columns-----------------------

industry_overview$redundant_young <- fill_redundant("percent_young_current")
industry_overview$redundant_old  <- fill_redundant("percent_old_current")
industry_overview$redundant_part_time <- fill_redundant("part_time_current")
industry_overview$redundant_self <- fill_redundant("self_employed_current")
industry_overview$redundant_private <- fill_redundant("private_sector_current")
industry_overview$redundant_temporary <- fill_redundant("temporary_current")
industry_overview$redundant_small <- fill_redundant("small_current")
industry_overview$redundant_unemployment <- fill_redundant("unemployment_current")
industry_overview$redundant_men <- fill_redundant("men")
industry_overview$redundant_women <- fill_redundant("women")
industry_overview$redundant_male_wage <- fill_redundant("male_average_wage_current")#missing
industry_overview$redundant_female_wage <- fill_redundant("female_average_wage_current")#missing
industry_overview$redundant_youth_wage <- fill_redundant("youth_wages_current")
industry_overview$redundant_cariboo <- fill_redundant("cariboo")
industry_overview$redundant_kootenay <- fill_redundant("kootenay")
industry_overview$redundant_lower_mainland_southwest <- fill_redundant("lower_mainland_southwest")
industry_overview$redundant_north_coast_nechako <- fill_redundant("north_coast_nechako")
industry_overview$redundant_northeast <- fill_redundant("northeast")
industry_overview$redundant_thompson_okanagan <- fill_redundant("thompson_okanagan")
industry_overview$redundant_vancouver_island_and_coast <- fill_redundant("vancouver_island_and_coast")

industry_cleaned <- industry_overview%>%
  select(aggregate_industry, yoy_growth, yoy_change, current_employment, men, women, redundant_men, redundant_women,
         percent_young_past, percent_young_current, redundant_young, percent_old_past, percent_old_current, redundant_old,
         part_time_past, part_time_current, redundant_part_time, self_employed_past, self_employed_current, redundant_self,
         temporary_past, temporary_current, redundant_temporary, small_past, small_current, redundant_small,
         private_sector_past, private_sector_current, redundant_private, unemployment_past, unemployment_current, redundant_unemployment,
         male_average_wage_past, male_average_wage_current, redundant_male_wage, female_average_wage_past, female_average_wage_current,
         redundant_female_wage, youth_wages_past, youth_wages_current, redundant_youth_wage, cariboo, redundant_cariboo,
         kootenay, redundant_kootenay, lower_mainland_southwest, redundant_lower_mainland_southwest, north_coast_nechako,
         redundant_north_coast_nechako, northeast, redundant_northeast, thompson_okanagan, redundant_thompson_okanagan,
         vancouver_island_and_coast, redundant_vancouver_island_and_coast)%>%
  camel_to_title()%>%
  slice(16, 1:15,17:19) #hacky way to put total (16th row) at top.

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
  pivot_wider(names_from = age_group, values_from = value, names_prefix = "in_age_group_")|>
  janitor::adorn_percentages()|>
  mutate(across(where(is.numeric), ~round(100*.x, digits=1)))%>%
  clean_tbbl()

#regional full-time rates----------------
regional_full_time <-reg_ft%>%
  clean_tbbl()%>%
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
  mutate(percent_full_time=round(100*full_time/`NA`, digits=1),
         full_time=round(full_time)
         )%>%
  select(region, percent_full_time, full_time)

#regional unemployment rates-------------
regional_unemployment <- reg_stat%>%
  janitor::clean_names()%>%
  filter(syear<=last_full_year,
         syear>=last_full_year-10,
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
  clean_tbbl()

urate_summaries <- regional_unemployment%>%
  group_by(region)%>%
  summarise(low=round(100*min(unemployment_rate), digits=1),
            high=round(100*max(unemployment_rate), digits=1),
            ave=round(100*mean(unemployment_rate), digits=1),
            )

regional_unemployment <- regional_unemployment%>%
  mutate(unemployment_rate=round(100*unemployment_rate, digits=1))%>%
  pivot_wider(names_from = syear, values_from = unemployment_rate, names_prefix = "urate_")%>%
  full_join(urate_summaries)

regional_profile_1 <- full_join(regional_population, regional_full_time)%>%
  full_join(regional_unemployment)%>%
  camel_to_title()


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

regional_employment_by_industry_percent <- regional_employment_by_industry%>%
  janitor::adorn_percentages("row")%>%
  mutate(across(where(is.numeric), ~round(100*.x, digits=1)))

regional_employment_by_industry_long <- regional_employment_by_industry|>
  pivot_longer(cols=-region)|>
  mutate(name=paste0(name,"_level"),
         value=round(value, 0))

regional_employment_by_industry_percent_long <- regional_employment_by_industry_percent|>
  pivot_longer(cols=-region)|>
  mutate(name=paste0(name,"_percent"))

regional_profile_2 <- bind_rows(regional_employment_by_industry_long, regional_employment_by_industry_percent_long)|>
  arrange(name)|>
  pivot_wider(id_cols = region, names_from = name, values_from = value)|>
  camel_to_title()

colnames(regional_profile_2) <- str_to_title(str_replace_all(colnames(regional_profile_2), "_"," "))

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
  mutate(across(where(is.numeric), ~round(100*.x, digits=1)))

#goods vs services--------------------
regional_goods_vs_services <- region_gvs%>%
  filter(is.na(NAICS_5) | NAICS_5!="missi")%>%
 # mutate(naics=as.numeric(NAICS_5))%>%
  clean_tbbl()%>%
  mutate(region=case_when(is.na(region)~"british_columbia",
                         region=="north_coast"~"north_coast_and_nechako",
                         region=="nechako"~"north_coast_and_nechako",
                         TRUE~region))%>%
  full_join(gvs_mapping)%>%
  filter(syear==last_full_year)%>%
  group_by(region, goods_vs_services)%>%
  summarise(count=sum(count)/12)%>%
  pivot_wider(names_from = goods_vs_services, values_from = count)%>%
  mutate(percent_goods=round(100*goods/`NA`, digits=1),
         percent_services=round(100*services/`NA`, digits=1))%>%
  select(region, percent_goods, percent_services)

regional_profile_3 <- full_join(regional_by_region, regional_goods_vs_services)%>%
  arrange(region)%>%
  camel_to_title()
colnames(regional_profile_3) <- str_to_title(str_replace_all(colnames(regional_profile_3), "_"," "))

#save data without styles---------------------

wb <- loadWorkbook(here("output_template","LFS_Data_Sheet_template.xlsx"))
# prcntg <- createCellStyle(wb)
# setDataFormat(prcntg, format = "0.0%")

write_workbook(industry_cleaned, "Industry Profiles", 5, 1)
write_workbook(regional_profile_1, "Regional Profiles", 6, 1)
write_workbook(regional_profile_2, "Regional Profiles", 21, 1)
write_workbook(regional_profile_3, "Regional Profiles", 35, 1)

# rc0 = expand.grid(row = 5:23, col = c(2,5:22, 29:35))
# rc1 = expand.grid(row = 6:13, col = c(2:7, 9:22))
# rc2 = expand.grid(row = 20:27, col = 2:19)
# rc3 = expand.grid(row = 34:41, col = 2:21)
#
# setCellStyle(wb, sheet = "Industry Profiles", row= rc0$row, col = rc0$col, cellstyle = prcntg)
# setCellStyle(wb, sheet = "Regional Profiles", row= rc1$row, col = rc1$col, cellstyle = prcntg)
# setCellStyle(wb, sheet = "Regional Profiles", row= rc2$row, col = rc2$col, cellstyle = prcntg)
# setCellStyle(wb, sheet = "Regional Profiles", row= rc3$row, col = rc3$col, cellstyle = prcntg)

saveWorkbook(wb, here(
  "out",
  paste0(last_full_year, "_LFS_data_sheet(",today(),").xlsx")
))


