total_employment_year <- function(tbbl, year) {
  tbbl %>%
    filter(
      syear %in% year,
      is.na(agegrp),
      is.na(sex)
    ) %>%
    mutate(count = round(count)) %>%
    pivot_wider(
      names_from = syear,
      values_from = count,
      names_prefix = "total_employment_"
    )
}

age_percentages <- function(ages, prefix) {
  temp <- agg_emp_naics %>%
    filter(
      syear %in% c((last_full_year - 5), last_full_year),
      is.na(sex),
      agegrp %in% c(ages, NA)
    ) %>%
    group_by(syear, in_age_group = !is.na(agegrp), aggregate_industry) %>%
    summarize(count = sum(count)) %>%
    pivot_wider(names_from = in_age_group, values_from = count, names_prefix = "in_age_group_") %>%
    mutate(percent_in_age_group = round(in_age_group_TRUE / in_age_group_FALSE, digits=3)) %>%
    ungroup() %>%
    select(syear, aggregate_industry, percent_in_age_group)%>%
    pivot_wider(names_from = syear, values_from = percent_in_age_group, names_prefix = prefix)
    colnames(temp) <- str_replace(colnames(temp),as.character(last_full_year), "current")
    colnames(temp) <- str_replace(colnames(temp),as.character(last_full_year-5), "past")
    temp
}

load_clean_aggregate <- function(pat) {
  #need to get rid of "missi" before converting to numeric, otherwise missi converted to NA and f's up aggregate.
  #note that by default filter drops NA's, must explicitly state we want to keep the NAs (which in RTRA land are the aggregates.)
  temp <- vroom(here("data", list.files(here("data"), pattern = pat)))%>%
  filter(is.na(NAICS_5) | NAICS_5!="missi")%>%
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
  tbbl <- tbbl%>%
    filter(syear %in% c(last_full_year, (last_full_year-5)))%>%
    pivot_wider(names_from = {{  var  }}, values_from = count)%>%
    mutate(percent=round({{  value  }}/`NA`, digits=3))

  temp <- tbbl%>% #for RTRA data NA indicates the aggregate of all levels.
    select(syear, aggregate_industry, percent)%>%
    pivot_wider(names_from = syear, values_from = percent, names_prefix= paste0(quoted_value, "_"))
  colnames(temp) <- str_replace(colnames(temp), as.character(last_full_year), "current")
  colnames(temp) <- str_replace(colnames(temp), as.character(last_full_year-5), "past")
  temp
}

# Function to quickly export data
write_workbook <- function(data, sheetname, startrow, startcol, head) {
  writeWorksheet(
    wb,
    data,
    sheetname,
    startRow = startrow,
    startCol = startcol,
    header = head
  )
}




