total_employment_year <- function(tbbl, year) {
  years <- (year-1):year
  tbbl %>%
    clean_names()|>
    filter(
      syear %in% years,
      is.na(agegrp),
      is.na(gender)
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
      is.na(gender),
      agegrp %in% c(ages, NA)
    ) %>%
    group_by(syear, in_age_group = !is.na(agegrp), aggregate_industry) %>%
    summarize(count = sum(count)) %>%
    pivot_wider(names_from = in_age_group, values_from = count, names_prefix = "in_age_group_") %>%
    mutate(percent_in_age_group = round(100*in_age_group_TRUE / in_age_group_FALSE, digits=1)) %>%
    ungroup() %>%
    select(syear, aggregate_industry, percent_in_age_group)%>%
    pivot_wider(names_from = syear, values_from = percent_in_age_group, names_prefix = prefix)
    colnames(temp) <- str_replace(colnames(temp),as.character(last_full_year), "current")
    colnames(temp) <- str_replace(colnames(temp),as.character(last_full_year-5), "past")
    temp
}

load_clean_aggregate <- function(paths) {
  #need to get rid of "missi" before converting to numeric, otherwise missi converted to NA and f's up aggregate.
  #note that by default filter drops NA's, must explicitly state we want to keep the NAs (which in RTRA land are the aggregates.)
  vroom(paths)%>%
    filter(is.na(NAICS_5) | NAICS_5!="missi")%>%
    clean_tbbl()%>%
    full_join(mapping)%>%
    select(-naics_5) %>%
    group_by(across(c(-count))) %>%
    summarize(count = sum(count) / 12) %>%
    filter(!is.na(aggregate_industry))
}

percentage <- function(tbbl, var, value, quoted_value){
  tbbl <- tbbl%>%
    filter(syear %in% c(last_full_year, (last_full_year-5)))%>%
    pivot_wider(names_from = {{  var  }}, values_from = count)%>%
    mutate(percent=round(100*{{  value  }}/`NA`, digits=1))

  temp <- tbbl%>% #for RTRA data NA indicates the aggregate of all levels.
    select(syear, aggregate_industry, percent)%>%
    pivot_wider(names_from = syear, values_from = percent, names_prefix= paste0(quoted_value, "_"))
  colnames(temp) <- str_replace(colnames(temp), as.character(last_full_year), "current")
  colnames(temp) <- str_replace(colnames(temp), as.character(last_full_year-5), "past")
  temp
}

# Function to quickly export data
write_workbook <- function(data, sheetname, startrow, startcol, head=FALSE) {
  writeWorksheet(
    wb,
    data,
    sheetname,
    startRow = startrow,
    startCol = startcol,
    header = head
  )
}

fill_redundant <- function(var){
  unlist(industry_overview[industry_overview$aggregate_industry=="Total, All Industries", var])
}

clean_tbbl <- function(tbbl) {
  tbbl %>%
    janitor::clean_names() %>%
    mutate(across(where(is.character), make_clean_factor))
}
make_clean_factor <- function(strng) {
  strng %>%
    stringr::str_replace_all("\t", "") %>%
    trimws() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("-", "_") %>%
    factor()
}

camel_to_title <- function(tbbl) {
  tbbl %>%
    rapply(as.character, classes = "factor", how = "replace") %>%
    tibble() %>%
    mutate(across(where(is.character), make_title))
}

make_title <- function(strng){
  strng <- str_to_title(str_replace_all(strng,"_"," "))
}






