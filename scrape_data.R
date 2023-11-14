
# Load Packages -----------------------------------------------------------

library("lubridate")
library("furrr")
library("tidyverse")
theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank())
library("rvest")
library("slider")



# Helper Functions --------------------------------------------------------

create_tb <- function(row) {
  if (length(row) == 10) {
    return(tibble(rank = row[1],
                  name = row[2],
                  nation = row[3],
                  born = row[4],
                  bw = row[5],
                  group = row[6],
                  attempt1 = row[7],
                  attempt2 = row[8],
                  attempt3 = row[9],
                  total = row[10]))
  }

}

results_tb <- function(row) {
  if (length(row) == 9) {
    return(tibble(comp_rank = row[1],
                         name = row[2],
                         born = row[4]))
  }
}


# Define Scrape Events Function -------------------------------------------

scrape_events <- function(event_list, new_bw = TRUE) {
  if (new_bw) {
    new_bw_urls <- read_html(event_list) %>%
      html_elements(".card") %>%
      html_attr("href") %>%
      map_chr(~ glue::glue("https://iwf.sport/results/results-by-events/{.x}"))
  }
  if (!new_bw) {
    new_bw_urls <- read_html(event_list) %>%
      html_elements(".card") %>%
      html_attr("href") %>%
      map_chr(~ glue::glue("https://iwf.sport/results/results-by-events/results-by-events-old-bw/{.x}"))
  }

  event <- read_html(event_list) %>%
    html_elements(".text") %>%
    html_text2()

  date_loc <- read_html(event_list) %>%
    html_elements(".normal__text") %>%
    html_text2()

  date <- date_loc[seq(1, length(date_loc), by = 2)]
  location <- date_loc[seq(2, length(date_loc), by = 2)]

  tibble(results_url = new_bw_urls,
         event = event,
         date = date,
         location = location)
}


# Define Scrape Function --------------------------------------------------

scrape_gender_results <- function(results_url, event, date, location, gender) {
  if (gender == "M") {
    results_gender <- "#men_snatchjerk"
  }
  if (gender == "F") {
    results_gender <- "#women_snatchjerk"
  }

  html <- read_html(results_url) %>%
    html_elements(results_gender) %>%
    html_elements(".card")

  if (length(html) == 0) {
    return()
  }
  tb <- html %>%
    as.character() %>%
    str_replace_all("<strike>","<strike>-") %>%
    map(minimal_html) %>%
    map_chr(html_text2) %>%
    map(~ str_split(.x, "\\n\\n") %>% unlist()) %>%
    map_dfr(create_tb) %>%
    filter(rank != "Rank:")

  results <- html %>%
    map_chr(html_text2) %>%
    map(~ str_split(.x, "\\n\\n") %>% unlist()) %>%
    map_dfr(results_tb) #%>%
    # filter(comp_rank != "Rank:")

  tb <- inner_join(tb, results, by = c("name", "born"))

  tb <- tb %>%
    mutate(rank = str_remove(rank, "Rank: ") %>% as.double(),
           comp_rank = str_remove(comp_rank, "Rank: ") %>% as.double(),
           nation = as.factor(nation),
           born = str_remove(born, "Born: ") %>% mdy(),
           bw = str_remove(bw, "B.weight: ") %>% as.double(),
           group = str_remove(group, "Group: ") %>% as.factor(),
           attempt1 = str_remove(attempt1, "1: ") %>% as.double(),
           attempt2 = str_remove(attempt2, "2: ") %>% as.double(),
           attempt3 = str_remove(attempt3, "3: ") %>% as.double(),
           total = str_remove(total, "Total: ") %>% as.double()) %>%
    unite("name_born", c(name, born), sep = " ", remove = FALSE) %>%
    suppressWarnings()

  ### Select only snatch results
  ### This assumes the snatch results are always first. If clean and jerk
  ### appears greater than snatch then investigate
  sn <- tb %>%
    distinct(name_born, .keep_all = TRUE)

  ### Create clean and jerk result table
  cj <- tb %>%
    anti_join(sn) %>%
    rename(rank_cj = rank,
           cj1 = attempt1,
           cj2 = attempt2,
           cj3 = attempt3,
           cj_best = total) %>%
    suppressMessages()

  ### Rename snatch columns
  sn <- sn %>%
    rename(rank_sn = rank,
           sn1 = attempt1,
           sn2 = attempt2,
           sn3 = attempt3,
           sn_best = total)

  ### Create final output tibble with all results
  inner_join(sn, cj, by = c("name_born" = "name_born",
                            "nation" = "nation",
                            "bw" = "bw",
                            "group" = "group",
                            "name" = "name",
                            "born" = "born",
                            "comp_rank" = "comp_rank")) %>%
    mutate(total = sn_best + cj_best,
           total_ratio = total/bw,
           cj_ratio = cj_best/bw,
           sn_ratio = sn_best/bw,
           gender = gender,
           event = event,
           date = mdy(date),
           location = location,
           gender = gender)
}

scrape_results <- function(results_url, event, date, location) {
  map_dfr(c("M","F"), ~ scrape_gender_results(results_url, event, date, location, .x))
}

# Scrape All Event Links --------------------------------------------------

#
# senior_new_bw_urls <- scrape_events("https://iwf.sport/results/results-by-events/?event_type=all&event_age=Senior&event_nation=all")
# senior_old_bw_urls <- scrape_events("https://iwf.sport/results/results-by-events/results-by-events-old-bw/?event_type=all&event_age=Senior&event_nation=all", new_bw = FALSE)
#
# junior_new_bw_urls <- scrape_events("https://iwf.sport/results/results-by-events/?event_type=all&event_age=Junior&event_nation=all")
# junior_old_bw_urls <- scrape_events("https://iwf.sport/results/results-by-events/results-by-events-old-bw/?event_type=all&event_age=Junior&event_nation=all", new_bw = FALSE)
#
# youth_new_bw_urls <- scrape_events("https://iwf.sport/results/results-by-events/?event_type=all&event_age=Youth&event_nation=all")
# youth_old_bw_urls <- scrape_events("https://iwf.sport/results/results-by-events/results-by-events-old-bw/?event_type=all&event_age=Youth&event_nation=all", new_bw = FALSE)

# Call Scraping Function --------------------------------------------------

# plan(multisession, workers = (availableCores()-2))
#
# senior_results <- future_pmap_dfr(senior_new_bw_urls, scrape_results)
# senior_old_bw_results <- future_pmap_dfr(senior_old_bw_urls, scrape_results)
#
# junior_results <- future_pmap_dfr(junior_new_bw_urls, scrape_results)
# junior_old_bw_results <- future_pmap_dfr(junior_old_bw_urls, scrape_results)
#
# youth_results <- future_pmap_dfr(youth_new_bw_urls, scrape_results)
# youth_old_bw_results <- future_pmap_dfr(youth_old_bw_urls, scrape_results)
#
# plan(sequential)
#
# write_rds(senior_results, file = "senior_results.rds")
# write_rds(senior_old_bw_results, file = "senior_old_bw_results.rds")
#
# write_rds(junior_results, file = "junior_results.rds")
# write_rds(junior_old_bw_results, file = "junior_old_bw_results.rds")
#
# write_rds(youth_results, file = "youth_results.rds")
# write_rds(youth_old_bw_results, file = "youth_old_bw_results.rds")


# Create Final Dataset ----------------------------------------------------

# alll_results <- read_rds("all_results.rds")

senior_results <- read_rds("senior_results.rds")
senior_old_bw_results <- read_rds("senior_old_bw_results.rds")
junior_results <- read_rds("junior_results.rds")
junior_old_bw_results <- read_rds("junior_old_bw_results.rds")
youth_results <- read_rds("youth_results.rds")
youth_old_bw_results <- read_rds("youth_old_bw_results.rds")

all_results <- bind_rows(senior_results %>% mutate(division = "senior"),
                         senior_old_bw_results %>% mutate(division = "senior"),
                         junior_results %>% mutate(division = "junior"),
                         junior_old_bw_results %>% mutate(division = "junior"),
                         youth_results %>% mutate(division = "youth"),
                         youth_old_bw_results %>% mutate(division = "youth")) %>%
  mutate(division = as.factor(division)) %>%
  distinct(.keep_all = TRUE)

all_results <- all_results %>%
  replace_na(list(sn1 = 0, sn2 = 0, sn3 = 0, sn_best = 0,
                  cj1 = 0, cj2 = 0, cj3 = 0, cj_best = 0,
                  rank_sn = 0, rank_cj = 0, comp_rank = 0,
                  total = 0, total_ratio = 0, cj_ratio = 0, sn_ratio = 0)) %>%
  mutate(age = time_length(interval(born, date), "year"),
         sn_make_perc = ((sn1 > 0) + (sn2 > 0) + (sn3 > 0)) / 3,
         cj_make_perc = ((cj1 > 0) + (cj2 > 0) + (cj3 > 0)) / 3,
         make_perc = (sn_make_perc + cj_make_perc)/2) %>%
  arrange(date)


all_results <- all_results %>%
  group_by(name_born) %>%
  slice(1:100) %>%
  mutate(max_sn = lag(slide_max(sn_best, before = Inf), 1),
         max_cj = lag(slide_max(cj_best, before = Inf)),
         max_total = lag(slide_max(total, before = Inf)),
         sn1_perc = sn1/max_sn,
         sn2_perc = sn2/max_sn,
         sn3_perc = sn3/max_sn,
         cj1_perc = cj1/max_cj,
         cj2_perc = cj2/max_cj,
         cj3_perc = cj3/max_cj,
         sn_career_make_perc = lag(slide_mean(sn_make_perc, before = Inf)),
         cj_career_make_perc = lag(slide_mean(cj_make_perc, before = Inf)),
         career_make_perc = lag(slide_mean(make_perc, before = Inf)),
         sn_avg_rank = lag(slide_mean(rank_sn, before = Inf)),
         cj_avg_rank = lag(slide_mean(rank_cj, before = Inf)),
         comp_avg_rank = lag(slide_mean(comp_rank, before = Inf)),
         years_wl = slide_vec(date, ~ as.numeric(.x[length(.x)] - .x[1])/365, .before = Inf)) %>%
  replace_na(list("max_sn" = 0, "max_cj" = 0, "max_total" = 0,
                  "sn_career_make_perc" = 0, "cj_career_make_perc" = 0, "career_make_perc" = 0,
                  "sn_avg_rank" = 0, "cj_avg_rank" = 0, "comp_avg_rank" = 0)) %>%
  arrange(date, .by_group = TRUE) %>%
  ungroup()

# write_rds(all_results, file = "all_results.rds")

# Testing -----------------------------------------------------------































