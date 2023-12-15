
# Load Packages -----------------------------------------------------------

library("tidyverse")



# Read Data ---------------------------------------------------------------

results <- read_rds("all_results.rds"t

men <- results |>
  filter(date > mdy("1/1/2020"), gender == "M") |>
  mutate(sinclair = total * (10^(0.722762521 * ((log10(bw/193.609))^2))),
         weight_class = factor(case_when(
           bw <= 55 ~ "55kg",
           bw > 55 & bw <= 61 ~ "61kg",
           bw > 61 & bw <= 67 ~ "67kg",
           bw > 67 & bw <= 73 ~ "73kg",
           bw > 73 & bw <= 81 ~ "81kg",
           bw > 81 & bw <= 89 ~ "89kg",
           bw > 89 & bw <= 96 ~ "96kg",
           bw > 96 & bw <= 102 ~ "102kg",
           bw > 102 & bw <= 109 ~ "109kg",
           bw > 109 ~ "109+kg",
           TRUE ~ NA  # for weights that don't fit any category
         ), levels = c("55kg", "61kg", "67kg", "73kg", "81kg", "89kg",
                       "96kg", "102kg", "109kg", "109+kg"),
         ordered = TRUE))

men |>
  # filter(sn_best > 0) |>
  ggplot(aes(bw, sn_best)) +
    geom_point(alpha = .5)

men |>
  filter(sinclair > 0) |>
  ggplot(aes(weight_class, sinclair)) +
    geom_jitter(height = 0, alpha = .5)

men |>
  ggplot(aes(bw, sinclair)) +
    geom_point()



