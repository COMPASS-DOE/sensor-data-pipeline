library(compasstools)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(lubridate)

set.seed(123)

# Nick asks:
# I wonder if you or someone has already done an analysis of how much we
# gain by having so much replication of Teros 12’s at TEMPEST? E.g., if we
# had 50% less sensors how much of the total variability would we not see?

# variable <- "soil-EC-15cm"
# variable <- "soil-temp-15cm"
variable <- "soil-vwc-15cm"
site <- "TMP"
dat <- read_L2_variable(variable, path = "~/sensor_data/Level2/v2-1/", site = site)

dat %>%
    filter(!is.na(Value)) %>%
    mutate(Year = year(TIMESTAMP),
           Day = yday(TIMESTAMP),
           Hour = hour(TIMESTAMP)) %>%
    filter(Year == 2024) %>%
    select(-Instrument, -Location, -N_avg, -N_drop, -Value_MAC) ->
    dat

# Figure out how many distinct sensors we have in each plot
dat %>%
    group_by(Plot) %>%
    summarise(n = n_distinct(Sensor_ID)) %>%
    summarise(min(n)) %>%
    pull() ->
    sensors_per_plot

sizes <- as.integer(seq(from = 2, to = sensors_per_plot, length.out = 10))

# Run through various sample sizes and then, for each plot,
# repeatedly sample that many sensors and compute the CV
results_list <- list()
for(sample_size in sizes) {
    for(p in unique(dat$Plot)) {
        dat %>% filter(Plot == p) -> dat_p
        message(sample_size)
        for(i in 1:10) {
            sensors_to_sample <- sample(unique(dat_p$Sensor_ID),
                                        sample_size,
                                        replace = FALSE)
            dat_p %>%
                filter(Sensor_ID %in% sensors_to_sample) %>%
                group_by(Plot, Day, Hour, research_name) %>%
                summarise(CV = sd(Value) / mean(Value), .groups = "drop") %>%
                mutate(n_sensors = sample_size) ->
                results_list[[paste(p, sample_size, i)]]
        }
    }
}

# Summarise and plot
bind_rows(results_list) %>%
    group_by(Plot, Day, research_name, n_sensors) %>%
    summarise(CV = mean(CV), .groups = "drop") ->
    results

p1 <- ggplot(results, aes(Day, CV, color = n_sensors)) +
    geom_point(na.rm = TRUE) +
    facet_grid(Plot ~ ., scales = "free") +
    ylab("Hourly CV of sensors") +
    ggtitle(paste(site, results$research_name[1]))

print(p1)
ggsave(paste(site, variable, "doy-var.png", sep = "_"), width = 8, height = 6)

p2 <- results %>%
    group_by(Plot, n_sensors) %>%
    summarise(CV = mean(CV, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(n_sensors, CV, color = Plot)) + geom_line() +
    ylab("Hourly CV of sensors") +
    ggtitle(paste(site, results$research_name[1]))

print(p2)
ggsave(paste(site, variable, "n-var.png", sep = "_"), width = 8, height = 6)

message("All done")
