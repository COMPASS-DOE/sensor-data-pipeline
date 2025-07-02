# Calculate and plot cumulative observations over time
# This script assumes that the working directory is the "L1/" folder
# of the COMPASS-FME L1 (Level 1) environmental sensor data
# BBL and SCP June 2025

# Find and count lines for all v2 files in the L1 folder
files <- list.files(pattern = "_L1_v2.*csv$",
                    full.names = TRUE, recursive = TRUE)

library(dplyr)
library(lubridate)
library(readr)

results <- list()
for(f in files) {
    message(basename(f))
    read_csv(f, col_types = "ccTc---dc----") %>%
        mutate(Year = year(TIMESTAMP),
               Quarter = quarter(TIMESTAMP),
               Month = month(TIMESTAMP)) %>%
        group_by(Site, Plot, Instrument, research_name, Year, Quarter, Month) %>%
        summarise(n = n(), .groups = "drop") ->
        results[[f]]
}

results %>%
    bind_rows() ->
    results

# Make some graphs
library(ggplot2)
theme_set(theme_bw())
library(scales)
library(viridis)
library(dplyr)
library(gganimate)

results %>%
    mutate(YearQuarter = Year + (Quarter - 1) * 0.25) %>%
    ggplot(aes(YearQuarter, Site, fill = n / 3)) + geom_tile() +
    xlab("Time") +
    scale_fill_gradient("Monthly\nobservations",
                        trans = scales::log_trans(base = 10)) ->
    p
print(p)
#ggsave("~/Desktop/heatmap.png", height = 6, width = 10)

# Compute cumulative observations by site and date
results %>%
    complete(Site, Date, fill = list(rows = 0)) %>%
    group_by(Site, year(Date)) %>%
    summarise(n = sum(rows, na.rm = TRUE), .groups = "drop") %>%
    rename(Year = `year(Date)`) %>%
    group_by(Site) %>%
    mutate(cum_n = cumsum(n)) ->
    smry

p2 <- ggplot(smry, aes(x = Date, y = cum_n, fill = Site)) +
    geom_line(alpha = 0.8 , linewidth = 0.5, colour = "white") +
    xlab("Year") + ylab("COMPASS-FME environmental sensor observations") +
    scale_fill_viridis(discrete = TRUE) +
    theme(axis.title = element_text(size = 14)) +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
    transition_reveal(Date) + view_follow(fixed_y = TRUE)
animate(p2)
print(p2)
ggsave("~/Desktop/sensors.png", height = 6, width = 8)

site_colors <- c(
    "CRC" = "#29363b",
    "GCW" = "#009875",
    "GWI" = "#99b898",
    "MSM" = "#fecea8",
    "OWC" = "#ff857b",
    "PTR" = "#e94a5f",
    "SWH" = "#c03a2b",
    "TMP" = "#96281b")

ggplot(smry, aes(x = factor(Year), y = n, fill = Site)) +
    geom_bar(position = "stack", stat = "identity") +
    theme(axis.title = element_text(size = 30), axis.text = element_text(size = 30),
          legend.text = element_text(size =20), legend.title = element_text(size =20)) +
    scale_fill_manual(values = site_colors) +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
    xlab("Year") + ylab("COMPASS-FME environmental sensor observations") +
    transition_states(Year) +
    shadow_mark() -> gif

animate(gif, fps = 10, duration = 10,
        width = 1000, height = 800, renderer = gifski_renderer(loop = FALSE))

anim_save("~/Documents/bar_chart.gif")

# All done!
