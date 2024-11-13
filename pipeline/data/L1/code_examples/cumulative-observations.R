# Calculate and plot cumulative observations over time


# Find and count lines for all files in the L1 folder
fls <- list.files("./data/L1/", pattern = "*.csv$", full.names = TRUE, recursive = TRUE)

library(tibble)
results <- tibble(file = basename(fls), rows = NA_real_)

for(i in seq_along(fls)) {
    message(basename(fls[i]))
    results$rows[i] <- length(readLines(fls[i])) - 1
}

# An example of how to parse the filenames into useful information:
# site, plot, time range, data level, and version number
library(tidyr)
results <- separate(results, file, sep = "_", into = c("Site", "Plot", "Timerange","Level","version"))
results <- separate(results, Timerange, sep = "-", into = c("Begin", "End"))
results$Date <- as.Date(results$Begin, format = "%Y%m%d")
results$Year <- year(results$Date)
results$Month <- month(results$Date)
results$Quarter <- quarter(results$Date)

# Make some graphs
library(ggplot2)
theme_set(theme_bw())
library(scales)
library(viridis)

results %>%
    group_by(Site, Year, Quarter) %>%
    summarise(rows = sum(rows)) %>%
    mutate(YearMonth = ymd(paste(Year, Quarter * 3, "01"))) %>%
    ggplot(aes(YearMonth, Site, fill = rows / 3)) + geom_tile() +
    xlab("Time") +
    scale_fill_gradient("Monthly\nobservations", trans = scales::log_trans(base = 10),
                        labels = unit_format(unit = "M", scale = 1e-6)) ->
    p
print(p)
ggsave("~/Desktop/heatmap.png", height = 6, width = 10)

# Compute cumulative observations by site and date
results %>%
    complete(Site, Date, fill = list(rows = 0)) %>%
    group_by(Site, Date) %>%
    summarise(n = sum(rows, na.rm = TRUE), .groups = "drop") %>%
    arrange(Date) %>%
    group_by(Site) %>%
    mutate(cum_n = cumsum(n)) ->
    smry

p2 <- ggplot(smry, aes(Date, cum_n, fill = Site)) +
    geom_area(alpha = 0.8 , linewidth = 0.5, colour = "white") +
    xlab("Year") + ylab("COMPASS-FME environmental sensor observations") +
    scale_fill_viridis(discrete = TRUE) +
    theme(axis.title = element_text(size = 14)) +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))
print(p2)
ggsave("~/Desktop/sensors.png", height = 6, width = 8)
