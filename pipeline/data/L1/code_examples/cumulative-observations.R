# Calculate and plot cumulative observations over time


# Find and count lines for all files in the L1 folder
fls <- list.files("~/Documents/v1-1/", pattern = "*.csv$", full.names = TRUE, recursive = TRUE)

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
    group_by(Site, year(Date)) %>%
    summarise(n = sum(rows, na.rm = TRUE), .groups = "drop") %>%
    rename(Year = `year(Date)`) %>%
    group_by(Site) %>%
    mutate(cum_n = cumsum(n)) ->
    smry

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

ggplot(smry, aes(x = factor(Year), y = n, fill = Site)) +
    geom_bar(position = "stack", stat = "identity") +
    theme(axis.title = element_text(size = 30), axis.text = element_text(size = 30),
          legend.text = element_text(size =20), legend.title = element_text(size =20)) +
    scale_fill_manual(values = site_colors) +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
    xlab("Year") + ylab("COMPASS-FME environmental sensor observations") +
    transition_states(Year) +
    shadow_mark() -> gif

site_colors <- c(
    "CRC" = "#29363b",
    "GCW" = "#009875",
    "GWI" = "#99b898",
    "MSM" = "#fecea8",
    "OWC" = "#ff857b",
    "PTR" = "#e94a5f",
    "SWH" = "#c03a2b",
    "TMP" = "#96281b")

animate(gif, fps = 10, duration = 10,
        width = 1000, height = 800, renderer = gifski_renderer(loop = FALSE))

anim_save("~/Documents/bar_chart.gif")
