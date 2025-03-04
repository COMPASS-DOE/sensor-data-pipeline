
library(tidyverse)

fls <- list.files("~/Documents/v1-1", pattern = "*.csv$", full.names = TRUE, recursive = TRUE)

# filter out TMP
fls[!grepl("TMP", fls)] -> fls

results <- list()

for(f in fls) {
    message(basename(f))

    results[[f]] <- readr::read_csv(f, col_types = "ccTccccdccii") %>%
        mutate(ts_str = format(TIMESTAMP, "%b-%Y")) %>%
        group_by(Site, Instrument, ts_str) %>%
            summarise(n = n(),
                      # retain the timestamp for correct sorting later
                      TIMESTAMP = mean(TIMESTAMP),
                      .groups = "drop")
}

bind_rows(results) %>%
    # each file is a site and plot; sum by site
    group_by(Site, Instrument, ts_str) %>%
    summarise(n = sum(n),
              TIMESTAMP = mean(TIMESTAMP),
              .groups = "drop") %>%
    # not sure why this next line is here
    filter(Site != "GCW", !is.na(Instrument)) %>%
    mutate(data_present = if_else(n > 0, "Yes", "No")) %>%
    # create the factor month-year
    arrange(TIMESTAMP) %>%
    mutate(ts_fct = factor(ts_str, levels = unique(ts_str))) %>%
    # ...and plot
    ggplot(aes(x = ts_fct, y = Instrument, fill = Instrument)) +
    geom_raster(hjust = 0, vjust = 0) +
    facet_wrap(~Site, ncol = 1, strip.position = "left") +
    theme_minimal() +
    scale_y_discrete(position = "right") +
    theme(axis.text.x = element_text(angle = 90, vjust = -0.8, hjust = 1),
          axis.text.y = element_text(vjust = 1.25),
          axis.ticks.y=element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid = element_line(color="darkgrey"),
          panel.border = element_rect(color = "darkgrey", fill = NA, size = 1)) +
    labs(x = "", y = "") +
    scale_fill_viridis_d(option = "rocket", begin = 0, end = 0.85) +
    theme(axis.title = element_text(size = 20),
          plot.title = element_text(size=20),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 16),
          strip.text = element_text(size = 18),
          legend.position="bottom",
          legend.text = element_text(size=16),
          legend.title = element_text(size=18)) -> p

ggsave("~/Documents/synoptic_avail.png", height = 12, width = 15)
