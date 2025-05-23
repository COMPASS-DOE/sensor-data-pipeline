# Generate diagnostic plots for a variety of sites and variables
# so that we can assess the MAD outlier identification approach
# BBL May 2025

source("pipeline/L2-utils.R")

# This is run on the v1-2 version of L1 data

L1 <- "/Users/d3x290/Library/CloudStorage/Dropbox/Documents/Work/Current/COMPASS/Data team/v1-2 release/v1-2"
OUTPUT <- "~/Desktop/mad_testing/"

SITE_YEAR <- "TMP_2024"

files <- list.files(file.path(L1, SITE_YEAR), pattern = "csv$", full.names = TRUE)

library(dplyr)
library(readr)
library(lubridate)
dat <- lapply(files, read_csv, col_types = "ccTccccdccii") %>%
    bind_rows() %>%
    select(-ID, -Location)

library(ggplot2)
theme_set(theme_bw())

message("Splitting...")
# Split once. This is much more efficient that filtering
# group by group!
dat %>%
    group_by(Plot, research_name) %>%
    group_split() ->
    dat_list

for(x in dat_list) {

    pl <- x$Plot[1]
    rn <- x$research_name[1]
    results <- list()

    message(paste(SITE_YEAR, pl, rn))
    for(u in 1:3) {
        unit <- c("1 day", "1 week", "1 month")[u]
        unitname <- c("a-1 day", "b-1 week", "c-1 month")[u] # for orderly faceting

        for(thr in c(3, 5)) {
            x %>%
                group_by(round_date(TIMESTAMP, unit)) %>%
                mutate(F_mad = QAQC_mad(Value, threshold = thr),
                       threshold = paste("threshold", thr),
                       unit = paste(unitname, "grouping")) ->
                results[[paste(unit, thr)]]
        } # for thr
    } # for u

    results <- bind_rows(results)
    if(!nrow(results)) next

    outfn <- paste("MADtesting", SITE_YEAR, pl, rn, sep = "_")
    p <- ggplot(results, aes(TIMESTAMP, Value, color = F_mad)) +
        geom_point(size = I(0.25), na.rm = TRUE) +
        facet_grid(threshold ~ unit) +
        ggtitle(outfn)

    path <- file.path(OUTPUT, rn)
    if(!dir.exists(path)) dir.create(path)
    ggsave(file.path(path, paste0(outfn, ".png")), height = 8, width = 10, plot = p)
} # for x

message("All done!")
