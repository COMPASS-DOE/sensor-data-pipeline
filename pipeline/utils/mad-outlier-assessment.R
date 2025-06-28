# Generate diagnostic plots for a variety of sites and variables
# so that we can assess the MAD outlier identification approach
# BBL May 2025

source(here::here("pipeline/L1-utils.R"))

# This is run on the v1-2 version of L1 data

L1 <- "~/Dropbox/Documents/Work/Current/COMPASS/Data team/v1-2 release/v1-2"
L1 <- "./data_PREFLIGHT/L1/"
OUTPUT <- "~/Desktop/mad_testing/"

library(ggplot2)
theme_set(theme_bw())

# ---------
# Visualize normal data and where MAD and 3*MAD fall
n <- 1000
test <- data.frame(x = rnorm(n), y = rnorm(n))
circle <- function(center = c(0, 0), r = 1, npoints = 100) {
    tt <- seq(0, 2*pi, length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}
c1 <- circle(r = 1)
c3 <- circle(r = 3)
c5 <- circle(r = 5)
ggplot(test, aes(x, y)) + geom_point(size = 0.75) +
    geom_path(data = c1, color = "red", linewidth = 2) +
    geom_path(data = c3, color = "red", linewidth = 1, linetype = 2) +
    geom_path(data = c5, color = "red", linewidth = 1, linetype = 2) +
    coord_fixed()
# ---------

#SITE_YEAR <- "CRC_2024"
#SITE_YEAR <- "OWC_2024"
#SITE_YEAR <- "PTR_2024"
#SITE_YEAR <- "MSM_2024"
#SITE_YEAR <- "GWI_2024"
#SITE_YEAR <- "SWH_2024"
#SITE_YEAR <- "TMP_2024"

siteyears <- c("CRC_2024", "OWC_2024", "PTR_2024",
               "MSM_2024", "GWI_2024", "SWH_2024", "TMP_2024")
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

for(SITE_YEAR in siteyears) {
    message(SITE_YEAR)
    files <- list.files(file.path(L1, SITE_YEAR), pattern = "csv$", full.names = TRUE)

    for(f in files) {

        x <- read_csv(f, col_types = "ccTccccdccii") %>%
            filter(!is.na(Value)) %>%
            select(-Source_file, -Location, -F_MAD)

        pl <- x$Plot[1]
        rn <- x$research_name[1]
        results <- list()

        message("\t", paste(SITE_YEAR, pl, rn))
        for(u in 1:3) {
            unit <- c("1 day", "1 week", "1 month")[u]
            unitname <- c("a-1 day", "b-1 week", "c-1 month")[u] # for orderly faceting

            for(thr in c(4, 6, 8)) {
                x %>%
                    group_by(round_date(TIMESTAMP, unit)) %>%
                    mutate(F_MAD = outliers_MAD(Value, threshold = thr),
                           F_OOB = F_OOB,
                           threshold = paste("threshold", thr),
                           unit = paste(unitname, "grouping")) ->
                    results[[paste(unit, thr)]]
            } # for thr
        } # for u

        # For better visualization, we drop out-of-bounds values
        # AFTER including them in the outlier test (since that's what
        # happens in the pipeline)
        results <- bind_rows(results) %>%
            replace_na(list(F_MAD = 0, F_OOB = 0)) %>%
            filter(F_OOB == 0)

        if(!nrow(results)) next

        outfn <- paste("MADtesting", rn, SITE_YEAR, pl, sep = "_")

        p <- ggplot(results, aes(TIMESTAMP, Value, color = F_MAD)) +
            geom_point(size = I(0.25), na.rm = TRUE) +
            scale_color_manual(values = c("black", "red")) +
            facet_grid(threshold ~ unit) +
            ggtitle(outfn)

        path <- file.path(OUTPUT, rn)
        if(!dir.exists(path)) dir.create(path)
        ggsave(file.path(path, paste0(outfn, ".png")), height = 12, width = 10, plot = p)
    } # for x
}

message("All done!")
