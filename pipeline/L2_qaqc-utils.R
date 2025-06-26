# L2-utils.R
# Utility functions, and test code, used exclusively by L2.qmd
# BBL May 2025

library(testthat)


# Aggregate (average) the data for each timestep, computing `n` and `n_drop`
# I have been trying not to use dplyr, a heavyweight dependency, but all
# the base R solutions for this step are super slow and we are short on time
library(dplyr)
L2_aggregate <- function(x) {
    x %>%
        group_by(Site, Plot, TIMESTAMP, Instrument, Instrument_ID,
                 Sensor_ID, Location, research_name) %>%
        summarise(Value = mean(Value[!F_drop]),
                  N_avg = sum(!is.na(F_drop) & !is.na(Value), na.rm = TRUE),
                  N_drop = sum(F_drop, na.rm = TRUE),
                  .groups = "drop") ->
        x_out

    x_out$Value[is.nan(x_out$Value)] <- NA
    return(x_out)
}


# Test the L2_aggregate function on various groups
test <- data.frame(Site = 1, Plot = 1, TIMESTAMP = 1, Instrument = 1,
                   Sensor_ID = 1, Location = 1, research_name = 1,
                   Instrument_ID = c(1, 1, 2, 3, 4, 4),
                   Value = c(1, 2, NA, NA, 3, 4),
                   F_drop = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE))
test_out <- L2_aggregate(test)
expect_equal(test_out$Instrument_ID, 1:4)
expect_equal(test_out$Value, c(1.5,  # Average of two good values
                               NA,   # F_drop TRUE for all good values
                               NA,   # F_drop FALSE but Value is NA
                               3.0)) # Two good values but F_drop TRUE for one


# Complete the L2 data: produce a smooth (no gaps, though data may be NA)
# time series for all combinations of metadata
# We do each plot separately because they may have been
# established at different points in time
L2_complete <- function(x) {
    x_split <- split(x,
                     list(x$Site, x$Plot))
    y <- lapply(x_split, function(x) {
        begin <- paste0(year(min(x$TIMESTAMP)), "-01-01 00:00")
        end <- paste0(year(max(x$TIMESTAMP)), "-12-31 23:45")
        timespan <- seq.POSIXt(ymd_hm(begin),
                               ymd_hm(end),
                               by = params$timestamp_round)
        complete(x,
                 TIMESTAMP = timespan,
                 nesting(Site, Plot, Instrument, Instrument_ID, Sensor_ID, Location, research_name),
                 fill = list(Value = NA_real_, n = 0, n_drop = 0))
    })
    do.call("rbind", y)
}

# Linear interpolate small gaps in the L2 data, up to size 'maxgap'
# We do each plot, instrument, and sensor separately
L2_linear_interp <- function(x, maxgap) {
    # split() chokes on NAs in grouping variables
    x <- replace_na(x, list(Instrument = "", Instrument_ID = "", Sensor_ID = ""))
    x_split <- split(x, ~Site + Plot + Instrument + Instrument_ID + Sensor_ID)
    y <- lapply(x_split, function(z) {
        z <- z[order(z$TIMESTAMP),]
        z$Value <- zoo::na.approx(z$Value, maxgap = maxgap, na.rm = FALSE)
        z
    })
    do.call("rbind", y)
}
