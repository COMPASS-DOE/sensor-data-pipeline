# L2-utils.R
# Utility functions, and test code, used exclusively by L2.qmd
# BBL May 2025

library(testthat)

# MAD (median absolute deviation) outlier test, based on how many
# multiples of the MAD a point is from the sample median
# See https://en.wikipedia.org/wiki/Median_absolute_deviation
# and e.g. https://doi.org/10.1016/j.jesp.2013.03.013
# Returns a logical vector of same length as input, with TRUE
# indicating that the corresponding value is an outlier
outliers_MAD <- function(x, threshold = 3, min_n = 100) {
    if(length(x) < min_n) return(rep(FALSE, length(x)))

    md <- mad(x, na.rm = TRUE)

    # return vector of flags, where TRUE signals an outlier
    return(abs(x - median(x, na.rm = TRUE)) > md * threshold)
}

do_outlier_test <- function(x, algorithm, time_grouping, otherparams) {

    # Split data
    message("\t\tTime grouping is ", time_grouping)
    time_groups <- lubridate::round_date(x$TIMESTAMP, time_grouping)
    x_split <- split(x, time_groups)
    message("\t\tData splits: ", length(x_split))

    function_name <- paste0("outliers_", algorithm)
    flag_name <- paste0("F_", algorithm)

    # Construct expression (the call to QAQC_mad) to evaluate
    if(is.na(otherparams)) {
        expr <- paste(function_name, "(x$Value)")
    } else {
        expr <- paste(function_name, "(x$Value,", otherparams, ")")
    }

    # For each group, call the QAQC function
    results <- lapply(x_split, function(x) {
        x[[flag_name]] <- eval(parse(text = expr)) # f(x$Value)
        x
    })

    # Bind and return; we don't do any filtering here
    return(do.call("rbind", results))
}

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

# Gap-fill the L2 data: linearly interpolate gaps up to size 'maxgap'
# We do each plot, instrument, and sensor separately
L2_linear_gapfill <- function(x, maxgap) {
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
