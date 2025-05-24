# L2-utils.R
# Utility functions, and test code, used exclusively by L2.qmd
# BBL May 2025


# MAD (mean absolute deviation) outlier test, based on how many
# multiples of the MAD a point is from the sample median
# See https://en.wikipedia.org/wiki/Median_absolute_deviation
# and e.g. https://doi.org/10.1016/j.jesp.2013.03.013
# Returns a logical vector of same length as input, with TRUE
# indicating that the corresponding value is an outlier
QAQC_mad <- function(x, threshold = 3, min_n = 100) {
    if(length(x) < min_n) return(rep(FALSE, length(x)))

    md <- mad(x, na.rm = TRUE)

    # return vector of flags
    return(abs(x - median(x, na.rm = TRUE)) > md * threshold)
}

do_outlier_test <- function(x, QAQC_name, time_grouping, otherparams) {

    # Split data
    message("\t\tTime grouping is ", time_grouping)
    time_groups <- lubridate::round_date(x$TIMESTAMP, time_grouping)
    x_split <- split(x, time_groups)
    message("\t\tData splits: ", length(x_split))

    flag_name <- paste0("F_", gsub("QAQC_", "", QAQC_name))

    # Construct expression (the call to QAQC_mad) to evaluate
    if(is.na(otherparams)) {
        expr <- paste(QAQC_name, "(x$Value)")
    } else {
        expr <- paste(QAQC_name, "(x$Value,", otherparams, ")")
    }

    # For each group, call the QAQC function
    results <- lapply(x_split, function(x) {
        x[[flag_name]] <- eval(parse(text = expr)) # f(x$Value)
        x
    })

    # Bind and return; we don't do any filtering here
    return(do.call("rbind", results))
}

# Complete the L2 data: smooth time series for all combinations of metadata
# We do each plot separately because they may have been
# established at different points in time
L2_complete <- function(x) {
    x_split <- split(x,
                     list(x$Site, x$Plot))
    y <- lapply(x_split, function(x) {
        timespan <- seq.POSIXt(min(x$TIMESTAMP),
                               max(x$TIMESTAMP),
                               by = params$timestamp_round)
        complete(x,
                 TIMESTAMP = timespan,
                 nesting(Site, Plot, Instrument, Instrument_ID, Sensor_ID, Location),
                 fill = list(Value = NA_real_, n = 0, n_drop = 0))
    })
    do.call("rbind", y)
}

# Gap-fill the L2 data: linearly interpolate gaps up to size 'maxgap'
# We do each plot, instrument, and sensor separately
L2_gapfill <- function(x, maxgap) {
    x_split <- split(x, list(x$Site, x$Plot, x$Instrument,
                             x$Instrument_ID, x$Sensor_ID))
    y <- lapply(x_split, function(z) {
        z <- z[order(z$TIMESTAMP),]
        z$Value <- zoo::na.approx(z$Value, maxgap = maxgap, na.rm = FALSE)
        z
    })
    do.call("rbind", y)
}
