# L2-utils.R
# Utility functions, and test code, used exclusively by L2.qmd
# BBL May 2025


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
L2_aggregate <- function(x) {
    # This would be easier and faster to do in dplyr or data.table but
    # I'm trying to minimize dependencies; so...
    x$Value[as.logical(x$F_drop)] <- NA

    # aggregate() chokes on NAs in grouping variables
    x <- replace_na(x, list(Instrument_ID = "", Sensor_ID = "", Location = ""))

    x_summarised <- aggregate(Value ~ Site + Plot + TIMESTAMP + Instrument +
                                  Instrument_ID + Sensor_ID + Location,
                              data = x,
                              FUN = mean, na.action = na.omit, drop = FALSE)
    # Summarise number of used and not used values
    x$F_keep <- !x$F_drop
    x2 <- aggregate(F_keep ~ Site + Plot + TIMESTAMP + Instrument +
                        Instrument_ID + Sensor_ID + Location,
                    data = x,
                    FUN = sum, na.action = na.omit, drop = FALSE)

    x3 <- aggregate(F_drop ~ Site + Plot + TIMESTAMP + Instrument +
                        Instrument_ID + Sensor_ID + Location,
                    data = x,
                    FUN = sum, na.action = na.omit, drop = FALSE)
    x_summarised$n <- x2$F_keep
    x_summarised$n_drop <- x3$F_drop
    return(x_summarised)
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
