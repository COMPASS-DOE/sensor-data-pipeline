# L2-utils.R
# Utility functions, and test code, used exclusively by L1.qmd
# BBL June 2025

library(testthat)

# Gap-filling functions -----------------------------------

# Fill a gap in a vector x, based on the values immediately before
# and after the gap and the mean annual cycle (mac)
# Returns a numeric vector exactly as long as the gap
fill_gap <- function(x, gap_begin, gap_end, mac) {
    if(!gap_end >= gap_begin) {
        stop("I am seeing ", gap_begin, " ", gap_end, " in ", length(x))
    }
    stopifnot(gap_end >= gap_begin)
    stopifnot(gap_begin > 0 && gap_begin <= length(x))
    stopifnot(gap_end > 0 && gap_end <= length(x))
    stopifnot(length(x) == length(mac))

    # x and mac are numeric vectors of equal length
    # gap_begin and gap_end are the indices of the first and last NAs of the gap

    if(gap_begin > 1) {
        left_adjust <- x[gap_begin - 1] - mac[gap_begin - 1]
    } else {
        left_adjust <- 0
    }
    #message("left_adjust ", left_adjust)
    if(gap_end < length(x)) {
        right_adjust <- x[gap_end + 1] - mac[gap_end + 1]
    } else {
        right_adjust <- 0
    }
    #message("right_adjust ", right_adjust)

    # Calculate adjustments for the gap points plus the pre- and post-gap
    # good data points that make up the calculation of left_adjust and right_adjust
    adj <- seq(left_adjust, right_adjust, length.out = gap_end - gap_begin + 3)
    # ...but then drop those out-of-gap adjustment values
    adj <- adj[c(-1, -length(adj))]
    # Return the adjusted mean annual cycle values to fill the gap
    mac[gap_begin:gap_end] + adj
}

# Test code
expect_identical(fill_gap(c(1, 2, NA), 3, 3, 1:3), 3)
expect_identical(fill_gap(c(1, 2, NA), 3, 3, c(1, 2, NA)), NA_real_)
expect_equivalent(fill_gap(c(1, NA, NA, 4), 2, 3, 1:4), 2:3)
expect_equivalent(fill_gap(c(2, NA, NA, 5), 2, 3, 1:4), 3:4)

expect_error(fill_gap(1:2, 3, 2), regexp = "I am seeing")
expect_error(fill_gap(1:2, 1, 1, 1), regexp = "length")
expect_error(fill_gap(1:2, -1, 2), regexp = "gap_begin")


# Find the gaps (NAs) in a vector and return a data frame listing their
# start and end positions
find_gaps <- function(x) {
    stopifnot(length(x) > 0)

    rle_x <- rle(is.na(x))
    # Compute endpoints of run
    end <- cumsum(rle_x$lengths)
    start <- c(1, head(end, -1) + 1)
    data.frame(start, end)[rle_x$values,] # return only TRUE value gaps
}

# Test code
expect_equivalent(find_gaps(c(1, NA, 3)), data.frame(start = 2, end = 2))
expect_equivalent(find_gaps(c(1, NA, NA, 3)), data.frame(start = 2, end = 3))
expect_equivalent(find_gaps(c(1, NA)), data.frame(start = 2, end = 2))
expect_equivalent(find_gaps(c(1, NA, 3, NA, 5)),
                  data.frame(start = c(2, 4), end = c(2, 4)))
expect_equivalent(find_gaps(NA), data.frame(start = 1, end = 1))

expect_identical(nrow(find_gaps(c(1, 2))), 0L) # 0-row d.f. if no gaps
expect_identical(nrow(find_gaps(c(1))), 0L) # 0-row d.f. if no gaps
expect_error(find_gaps())

# Fill all the gaps in a vector based on the mean annual cycle
fill_all_gaps <- function(x, mac) {
    stopifnot(length(x) == length(mac))

    gaps <- find_gaps(x)
    gapfilled <- rep(NA_real_, length(x))

    for(i in seq_len(nrow(gaps))) {
        thisgap <- gaps$start[i]:gaps$end[i]
        gapfilled[thisgap] <- fill_gap(x, gaps$start[i], gaps$end[i], mac)
        # Just to make the graphs look good, return the boundary points too
        # This code (these two ifs) should not be in production code
        if(gaps$start[i] > 1)
            gapfilled[gaps$start[i] - 1] <- x[gaps$start[i] - 1]
        if(gaps$end[i] < length(x))
            gapfilled[gaps$end[i] + 1] <- x[gaps$end[i] + 1]
    }
    return(round(gapfilled, 4))
}

# Derived variable functions -----------------------------------
# Name of the function must start with "CALC_DERIVED" followed by
# name of new research name (from derived variables table)

# Generate full range of possible VWC values and corresponding roots
# This code is outside a function because we only need to do it once
VWCs <- seq(0, 1, length.out = 100)
get_roots <- function(VWC) {
    # Polynomial: -10.848-VWC + 1.302e-2*x - 5.105e-6*x^2 + 6.771e-10*x^3 = 0
    coeffs <- c(-10.848 - VWC, 1.302e-2, -5.105e-6, 6.771e-10)
    roots <- polyroot(coeffs)
    # Return real part of real roots
    rrt <- Re(roots[abs(Im(roots)) < 1e-8])
    if(length(rrt)) {
        return(rrt)
    } else {
        return(NA)
    }
}
raws <- sapply(VWCs, get_roots)
raws <- zoo::na.approx(raws) # fill in five missing values (when no real roots)

compute_salinity <- function(T, VWC, EC) {
    # The three data frames passed in should *normally* have the
    # exact same dimensions and ordering, since they're linked
    # TEROS data from the same site and plot
    if(length(unique(sapply(list(T, VWC, EC), length))) != 1) {
        stop("The T,VWC,EC data frames are not the same size!")
    }

    # The following steps are from Fausto Machado-Silva
    # See also Hilhorst 2000, A pore water conductivity sensor,
    # Soil Science Society of America Journal 64:6 1922–1925.

    # 1. Convert TEROS EC to porewater EC
    # METER recommends that σp not be calculated in soils with
    # VWC < 0.10 m3/m3 using this method
    VWC[VWC < 0.1] <- NA

    # Convert conductivity to porewater conductivity
    ep <- 80.3 - 0.37 * (T - 20)
    esb0 <- 4.1
    # Interpolate raw values for given VWCs
    raw <- approx(VWCs, raws, xout = VWC)$y

    eb <- (2.887e-9 * raw ** 3 - 2.080e-5 * raw ** 2 + 5.276e-2 * raw - 43.39) ** 2
    cond_porewater <- (ep * EC) / (eb - esb0)

    # 2. Convert porewater EC to salinity
    -2.060e-1 + 5.781e-4 * cond_porewater
}

`CALC_DERIVED_soil-salinity-10cm` <- function(x) {

    T <- x$`soil-temp-10cm`$Value
    VWC <- x$`soil-vwc-10cm`$Value
    EC <- x$`soil-EC-10cm`$Value

    # Pick one of our input data frames, overwrite its Value column,
    # and return it. The calling code in L2.qmd will take care of changing
    # the research_name and Type columns
    x$`soil-EC-10cm`$Value <- compute_salinity(T, VWC, EC)
    return(x$`soil-EC-10cm`)
}

`CALC_DERIVED_soil-salinity-30cm` <- function(x) {

    T <- x$`soil-temp-30cm`$Value
    VWC <- x$`soil-vwc-30cm`$Value
    EC <- x$`soil-EC-30cm`$Value

    # Pick one of our input data frames, overwrite its Value column,
    # and return it. The calling code in L2.qmd will take care of changing
    # the research_name and Type columns
    x$`soil-EC-30cm`$Value <- compute_salinity(T, VWC, EC)
    return(x$`soil-EC-30cm`)
}


# Well dimensions, for computing water level below surface
WELL_DIMENSIONS <- read_csv("metadata/L2_metadata/well_dimensions.csv",
                            comment = "#",
                            col_types = "ccddcddd")
WELL_DIMENSIONS$Plot <- c("Wetland" = "W",
                          "Swamp" = "SWAMP",
                          "Transition" = "TR",
                          "Upland" = "UP")[WELL_DIMENSIONS$transect_location]
WELL_DIMENSIONS$ground_to_sensor_cm <-
    with(WELL_DIMENSIONS, ring_to_pressure_sensor_cm - (well_top_to_ground_cm - bolt_to_cap_cm))
WELL_DIMENSIONS <- WELL_DIMENSIONS[c("Site", "Plot", "ground_to_sensor_cm")]

`CALC_DERIVED_gw-wl-below-surface` <- function(x) {
    # The two data frames passed in should *normally* have the
    # exact same dimensions and ordering, since they're linked
    # TEROS data from the same site and plot
    gw_density <- x$`gw-density`$Value
    gw_pressure <- x$`gw-pressure`$Value
    temp <- merge(x$`gw-density`, WELL_DIMENSIONS, by = c("Site", "Plot"))
    ground_to_sensor_cm <- temp$ground_to_sensor_cm

    # This follows Peter Regier's code, and Fausto M-S's logic
    density_gcm3_cor <- ifelse(gw_density >= 0.98 & gw_density <= 1.05, gw_density, 1)
    pressure_mbar <- ifelse(gw_pressure == -99999, 0, gw_pressure)
    pressurehead_m <- (pressure_mbar * 100) / (density_gcm3_cor * 1000 * 9.80665)

    # Pick one of our input data frames, overwrite its Value column,
    # and return it. The calling code in L2.qmd will take care of changing
    # the research_name and Type columns
    x$`gw-density`$Value <- round(pressurehead_m - (ground_to_sensor_cm / 100), 4)
    return(x$`gw-density`)
}
