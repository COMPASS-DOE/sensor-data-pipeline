# wx_par_tot15_fixer.R
# Helper script to correct past wx_par_tot15 data at a variety of sites
# See https://github.com/COMPASS-DOE/sensor-data-pipeline/issues/287
# BBL April 2025

# Note: this script should NOT have to be run in the future!
# It's committed to the repo as documentation of what was done,
# and how, and in case useful as a model in future work

library(readr)

# Prefix of files to fix -- files with potentially bad wx_par_tot15
#file_prefix <- "Compass_CRC_W_301_ClimaVue50_15min"
#file_prefix <- "Compass_OWC_W_321_ClimaVue50_15min"
#file_prefix <- "Compass_PTR_W_311_ClimaVue50_15min"
#file_prefix <- "Compass_GWI_W_411_ClimaVue50_15min"
file_prefix <- "Compass_MSM_W_401_ClimaVue50_15min"

# These times are when the wx_par_tot15 problem was fixed,
# via visual inspection of the L1 data
#fix_timestamp <- "2022-06-28 10:30:00" # CRC, OWC
#fix_timestamp <- "2023-03-10 10:30:00" # PTR
#fix_timestamp <- "2024-08-23 00:00:00" # GWI
fix_timestamp <- "2023-01-12 00:00:00" # MSM

# Where to put the edited files
EDITED <- "pipeline/data/Raw/Raw_edited/"

# Files to fix are assumed to be in the repo's data/Raw folder
files <- list.files("pipeline/data/Raw/Raw_original/",
                    pattern = file_prefix, full.names = TRUE)

for(f in files) {

    message("Reading ", f)
    x <- read_lines(f)

    # Read the lines of data, forcing everything to be character except
    # the PAR_Tot_C_Tot columns that we want to fix
    dat <- read_csv(I(x[c(-1, -3, -4)]),
                    col_types = cols(
                        TIMESTAMP = col_character(),
                        RECORD = col_character(),
                        SlrFD_W_Avg = col_character(),
                        SlrTF_MJ_Tot = col_character(),
                        Rain_mm_Tot = col_character(),
                        WS_ms_S_WVT = col_character(),
                        WindDir_D1_WVT = col_character(),
                        WindDir_SD1_WVT = col_character(),
                        MaxWS_ms_Max = col_character(),
                        MaxWS_ms_TMx = col_character(),
                        Invalid_Wind_Tot = col_character(),
                        AirT_C_Avg = col_character(),
                        AirT_C_Max = col_character(),
                        AirT_C_TMx = col_character(),
                        AirT_C_Min = col_character(),
                        AirT_C_TMn = col_character(),
                        VP_mbar_Avg = col_character(),
                        BP_mbar = col_character(),
                        BP_mbar_Max = col_character(),
                        BP_mbar_TMx = col_character(),
                        BP_mbar_Min = col_character(),
                        BP_mbar_TMn = col_character(),
                        RH = col_character(),
                        RHT_C_Avg = col_character(),
                        TiltNS_deg_Avg = col_character(),
                        TiltWE_deg_Avg = col_character(),
                        Strikes_Tot = col_character(),
                        Dist_km_Min = col_character(),
                        Dist_km_TMn = col_character(),
                        PAR_Den_C_Avg = col_character(),
                        PAR_Tot_C_Tot = col_double(),
                        CVMeta = col_character()
                    ), na = "7999")

    fixrows <- dat$TIMESTAMP < fix_timestamp
    n_fixrows <- sum(fixrows, na.rm = TRUE)
#    n_narows <- sum(is.na(dat$PAR_Tot_C_Tot))

    message("Rows = ", nrow(dat))
    message("Rows with PAR_Tot_C_Tot data = ", sum(!is.na(dat$PAR_Tot_C_Tot)))
    n_high <- sum(dat$PAR_Tot_C_Tot > 1500, na.rm = TRUE)
    message("PAR_Tot_C_Tot values over 1500 = ", n_high)
    message("fixrows = ", n_fixrows)

#    if(n_fixrows == 0 & n_narows == 0) next  # nothing to do
    if(n_fixrows == 0) next  # nothing to do

    # Fix
    message("Fixing...")
    dat$PAR_Tot_C_Tot[fixrows] <- dat$PAR_Tot_C_Tot[fixrows] / 4

    # Write out data, read, add Campbell header lines back
    tf <- tempfile()
    write_csv(dat, tf, na = "7999", quote = "needed")
    x_out <- read_lines(tf)
    x_out <- c(x[1:4], x_out[-1])

    # Write to a modified version of the original file
    bn <- basename(f)
    message("Writing ", bn)
    write_lines(x_out, file.path(EDITED, bn))

}

message("All done")
