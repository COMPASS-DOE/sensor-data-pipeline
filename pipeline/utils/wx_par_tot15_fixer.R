# wx_par_tot15_fixer.R
# See https://github.com/COMPASS-DOE/sensor-data-pipeline/issues/287

f <- file.choose()

library(readr)
message("Reading ", f)
x <- read_lines(f)

# Read the lines of data, forcing everything to be character except
# the PAR_Tot_C_Tot columns that we want to fix
dat <- read_csv(I(x[c(-1, -3, -4)]),
                col_types = cols(
                    TIMESTAMP = col_character(),
                    RECORD = col_character(),
                    BattV_Min = col_character(),
                    Rain_mm_Tot = col_character(),
                    SlrFD_W_Avg = col_character(),
                    SlrTF_MJ_Tot = col_character(),
                    `WS_ms_WVc(1)` = col_character(),
                    `WS_ms_WVc(2)` = col_character(),
                    WS_ms_S_WVT = col_character(),
                    MaxWS_ms_Max = col_character(),
                    MaxWS_ms_TMx = col_character(),
                    AirT_C_Avg = col_character(),
                    AirT_C_Max = col_character(),
                    AirT_C_TMx = col_character(),
                    AirT_C_Min = col_character(),
                    AirT_C_TMn = col_character(),
                    VP_mbar_Avg = col_character(),
                    BP_mbar_Max = col_character(),
                    BP_mbar_TMx = col_character(),
                    BP_mbar_Min = col_character(),
                    BP_mbar_TMn = col_character(),
                    RH_Max = col_character(),
                    RH_Min = col_character(),
                    RHT_C_Max = col_character(),
                    RHT_C_Min = col_character(),
                    TiltNS_deg_Max = col_character(),
                    TiltNS_deg_TMx = col_character(),
                    TiltNS_deg_Min = col_character(),
                    TiltNS_deg_TMn = col_character(),
                    TiltWE_deg_Max = col_character(),
                    TiltWE_deg_TMx = col_character(),
                    TiltWE_deg_Min = col_character(),
                    PAR_Den_C_Avg = col_character(),
                    PAR_Tot_C_Tot = col_double(),
                    CVMeta = col_character()
                ), na = "7999")

message("Rows = ", nrow(dat))
message("Rows with PAR_Tot_C_Tot data = ", sum(!is.na(dat$PAR_Tot_C_Tot)))
n_high <- sum(dat$PAR_Tot_C_Tot > 1500, na.rm = TRUE)
message("PAR_Tot_C_Tot values over 1500 = ", n_high)
if(n_high == 0) {
    stop("It doesn't look like you should be 'fixing' this file!")
}

# Fix
message("Fixing...")
dat$PAR_Tot_C_Tot <- dat$PAR_Tot_C_Tot / 4

# Write out data, read, add Campbell header lines back
tf <- tempfile()
write_csv(dat, tf, na = "7999", quote = "needed")
x_out <- read_lines(tf)
x_out <- c(x[1:4], x_out[-1])

# Write to a modified version of the original file
bn <- basename(f)
dn <- dirname(f)
bn_out <- gsub(".dat", "_MODIFIED.dat", bn, fixed = TRUE)
message("Writing ", bn_out)
write_lines(x_out, file.path(dn, bn_out))

message("All done")
