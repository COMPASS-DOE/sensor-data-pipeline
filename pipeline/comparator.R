# Build a list of differences between v1-2 raw and Dropbox

dropbox_raw <- c("~/Dropbox/COMPASS_PNNL_Data/COMPASS_PNNL_Rawdata_Archive/",
                 "~/Dropbox/GCREW_LOGGERNET_DATA/archive_data/",
                 "~/Dropbox/TEMPEST_PNNL_Data/Loggernet_Rawdata_Archive/")
patterns <- c("\\.dat$", "^GCREW_MET_GCREW_MET_15min", "\\.dat$")
drfiles <- c()
for(i in seq_along(dropbox_raw)) {
    drfiles <- append(drfiles, list.files(dropbox_raw[i], patterns[i], full.names = TRUE))
}

pipeline_raw <- "data/Raw/"
prfiles <- list.files(pipeline_raw, "\\.dat$", full.names = TRUE, recursive = TRUE)

message("There are ", length(drfiles), " Dropbox raw files")
message("There are ", length(prfiles), " Dropbox raw files")

# Identify files that are in Dropbox but not Raw/
# These are either new (logged since last data release) or were removed
removals <- setdiff(basename(drfiles), basename(prfiles))
message("There are ", length(removals), " Dropbox files but not Raw")
recents <- grep("_2025", removals)
message("...of these ", length(recents), " have a '_2025' pattern")

# Identify identically-named files whose contents differ
differs <- rep(FALSE, length(drfiles))
pb <- txtProgressBar(1, length(drfiles))
for(i in seq_along(drfiles)) {
    setTxtProgressBar(pb, i)
    dfile <- drfiles[i]
    dfile_base <- basename(dfile)
    if(dfile_base %in% removals) next
    pfile <- prfiles[grep(dfile_base, prfiles, fixed = TRUE)]

    df_md5 <- digest::digest(file = dfile, algo = "md5")
    pf_md5 <- digest::digest(file = pfile, algo = "md5")
    differs[i] <- df_md5 != pf_md5
}

message("\nThere are ", sum(differs), " Dropbox files whose contents differ from Raw")
