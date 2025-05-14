# Locate and read in L1 files from the Google Drive

# Settings

SITE <- "CRC" # change this to e.g. "CRC_W" if you want a particular plot
L1_VERSION <- "v1-2"
DOWNLOAD_PATH <- "./" # where to save the data files to

library(googledrive)

# If you're not authenticated to Google Drive, running this script
# will immediately produce a message similar to this one:

## The googledrive package is requesting access to your Google account.
## Enter '1' to start a new auth process or select a pre-authorized
## account.
## 1: Send me to the browser for a new auth process.
## 2: <email>

# You probably want to select '1'; see the help page for
# googledrive::drive_auth() for more information

# Locate the root of the data release

message("Looking for ", L1_VERSION, " on Google Drive...")
root <- drive_find(pattern = L1_VERSION, n_max = 30, type = "folder")
if(nrow(root) > 1) {
    stop("Hmm, there seem to be multiple folders named ", L1_VERSION)
}

# Construct a 'regular expression' to find the files we want:
# in this case, CSV files starting with the site code above
regex <- paste0("^", SITE, ".*", L1_VERSION)

# Get the file list; this might be slow
files <- drive_ls(path = root$id[1],
                  pattern = regex, #"^CRC.*v1-2",
                  type = "csv",
                  n_max = 3000,
                  recursive = TRUE)

# Download the data, saving the files to DOWNLOAD_PATH

for(i in seq_len(nrow(files))) {
    message("Downloading ", files$name[i])
    drive_download(files$id[i],
                   path = file.path(DOWNLOAD_PATH, files$name[i]))
}

# All done!
