# List the 'important' files in the process system
# This list is then used to build the docs/structure.md file

files <- list.files(recursive = TRUE, full.names = TRUE, include.dirs = TRUE)

files <- files[grep("^./data/.+", files, invert = TRUE)]

files <- files[grep("^./data_TEST/Raw/.+", files, invert = TRUE)]
files <- files[grep("^./data_TEST/L0/.+", files, invert = TRUE)]
files <- files[grep("^./data_TEST/L1_normalize/.+", files, invert = TRUE)]
files <- files[grep("^./data_TEST/L1/.+", files, invert = TRUE)]
files <- files[grep("^./data_TEST/Logs/.+", files, invert = TRUE)]

files <- files[grep("^./L0_files/.+", files, invert = TRUE)]
files <- files[grep("^./L1_normalize_files/.+", files, invert = TRUE)]
files <- files[grep("^./L1_files/.+", files, invert = TRUE)]

files <- files[grep("html$", files, invert = TRUE)]
files <- files[grep("README.md$", files, invert = TRUE)]

x <- data.frame(dir = dirname(files), file = basename(files))
x <- x[order(x$dir),]


writeLines(paste(paste0("`", x$file, "`"), "| description"), "files.txt")

