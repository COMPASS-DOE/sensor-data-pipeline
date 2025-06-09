# Programmatically get the list of data releases
# Note that git needs to be installed on your system for this to work
# BBL May 2025

tags <- system2("git",
                args = c("ls-remote", "--tags",
                         "https://github.com/COMPASS-DOE/data-workflows.git"),
                stdout = TRUE)
releases <- gsub("^.*/tags/", "", tags)

message("The latest COMPASS-FME sensor data release is ", tail(releases, 1))

# All done!
