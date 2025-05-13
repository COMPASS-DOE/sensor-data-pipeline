# Programmatically get the list of data releases

tags <- system2("git",
                args = c("ls-remote", "--tags", "https://github.com/COMPASS-DOE/data-workflows.git"),
                stdout = TRUE)
releases <- gsub("^.*/tags/", "", tags)

message("The latest COMPASS-FME sensor data release is ", tail(releases, 1))

# All done!
