# Install the 'devtools' package to manage specific version installations if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# List of required packages and their respective versions
required_packages <- c("dplyr", "tsbox", "readxl", "rsdmx", "lubridate", 
                       "zoo", "ggplot2", "car", "sandwich", "dlm", 
                       "data.table", "tidyverse", "xtable")

package_versions <- c("1.1.3", "0.4.1", "1.4.3", "0.6-3", "1.9.2", 
                      "1.8-12", "3.4.3", "3.1-2", "3.0-2", "1.1-6", 
                      "1.14.8", "2.0.0", "1.8-4")

# Function to install specific version of a package if not already installed
for (i in seq_along(required_packages)) {
  if (!requireNamespace(required_packages[i], quietly = TRUE)) {
    devtools::install_version(required_packages[i], version = package_versions[i])
  }
}

# Notify user that the packages are installed
message("All necessary packages have been installed with specified versions.")
