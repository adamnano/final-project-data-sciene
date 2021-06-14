library(readr)
library(dplyr)
Major_required <- function(file) {
  Job_info <- read_csv(file)
  Major_requirement <- Job_info %>%
    select(科系要求)
  return(mean(Major_requirement[[1]] != "不拘"))
}
Major_required(File_location)