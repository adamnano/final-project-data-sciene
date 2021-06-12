library(readr)
library(dplyr)
Job_info <- read_csv("檔案路徑")
Job_major <- Job_info %>%
  select(科系要求) %>%
  filter(科系要求 != "不拘")
new_vector <- vector("character", length = length(Job_major))
for (i in 1:length(Job_major)) {
  new_vector <- c(new_vector, strsplit(Job_major[[i]], split = "、"))
}
new_vector <- unlist(new_vector)
new_df <- data.frame(
  major = new_vector
)
new_df <- new_df %>%
  filter(major != "")
View(new_df)