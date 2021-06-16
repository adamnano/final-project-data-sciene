library(readr)
library(dplyr)
##Jobs <- read_csv("./job_info.csv")

# get job required majors
get_required_majors <- function(Job_info, no_specify = FALSE)
{
  Job_major <- Job_info %>% select(科系要求)
  if (no_specify == FALSE) Job_major <- Job_major %>% filter(科系要求 != "不拘")
  required_major <- vector("character", length = length(Job_major))
  for (i in 1:length(Job_major[[1]])) {
    required_major <- c(required_major, strsplit(Job_major[[1]][i], split = "、"))
  }
  required_major <- required_major %>% unlist() %>% unname()
  return(required_major)
}
  
# get job required languages
get_required_languages <- function(Job_info, no_specify = FALSE)
{
  Job_language <- Job_info %>% select(語文條件)
  if (no_specify == FALSE) Job_language <- Job_language %>% filter(語文條件 != "不拘")
  required_language <- vector("character", length = length(Job_language))
  for (i in 1:length(Job_language[[1]]))
  {
    required_language <- c(required_language, sapply(strsplit(Job_language[[1]][i], split = "  ")[[1]], function(x) return(strsplit(x, split = " -- ")[[1]][1])))
  }
  required_language <- required_language %>% unlist() %>% unname()
  return(required_language)
}

get_required_skills <- function(Job_info, no_specify = FALSE)
{
  Job_skill <- Job_info %>% select(工作技能) 
  if (no_specify == FALSE) Job_skill <- Job_skill %>% filter(工作技能 != "不拘")
  required_skill <- vector("character", length = length(Job_skill))
  for (i in 1:length(Job_skill[[1]]))
  {
    required_skill <- c(required_skill, strsplit(Job_skill[[1]][i], split = "、"))
  }
  required_skill <- required_skill %>% unlist() %>% unname()
  return(required_skill)
}

get_required_tools <- function(Job_info, no_specify = FALSE)
{
  Job_tool <- Job_info %>% select(擅長工具)
  if (no_specify == FALSE) Job_tool <- Job_tool %>% filter(擅長工具 != "不拘")
  required_tool <- vector("character", length = length(Job_tool))
  for (i in 1:length(Job_tool[[1]]))
  {
    required_tool <- c(required_tool, strsplit(Job_tool[[1]][i], split = "、"))
  }
  required_tool <- required_tool %>% unlist() %>% unname()
  return (required_tool)
}

get_least_degree <- function(Job_info, no_specify = TRUE)
{
  Job_degree <- Job_info %>% select(學歷要求)
  if (no_specify == FALSE) Job_degree <- Job_degree %>% filter(學歷要求 != "不拘")
  required_degree <- sapply(Job_degree[[1]], function(x){return(strsplit(x, split = "、")[1])}) %>% sapply(function(x){return(x[1] %>% str_trim())}) %>% unlist() %>% unname()
  return(required_degree)
}




# creating dataframe
#new_df <- data.frame(
#  major = required_major,
#)
#new_df <- new_df %>%
#  filter(major != "")
#View(new_df)
