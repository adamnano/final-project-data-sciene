#Data
library("readr")
Job_info <- read_csv("HR.csv")

#English
get_required_languages <- function(no_specify = FALSE) #split all the languages
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
total_language_vec <- get_required_languages(Job_info) #a vec of all the languages and 不拘
language_tb <- tibble(語言 = total_language_vec) #build a tibble
English_rate <- language_tb %>%
  filter(語言 == c("不拘", "英文")) %>%
  count(語言) %>%
  mutate(total = sum(n)) %>%
  mutate(rate = n / total * 100) %>%
  filter(語言 == "英文") #rate of jobs that required English 


#College degree

#Major#1
Major_required <- function(Job_info) {
  Major_requirement <- Job_info %>%
    select(科系要求)
  return(mean(Major_requirement[[1]] != "不拘"))
}
Major_required_rate <- Major_required(Job_info) #rate of jobs that required certain majors

#major#2
get_required_majors <- function(no_specify = FALSE)
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
total_major_vec <- get_required_majors(Job_info) #vec of all the majors and 不拘
major_vec <- total_major_vec[total_major_vec != "不拘"] #select words except 不拘

library("RColorBrewer") #make a wordcloud
library("tm")
docs <- Corpus(VectorSource(major_vec))
inspect(docs)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#tool
get_required_tools <- function(no_specify = FALSE)
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
total_tool_vec <- get_required_tools(Job_info) #vec of all the tools and 不拘
tool_vec <- total_tool_vec[total_tool_vec != "不拘"] #select words except 不拘

library("RColorBrewer") #make a wordcloud
library("tm")
docs <- Corpus(VectorSource(tool_vec))
inspect(docs)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#skill
get_required_skills <- function(no_specify = FALSE)
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
total_skill_vec <- get_required_skills(Job_info) #vec of all the tools and 不拘
skill_vec <- total_skill_vec[total_skill_vec != "不拘"] #select words except 不拘

library("RColorBrewer") #make a wordcloud
library("tm")
docs <- Corpus(VectorSource(skill_vec))
inspect(docs)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))




