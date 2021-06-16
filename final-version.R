#Data
library("readr")
# files <- c("HR.csv", "文字_傳媒工作類.csv")
# Job_info <- sapply(files, read_csv)

Job_info1 <- read_csv("HR.csv")
Job_info2 <- read_csv("文字_傳媒工作類.csv")
Job_info3 <- read_csv("生產製造_品管_環衛類.csv")
Job_info4 <- read_csv("行政_總務_法務類.csv")
Job_info5 <- read_csv("行銷_企劃_專案管理類.csv")
Job_info6 <- read_csv("客服_門市_業務_貿易類.csv")
Job_info7 <- read_csv("研發相關類.csv")
Job_info8 <- read_csv("軍警消_保全類.csv")
Job_info9 <- read_csv("財會_金融專業類.csv")
Job_info10 <- read_csv("傳播藝術_設計類.csv")
Job_info11 <- read_csv("資材_物流_運輸類.csv")
Job_info12 <- read_csv("資訊軟體系統類.csv")
Job_info13 <- read_csv("學術_教育_輔導類.csv")
Job_info14 <- read_csv("操作_技術_維修類.csv")
Job_info15 <- read_csv("餐飲_旅遊_美容美髮類.csv")
Job_info16 <- read_csv("營建_製圖類.csv")
Job_info17 <- read_csv("醫療_保健服務類.csv")
Job <- list(Job_info1, Job_info2, Job_info3, Job_info4, Job_info5, Job_info6, Job_info7, Job_info8, 
            Job_info9, Job_info10, Job_info11, Job_info12, Job_info13, Job_info14, Job_info15, Job_info16, Job_info17)
field <- c("經營_人資類", "文字_傳媒工作類", "生產製造_品管_環衛類", "行政_總務_法務類", "行銷_企劃_專案管理類", 
           "客服_門市_業務_貿易類", "研發相關類", "軍警消_保全類", "財會_金融專業類", "傳播藝術_設計類", 
           "資材_物流_運輸類", "資訊軟體系統類", "學術_教育_輔導類", "操作_技術_維修類", "餐飲_旅遊_美容美髮類", 
           "營建_製圖類", "醫療_保健服務類")
#English
library("dplyr")
get_required_languages <- function(Job_language, no_specify = TRUE) #split all the languages
{ Job_language <- Job_language %>% select(語文條件)
  if (no_specify == FALSE) Job_language <- Job_language %>% filter(語文條件 != "不拘")
  required_language <- vector("character", length = length(Job_language))
  for (i in 1:length(Job_language[[1]]))
  {
    required_language <- c(required_language, sapply(strsplit(Job_language[[1]][i], split = "  ")[[1]], function(x) return(strsplit(x, split = " -- ")[[1]][1])))
  }
  required_language <- required_language %>% unlist() %>% unname()
  return(required_language)
}
languages_vecs <- sapply(Job, get_required_languages)

English_rates <- vector(mode = "list", length = 17)
for(i in 1:length(languages_vecs)){
total_language_vec <- languages_vecs[[i]] #a vec of all the languages and 不拘
language_tb <- tibble(語言 = total_language_vec) #build a tibble
English_rate <- language_tb %>%
  filter(語言 %in% c("不拘", "英文")) %>%
  count(語言) %>%
  mutate(total = sum(n)) %>%
  mutate(rate = n / total * 100) %>%
  filter(語言 == "英文") #rate of jobs that required English 
English_rates[[i]] <- English_rate
}
English_rates <- bind_rows(English_rates)
English_rates <- English_rates %>%
  mutate(領域 = field) %>%
  arrange(desc(rate))

library("ggplot2")
ggplot(English_rates, aes(reorder(x = 領域, -rate), y = rate)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "各領域需要英文能力的職缺比例",
       x = "領域", 
       y = "比例")

#College degree

#Major#1
Major_required <- function(Job_info) {
  Major_requirement <- Job_info %>%
    select(科系要求)
  return(mean(Major_requirement[[1]] != "不拘"))
}
Major_required_rate <- sapply(Job, Major_required) #rate of jobs that required certain majors
Major_rates <- tibble(rate = Major_required_rate * 100, 領域 = field)
ggplot(Major_rates, aes(reorder(x = 領域, -rate), y = rate)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "各領域需要特定科系的職缺比例",
       x = "領域", 
       y = "比例")


#major#2
get_required_majors <- function(Job_major, no_specify = FALSE)
{
  Job_major <- Job_major %>% select(科系要求)
  if (no_specify == FALSE) Job_major <- Job_major %>% filter(科系要求 != "不拘")
  required_major <- vector("character", length = length(Job_major))
  for (i in 1:length(Job_major[[1]])) {
    required_major <- c(required_major, strsplit(Job_major[[1]][i], split = "、"))
  }
  required_major <- required_major %>% unlist() %>% unname()
  return(required_major)
}
total_major_list <- sapply(Job, get_required_majors) #list of all the majors

library("RColorBrewer") #wordcloud
library("tm")
library("wordcloud")
seventeen_word_clouds <- for(i in 1:length(total_major_list)){ #17 word clouds of majors from 17 industries respectively
  docs <- Corpus(VectorSource(total_major_list[[i]]))
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
}
docs <- Corpus(VectorSource(unlist(total_major_list))) #1 word cloud including all majors from all industries
inspect(docs)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
one_word_cloud <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                            max.words=10, random.order=FALSE, rot.per=0.35, 
                            colors=brewer.pal(8, "Dark2"), scale = c(3, 0.2))


#tool
get_required_tools <- function(Job_tool, no_specify = FALSE)
{
  Job_tool <- Job_tool %>% select(擅長工具)
  if (no_specify == FALSE) Job_tool <- Job_tool %>% filter(擅長工具 != "不拘")
  required_tool <- vector("character", length = length(Job_tool))
  for (i in 1:length(Job_tool[[1]]))
  {
    required_tool <- c(required_tool, strsplit(Job_tool[[1]][i], split = "、"))
  }
  required_tool <- required_tool %>% unlist() %>% unname()
  return (required_tool)
}
total_tool_list <- sapply(Job, get_required_tools) #list of all the tools

library("RColorBrewer") #wordcloud
library("tm")
library("wordcloud")
seventeen_word_clouds <- for(i in 1:length(total_tool_list)){ #17 word clouds of tools from 17 industries respectively
  docs <- Corpus(VectorSource(total_tool_list[[i]]))
  inspect(docs)
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"), scale = c(3, 0.2))
}
docs <- Corpus(VectorSource(unlist(total_tool_list))) #1 word cloud including all tools from all industries
inspect(docs)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
one_word_cloud <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                            max.words=10, random.order=FALSE, rot.per=0.35, 
                            colors=brewer.pal(8, "Dark2"), scale = c(3, 0.2))



#skill
get_required_skills <- function(Job_skill, no_specify = FALSE)
{
  Job_skill <- Job_skill %>% select(工作技能) 
  if (no_specify == FALSE) Job_skill <- Job_skill %>% filter(工作技能 != "不拘")
  required_skill <- vector("character", length = length(Job_skill))
  for (i in 1:length(Job_skill[[1]]))
  {
    required_skill <- c(required_skill, strsplit(Job_skill[[1]][i], split = "、"))
  }
  required_skill <- required_skill %>% unlist() %>% unname()
  return(required_skill)
}
total_skill_list <- sapply(Job, get_required_skills) #list of all the skills

library("RColorBrewer") #wordcloud
library("tm")
library("wordcloud")
seventeen_word_clouds <- for(i in 1:length(total_skill_list)){ #17 word clouds of skills from 17 industries respectively
  docs <- Corpus(VectorSource(total_skill_list[[i]]))
  inspect(docs)
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"), scale = c(3, 0.2))
}
docs <- Corpus(VectorSource(unlist(total_skill_list))) #1 word cloud including all skills from all industries
inspect(docs)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
one_word_cloud <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                            max.words=10, random.order=FALSE, rot.per=0.35, 
                            colors=brewer.pal(8, "Dark2"), scale = c(3, 0.2))


#工作內容
library(readr)
Job_content1 <- read_csv("經營_人資類content.csv")
Job_content2 <- read_csv("文字_傳媒工作類content.csv")
Job_content3 <- read_csv("生產製造_品管_環衛類content.csv")
Job_content4 <- read_csv("行政_總務_法務類content.csv")
Job_content5 <- read_csv("行銷_企劃_專案管理類content.csv")
Job_content6 <- read_csv("客服_門市_業務_貿易類content.csv")
Job_content7 <- read_csv("研發相關類content.csv")
Job_content8 <- read_csv("軍警消_保全類content.csv")
Job_content9 <- read_csv("財會_金融專業類content.csv")
Job_content10 <- read_csv("傳播藝術_設計類content.csv")
Job_content11 <- read_csv("資材_物流_運輸類content.csv")
Job_content12 <- read_csv("資訊軟體系統類content.csv")
Job_content13 <- read_csv("學術_教育_輔導類content.csv")
Job_content14 <- read_csv("操作_技術_維修類content.csv")
Job_content15 <- read_csv("餐飲_旅遊_美容美髮類content.csv")
Job_content16 <- read_csv("營建_製圖類content.csv")
Job_content17 <- read_csv("醫療_保健服務類content.csv")


library(stringr)
library(ggplot2)
job_content <- function(File_path) {
  file <- read_csv(File_path)
  keyword <- c("分析", "批判性思考", "邏輯", "積極學習", "自我學習", "問題解決", "自我管理", "創意", "創造力", "創新", "領導力", "社群影響力", "適應力", "抗壓性高", "理解能力", "溝通能力", "說服", "協商", "團隊合作")
  frequent <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  for (r in 1:length(file[[1]])) {
    for (w in 1:length(keyword)) {
      if (str_detect(file[[2]][r], keyword[w])) {
        frequent[w] <- frequent[w] + 1
      }
    }
  }
  result <- data.frame(
    關鍵字 = keyword,
    出現次數 = frequent
  )
  ggplot(data = result) +
    geom_bar(mapping = aes(reorder(x = 關鍵字, -出現次數), y = 出現次數), stat = "identity") +
    labs(title = "行銷_企劃_專案管理類",
         x = "關鍵字") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

job_content("行銷_企劃_專案管理類content.csv")