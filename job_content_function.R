library(readr)
library(stringr)
library(ggplot2)
job_content <- function(File_path) {
  file <- read.csv(File_path)
  keyword <- c("分析", "批判性思考", "邏輯", "積極學習", "自我學習", "問題解決", "自我管理", "創意", "創造力", "創新", "領導力", "社群影響力", "適應力", "抗壓性高", "理解能力", "溝通能力", "說服", "協商", "團隊合作")
  frequent <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  for (r in 1:length(file[[1]])) {
    for (w in 1:length(keyword)) {
      if (str_detect(file[[1]][r], keyword[w])) {
        frequent[w] <- frequent[w] + 1
      }
    }
  }
  result <- data.frame(
    關鍵詞 = keyword,
    出現次數 = frequent
  )
  ggplot(data = result) +
    geom_bar(mapping = aes(x = 關鍵詞, y = 出現次數), stat = "identity")
}
job_content(File_path)