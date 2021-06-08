library(rvest)
library(httr)
job_function <- function(search_page_link, attribute_selector) {
  resp <- GET(search_page_link)
  html <- content(resp)
  link <- html %>% html_nodes("#maincontent > div.container__left > ul > li:nth-child(1) div.item__job-info > div.item__job-position0.item__m--link > a") %>% html_attr("href")
  Job_attr <- vector("character", length = length(link))
  for (i in 1:length(link)) {
    resp_b <- GET(link[i])
    html_b <- content(resp_b)
    Job_attr[i] <- html_b %>% 
      html_nodes(attribute_selector) %>%
      html_text()
  }
  Job_info <- data.frame(
    index = 1:length(link),
    Job_attr = Job_attr
  )
  View(Job_info)
}
job_function("https://www.1111.com.tw/search/job?ks=%E8%A1%8C%E9%8A%B7%E5%AF%A6%E7%BF%92", "#job-detail-info > div:nth-child(4) > div.job-detail-panel-content > dl > dd:nth-child(8) > span")
