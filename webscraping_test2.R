library(rvest)
library(httr)
resp <- GET("https://www.1111.com.tw/search/job?ks=%E5%AF%A6%E7%BF%92%E7%94%9F&d0=130103&page=1")
html <- content(resp)
link <- html %>% html_nodes("#maincontent > div.container__left > ul > li:nth-child(1) div.item__job-info > div.item__job-position0.item__m--link > a") %>% html_attr("href")
Job_attr <- vector("character", length = length(link))
for (i in 1:length(link)) {
  resp_b <- GET(link[i])
  html_b <- content(resp_b)
  Job_attr[i] <- html_b %>% 
    html_nodes("#job-detail-info > div:nth-child(4) > div.job-detail-panel-content > dl > dd:nth-child(6) > span") %>%
    html_text()
}
Job_attr