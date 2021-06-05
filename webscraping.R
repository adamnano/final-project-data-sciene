library(rvest)
library(httr)
resp <- GET("https://www.104.com.tw/jobs/search/?ro=0&isnew=14&kwop=7&keyword=%E5%AF%A6%E7%BF%92%20intern&order=7&asc=0&sr=99&rostatus=1024&page=2&mode=s&jobsource=intern_hot")
html <- content(resp)
link <- html %>% html_nodes("#js-job-content div.b-block__left > h2 > a") %>% html_attr("href")
for (x in 1:length(link)) {
  link[x] <- paste0("https:", link[x])
}
Job_attr <- vector("character", length = length(link))
for (i in 1:length(link)) {
  resp_b <- GET(link[i])
  html_b <- content(resp_b)
  Job_attr[i] <- html_b %>% 
    html_nodes("#app > div.container.jb-container.pt-4.position-relative > div > div.col.main > div.dialog.container-fluid.bg-white.rounded.job-description.mb-4.pt-6.pb-6 > div.job-description-table.row > div:nth-child(9) > div.col.p-0.job-description-table__data > p") %>%
    html_text()
}
Job_attr