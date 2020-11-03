library(lubridate)
library(httr)

slackMsg <- function (url=NULL, title, msg, quote="", code="") {
  
  if (is.null(url)) 
    url<-"https://hooks.slack.com/services/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"
  
  # data structure required by slack (jsonified)
  body <- list(text = 
                 paste(paste0(now(),"\n*",title,"*\n"), msg,
                       paste0("\n> ",quote),
                       paste0("\n```"),code,"```"))
  
  r <- POST(incoming_webhook_url, content_type_json(), body = body, encode = "json")
  invisible(r)
}