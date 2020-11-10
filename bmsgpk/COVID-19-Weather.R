library(httr)

url <- "https://community-open-weather-map.p.rapidapi.com/onecall/timemachine"

queryString <- list(
  lat = "37.774929",
  lon = "-122.419418",
  dt = "1590094153"
)

response <- VERB("GET", url, add_headers(x_rapidapi-key = 'SIGN-UP-FOR-KEY', x_rapidapi-host = 'community-open-weather-map.p.rapidapi.com', '), query = queryString, content_type("application/octet-stream"))

content(response, "text")