#' API connection
#'
#' @param start_date character
#' @param end_date character
#' @return dataframe
#' @export
#' @import httr jsonlite

spacejam <- function(start_date, end_date){
  stopifnot({
    is.character(start_date) && is.character(end_date)
  })
  start_date <- as.Date(start_date, format = ("%Y-%m-%d"))
  end_date <- as.Date(end_date, format = ("%Y-%m-%d"))
  url <- paste0("https://api.nasa.gov/DONKI/CMEAnalysis?startDate=",
                start_date, "&endDate=", end_date,
                "&mostAccurateOnly=true&speed=500&halfAngle=30&catalog=ALL&api_key=",
                "mdy0Me6QgbdT9odj7fbn6pfQkshRpd1wEa5VZDau")
  res = GET(url)
  if (http_type(res) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed_content <- fromJSON(rawToChar(res$content))

  if (http_error(res)){
    stop(
      sprintf(
        "API request failed",
        status_code(res)
      ),
      call. = TRUE
    )
  }
  # Variables to include
  index <- c(1:length(parsed_content$time21_5))
  time <- parsed_content$time21_5
  speed <- parsed_content$speed
  type <- parsed_content$type

  df = data.frame(index, time, speed, type)

  # Response
  limit = as.character(res[3]$headers$`x-ratelimit-limit`)
  remain = res[3]$headers$`x-ratelimit-remaining`
  usage = paste0(as.character(remain), "/", limit)
  call_time = res$times[6]
  status = status_code(res)
  response = data.frame("usage" = usage,
                        "call_time" = call_time,
                        "status" = status)

  if(remain == 0){
    stop("Exceeded call limit:", usage)
  }
  structure(
    list(data = df,
         response = response),
    class = "spacejam_API")
}
