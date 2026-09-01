library(httr2)
library(data.table)

`%||%` <- function(a, b) if (!is.null(a)) a else b

dg_get <- function(endpoint, params = list(), timeout_sec = 60, to_file = FALSE) {
  key <- Sys.getenv("DATAGOLF_API_KEY")
  if (identical(key, "") || is.na(key)) {
    stop("DATAGOLF_API_KEY missing in .Renviron; restart R.")
  }
  
  base <- "https://feeds.datagolf.com"
  url  <- paste0(base, "/", endpoint)
  
  # DataGolf expects key= in query string
  params$key <- key
  
  # 1) Build request
  req <- httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_options(timeout = timeout_sec)
  
  # 2) Only error on HTTP >= 400, but print body for debugging
  req <- httr2::req_error(
    req,
    is_error = ~ httr2::resp_status(.x) >= 400,
    body = ~ httr2::resp_body_string(.x)
  )
  
  # 3) Perform
  if (isTRUE(to_file)) {
    ext <- if (!is.null(params$file_format) && identical(params$file_format, "csv")) ".csv" else ".json"
    f <- tempfile(fileext = ext)
    httr2::req_perform(req, path = f)
    return(f)
  } else {
    resp <- httr2::req_perform(req)
    return(httr2::resp_body_json(resp, simplifyVector = TRUE))
  }
}
dg_try <- function(endpoint, params = list()) {
  tryCatch(
    dg_get(endpoint, params, timeout_sec = 60, to_file = FALSE),
    error = function(e) e
  )
}