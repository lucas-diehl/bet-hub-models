library(data.table)

dg_betting_outrights <- function(tour = "pga",
                                 market = "top_20",
                                 odds_format = "american",
                                 file_format = "json") {
  x <- dg_get(
    "betting-tools/outrights",
    list(
      tour = tour,
      market = market,
      odds_format = odds_format,
      file_format = file_format
    )
  )
  
  if (is.data.frame(x)) return(as.data.table(x))
  if (!is.null(x$odds) && is.data.frame(x$odds)) return(as.data.table(x$odds))
  if (!is.null(x$data) && is.data.frame(x$data)) return(as.data.table(x$data))
  
  # last resort
  as.data.table(x)
}