fetch_historic_prices <- function(symbol) {
  url <- glue::glue("https://financialmodelingprep.com/api/v3/historical-price-full/{symbol}?serietype=line")
  content <- url %>%
    httr::GET() %>%
    httr::content()

  content <- content$historical

  dates_array <- sapply(content, function(x) x$date)
  prices_array <- sapply(content, function(x) x$close)


  tibble::tibble(date = as.Date(dates_array), price = as.numeric(prices_array), name = as.character(symbol))
}


#' Functions to call and retrieve financial data from remote databases, by using company tickers (symbols). It uses publicly available API to download all data.
#'
#' @return Returns a tibble
#' @param symbol Specifies the ticker or symbol the user wants to download the data of, in the form of a string. Example: 'MSFT'
#'
#'
#' @author Alejandro Jiménez Rico \email{aljrico@@gmail.com}, \href{https://aljrico.github.io}{Personal Website}
#'
#'
#' @examples
#'
#' prices <- get_historic_prices('MSFT')
#' @rdname get_historic_prices
#' @export
#'
#'
#'
get_historic_prices <- function(symbol) {
  suppressWarnings(dplyr::bind_rows(pbapply::pblapply(symbol, FUN = fetch_historic_prices)))
}
