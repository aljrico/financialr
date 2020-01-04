#' Functions to call and retrieve financial data from remote databases, by using company tickers (symbols). It uses publicly available API to download all data.
#'
#' @return Returns an array.
#'
#' @author Alejandro Jiménez Rico \email{aljrico@@gmail.com}, \href{https://aljrico.github.io}{Personal Website}
#'
#'
#' @examples
#'
#' symbols <- get_symbols_list()
#' @rdname get_symbols_list
#' @export
#'
#'
#'
get_symbols_list <- function() {
  url <- glue::glue("https://financialmodelingprep.com/api/v3/company/stock/list")
  content <- url %>%
    httr::GET() %>%
    httr::content()

  symbolsList <- content$symbolsList

  sapply(symbolsList, FUN = function(x) x$symbol)
}
