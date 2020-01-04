financial_statement <-
  R6::R6Class(
    classname = "financial_statement",
    public = list(
      data = NULL,
      parse_data = function(content, symbol) {
        parsed_data <- t(content)
        variable_names <- parsed_data[1, ] %>% unlist() %>% as.character()
        colnames(parsed_data) <- variable_names
        d <- rownames(parsed_data)[-1]

        parsed_data <- parsed_data[-1, ] %>% as.data.frame()

        # When we only have one date worth of data, the format is slightly different and it gets organised transposed. We need to correct that
        if(length(d) == 1){
          parsed_data <- t(parsed_data) %>% as.data.frame()
        }

        parsed_data <- suppressWarnings(dplyr::mutate_all(parsed_data, clean_numbers))

        clean_data <-
          parsed_data %>%
          dplyr::mutate(date = lubridate::ymd(d)) %>%
          dplyr::arrange(date) %>%
          dplyr::select(date, dplyr::everything()) %>%
          dplyr::mutate(ticker = symbol) %>%
          dplyr::select(date, ticker, dplyr::everything())

        self$data <- dplyr::bind_rows(self$data, clean_data)
      }
    )
  )

error_api <-
  R6::R6Class(
    classname = 'error_api',
    public = list(
      error_message = NULL
    )
  )


call_api <- function(url){
  response <- httr::GET(url)
  content <- suppressWarnings(httr::content(response, as = "parsed", col_types = readr::cols()))
  content <- as.data.frame(content)
}

check_response <- function(response) {
  if (names(response)[[1]] == "Error") {
    return(error_api$new())
  } else {
    return(response)
  }
}

clean_numbers <- function(x) {
  x %>%
    stringr::str_remove_all("//~") %>%
    as.numeric()
}

build_url <- function(symbol, statement_type) {
  glue::glue("https://financialmodelingprep.com/api/v3/financials/{statement_type}-statement/{symbol}?datatype=csv")
}

fetch_financial_statement <- function(symbol, type){

  download_data <- function(s, type){

    url <- build_url(symbol = s, statement_type = type)
    response <- call_api(url)
    response_check <- check_response(response)

    if (class(response_check) == "error_api") {
      warning(glue::glue('Invalid API Call. Symbol "{s}" was probably incorrect and was skipped.'))
      return()
    }

    fs$parse_data(content = response, symbol = s)
  }

  fs <- financial_statement$new()
  pbapply::pboptions(char = '.', style = 3, min_time = 5)
  pbapply::pbsapply(symbol, download_data, type)
  fs$data
}


#' Functions to call and retrieve financial data from remote databases, by using company tickers (symbols). It uses publicly available API to download all data.
#'
#' @param symbol Specifies the ticker or symbol the user wants to download the data of, in the form of a string. Example: 'MSFT'
#'
#'
#' @return Returns a data frame.
#'
#' @author Alejandro Jiménez Rico \email{aljrico@@gmail.com}, \href{https://aljrico.github.io}{Personal Website}
#'
#'
#' @examples
#'
#' df <- get_income_statement(symbol = "MSFT")
#' df <- get_balance_sheet_statement(symbol = "MSFT")
#' df <- get_cash_flow_statement(symbol = "MSFT")
#'
#' @rdname get_statement
#' @export
#'
get_income_statement <- function(symbol) {
  force(symbol)
  fetch_financial_statement(symbol = symbol, type = "income")
}

#' @rdname get_statement
#' @export
#'
get_balance_sheet_statement <- function(symbol) {
  force(symbol)
  fetch_financial_statement(symbol = symbol, type = "balance-sheet")
}

#' @rdname get_statement
#' @export
#'
get_cash_flow_statement <- function(symbol) {
  force(symbol)
  fetch_financial_statement(symbol = symbol, type = "cash-flow")
}
