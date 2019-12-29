call_api <- function(url) {
  content <-
    url %>%
    httr::GET()

  o <- suppressWarnings(content$content %>% readr::read_csv(col_types = readr::cols()))
  return(o)
}

check_api_call <- function(api_call, symbol) {
  if (names(api_call)[[1]] == "Error") {
    message(glue::glue('Invalid API Call. Symbol "{symbol}" was probably incorrect and was skipped.'))
    return(NULL)
  } else {
    return(api_call)
  }
}

parse_data <- function(raw_data) {
  clean_numbers <- function(x) {
    x %>%
      stringr::str_remove_all("//~") %>%
      as.numeric()
  }

  parsed_data <- raw_data %>% t()
  colnames(parsed_data) <- parsed_data[1, ] %>%
    unlist() %>%
    as.character()
  d <- parsed_data[-1, ] %>% rownames()
  parsed_data <- parsed_data[-1, ] %>% as.data.frame()
  parsed_data <- suppressWarnings(dplyr::mutate_all(parsed_data, clean_numbers))

  parsed_data %>%
    dplyr::mutate(date = lubridate::ymd(d)) %>%
    dplyr::arrange(date) %>%
    dplyr::select(date, dplyr::everything())
}

get_statements <- function(symbol, statement_type = "balance-sheet-statement") {
  df_list <- list()

  for (i in seq_along(symbol)) {
    this_symbol <- symbol[[i]]
    url <- glue::glue("https://financialmodelingprep.com/api/v3/financials/{statement_type}/{this_symbol}?datatype=csv")

    api_call <- call_api(url)
    error_check <- check_api_call(api_call, symbol = this_symbol)

    if (is.null(error_check)) next

    df_list[[i]] <-
      api_call %>%
      parse_data() %>%
      dplyr::mutate(ticker = this_symbol) %>%
      dplyr::select(date, ticker, dplyr::everything())
  }

  df_list %>%
    dplyr::bind_rows() %>%
    as.data.frame()
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
#' @rdname get_statement
#' @export
#'
get_income_statement <- function(symbol) {
  force(symbol)
  get_statements(symbol = symbol, statement_type = "income-statement")
}

#' @rdname get_statement
#' @export
#'
get_balance_sheet_statement <- function(symbol) {
  force(symbol)
  get_statements(symbol = symbol, statement_type = "balance-sheet-statement")
}

#' @rdname get_statement
#' @export
#'
get_cash_flow_statement <- function(symbol) {
  force(symbol)
  get_statements(symbol = symbol, statement_type = "cash-flow-statement")
}
