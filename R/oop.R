# methods::setClass("financial-statement", "data.frame")
# methods::setClass("income-statement", "financial-statement")
# methods::setClass("cash-flow-statement", "financial-statement")
# methods::setClass("balance-sheet-statement", "financial-statement")
# methods::setClass("error_api", representation(error_message = "character"))
#
# methods::setGeneric("call_api", function(url, type) standardGeneric("call_api"))
# methods::setGeneric("parse_data", function(content) standardGeneric("parse_data"))
# methods::setGeneric("check", function(response) standardGeneric("check"))
# methods::setGeneric("fetch_financial_statement", function(symbol, type) standardGeneric("fetch_financial_statement"))
#
# methods::setMethod("call_api",
#   signature = c("character", "character"),
#   definition = function(url, type) {
#     response <- httr::GET(url)
#     content <- suppressWarnings(httr::content(response, as = "parsed", col_types = readr::cols()))
#     content <- as.data.frame(content)
#     methods::as(content, type)
#   }
# )
#
# clean_numbers <- function(x) {
#   x %>%
#     stringr::str_remove_all("//~") %>%
#     as.numeric()
# }
#
# build_url <- function(symbol, statement_type) {
#   glue::glue("https://financialmodelingprep.com/api/v3/financials/{statement_type}/{symbol}?datatype=csv")
# }
#
# setMethod("parse_data",
#   signature = c("financial-statement"),
#   definition = function(content) {
#     original_class <- class(content)
#     parsed_data <- t(content)
#     colnames(parsed_data) <- parsed_data[1, ] %>%
#       unlist() %>%
#       as.character()
#     d <- parsed_data[-1, ] %>% rownames()
#     parsed_data <- parsed_data[-1, ] %>% as.data.frame()
#     parsed_data <- suppressWarnings(dplyr::mutate_all(parsed_data, clean_numbers))
#
#     parsed_data %>%
#       dplyr::mutate(date = lubridate::ymd(d)) %>%
#       dplyr::arrange(date) %>%
#       dplyr::select(date, dplyr::everything())
#   }
# )
#
# setMethod("check",
#   signature = c("financial-statement"),
#   definition = function(response) {
#     if (names(response)[[1]] == "Error") {
#       return(new("error_api"))
#     } else {
#       return(response)
#     }
#   }
# )
#
# setMethod("fetch_financial_statement",
#   signature = c("character", "character"),
#   definition = function(symbol, type) {
#     df_list <- list()
#
#     pb <- progress::progress_bar$new(
#       format = "  Fetching Financial Data [:bar] :percent in :elapsed",
#       total = length(symbol)
#     )
#
#
#     for (i in seq_along(symbol)) {
#       pb$tick()
#       this_symbol <- symbol[[i]]
#       url <- build_url(this_symbol, statement_type = type)
#
#       response <- call_api(url, type = type)
#       response_check <- check(response)
#
#       if (class(response_check) == "error_api") {
#         warning(glue::glue('Invalid API Call. Symbol "{symbol}" was probably incorrect and was skipped.'))
#         next
#       }
#
#       df_list[[i]] <-
#         response %>%
#         parse_data() %>%
#         dplyr::mutate(ticker = this_symbol) %>%
#         dplyr::select(date, ticker, dplyr::everything())
#     }
#
#     df_list %>%
#       dplyr::bind_rows() %>%
#       tibble::as_tibble() %>%
#       as.data.frame()
#   }
# )
