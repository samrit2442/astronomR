#' Query Gaia Archive Data
#'
#' Queries the Gaia Archive TAP service to retrieve stellar data based on
#' specified variables and filter conditions. Uses the Gaia Early Data Release
#' 3 (EDR3) catalog.
#'
#' @param vars A character string specifying the variables (columns) to
#'   retrieve, separated by commas (e.g., \code{"source_id, ra, dec"}).
#' @param condition A character string specifying the SQL WHERE clause used to
#'   filter rows (e.g., \code{"parallax > 10"}).
#' @return A data frame containing the requested columns for all rows matching
#'   \code{condition}.
#' @details This function sends a synchronous ADQL query to the Gaia Archive
#'   TAP service at \url{https://gea.esac.esa.int/tap-server/tap/sync}. An
#'   internet connection is required.
#' @examples
#' \dontrun{
#' vars <- "source_id, ra, dec, phot_bp_mean_mag, phot_rp_mean_mag, parallax"
#' condition <- "parallax > 40"
#' result <- get_gaia_data(vars, condition)
#' head(result)
#' }
#' @export
get_gaia_data <- function(vars, condition) {
  base_url <- "https://gea.esac.esa.int/tap-server/tap/sync"

  query <- paste0(
    "SELECT ",
    vars,
    " FROM external.gaiaedr3_gcns_main_1 ",
    "WHERE ",
    condition
  )

  body <- list(
    REQUEST = "doQuery",
    LANG = "ADQL",
    FORMAT = "json",
    PHASE = "RUN",
    QUERY = query
  )

  response <- tryCatch(
    httr::POST(base_url, body = body, encode = "form"),
    error = function(e) {
      stop("Failed to connect to Gaia Archive: ", e$message)
    }
  )

  if (httr::status_code(response) != 200) {
    stop(
      "Failed to query Gaia Archive. HTTP status code: ",
      httr::status_code(response)
    )
  }

  content <- httr::content(response, "text", encoding = "UTF-8")
  data <- tryCatch(
    jsonlite::fromJSON(content, flatten = TRUE),
    error = function(e) {
      stop("Failed to parse JSON response from Gaia Archive.")
    }
  )

  if (length(data$data) == 0) {
    stop("No results found for the query.")
  }

  df <- as.data.frame(data$data)
  colnames(df) <- stringr::str_split_1(vars, ",") |>
    stringr::str_squish()

  df
}
