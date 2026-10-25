#' Query Gaia Archive Data
#'
#' Queries the Gaia Archive TAP service for stars in the Gaia Catalogue of
#' Nearby Stars (GCNS), the census of objects within 100 pc of the Sun built
#' from Gaia Early Data Release 3 (EDR3) and served as the table
#' \code{external.gaiaedr3_gcns_main_1}. Returns the requested columns for the
#' rows that match a filter condition.
#'
#' @param vars A single character string giving the columns to retrieve,
#'   separated by commas (e.g., \code{"source_id, ra, dec"}).
#' @param condition A single character string giving the ADQL \code{WHERE}
#'   clause used to filter rows (e.g., \code{"parallax > 10"}).
#' @param timeout A single number, at least \code{0.001}: the maximum time, in
#'   seconds, to wait for the whole request, including the time the archive
#'   spends running the query. Default is \code{60}.
#' @return A data frame with one row per matching star and one column per
#'   entry of \code{vars}, named as written there. Values are in the units of
#'   the GCNS table (e.g., \code{ra} and \code{dec} in degrees,
#'   \code{parallax} in mas).
#'
#'   If the query runs but no rows match, a data frame with zero rows and the
#'   same column names (all numeric) is returned, with a message.
#'
#'   If the Gaia Archive cannot be reached, does not answer in time, returns an
#'   HTTP error status, or sends a response that is not valid JSON or does not
#'   hold a table of results with a column for every entry of \code{vars}, a
#'   message gives the reason and \code{NULL} is returned invisibly.
#' @details This function sends the synchronous ADQL query
#'   \code{SELECT vars FROM external.gaiaedr3_gcns_main_1 WHERE condition} to
#'   the Gaia Archive TAP service at
#'   \url{https://gea.esac.esa.int/tap-server/tap/sync}. An internet
#'   connection is required.
#'
#'   The function fails gracefully: problems on the remote side (no
#'   connection, a timeout, an HTTP error status such as a rejected query, or
#'   a malformed response) never raise an error or a warning. Instead a
#'   message explains what went wrong, including the HTTP status and the
#'   archive's error text when there is one, and \code{NULL} is returned
#'   invisibly, so check the result with \code{is.null()} before using it.
#'   Invalid arguments are still errors.
#'
#'   The columns of the result are named by splitting \code{vars} at its
#'   commas. If the archive returns fewer columns than this gives names, for
#'   example because an entry of \code{vars} is a function call with several
#'   arguments, the result is also \code{NULL} with a message.
#'
#'   \code{timeout} caps how long the function can block. Queries that return
#'   many rows can need more than the default 60 seconds; increase
#'   \code{timeout} for them. Opening the connection is also limited,
#'   separately, by the curl package, and a larger \code{timeout} does not
#'   extend that limit.
#' @seealso \code{\link{deg_to_hms}} and \code{\link{deg_to_dms}} to express
#'   the returned \code{ra} and \code{dec} in sexagesimal form.
#' @examples
#' \donttest{
#' vars <- "source_id, ra, dec, phot_bp_mean_mag, phot_rp_mean_mag, parallax"
#' condition <- "parallax > 40"
#' result <- get_gaia_data(vars, condition)
#' head(result)
#' }
#' @export
get_gaia_data <- function(vars, condition, timeout = 60) {
  if (!is.character(vars) || length(vars) != 1L || is.na(vars) ||
      !nzchar(trimws(vars))) {
    stop("'vars' must be a single non-empty character string.")
  }
  if (!is.character(condition) || length(condition) != 1L ||
      is.na(condition) || !nzchar(trimws(condition))) {
    stop("'condition' must be a single non-empty character string.")
  }
  # 0.001 s is the smallest timeout httr::timeout() accepts
  if (!is.numeric(timeout) || length(timeout) != 1L || !is.finite(timeout) ||
      timeout < 0.001) {
    stop("'timeout' must be a single number of seconds, at least 0.001.")
  }

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
    gaia_post(base_url, body = body, timeout = timeout),
    error = function(e) e
  )
  if (inherits(response, "error")) {
    # no limit is named here: curl's own text says which one was reached
    reason <- if (inherits(response, "curl_error_operation_timedout")) {
      "timed out"
    } else {
      "failed"
    }
    message(
      "Gaia Archive request ", reason, ", returning NULL: ",
      stringr::str_squish(conditionMessage(response))
    )
    return(invisible(NULL))
  }

  # NA when the body is not valid UTF-8
  txt <- tryCatch(
    httr::content(response, "text", encoding = "UTF-8"),
    error = function(e) NA_character_
  )
  if (length(txt) != 1L) {
    txt <- NA_character_
  }

  status <- httr::status_code(response)
  if (!isTRUE(status == 200)) {
    detail <- gaia_tap_error(txt)
    message(
      "Gaia Archive returned HTTP status ", status, ", returning NULL",
      if (is.null(detail)) "." else paste0(": ", detail)
    )
    return(invisible(NULL))
  }
  if (is.na(txt)) {
    message(
      "Gaia Archive response could not be read as UTF-8 text, returning NULL."
    )
    return(invisible(NULL))
  }

  # validate() first: fromJSON() would read a non-JSON string that looks like
  # a URL or a file path from that location instead of failing
  valid <- jsonlite::validate(txt)
  parsed <- if (isTRUE(valid)) {
    tryCatch(jsonlite::fromJSON(txt, flatten = TRUE), error = function(e) e)
  } else {
    simpleError(attr(valid, "err"))
  }
  if (inherits(parsed, "error")) {
    detail <- gaia_tap_error(txt)
    if (is.null(detail)) {
      # drop jsonlite's "(right here) ------^" pointer line
      detail <- stringr::str_squish(
        sub("\\(right here\\).*", "", conditionMessage(parsed))
      )
    }
    message("Gaia Archive response is not valid JSON, returning NULL: ", detail)
    return(invisible(NULL))
  }
  if (!is.list(parsed) || !("data" %in% names(parsed))) {
    message("Gaia Archive response has no 'data' element, returning NULL.")
    return(invisible(NULL))
  }
  data <- parsed$data

  cols <- stringr::str_split_1(vars, ",") |>
    stringr::str_squish()

  if (length(data) == 0L) {
    message("Gaia Archive query returned no rows.")
    df <- as.data.frame(matrix(numeric(0), nrow = 0L, ncol = length(cols)))
    colnames(df) <- cols
    return(df)
  }

  # TAP JSON rows are arrays of equal length, which fromJSON() makes a matrix
  if (!is.matrix(data) && !is.data.frame(data)) {
    message(
      "Gaia Archive response has an unexpected 'data' layout, returning NULL."
    )
    return(invisible(NULL))
  }
  if (ncol(data) < length(cols)) {
    message(
      "Gaia Archive returned fewer columns (", ncol(data), ") than 'vars' ",
      "has comma-separated names (", length(cols), "), returning NULL."
    )
    return(invisible(NULL))
  }

  df <- as.data.frame(data)
  colnames(df) <- cols

  df
}


# The only function that touches the network; tests replace it with
# testthat::local_mocked_bindings().
gaia_post <- function(url, body, timeout) {
  httr::POST(url, body = body, encode = "form", httr::timeout(timeout))
}


# Text of the QUERY_STATUS INFO element of a TAP (VOTable) error document,
# or NULL when there is none.
gaia_tap_error <- function(txt) {
  hit <- regmatches(
    txt,
    regexec("(?s)<INFO[^>]*QUERY_STATUS[^>]*[^/]>(.*?)</INFO>", txt,
            perl = TRUE)
  )[[1]]
  if (length(hit) < 2L) {
    return(NULL)
  }
  msg <- hit[2]
  entities <- c("&lt;" = "<", "&gt;" = ">", "&quot;" = "\"", "&apos;" = "'",
                "&amp;" = "&")
  for (ent in names(entities)) {
    msg <- gsub(ent, entities[[ent]], msg, fixed = TRUE)
  }
  msg <- stringr::str_squish(msg)
  if (nzchar(msg)) msg else NULL
}
