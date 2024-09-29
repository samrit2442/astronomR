# query2 <- paste0(
#   "SELECT source_id, ra, dec, phot_bp_mean_mag, phot_rp_mean_mag, phot_g_mean_mag, parallax ",
#   "FROM external.gaiaedr3_gcns_main_1 ",
#   "WHERE parallax >50")

# vars <- "source_id, ra, dec, phot_bp_mean_mag, phot_rp_mean_mag, parallax"
# condition <- "parallax > 40"

get_gaia_data <- function(vars, condition) {
  # URL for the Gaia Archive TAP service
  base_url <- "https://gea.esac.esa.int/tap-server/tap/sync"

  query <- paste0("SELECT ",
                  vars,
                  " FROM external.gaiaedr3_gcns_main_1 ",
                  "WHERE ",
                  condition)

  body <- list(
    REQUEST = "doQuery",
    LANG = "ADQL",
    FORMAT = "json",
    PHASE = "RUN",
    QUERY = query)
  print(query)
  # Perform the query
  response <- tryCatch({
    httr::POST(base_url, body = body, encode = "form")
  }, error = function(e) {
    stop("Failed to connect to Gaia Archive. Error: ", e$message)
  })

  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    stop(paste("Failed to query Gaia Archive. HTTP status code:", httr::status_code(response)))
  }

  # Parse the response content
  content <- httr::content(response, "text", encoding = "UTF-8")
  data <- tryCatch({
    jsonlite::fromJSON(content, flatten = TRUE)
  }, error = function(e) {
    stop("Failed to parse JSON response.")
  })

  # Check if the response contains data
  if (length(data$data) == 0) {
    stop("No results found for the query.")
  }

  # Convert to data frame and set correct column names
  df <- as.data.frame(data$data)
  colnames(df) <- stringr::str_split_1(vars, ",") |>
    stringr::str_squish()

  return(df)
}

