# tests/testthat/test-gaia.R
# get_gaia_data() is tested offline: gaia_post(), the only function that
# touches the network, is replaced with local_mocked_bindings(). The fake
# responses are minimal httr "response" objects, so httr::status_code() and
# httr::content() run for real on them.

gaia_response <- function(text, status = 200L, type = "application/json") {
  structure(
    list(
      url = "https://gea.esac.esa.int/tap-server/tap/sync",
      status_code = status,
      headers = structure(list("content-type" = type),
                          class = c("insensitive", "list")),
      content = charToRaw(text),
      date = Sys.time()
    ),
    class = "response"
  )
}

# Answer of the live archive (October 2026) to
#   SELECT source_id, ra, parallax FROM external.gaiaedr3_gcns_main_1
#   WHERE parallax > 540
# (Barnard's star and Proxima Centauri), with the metadata trimmed and a
# made-up third row whose parallax is null, to check NA handling.
gaia_json <- paste0(
  '{"metadata": [',
  '{"name": "source_id", "datatype": "long", "unit": null},',
  '{"name": "ra", "datatype": "double", "unit": "deg"},',
  '{"name": "parallax", "datatype": "float", "unit": "mas"}],',
  '"data": [',
  "[4472832130942575872, 269.44850252543836, 546.97595],",
  "[5853498713190525696, 217.39232147200883, 768.0665],",
  "[1, 0.5, null]]}"
)

# Error document the live archive sends (HTTP 400) for an unknown column;
# note the escaped "<" in the echoed query.
gaia_votable_error <- paste0(
  '<VOTABLE version="1.2" xmlns="http://www.ivoa.net/xml/VOTable/v1.2">\n',
  '<RESOURCE type="results">\n',
  '<INFO name="QUERY_STATUS" value="ERROR">\n',
  "Cannot parse query 'SELECT foo FROM external.gaiaedr3_gcns_main_1 ",
  "WHERE parallax &lt; 50' for job 'f381dbc6-O': 1 unresolved ",
  "identifiers: foo [l.1 c.8 - l.1 c.11] !\n",
  ' - Unknown column "foo" !\n',
  "</INFO>\n",
  '<INFO name="HttpErrorCode" value="400"/>\n',
  '<INFO name="Action" value="Handling request"/>\n',
  "</RESOURCE>\n",
  "</VOTABLE>\n"
)

# ---- fake response ---------------------------------------------------------
test_that("the fake response is a valid httr response", {
  resp <- gaia_response('{"a": 1}', status = 503L)
  expect_identical(httr::status_code(resp), 503L)
  expect_identical(httr::content(resp, "text", encoding = "UTF-8"),
                   '{"a": 1}')
})

# ---- success ---------------------------------------------------------------
test_that("get_gaia_data sends the ADQL query and parses the result", {
  calls <- list()
  local_mocked_bindings(gaia_post = function(url, body, timeout) {
    calls[[length(calls) + 1L]] <<- list(url = url, body = body,
                                         timeout = timeout)
    gaia_response(gaia_json)
  })

  res <- expect_silent(
    get_gaia_data("source_id, ra, parallax", "parallax > 540")
  )

  # one request, carrying the vars, the GCNS table and the condition
  expect_length(calls, 1L)
  expect_identical(calls[[1]]$url,
                   "https://gea.esac.esa.int/tap-server/tap/sync")
  expect_identical(
    calls[[1]]$body,
    list(
      REQUEST = "doQuery",
      LANG = "ADQL",
      FORMAT = "json",
      PHASE = "RUN",
      QUERY = paste(
        "SELECT source_id, ra, parallax",
        "FROM external.gaiaedr3_gcns_main_1",
        "WHERE parallax > 540"
      )
    )
  )
  expect_identical(calls[[1]]$timeout, 60)

  # values are the ones in gaia_json; all columns are double
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(3L, 3L))
  expect_named(res, c("source_id", "ra", "parallax"))
  expect_true(all(vapply(res, is.double, logical(1))))
  expect_equal(res$source_id,
               c(4472832130942575872, 5853498713190525696, 1),
               tolerance = 1e-15)
  expect_equal(res$ra, c(269.44850252543836, 217.39232147200883, 0.5),
               tolerance = 1e-15)
  expect_equal(res$parallax[1:2], c(546.97595, 768.0665), tolerance = 1e-15)
  expect_identical(is.na(res$parallax), c(FALSE, FALSE, TRUE))
})

test_that("get_gaia_data squishes column names and passes 'timeout' on", {
  calls <- list()
  local_mocked_bindings(gaia_post = function(url, body, timeout) {
    calls[[length(calls) + 1L]] <<- list(body = body, timeout = timeout)
    gaia_response(gaia_json)
  })
  vars <- "  source_id ,ra,   parallax "

  res <- get_gaia_data(vars, "parallax > 540", timeout = 5)

  expect_named(res, c("source_id", "ra", "parallax"))
  expect_identical(calls[[1]]$timeout, 5)
  expect_true(grepl(vars, calls[[1]]$body$QUERY, fixed = TRUE))
})

# ---- request errors --------------------------------------------------------
test_that("get_gaia_data returns NULL with a message on a connection error", {
  # real curl failures carry curl_error_* classes; this one must not be
  # reported as a timeout
  local_mocked_bindings(gaia_post = function(url, body, timeout) {
    stop(errorCondition(
      paste0("Could not resolve hostname [gea.esac.esa.int]:\n",
             "Could not resolve host: gea.esac.esa.int"),
      class = c("curl_error_couldnt_resolve_host", "curl_error")
    ))
  })

  expect_no_warning(expect_message(
    res <- withVisible(get_gaia_data("ra", "parallax > 40")),
    paste("Gaia Archive request failed, returning NULL:",
          "Could not resolve hostname [gea.esac.esa.int]:",
          "Could not resolve host: gea.esac.esa.int"),
    fixed = TRUE
  ))
  expect_null(res$value)
  expect_false(res$visible)
})

test_that("get_gaia_data reports a timeout without naming which limit fired", {
  local_mocked_bindings(gaia_post = function(url, body, timeout) {
    stop(errorCondition(
      paste0("Timeout was reached [gea.esac.esa.int]:\nOperation timed out ",
             "after 5001 milliseconds with 0 bytes received"),
      class = c("curl_error_operation_timedout", "curl_error")
    ))
  })

  expect_no_warning(expect_message(
    res <- withVisible(get_gaia_data("ra", "parallax > 40", timeout = 5)),
    paste("Gaia Archive request timed out, returning NULL:",
          "Timeout was reached [gea.esac.esa.int]: Operation timed out",
          "after 5001 milliseconds with 0 bytes received"),
    fixed = TRUE
  ))
  expect_null(res$value)
  expect_false(res$visible)
})

test_that("gaia_post hands 'timeout' to httr::timeout() before connecting", {
  # httr::timeout() rejects values below 1 ms while building the request, so
  # this fails before any connection is attempted
  expect_error(gaia_post("http://127.0.0.1:1/", list(a = "1"), timeout = 1e-4),
               "Timeout cannot be less than 1 ms", fixed = TRUE)
})

# ---- HTTP errors -----------------------------------------------------------
test_that("get_gaia_data returns NULL with the status on HTTP 500", {
  local_mocked_bindings(gaia_post = function(url, body, timeout) {
    gaia_response("<html><body>Internal Server Error</body></html>",
                  status = 500L, type = "text/html")
  })

  expect_no_warning(expect_message(
    res <- withVisible(get_gaia_data("ra", "parallax > 40")),
    "Gaia Archive returned HTTP status 500, returning NULL.",
    fixed = TRUE
  ))
  expect_null(res$value)
  expect_false(res$visible)
})

test_that("get_gaia_data passes on the archive's error text (HTTP 400)", {
  local_mocked_bindings(gaia_post = function(url, body, timeout) {
    gaia_response(gaia_votable_error, status = 400L,
                  type = "application/xml;charset=UTF-8")
  })

  expect_message(
    res <- get_gaia_data("foo", "parallax < 50"),
    paste("Gaia Archive returned HTTP status 400, returning NULL:",
          "Cannot parse query 'SELECT foo FROM",
          "external.gaiaedr3_gcns_main_1 WHERE parallax < 50' for job",
          "'f381dbc6-O': 1 unresolved identifiers: foo [l.1 c.8 - l.1 c.11]",
          "! - Unknown column \"foo\" !"),
    fixed = TRUE
  )
  expect_null(res)
})

# ---- malformed responses ---------------------------------------------------
test_that("get_gaia_data returns NULL with a message on malformed JSON", {
  bodies <- c(
    html = "<html><body>Service Unavailable</body></html>",
    truncated = '{"metadata": [{"name": "ra"',
    empty = ""
  )
  reasons <- c(
    html = "lexical error: invalid char in json text.",
    truncated = "parse error: premature EOF",
    empty = "parse error: premature EOF"
  )
  for (case in names(bodies)) {
    local_mocked_bindings(gaia_post = function(url, body, timeout) {
      gaia_response(bodies[[case]])
    })
    expect_no_warning(expect_message(
      res <- withVisible(get_gaia_data("ra", "parallax > 40")),
      paste("Gaia Archive response is not valid JSON, returning NULL:",
            reasons[[case]]),
      fixed = TRUE
    ))
    expect_null(res$value)
    expect_false(res$visible)
  }
})

test_that("get_gaia_data shows a TAP error sent with status 200", {
  local_mocked_bindings(gaia_post = function(url, body, timeout) {
    gaia_response(gaia_votable_error, type = "application/xml")
  })

  expect_message(
    res <- get_gaia_data("foo", "parallax < 50"),
    paste("Gaia Archive response is not valid JSON, returning NULL:",
          "Cannot parse query"),
    fixed = TRUE
  )
  expect_null(res)
})

test_that("get_gaia_data does not read a file named by a non-JSON body", {
  # jsonlite::fromJSON() alone would read this file and return its data
  path <- tempfile(fileext = ".json")
  writeLines('{"data": [[1]]}', path)
  local_mocked_bindings(gaia_post = function(url, body, timeout) {
    gaia_response(path)
  })

  expect_message(
    res <- get_gaia_data("ra", "parallax > 40"),
    "Gaia Archive response is not valid JSON, returning NULL:",
    fixed = TRUE
  )
  expect_null(res)
  unlink(path)
})

test_that("get_gaia_data returns NULL when the JSON has no 'data' element", {
  for (json in c('{"error": "service unavailable"}', "[1, 2]")) {
    local_mocked_bindings(gaia_post = function(url, body, timeout) {
      gaia_response(json)
    })
    expect_message(
      res <- withVisible(get_gaia_data("ra", "parallax > 40")),
      "Gaia Archive response has no 'data' element, returning NULL.",
      fixed = TRUE
    )
    expect_null(res$value)
    expect_false(res$visible)
  }
})

# ---- empty result ----------------------------------------------------------
test_that("get_gaia_data returns a zero-row data frame when nothing matches", {
  # same layout as the live archive's answer to a query with no matches
  local_mocked_bindings(gaia_post = function(url, body, timeout) {
    gaia_response(paste0(
      '{"metadata": [{"name": "source_id", "datatype": "long"},',
      '{"name": "ra", "datatype": "double"},',
      '{"name": "parallax", "datatype": "float"}],\n',
      '"data":\n[\n\n]\n}'
    ))
  })

  expect_no_warning(expect_message(
    res <- withVisible(
      get_gaia_data("source_id, ra, parallax", "parallax > 100000")
    ),
    "Gaia Archive query returned no rows.",
    fixed = TRUE
  ))
  expect_true(res$visible)
  df <- res$value
  expect_s3_class(df, "data.frame")
  expect_identical(dim(df), c(0L, 3L))
  expect_named(df, c("source_id", "ra", "parallax"))
  expect_true(all(vapply(df, is.double, logical(1))))
})

# ---- argument validation ---------------------------------------------------
test_that("get_gaia_data rejects invalid arguments before any request", {
  called <- FALSE
  local_mocked_bindings(gaia_post = function(url, body, timeout) {
    called <<- TRUE
    gaia_response(gaia_json)
  })

  bad_strings <- list(NULL, NA, NA_character_, "", "   ", character(0),
                      c("ra", "dec"), 1, list("ra"), factor("ra"))
  for (bad in bad_strings) {
    expect_error(get_gaia_data(bad, "parallax > 40"),
                 "'vars' must be a single non-empty character string.",
                 fixed = TRUE)
    expect_error(get_gaia_data("ra", bad),
                 "'condition' must be a single non-empty character string.",
                 fixed = TRUE)
  }

  bad_timeouts <- list(NULL, NA, NA_real_, NaN, 0, -1, 1e-4, Inf, "60",
                       c(10, 20), numeric(0), TRUE)
  for (bad in bad_timeouts) {
    expect_error(get_gaia_data("ra", "parallax > 40", timeout = bad),
                 "'timeout' must be a single number of seconds, at least 0.001.",
                 fixed = TRUE)
  }

  expect_error(get_gaia_data(condition = "parallax > 40"), "vars")
  expect_error(get_gaia_data("ra"), "condition")
  expect_false(called)
})

test_that("get_gaia_data accepts the smallest allowed timeout", {
  local_mocked_bindings(gaia_post = function(url, body, timeout) {
    gaia_response(gaia_json)
  })
  res <- get_gaia_data("source_id, ra, parallax", "parallax > 540",
                       timeout = 0.001)
  expect_identical(dim(res), c(3L, 3L))
})

# ---- unexpected response layouts -------------------------------------------
test_that("get_gaia_data returns NULL when 'data' is not a table", {
  local_mocked_bindings(gaia_post = function(url, body, timeout) {
    gaia_response('{"metadata": [], "data": {"ra": 1}}')
  })
  expect_no_warning(expect_message(
    res <- withVisible(get_gaia_data("ra", "parallax > 40")),
    "Gaia Archive response has an unexpected 'data' layout, returning NULL.",
    fixed = TRUE
  ))
  expect_null(res$value)
  expect_false(res$visible)
})

test_that("get_gaia_data returns NULL when fewer columns come back than names", {
  local_mocked_bindings(gaia_post = function(url, body, timeout) {
    gaia_response('{"metadata": [], "data": [[1.5], [2.5]]}')
  })
  expect_no_warning(expect_message(
    res <- withVisible(get_gaia_data("ra, dec", "parallax > 40")),
    "Gaia Archive returned fewer columns (1) than 'vars'",
    fixed = TRUE
  ))
  expect_null(res$value)
})

# ---- gaia_tap_error() ------------------------------------------------------
test_that("gaia_tap_error ignores self-closing and unrelated INFO elements", {
  expect_null(gaia_tap_error(
    '<INFO name="QUERY_STATUS" value="ERROR"/><INFO name="Other">unrelated</INFO>'
  ))
  expect_null(gaia_tap_error("plain text without any INFO element"))
})

test_that("gaia_tap_error unescapes entities, with &amp; last", {
  expect_identical(
    gaia_tap_error('<INFO name="QUERY_STATUS" value="ERROR">a &amp;lt; b &amp; c</INFO>'),
    "a &lt; b & c"
  )
})
