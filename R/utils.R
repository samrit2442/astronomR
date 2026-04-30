# Suppress R CMD check NOTEs for dplyr NSE column name references
utils::globalVariables(c(
  # deg_to_dms
  "DEG", "MIN", "SEC", "deg_sign", "SIGN",
  # deg_to_hms
  "HRS",
  # dms_to_deg
  "deg",
  # hms_to_deg
  "H", "M", "S"
))
