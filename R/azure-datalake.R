#' @rdname format_fileinfo_local
#'
#' @export
#'
format_fileinfo_adls <- function(.data, tz = "UTC") {

  .data <-
    .data %>%
    dplyr::transmute(
      basename = .data$pathSuffix,
      type = tolower(.data$type),
      length = .data$length,
      modified = .data$modificationTime,
      ...
    ) %>%
    lubridate::with_tz(tzone = tz) %>%
    arrange_fileinfo()

  .data
}
