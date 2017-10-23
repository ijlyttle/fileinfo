#' Format a file-info data frame
#'
#' @param .data object containing file information, usually a `data.frame`
#' @param tz    `character`, name of timezone to use
#' @param ...   additional arguments passed to [dplyr::transmute()] to add
#'   variables to the data frame.
#'
#' @return [tibble::tibble()] with varaibles:
#' \describe{
#'   \item{basename}{`character`, basename of the file path}
#'   \item{type}{`character`, can be `"file"` or `"directory"`}
#'   \item{length}{`double`, file-length (bytes)}
#'   \item{modified}{`POSIXct`, modification time}
#' }
#'
#' @export
#'
format_fileinfo_local <- function(.data, tz = "UTC", ...) {

  .data <-
    .data %>%
    tibble::as.tibble() %>%
    tibble::rownames_to_column(var = "path") %>%
    dplyr::transmute(
      basename = basename(.data$path),
      type = dplyr::case_when(
        .data$isdir ~ "directory",
        !.data$isdir ~ "file"
      ),
      length = .data$size,
      modified = .data$mtime,
      ...
    ) %>%
    lubridate::with_tz(tzone = tz) %>%
    arrange_fileinfo()

  .data
}

#' Get file information from local filesystem
#'
#' @param path `character` path to directory
#'
#' @return `data.frame` returned by [file.info()]
#' @examples
#' \dontrun{
#'   fileinfo_local(".")
#' }
#' @export
#'
fileinfo_local <- function(path) {
  path %>%
    list.files(full.names = TRUE) %>%
    file.info()
}
