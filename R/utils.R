arrange_fileinfo <- function(.data) {

  .data <-
    .data %>%
    dplyr::arrange(
      dplyr::desc(.data$type),
      dplyr::desc(.data$modified),
      .data$basename
    )

  .data
}
