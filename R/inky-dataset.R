#' Title
#'
#' @param offset something
#'
#' @return something
#' @export
inky <- function(offset = 9) {
  dplyr::tribble(
    ~x, ~y,
    rep(11, offset * 3), rep(13, offset * 3),
    11, 14,
    10, 14:16,
    9:7, 16,
    7, 15:8,
    8:9, 8,
    9, 7:6,
    10:15, 6,
    15, 6:3,
    16:19, 3,
    19, 2:1,
    18:6, 1
  ) %>%
    dplyr::mutate(colour = "Inky")
}