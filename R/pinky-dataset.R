#' Title
#'
#' @param offset something
#'
#' @return something
#' @export
pinky <- function(offset = 9) {
  dplyr::tribble(
    ~x, ~y,
    rep(9, offset * 2), rep(13, offset * 2),
    9, 14,
    10, 14:16,
    11:13, 16,
    13, 15:11,
    12:7, 11,
    7, 10:8,
    8:9, 8,
    9, 7:6,
    8:5, 6,
    5, 5:3,
    4:3, 3,
    3, 4:6,
    2:1, 6,
    1, 7:8,
    2:5, 8,
    5, 9:18,
    4:3, 18
  ) %>%
    dplyr::mutate(colour = "Pinky")
}