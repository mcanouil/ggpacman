#' Title
#'
#' @param offset something
#'
#' @return something
#' @export
blinky <- function(offset = 9) {
  dplyr::tribble(
    ~x, ~y,
    rep(10, offset), rep(16, offset),
    9:7, 16,
    7, 15:13,
    6:5, 13,
    5, 12:8,
    4, 8,
    rep(5, 3), rep(8, 3), # dead
    6, 8,
    6, 9,
    7, 9,
    7, 10,
    8, 10,
    8, 11,
    9, 11,
    9, 12,
    10, 12,
    10, 13,
    rep(10, 4), rep(14, 4),
    10, 15, # alive
    10:13, 16,
    13, 15:8,
    12:11, 8,
    11, 7:6,
    12:15, 6,
    15, 6:3,
    16:17, 3,
    17, 4:6,
    17:19, 6,
    19, 7:8
  ) %>%
    dplyr::mutate(colour = "Blinky")
}
