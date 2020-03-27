#' Title
#'
#' @param offset something
#'
#' @return something
#' @export
clyde <- function(offset = 9) {
  dplyr::tribble(
    ~x, ~y,
    rep(10, offset * 4), rep(13, offset * 4),
    10, 14:16,
    11, 16,
    11, 17:18,
    12:13, 18,
    13, 19:21,
    14:15, 21,
    15, 22:25,
    16:17, 25, # dead
    16, 25,
    15, 25:24,
    14, 23:22,
    13, 21:20,
    12, 19:18,
    11, 17:16,
    10, 15,
    rep(10, 4), rep(14, 4),
    10, 15, # alive
    10, 16,
    10, 16,
    10, 16
  ) %>%
    dplyr::mutate(colour = "Clyde")
}