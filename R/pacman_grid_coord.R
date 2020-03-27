pacman_grid_coord <- function() {
  left_vertical_segments <- dplyr::tribble(
    ~x, ~y, ~xend, ~yend,
    0, 0, 0, 9,
    0, 17, 0, 26,
    2, 4, 2, 5,
    2, 19, 2, 20,
    2, 22, 2, 24,
    4, 4, 4, 7,
    4, 9, 4, 12,
    4, 14, 4, 17,
    4, 19, 4, 20,
    4, 22, 4, 24,
    6, 2, 6, 5,
    6, 9, 6, 12,
    6, 14, 6, 20,
    6, 22, 6, 24,
    8, 4, 8, 5,
    8, 9, 8, 10,
    8, 12, 8, 15,
    8, 19, 8, 20,
    8, 22, 8, 24
  )

  centre_vertical_segments <- dplyr::tribble(
    ~x, ~y, ~xend, ~yend,
    10, 2, 10, 4,
    10, 7, 10, 9,
    10, 17, 10, 19,
    10, 22, 10, 26
  )

  left_horizontal_segments <- dplyr::tribble(
    ~x, ~y, ~xend, ~yend,
    0, 0, 10, 0,
    2, 2, 8, 2,
    0, 4, 2, 4,
    8, 4, 10, 4,
    0, 5, 2, 5,
    8, 5, 10, 5,
    2, 7, 4, 7,
    6, 7, 8, 7,
    0, 9, 4, 9,
    8, 9, 10, 9,
    8, 10, 10, 10,
    0, 12, 4, 12,
    8, 12, 10, 12,
    0, 14, 4, 14,
    8, 15, 9, 15,
    0, 17, 4, 17,
    6, 17, 8, 17,
    2, 19, 4, 19,
    8, 19, 10, 19,
    2, 20, 4, 20,
    8, 20, 10, 20,
    2, 22, 4, 22,
    6, 22, 8, 22,
    2, 24, 4, 24,
    6, 24, 8, 24,
    0, 26, 10, 26
  )

  left_segments <- dplyr::bind_rows(left_vertical_segments, left_horizontal_segments)

  right_segments <-  dplyr::mutate(
    .data = left_segments,
    x = abs(.data[["x"]] - 20),
    xend = abs(.data[["xend"]] - 20)
  )

  segments <- dplyr::bind_rows(
    left_segments,
    centre_vertical_segments,
    right_segments
  ) %>%
    dplyr::mutate(type = "wall") %>%
    dplyr::bind_rows(dplyr::tibble(x = 9, y = 15, xend = 11, yend = 15, type = "door"))
}