make_ghost_coord <- function(data) {
  eyes_circle <- dplyr::tribble(
    ~x0, ~y0, ~r, ~part, ~direction,
    1/5, 1/8, 1/8, "eye", c("up", "down", "right", "left", "middle"),
    -1/5, 1/8, 1/8, "eye", c("up", "down", "right", "left", "middle"),
    5/20, 1/8, 1/20, "iris", "right",
    -3/20, 1/8, 1/20, "iris", "right",
    1/5, 1/16, 1/20, "iris", "down",
    -1/5, 1/16, 1/20, "iris", "down",
    3/20, 1/8, 1/20, "iris", "left",
    -5/20, 1/8, 1/20, "iris", "left",
    1/5, 3/16, 1/20, "iris", "up",
    -1/5, 3/16, 1/20, "iris", "up",
    1/5, 1/8, 1/20, "iris", "middle",
    -1/5, 1/8, 1/20, "iris", "middle"
  ) %>%
    tidyr::unnest("direction")

  bg <- base_ghost()

  ghost_out <- data %>%
    tidyr::unnest(c("x", "y")) %>%
    dplyr::mutate(
      X0 = .data[["x"]],
      Y0 = .data[["y"]],
      state_x = sign(round(.data[["x"]]) - dplyr::lag(round(.data[["x"]]))),
      state_y = sign(round(.data[["y"]]) - dplyr::lag(round(.data[["y"]]))),
      direction = dplyr::case_when(
        (is.na(.data[["state_x"]]) | .data[["state_x"]] %in% 0) &
          (is.na(.data[["state_y"]]) | .data[["state_y"]] %in% 0) ~ "middle",
        .data[["state_x"]] == 1 & .data[["state_y"]] == 0 ~ "right",
        .data[["state_x"]] == -1 & .data[["state_y"]] == 0 ~ "left",
        .data[["state_x"]] == 0 & .data[["state_y"]] == -1 ~ "down",
        .data[["state_x"]] == 0 & .data[["state_y"]] == 1 ~ "up"
      )
    ) %>%
    tidyr::unnest("direction") %>%
    dplyr::mutate(state = list(1:4)) %>%
    tidyr::unnest("state") %>%
    dplyr::group_by(.data[["colour"]]) %>%
    dplyr::mutate(
      step = 1:dplyr::n(),
      noise_x = stats::rnorm(dplyr::n(), mean = 0, sd = 0.05),
      noise_y = stats::rnorm(dplyr::n(), mean = 0, sd = 0.05),
      body = purrr::pmap(
        .l = list(.data[["x"]], .data[["y"]], .data[["noise_x"]], .data[["noise_y"]]),
        .f = function(.x, .y, .noise_x, .noise_y) {
          dplyr::mutate(
            .data = bg,
            x = .data[["x"]] + .x + .noise_x,
            y = .data[["y"]] + .y + .noise_y
          )
        }
      ),
      eyes = purrr::pmap(
        .l = list(.data[["x"]], .data[["y"]], .data[["noise_x"]], .data[["noise_y"]], .data[["direction"]]),
        .f = function(.x, .y, .noise_x, .noise_y, .direction) {
          dplyr::mutate(
            .data = dplyr::filter(eyes_circle, .data[["direction"]] == .direction),
            x0 = .data[["x0"]] + .x + .noise_x,
            y0 = .data[["y0"]] + .y + .noise_y,
            direction = NULL
          )
        }
      ),
      x = NULL,
      y = NULL
    ) %>%
    dplyr::ungroup()

  ghost_out
}