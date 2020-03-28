
<!-- README.md is generated from README.Rmd. Please edit that file -->

# A `ggplot2` and `gganimate` Version of Pac-Man

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![GitHub
tag](https://img.shields.io/github/tag/mcanouil/ggpacman.svg?label=latest%20tag)](https://github.com/mcanouil/ggpacman)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version-ago/ggpacman)](https://cran.r-project.org/package=ggpacman)
[![cran
checks\_worst](https://cranchecks.info/badges/worst/ggpacman)](https://cran.r-project.org/web/checks/check_results_ggpacman.html)
[![CRAN\_Download\_total](https://cranlogs.r-pkg.org/badges/ggpacman)](https://cran.r-project.org/package=ggpacman)
[![R build
status](https://github.com/mcanouil/ggpacman/workflows/R-CMD-check/badge.svg)](https://github.com/mcanouil/ggpacman/actions)
<!-- badges: end -->

The goal of `ggpacman` is to …  
Build a GIF of the game Pac-Man (*not to develop R version of Pac-Man*)
…

## Installation

``` r
# Install ggpacman from CRAN:
install.packages("ggpacman")

# Or the the development version from GitHub:
# install.packages("remotes")
remotes::install_github("mcanouil/ggpacman")
```

## Pac-Man in action

``` r
library(ggpacman)
animate_pacman(
  pacman = pacman,
  ghosts = list(blinky, pinky, inky, clyde),
  font_family = "xkcd"
)
```

![](man/figures/README-pacman-1.gif)<!-- -->

## The Story of `ggpacman`

It started on a Saturday evening …

It was the 21<sup>st</sup> of March (*for the sake of precision*),
around 10 pm CET (*also for the sake of precision and mostly because it
is not relevant*). I was playing around with my data on ‘all’ the movies
I have seen so far
([mcanouil/IMDbRating](https://github.com/mcanouil/IMDbRating)) and
looking on possibly new ideas of visualisation on twitter using
`#ggplot2` and `#gganimate` (by the way the first time I played with
[`gganimate`](https://gganimate.com/) was at [useR-2018 (Brisbane,
Australia)](https://www.r-project.org/conferences/useR-2018/), just
before and when @thomasp85 released the actual framework). The only
thing on the feed was “contaminated/deaths and covid-19” curves made
with [`ggplot2`](https://ggplot2.tidyverse.org/) and a few with
[`gganimate`](https://gganimate.com/) … Let’s say, it was not as funny
and interesting as I was hoping for … Then, I’ve got an idea, what if I
can do something funny and not expected with
[`ggplot2`](https://ggplot2.tidyverse.org/) and
[`gganimate`](https://gganimate.com/)? My first thought, was let’s draw
and animate Pac-Man, that should not be that hard\!

Well, it was not that easy after-all … But, I am going to go through my
code here (you might be interested to actually look at the [commits
history](https://github.com/mcanouil/ggpacman/commits/master).

### The packages

``` r
library("stats")
library("utils")
library("rlang")
library("magrittr")
library("dplyr")
library("tidyr")
library("purrr")
library("ggplot2")
library("ggforce")
library("gganimate")
library("ggtext")
```

### The maze layer

#### The base layer

First thing first, I needed to set-up the base layer, meaning, the maze
from Pac-Man.  
I did start by setting the coordinates of the maze.

``` r
base_layer <- ggplot() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black", colour = "black"),
    panel.background = element_rect(fill = "black", colour = "black"),
  ) +
  coord_fixed(xlim = c(0, 20), ylim = c(0, 26))
```

For later use, let’s add some scales (actually those scales, where
defined way after chronologically speaking). I am using those to define
size and colours for all the `geom_*()` I am going to use to achieve the
Pac-Man GIF.

``` r
map_colours <- c(
  "READY!" = "goldenrod1",
  "wall" = "dodgerblue3", "door" = "dodgerblue3",
  "normal" = "goldenrod1", "big" = "goldenrod1", "eaten" = "black",
  "Pac-Man" = "yellow",
  "eye" = "white", "iris" = "black",
  "Blinky" = "red", "Blinky_weak" = "blue", "Blinky_eaten" = "transparent",
  "Pinky" = "pink", "Pinky_weak" = "blue", "Pinky_eaten" = "transparent",
  "Inky" = "cyan", "Inky_weak" = "blue", "Inky_eaten" = "transparent",
  "Clyde" = "orange", "Clyde_weak" = "blue", "Clyde_eaten" = "transparent"
)
```

``` r
base_layer <- base_layer +
  scale_size_manual(values = c("wall" = 2.5, "door" = 1, "big" = 2.5, "normal" = 0.5, "eaten" = 3)) +
  scale_fill_manual(breaks = names(map_colours), values = map_colours) +
  scale_colour_manual(breaks = names(map_colours), values = map_colours) 
base_layer
```

![](man/figures/README-base-layer-colours-1.png)<!-- -->

My `base_layer` here is not really helpful, so I temporarily added some
elements to help me draw everything.  
*Note*: I won’t use it in the following.

``` r
base_layer +
  scale_x_continuous(breaks = 0:21, sec.axis = dup_axis()) +
  scale_y_continuous(breaks = 0:26, sec.axis = dup_axis()) +
  theme(
    panel.grid.major = element_line(colour = "white"), 
    axis.text = element_text(colour = "white")
  ) +
  annotate("rect", xmin = 0, xmax = 21, ymin = 0, ymax = 26, fill = NA)
```

![](man/figures/README-base-layer-dev-1.png)<!-- -->

Quite better, isn’t it?\!

#### The grid layer

I started drawing the vertical lines on the left (as you may have
noticed, the first level is symmetrical).

``` r
left_vertical_segments <- tribble(
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
```

``` r
base_layer + 
  geom_segment(
    data = left_vertical_segments,
    mapping = aes(x = x, y = y, xend = xend, yend = yend),
    lineend = "round",
    inherit.aes = FALSE,
    colour = "white"
  )
```

![](man/figures/README-left-vertical-plot-1.png)<!-- -->

Let’s add the horizontal lines (still on the left side of the maze)\!

``` r
left_horizontal_segments <- tribble(
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

left_segments <- bind_rows(left_vertical_segments, left_horizontal_segments)
```

``` r
base_layer + 
  geom_segment(
    data = left_segments,
    mapping = aes(x = x, y = y, xend = xend, yend = yend),
    lineend = "round",
    inherit.aes = FALSE,
    colour = "white"
  )
```

![](man/figures/README-left-plot-1.png)<!-- -->

Now, we can see the maze. As I said earlier, the first level is
symmetrical, let’s use `left_segments` to compute a `right_segments`.

``` r
right_segments <-  mutate(
  .data = left_segments,
  x = abs(x - 20),
  xend = abs(xend - 20)
)
```

``` r
base_layer + 
  geom_segment(
    data = bind_rows(left_segments, right_segments),
    mapping = aes(x = x, y = y, xend = xend, yend = yend),
    lineend = "round",
    inherit.aes = FALSE,
    colour = "white"
  )
```

![](man/figures/README-right-plot-1.png)<!-- -->

Now, let’s add the missing vertical lines in the middle and the ghost
“house” door.

``` r
centre_vertical_segments <- tribble(
  ~x, ~y, ~xend, ~yend,
  10, 2, 10, 4,
  10, 7, 10, 9,
  10, 17, 10, 19,
  10, 22, 10, 26
)
door_segment <- tibble(x = 9, y = 15, xend = 11, yend = 15, type = "door")
```

Combine all the segments and draw them all.

``` r
maze_walls <- bind_rows(
  left_segments,
  centre_vertical_segments,
  right_segments
) %>%
  mutate(type = "wall") %>%
  bind_rows(door_segment)
```

``` r
base_layer + 
  geom_segment(
    data = maze_walls,
    mapping = aes(x = x, y = y, xend = xend, yend = yend),
    lineend = "round",
    inherit.aes = FALSE,
    colour = "white"
  )
```

![](man/figures/README-maze-plot-1.png)<!-- -->

The maze is complete, but no-one can actually see the door, since it
appears the same way at the walls. You may have noticed, I added a
column `type`, which can hold two values: `wall` and `door`. I am going
to use that as an aesthetic, you may already have guessed which ones.  
The answer is the `colour` and `size` aesthetics.

``` r
base_layer + 
  geom_segment(
    data = maze_walls,
    mapping = aes(x = x, y = y, xend = xend, yend = yend, colour = type, size = type),
    lineend = "round",
    inherit.aes = FALSE
  )
```

![](man/figures/README-maze-plot-colour-1.png)<!-- -->

*Note: `maze_walls` is a dataset of `ggpacman` (`data("maze_walls",
package = "ggpacman")`).*

#### The bonus points layer

The strategy was quite the same as for the grid layer:

  - Setting up the point coordinates for the left side and the middle.
  - Compute the coordinates for the right side.
  - Use a column `type` for the two types of bonus points, *i.e.*,
    `"normal"` and `"big"` (the one who weaken the ghosts).

<!-- end list -->

``` r
bonus_points_coord <- function() {
  left_bonus_points <- tribble(
    ~x, ~y, ~type,
    1, c(1:3, 7:8, 18:22, 24:25), "normal",
    1, c(6, 23), "big",
    2, c(1, 3, 6, 8, 18, 21, 25), "normal",
    3, c(1, 3:6, 8, 18, 21, 25), "normal",
    4, c(1, 3, 8, 18, 21, 25), "normal",
    5, c(1, 3:25), "normal",
    6, c(1, 6, 8, 21, 25), "normal",
    7, c(1, 3:6, 8, 18:21, 25), "normal",
    8, c(1, 3, 6, 8, 18, 21, 25), "normal",
    9, c(1:3, 6:8, 18, 21:25), "normal"
  )

  bind_rows(
    left_bonus_points,
    tribble(
      ~x, ~y, ~type,
      10, c(1, 21), "normal"
    ),
    mutate(left_bonus_points, x = abs(x - 20))
  ) %>%
    unnest("y")
}
maze_points <- bonus_points_coord()
```

``` r
maze_layer <- base_layer + 
  geom_segment(
    data = maze_walls,
    mapping = aes(x = x, y = y, xend = xend, yend = yend, colour = type, size = type),
    lineend = "round",
    inherit.aes = FALSE
  ) +
  geom_point(
    data = maze_points,
    mapping = aes(x = x, y = y, size = type, colour = type),
    inherit.aes = FALSE
  )
maze_layer
```

![](man/figures/README-maze-point-plot-1.png)<!-- -->

*Note: `maze_points` is a dataset of `ggpacman` (`data("maze_points",
package = "ggpacman")`).*

### Pac-Man character

Time to draw the main character. To draw Pac-Man, I needed few things
first:

  - Pac-Man moves, *i.e.*, all the coordinates where Pac-Man is supposed
    to be at every step.
    
    ``` r
    data("pacman", package = "ggpacman")
    unnest(pacman, c("x", "y"))
    #> # A tibble: 150 x 3
    #>        x     y colour 
    #>    <dbl> <dbl> <chr>  
    #>  1    10     6 Pac-Man
    #>  2    10     6 Pac-Man
    #>  3    10     6 Pac-Man
    #>  4    10     6 Pac-Man
    #>  5    10     6 Pac-Man
    #>  6    10     6 Pac-Man
    #>  7    10     6 Pac-Man
    #>  8    10     6 Pac-Man
    #>  9    10     6 Pac-Man
    #> 10    10     6 Pac-Man
    #> # … with 140 more rows
    ```
    
    ``` r
    maze_layer +
      geom_point(
        data = unnest(pacman, c("x", "y")), 
        mapping = aes(x = x, y = y, colour = colour),
        size = 4
      )
    ```
    
    ![](man/figures/README-pacman-position-plot-1.png)<!-- -->

  - Pac-Man shape (open and closed mouth). Since, Pac-Man is not a
    complete circle shape, I used `geom_arc_bar()`, and defined the
    properties of each state of Pac-Man based on the aesthetics required
    by it. *Note*: At first, I wanted a smooth animation/transition of
    Pac-Man opening and closing its mouth, this is why there are four
    `"close_"` states.
    
    ``` r
    pacman_state <- tribble(
      ~state, ~start, ~end,
      "open_right", 14 / 6 * pi, 4 / 6 * pi,
      "close_right", 15 / 6 * pi, 3 / 6 * pi,
      "open_up", 11 / 6 * pi, 1 / 6 * pi,
      "close_up", 12 / 3 * pi, 0 / 6 * pi,
      "open_left", 8 / 6 * pi, - 2 / 6 * pi,
      "close_left", 9 / 6 * pi, - 3 / 6 * pi,
      "open_down", 5 / 6 * pi, - 5 / 6 * pi,
      "close_down", pi, - pi
    )
    ```
    
    ``` r
    ggplot() +
      geom_arc_bar(
        data = pacman_state,
        mapping = aes(
          x0 = 0, y0 = 0,
          r0 = 0, r = 0.5,
          start = start, end = end
        ),
        fill = "yellow",
        inherit.aes = FALSE
      ) +
      facet_wrap(vars(state), ncol = 4)
    ```
    
    ![](man/figures/README-pacman-state-plot-1.png)<!-- -->

<blockquote class="twitter-tweet">

<p lang="en" dir="ltr">

Next mission, should you choose to accept, is to make Pac-Man face the
direction of movement🎖

</p>

— Thomas Lin Pedersen (@thomasp85)
<a href="https://twitter.com/thomasp85/status/1241767912370774020?ref_src=twsrc%5Etfw">March
22,
2020</a>

</blockquote>

<!-- <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> -->

Once those things available, how to make Pac-Man looks where he is
headed?  
Short answer, I just computed the differences between two successive
positions of Pac-Man and added both open/close state to a new column
`state`.

``` r
pacman %>%
  unnest(c("x", "y")) %>%
  mutate(
    state_x = sign(x - lag(x)),
    state_y = sign(y - lag(y)),
    state = case_when(
      (is.na(state_x) | state_x %in% 0) &
        (is.na(state_y) | state_y %in% 0) ~ list(c("open_right", "close_right")),
      state_x == 1 & state_y == 0 ~ list(c("open_right", "close_right")),
      state_x == -1 & state_y == 0 ~ list(c("open_left", "close_left")),
      state_x == 0 & state_y == -1 ~ list(c("open_down", "close_down")),
      state_x == 0 & state_y == 1 ~ list(c("open_up", "close_up"))
    )
  )  %>%
  unnest("state")
#> # A tibble: 300 x 6
#>        x     y colour  state_x state_y state      
#>    <dbl> <dbl> <chr>     <dbl>   <dbl> <chr>      
#>  1    10     6 Pac-Man      NA      NA open_right 
#>  2    10     6 Pac-Man      NA      NA close_right
#>  3    10     6 Pac-Man       0       0 open_right 
#>  4    10     6 Pac-Man       0       0 close_right
#>  5    10     6 Pac-Man       0       0 open_right 
#>  6    10     6 Pac-Man       0       0 close_right
#>  7    10     6 Pac-Man       0       0 open_right 
#>  8    10     6 Pac-Man       0       0 close_right
#>  9    10     6 Pac-Man       0       0 open_right 
#> 10    10     6 Pac-Man       0       0 close_right
#> # … with 290 more rows
```

Here, in preparation for [`gganimate`](https://gganimate.com/), I also
added a column `step` before merging the new upgraded `pacman` with the
Pac-Man `state` column with the `pacman_state` defined earlier.

``` r
pacman_moves <- ggpacman::compute_pacman_coord(pacman)
pacman_moves
#> # A tibble: 300 x 9
#>        x     y colour  state_x state_y state        step start   end
#>    <dbl> <dbl> <chr>     <dbl>   <dbl> <chr>       <int> <dbl> <dbl>
#>  1    10     6 Pac-Man      NA      NA open_right      1  7.33  2.09
#>  2    10     6 Pac-Man      NA      NA close_right     2  7.85  1.57
#>  3    10     6 Pac-Man       0       0 open_right      3  7.33  2.09
#>  4    10     6 Pac-Man       0       0 close_right     4  7.85  1.57
#>  5    10     6 Pac-Man       0       0 open_right      5  7.33  2.09
#>  6    10     6 Pac-Man       0       0 close_right     6  7.85  1.57
#>  7    10     6 Pac-Man       0       0 open_right      7  7.33  2.09
#>  8    10     6 Pac-Man       0       0 close_right     8  7.85  1.57
#>  9    10     6 Pac-Man       0       0 open_right      9  7.33  2.09
#> 10    10     6 Pac-Man       0       0 close_right    10  7.85  1.57
#> # … with 290 more rows
```

``` r
maze_layer +
  geom_arc_bar(
    data = pacman_moves,
    mapping = aes(
      x0 = x, y0 = y,
      r0 = 0, r = 0.5,
      start = start, end = end,
      colour = colour, fill = colour,
      group = step
    ),
    inherit.aes = FALSE
  )
```

![](man/figures/README-pacman-moves-plots-1.png)<!-- -->

We can’t see much, right?  
Ok, perhaps it’s time to use [`gganimate`](https://gganimate.com/).  
I am going to animate Pac-Man based on the column `step`, which is, if
you looked at the code above, just the line number of `pacman_moves`.

``` r
animated_pacman <- maze_layer +
  geom_arc_bar(
    data = pacman_moves,
    mapping = aes(
      x0 = x, y0 = y,
      r0 = 0, r = 0.5,
      start = start, end = end,
      colour = colour, fill = colour,
      group = step
    ),
    inherit.aes = FALSE
  ) + 
  transition_manual(step)
```

![](man/figures/README-pacman-plot-animated-1.gif)<!-- -->

*Note: `pacman` is a dataset of `ggpacman` (`data("pacman", package =
"ggpacman")`).*

### The Ghosts characters (Blinky, Pinky, Inky and Clyde)

#### Body

``` r
ghost_arc <- unnest(tribble(
  ~x0, ~y0, ~r, ~start, ~end, ~part,
  0, 0, 0.5, - 1 * pi / 2, 1 * pi / 2, "top",
  c(-1 / 6, 1 / 6), -0.5 + 1/8, 0.125, pi / 2, 3 * pi / 2, "bottom",
  -0.5, -0.5 + 1/8, 0.125, 2 * pi / 4, 4 * pi / 4, "bottom",
  0.5, -0.5 + 1/8, 0.125, - 2 * pi / 4, - 4 * pi / 4, "bottom",
  c(-1 / 3, 0, 1 / 3), -0.5 + 1/8, 1 / 24, - 1 * pi / 2, 1 * pi / 2, "bottom",
), "x0")
```

``` r
top <- ggplot() +
  geom_arc_bar(
    data = ghost_arc[1, ],
    mapping = aes(x0 = x0, y0 = y0, r0 = 0, r = r, start = start, end = end)
  ) +
  coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1))
```

![](man/figures/README-ghost-top-plot-1.png)<!-- -->

``` r
top_polygon <- ggplot_build(top)$data[[1]][, c("x", "y")]
```

``` r
bottom <- ggplot() +
  geom_arc_bar(
    data = ghost_arc[c(5, 3, 2, 4), ],
    mapping = aes(
      x0 = x0, y0 = y0,
      r0 = 0, r = 0.17,
      start = start, end = end
    )
  ) +
  coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1))
```

![](man/figures/README-ghost-bottom-plot-1.png)<!-- -->

``` r
bottom_polygon <- ggplot_build(bottom)$data[[1]][, c("x", "y")]
```

``` r
ghost_body <- bind_rows(
  top_polygon[-nrow(top_polygon), ],
  tibble(x = 0.5, y = -0.5 + 1/8),
  bottom_polygon
)
```

``` r
ggplot() +
  coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
  geom_polygon(
    data = ghost_body,
    mapping = aes(x = x, y = y),
    inherit.aes = FALSE
  )
```

![](man/figures/README-ghost-body-plot-1.png)<!-- -->

*Note: `ghost_body` is a dataset of `ggpacman` (`data("ghost_body",
package = "ggpacman")`).*

#### Eyes

``` r
ghost_eyes <- tribble(
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
  unnest("direction")
```

``` r
map_eyes <- c("eye" = "white", "iris" = "black")
ggplot() +
  coord_fixed(xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5)) +
  scale_fill_manual(breaks = names(map_eyes), values = map_eyes) +
  scale_colour_manual(breaks = names(map_eyes), values = map_eyes) +
  geom_circle(
    data = ghost_eyes,
    mapping = aes(x0 = x0, y0 = y0, r = r, colour = part, fill = part),
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  facet_wrap(vars(direction), ncol = 3)
```

![](man/figures/README-ghost-eyes-plot-1.png)<!-- -->

*Note: `ghost_eyes` is a dataset of `ggpacman` (`data("ghost_eyes",
package = "ggpacman")`).*

#### Ghost shape

``` r
ggplot() +
  coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
  scale_fill_manual(breaks = names(map_colours), values = map_colours) +
  scale_colour_manual(breaks = names(map_colours), values = map_colours) +
  geom_polygon(
    data = get(data("ghost_body", package = "ggpacman")),
    mapping = aes(x = x, y = y),
    inherit.aes = FALSE
  ) +
  geom_circle(
    data = get(data("ghost_eyes", package = "ggpacman")),
    mapping = aes(x0 = x0, y0 = y0, r = r, colour = part, fill = part),
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  facet_wrap(vars(direction), ncol = 3)
```

![](man/figures/README-ghost-shape-plot-1.png)<!-- -->

``` r
blinky_ghost <- tibble(x = c(0, 1, 1, 0, 0), y = c(0, 0, 1, 1, 0), colour = "Blinky") %>%
  unnest(c("x", "y")) %>%
  mutate(
    X0 = x,
    Y0 = y,
    state_x = sign(round(x) - lag(round(x))),
    state_y = sign(round(y) - lag(round(y))),
    direction = case_when(
      (is.na(state_x) | state_x %in% 0) &
        (is.na(state_y) | state_y %in% 0) ~ "middle",
      state_x == 1 & state_y == 0 ~ "right",
      state_x == -1 & state_y == 0 ~ "left",
      state_x == 0 & state_y == -1 ~ "down",
      state_x == 0 & state_y == 1 ~ "up"
    )
  ) %>%
  unnest("direction") %>%
  mutate(state = list(1:4)) %>%
  unnest("state")
blinky_ghost
#> # A tibble: 20 x 9
#>        x     y colour    X0    Y0 state_x state_y direction state
#>    <dbl> <dbl> <chr>  <dbl> <dbl>   <dbl>   <dbl> <chr>     <int>
#>  1     0     0 Blinky     0     0      NA      NA middle        1
#>  2     0     0 Blinky     0     0      NA      NA middle        2
#>  3     0     0 Blinky     0     0      NA      NA middle        3
#>  4     0     0 Blinky     0     0      NA      NA middle        4
#>  5     1     0 Blinky     1     0       1       0 right         1
#>  6     1     0 Blinky     1     0       1       0 right         2
#>  7     1     0 Blinky     1     0       1       0 right         3
#>  8     1     0 Blinky     1     0       1       0 right         4
#>  9     1     1 Blinky     1     1       0       1 up            1
#> 10     1     1 Blinky     1     1       0       1 up            2
#> 11     1     1 Blinky     1     1       0       1 up            3
#> 12     1     1 Blinky     1     1       0       1 up            4
#> 13     0     1 Blinky     0     1      -1       0 left          1
#> 14     0     1 Blinky     0     1      -1       0 left          2
#> 15     0     1 Blinky     0     1      -1       0 left          3
#> 16     0     1 Blinky     0     1      -1       0 left          4
#> 17     0     0 Blinky     0     0       0      -1 down          1
#> 18     0     0 Blinky     0     0       0      -1 down          2
#> 19     0     0 Blinky     0     0       0      -1 down          3
#> 20     0     0 Blinky     0     0       0      -1 down          4
```

``` r
blinky_ghost <- blinky_ghost %>% 
  mutate(
    step = 1:n(),
    noise_x = rnorm(n(), mean = 0, sd = 0.05),
    noise_y = rnorm(n(), mean = 0, sd = 0.05)
  )
blinky_ghost
#> # A tibble: 20 x 12
#>        x     y colour    X0    Y0 state_x state_y direction state  step  noise_x
#>    <dbl> <dbl> <chr>  <dbl> <dbl>   <dbl>   <dbl> <chr>     <int> <int>    <dbl>
#>  1     0     0 Blinky     0     0      NA      NA middle        1     1  1.70e-2
#>  2     0     0 Blinky     0     0      NA      NA middle        2     2  2.94e-2
#>  3     0     0 Blinky     0     0      NA      NA middle        3     3 -6.37e-2
#>  4     0     0 Blinky     0     0      NA      NA middle        4     4  2.46e-2
#>  5     1     0 Blinky     1     0       1       0 right         1     5 -5.89e-2
#>  6     1     0 Blinky     1     0       1       0 right         2     6  3.15e-2
#>  7     1     0 Blinky     1     0       1       0 right         3     7  7.03e-3
#>  8     1     0 Blinky     1     0       1       0 right         4     8  4.88e-2
#>  9     1     1 Blinky     1     1       0       1 up            1     9 -4.41e-2
#> 10     1     1 Blinky     1     1       0       1 up            2    10 -1.62e-2
#> 11     1     1 Blinky     1     1       0       1 up            3    11 -5.27e-2
#> 12     1     1 Blinky     1     1       0       1 up            4    12 -3.19e-2
#> 13     0     1 Blinky     0     1      -1       0 left          1    13 -6.48e-2
#> 14     0     1 Blinky     0     1      -1       0 left          2    14 -1.43e-2
#> 15     0     1 Blinky     0     1      -1       0 left          3    15 -1.68e-2
#> 16     0     1 Blinky     0     1      -1       0 left          4    16  9.09e-2
#> 17     0     0 Blinky     0     0       0      -1 down          1    17  6.70e-2
#> 18     0     0 Blinky     0     0       0      -1 down          2    18 -3.49e-4
#> 19     0     0 Blinky     0     0       0      -1 down          3    19 -6.59e-2
#> 20     0     0 Blinky     0     0       0      -1 down          4    20 -1.09e-2
#> # … with 1 more variable: noise_y <dbl>
```

``` r
blinky_ghost <- blinky_ghost %>% 
  mutate(
    body = pmap(
      .l = list(x, y, noise_x, noise_y),
      .f = function(.x, .y, .noise_x, .noise_y) {
        mutate(
          .data = get(data("ghost_body")),
          x = x + .x + .noise_x,
          y = y + .y + .noise_y
        )
      }
    ),
    eyes = pmap(
      .l = list(x, y, noise_x, noise_y, direction),
      .f = function(.x, .y, .noise_x, .noise_y, .direction) {
        mutate(
          .data = filter(get(data("ghost_eyes")), direction == .direction),
          x0 = x0 + .x + .noise_x,
          y0 = y0 + .y + .noise_y,
          direction = NULL
        )
      }
    ),
    x = NULL,
    y = NULL
  )
blinky_ghost
#> # A tibble: 20 x 12
#>    colour    X0    Y0 state_x state_y direction state  step  noise_x  noise_y
#>    <chr>  <dbl> <dbl>   <dbl>   <dbl> <chr>     <int> <int>    <dbl>    <dbl>
#>  1 Blinky     0     0      NA      NA middle        1     1  1.70e-2  0.0677 
#>  2 Blinky     0     0      NA      NA middle        2     2  2.94e-2 -0.0244 
#>  3 Blinky     0     0      NA      NA middle        3     3 -6.37e-2 -0.0136 
#>  4 Blinky     0     0      NA      NA middle        4     4  2.46e-2 -0.0582 
#>  5 Blinky     1     0       1       0 right         1     5 -5.89e-2 -0.0323 
#>  6 Blinky     1     0       1       0 right         2     6  3.15e-2  0.0506 
#>  7 Blinky     1     0       1       0 right         3     7  7.03e-3  0.0118 
#>  8 Blinky     1     0       1       0 right         4     8  4.88e-2  0.0910 
#>  9 Blinky     1     1       0       1 up            1     9 -4.41e-2  0.0790 
#> 10 Blinky     1     1       0       1 up            2    10 -1.62e-2  0.0272 
#> 11 Blinky     1     1       0       1 up            3    11 -5.27e-2  0.00900
#> 12 Blinky     1     1       0       1 up            4    12 -3.19e-2  0.0534 
#> 13 Blinky     0     1      -1       0 left          1    13 -6.48e-2 -0.106  
#> 14 Blinky     0     1      -1       0 left          2    14 -1.43e-2 -0.0288 
#> 15 Blinky     0     1      -1       0 left          3    15 -1.68e-2  0.0153 
#> 16 Blinky     0     1      -1       0 left          4    16  9.09e-2  0.0151 
#> 17 Blinky     0     0       0      -1 down          1    17  6.70e-2 -0.0306 
#> 18 Blinky     0     0       0      -1 down          2    18 -3.49e-4  0.0584 
#> 19 Blinky     0     0       0      -1 down          3    19 -6.59e-2  0.0807 
#> 20 Blinky     0     0       0      -1 down          4    20 -1.09e-2  0.0192 
#> # … with 2 more variables: body <list>, eyes <list>
```

``` r
blinky_ghost <- tibble(x = c(0, 1, 1, 0, 0), y = c(0, 0, 1, 1, 0), colour = "Blinky")
blinky_moves <- ggpacman::compute_ghost_coord(blinky_ghost)
```

``` r
blinky_plot <- base_layer +
  coord_fixed(xlim = c(-1, 2), ylim = c(-1, 2)) +
  geom_polygon(
    data = unnest(blinky_moves, "body"),
    mapping = aes(x = x, y = y, fill = colour, colour = colour, group = step),
    inherit.aes = FALSE
  ) +
  geom_circle(
    data = unnest(blinky_moves, "eyes"),
    mapping = aes(x0 = x0, y0 = y0, r = r, colour = part, fill = part, group = step),
    inherit.aes = FALSE
  )
#> Coordinate system already present. Adding new coordinate system, which will replace the existing one.
```

``` r
animated_blinky <- blinky_plot + transition_manual(step)
```

    #> nframes and fps adjusted to match transition

![](man/figures/README-blinky-plot-animated-1.gif)<!-- -->

### How Pac-Man interacts with the maze?

#### Bonus points

``` r
pacman_moves <- ggpacman::compute_pacman_coord(get(data("pacman", package = "ggpacman")))
right_join(get(data("maze_points")), pacman_moves, by = c("x", "y")) %>%
  distinct(step, x, y, type) %>%
  mutate(
    step = map2(step, max(step), ~ seq(.x, .y, 1)),
    colour = "eaten"
  ) %>%
  unnest("step")
#> # A tibble: 45,150 x 5
#>     step     x     y type  colour
#>    <dbl> <dbl> <dbl> <chr> <chr> 
#>  1     1    10     6 <NA>  eaten 
#>  2     2    10     6 <NA>  eaten 
#>  3     3    10     6 <NA>  eaten 
#>  4     4    10     6 <NA>  eaten 
#>  5     5    10     6 <NA>  eaten 
#>  6     6    10     6 <NA>  eaten 
#>  7     7    10     6 <NA>  eaten 
#>  8     8    10     6 <NA>  eaten 
#>  9     9    10     6 <NA>  eaten 
#> 10    10    10     6 <NA>  eaten 
#> # … with 45,140 more rows
```

``` r
pacman_moves <- ggpacman::compute_pacman_coord(get(data("pacman", package = "ggpacman")))
bonus_points_eaten <- ggpacman::compute_points_eaten(get(data("maze_points")), pacman_moves)
maze_layer_points <- maze_layer +
  geom_point(
    data = bonus_points_eaten,
    mapping = aes(x = x, y = y, colour = colour, size = colour, group = step),
    inherit.aes = FALSE
  )
maze_layer_points
```

![](man/figures/README-points-eaten-plot-1.png)<!-- -->

``` r
animated_points <- maze_layer_points + transition_manual(step)
```

![](man/figures/README-points-eaten-plot-animated-1.gif)<!-- -->

#### Ghost “weak” and “eaten” status

``` r
ghosts_vulnerability <- bonus_points_eaten %>%
  filter(type == "big") %>%
  group_by(x, y) %>%
  summarise(step_init = min(step)) %>%
  ungroup() %>%
  mutate(
    step = map(step_init, ~ seq(.x, .x + 30, 1)),
    vulnerability = TRUE,
    x = NULL,
    y = NULL
  ) %>%
  unnest("step")
ghosts_vulnerability
#> # A tibble: 93 x 3
#>    step_init  step vulnerability
#>        <dbl> <dbl> <lgl>        
#>  1        79    79 TRUE         
#>  2        79    80 TRUE         
#>  3        79    81 TRUE         
#>  4        79    82 TRUE         
#>  5        79    83 TRUE         
#>  6        79    84 TRUE         
#>  7        79    85 TRUE         
#>  8        79    86 TRUE         
#>  9        79    87 TRUE         
#> 10        79    88 TRUE         
#> # … with 83 more rows
```

``` r
ggpacman::compute_ghost_status
#> function(ghost, pacman_moves, bonus_points_eaten) {
#>   ghosts_vulnerability <- bonus_points_eaten %>%
#>     dplyr::filter(.data[["type"]] == "big") %>%
#>     dplyr::group_by(.data[["x"]], .data[["y"]]) %>%
#>     dplyr::summarise(step_init = min(.data[["step"]])) %>%
#>     dplyr::ungroup() %>%
#>     dplyr::mutate(
#>       step = purrr::map(.data[["step_init"]], ~ seq(.x, .x + 30, 1)),
#>       vulnerability = TRUE,
#>       x = NULL,
#>       y = NULL
#>     ) %>%
#>     tidyr::unnest("step")
#> 
#>   ghost_out <- dplyr::left_join(
#>     x = compute_ghost_coord(ghost),
#>     y = pacman_moves %>%
#>       dplyr::mutate(ghost_eaten = TRUE) %>%
#>       dplyr::select(c("X0" = "x", "Y0" = "y", "step", "ghost_eaten")),
#>     by = c("X0", "Y0", "step")
#>   ) %>%
#>     dplyr::left_join(y = ghosts_vulnerability, by = "step") %>%
#>     dplyr::mutate(
#>       vulnerability = tidyr::replace_na(.data[["vulnerability"]], FALSE),
#>       ghost_name = .data[["colour"]],
#>       ghost_eaten = .data[["ghost_eaten"]] & .data[["vulnerability"]],
#>       colour = ifelse(.data[["vulnerability"]], paste0(.data[["ghost_name"]], "_weak"), .data[["colour"]])
#>     )
#> 
#>   pos_eaten_start <- which(ghost_out[["ghost_eaten"]])
#>   ghosts_home <- which(ghost_out[["X0"]] == 10 & ghost_out[["Y0"]] == 14)
#>   for (ipos in pos_eaten_start) {
#>     pos_eaten_end <- min(ghosts_home[ghosts_home>=ipos])
#>     ghost_out[["colour"]][ipos:pos_eaten_end] <- paste0(unique(ghost_out[["ghost_name"]]), "_eaten")
#>   }
#> 
#>   dplyr::left_join(
#>     x = ghost_out,
#>     y = ghost_out %>%
#>       dplyr::filter(.data[["step"]] == .data[["step_init"]] & grepl("eaten", .data[["colour"]])) %>%
#>       dplyr::mutate(already_eaten = TRUE) %>%
#>       dplyr::select(c("step_init", "already_eaten")),
#>       by = "step_init"
#>   ) %>%
#>     dplyr::mutate(
#>       colour = dplyr::case_when(
#>         .data[["already_eaten"]] & .data[["X0"]] == 10 & .data[["Y0"]] == 14 ~ paste0(.data[["ghost_name"]], "_eaten"),
#>         grepl("weak", .data[["colour"]]) & .data[["already_eaten"]] ~ .data[["ghost_name"]],
#>         TRUE ~ .data[["colour"]]
#>       )
#>     )
#> }
#> <bytecode: 0x55a6e92a3930>
#> <environment: namespace:ggpacman>
```

``` r
ghost_moves <- ggpacman::compute_ghost_status(
  ghost = get(data("blinky", package = "ggpacman")),
  pacman_moves = pacman_moves,
  bonus_points_eaten = bonus_points_eaten
)
ghost_moves %>% 
  filter(state == 1) %>% 
  distinct(step, direction, colour, vulnerability) %>% 
  as.data.frame()
#>    step direction       colour vulnerability
#> 1     1    middle       Blinky         FALSE
#> 2     5    middle       Blinky         FALSE
#> 3     9    middle       Blinky         FALSE
#> 4    13    middle       Blinky         FALSE
#> 5    17    middle       Blinky         FALSE
#> 6    21    middle       Blinky         FALSE
#> 7    25    middle       Blinky         FALSE
#> 8    29    middle       Blinky         FALSE
#> 9    33    middle       Blinky         FALSE
#> 10   37      left       Blinky         FALSE
#> 11   41      left       Blinky         FALSE
#> 12   45      left       Blinky         FALSE
#> 13   49      down       Blinky         FALSE
#> 14   53      down       Blinky         FALSE
#> 15   57      down       Blinky         FALSE
#> 16   61      left       Blinky         FALSE
#> 17   65      left       Blinky         FALSE
#> 18   69      down       Blinky         FALSE
#> 19   73      down       Blinky         FALSE
#> 20   77      down       Blinky         FALSE
#> 21   81      down  Blinky_weak          TRUE
#> 22   85      down  Blinky_weak          TRUE
#> 23   89      left Blinky_eaten          TRUE
#> 24   93     right Blinky_eaten          TRUE
#> 25   97    middle Blinky_eaten          TRUE
#> 26  101    middle Blinky_eaten          TRUE
#> 27  105     right Blinky_eaten          TRUE
#> 28  109        up Blinky_eaten          TRUE
#> 29  113     right Blinky_eaten         FALSE
#> 30  117        up Blinky_eaten         FALSE
#> 31  121     right Blinky_eaten         FALSE
#> 32  125        up Blinky_eaten         FALSE
#> 33  129     right Blinky_eaten         FALSE
#> 34  133        up Blinky_eaten         FALSE
#> 35  137     right Blinky_eaten         FALSE
#> 36  141        up Blinky_eaten          TRUE
#> 37  145        up Blinky_eaten          TRUE
#> 38  149    middle Blinky_eaten          TRUE
#> 39  153    middle Blinky_eaten          TRUE
#> 40  157    middle Blinky_eaten          TRUE
#> 41  161        up       Blinky          TRUE
#> 42  165        up       Blinky          TRUE
#> 43  169     right       Blinky          TRUE
#> 44  173     right       Blinky         FALSE
#> 45  177     right       Blinky         FALSE
#> 46  181      down       Blinky         FALSE
#> 47  185      down       Blinky         FALSE
#> 48  189      down       Blinky         FALSE
#> 49  193      down       Blinky         FALSE
#> 50  197      down       Blinky         FALSE
#> 51  201      down       Blinky         FALSE
#> 52  205      down       Blinky         FALSE
#> 53  209      down       Blinky         FALSE
#> 54  213      left       Blinky         FALSE
#> 55  217      left  Blinky_weak          TRUE
#> 56  221      down  Blinky_weak          TRUE
#> 57  225      down  Blinky_weak          TRUE
#> 58  229     right  Blinky_weak          TRUE
#> 59  233     right  Blinky_weak          TRUE
#> 60  237     right  Blinky_weak          TRUE
#> 61  241     right  Blinky_weak          TRUE
#> 62  245    middle  Blinky_weak          TRUE
#> 63  249      down       Blinky         FALSE
#> 64  253      down       Blinky         FALSE
#> 65  257      down       Blinky         FALSE
#> 66  261     right       Blinky         FALSE
#> 67  265     right       Blinky         FALSE
#> 68  269        up       Blinky         FALSE
#> 69  273        up       Blinky         FALSE
#> 70  277        up       Blinky         FALSE
#> 71  281    middle       Blinky         FALSE
#> 72  285     right       Blinky         FALSE
#> 73  289     right       Blinky         FALSE
#> 74  293        up       Blinky         FALSE
#> 75  297        up       Blinky         FALSE
```

``` r
blinky_ghost <- bind_rows(
  tibble(x = 1:4, y = 0, colour = "Blinky"),
  tibble(x = 5:8, y = 0, colour = "Blinky_weak"),
  tibble(x = 9:12, y = 0, colour = "Blinky_eaten")
)
blinky_moves <- ggpacman::compute_ghost_coord(blinky_ghost)
```

``` r
blinky_plot <- base_layer +
  coord_fixed(xlim = c(0, 13), ylim = c(-1, 1)) +
  geom_polygon(
    data = unnest(blinky_moves, "body"),
    mapping = aes(x = x, y = y, fill = colour, colour = colour, group = step),
    inherit.aes = FALSE
  ) +
  geom_circle(
    data = unnest(blinky_moves, "eyes"),
    mapping = aes(x0 = x0, y0 = y0, r = r, colour = part, fill = part, group = step),
    inherit.aes = FALSE
  )
#> Coordinate system already present. Adding new coordinate system, which will replace the existing one.
```

``` r
animated_blinky <- blinky_plot + transition_manual(step)
```

    #> nframes and fps adjusted to match transition

![](man/figures/README-blinky-state-plot-animated-1.gif)<!-- -->

## Plot time

``` r
data("pacman", package = "ggpacman")
data("maze_points", package = "ggpacman")
data("maze_walls", package = "ggpacman")
data("blinky", package = "ggpacman")
data("pinky", package = "ggpacman")
data("inky", package = "ggpacman")
data("clyde", package = "ggpacman")
ghosts <- list(blinky, pinky, inky, clyde)
pacman_moves <- ggpacman::compute_pacman_coord(pacman)
bonus_points_eaten <- ggpacman::compute_points_eaten(maze_points, pacman_moves)
map_colours <- c(
  "READY!" = "goldenrod1",
  "wall" = "dodgerblue3", "door" = "dodgerblue3",
  "normal" = "goldenrod1", "big" = "goldenrod1", "eaten" = "black",
  "Pac-Man" = "yellow",
  "eye" = "white", "iris" = "black",
  "Blinky" = "red", "Blinky_weak" = "blue", "Blinky_eaten" = "transparent",
  "Pinky" = "pink", "Pinky_weak" = "blue", "Pinky_eaten" = "transparent",
  "Inky" = "cyan", "Inky_weak" = "blue", "Inky_eaten" = "transparent",
  "Clyde" = "orange", "Clyde_weak" = "blue", "Clyde_eaten" = "transparent"
)
```

``` r
base_grid <- ggplot() + 
  theme_void() +
  theme(
    legend.position = "none",
    plot.caption = element_textbox_simple(halign = 0.5, colour = "white"),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "black", colour = "black"),
    panel.background = element_rect(fill = "black", colour = "black")
  ) +
  labs(caption = "&copy; Micka&euml;l '<i style='color:#21908CFF;'>Coeos</i>' Canouil") +
  scale_size_manual(values = c("wall" = 2.5, "door" = 1, "big" = 2.5, "normal" = 0.5, "eaten" = 3)) +
  scale_fill_manual(breaks = names(map_colours), values = map_colours) +
  scale_colour_manual(breaks = names(map_colours), values = map_colours) +
  coord_fixed(xlim = c(0, 20), ylim = c(0, 26)) +
  geom_segment(
    data = maze_walls,
    mapping = aes(x = x, y = y, xend = xend, yend = yend, size = type, colour = type),
    lineend = "round",
    inherit.aes = FALSE
  ) +
  geom_point(
    data = maze_points,
    mapping = aes(x = x, y = y, size = type, colour = type),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = tibble(x = 10, y = 11, label = "READY!", step = 1:20),
    mapping = aes(x = x, y = y, label = label, colour = label, group = step),
    size = 6
  )
base_grid
```

![](man/figures/README-plot-time-base-1.png)<!-- -->

``` r
p_points <- list(
  geom_point(
    data = bonus_points_eaten,
    mapping = aes(x = x, y = y, colour = colour, size = colour, group = step),
    inherit.aes = FALSE
  )
)
base_grid + p_points
```

![](man/figures/README-plot-time-points-1.png)<!-- -->

``` r
p_pacman <- list(
  geom_arc_bar(
    data = pacman_moves,
    mapping = aes(
      x0 = x, y0 = y,
      r0 = 0, r = 0.5,
      start = start, end = end,
      colour = colour, fill = colour,
      group = step
    ),
    inherit.aes = FALSE
  )
)
base_grid + p_pacman
```

![](man/figures/README-plot-time-pacman-1.png)<!-- -->

``` r
p_ghosts <- map(.x = ghosts, .f = function(data) {
  ghost_moves <- compute_ghost_status(
    ghost = data,
    pacman_moves = pacman_moves,
    bonus_points_eaten = bonus_points_eaten
  )
  list(
    geom_polygon(
      data = unnest(ghost_moves, "body"),
      mapping = aes(
        x = x, y = y,
        fill = colour, colour = colour,
        group = step
      ),
      inherit.aes = FALSE
    ),
    geom_circle(
      data = unnest(ghost_moves, "eyes"),
      mapping = aes(
        x0 = x0, y0 = y0,
        r = r,
        colour = part, fill = part,
        group = step
      ),
      inherit.aes = FALSE
    )
  )
})
base_grid + p_ghosts
```

![](man/figures/README-plot-time-ghosts-1.png)<!-- -->

``` r
base_grid + p_points + p_pacman + p_ghosts
```

![](man/figures/README-plot-time-all-1.png)<!-- -->

``` r
PacMan <- base_grid + p_points + p_pacman + p_ghosts + transition_manual(step)
```

![](man/figures/README-plot-time-all-animated-1.gif)<!-- -->

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [github](https://github.com/mcanouil/ggpacman/issues).  
For questions and other discussion, please contact the package
maintainer.

-----

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/mcanouil/ggpacman/blob/master/.github/CODE_OF_CONDUCT.md).  
By participating in this project you agree to abide by its terms.
