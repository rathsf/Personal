library(tidyverse)
library(ggthemes)
library(ambient)



ggplot(aes(x = seq(0, 10, 1), y = seq(0, 10, 1)))


thm_void <- theme(
        text = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray15", color = "gray15"),
        panel.background = element_blank()
    )


mpg %>% ggplot(aes(x = cty, y = cty, angle = year)) + 
    geom_segment(x = 5, xend = 20, y = 5, yend = 20)


mpg %>% ggplot(aes(x = displ, y = cty, alpha = year, size = hwy)) +
    geom_point(color = "turquoise", aes(alpha = hwy), position = position_jitter(width = 8)) +
    geom_point(aes(x = cty, y = hwy, alpha = cty, size = year),
        color = "darksalmon",
        position = position_jitter(width = 1.5)
    ) +
    scale_size(range = c(5, 10)) +
    scale_y_log10() +
    scale_x_sqrt() +
    coord_polar(theta = "y", direction = -1, clip = "off") +
    scale_x_binned() +



set.seed(1)
n <- 50
dat <- tibble(
  x0 = runif(n),
  y0 = runif(n),
  x1 = x0 + runif(n, min = -.2, max = .2),
  y1 = y0 + runif(n, min = -.2, max = .2),
  shade = runif(n), 
  size = runif(n)
)
dat

dat %>%
  ggplot(aes(
    x = x0,
    y = y0,
    xend = x1,
    yend = y1,
    colour = shade,
    size = size
  )) +
  geom_segment(show.legend = FALSE) +
  coord_polar() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_color_viridis_c() + 
  scale_size(range = c(0, 10)) + 
  thm_void






sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

sample_canva()


sample_cross_matrix <- function(n = 10, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  mat <- matrix(data = 0, nrow = n, ncol = n)
  mat[sample(n, 1), ] <- 1
  mat[, sample(n, 1)] <- 1
  return(mat)
}

sample_cross_matrix()

x_coords <- seq(from = 0, to = 1, length.out = 800)
y_coords <- seq(from = 0, to = 1, length.out = 800)

canvas <- long_grid(x = x_coords, y = y_coords) 

canvas %>% mutate(xend = )
ggplot(aes())

gen_perlin(x = 5, y = 5, z = 5, frequency = .001, seed = 1)
