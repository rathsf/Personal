#library("tidyverse")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(viridis))
library("ggsci")

col <- pal_d3("category20")(20)
mode20b <- c(
    "#8a6cae", "#f7cf4a", "#76ccb6", "#d15d58", "#6296bc",
    "#f19900", "#37b067", "#376c72", "#a84680", "#a17b64",
    "#c5b3e8", "#bbe2ad", "#ffac97", "#cef2df", "#d48ad4",
    "#7db8b9", "#d4bbac", "#b7e0e6", "#ebcae8", "#eedebe"
)

mode20 <- c(
    "#9473ae", "#f19900", "#37b067", "#d15d58", "#6296bc",
    "#85ccba", "#f7cf4a", "#376c72", "#d48ad4", "#9f765e",
    "#c7c1d1", "#ffc097", "#bbe2ad", "#808080", "#b7e0e6",
    "#cef2df", "#eedebe", "#7db8b9", "#ebc7e8", "#dac1b1"
)

`%notin%` <- Negate(`%in%`)
options(
    scipen = 999,
    tibble.print_max = 65,
    tibble.print_min = 30,
    width = 135,
    tibble.max_extra_cols = 10,
    pillar.min_title_chars = 8
)

if (str_split(getwd(), "/")[[1]][2] == "mnt") {
    library("systemfonts")
    location <- "remote"
} else if (str_split(getwd(), "/")[[1]][2] == "Users") {
    location <- "local"
}

if (location == "remote") {
    latopth <- "/mnt/user/rathsf/assets/Fonts/Lato"
    register_font("Lato",
        plain = file.path(latopth, "Lato-Regular.ttf"),
        bold = file.path(latopth, "Lato-Bold.ttf"),
        italic = file.path(latopth, "Lato-Italic.ttf"),
        bolditalic = file.path(latopth, "Lato-BoldItalic.ttf")
    )
}

style <- theme(
    text = element_text(family = "Lato", size = 20),
    title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "grey65", linewidth = 2),
    strip.background = element_blank(),
    legend.key = element_blank()
)

figSze <- function(width, heigth){
     options(repr.plot.width = width, repr.plot.height = heigth)
}