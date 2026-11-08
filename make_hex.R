
library(ggplot2)
library(hexSticker)
library(grid)
tidy_size <- 30
sem_size <- 35
# Colors
bordeaux <- "#A61919"
black <- "#111111"

# Draw the internal SEM diagram
p <- ggplot() +

  # Dotted connections (draw behind nodes)
  geom_segment(
    aes(x = 0, y = min( 0.72 + 0.23 * sin(seq(0, 2*pi, length.out = 200))), xend = 0, yend = 0.36),
    linewidth = 0.55,
    linetype = "dotted",
    color = black
  ) +
  geom_segment(
    aes(x = 0, y = min( 0.72 + 0.23 * sin(seq(0, 2*pi, length.out = 200))), xend = -0.62, yend = 0.08),
    linewidth = 0.55,
    linetype = "dotted",
    color = black
  ) +
  geom_segment(
    aes(x = 0, y = min( 0.72 + 0.23 * sin(seq(0, 2*pi, length.out = 200))), xend = 0, yend = 0.08),
    linewidth = 0.55,
    linetype = "dotted",
    color = black
  ) +
  geom_segment(
    aes(x = 0, y = min( 0.72 + 0.23 * sin(seq(0, 2*pi, length.out = 200))), xend = 0.62, yend = 0.08),
    linewidth = 0.55,
    linetype = "dotted",
    color = black
  ) +

  # Oval containing "tidy"
  annotate(
    "path",
    x = 0.53 * cos(seq(0, 2*pi, length.out = 200)),
    y = 0.72 + 0.23 * sin(seq(0, 2*pi, length.out = 200)),
    color = black,
    linewidth = 0.9
  ) +
  annotate(
    "text",
    x = 0,
    y = 0.72,
    label = "tidy",
    family = "sans",
    size = tidy_size,
    color = black
  )+

  # Three rounded squares
  annotation_custom(
    roundrectGrob(
      r = unit(0.08, "snpc"),
      gp = gpar(
        fill = "white",
        col = black,
        lwd = 2
      )
    ),
    xmin = -0.88, xmax = -0.36,
    ymin = -0.44, ymax = 0.08
  ) +
  annotation_custom(
    roundrectGrob(
      r = unit(0.08, "snpc"),
      gp = gpar(
        fill = "white",
        col = black,
        lwd = 2
      )
    ),
    xmin = -0.26, xmax = 0.26,
    ymin = -0.44, ymax = 0.08
  ) +
  annotation_custom(
    roundrectGrob(
      r = unit(0.08, "snpc"),
      gp = gpar(
        fill = "white",
        col = black,
        lwd = 2
      )
    ),
    xmin = 0.36, xmax = 0.88,
    ymin = -0.44, ymax = 0.08
  ) +

  # Monospaced letters
  annotate(
    "text",
    x = c(-0.62, 0, 0.62),
    y = -0.18,
    label = c("S", "E", "M"),
    family = "mono",
    fontface = "bold",
    size = sem_size,
    color = black
  ) +

  coord_fixed(
    xlim = c(-1, 1),
    ylim = c(-0.55, 1.05),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "transparent",
      color = NA
    ),
    panel.background = element_rect(
      fill = "transparent",
      color = NA
    ),
    plot.margin = margin(0, 0, 0, 0)
  )

# Create the hex sticker
hexSticker::sticker(
  subplot = p,

  # Diagram placement
  s_x = 1,
  s_y = 1.12,
  s_width = 1.65,
  s_height = 1.65,

  # Hexagon
  h_fill = "white",
  h_color = bordeaux,
  h_size = 3,

  # No package name outside the diagram
  package = "",

  # Output
  filename = "tidySEM_hex.png",
  dpi = 600
)
