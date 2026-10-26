# Regenerates man/figures/logo.png: `f()` above a bow, with the package name below.
library(grid)
library(hexSticker)

red <- "#C81D32"
bright <- "#EF233C"
dark <- "#9E1226"

# Evaluate a path of M/L/C commands (SVG-style, y pointing down on a 600 x 600
# canvas) into polygon points.
bezier <- function(p0, p1, p2, p3, n = 40) {
  t <- seq(0, 1, length.out = n)
  cbind(
    (1 - t)^3 * p0[1] + 3 * (1 - t)^2 * t * p1[1] + 3 * (1 - t) * t^2 * p2[1] + t^3 * p3[1],
    (1 - t)^3 * p0[2] + 3 * (1 - t)^2 * t * p1[2] + 3 * (1 - t) * t^2 * p2[2] + t^3 * p3[2]
  )
}

path <- function(...) {
  pts <- NULL
  cur <- NULL
  for (cmd in list(...)) {
    v <- unlist(cmd[-1])
    if (cmd[[1]] == "M") {
      cur <- v
      pts <- rbind(pts, cur)
    } else if (cmd[[1]] == "L") {
      cur <- v
      pts <- rbind(pts, cur)
    } else {
      pts <- rbind(pts, bezier(cur, v[1:2], v[3:4], v[5:6]))
      cur <- v[5:6]
    }
  }
  pts
}

shape <- function(pts, fill = NA, col = NA, lwd = 1) {
  polygonGrob(
    x = unit(pts[, 1] / 600, "npc"),
    y = unit(1 - pts[, 2] / 600, "npc"),
    gp = gpar(fill = fill, col = col, lwd = lwd)
  )
}

# A bow: two loops with an inner fold, a knot and two swallowtail tails.
petal_left <- path(
  list("M", 300, 340), list("C", 240, 235, 105, 245, 115, 340),
  list("C", 125, 425, 240, 435, 300, 340)
)
petal_right <- cbind(600 - petal_left[, 1], petal_left[, 2])

fold_left <- path(
  list("M", 300, 340), list("C", 255, 275, 165, 280, 170, 340),
  list("C", 175, 395, 250, 400, 300, 340)
)
fold_right <- cbind(600 - fold_left[, 1], fold_left[, 2])

tail_left <- path(
  list("M", 290, 350), list("C", 270, 400, 240, 440, 200, 490),
  list("L", 235, 480), list("L", 250, 512),
  list("C", 275, 460, 305, 420, 315, 365)
)
tail_right <- cbind(600 - tail_left[, 1], tail_left[, 2])

art <- gTree(children = gList(
  textGrob(
    "f()", x = 0.5, y = 1 - 170 / 600, gp = gpar(
      fontfamily = "mono", fontface = "bold", fontsize = 300, col = "white"
    )
  ),
  shape(tail_left, red),
  shape(tail_right, red),
  shape(petal_left, bright),
  shape(petal_right, bright),
  shape(fold_left, red),
  shape(fold_right, red),
  roundrectGrob(
    x = 0.5, y = 1 - 345 / 600, width = 0.085, height = 0.1,
    r = unit(0.25, "snpc"), gp = gpar(fill = dark, col = NA)
  )
))

artwork <- tempfile(fileext = ".png")
png(artwork, width = 1200, height = 1200, bg = "transparent", type = "cairo")
grid.draw(art)
dev.off()

sticker(
  subplot = artwork,
  package = "adorn",
  p_size = 28, p_color = "#FFFFFF", p_y = 0.38,
  s_x = 1, s_y = 1.12, s_width = 0.8,
  h_fill = "#2B2D42", h_color = "#EF233C",
  filename = "man/figures/logo.png",
  dpi = 600
)
