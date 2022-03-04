library(ggplot2)
library(readr)
library(dplyr)

## convert our format to format with colours etc.
GTS <- read_csv("GTS.csv") %>%
  mutate(col = rgb(R, G, B, maxColorValue = 255)) %>%
  mutate(meanage = (bot - top) / 2 + top) %>%
  mutate(type = factor(type, levels = c("supereon", "Eon", "Era", "Period",
    "superepoch", "Epoch", "Age"))) %>%
  mutate(fontface = as.integer((fontstyle == "italics") * 2 + 1))

# metadata on how wide we want to draw the different types
typeinfo <- tibble(
  type = factor(levels(GTS$type), levels(GTS$type)),
  start = c(0,  .5,   1, 1.5,   2,  2, 3.5),
  end =   c(.5,  1, 1.5,   2, 2.5,  3.5, 5),
  fontsize = as.integer(c(14, 14, 12, 10, 9, 8, 8))) %>%
  mutate(meanwidth = (end - start) / 2 + start) %>%
  mutate(fontangle = as.integer((as.integer(type) <= 5) * 90)) %>%
  mutate(fonthjust = (as.integer(type) <= 3) * .5)

# some manual tweaks
GTS <- GTS %>%
  left_join(typeinfo)
GTS[is.na(GTS$name), "name"] <- ""
GTS[GTS$name == "Hadean" &
      GTS$type == "Eon", c("meanwidth", "fontangle", "end")] <- c((2 + 0.5)/2, 0, 2)
GTS[GTS$top > 290 & GTS$bot < 360 &
      GTS$type == "Epoch", c("meanwidth", "start")] <- c(rep((2.5+3.5)/2, 6), rep(2.5, 6))
GTS[GTS$type == "superepoch", "name"] <- c("Pennsyl-\nvanian", "Missis-\nsippian")
GTS[GTS$name == "Quaternary" & GTS$type == "Period", "fontangle"] <- 0
write_csv(GTS, "GTS_widths.csv")

# load result from previous calculations
GTS <- read_csv("GTS_widths.csv")

# make the plot
gts <- GTS %>%
  ggplot() +
  # these are the ages of the periods
  geom_text(aes(x = bot, y = 2.1, label = bot),
            data = filter(GTS, type == "Period"), size = 8, hjust = 0) +
  # these are the rectangles for each time period
  geom_rect(aes(ymin = start, ymax = end,
    xmin = top, xmax = bot), show.legend = FALSE, fill = GTS$col,
    col = "black") +
  # the labels for each time period, with the direction etc.
  geom_text(aes(x = meanage, y = meanwidth, label = name),
    size = GTS$fontsize * .3,
    angle = GTS$fontangle,
    ## hjust = GTS$fonthjust,
    col = GTS$fontcolor,
    fontface = GTS$fontface) +
  # the ages for the older ones
  geom_text(aes(x = bot, y = 5.1, label = bot),
            data = filter(GTS, type == "Age"), size = 6, hjust = 0) +
  labs(x = "Age (million years before present)", y = "") +
  ## scale_x_reverse(breaks = c(seq(0, 65, 5), seq(70, 4600, 10))) +
  ## scale_x_log10() +
  ## scale_x_continuous(trans = reverselog_trans(10)) +
  ## coord_polar() + # almost a pie-chart!
  ## coord_flip() +
  # theme tweaks: don't plot anything for the y axis
  theme_classic() + theme(axis.title.y = element_blank(),
                          axis.text.y = element_blank(),
                          axis.line.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.x = element_text(size = 18),
                          axis.title.x = element_text(size = 18))

write_rds(gts, "out/gts_plot.rds")

## gts
# width = 1 x A3/A4, height = 4 x A3 paper heights
ggsave("geologic_time_fromR.pdf", gts, width = 100, height = 420 * 24,
  units = "mm", limitsize = FALSE)

# optional reversed log axis?
library("scales")
reverselog_trans <- function(base = exp(1)) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    trans_new(paste0("reverselog-", format(base)), trans, inv,
              log_breaks(base = base),
              domain = c(1e-100, Inf))
}
