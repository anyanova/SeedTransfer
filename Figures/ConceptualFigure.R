# Load libraries
library(ggplot2)
library(patchwork)
setwd("/Volumes/GoogleDrive/My Drive/Seed Transfer Project/Figures")

# Normal distribution
x_vals <- seq(-5, 5, length.out = 300)
df<- data.frame(
  x = x_vals,
  y = dnorm(x_vals, mean = 0, sd = 1.5))
max_y<- max(df$y)

# Panel 1 with conceptual x-axis
p1 <- ggplot(df[df$x > -4 & df$x < 4,], aes(x = x, y = y)) +
  geom_area(alpha = 0.5, fill = "lightblue") +
  geom_segment(aes(x = -4, y = -0.01, xend = 4, yend = -0.01),
               arrow = arrow(length = unit(0.2, "cm"), ends = "both"), color = "black", size = 0.6) +
  geom_text(aes(x = -4, y = -0.02, label = "-"), color = "black", size = 5) +
  geom_text(aes(x = 4, y = -0.02, label = "+"), color = "black", size = 5) +
  geom_segment(aes(x = 0, y = -0.02, xend = 0, yend = max_y + 0.01), linetype = "solid", color = "darkgrey") +
  geom_segment(aes(x = 2, y = -0.02, xend = 2, yend = max_y + 0.01), linetype = "dashed", color = "darkgrey") +
  geom_segment(aes(x = 0.2, y = 0.2, xend = 1.8, yend = 0.2),
               arrow = arrow(length = unit(0.25, "cm"), ends = "last"),
               color = "darkgrey", size = 0.8) +
  labs(title = NULL, y = "Expected implementation", x = NULL) +
  scale_x_continuous(limits = c(-5, 5), breaks = NULL) +
  geom_text(aes(x = 0, y = -0.03, label = "STD"), color = "black", size = 4) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# Panel 2 with conceptual x-axis and shifted curve
p2 <- ggplot(df[df$x > -6 & df$x < 2,], aes(x = x+2, y = y)) +
  geom_area(fill = "darkgreen", alpha = 0.7) +
  geom_segment(aes(x = -4, y = -0.01, xend = 4, yend = -0.01),
               arrow = arrow(length = unit(0.2, "cm"), ends = "both"), color = "black", size = 0.6) +
  geom_text(aes(x = -4, y = -0.02, label = "-"), color = "black", size = 5) +
  geom_text(aes(x = 4, y = -0.02, label = "+"), color = "black", size = 5) +
  geom_segment(aes(x = 0, y = -0.02, xend = 0, yend = max_y + 0.01), linetype = "solid", color = "darkgrey") +
  geom_segment(aes(x = 2, y = -0.02, xend = 2, yend = max_y + 0.01), linetype = "dashed", color = "darkgrey") +
  geom_segment(aes(x = 0.2, y = 0.2, xend = 1.8, yend = 0.2),
               arrow = arrow(length = unit(0.25, "cm"), ends = "last"),
               color = "darkgrey", size = 0.8) +
  labs(y = "Expected seedling performance", title = NULL, x = NULL) +
  scale_x_continuous(limits = c(-5, 5), breaks = NULL) +
  geom_text(aes(x = 0, y = -0.03, label = "STD"), color = "black", size = 4) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Combine plots side-by-side
combined <- p1 + p2 + plot_layout(nrow = 1)

# Add a caption
c2 <- combined + plot_annotation(
  title = "Seed Transfer Distance",
  subtitle = "(Planting site - Seed collection site)",
  caption = "Seedling survival under future drier conditions while be higher when \ntrees are planted in higher-elevation/wetter sites than the conditions of their seed collection site.",
  theme = theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic")))

c2

ggsave("ConceptualFig_13Aug.svg", c2, width = 7, height = 4, units = "in")
ggsave("ConceptualFig_13Aug.png", c2, width = 7, height = 4, units = "in")



