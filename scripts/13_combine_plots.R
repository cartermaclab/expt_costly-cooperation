# ----------------------------------------------------------------
# TRADEOFFS BETWEEN INSTRUMENTAL UTILITY AND SOCIAL REWARDS SHAPE PREFERENCES FOR COSTLY COOPERATION
#   -- Lalli, Al Afif, Tang, Croteau, Rathwell, Lokesh, Cashaback, Carter
#
# Experiment 1 data wrangling
#
# Author(s):
#   - Mikayla Lalli
#   - Mike Carter
# ----------------------------------------------------------------


library(cowplot)
library(ggplot2)

#-- [ FIGURE 2 ]

#-- [[ Load saved plots for Fig 2 ]]

fig2a <- ggdraw() + draw_image("figs/fig2a.png")
fig2b <- ggdraw() + draw_image("figs/fig2b.png")
fig2c <- ggdraw() + draw_image("figs/fig2c.png")
fig2d <- ggdraw() + draw_image("figs/fig2d.png")

#-- [[ Patch plots together and save as combined Fig 2 ]]

fig2 <- plot_grid(
  fig2a, fig2b, fig2c, fig2d,
  ncol = 2,
  align = "hv"
)
fig2

ggsave(
  "fig2-portrait.pdf",
  device = "pdf",
  path = "figs/",
  width = 8.5,
  height = 11,
  units = "in",
  dpi = 300,
  #bg = "white"
)

ggsave(
  "fig2-landscape.pdf",
  device = "pdf",
  path = "figs/",
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300,
  #bg = "white"
)


#-- [ FIGURE 3 ]

#-- [[ Load saved plots for Fig 3 ]]

fig3a <- ggdraw() + draw_image("figs/fig3a.png")
fig3b <- ggdraw() + draw_image("figs/fig3b.png")
fig3c <- ggdraw() + draw_image("figs/fig3c.png")
fig3d <- ggdraw() + draw_image("figs/fig3d.png")

#-- [[ Patch plots together and save as combined Fig 3 ]]

fig3 <- plot_grid(
  fig3a, fig3b, fig3c, fig3d,
  ncol = 2,
  align = "hv"
)
fig3

ggsave(
  "fig3-portrait.pdf",
  device = "pdf",
  path = "figs/",
  width = 8.5,
  height = 11,
  units = "in",
  dpi = 300,
  #bg = "white"
)

ggsave(
  "fig3-landscape.pdf",
  device = "pdf",
  path = "figs/",
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300,
  #bg = "white"
)


#-- [ FIGURE 4 ]

#-- [[ Load saved plots for Fig 4 ]]

fig4a <- ggdraw() + draw_image("figs/fig4a.png")
fig4b <- ggdraw() + draw_image("figs/fig4b.png")
fig4c <- ggdraw() + draw_image("figs/fig4c.png")
fig4d <- ggdraw() + draw_image("figs/fig4d.png")

#-- [[ Patch plots together and save as combined Fig 4 ]]

figSA <- plot_grid(
  fig4a, fig4b, fig4c, fig4d,
  ncol = 2,
  align = "hv"
)
figSA

ggsave(
  "figSA-portrait.pdf",
  device = "pdf",
  path = "figs/",
  width = 8.5,
  height = 11,
  units = "in",
  dpi = 300,
  #bg = "white"
)

ggsave(
  "figSA-landscape.pdf",
  device = "pdf",
  path = "figs/",
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300,
  #bg = "white"
)
