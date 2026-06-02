# ----------------------------------------------------------------
# TRADEOFFS BETWEEN INSTRUMENTAL UTILITY AND SOCIAL REWARDS SHAPE PREFERENCES FOR COSTLY COOPERATION
#   -- Lalli, Al Afif, Tang, Croteau, Rathwell, Lokesh, Cashaback, Carter
#
# Experiment 4 data visualization
#
# Author(s):
#   - Mikayla Lalli
#   - Mike Carter
# ----------------------------------------------------------------


#-- [ SCRIPT SETUP ] ----
source(here::here("scripts/11_expt4_analysis.R"))

#-- [[ Color and theme setup ]]
expt4_colour_theme <- c("#ffbf69", "#ff6d00", "#bc3908")

ggplot2::theme_set(
  theme_classic() +
    theme(
      axis.title = element_text(size = 30, face = "bold"),
      axis.text = element_text(size = 28, face = "bold"),
    )
)


#-- [ FIGURE 1: METHODS FIGURE ]
#-- Please note this figure was created using PowerPoint (https://www.microsoft.com/en-ca/microsoft-365/powerpoint).
#-- The .svg file is available in the fig directory of the project repository.


#-- [ FIGURE 2: CHOICE DATA ]
#--
#-- [[ Create the individual plots of bimanual choices as a function of box number for multi-panel Fig 2 ]]

fig2d <- ggplot(
  expt4_bimanual_choices_b,
  aes(x = as.factor(box_number),
      y = proportion)
  ) +
  geom_line(
    aes(group = sub_id),
    colour = "#d8dee9",
    position = position_dodge(0.3),
    linewidth = 0.5,
  ) +
  geom_point(
    aes(group = sub_id, colour = as.factor(box_number)),
    position = position_dodge(0.3),
    size = 3,
    alpha = 0.3
    ) +
  scale_color_manual(values = expt4_colour_theme) +
  geom_boxplot(
    lwd = 1,
    median.linewidth = 1,
    alpha = 0,
    color = expt4_colour_theme
    ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = after_stat(x) + 0.25,
        yend = after_stat(y)),
    position = position_nudge(-0.125),
    linewidth = 1
    ) +
  scale_x_discrete(
    labels = c("4" = "Four",
               "8" = "Eight",
               "12" = "Twelve")
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  labs(
    x = "Number of boxes",
    y = "Bimanual choices"
    ) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(margin = margin(5, 0, 0, 0, unit = "mm")),
    axis.title.y = element_text(margin = margin(0, 5, 0, 0, unit = "mm"))
  )
fig2d

(fig2d + plot_annotation(
  tag_levels = list("d"),
  subtitle = "Experiment 4: Solo Control") &
    theme(
      plot.tag = element_text(
        size = 30,
        face = "bold"),
      plot.subtitle = element_text(
        size = 30,
        face = "bold")
    )
)

ggplot2::ggsave(
  "fig2d.png", # or whatever filename you want to use
  device = "png",
  path = "figs", # where ever you want to save it
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300,
  #    bg = "white" # you can remove this one and the background will be transparent
)


#-- [ FIGURE 3: TIME DATA ]
#--
#-- [[ Create the individual plots of mean trial time (2 trial mode x 3 box number) for multi-panel Fig 3 ]]

fig3_labels <- c("Forced Unimanual", "Forced Bimanual")
names(fig3_labels) <- c(1, 2)

fig3d <- ggplot(
  expt4_mean_time_b_p_tm,
  aes(x = as.factor(box_number),
      y = mean_trial_time,
      color = as.factor(box_number))
) +
  geom_line(
    aes(group = sub_id),
    colour = "#d8dee9",
    position = position_dodge(0.3),
    linewidth = 0.5
  ) +
  geom_point(
    aes(group = interaction(sub_id, box_number, trial_mode), colour = as.factor(box_number)),
    position = position_dodge(0.3),
    size = 3,
    alpha = 0.3
  ) +
  scale_color_manual(values = expt4_colour_theme) +
  geom_boxplot(
    aes(fill = as.factor(trial_mode)),
    lwd = 1,
    median.linewidth = 1,
    alpha = 0,
  ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = after_stat(x) + 0.25,
        yend = after_stat(y)),
    position = position_nudge(-0.125),
    linewidth = 1,
    color = "black"
  ) +
  scale_color_manual(
    values = expt4_colour_theme
  ) +
  scale_x_discrete(
    name = NULL,
    labels = c("4" = "Four",
               "8" = "Eight",
               "12" = "Twelve")
  ) +
  scale_y_continuous(
    name = "Trial time (s)",
    breaks = seq(0, 20, 5),
    limits = c(0, 20)
  ) +
  facet_wrap(~trial_mode,
             strip.position = "bottom",
             labeller = labeller(trial_mode = fig3_labels)
  ) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(margin = margin(5, 0, 0, 0, unit = "mm")),
    axis.title.y = element_text(margin = margin(0, 5, 0, 0, unit = "mm")),
    strip.text.x = element_text(size = 30, face = "bold"),
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.title = element_text(size = 30, face = "bold"),
    axis.text = element_text(size = 28, face = "bold")
  )
fig3d

(fig3d + plot_annotation(
  tag_levels = list("d"),
  subtitle = "Experiment 4: Solo Control") &
    theme(
      plot.tag = element_text(
        size = 30,
        face = "bold"),
      plot.subtitle = element_text(
        size = 30,
        face = "bold")
    )
)

ggplot2::ggsave(
  "fig3d.png", # or whatever filename you want to use
  device = "png",
  path = "figs", # where ever you want to save it
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300,
  #    bg = "white" # you can remove this one and the background will be transparent
)


#-- [ FIGURE 4: DISTANCE DATA ]
#--
#-- [[ Create the individual plots of mean distance traveled (2 trial mode x 3 box number) for multi-panel Fig 4 ]]

fig4d <- ggplot(
  expt4_mean_distance_b_p_tm,
  aes(x = as.factor(box_number),
      y = mean_distance,
      color = as.factor(box_number))
  ) +
  geom_line(
    aes(group = sub_id),
    colour = "#d8dee9",
    position = position_dodge(0.3),
    linewidth = 0.5
    ) +
  geom_point(
    aes(group = interaction(sub_id, box_number, trial_mode), colour = as.factor(box_number)),
    position = position_dodge(0.3),
    size = 3,
    alpha = 0.3
    ) +
  scale_colour_manual(values = expt4_colour_theme) +
  geom_boxplot(
    aes(fill = as.factor(trial_mode)),
    lwd = 1,
    median.linewidth = 1,
    alpha = 0
    ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = after_stat(x) + 0.25,
        yend = after_stat(y)),
    position = position_nudge(-0.125),
    linewidth = 1,
    color = "black"
    ) +
  scale_color_manual(
    values = expt4_colour_theme
    ) +
  scale_x_discrete(
    name = NULL,
    labels = c("4" = "Four",
               "8" = "Eight",
               "12" = "Twelve")
    ) +
  scale_y_continuous(
    name = "Distance traveled (m)",
    breaks = seq(0, 3, 0.5),
    limits = c(0, 3)
    ) +
  facet_wrap(~trial_mode,
             strip.position = "bottom",
             labeller = labeller(trial_mode = fig3_labels)
             ) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(margin = margin(5, 0, 0, 0, unit = "mm")),
    axis.title.y = element_text(margin = margin(0, 5, 0, 0, unit = "mm")),
    strip.text.x = element_text(size = 30, face = "bold"),
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.title = element_text(size = 30, face = "bold"),
    axis.text = element_text(size = 28, face = "bold")
    )
fig4d

(fig4d + plot_annotation(
  tag_levels = list("d"),
  subtitle = "Experiment 4: Solo Control") &
    theme(
      plot.tag = element_text(
        size = 30,
        face = "bold"),
      plot.subtitle = element_text(
        size = 30,
        face = "bold")
    )
)

ggplot2::ggsave(
  "fig4d.png", # or whatever filename you want to use
  device = "png",
  path = "figs", # where ever you want to save it
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300,
  #    bg = "white" # you can remove this one and the background will be transparent
  )
