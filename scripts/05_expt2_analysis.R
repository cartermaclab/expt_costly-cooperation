# ----------------------------------------------------------------
# TRADEOFFS BETWEEN INSTRUMENTAL UTILITY AND SOCIAL REWARDS SHAPE PREFERENCES FOR COSTLY COOPERATION
#   -- Lalli, Al Afif, Tang, Croteau, Rathwell, Lokesh, Cashaback, Carter
#
# Experiment 2 statistical analyses
#
# Author(s):
#   - Mikayla Lalli
#   - Mike Carter
# ----------------------------------------------------------------


#-- [ SCRIPT SETUP ] ----
source(here::here("scripts/04_expt2_wrangle.R"))


#-- [ BEHAVIOURAL DATA ]

#-- [[ Proportion of together choices ]]
expt2_total_choices_count <- expt2_choice_tib |>
  dplyr::count(trial_mode)

expt2_choice_descriptives <- expt2_choices_p |>
  dplyr::group_by(trial_mode) |>
  dplyr::summarize(
    n = n(),
    mean_choice = mean(proportion, na.rm = TRUE),
    sd_choice = sd(proportion, na.rm = TRUE),
    median_choice = median(proportion, na.rm = TRUE),
    q1_choice = quantile(proportion, 0.25),
    q3_choice = quantile(proportion, 0.75)
  ) |>
  dplyr::ungroup()

#-- [[ Test proportion of together choices against 50% chance level ]]
set.seed(1234)
WRS2::onesampb(
  x = expt2_choices_p$proportion[expt2_choices_p$trial_mode == "4"],
  est = "median",
  nv = 0.5
)
##-- Robust location estimate: 0.4778
##-- 0.95% confidence interval: 0.3667 0.5389
##-- p-value: 0.518


#-- [ PROPORTION OF TOGETHER CHOICES BY BOX NUMBER]

#-- [[ Omnibus test ]]
expt2_prop_together_aov <- afex::aov_ez(
  data = expt2_together_choices_b,
  id = "sub_id",
  dv = "proportion",
  within = "box_number"
)
expt2_prop_together_aov
##-- F(1.28, 62.79) = 7.13, p = .006, ges = .037

#-- [[ Post hoc tests and compute Cohen's d with Hedge's correction ]]
emmeans::emmeans(
  expt2_prop_together_aov,
  specs = "box_number"
) |>
  pairs(adjust = "holm")
##-- contrast estimate     SE df t.ratio p.value
##--  X4 - X8    0.0747 0.0356 49   2.097  0.0412
##--  X4 - X12   0.1260 0.0421 49   2.992  0.0130
##--  X8 - X12   0.0513 0.0183 49   2.805  0.0144

effectsize::rm_d(
  expt2_together_choices_b_wide$`4`, expt2_together_choices_b_wide$`8`, adjust = FALSE
)
effectsize::rm_d(
  expt2_together_choices_b_wide$`4`, expt2_together_choices_b_wide$`12`, adjust = FALSE
)
effectsize::rm_d(
  expt2_together_choices_b_wide$`8`, expt2_together_choices_b_wide$`12`, adjust = FALSE
)
##-- 4 vs 8: d(rm) = 0.45, 95 CI [0.15, 0.74]
##-- 4 vs 12: d(rm) = 0.75, 95 CI [0.34, 1.16]
##-- 8 vs 12: d(rm) = 0.33, 95 CI [0.12, 0.55]


#-- [ TRIAL TIME IN FORCED ALONE AND TOGETHER TRIALS ]

#-- [[ Create tibbles of descriptives for trial time:
#--    - trial mode x box number
#--    - trial mode only
#--    - box number only ]]
expt2_trial_time_descriptives <- expt2_mean_time_b_p_tm |>
  dplyr::group_by(trial_mode, box_number) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_trial_time, na.rm = TRUE),
    sd_time = sd(mean_trial_time, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

expt2_forced_time_descriptives <- expt2_mean_time_p_tm |>
  dplyr::group_by(trial_mode) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_trial_time, na.rm = TRUE),
    sd_time = sd(mean_trial_time, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

expt2_box_time_descriptives <- expt2_mean_time_p_b |>
  dplyr::group_by(box_number) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_trial_time, na.rm = TRUE),
    sd_time = sd(mean_trial_time, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

#-- [[ Omnibus test ]]
expt2_trial_time_aov <- afex::aov_ez(
  data = expt2_mean_time_b_p_tm,
  id = "sub_id",
  dv = "mean_trial_time",
  within = c("trial_mode", "box_number")
)
expt2_trial_time_aov
##-- Main effect trial mode: F(1, 49) = 44.44, p < .001, ges = .218
##-- Main effect box number: F(1.28, 62.92) = 690.26, p < .001, ges = .561
##-- Interaction: F(1.25, 61.47) = 72.87, p < .001, ges = .094


#-- [[ Post hoc interaction and compute Cohen's d with Hedge's correction ]]
emmeans::emmeans(
  expt2_trial_time_aov,
  specs = c("trial_mode", "box_number")
) |>
  pairs(adjust = "holm")
##-- contrast        estimate     SE df t.ratio p.value
##-- X1 X4 - X2 X4     -0.276 0.0874 49  -3.161  0.0054 [*]
##-- X1 X4 - X1 X8     -1.082 0.0280 49 -38.670  <.0001 [*]
##-- X1 X4 - X2 X8     -2.013 0.1444 49 -13.946  <.0001 [*]
##-- X1 X4 - X1 X12    -1.849 0.0453 49 -40.795  <.0001 [*]
##-- X1 X4 - X2 X12    -3.596 0.2324 49 -15.469  <.0001 [*]
##-- X2 X4 - X1 X8     -0.806 0.0912 49  -8.829  <.0001 [*]
##-- X2 X4 - X2 X8     -1.737 0.0783 49 -22.175  <.0001 [*]
##-- X2 X4 - X1 X12    -1.573 0.0971 49 -16.198  <.0001 [*]
##-- X2 X4 - X2 X12    -3.319 0.1647 49 -20.151  <.0001 [*]
##-- X1 X8 - X2 X8     -0.932 0.1433 49  -6.503  <.0001 [*]
##-- X1 X8 - X1 X12    -0.767 0.0296 49 -25.897  <.0001 [*]
##-- X1 X8 - X2 X12    -2.514 0.2308 49 -10.892  <.0001 [*]
##-- X2 X8 - X1 X12     0.164 0.1470 49   1.118  0.2689
##-- X2 X8 - X2 X12    -1.582 0.1202 49 -13.159  <.0001 [*]
##-- X1 X12 - X2 X12   -1.747 0.2285 49  -7.644  <.0001 [*]

##-- P value adjustment: holm method for 15 tests

effectsize::rm_d(
  expt2_mean_time_b_p_tm_wide$alone_4, expt2_mean_time_b_p_tm_wide$alone_8, adjust = FALSE
)
effectsize::rm_d(
  expt2_mean_time_b_p_tm_wide$alone_4, expt2_mean_time_b_p_tm_wide$alone_12, adjust = FALSE
)
effectsize::rm_d(
  expt2_mean_time_b_p_tm_wide$alone_8, expt2_mean_time_b_p_tm_wide$alone_12, adjust = FALSE
)

effectsize::rm_d(
  expt2_mean_time_b_p_tm_wide$together_4, expt2_mean_time_b_p_tm_wide$together_8, adjust = FALSE
)
effectsize::rm_d(
  expt2_mean_time_b_p_tm_wide$together_4, expt2_mean_time_b_p_tm_wide$together_12, adjust = FALSE
)
effectsize::rm_d(
  expt2_mean_time_b_p_tm_wide$together_8, expt2_mean_time_b_p_tm_wide$together_12, adjust = FALSE
)

effectsize::rm_d(
  expt2_mean_time_b_p_tm_wide$alone_4, expt2_mean_time_b_p_tm_wide$together_4, adjust = FALSE
)
effectsize::rm_d(
  expt2_mean_time_b_p_tm_wide$alone_8, expt2_mean_time_b_p_tm_wide$together_8, adjust = FALSE
)
effectsize::rm_d(
  expt2_mean_time_b_p_tm_wide$alone_12, expt2_mean_time_b_p_tm_wide$together_12, adjust = FALSE
)
##-- A4 vs A8: d(rm) = 2.24, 95CI [2.02, 2.45]
##-- A4 vs A12: d(rm) = 2.82, 95CI [2.52, 3.12]
##-- A8 vs A12: d(rm) = 1.37, 95CI [1.23, 1.52]
##--
##-- T4 vs T8: d(rm) = 1.25, 95CI [1.10, 1.40]
##-- T4 vs T12: d(rm) = 1.22, 95CI [1.06, 1.37]
##-- T8 vs T12: d(rm) = 0.75, 95CI [0.62, 0.87]
##--
##-- A4 vs T4: d(rm) = 0.46, 95CI [0.16, 0.77]
##-- A8 vs T8: d(rm) = 0.98, 95CI [0.62, 1.33]
##-- A12 vs T12: d(rm) = 1.17, 95CI [0.78, 1.56]


#-- [ DISTANCE TRAVELED IN FORCED ALONE AND TOGETHER TRIALS ]
#--
#-- [[ Create tibbles of descriptives for trial time:
#--    - trial mode x box number
#--    - trial mode only
#--    - box number only ]]
expt2_distance_descriptives <- expt2_mean_distance_b_p_tm |>
  dplyr::group_by(trial_mode, box_number) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_distance, na.rm = TRUE),
    sd_time = sd(mean_distance, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

expt2_forced_distance_descriptives <- expt2_mean_distance_p_tm |>
  dplyr::group_by(trial_mode) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_distance, na.rm = TRUE),
    sd_time = sd(mean_distance, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

expt2_box_distance_descriptives <- expt2_mean_distance_p_b |>
  dplyr::group_by(box_number) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_distance, na.rm = TRUE),
    sd_time = sd(mean_distance, na.rm = TRUE)
  ) |>
  dplyr::ungroup()


#-- [[ Omnibus test ]]
expt2_distance_aov <- afex::aov_ez(
  data = expt2_mean_distance_b_p_tm,
  id = "sub_id",
  dv = "mean_distance",
  within = c("trial_mode", "box_number")
)
expt2_distance_aov
##-- Main effect trial mode: F(1, 49) = 8.32, p = .006, ges = .046
##-- Main effect box number: F(1.27, 62.41) = 894.65, p < .001, ges = .719
##-- Interaction: F(1.33, 65.08) = 10.37, p < .001, ges = .021

#-- [[ Post hoc interaction and compute effect size ]]
emmeans::emmeans(
  expt2_distance_aov,
  specs = c("trial_mode", "box_number")
) |>
  pairs(adjust = "holm")
##-- contrast        estimate      SE df t.ratio p.value
##-- X1 X4 - X2 X4     0.0963 0.00803 49  11.996  <.0001 [*]
##-- X1 X4 - X1 X8    -0.2414 0.00515 49 -46.925  <.0001 [*]
##-- X1 X4 - X2 X8    -0.2010 0.01788 49 -11.241  <.0001 [*]
##-- X1 X4 - X1 X12   -0.4084 0.01053 49 -38.772  <.0001 [*]
##-- X1 X4 - X2 X12   -0.3930 0.02931 49 -13.411  <.0001 [*]
##-- X2 X4 - X1 X8    -0.3378 0.00940 49 -35.938  <.0001 [*]
##-- X2 X4 - X2 X8    -0.2973 0.01286 49 -23.126  <.0001 [*]
##-- X2 X4 - X1 X12   -0.5047 0.01242 49 -40.639  <.0001 [*]
##-- X2 X4 - X2 X12   -0.4893 0.02366 49 -20.680  <.0001 [*]
##-- X1 X8 - X2 X8     0.0405 0.01873 49   2.160  0.0713
##-- X1 X8 - X1 X12   -0.1670 0.00835 49 -20.005  <.0001 [*]
##-- X1 X8 - X2 X12   -0.1516 0.02959 49  -5.122  <.0001 [*]
##-- X2 X8 - X1 X12   -0.2074 0.02023 49 -10.254  <.0001 [*]
##-- X2 X8 - X2 X12   -0.1920 0.01577 49 -12.173  <.0001 [*]
##-- X1 X12 - X2 X12   0.0154 0.02904 49   0.530  0.5983
##-- P value adjustment: holm method for 15 tests

effectsize::rm_d(
  expt2_mean_distance_b_p_tm_wide$alone_4, expt2_mean_distance_b_p_tm_wide$alone_8, adjust = FALSE
)
effectsize::rm_d(
  expt2_mean_distance_b_p_tm_wide$alone_4, expt2_mean_distance_b_p_tm_wide$alone_12, adjust = FALSE
)
effectsize::rm_d(
  expt2_mean_distance_b_p_tm_wide$alone_8, expt2_mean_distance_b_p_tm_wide$alone_12, adjust = FALSE
)

effectsize::rm_d(
  expt2_mean_distance_b_p_tm_wide$together_4, expt2_mean_distance_b_p_tm_wide$together_8, adjust = FALSE
)
effectsize::rm_d(
  expt2_mean_distance_b_p_tm_wide$together_4, expt2_mean_distance_b_p_tm_wide$together_12, adjust = FALSE
)
effectsize::rm_d(
  expt2_mean_distance_b_p_tm_wide$together_8, expt2_mean_distance_b_p_tm_wide$together_12, adjust = FALSE
)

effectsize::rm_d(
  expt2_mean_distance_b_p_tm_wide$alone_4, expt2_mean_distance_b_p_tm_wide$together_4, adjust = FALSE
)
effectsize::rm_d(
  expt2_mean_distance_b_p_tm_wide$alone_8, expt2_mean_distance_b_p_tm_wide$together_8, adjust = FALSE
)
effectsize::rm_d(
  expt2_mean_distance_b_p_tm_wide$alone_12, expt2_mean_distance_b_p_tm_wide$together_12, adjust = FALSE
)
##-- A4 vs A8: d(rm) = 5.32, 95CI [4.46, 6.19]
##-- A4 vs A12: d(rm) = 4.86, 95CI [3.98, 5.73]
##-- A8 vs A12: d(rm) = 1.90, 95CI [1.59, 2.21]
##--
##-- T4 vs T8: d(rm) = 1.97, 95CI [1.68, 2.25]
##-- T4 vs T12: d(rm) = 1.74, 95CI [1.48, 2.00]
##-- T8 vs T12: d(rm) = 0.77, 95CI [0.63, 0.91]
##--
##-- A4 vs T4: d(rm) = -1.65, 95CI [-1.23, 2.06]
##-- A8 vs T8: d(rm) = -0.37, 95CI [-0.72, -0.02]
##-- A12 vs T12: d(rm) = -0.09, 95CI [-0.41, -0.23]
