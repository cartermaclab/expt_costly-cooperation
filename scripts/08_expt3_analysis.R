# ----------------------------------------------------------------
# TRADEOFFS BETWEEN INSTRUMENTAL UTILITY AND SOCIAL REWARDS SHAPE PREFERENCES FOR COSTLY COOPERATION
#   -- Lalli, Al Afif, Tang, Croteau, Rathwell, Lokesh, Cashaback, Carter
#
# Experiment 3 statistical analyses
#
# Author(s):
#   - Mikayla Lalli
#   - Mike Carter
# ----------------------------------------------------------------


#-- [ SCRIPT SETUP ] ----
source(here::here("scripts/07_expt3_wrangle.R"))


#-- [ BEHAVIOURAL DATA ]

#-- [[ Proportion of together choices ]]
expt3_total_choices_count <- expt3_choice_tib |>
  dplyr::count(trial_mode)

expt3_choice_descriptives <- expt3_choices_p |>
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
  x = expt3_choices_p$proportion[expt3_choices_p$trial_mode == "4"],
  est = "median",
  nv = 0.5,
)
##-- Robust location estimate: 0.6
##-- 0.95% confidence interval: 0.4 0.6556
##-- p-value: 0.129


#-- [ PROPORTION OF TOGETHER CHOICES BY BOX NUMBER]

#-- [[ Omnibus test ]]
expt3_prop_together_aov <- afex::aov_ez(
  data = expt3_together_choices_b,
  id = "sub_id",
  dv = "proportion",
  within = "box_number"
)
expt3_prop_together_aov
##-- F(1.20, 58.89) = 13.92, p < .001, ges = .045

#-- [[ Post hoc tests and compute Cohen's d with Hedge's correction ]]
emmeans::emmeans(
  expt3_prop_together_aov,
  specs = "box_number"
) |>
  pairs(adjust = "holm")
##-- contrast estimate     SE df t.ratio p.value
##--   X4 - X8    0.1133 0.0333 49   3.404  0.0027
##--   X4 - X12   0.1527 0.0376 49   4.062  0.0005
##--   X8 - X12   0.0393 0.0137 49   2.866  0.0061

effectsize::rm_d(
  expt3_together_choices_b_wide$`4`, expt3_together_choices_b_wide$`8`, adjust = FALSE
)
effectsize::rm_d(
  expt3_together_choices_b_wide$`4`, expt3_together_choices_b_wide$`12`, adjust = FALSE
)
effectsize::rm_d(
  expt3_together_choices_b_wide$`8`, expt3_together_choices_b_wide$`12`, adjust = FALSE
)
##-- 4 vs 8: d(rm) = 0.37, 95 CI [0.15, 0.59]
##-- 4 vs 12: d(rm) = 0.51, 95 CI [0.25, 0.78]
##-- 8 vs 12: d(rm) = 0.13, 95 CI [0.04, 0.22]


#-- [ TRIAL TIME IN FORCED ALONE AND TOGETHER TRIALS ]

#-- [[ Create tibbles of descriptives for trial time:
#--    - trial mode x box number
#--    - trial mode only
#--    - box number only ]]
expt3_trial_time_descriptives <- expt3_mean_time_b_p_tm |>
  dplyr::group_by(trial_mode, box_number) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_trial_time, na.rm = TRUE),
    sd_time = sd(mean_trial_time, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

expt3_forced_time_descriptives <- expt3_mean_time_p_tm |>
  dplyr::group_by(trial_mode) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_trial_time, na.rm = TRUE),
    sd_time = sd(mean_trial_time, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

expt3_box_time_descriptives <- expt3_mean_time_p_b |>
  dplyr::group_by(box_number) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_trial_time, na.rm = TRUE),
    sd_time = sd(mean_trial_time, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

#-- [[ Omnibus test ]]
expt3_trial_time_aov <- afex::aov_ez(
  data = expt3_mean_time_b_p_tm,
  id = "sub_id",
  dv = "mean_trial_time",
  within = c("trial_mode", "box_number")
)
expt3_trial_time_aov
##-- Main effect trial mode: F(1, 49) = 46.83, p < .001, ges = .225
##-- Main effect box number: F(1.26, 61.92) = 404.93, p < .001, ges = .500
##-- Interaction: F(1.31, 63.97) = 81.07, p < .001, ges = .130


#-- [[ Post hoc interaction and compute Cohen's d with Hedge's correction ]]
emmeans::emmeans(
  expt3_trial_time_aov,
  specs = c("trial_mode", "box_number")
) |>
  pairs(adjust = "holm")
##-- contrast        estimate     SE df t.ratio p.value
##--  X1 X4 - X2 X4     -0.305 0.0911 49  -3.344  0.0032 [*]
##--  X1 X4 - X1 X8     -1.059 0.0259 49 -40.852  <.0001 [*]
##--  X1 X4 - X2 X8     -2.088 0.2143 49  -9.746  <.0001 [*]
##--  X1 X4 - X1 X12    -1.806 0.0423 49 -42.731  <.0001 [*]
##--  X1 X4 - X2 X12    -4.314 0.3091 49 -13.957  <.0001 [*]
##--  X2 X4 - X1 X8     -0.754 0.0900 49  -8.378  <.0001 [*]
##--  X2 X4 - X2 X8     -1.784 0.1388 49 -12.853  <.0001 [*]
##--  X2 X4 - X1 X12    -1.501 0.0900 49 -16.675  <.0001 [*]
##--  X2 X4 - X2 X12    -4.009 0.2484 49 -16.140  <.0001 [*]
##--  X1 X8 - X2 X8     -1.030 0.2063 49  -4.992  <.0001 [*]
##--  X1 X8 - X1 X12    -0.747 0.0285 49 -26.189  <.0001 [*]
##--  X1 X8 - X2 X12    -3.255 0.3009 49 -10.820  <.0001 [*]
##--  X2 X8 - X1 X12     0.282 0.2005 49   1.407  0.1657
##--  X2 X8 - X2 X12    -2.226 0.1584 49 -14.056  <.0001 [*]
##--  X1 X12 - X2 X12   -2.508 0.2921 49  -8.585  <.0001 [*]

##-- P value adjustment: holm method for 15 tests

effectsize::rm_d(
  expt3_mean_time_b_p_tm_wide$alone_4, expt3_mean_time_b_p_tm_wide$alone_8, adjust = FALSE
)
effectsize::rm_d(
  expt3_mean_time_b_p_tm_wide$alone_4, expt3_mean_time_b_p_tm_wide$alone_12, adjust = FALSE
)
effectsize::rm_d(
  expt3_mean_time_b_p_tm_wide$alone_8, expt3_mean_time_b_p_tm_wide$alone_12, adjust = FALSE
)

effectsize::rm_d(
  expt3_mean_time_b_p_tm_wide$together_4, expt3_mean_time_b_p_tm_wide$together_8, adjust = FALSE
)
effectsize::rm_d(
  expt3_mean_time_b_p_tm_wide$together_4, expt3_mean_time_b_p_tm_wide$together_12, adjust = FALSE
)
effectsize::rm_d(
  expt3_mean_time_b_p_tm_wide$together_8, expt3_mean_time_b_p_tm_wide$together_12, adjust = FALSE
)

effectsize::rm_d(
  expt3_mean_time_b_p_tm_wide$alone_4, expt3_mean_time_b_p_tm_wide$together_4, adjust = FALSE
)
effectsize::rm_d(
  expt3_mean_time_b_p_tm_wide$alone_8, expt3_mean_time_b_p_tm_wide$together_8, adjust = FALSE
)
effectsize::rm_d(
  expt3_mean_time_b_p_tm_wide$alone_12, expt3_mean_time_b_p_tm_wide$together_12, adjust = FALSE
)
##-- A4 vs A8: d(rm) = 1.90, 95CI [-2.05, -1.75]
##-- A4 vs A12: d(rm) = 2.53, 95CI [-2.77, -2.29]
##-- A8 vs A12: d(rm) = 1.42, 95CI [-1.58, -1.27]
##--
##-- T4 vs T8: d(rm) = 0.67, 95CI [-0.78, -0.55]
##-- T4 vs T12: d(rm) = 1.36, 95CI [-1.59, -1.13]
##-- T8 vs T12: d(rm) = 0.92, 95CI [-1.08, -0.77]
##--
##-- A4 vs T4: d(rm) = 0.54, 95CI [-0.88, -0.20]
##-- A8 vs T8: d(rm) = 0.75, 95CI [-1.08, -0.41]
##-- A12 vs T12: d(rm) = 1.20, 95CI [-1.56, -0.84]


#-- [ DISTANCE TRAVELED IN FORCED ALONE AND TOGETHER TRIALS ]
#--
#-- [[ Create tibbles of descriptives for trial time:
#--    - trial mode x box number
#--    - trial mode only
#--    - box number only ]]
expt3_distance_descriptives <- expt3_mean_distance_b_p_tm |>
  dplyr::group_by(trial_mode, box_number) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_distance, na.rm = TRUE),
    sd_time = sd(mean_distance, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

expt3_forced_distance_descriptives <- expt3_mean_distance_p_tm |>
  dplyr::group_by(trial_mode) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_distance, na.rm = TRUE),
    sd_time = sd(mean_distance, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

expt3_box_distance_descriptives <- expt3_mean_distance_p_b |>
  dplyr::group_by(box_number) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_distance, na.rm = TRUE),
    sd_time = sd(mean_distance, na.rm = TRUE)
  ) |>
  dplyr::ungroup()


#-- [[ Omnibus test ]]
expt3_distance_aov <- afex::aov_ez(
  data = expt3_mean_distance_b_p_tm,
  id = "sub_id",
  dv = "mean_distance",
  within = c("trial_mode", "box_number")
)
expt3_distance_aov
##-- Main effect trial mode: F(1, 49) = 10.22, p = .002, ges = .046
##-- Main effect box number: F(1.49, 73.24) = 934.59, p < .001, ges = .790
##-- Interaction: F(1.59, 77.98) = 94.38, p < .001, ges = .188

#-- [[ Post hoc interaction and compute effect size ]]
emmeans::emmeans(
  expt3_distance_aov,
  specs = c("trial_mode", "box_number")
) |>
  pairs(adjust = "holm")
##-- X1 == alone; X2 == together
##-- X4, X8, X12 == 4, 8, and 12 boxes, respectively
##-- contrast        estimate      SE df t.ratio p.value
##-- X1 X4 - X2 X4     0.0709 0.00799 49   8.863  <.0001 [*]
##-- X1 X4 - X1 X8    -0.2365 0.00465 49 -50.831  <.0001 [*]
##-- X1 X4 - X2 X8    -0.2636 0.02120 49 -12.431  <.0001 [*]
##-- X1 X4 - X1 X12   -0.4018 0.00850 49 -47.292  <.0001 [*]
##-- X1 X4 - X2 X12   -0.5928 0.02820 49 -21.018  <.0001 [*]
##-- X2 X4 - X1 X8    -0.3074 0.00795 49 -38.652  <.0001 [*]
##-- X2 X4 - X2 X8    -0.3344 0.01649 49 -20.276  <.0001 [*]
##-- X2 X4 - X1 X12   -0.4727 0.01010 49 -46.790  <.0001 [*]
##-- X2 X4 - X2 X12   -0.6636 0.02602 49 -25.506  <.0001 [*]
##-- X1 X8 - X2 X8    -0.0271 0.01954 49  -1.385  0.1724
##-- X1 X8 - X1 X12   -0.1653 0.00650 49 -25.444  <.0001 [*]
##-- X1 X8 - X2 X12   -0.3563 0.02681 49 -13.290  <.0001 [*]
##-- X2 X8 - X1 X12   -0.1382 0.01955 49  -7.068  <.0001 [*]
##-- X2 X8 - X2 X12   -0.3292 0.01959 49 -16.802  <.0001 [*]
##-- X1 X12 - X2 X12  -0.1910 0.02514 49  -7.599  <.0001 [*]

##-- P value adjustment: holm method for 15 tests

effectsize::rm_d(
  expt3_mean_distance_b_p_tm_wide$alone_4, expt3_mean_distance_b_p_tm_wide$alone_8, adjust = FALSE
)
effectsize::rm_d(
  expt3_mean_distance_b_p_tm_wide$alone_4, expt3_mean_distance_b_p_tm_wide$alone_12, adjust = FALSE
)
effectsize::rm_d(
  expt3_mean_distance_b_p_tm_wide$alone_8, expt3_mean_distance_b_p_tm_wide$alone_12, adjust = FALSE
)

effectsize::rm_d(
  expt3_mean_distance_b_p_tm_wide$together_4, expt3_mean_distance_b_p_tm_wide$together_8, adjust = FALSE
)
effectsize::rm_d(
  expt3_mean_distance_b_p_tm_wide$together_4, expt3_mean_distance_b_p_tm_wide$together_12, adjust = FALSE
)
effectsize::rm_d(
  expt3_mean_distance_b_p_tm_wide$together_8, expt3_mean_distance_b_p_tm_wide$together_12, adjust = FALSE
)

effectsize::rm_d(
  expt3_mean_distance_b_p_tm_wide$alone_4, expt3_mean_distance_b_p_tm_wide$together_4, adjust = FALSE
)
effectsize::rm_d(
  expt3_mean_distance_b_p_tm_wide$alone_8, expt3_mean_distance_b_p_tm_wide$together_8, adjust = FALSE
)
effectsize::rm_d(
  expt3_mean_distance_b_p_tm_wide$alone_12, expt3_mean_distance_b_p_tm_wide$together_12, adjust = FALSE
)
##-- A4 vs A8: d(rm) = 6.15, 95CI [-7.20, -5.09]
##-- A4 vs A12: d(rm) = 5.87, 95CI [-6.90, -4.83]
##-- A8 vs A12: d(rm) = 2.22, 95CI [-2.54, -1.90]
##--
##-- T4 vs T8: d(rm) = 2.04, 95CI [-2.39, -1.70]
##-- T4 vs T12: d(rm) = 3.54, 95CI [-4.27, -2.81]
##-- T8 vs T12: d(rm) = 1.71, 95CI [-2.02, -1.39]
##--
##-- A4 vs T4: d(rm) = -1.55, 95CI [1.04, 2.05]
##-- A8 vs T8: d(rm) = 0.20, 95CI [-0.49, 0.09]
##-- A12 vs T12: d(rm) = 1.20, 95CI [-1.35, -0.70]
