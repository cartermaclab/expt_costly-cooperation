# ----------------------------------------------------------------
# TRADEOFFS BETWEEN INSTRUMENTAL UTILITY AND SOCIAL REWARDS SHAPE PREFERENCES FOR COSTLY COOPERATION
#   -- Lalli, Al Afif, Tang, Croteau, Rathwell, Lokesh, Cashaback, Carter
#
# Experiment 4 statistical analyses
#
# Author(s):
#   - Mikayla Lalli
#   - Mike Carter
# ----------------------------------------------------------------


#-- [ SCRIPT SETUP ] ----
source(here::here("scripts/10_expt4_wrangle.R"))


#-- [ BEHAVIOURAL DATA ]

#-- [[ Proportion of bimanual choices ]]
expt4_total_choices_count <- expt4_choice_tib |>
  dplyr::count(trial_mode)

expt4_choice_descriptives <- expt4_choices_p |>
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

#-- [[ Test proportion of bimanual choices against 50% chance level ]]
set.seed(1234)
WRS2::onesampb(
  x = expt4_choices_p$proportion[expt4_choices_p$trial_mode == "4"],
  est = "median",
  nv = 0.5,
)
##-- Robust location estimate: 0.1889
##-- 0.95% confidence interval: 0.0722 0.2833
##-- p-value: 0.002


#-- [ PROPORTION OF BIMANUAL CHOICES BY BOX NUMBER]

#-- [[ Omnibus test ]]
expt4_prop_bimanual_aov <- afex::aov_ez(
  data = expt4_bimanual_choices_b,
  id = "sub_id",
  dv = "proportion",
  within = "box_number"
)
expt4_prop_bimanual_aov
##-- F(1.16, 21.97) = 14.69, p < .001, ges = .077

#-- [[ Post hoc tests and compute Cohen's d with Hedge's correction ]]
emmeans::emmeans(
  expt4_prop_bimanual_aov,
  specs = "box_number"
) |>
  pairs(adjust = "holm")
##-- contrast estimate     SE df t.ratio p.value
##--   X4 - X8    0.1317 0.0380 19   3.461  0.0052
##--   X4 - X12   0.1633 0.0383 19   4.264  0.0013
##--   X8 - X12   0.0317 0.0122 19   2.594  0.0178

##-- P value adjustment: holm method for 3 tests

effectsize::rm_d(
  expt4_bimanual_choices_b_wide$`4`, expt4_bimanual_choices_b_wide$`8`, adjust = FALSE
)
effectsize::rm_d(
  expt4_bimanual_choices_b_wide$`4`, expt4_bimanual_choices_b_wide$`12`, adjust = FALSE
)
effectsize::rm_d(
  expt4_bimanual_choices_b_wide$`8`, expt4_bimanual_choices_b_wide$`12`, adjust = FALSE
)
##-- 4 vs 8: d(rm) = 0.48, 95 CI [0.19, 0.76]
##-- 4 vs 12: d(rm) = 0.59, 95 CI [0.30, 0.88]
##-- 8 vs 12: d(rm) = 0.13, 95 CI [0.03, 0.23]


#-- [ TRIAL TIME IN FORCED UNIMANUAL AND BIMANUAL TRIALS ]

#-- [[ Create tibbles of descriptives for trial time:
#--    - trial mode x box number
#--    - trial mode only
#--    - box number only ]]
expt4_trial_time_descriptives <- expt4_mean_time_b_p_tm |>
  dplyr::group_by(trial_mode, box_number) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_trial_time, na.rm = TRUE),
    sd_time = sd(mean_trial_time, na.rm = TRUE)
    ) |>
  dplyr::ungroup()

expt4_forced_time_descriptives <- expt4_mean_time_p_tm |>
  dplyr::group_by(trial_mode) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_trial_time, na.rm = TRUE),
    sd_time = sd(mean_trial_time, na.rm = TRUE)
    ) |>
  dplyr::ungroup()

expt4_box_time_descriptives <- expt4_mean_time_p_b |>
  dplyr::group_by(box_number) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_trial_time, na.rm = TRUE),
    sd_time = sd(mean_trial_time, na.rm = TRUE)
    ) |>
  dplyr::ungroup()

#-- [[ Omnibus test ]]
expt4_trial_time_aov <- afex::aov_ez(
  data = expt4_mean_time_b_p_tm,
  id = "sub_id",
  dv = "mean_trial_time",
  within = c("trial_mode", "box_number")
  )
expt4_trial_time_aov
##-- Main effect trial mode: F(1, 19) = 43.94, p < .001, ges = .384
##-- Main effect box number: F(1.23, 23.42) = 302.96, p < .001, ges = .507
##-- Interaction: F(1.22, 23.16) = 78.91, p < .001, ges = .131


#-- [[ Post hoc interaction and compute Cohen's d with Hedge's correction ]]
emmeans::emmeans(
  expt4_trial_time_aov,
  specs = c("trial_mode", "box_number")
  ) |>
  pairs(adjust = "holm")
##-- contrast        estimate     SE df t.ratio p.value
##--   X1 X4 - X2 X4     -0.881 0.2225 19  -3.958  0.0025 [*]
##--   X1 X4 - X1 X8     -1.132 0.0550 19 -20.556  <.0001 [*]
##--   X1 X4 - X2 X8     -3.138 0.3252 19  -9.649  <.0001 [*]
##--   X1 X4 - X1 X12    -2.035 0.0835 19 -24.364  <.0001 [*]
##--   X1 X4 - X2 X12    -5.438 0.4869 19 -11.167  <.0001 [*]
##--   X2 X4 - X1 X8     -0.251 0.2156 19  -1.164  0.2587
##--   X2 X4 - X2 X8     -2.258 0.1427 19 -15.824  <.0001 [*]
##--   X2 X4 - X1 X12    -1.155 0.1984 19  -5.819  0.0001 [*]
##--   X2 X4 - X2 X12    -4.557 0.3020 19 -15.090  <.0001 [*]
##--   X1 X8 - X2 X8     -2.007 0.3046 19  -6.587  <.0001 [*]
##--   X1 X8 - X1 X12    -0.904 0.0540 19 -16.724  <.0001 [*]
##--   X1 X8 - X2 X12    -4.306 0.4672 19  -9.217  <.0001 [*]
##--   X2 X8 - X1 X12     1.103 0.2846 19   3.876  0.0025 [*]
##--   X2 X8 - X2 X12    -2.299 0.2093 19 -10.986  <.0001 [*]
##--   X1 X12 - X2 X12   -3.402 0.4452 19  -7.642  <.0001 [*]

##-- P value adjustment: holm method for 15 tests

effectsize::rm_d(
  expt4_mean_time_b_p_tm_wide$unimanual_4, expt4_mean_time_b_p_tm_wide$unimanual_8, adjust = FALSE
)
effectsize::rm_d(
  expt4_mean_time_b_p_tm_wide$unimanual_4, expt4_mean_time_b_p_tm_wide$unimanual_12, adjust = FALSE
)
effectsize::rm_d(
  expt4_mean_time_b_p_tm_wide$unimanual_8, expt4_mean_time_b_p_tm_wide$unimanual_12, adjust = FALSE
)

effectsize::rm_d(
  expt4_mean_time_b_p_tm_wide$bimanual_4, expt4_mean_time_b_p_tm_wide$bimanual_8, adjust = FALSE
)
effectsize::rm_d(
  expt4_mean_time_b_p_tm_wide$bimanual_4, expt4_mean_time_b_p_tm_wide$bimanual_12, adjust = FALSE
)
effectsize::rm_d(
  expt4_mean_time_b_p_tm_wide$bimanual_8, expt4_mean_time_b_p_tm_wide$bimanual_12, adjust = FALSE
)

effectsize::rm_d(
  expt4_mean_time_b_p_tm_wide$unimanual_4, expt4_mean_time_b_p_tm_wide$bimanual_4, adjust = FALSE
)
effectsize::rm_d(
  expt4_mean_time_b_p_tm_wide$unimanual_8, expt4_mean_time_b_p_tm_wide$bimanual_8, adjust = FALSE
)
effectsize::rm_d(
  expt4_mean_time_b_p_tm_wide$unimanual_12, expt4_mean_time_b_p_tm_wide$bimanual_12, adjust = FALSE
)
##-- A4 vs A8: d(rm) = -1.45, 95CI [-1.65, -1.25]
##-- A4 vs A12: d(rm) = -2.04, 95CI [-2.33, -1.75]
##-- A8 vs A12: d(rm) = -1.24, 95CI [-1.43, -1.04]
##--
##-- T4 vs T8: d(rm) = -0.95, 95CI [-1.09, -0.81]
##-- T4 vs T12: d(rm) = -1.18, 95CI [-1.38, -0.98]
##-- T8 vs T12: d(rm) = -0.75, 95CI [-0.90, -0.60]
##--
##-- A4 vs T4: d(rm) = -0.73, 95CI [-1.13, -0.32]
##-- A8 vs T8: d(rm) = -1.12, 95CI [-1.54, -0.69]
##-- A12 vs T12: d(rm) = -1.32, 95CI [-1.79, -0.86]


#-- [ DISTANCE TRAVELED IN FORCED UNIMANUAL AND BIMANUAL TRIALS ]
#--
#-- [[ Create tibbles of descriptives for trial time:
#--    - trial mode x box number
#--    - trial mode only
#--    - box number only ]]
expt4_distance_descriptives <- expt4_mean_distance_b_p_tm |>
  dplyr::group_by(trial_mode, box_number) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_distance, na.rm = TRUE),
    sd_time = sd(mean_distance, na.rm = TRUE)
    ) |>
  dplyr::ungroup()

expt4_forced_distance_descriptives <- expt4_mean_distance_p_tm |>
  dplyr::group_by(trial_mode) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_distance, na.rm = TRUE),
    sd_time = sd(mean_distance, na.rm = TRUE)
    ) |>
  dplyr::ungroup()

expt4_box_distance_descriptives <- expt4_mean_distance_p_b |>
  dplyr::group_by(box_number) |>
  dplyr::summarize(
    n = n(),
    mean_time = mean(mean_distance, na.rm = TRUE),
    sd_time = sd(mean_distance, na.rm = TRUE)
    ) |>
  dplyr::ungroup()


#-- [[ Omnibus test ]]
expt4_distance_aov <- afex::aov_ez(
  data = expt4_mean_distance_b_p_tm,
  id = "sub_id",
  dv = "mean_distance",
  within = c("trial_mode", "box_number")
  )
expt4_distance_aov
##-- Main effect trial mode: F(1, 19) = 3.22, p = .089, ges = .022
##-- Main effect box number: F(1.39, 26.50) = 585.62, p < .001, ges = .823
##-- Interaction: F(1.66, 31.56) = 46.59, p < .001, ges = .109

#-- [[ Post hoc interaction and compute effect size ]]
emmeans::emmeans(
  expt4_distance_aov,
  specs = c("trial_mode", "box_number")
  ) |>
  pairs(adjust = "holm")
##-- X1 == unimanual; X2 == bimanual
##-- X4, X8, X12 == 4, 8, and 12 boxes, respectively
##-- contrast        estimate      SE df t.ratio p.value
##-- X1 X4 - X2 X4     0.0556 0.01384 19   4.016  0.0015 [*]
##-- X1 X4 - X1 X8    -0.2623 0.00907 19 -28.913  <.0001 [*]
##-- X1 X4 - X2 X8    -0.2912 0.02034 19 -14.313  <.0001 [*]
##-- X1 X4 - X1 X12   -0.4442 0.01653 19 -26.871  <.0001 [*]
##-- X1 X4 - X2 X12   -0.5610 0.02747 19 -20.423  <.0001 [*]
##-- X2 X4 - X1 X8    -0.3179 0.01926 19 -16.504  <.0001 [*]
##-- X2 X4 - X2 X8    -0.3468 0.01845 19 -18.796  <.0001 [*]
##-- X2 X4 - X1 X12   -0.4998 0.02449 19 -20.410  <.0001 [*]
##-- X2 X4 - X2 X12   -0.6166 0.02752 19 -22.411  <.0001 [*]
##-- X1 X8 - X2 X8    -0.0289 0.01952 19  -1.479  0.1556
##-- X1 X8 - X1 X12   -0.1819 0.01267 19 -14.354  <.0001 [*]
##-- X1 X8 - X2 X12   -0.2987 0.02571 19 -11.621  <.0001 [*]
##-- X2 X8 - X1 X12   -0.1530 0.02222 19  -6.885  <.0001 [*]
##-- X2 X8 - X2 X12   -0.2699 0.01781 19 -15.154  <.0001 [*]
##-- X1 X12 - X2 X12  -0.1169 0.02425 19  -4.820  0.0004 [*]

##-- P value adjustment: holm method for 15 tests

effectsize::rm_d(
  expt4_mean_distance_b_p_tm_wide$unimanual_4, expt4_mean_distance_b_p_tm_wide$unimanual_8, adjust = FALSE
)
effectsize::rm_d(
  expt4_mean_distance_b_p_tm_wide$unimanual_4, expt4_mean_distance_b_p_tm_wide$unimanual_12, adjust = FALSE
)
effectsize::rm_d(
  expt4_mean_distance_b_p_tm_wide$unimanual_8, expt4_mean_distance_b_p_tm_wide$unimanual_12, adjust = FALSE
)

effectsize::rm_d(
  expt4_mean_distance_b_p_tm_wide$bimanual_4, expt4_mean_distance_b_p_tm_wide$bimanual_8, adjust = FALSE
)
effectsize::rm_d(
  expt4_mean_distance_b_p_tm_wide$bimanual_4, expt4_mean_distance_b_p_tm_wide$bimanual_12, adjust = FALSE
)
effectsize::rm_d(
  expt4_mean_distance_b_p_tm_wide$bimanual_8, expt4_mean_distance_b_p_tm_wide$bimanual_12, adjust = FALSE
)

effectsize::rm_d(
  expt4_mean_distance_b_p_tm_wide$unimanual_4, expt4_mean_distance_b_p_tm_wide$bimanual_4, adjust = FALSE
)
effectsize::rm_d(
  expt4_mean_distance_b_p_tm_wide$unimanual_8, expt4_mean_distance_b_p_tm_wide$bimanual_8, adjust = FALSE
)
effectsize::rm_d(
  expt4_mean_distance_b_p_tm_wide$unimanual_12, expt4_mean_distance_b_p_tm_wide$bimanual_12, adjust = FALSE
)
##-- A4 vs A8: d(rm) = -3.14, 95CI [-3.66, -2.62]
##-- A4 vs A12: d(rm) = -3.48, 95CI [-4.15, -2.81]
##-- A8 vs A12: d(rm) = -1.56, 95CI [-1.87, -1.24]
##--
##-- T4 vs T8: d(rm) = -3.08, 95CI [-3.85, -2.31]
##-- T4 vs T12: d(rm) = -3.64, 95CI [-4.96, -2.92]
##-- T8 vs T12: d(rm) = -1.71, 95CI [-2.05, -1.36]
##--
##-- A4 vs T4: d(rm) = 0.95, 95CI [0.39, 1.51]
##-- A8 vs T8: d(rm) = -0.27, 95CI [-0.64, 0.10]
##-- A12 vs T12: d(rm) = -0.80, 95CI [-1.17, -0.43]
