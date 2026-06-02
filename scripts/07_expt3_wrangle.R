# ----------------------------------------------------------------
# TRADEOFFS BETWEEN INSTRUMENTAL UTILITY AND SOCIAL REWARDS SHAPE PREFERENCES FOR COSTLY COOPERATION
#   -- Lalli, Al Afif, Tang, Croteau, Rathwell, Lokesh, Cashaback, Carter
#
# Experiment 3 data wrangling
#
# Author(s):
#   - Mikayla Lalli
#   - Mike Carter
# ----------------------------------------------------------------


#-- [ SCRIPT SETUP ] ----
source(here::here("scripts/00_libraries-and-functions.R"))

#-- [[ Load data files ]]
expt3_data <- readr::read_csv(
  here("data/expt3_behaviour-data.csv")
  )

#-- [[ Create workable tibbles and make variables factors ]]
expt3_tib <- expt3_data |>
  dplyr::mutate(
    sub_id = forcats::as_factor(sub_id)
    )


#-- [ CHOICE PROPORTIONS IN CHOICE TRIALS ] ----

#-- [[ Create needed tibbles and get proportions at the participant (p)
#--    level then add choice value of 0 for participants that never chose
#--    the together option on choice trials and append to tibble ]]
expt3_choice_tib <- expt3_tib |>
  #-- Trial mode 3 == choice-alone and trial mode 4 == choice-together
  dplyr::filter(trial_mode == 3 | trial_mode == 4)

expt3_choices_p <- expt3_choice_tib |>
  dplyr::group_by(sub_id) |>
  dplyr::count(trial_mode) |>
  dplyr::rename("choices" = "n") |>
  dplyr::mutate(proportion = choices/90)

expt3_together_s112 <- data.frame(
  sub_id = "s112",
  trial_mode = 4,
  choices = 0,
  proportion = 0)

expt3_together_s206 <- data.frame(
  sub_id = "s206",
  trial_mode = 3,
  choices = 0,
  proportion = 0)

expt3_together_s404 <- data.frame(
  sub_id = "s404",
  trial_mode = 3,
  choices = 0,
  proportion = 0)

expt3_choices_p <- rbind(
  expt3_choices_p,
  expt3_together_s112,
  expt3_together_s206,
  expt3_together_s404
  ) |>
  dplyr::arrange(sub_id) |>
  dplyr::mutate(trial_mode = forcats::as_factor(trial_mode))


#-- [[ Create a tibble with proportion of together choices at the box number
#--    (b) level and grouped by participant. Add rows with choice values of 0
#--    for those who never chose together under certain box number conditions
#--    then append to the main tibble ]]
expt3_together_choices_b <- expt3_choice_tib |>
  dplyr::filter(trial_mode == 4) |>
  dplyr::select(sub_id,
                box_number) |>
  dplyr::group_by(sub_id) |>
  dplyr::count(box_number) |>
  dplyr::mutate(proportion = n/30) |> # 30 choice trials for each box number
  dplyr::rename("together_choices" = "n")

expt3_s112 <- data.frame(
  "sub_id" = c("s112", "s112", "s112"),
  "box_number" = c(4, 8, 12),
  "together_choices" = c(0, 0, 0),
  "proportion" = c(0, 0, 0))

expt3_s204 <- data.frame(
  "sub_id" = c("s204", "s204"),
  "box_number" = c(4, 8),
  "together_choices" = c(0, 0),
  "proportion" = c(0, 0))

expt3_s207 <- data.frame(
  "sub_id" = ("s207"),
  "box_number" = (8),
  "together_choices" = (0),
  "proportion" = (0))

expt3_s302 <- data.frame(
  "sub_id" = c("s302", "s302"),
  "box_number" = c(4, 12),
  "together_choices" = c(0, 0),
  "proportion" = c(0, 0))

expt3_s307 <- data.frame(
  "sub_id" = ("s307"),
  "box_number" = (8),
  "together_choices" = (0),
  "proportion" = (0))

expt3_s313 <- data.frame(
  "sub_id" = ("s313"),
  "box_number" = (8),
  "together_choices" = (0),
  "proportion" = (0))

expt3_together_choices_b <- rbind(
  expt3_together_choices_b,
  expt3_s112,
  expt3_s204,
  expt3_s207,
  expt3_s302,
  expt3_s307,
  expt3_s313
  ) |>
  dplyr::arrange(sub_id, box_number) |>
  dplyr::group_by(sub_id) |>
  dplyr::mutate(box_number = forcats::as_factor(box_number))

#-- [[ Create wide format tibbles of together_choices_b ]]
expt3_together_choices_b_wide <- tidyr::pivot_wider(
  expt3_together_choices_b,
  id_cols = sub_id,
  names_from = box_number,
  values_from = proportion
  )

#-- [[ Create a tibble with mean proportion of choices across all
#--    participants for all trials. Then create a column with mean proportion
#--    of choices for each trial in each trial mode. Next create a tibble
#--    with mean proportion of together choices across all participants and
#--    change trial numbers to be 1 to 90 ]]
expt3_mean_choice_tib <- expt3_choice_tib |>
  dplyr::group_by(trial_number) |>
  dplyr::count(trial_mode) |>
  dplyr::rename("choices" = "n") |>
  dplyr::mutate(proportion = choices/50)

expt3_mean_choice_tib <- expt3_mean_choice_tib |>
  dplyr::group_by(trial_number, trial_mode) |>
  dplyr::summarise(mean_proportion = mean(proportion))

expt3_mean_together_choice_tib <- expt3_mean_choice_tib |>
  dplyr::filter(trial_mode == 4)

expt3_mean_together_choice_tib$trial_number[1:90] <- c(1:90)


#-- [ TRIAL TIME IN THE FORCED ALONE AND TOGETHER TRIALS ] ----

#-- [[ Create tibble for trial time outcome variable then create a tibble
#--    with mean trial times at the participant (p) level and grouped by
#--    trial mode (tm). Also create a tibble with mean times at the box
#--    number (b) level grouped by participant and trial mode ]]
expt3_time_tib <- expt3_tib |>
  #-- Trial mode 1 == forced alone and trial mode 2 == forced together
  dplyr::filter(trial_mode == 1 | trial_mode == 2)

expt3_mean_time_p_tm <- expt3_time_tib |>
  dplyr::group_by(sub_id, trial_mode) |>
  dplyr::summarise(mean_trial_time = mean(trial_time), .groups = "keep")

expt3_mean_time_b_p_tm <- expt3_time_tib |>
  dplyr::group_by(sub_id, trial_mode, box_number) |>
  dplyr::summarise(mean_trial_time = mean(trial_time), .groups = "keep")

#-- [[ Create a wide format tibble of mean trial times and rename columns ]]
expt3_mean_time_b_p_tm_wide <- tidyr::pivot_wider(
  expt3_mean_time_b_p_tm,
  id_cols = sub_id,
  names_from = c(trial_mode, box_number),
  values_from = mean_trial_time
  )

expt3_mean_time_b_p_tm_wide <- expt3_mean_time_b_p_tm_wide |>
  dplyr::rename(
    alone_12 = "1_12",
    alone_4 = "1_4",
    alone_8 = "1_8",
    together_4 = "2_4",
    together_8 = "2_8",
    together_12 = "2_12"
    )

#-- [[ Create a tibble with mean times at the box number (b) level grouped
#--    by participant and also make one in wide format ]]
expt3_mean_time_p_b <- expt3_time_tib |>
  dplyr::group_by(sub_id, box_number) |>
  dplyr::summarise(mean_trial_time = mean(trial_time), .groups = "keep")

expt3_mean_time_p_b_wide <- tidyr::pivot_wider(
  expt3_mean_time_p_b,
  id_cols = sub_id,
  names_from = box_number,
  values_from = mean_trial_time
  )


#-- [ DISTANCE TRAVELED IN FORCED ALONE AND TOGETHER TRIALS ]

#-- [[ Create needed tibbles for distance travelled outcome variable:
#--    i) tibble with mean distance traveled (in metres) at the participant (p)
#--    level grouped by trial mode (tm)
#--    ii) tibble with mean distance traveled at the participant level (p) and
#--    grouped by box number (b)
#--    iii) tibble with mean distance traveled at the participant (p) level and
#--     grouped by box number (b) in wide format
#--    iv) tibble with mean distance traveled at the box number (b) level and
#--    grouped by participant (p) and trial mode (tm); also make a wide one ]]
expt3_distance_tib <- expt3_tib |>
  #-- Trial mode 1 == forced alone and trial mode 2 == forced together
  dplyr::filter(trial_mode == 1 | trial_mode == 2)

expt3_mean_distance_p_tm <- expt3_distance_tib |>
  dplyr::group_by(
    sub_id,
    trial_mode
  ) |>
  dplyr::summarise(
    mean_distance = mean(total_distance),
    groups = "keep"
  ) |>
  dplyr::ungroup()

expt3_mean_distance_p_b <- expt3_distance_tib |>
  dplyr::group_by(
    sub_id,
    box_number
  ) |>
  dplyr::summarise(
    mean_distance = mean(total_distance),
    .groups = "keep"
  ) |>
  dplyr::ungroup()

expt3_mean_distance_p_b_wide <- tidyr::pivot_wider(
  expt3_mean_distance_p_b,
  id_cols = sub_id,
  names_from = (box_number),
  values_from = mean_distance
  )

expt3_mean_distance_b_p_tm <- expt3_distance_tib |>
  dplyr::group_by(
    sub_id,
    trial_mode,
    box_number
  ) |>
  dplyr::summarise(
    mean_distance = mean(total_distance),
    .groups = "keep"
  )

expt3_mean_distance_b_p_tm_wide <- tidyr::pivot_wider(
  expt3_mean_distance_b_p_tm,
  id_cols = sub_id,
  names_from = c(trial_mode, box_number),
  values_from = mean_distance
)

expt3_mean_distance_b_p_tm_wide <- expt3_mean_distance_b_p_tm_wide |>
  dplyr::rename(
    alone_12 = "1_12",
    alone_4 = "1_4",
    alone_8 = "1_8",
    together_4 = "2_4",
    together_8 = "2_8",
    together_12 = "2_12"
  )
