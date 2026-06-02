# ----------------------------------------------------------------
# SOLO VERSUS JOINT ACTION IN A COSTLY COOPERATION TASK
#   -- Lalli, Al Afif, Tang, Croteau, Rathwell, Cashaback, Carter
#
# Required libraries and functions
#
# Author(s):
#   - Mikayla Lalli
#   - Mike Carter
# ----------------------------------------------------------------


#-- [ SCRIPT SETUP ] ----
#
#-- [[ Libraries ]]
here::i_am("scripts/00_libraries-and-functions.R")

library(afex)
library(easystats)
library(conflicted)
library(emmeans)
library(grateful)
library(here)
library(Hmisc)
library(magick)
library(patchwork)
library(rcompanion)
library(tidyverse)
library(WRS2)

conflicted::conflicts_prefer(
  dplyr::filter(),
  dplyr::lag(),
  dplyr::src(),
  dplyr::summarize(),
  tidyr::expand(),
  tidyr::pack(),
  tidyr::unpack()
)
