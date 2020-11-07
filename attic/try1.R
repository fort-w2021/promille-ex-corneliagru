library(tidyverse)
library(checkmate)
library(testthat)


#TODO: own function for each calculation

drinks <- list("massn" = 2, "schnaps" = 3)
age <- 22
sex <- "female"
height <- 160
weight <- 60
drinking_time <- as.POSIXct(c("2020-11-04 12:00", "2020-11-04 16:00"))
# losgehts ----------------------------------------------------------------

# calculate the blood alcohol level (promille)
tell_me_how_drunk <- function(age, sex = c("male", "female"),
                              height, weight, drinking_time, drinks) {
  sex <- tolower(sex)
  sex <- match.arg(sex, c("male", "female"))

  assert_numeric(age, lower = 0, upper = 150)
  assert_numeric(height, lower = 0, upper = 300)
  assert_numeric(weight, lower = 0, upper = 800)
  assert_posixct(drinking_time, len = 2, sorted = TRUE)
  # assert(assert_list(drinks), assert_numeric(drinks), combine = "or")
  assert_numeric(drinks, lower = 0)
  # drinks non negativ
  # drinks only the ones that are allowed and they are named
  # driks are not null


  if (age < 16) {
    warning("Drinking under the age of 16 is illegal")
  }


  # bring lists or vectors in same format
  drinks <- unlist(drinks)

  # extend drinks with all possible drinks
  all_drinks <- c("hoibe", "massn", "schnaps", "wein")
  for (d in all_drinks) {
    if (is.na(drinks[[d]])) {
      drinks[[d]] <- 0
    }
  }

  if (age < 18 & drinks[["schnaps"]] > 0) {
    warning("Drinking hard liquor under the age of 18 is illegal")
  }


  # combine mulpile mentions of same drink
  drinks <- tapply(drinks, names(drinks), sum)


  alc_intake <- 0.8 * (drinks[["massn"]] * 1000 * 0.06 +
    drinks[["hoibe"]] * 500 * 0.06 +
    drinks[["schnaps"]] * 40 * 0.4 +
    drinks[["wein"]] * 200 * 0.11)



  total_body_water <- switch(sex,
    "male" = 2.447 - 0.09516 * age + 0.1074 * height + 0.3362 * weight,
    "female" = 0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight
  )



  promille <- (0.8 * alc_intake) / (1.055 * total_body_water)
  duration <- as.numeric(difftime(drinking_time[[2]], 
                                  drinking_time[[1]], units = "h"))

  if (duration > 1) {
    # calculate alcohol breakdown after first hour until 0 promille
    promille <- max(0, promille - ((duration - 1) * 0.15))
  }
  return(promille)
}

tell_me_how_drunk(
  age = 21,
  sex = "f",
  height = 177,
  weight = 66,
  drinking_time = as.POSIXct(c("2016-10-03 18:00:00", "2016-10-03 19:00:00")),
  drinks = c("schnaps" = 1, "wein" = 2)
)
