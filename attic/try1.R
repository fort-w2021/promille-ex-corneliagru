library(tidyverse)
library(checkmate)
library(testthat)

# TODO: frage: wann werden die getränke getrunken


drinks <- list("massn" = 2, "schnaps" = 3)
age <- 22
sex <- "female"
height <- 160
weight <- 60
drinking_time <- as.POSIXct(c("2020-11-04 12:00", "2020-11-04 16:00"))

# losgehts ----------------------------------------------------------------

tell_me_how_drunk <- function(age, sex = c("male", "female"),
                              height, weight, drinking_time, drinks) {
  
  sex <- tolower(sex)
  sex <- match.arg(sex, c("male", "female"))
  
  assert_numeric(age, lower = 0, upper = 150)
  assert_numeric(height, lower = 0, upper = 300)
  assert_numeric(weight, lower = 0, upper = 800)
  assert_posixct(drinking_time, len = 2, sorted = TRUE)
  #assert(assert_list(drinks), assert_numeric(drinks), combine = "or")
  #drinks non negativ
  # drinks only the ones that are allowed and they are named
  # driks are not null
  
  
  
  
  if (age < 16)
    warning("Drinking under the age of 16 is illegal")


  drinks <- unlist(drinks)

  

  #browser()
  all_drinks <- c("hoibe", "massn", "schnaps", "wein")
  # extend drinks with all possible drinks
  for (d in all_drinks) {
    if (is.na(drinks[d])) {
      drinks[d] <- 0
    }
  }
  
  if (age < 18 & drinks["schnaps"] >0)
    warning("Drinking hard liquor under the age of 18 is illegal")
    
  
  #mulpile mentions of same drink
  drinks <- tapply(drinks, names(drinks), sum)


  # A Alcohol intake
  A <- 0.8 * (drinks["massn"] * 1000 * 0.06 + drinks["hoibe"] * 500 * 0.06 +
    drinks["schnaps"] * 40 * 0.4 + drinks["wein"] * 200 * 0.11)



  GKW <- switch(sex,
    "male" = 2.447 - 0.09516 * age + 0.1074 * height + 0.3362 * weight,
    "female" = 0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight
  )



  c <- (0.8 * A) / (1.055 * GKW)
  duration <- as.numeric(difftime(drinking_time[2], drinking_time[1], units = "h"))

  if (duration > 1) {
    c <- c - ((duration - 1) * 0.15)
  }
  c <- max(0, c)
  return(c)
}

tell_me_how_drunk(
  age = 21,
  sex = "f",
  height = 177,
  weight = 66,
  drinking_time = as.POSIXct(c("2016-10-03 18:00:00", "2016-10-03 19:00:00")),
  drinks = c("schnaps" = 1, "wein" = 2)
)
