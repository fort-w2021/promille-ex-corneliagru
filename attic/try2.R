# format_drinks ####

format_drinks <- function(drinks) {
  # assert(assert_list(drinks), assert_numeric(drinks), combine = "or")
  assert_numeric(drinks, lower = 0)
  # drinks non negativ
  # drinks only the ones that are allowed and they are named
  # driks are not null
  
  
  # bring lists or vectors in same format
  drinks <- unlist(drinks)
  
  # extend drinks with all possible drinks
  all_drinks <- c("hoibe", "massn", "schnaps", "wein")
  for (d in all_drinks) {
    if (is.na(drinks[d])) {
      drinks[[d]] <- 0
    }
  }
  
  
  # combine multiple mentions of same drink
  drinks <- tapply(drinks, names(drinks), sum)
  
  return(drinks)
}


# check_drinking_age ####
check_drinking_age <- function(age, drinks) {
  
  if (age < 16 & any(drinks > 0)) {
    warning("Drinking under the age of 16 is illegal")
  }
  
  if (age < 18 & drinks[["schnaps"]] > 0) {
    warning("Drinking hard liquor under the age of 18 is illegal")
  }
  
}

# tell_me_your_alc_intake ####

tell_me_your_alc_intake <- function(drinks) {
  
  alc_intake <- 0.8 * (drinks[["massn"]] * 1000 * 0.06 +
                         drinks[["hoibe"]] * 500 * 0.06 +
                         drinks[["schnaps"]] * 40 * 0.4 +
                         drinks[["wein"]] * 200 * 0.11)
  
  return(alc_intake)
}

# tell_me_your_body_water #####

tell_me_your_body_water <- function(age, sex = c("male", "female"),
                                    height, weight) {
  sex <- tolower(sex)
  sex <- match.arg(sex, c("male", "female"))
  
  assert_numeric(age, lower = 0, upper = 150)
  assert_numeric(height, lower = 0, upper = 300)
  assert_numeric(weight, lower = 0, upper = 800)
  
  
  total_body_water <- switch(sex,
                             "male" = 2.447 - 0.09516 * age + 0.1074 * height + 0.3362 * weight,
                             "female" = 0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight)
  return(total_body_water)
}




# tell_me_how_drunk ####

tell_me_how_drunk <- function(age, sex = c("male", "female"),
                              height, weight, drinking_time, drinks) {
  assert_posixct(drinking_time, len = 2, sorted = TRUE)
  
  
  drinks <- format_drinks(drinks)
  check_drinking_age(age, drinks)
  alc_intake <- tell_me_your_alc_intake(drinks)
  total_body_water <- tell_me_your_body_water(age, sex, height, weight)
  
  
  
  promille <- (0.8 * alc_intake) / (1.055 * total_body_water)
  duration <- as.numeric(difftime(drinking_time[[2]], 
                                  drinking_time[[1]], units = "h"))
  
  if (duration > 1) {
    # calculate alcohol breakdown after first hour until 0 promille
    promille <- max(0, promille - ((duration - 1) * 0.15))
  }
  return(promille)
  
}

