library(checkmate)

# Frage1: lieber alle assert_bla in den jeweiligen Unterfunktionen in denen die
# variable genutzt wird oder alle am anfang des aufrufs von tell_me_how_drunk?
# Vorteil bei Unterfunktionen: alle abfragen dort wo sie benötigt werden,
# nachteil: es wird im schlimmsten fall bereits viel vorberechnet bis man auf
# einen fehler stößt

# Frage2: wie kann man den error wenn die getränke nicht den erlaubten drinks
# entsprechen besser machen? ich hätte gerne die fehlermeldung "only bla allowed.
# not 'your_stupid_fancy_drink'" (check ist in format_drinks())

# Frage 3: was muss ich machen damit ich bei den tests oben rechts in Rstudio
# einfach "run tests" laufen lassen kann? Error: konnte Funktion
# "tell_me_how_drunk" nicht finden. Vermutung: es reicht nicht dass die funktion
# im global environment ist?

#Frage 4: wann braucht am ende einer funktion "return(bla)" anstatt nur "bla"?
#was ist zu bevorzugen?



# format_drinks ####
# bring all drinks in a homogenous format
format_drinks <- function(drinks) {
  assert(check_list(drinks), check_numeric(drinks), combine = "or")

  # bring lists or vectors in same format
  drinks <- unlist(drinks)

  assert_numeric(drinks, lower = 0, any.missing = FALSE)
 
  all_drinks <- c("hoibe", "massn", "schnaps", "wein")
  
  assertNames(names(drinks), subset.of = all_drinks)
 

  # extend drinks with all possible drinks
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
# check for legal drinking age: below 16 no alcoholic drinks, below 18 no liquor
check_drinking_age <- function(age, drinks) {
  assert_numeric(age, lower = 0, upper = 150, any.missing = FALSE, len = 1)

  if (age < 16 & any(drinks > 0)) {
    warning("Drinking under the age of 16 is illegal")
  }

  if (age < 18 & drinks[["schnaps"]] > 0) {
    warning("Drinking hard liquor under the age of 18 is illegal")
  }
}

# tell_me_your_alc_intake ####
# calculate the alcohol intake for drinks
tell_me_your_alc_intake <- function(drinks) {
  alc_intake <- 0.8 * (drinks[["massn"]] * 1000 * 0.06 +
    drinks[["hoibe"]] * 500 * 0.06 +
    drinks[["schnaps"]] * 40 * 0.4 +
    drinks[["wein"]] * 200 * 0.11)

  return(alc_intake)
}

# tell_me_your_body_water #####
# calculate total body water of the person
tell_me_your_body_water <- function(age, sex = c("male", "female"),
                                    height, weight) {
  sex <- tolower(sex)
  sex <- match.arg(sex, c("male", "female"))

  assert_numeric(height, lower = 0, upper = 300, any.missing = FALSE, len = 1)
  assert_numeric(weight, lower = 0, upper = 800, any.missing = FALSE, len = 1)


  total_body_water <-
    switch(sex,
      "male" = 2.447 - 0.09516 * age + 0.1074 * height + 0.3362 * weight,
      "female" = 0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight
    )
  return(total_body_water)
}



# tell_me_how_drunk ####
# calculate blood alcohol level
tell_me_how_drunk <- function(age, sex = c("male", "female"),
                              height, weight, drinking_time, drinks) {
  assert_posixct(drinking_time, len = 2, sorted = TRUE, any.missing = FALSE)


  drinks <- format_drinks(drinks)
  check_drinking_age(age, drinks)
  alc_intake <- tell_me_your_alc_intake(drinks)
  total_body_water <- tell_me_your_body_water(age, sex, height, weight)


  promille <- (0.8 * alc_intake) / (1.055 * total_body_water)
  duration <- as.numeric(difftime(drinking_time[[2]],
    drinking_time[[1]],
    units = "h"
  ))

  # calculate alcohol breakdown after first hour until 0 promille
  if (duration > 1) {
    promille <- max(0, promille - ((duration - 1) * 0.15))
  }
  return(promille)
}
