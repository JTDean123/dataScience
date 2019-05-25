##--------------------------------------------
##
## Jason Dean
## April 14, 2017
## Monty Hall Simulations
##
##--------------------------------------------

# load libraries
library(logging)


log = function(){
  # set the working directory
  setwd("your directory")
  
  # extract the system time and incorporate this into the log title
  log_file_name = format(Sys.time(), format="HW2_log_%Y_%m_%d_%H%M%S.log")
  
  # set the parameters for logging
  log_level = "INFO"
  basicConfig(level=log_level)
  addHandler(writeToFile, file=log_file_name, level=log_level)
}


game = function(choice){
  
  # choice = TRUE -> keep pick
  # choice = FALSE -> switch
  
  # set up the game by putting the prizes behind doors at random
  prizes = c("car", "goat", "goat")
  doors = sample(prizes, 3, replace=FALSE)

  # pick a random door
  pick = sample(1:3, 1, replace=TRUE)
  
  # option 1: keep the pick
  if (choice == TRUE){
    if (doors[pick] == 'car'){
      # winner!
      return(1)
    } else {
      # loser
      return(0)
    }
  }
  
  # option 2:  switch it up
  if (choice == FALSE){
    if (doors[pick] != 'car'){
      # if the first pick is not the car and the player switches they will win
      return(1)
    } else {
      # else they lose
      return(0)
    }
  }
}


game_unitTest = function(){
  
  # Hmm.. I can't think of a good way to test this function
  # test with both switch and stay.  we should get either a 1 or a 0 back
  test_inputs = c(TRUE, FALSE)
  expected_outputs = c(0, 1)
  
  # ensure that either a 1 or 0 (but only one) is returned for each logical value
  for (i in test_inputs){
    game_output = game(i)
    stopifnot(length(unique(grepl(game_output, expected_outputs))) == 2)
  }
}


probs = function(results){
  # calculate probabilities
  probability = mean(results)
  return(probability)
}


probs_unitTest = function(){
  test_inputs = c(0, 1, 1, 1, 0)
  expected_outputs = mean(test_inputs)
  probs_outputs = probs(test_inputs)
  stopifnot(expected_outputs == probs_outputs)
}


vars = function(results){
  # calculate variances
  variance = var(results)
  return(variance)
}


vars_unitTest = function(){
  test_inputs = c(0, 1, 1, 1, 0)
  expected_outputs = var(test_inputs)
  vars_outputs = vars(test_inputs)
  stopifnot(expected_outputs == vars_outputs)
}


unit_tests = function(){
  
  #test the probability function
  probs_unitTest()
  
  #test the variance function
  vars_unitTest() 
  
  # test the game function
  game_unitTest()
}


if (interactive()){
  
  # start logging
  log()
  loginfo('Monty Hall Simulation')  
  loginfo(paste('Start time: ', Sys.time()))
  
  # perform unit tests
  unit_tests()
  
  # set the number of simulations
  nSims = 10000
  loginfo(paste('Number of simulations', nSims))
          
  # play the game keeping the first pick and calculate statistics
  stay = sapply(1:nSims, function(x){game(TRUE)})
  mean_stay = probs(stay)
  var_stay = vars(stay)
  
  # log stay results
  loginfo(paste("Probability of winning for stay:  ", mean_stay))
  loginfo(paste("Variance of winning for stay:  ", var_stay))
  
  # play the game discarding the first pick
  switch = sapply(1:nSims, function(x){game(FALSE)})
  mean_switch = probs(switch)
  var_switch = vars(switch)
  
  # log switch results
  loginfo(paste("Probability of winning for switch:  ", mean_switch))
  loginfo(paste("Variance of winning for switch:  ", var_switch))
  
  loginfo(paste("Finsih time:  ", Sys.time()))
}
