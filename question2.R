###Word Guessing Game, but in R-edition
#list of words to guess; equiv. to myList in py
words <- c("apple", "brave", "crane", "drive", "eagle", 
           "fable", "grape", "house", "input", "joker",
           "knife", "lemon", "mango", "nerve", "ocean")

#game function; equiv. to Def in py
guess_word <- function(max_tries) {
  #choose a random word
  word <- sample(words, 1)
  tries <- 0
  #print welcome message and # of tries
  print("Welcome to the Word Guessing Game!")
  print(paste("You have", max_tries, "attempts to guess the word."))
  #game loop and conditioning
  while (tries < max_tries) {
    #get user input
    print("Enter your guess:")
    guess <- tolower(readline()) #lowercases the input
    #validate input
    if (nchar(guess) != 5 || !grepl("^[a-z]+$", guess)) { #informs if input is invalid if larger than 5 letters
      print("Invalid input. Enter a 5-letter word.")
      next #trigger
    }
    #increment tries
    tries <- tries + 1
    #check if guess is correct
    if (guess == word) {
      print(paste("Congratulations! You won in", tries, "attempts."))
      return()
    }
    #provide letter matching info
    feedback <- character(5)
    for (i in 1:5) {
      if (substr(guess, i, i) == substr(word, i, i)) {
        feedback[i] <- substr(guess, i, i)
      } else {
        feedback[i] <- "_"
      }
    }
    #print feedback
    print("Wrong guess!")
    print(paste("Matched letters:", paste(feedback, collapse = "")))
    print(paste("Attempts left:", max_tries - tries))
  }
  #game over message
  print(paste("Sorry, you lost. The word was:", word))
}

#start the game
set.seed(123) #allows for reproduceability
guess_word(5) #allows for 5 guesses
