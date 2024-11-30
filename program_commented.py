#similar to packages in R
import random

#below is a list of  15 five-letter words
mylist = [
    "apple", "brave", "crane", "drive", "eagle",
    "fable", "grape", "house", "input", "joker",
    "knife", "lemon", "mango", "nerve", "ocean"
]

#like a function
def myfunction1(mylist, mynumber2):
    mystring1 = random.choice(mylist).lower() #pulls a random word that the user must guess
    mynumber1 = 0 #the base of the # of times the user tries the game

#a welcome statements followed by dynamic language because it inputs what your choose
#and informs if you're right or wrong, and the # of attempts left
    print("Welcome!")
    print(f"You have {mynumber2} attempts.")
#loop, this will let the game continue until the user gets the right word
#or when attempts finish
    while mynumber1 < mynumber2:
        #capture the user input, lowercases inputs
        mystring2 = input("Enter your guess: ").lower().strip()
        #conditions the input to be either in the list or exactly 5 letters
        if len(mystring2) != 5 or not mystring2.isalpha():
            print("Invalid input.") #tells user their input is invalid if triggered
            continue #continues if the above isn't trigger
        #attempts are counted with this below
        mynumber1 = mynumber1 + 1
        #if there's a match between user input and random word selected,
        #the user is congratulated by notification of the message below
        #and the number of attempts it took to win
        if mystring2 == mystring1:
            print(f"Congratulations! You won in {mynumber1} attempts.")
            break
        else: #gives hint of what letters match for the correct answer with
        #the letters of the wrong answers by letter position in below
        #but also tells what letters are unmatched with '_'
            mymessage = ''
            for i in range(5):
                if mystring2[i] == mystring1[i]:
                    mymessage = mymessage + mystring1[i]
                else:
                    mymessage = mymessage + '_'
            mynumber3 = mynumber2 - mynumber1 #calculates remaining attempts and informs if wrong
            print(f"Wrong! Here's what you got right: {mymessage}")  
            print(f"You have {mynumber3} attempts left.")
    #indicates you lost if you're run out of attempts and gives right answer
    if mynumber1 == mynumber2:
        print(f"Sorry, you lost. The correct answer was: '{mystring1}'.")

#starts the game
myfunction1(mylist, 5)
