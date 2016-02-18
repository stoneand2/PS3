# Andy Stone
# Problem Set 3
# February 25, 2016

setwd("~/github/PS3") # Setting working directory

#### Problem 1 (S3) ####
# Define a new class: door. Objects of this class simply take on one numeric value: 
# 1, 2, or 3 – indicating which door a candidate chooses.

doorchoice <- list(choice=) # creating list
class(doorchoice) <- "door" # making our list of class "door"

# Function to create a new object (a list) of the class "door"

Door_Object <- function(whichdoor=NA){
  if(whichdoor %in% c(1,2,3)){
    doorchoice <- list(choice=whichdoor)
    class(doorchoice) <- "door"
    return(doorchoice)
  }
  else{
    draw <- sample(c(1,2,3), 1)
    doorchoice <- list(choice=draw)
    class(doorchoice) <- "door"
    print(paste("You didn't pick one of the doors. So, your door was randomly assigned to be", draw))
    return(doorchoice)
  }
}

# object.door <- Door_Object()
# class(object.door)


#### Problem 2 (S3) #### 
# Create a method for door objects that is called PlayGame. 
# This method is supposed to do the following:

# Take the numeric value that is stored in the door object,

# draw a random number between 1 and 3 that presents the door
# behind which the car is hidden,

# compare the two numbers, and print a message congratulating a winning candidate 
# that chose the correct door, or expressing sympathies for a losing candidate that chose the 
# wrong door.


# Creating a generic
PlayGame <- function(x){
  UseMethod("PlayGame", x)
}
# Backup default method
PlayGame.default <- function(x){
  print("This object doesn't have an associated PlayGame method. This is the default response.
        Try methods('PlayGame') to see objects with a PlayGame method.")
  return(x)
}
# The method for door objects 
PlayGame.door <- function(x){
  print("This is a door object! Let's figure out if you won or lost.")
  winning.door <- sample(c(1,2,3), 1)
  if(winning.door == x$choice){
    print(paste("Remember, you chose door number ", x$choice, ".", sep=""))
    print("Unveiling door....")
    Sys.sleep(1)
    print("....")
    Sys.sleep(1)
    print(paste("The winning door is door number ", winning.door,".", sep=""))
    print("Yay! You win the car!") 
    print("Visit https://tinyurl.com/winningcar to view your prize.")
  }
  else{
    print(paste("Remember, you chose door number ", x$choice, ".", sep=""))
    print("Unveiling door....")
    Sys.sleep(1)
    print("....")
    Sys.sleep(1)
    print(paste("The winning door is door number ", winning.door, ".", sep=""))
    print("Sorry. You didn't choose the winning door. You got a ZONK!")
    print("Visit https://tinyurl.com/yourZONK to view your prize.") 
  }
}


#### Problem 1 (S4) ####

door <- setClass(Class="door", slots = c(doorchoice = "numeric"),
                  validity=function(object){
                    if((object@doorchoice %in% c(1,2,3)) == FALSE) {
                      return("A door choice not equal to the numeric 1, 2, or 3 was given.")
                    }
                  return(TRUE)
                }
)

# door(doorchoice=1) # works
# door(doorchoice=4) # validity check determines not a valid door choice

# # Validity function to make sure any changes to object of door class are okay
# setValidity("door", function(object){
#   if((object@doorchoice %in% c(1,2,3)) == FALSE) {
#     return("A door choice not equal to the numeric 1, 2, or 3 was given.")
#   }
# }
# )

# test.door <- door(doorchoice=1)
# test.door@doorchoice <- 6 
# test.door <- door(doorchoice=3)





