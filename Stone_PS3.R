# Andy Stone
# Problem Set 3
# February 25, 2016

setwd("~/github/PS3") # Setting working directory

#### Problem 1 (S3) ####
# Define a new class: door. Objects of this class simply take on one numeric value: 
# 1, 2, or 3 – indicating which door a candidate chooses.

## Function for creating a new list object of the class "door"
#'
#' This function takes an optional input that is an integer of the value 1, 2, or 3, corresponding to 
#' a door choice in the Let's Make a Deal game. 
#' 
#' @param whichdoor (optional) An integer of the value 1, 2, or 3. If a value is input that is not
#' one of these values, the function will automatically choose one of the three values for the user.
#' 
#' @return A list of the class "door" with the element choice, where choice has one element, equal to 
#' 1, 2, or 3. 
#' 
#' @author Andy Stone.


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


## Creates a generic function PlayGame.
#'
#' This function is the generic for PlayGame methods.  
#' 
#' @param x An object input. 
#' 
#' @author Andy Stone.

# Creating a generic
PlayGame <- function(x){
  UseMethod("PlayGame", x)
}

## Creates a default method for the PlayGame generic. 
#'
#' This function is the default method for PlayGame. If no specific method exists for the class of 
#' the object passed to PlayGame, it will call this function. It will warn the user that no method 
#' exists for an object of the class passed to the function.
#' 
#' @param x An object input. 
#' 
#' @return The object passed to the function.
#' 
#' @author Andy Stone.

# Backup default method
PlayGame.default <- function(x){
  print("This object doesn't have an associated PlayGame method. This is the default response.
        Try methods('PlayGame') to see objects with a PlayGame method.")
  return(x)
}

## Creates a method for the PlayGame generic for objects of the class "door."
#'
#' This function creates a PlayGame method for objects of the class "door," allowing the user to play
#' a simplified version of Let's Make a Deal. The input is an object of the class "door," which
#' should have the element choice with one element equal to 1, 2, or 3. Such an object can be created 
#' using the Door_Object() function. The function randomly draws a winning door equal to 1, 2, or 3. It
#' then checks to see whether the user's choice is equal to the winning door or not. It then reveals
#' whether the user wins or loses, and prints a web link letting the user know what their prize looks 
#' like. 
#' 
#' @param x A list object of the class "door."
#' 
#' @author Andy Stone.


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


setGeneric(name="PlayGame", 
           def=function(x){
             standardGeneric("PlayGame")
             }
)

setMethod(f="PlayGame",
          signature="door",
          definition=function(x)
          {
            validObject(x)
            print("This is a door object! Let's figure out if you won or lost.")
            winning.door <- sample(c(1,2,3), 1)
            if(winning.door == x@doorchoice){
              print(paste("Remember, you chose door number ", x@doorchoice, ".", sep=""))
              print("Unveiling door....")
              Sys.sleep(1)
              print("....")
              Sys.sleep(1)
              print(paste("The winning door is door number ", winning.door,".", sep=""))
              print("Yay! You win the car!") 
              print("Visit https://tinyurl.com/winningcar to view your prize.")
            }
            else{
              print(paste("Remember, you chose door number ", x@doorchoice, ".", sep=""))
              print("Unveiling door....")
              Sys.sleep(1)
              print("....")
              Sys.sleep(1)
              print(paste("The winning door is door number ", winning.door, ".", sep=""))
              print("Sorry. You didn't choose the winning door. You got a ZONK!")
              print("Visit https://tinyurl.com/yourZONK to view your prize.") 
            }
          }
)

test.door <- door(doorchoice=3)
test.door@doorchoice <- 6

PlayGame(test.door)





