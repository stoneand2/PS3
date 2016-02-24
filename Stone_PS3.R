# Andy Stone
# Problem Set 3
# February 25, 2016

setwd("~/github/PS3") # Setting working directory

#### Problem 1 (S3) ####

## Function for creating a new list object of the class "door"
#'
#' This function takes an optional input that is an integer of the value 1, 2, or 3, corresponding to 
#' a door choice in the Let's Make a Deal game. If such an integer is not input, one will be assigned.
#' A list of the class "door" is returned. 
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


## This function creates the generic function PlayGame.
#'
#' This function is the generic for PlayGame methods.  
#' 
#' @param x An object input. 
#' 
#' @author Andy Stone.

PlayGame <- function(x){
  # UseMethod() tells the R system to search for the method associated with the class of object iput
  UseMethod("PlayGame", x)
}

## Creates a default method for the PlayGame generic. 
#'
#' This function is the default method for PlayGame. If no specific method exists for the class of 
#' the object passed to PlayGame, it will call this function. It will warn the user that no method 
#' exists for an object of the class passed to the function and return the input as it was input. 
#' 
#' @param x An object input. 
#' 
#' @return The object passed to the function.
#' 
#' @author Andy Stone.

# Backup default method
PlayGame.default <- function(x){
  print("This object doesn't have an associated PlayGame method. This is the default response.")
  print("Try methods('PlayGame') to see objects with a PlayGame method.")
  return(x)
}

## Creates a method for the PlayGame generic for objects of the class "door."
#'
#' This function creates a PlayGame method for objects of the class "door," allowing the user to play
#' a simplified version of Let's Make a Deal. The input is an object of the class "door," which
#' should have the element choice with one element equal to 1, 2, or 3. Such an object can be created 
#' using the Door_Object() function. The function randomly draws a winning door equal to 1, 2, or 3. 
#' It then checks to see whether the user's choice is equal to the winning door or not. It then 
#' dramatically reveals whether the user wins or loses, and prints a web link letting the user know 
#' what their prize looks like. 
#' 
#' @param x A list object of the class "door."
#' 
#' @author Andy Stone.


# The method for door objects 
PlayGame.door <- function(x){
  print("Let's figure out if you won or lost.")
  # Randomly sampling the winning door
  winning.door <- sample(c(1,2,3), 1)
  # If/else statement to determine if the user's choice was the winner or loser
  if(winning.door == x$choice){
    print(paste("Remember, you chose door number ", x$choice, ".", sep=""))
    print("Unveiling door....")
    # Pausing for a few seconds to make the reveal more dramatic 
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
    # Pausing for a few seconds to make the reveal more dramatic
    Sys.sleep(1)
    print("....")
    Sys.sleep(1)
    print(paste("The winning door is door number ", winning.door, ".", sep=""))
    print("Sorry. You didn't choose the winning door. You got a ZONK!")
    print("Visit https://tinyurl.com/yourZONK to view your prize.") 
  }
}


#### Problem 1 (S4) ####

## Function that allows the user to create an object of the class "door."
#'
#' This function allows the user to create a "door" object. The function door() has one data element,
#' defined by the slot, which is doorchoice. The user is required to specify the choice when running
#' the function using doorchoice=X. The doorchoice is numeric, and is restricted to be equal
#' to 1, 2, or 3. The restriction is ensured by the validity argument. If a value is passed that is 
#' not doorchoice=1, 2, or 3, the function will break. If no argument is passed, there will be no
#' doorchoice assigned.
#' 
#' It is important to note that this validity check IS NOT automatically applied if the user later 
#' modifies the slots of the "door" object directly (see Hadley, http://adv-r.had.co.nz/S4.html). 
#' However, this can be dealt with by checking the validity using validObject() within any function 
#' that this object is passed to. This is the strategy I employ below.
#' 
#' @return x An object of the class "door."
#' 
#' @author Andy Stone.

door <- setClass(Class="door", slots = c(doorchoice = "numeric"),
                  validity=function(object){
                    if((object@doorchoice %in% c(1,2,3)) == FALSE) {
                      return("A door choice not equal to the numeric 1, 2, or 3 was given.")
                    }
                  return(TRUE)
                }
)

# ourchoice <- door(doorchoice=1) # works, creates object of class "door" 
# door(doorchoice=4) # validity check determines not a valid door choice

# As noted, we CAN assign invalid value to slot if we do it directly after creating object:
# ourchoice@doorchoice <- 6
# BUT, we will deal with this below by not allowing the PlayGame method to run with an invalid
# number assigned to the element

# # This is an alternative way of specifying the validity argument, just oustide of setClass 
# setValidity("door", function(object){
#   if((object@doorchoice %in% c(1,2,3)) == FALSE) {
#     return("A door choice not equal to the numeric 1, 2, or 3 was given.")
#   }
# }
# )

## This function creates the generic function PlayGame.
#'
#' This function is the generic for PlayGame methods.  
#' 
#' @param x An object input. 
#' 
#' @author Andy Stone.

setGeneric(name="PlayGame", 
           def=function(x){
             # Initiates dispatch of the S4 method
             standardGeneric("PlayGame")
             }
)

## This function creates the method PlayGame for objects of "door" class.
#'
#' This function creates a PlayGame method for objects of the class "door," allowing the user to play
#' a simplified version of Let's Make a Deal. The input is an object of the class "door," which
#' should have the element choice with one element equal to 1, 2, or 3. The method carries out a
#' validity check with the validObject(x) function, which calls the validity argument of the setClass
#' function defined above to ensure the door value is equal to 1, 2, or 3. If it is invalid, the 
#' function breaks and returns the message defined in setClass above. If it is valid, the function 
#' randomly draws a winning door equal to 1, 2, or 3. It then checks to see whether the user's choice
#' is equal to the winning door or not. It then dramatically reveals whether the user wins or loses,
#' and prints a web link letting the user know what their prize looks like. 
#' 
#' @param x An object of the class "door."
#' 
#' @author Andy Stone.

setMethod(f="PlayGame",
          # Class the method is used for
          signature="door",
          # The method itself
          definition=function(x)
          {
            # Forcing the validity check
            validObject(x)
            print("This is a door object! Let's figure out if you won or lost.")
            # Randomly sampling winning door
            winning.door <- sample(c(1,2,3), 1)
            # If/else statement to check if user's choice is winner or loser
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

# This will work:
# newdoor <- door(doorchoice=3)
# PlayGame(newdoor)

# This will not work, because we have an invalid element assigned to the doorchoice slot:
# PlayGame(ourchoice)





