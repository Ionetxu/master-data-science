library(lattice)
library(beeswarm)
library(ggplot2)
library(dplyr)


#The monty hall problem
B<- 10000
stick <- replicate(B, {
  doors <- as.character (1:3)
  prize <- sample(c("car","goat","goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick,prize_door)],1)
  stick <- my_pick
  stick == prize_door
})

mean(stick)

switch <- replicate(B, {
 doors <- as.character(1:3)
 prize <- sample(c("car","goat","goat"))
 prize_door <- doors[prize == "car"]
 my_pick <- sample(doors, 1)
 show <- sample(doors[!doors %in% c(my_pick,prize_door)],1)
 stick <- my_pick
switch <- doors [!doors %in%c(my_pick, show)]
switch ==prize_door
})
mean(switch)
