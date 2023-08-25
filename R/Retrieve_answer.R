library(nycflights13)
library(tidyverse)

f_d <- nycflights13::flights
myDf1 <- filter(f_d, arr_delay >= 2.0)
retrieve_answer <- function(exercise_num){
  if (exercise_num >=1 && exercise_num <=6)
    if(exercise_num ==1){
      myDf1 <- filter(f_d, arr_delay >= 2)
      myDf2 <- filter(f_d,dest =="HOU")
      print(myDf1,myDf2)
    }else if( exercise_num ==2){
      sorted_flights_missing_first <- flights %>%
        arrange(desc(is.na(dep_time)))
      most_delayed_flights <- flights %>%
        arrange(desc(arr_delay))
      fastest_flights_desc <- flights %>%
        mutate(speed = distance / air_time) %>%
        arrange(desc(speed))
      farthest_flights <- flights %>%
        arrange(desc(distance))
      print(sorted_flights_missing_first,most_delayed_flights,fastest_flights_desc,farthest_flights,closest_flights <- flights %>%
              arrange(distance))
  else{
      return("Error")
    }
  return("El número no se encuentra en un rango entre 1 y 6")
  }


retrieve_answer(2)

