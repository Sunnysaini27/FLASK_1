library(tidyverse)
library(rvest)
library(dplyr)



#PROBLEM 1
df <- read_html("https://www.icc-cricket.com/rankings/womens/player-rankings/odi/batting")
df <- df %>% html_table()
df
df$Pos <- 1:100
df[[1]]
df[[1]]$Pos <- 1:100
df
typeof(df)
#problem 2
df <-   as.tibble(df)
typeof(df)
#PROBLEM 3 
data(starwars)
starwars
head(starwars,6)
s_updated <- starwars %>% drop_na()
classfication <- s_updated %>% arrange(desc(height))
classfication

average <- s_updated %>% group_by(sex) %>% summarize(average_mean = mean(mass), average_height = mean(height))
average

#problem 4
data(mtcars)
mtcars
head(mtcars)


#problem 5 
avr_cyl <- mtcars %>% group_by(cyl) %>% summarize(average_displacement = mean(disp), average_horsepower = mean(hp))
avr_cyl



#PROBLEM 6A
batting <- df[[1]]
filter(batting, Team == "IND")


#PROBLEM 6 B C
n_of_players <- batting %>% group_by(Team) %>% summarise(numberofPlayers = n())
n_of_players

#problem 6 c
avr_ranking <- batting %>% group_by(Team) %>% summarise(average_ranking = mean(Pos))
avr_ranking

#problem 6 d
arrange_avr_rank <- avr_ranking %>% arrange(desc(average_ranking))
arrange_avr_rank


#problem 7 
asia <- function(team)
{
  k <- length(team)
  cont <- numeric(length = k)
  for(i in 1:k)
  {
    if( sum(team[i]  == c("SL", "IND", "PAK", "THA", "BAN") ) > 0)
    {
      cont[i] <- "Asia"
    } else{
      cont[i] <- "Not Asia"
    }
  }
  return(cont)
}
asia_batting <- batting %>% mutate(continent = asia(Team))
by_continent <- asia_batting %>% group_by(continent) 

# (a) how many Asia/Non-Asia players
by_continent %>% summarise(n = n())

# (b) Average Rating
by_continent %>% summarise(Rating = mean(Rating)) %>% arrange(desc(Rating))



