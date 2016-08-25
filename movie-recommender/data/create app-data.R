## This script builds the environment that the app loads in global.R
## Given the length of time it takes to process the raw data, I determined
## it would be a good idea to preprocess the data and then load it into the 
## environment.

library(dplyr)
library(tidyr)
library(reshape)
library(reshape2)

## Read in the MovieLens data (downloaded from http://grouplens.org/datasets/movielens/)
data <- read.table('data/ratings.dat', header=FALSE, sep=":", stringsAsFactors=FALSE)
names(data) <- c('user_id', 'drop1', 'movie_id', 'drop2', 'rating' , 'drop3', 'timestamp')
data <- select(data, user_id, movie_id, rating)

genres <- read.csv('data/u.genre', header=FALSE, sep='|', stringsAsFactors=FALSE)
genres <- genres[2:19,1] # Removing Unkown

movies <- read.csv('data/u.item', header=FALSE, stringsAsFactors=FALSE, sep='|')
names(movies) <- c('id', 'title', 'release_date', 'video_release_date', 'IMDb_url', 'unknown', genres)
movies$title <- enc2utf8(movies$title)
## There are duplicate records in the movies data frame so we need to correct it
dup.titles <- movies %>%
  group_by(title) %>%
  summarise (count = n()) %>%
  filter(count > 1) %>%
  select(title)
dup.titles <- dup.titles$title

corrections <- movies %>%
  filter(title %in% dup.titles) %>%
  arrange(title) %>%
  group_by(title) %>%
  mutate(movie_id = id) %>%
  mutate(new.id = min(id)) %>%
  select(movie_id, new.id) %>%
  filter(!movie_id == new.id)
corrections <- corrections[,2:3]

dup.ids <- corrections$movie_id

## Remove the duplicate movies and the unknown title
movies <- movies %>%
  filter(!id %in% dup.ids) %>%
  filter(title != 'unknown')

## Correct the movie_id
data <- merge(data, corrections, all.x=TRUE)
data[!is.na(data$new.id),]$movie_id <- data[!is.na(data$new.id),]$new.id
data <- data %>%
  filter(movie_id %in% movies$id) %>%
  select(movie_id, user_id, rating) %>%
  unique()

## We only want movies with 400+ reviews to be included in the drop down
enough.reviews <- data %>%
  group_by(movie_id) %>%
  summarise(count = n()) %>%
  filter(count >= 400)

## We only want to prompt the user to rate films we have all ratings for (no NA's)
all.ratings <- data %>%
  select(movie_id, rating) %>%
  group_by(movie_id, rating) %>%
  summarise(value=n()) %>%
  mutate(rating=paste0('n.',rating,'.star')) %>%
  spread(rating, value)
all.ratings <- all.ratings[!is.na(all.ratings$n.1.star),]
all.ratings <- all.ratings[!is.na(all.ratings$n.2.star),]
all.ratings <- all.ratings[!is.na(all.ratings$n.3.star),]
all.ratings <- all.ratings[!is.na(all.ratings$n.4.star),]
all.ratings <- all.ratings[!is.na(all.ratings$n.5.star),]

## Build the dropdown list
review.dropdown <- movies %>%
  filter(id %in% all.ratings$movie_id)%>%
  filter(id %in% enough.reviews$movie_id)%>%
  select(id, title) %>%
  arrange(title)

dropdown <- review.dropdown$id
names(dropdown) <- review.dropdown$title

## We don't want movies with a few good reviews to trump more robust
## agerage reviews.
enough.reviews <- data %>%
  group_by(movie_id) %>%
  summarise(count = n()) %>%
  filter(count >= 100)

data <- data %>%
  filter(movie_id %in% all.ratings$movie_id)%>%
  filter(movie_id %in% enough.reviews$movie_id)

movies <- movies %>%
  filter(id %in% all.ratings$movie_id)%>%
  filter(id %in% enough.reviews$movie_id)

## Clean up R Environment
rm(corrections, dup.ids, dup.titles, enough.reviews, all.ratings, review.dropdown)

## Save Global Environment
save.image('data/app-data.RData')