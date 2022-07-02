setwd('/Users/lcspk/Desktop/Final_Project_3010')
df <- read.csv('spotify_top50_2021.csv')
library('ggplot2')
library('tidyverse')
library('dplyr')
library('rlang')


summary(df)

hist(df$popularity,breaks= 14)

hist(df$speechiness,breaks=14)

ggplot(data = df) +
  geom_smooth(mapping = aes(x = popularity, y = liveness))

ggplot(data = df) +
  geom_smooth(mapping = aes(x = popularity, y = danceability))

ggplot(data = df) +
  geom_smooth(mapping = aes(x = popularity, y = loudness))


ggplot(data = df) +
  geom_smooth(mapping = aes(x = popularity, y = energy))

ggplot(data = df) +
  geom_smooth(mapping = aes(x = popularity, y = speechiness))

ggplot(data = df) +
  geom_smooth(mapping = aes(x = popularity, y = tempo))

ggplot(data = df) +
  geom_smooth(mapping = aes(x = popularity, y = duration_ms))


ggplot(data = df) +
  geom_smooth(mapping = aes(x = popularity, y = valence))

ggplot(data = df) +
  geom_smooth(mapping = aes(x = popularity, y = key))



ggplot(data = df) +
  geom_smooth(mapping = aes(x = popularity, y = acousticness))




min(df$popularity)

least_popular<-df
least_popular<-select(df,popularity,track_name)
filter(least_popular,popularity==47)


max(df$popularity)

most_popular<-df
most_popular<-select(df,popularity,artist_name)
filter(most_popular,popularity==95)







ggplot(data, aes(popularity, danceability)) +                                    # Decreasingly ordered barchart
  geom_bar(stat = "identity")



pairs(~popularity+danceability+tempo+key,data=df,
      main="Scatterplot Matrix", cex= .65)






model <- lm(popularity~loudness+energy+danceability+liveness, 
            data = df)
summary(model)



length(unique(df$artist_name))
