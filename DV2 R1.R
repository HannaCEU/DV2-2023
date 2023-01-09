source('http://bit.ly/CEU-R-shoes')
ls()
str(students)
plot(students$shoe, students$math)
abline(lm(math~shoe, students), col = 'red')

library(GGally)

plot(students)

ggpairs(students)

rm(list=ls())
?ls

ls(all.names = TRUE)

## visualisation for distance cities in Hungary

download.file('https://bit.ly/hun-cities-distance', 'cities.xls')

library(readxl)

cities <- read_excel('cities.xls')

##get rid of the last row and the first column

cities <- cities [, -1]
cities <- cities [-nrow(cities), ]

plot(cities)

library(GGally)
ggpairs(cities)

#building function in R
mds <- cmdscale(as.dist(cities))
mds
plot(mds)

text(mds[, 1], mds[, 2], names (cities))

mds[, 1] <- -1 * mds [, 1]
plot(mds)
text(mds[, 1], mds[, 2], names(cities))

## to do redo with ggplot
library(ggplot2)
str(mds)
mds <- data.frame(mds)
str(mds)
mds$city <- names(cities)
str(mds)
ggplot(mds, aes (X1, X2, label = city)) +
  geom_text()

library(ggrepel)
ggplot(mds, aes(X1, X2, label = city)) + geom_text_repel()

# use inbuilt dataset ?eurodist
mds1 <- cmdscale(eurodist)
mds1
mds1 <- data.frame(mds1)

mds1$city <- row.names(mds1)

ggplot(mds1, aes (X1, -X2, label = city)) +
  geom_text_repel()


## inbuilt mtcars 
?mtcars

mds2 <- cmdscale(dist(scale(mtcars)))
mds2
mds2 <- data.frame(mds2)
mds2$car <- row.names(mtcars)


ggplot(mds2, aes (X1, X2, label = car)) +
  geom_text_repel()


scale(mtcars)
#std should be 1

UCBAdmissions

as.data.frame(UCBAdmissions)

berkley <- as.data.frame(UCBAdmissions)

ggplot(berkley, aes(Gender, Freq, fill = Admit)) + 
  facet_wrap(~Dept)+
  geom_col(position = 'fill') + 
  scale_fill_manual(values = c ("Admitted" = 'darkgreen',
                              "Rejected" = 'darkred'))  

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species))  +
  scale_fill_manual(values = c("setosa" = "green",
                               "versicolor" = "red",
                               "virginica" = 'blue')) +
  geom_point(aes(color = Species)) +
  geom_smooth(method = 'lm', color = 'black') +
  geom_smooth(aes(color = Species), method = 'lm')+
  theme_minimal()

library(data.table)

bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')

bookings[1:5]

bookings[price < 100 & holiday == 1]

bookings [price < 100] [holiday == 1] [1:5]

bookings[price < 100 & holiday == 1, .N]

bookings[price < 100 & holiday == 1, mean(price)]

bookings[price < 100 & holiday == 1, hist(price)]

bookings[price < 100 & holiday == 1, summary(price)]

#compute average price on bookings and on weekdays

bookings[weekend == 1, mean(price)]

# calc average price of bookings on weekdays
bookings[weekend == 0, mean(price)]

bookings[ ,mean(price), by = weekend]

## bookings$price_per_night <- bookings$price/bookings$nnights
bookings[, price_per_night := price / nnights]


## list can be replaced with a .
bookings[, .(price = mean(price), min = min(price), max = max(price)), by = .(weekend, nnights, holiday)]

features <- fread('http://bit.ly/CEU-R-hotels-2018-features')

## inner join, observations are missing
merge(bookings, features, all.x = TRUE)[is.na(city)]

# to do country level aggregated data on average rating of hotels

countries <- features[, .(rating = mean(rating, na.rm = TRUE)), by = country][!is.na(country)]

setorder(countries, rating)
countries


countries[order(country)]
countries[order(rating)]

library(ggmap)


#nominatim 

library(tidygeocoder)
#can provide dataframe instead of character vector

geocode(countries, 'country')

countries <- data.table(tidygeocoder::geocode(countries, 'country'))

library(maps)
map('world', fill = TRUE, col = 1:10)

#create world variable 
world <- map_data ('world')
str(world)

map <- ggplot()+
  geom_map(data=world, map = world, aes(long, lat, map_id = region)) +
  theme_void()+
  coord_fixed(1.3)


map + geom_point(data = countries, aes(long, lat, size = rating), color = 'orange')

?get_stamenmap


bbox = c(left = min(countries$long), bottom = min(countries$lat),
         right = max(countries$long), top = max(countries$lat))
get_stamenmap(bbox = bbox, zoom = 4) %>% ggmap() +
  geom_point(aes(x = long, y = lat), data = countries, colour = "red", size = 2)+
  theme_void()+
  coord_fixed(1.3)

anscombe

plot(anscombe[, c(1, 5)])
plot(anscombe[, c(2, 6)])
plot(anscombe[, c(3, 7)])
plot(anscombe[, c(4, 8)])

cor(anscombe[, c(1,5)])
cor(anscombe[, c(2,6)])

mean(anscombe[, c(1)])
mean(anscombe[, c(2)])
mean(anscombe[, c(3)])
mean(anscombe[, c(4)])

#hidden forloop. Return a list for us
#anonymous version function
lapply(1:4, function(i) mean(anscombe[, c(i)]))

#alternative function 
computemean <- function (i){
  mean(anscombe[, c(i)])
}

lapply(1:4, computemean)

anscombe_df <- rbindlist(lapply(1:4, function(i) {
  data.frame(
    x = anscombe[, c(i)],
    y = anscombe[, c(i+4)],
    dataset = i)
}
))

## data.frame with 4x11 rows, 3 columns: x, y, dataset id
ggplot(anscombe_df, aes(x, y)) + geom_point() + facet_wrap(~dataset)

df <-datasauRus::datasaurus_dozen_wide

dino_df <- rbindlist(lapply(seq(1, 26, by = 2), function(i) {
  data.frame(
    x = df[, c(i), drop = TRUE], 
    y = df[, c(i+1), drop = TRUE],
    # sub('_x$', '', names(df)[i])
    dataset = substr(names(df)[i], 1, nchar(names(df)[i])-2))
}
))

ggplot(dino_df, aes(x, y)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_wrap(~dataset) +
  theme_bw()













