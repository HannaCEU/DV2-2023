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





