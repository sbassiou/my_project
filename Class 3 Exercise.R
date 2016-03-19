vectormultiplysum <-function (a,b=2) {
  the_sum2 <- a*b
  the_realsum <- sum(a,b)
  return(the_sum)
}
> vectormultiplysum(practice)

numbersfunction <- function(numbers) {
  numbers_data <- number*2
  (sum(numbers_data))
  
}

numbersfunction <- function(numbers) {
  numbers_data <- number*2
  (sum(numbers_data))
  
}

my_sum <- function(a, b) {
  the_sum <- a + b
  return(the_sum)
}
fahr_to_kelvin <- function(temp) {
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}
#example 1
fahr_to_kelvin(32)
fahr_to_kelvin(102)

#example 2
x <- rpois(1, lambda=8)
if (x <= 10) {
  print("x is greater than or equal to 10")
}

#example 2.2
if (x <= 10) {
  print("x is greater than or equal to 10")
} else if (x > 5) {
  print("x is greater than 5")
} else {
  print("x is less than 5")
}

#example 2.3
x  <-  4 == 3
if (x) {
  "4 equals 3"
} else {
  print ("This is a lie")
}

#Installing the plyr package
install.packages("plyr")

#Split-apply data
# Takes a dataset and multiplies the population column
# with the GDP per capita column.
calcGDP <- function(dat, year=NULL, country=NULL) {
  if(!is.null(year)) {
    dat <- dat[dat$year %in% year, ]
  }
  if (!is.null(country)) {
    dat <- dat[dat$country %in% country,]
  }
  gdp <- dat$pop * dat$gdpPercap
  
  new <- cbind(dat, gdp=gdp)
  return(new)
}

mean_gdp <- ddply(
  .data = calcGDP(gapminder),
  .variables = "continent",
  .fun = function(x) mean(x$gdp)
)
#Read out just Asia
mean_gdp["Asia",]

#
dlply(
  .data = calcGDP(gapminder),
  .variables = "continent",
  .fun = function(x) mean(x$gdp)
)

daply(
  .data = calcGDP(gapminder),
  .variables = c("continent", "year"),
  .fun = function(x) mean(x$gdp)
)
#We called the same function again
#but changed the second letter to an l, 
#so the output was returned as a list.
#We can specify multiple columns to group by:
d_ply(
  .data=gapminder,
  .variables = "continent",
  .fun = function(x) {
    meanGDPperCap <- mean(x$gdpPercap)
    print(paste(
      "The mean GDP per capita for", unique(x$continent),
      "is", format(meanGDPperCap, big.mark=",")
    ))
  }
)

#Challenge 1. Calculate the average life expectancy per continent. 
#We have a data frame and we would like a data frame to come out.
#Which has the longest? Which had the shortest?
#Answers: Africa has the shortest; Oceania had the longest.
ddply(
  .data = calclifeExp(gapminder),
  .variables = "continent",
  .fun = function(x) mean(x$lifeExp)
)

#Alternative method:
#input is data frame, output is dataframe
#data is gapminder
#variable want to split on is continent
#for each split of data, want to get average life expectancy
ddply(
  .data=gapminder,
  .variable="continent",
  .fun=function(x) mean(x$lifeExp)
)

#Can also show as an array
#To output a vector
daply(
  .data = calclifeExp(gapminder),
  .variables = "continent",
  .fun = function(x) mean(x$lifeExp)
)
#Alternative method:
daply(
  .data=gapminder,
  .variable="continent",
  .fun=function(x) mean(x$lifeExp)
)


#Challenge 2. Calculate the average life expectancy per continent and year. 
#Pat's thought process: make then dataframe for 1952,
#     a now dataframe for 2007,
#     will get an arrray with each of the continents for 1952 and 2007
#     then compare them to each other.
#     So this will be two vectors

#Average life expectancy per continent in 1952. 
then <- daply(
  .data = gapminder[gapminder$year==1952,], 
  .variables ="continent",
  .fun = function(x) mean(x$lifeExp)
)

#Average life expectancy per continent in 2007. 
now <- daply(
  .data = gapminder[gapminder$year==2007,], 
  .variables ="continent",
  .fun = function(x) mean(x$lifeExp)
)

#Now subtract to get the difference in life expectancy
now-then

#Which had the longest and shortest life expectancy in 2007? 
#Answer: longest=Oceania; shortest=Africa
daply(
  .data = calcGDP(gapminder),
  .variables = c("continent", "2007"),
  .fun = function(x) mean(x$lifeExp) 
)
#Which had the greatest change in life expectancy between 1952 and 2007?
daply(
  .data = calcGDP(gapminder),
  .variables = c("continent", "year"),
  .fun = function(x) max(x$lifeExp)
)

#Trying to return the mean and standard deviation
now2 <- daply(
  .data = gapminder[gapminder$year==2007,], 
  .variables ="continent",
  .fun = function(x) {c(avg=mean(x$lifeExp), sd=sd(x$lifeEx))})
now2

#Installing GGPlot2 package
install.packages("ggplot2")
library(ggplot2)

#Three components of GGPlot
#1. dataset; 2.coordinate system ; 3.geoms
ggplot(data = gapminder, aes(x = lifeExp, y = gdpPercap)) + geom_point()

#Challenge 1. To plot life expectancy vs. time
ggplot(data = gapminder, aes(x = year, y = lifeExp)) + geom_point()

#Challenge 2. Modify plot to link color to the continent.
ggplot(data = gapminder, 
       aes(x = year, y = lifeExp, color=continent)) + geom_point()

ggplot(data = gapminder, 
       aes(x = year, y = lifeExp, color=continent, size=gdpPercap)) 
+ geom_point()

#Challenge 3. Modify plot from points to lines
ggplot(data = gapminder,
       aes(x = year, y = lifeExp, by=country, color=continent)) 
+ geom_line()

#Want to put points on top of the line? (As if it weren't busy enoug)
ggplot(data = gapminder, 
       aes(x = year, y = lifeExp, by=country, color=continent)) 
+ geom_line() + geom_point()


ggplot(data = gapminder, aes(x=year, y=lifeExp, by=country)) +
  geom_line(aes(color=continent)) + geom_point()
  geom_point()

#Using varied geometric points
ggplot(data = gapminder, 
       aes(x = lifeExp, y = gdpPercap, color=continent))+ geom_point()

ggplot(data = gapminder, aes(x = lifeExp, y = gdpPercap)) +
  geom_point() + scale_y_log10()

ggplot(data = gapminder, aes(x = lifeExp, y = gdpPercap, color=continent)) +
  geom_point() + scale_y_log10() + geom_smooth(method="lm")

ggplot(data = gapminder, aes(x = lifeExp, y = gdpPercap, color=continent)) +
  geom_point() + scale_y_log10() + geom_smooth(method="lm", size=0.5)

ggplot(data = gapminder, aes(x = year, y = lifeExp, color=continent)) +
  geom_line() + facet_wrap( ~ country)

#Note: to call names of columns: use head, summary, call names

ggplot(data = gapminder, aes(x = year, y = lifeExp, color=continent)) +
  geom_line() + facet_wrap( ~ country) +
  xlab("Year") + ylab("Life expectancy") + ggtitle("Figure 1") +
  scale_fill_discrete(name="Continent") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())


