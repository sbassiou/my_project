
#Inputting gapminder dataset
gapminder <- read.csv("data/gapminder-FiveYearData.csv")

#Determining variable type for 'country'
typeof(gapminder$country)

#Determining names of columns in gapminder
colnames(gapminder)

#Familiarizing self with gapminder
str(gapminder)


#Creating plot, part 1.
library("ggplot2")

#Creating plot, part 2.
ggplot(data = gapminder, aes(x =year, y =lifeExp, color=continent)) +
  geom_point() + geom_smooth(method="lm") +
  xlab("Year") + ylab("Life expectancy") + ggtitle("Figure 1. Life expectancy from 1952 to 2007") +
  scale_colour_discrete(name="Continent") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

