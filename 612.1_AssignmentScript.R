gapminder <- read.csv("data/gapminder-FiveYearData.csv")
str(gapminder)
typeof(gapminder$year)
head(gapminder)

#Question to ask: 
#How does th the U.S. population change 
#over the past 50 years as compared with 
#China's population and Brazil's population?

pop <- function(dat, year=NULL, country=NULL) {
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

#Creating variable popUS only containing US population
popUS <- pop(gapminder, country="United States")

#Verifiying variable creation
popUS

#Creating variable popBR containing Brazil population
popBR <- pop(gapminder, country="Brazil")

#Verifiying variable creation
popBR

#Creating plot
ggplot(data = gapminder, 
       aes(x = year, y = country, color=continent)) + geom_point()


ggplot(data = gapminder, aes(x = year, y = country, color=continent)) +
  geom_line() + facet_wrap( ~ country) +
  xlab("Year") + ylab("Country") + ggtitle("Figure 1") +
  scale_fill_discrete(name="Continent") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())


#test
library("ggplot2")
ggplot(data = gapminder, aes(x = lifeExp, y = gdpPercap)) +
  geom_point(
    
#To output vector:
daply(
  .data = calclifeExp(gapminder),
  .variables = "continent",
  .fun = function(x) mean(x$lifeExp)
)

#U.S. population 1952-2007 
country <- daply(
  .data = gapminder[gapminder$country=="United States"], 
  .variables ="country",
  .fun = function(x) mean(x$lifeExp)
)