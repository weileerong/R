#######################################################
#base
#######################################################
#load data
data <- read.csv("input.csv")
head(data)

#subset
retval <- subset(data, salary == max(salary))
print(retval)

#output data
write.csv(retval,"output.csv", row.names = FALSE)

#input dataframe
BMI <- 	data.frame(
  gender = c("Male", "Male","Female"), 
  height = c(152, 171.5, 165), 
  weight = c(81,93, 78),
  Age = c(42,38,26)
)
print(BMI)

#merge dataframe
library(MASS)
merged.Pima <- merge(x = Pima.te, y = Pima.tr,
                     by.x = c("bp", "bmi"),
                     by.y = c("bp", "bmi"),all.x=TRUE
)
print(merged.Pima)
nrow(merged.Pima)

##################################################
#if statement
x <- 30L
if(is.integer(x)) {
  print("X is an Integer")
}
if("Truth" %in% x) {
  print("Truth is found")
} else {
  print("Truth is not found")
}
if("Truth" %in% x) {
  print("Truth is found the first time")
} else if ("truth" %in% x) {
  print("truth is found the second time")
} else {
  print("No truth found")
}

#loops
v <- c("Hello","loop")
cnt <- 2
repeat {
  print(v)
  cnt <- cnt+1
  if(cnt > 5) {
    break
  }
}
while (cnt < 7) {
  print(v)
  cnt = cnt + 1
}
v <- LETTERS[1:4]
for ( i in v) {
  print(i)
}

#functions
print(getwd())
setwd("/Shiny")

##############################
#string and text manipulation
a <- "Hello"
b <- 'How'
c <- "are you? "
paste(a,b,c, sep = " ", collapse = NULL)
result <- toupper("Changing To Upper")
print(result)
result <- substring("Extract", 5, 7)
print(result)

############################
#datatype
#list
print(seq(5, 9, by = 0.4))
v <- c("Red","Blue","yellow","violet")
sort.result <- sort(v)
print(sort.result)
list_data <- list("Red", "Green", c(21,32,11), TRUE, 51.23, 119.1)
print(list_data)
list_data <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2),
                  list("green",12.3))
names(list_data) <- c("1st Quarter", "A_Matrix", "A Inner list")
print(list_data)
#matrix
M <- matrix(c(3:14), nrow = 4, byrow = TRUE)
print(M)
rownames = c("row1", "row2", "row3", "row4")
colnames = c("col1", "col2", "col3")
P <- matrix(c(3:14), nrow = 4, byrow = TRUE, dimnames = list(rownames, colnames))
print(P)
#date
start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                       "2015-03-27"))

#############################
#date reshaping
city <- c("Tampa","Seattle","Hartford","Denver")
state <- c("FL","WA","CT","CO")
zipcode <- c(33602,98104,06161,80294)
addresses <- cbind(city,state,zipcode)
addresses
########################################################
#dplyr
#######################################################
library(nycflights13)
library(dplyr)
dim(flights)
flights

#filter() to select cases based on their values.
#arrange() to reorder the cases.
#select() and rename() to select variables based on their names.
#mutate() and transmute() to add new variables that are functions of existing variables.
#summarise() to condense multiple values to a single value.
#sample_n() and sample_frac() to take random samples.

#select
filter(flights, month == 1, day == 1)
#base R
flights[flights$month == 1 & flights$day == 1, ]

#reorder
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))

#select columns
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

#rename columns
rename(flights, tail_num = tailnum)

#add new column
mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
)
#only keep new variables
transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60)
)

#summarise
summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE)
)

#randomly take 10 rows
sample_n(flights, 10)
#take 1% rows
sample_frac(flights, 0.01)
#Use replace = TRUE to perform a bootstrap sample.

#data aggregate
destinations <- group_by(flights, dest)
summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n()
)
by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

##############################3
#piping
data <- flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)

