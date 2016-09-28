# informalexample A.1 of section A.1.5 
# (informalexample A.1 of section A.1.5)  : Working with R and other tools : Installing the tools : R resources 

install.packages('ctv',repos='https://cran.r-project.org')
library('ctv')
# install.views('TimeSeries') # can take a LONG time

# example A.1 of section A.2 
# (example A.1 of section A.2)  : Working with R and other tools : Starting with R 
# Title: Trying a few R commands 

1
## [1] 1
1/2
## [1] 0.5
'Joe'
## [1] "Joe"
"Joe"
## [1] "Joe"
"Joe"=='Joe'
## [1] TRUE
c()
## NULL
is.null(c())
## [1] TRUE
is.null(5)
## [1] FALSE
c(1)
## [1] 1
c(1,2)
## [1] 1 2
c("Apple",'Orange')
## [1] "Apple"  "Orange"
length(c(1,2))
## [1] 2
vec <- c(1,2)
vec
## [1] 1 2

# informalexample A.2 of section A.2.1 
# (informalexample A.2 of section A.2.1)  : Working with R and other tools : Starting with R : Primary features of R 

x <- 2
x < - 3
## [1] FALSE
print(x)
## [1] 2

# example A.2 of section A.2.1 
# (example A.2 of section A.2.1)  : Working with R and other tools : Starting with R : Primary features of R 
# Title: Binding values to function arguments 

divide <- function(numerator,denominator) { numerator/denominator }
divide(1,2)
## [1] 0.5
divide(2,1)
## [1] 2
divide(denominator=2,numerator=1)
## [1] 0.5
divide(denominator<-2,numerator<-1)  # yields 2, a wrong answer
## [1] 2

# example A.3 of section A.2.1 
# (example A.3 of section A.2.1)  : Working with R and other tools : Starting with R : Primary features of R 
# Title: Demonstrating side effects 

x<-1
good <- function() { x <- 5}
good()
print(x)
## [1] 1
bad <- function() { x <<- 5}
bad()
print(x)
## [1] 5

# example A.4 of section A.2.1 
# (example A.4 of section A.2.1)  : Working with R and other tools : Starting with R : Primary features of R 
# Title: R truth tables for Boolean operators 

c(T,T,F,F) == c(T,F,T,F)
## [1]  TRUE FALSE FALSE  TRUE
c(T,T,F,F) & c(T,F,T,F)
## [1]  TRUE FALSE FALSE FALSE
c(T,T,F,F) | c(T,F,T,F)
## [1]  TRUE  TRUE  TRUE FALSE

# informalexample A.3 of section A.2.1 
# (informalexample A.3 of section A.2.1)  : Working with R and other tools : Starting with R : Primary features of R 

add <- function(a,b) { a + b}
add(1,2)
## [1] 3

# informalexample A.4 of section A.2.1 
# (informalexample A.4 of section A.2.1)  : Working with R and other tools : Starting with R : Primary features of R 

add(1,'fred')
## Error in a + b : non-numeric argument to binary operator

# example A.5 of section A.2.1 
# (example A.5 of section A.2.1)  : Working with R and other tools : Starting with R : Primary features of R 
# Title: Call-by-value effect 

vec <- c(1,2)
fun <- function(v) { v[[2]]<-5; print(v)}
fun(vec)
## [1] 1 5
print(vec)
## [1] 1 2

# informalexample A.5 of section A.2.2 
# (informalexample A.5 of section A.2.2)  : Working with R and other tools : Starting with R : Primary R data types 

vec <- c(2,3)
vec[[2]] <- 5
print(vec)
## [1] 2 5

# example A.6 of section A.2.2 
# (example A.6 of section A.2.2)  : Working with R and other tools : Starting with R : Primary R data types 
# Title: Examples of R indexing operators 

x <- list('a'=6,b='fred')
names(x)
## [1] "a" "b"
x$a
## [1] 6
x$b
## [1] "fred"
x[['a']]
## $a
## [1] 6

x[c('a','a','b','b')]
## $a
## [1] 6
##
## $a
## [1] 6
##
## $b
## [1] "fred"
##
## $b
## [1] "fred"

# example A.7 of section A.2.2 
# (example A.7 of section A.2.2)  : Working with R and other tools : Starting with R : Primary R data types 
# Title: R’s treatment of unexpected factor levels 

factor('red',levels=c('red','orange'))
## [1] red
## Levels: red orange
factor('apple',levels=c('red','orange'))
## [1] <NA>
## Levels: red orange

# example A.8 of section A.2.2 
# (example A.8 of section A.2.2)  : Working with R and other tools : Starting with R : Primary R data types 
# Title: Confirm lm() encodes new strings correctly. 

d <- data.frame(x=factor(c('a','b','c')),
                   y=c(1,2,3))
m <- lm(y~0+x,data=d) 	# Note: 1 
print(predict(m,
   newdata=data.frame(x='b'))[[1]]) 	# Note: 2 
# [1] 2
print(predict(m,
   newdata=data.frame(x=factor('b',levels=c('b'))))[[1]]) 	# Note: 3 
# [1] 2

# Note 1: 
#   Build a data frame and linear model mapping 
#   a,b,c to 1,2,3. 

# Note 2: 
#   Show that model gets correct prediction for 
#   b as a string. 

# Note 3: 
#   Show that model gets correct prediction for 
#   b as a factor, encoded with a different number of 
#   levels. This shows that lm() is correctly treating 
#   factors as strings. 

# example A.9 of section A.2.3 
# (example A.9 of section A.2.3)  : Working with R and other tools : Starting with R : Loading data from HTTPS sources 
# Title: Loading UCI car data directly from GitHub using HTTPS 

require(RCurl) 	# Note: 1 
urlBase <- 
  'https://raw.githubusercontent.com/WinVector/zmPDSwR/master/' 	# Note: 2 
mkCon <- function(nm) { 	# Note: 3 
   textConnection(getURL(paste(urlBase,nm,sep='/')))
}
cars <- read.table(mkCon('car.data.csv'), 	# Note: 4 
    sep=',',header=T,comment.char='')

# Note 1: 
#   Bring in the RCurl library for more connection 
#   methods. 

# Note 2: 
#   Form a valid HTTPS base URL for raw access to 
#   the GitHub repository. 

# Note 3: 
#   Define a function that wraps a URL path 
#   fragment into a usable HTTPS connection. 

# Note 4: 
#   Load the car data from GitHub over 
#   HTTPS. 

# example A.10 of section A.3.2 
# (example A.10 of section A.3.2)  : Working with R and other tools : Using databases with R : Starting with SQuirreL SQL 
# Title: Reading database data into R 

install.packages('RJDBC',repos='https://cran.r-project.org')  	# Note: 1 
library('RJDBC') 	# Note: 2 
drv <- JDBC("org.h2.Driver","h2-1.3.170.jar",identifier.quote="'") 	# Note: 3 
conn <- dbConnect(drv,"jdbc:h2:h2demodb_h2","u","u") 	# Note: 4 
d <- dbGetQuery(conn,"SELECT * FROM example_table") 	# Note: 5 
print(d)  	# Note: 6 
##   STATUSID NAME
## 1        1  Joe
## 2        2 Fred       	# Note: 7

# Note 1: 
#   Install the RJDBC package from the CRAN 
#   package repository. 

# Note 2: 
#   Load the RJDBC library. 

# Note 3: 
#   Use the RJDBC library to build a database 
#   driver. 

# Note 4: 
#   Use the database driver to build a database 
#   connection. In our SQuirreL SQL example, we used 
#   the path /Users/johnmount/Downloads/h2demodb_h2. 
#   So the path fragment given here (h2demodb_h2) 
#   works only if R is working in the directory 
#   /Users/johnmount/Downloads. You would alter all of 
#   these paths and URLs to work for your own 
#   directories. 

# Note 5: 
#   Run a SQL select query using the database 
#   connection to populate a data frame. 

# Note 6: 
#   Print the result data frame. 

# Note 7: 
#   The database table as an R data frame. 

# example A.11 of section A.3.4 
# (example A.11 of section A.3.4)  : Working with R and other tools : Using databases with R : An example SQL data transformation task 
# Title: Loading an Excel spreadsheet 

library(gdata)
bookings <- read.xls('Workbook1.xlsx',sheet=1,pattern='date',
   stringsAsFactors=F,as.is=T)
prices <- read.xls('Workbook1.xlsx',sheet=2,pattern='date',
   stringsAsFactors=F,as.is=T)

# example A.12 of section A.3.4 
# (example A.12 of section A.3.4)  : Working with R and other tools : Using databases with R : An example SQL data transformation task 
# Title: The hotel reservation and price data 

print(bookings)
##         date day.of.stay X1.before X2.before X3.before
## 1 2013-07-01         105        98        95        96
## 2 2013-07-02         103       100        98        95
## 3 2013-07-03         105        95        90        80
## 4 2013-07-04         105       105       107        98
print(prices)
##         date day.of.stay X1.before X2.before X3.before
## 1 2013-07-01     $250.00   $200.00   $280.00   $300.00
## 2 2013-07-02     $200.00   $250.00   $290.00   $250.00
## 3 2013-07-03     $200.00   $200.00   $250.00   $275.00
## 4 2013-07-04     $250.00   $300.00   $300.00   $200.00

# example A.13 of section A.3.4 
# (example A.13 of section A.3.4)  : Working with R and other tools : Using databases with R : An example SQL data transformation task 
# Title: Using melt to restructure data 

library('reshape2')
bthin <- melt(bookings,id.vars=c('date'),
   variable.name='daysBefore',value.name='bookings') 	# Note: 1 
pthin <- melt(prices,id.vars=c('date'),
   variable.name='daysBefore',value.name='price') 	# Note: 2 
daysCodes <- c('day.of.stay', 'X1.before', 'X2.before', 'X3.before')
bthin$nDaysBefore <- match(bthin$daysBefore,daysCodes)-1 	# Note: 3 
pthin$nDaysBefore <- match(pthin$daysBefore,daysCodes)-1 	# Note: 4 
pthin$price <- as.numeric(gsub('\\$','',pthin$price)) 	# Note: 5 
print(head(pthin))
##         date  daysBefore price nDaysBefore
## 1 2013-07-01 day.of.stay   250           0
## 2 2013-07-02 day.of.stay   200           0
## 3 2013-07-03 day.of.stay   200           0
## 4 2013-07-04 day.of.stay   250           0
## 5 2013-07-01   X1.before   200           1
## 6 2013-07-02   X1.before   250           1

# Note 1: 
#   Use melt to change columns that are not date 
#   (day.of.stay, Xn.before) to values stored in a new 
#   column called daysBefore. Each booking count 
#   becomes a new row (instead of having many 
#   different bookings in the same row). 

# Note 2: 
#   Each price entry becomes a new row (instead 
#   of having many different prices in the same 
#   row). 

# Note 3: 
#   Use match and dayCodes list to convert key 
#   strings to numeric nDaysBefore in our bookings 
#   data. 

# Note 4: 
#   Use match and dayCodes list to convert key 
#   strings to numeric nDaysBefore in our price 
#   data. 

# Note 5: 
#   Remove dollar sign and convert prices to 
#   numeric type. 

# example A.14 of section A.3.4 
# (example A.14 of section A.3.4)  : Working with R and other tools : Using databases with R : An example SQL data transformation task 
# Title: Assembling many rows using SQL 

options(gsubfn.engine = "R") 	# Note: 1 
library('sqldf')
joined <- sqldf(' 	# Note: 2 
  select 	# Note: 3 
     bCurrent.date as StayDate, 	# Note: 4 
     bCurrent.daysBefore as daysBefore,
     bCurrent.nDaysBefore as nDaysBefore,
     p.price as price,
     bCurrent.bookings as bookingsCurrent,
     bPrevious.bookings as bookingsPrevious,
     bCurrent.bookings - bPrevious.bookings as pickup
  from
     bthin bCurrent 	# Note: 5 
  join
     bthin bPrevious 	# Note: 6 
  on
     bCurrent.date=bPrevious.date
     and bCurrent.nDaysBefore+1=bPrevious.nDaysBefore 	# Note: 7 
  join
     pthin p 	# Note: 8 
  on
     bCurrent.date=p.date
     and bCurrent.nDaysBefore=p.nDaysBefore 	# Note: 9 
')
print(joined)

# Note 1: 
#   Prevent library(sqldf) from triggering a 
#   tcl/tk dependency which causes R to exit on OS X 
#   if X11 isn’t installed. See 
#   https://code.google.com/p/sqldf/ for 
#   troubleshooting details. 

# Note 2: 
#   Create a new data frame of rows built out of 
#   triples of rows from pthin and bthin. 

# Note 3: 
#   SQL statements typically start with the word 
#   “select.” 

# Note 4: 
#   List of derived columns (and their new 
#   names) for our new data frame. 

# Note 5: 
#   First data frame we’re pulling data from: 
#   bthin. 

# Note 6: 
#   Second pull from bthin. 

# Note 7: 
#   Conditions to match b1 rows to b2 
#   rows. 

# Note 8: 
#   Third data frame we are pulling data from: 
#   pthin. 

# Note 9: 
#   Conditions to match p to b2 (and implicitly 
#   b1). 

# example A.15 of section A.3.4 
# (example A.15 of section A.3.4)  : Working with R and other tools : Using databases with R : An example SQL data transformation task 
# Title: Showing our hotel model results 

library('ggplot2')
ggplot(data=joined,aes(x=price,y=pickup)) +
  geom_point() + geom_jitter() + geom_smooth(method='lm')
print(summary(lm(pickup~price,data=joined)))
#
#Call:
#lm(formula = pickup ~ price, data = joined)
#
#Residuals:
#   Min     1Q Median     3Q    Max
#-4.614 -2.812 -1.213  3.387  6.386
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)
#(Intercept) 11.00765    7.98736   1.378    0.198
#price       -0.02798    0.03190  -0.877    0.401
#
#Residual standard error: 4.21 on 10 degrees of freedom
#Multiple R-squared:  0.07144,  Adjusted R-squared:  -0.02142
#F-statistic: 0.7693 on 1 and 10 DF,  p-value: 0.401

