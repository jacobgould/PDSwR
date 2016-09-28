# example 2.1 of section 2.1.1 
# (example 2.1 of section 2.1.1)  : Loading data into R : Working with data from files : Working with well-structured data from files or URLs 
# Title: Reading the UCI car data 

uciCar <- read.table(  	# Note: 1 
   'http://www.win-vector.com/dfiles/car.data.csv', 	# Note: 2 
   sep=',', 	# Note: 3 
   header=T 	# Note: 4 
   )

# Note 1: 
#   Command to read from a file or URL and store the result in a new data frame object 
#   called 
#   uciCar. 

# Note 2: 
#   Filename or URL to get the data from. 

# Note 3: 
#   Specify the column or field separator as a 
#   comma. 

# Note 4: 
#   Tell R to expect a header line that defines 
#   the data column names. 

# example 2.2 of section 2.1.1 
# (example 2.2 of section 2.1.1)  : Loading data into R : Working with data from files : Working with well-structured data from files or URLs 
# Title: Exploring the car data 

class(uciCar)
## [1] "data.frame" 	# Note: 1 
summary(uciCar)
##    buying      maint       doors
##  high :432   high :432   2    :432
##  low  :432   low  :432   3    :432
##  med  :432   med  :432   4    :432
##  vhigh:432   vhigh:432   5more:432
##
##  persons     lug_boot    safety
##  2   :576   big  :576   high:576
##  4   :576   med  :576   low :576
##  more:576   small:576   med :576
##
##    rating
##  acc  : 384
##  good :  69
##  unacc:1210
##  vgood:  65

dim(uciCar)
## [1] 1728    7   	# Note: 2

# Note 1: 
#   The loaded object uciCar is of type data.frame. 

# Note 2: 
#   The [1] is just an output sequence 
#   marker. The actual information is this: uciCar has 
#   1728 rows and 7 columns. Always try to confirm you 
#   got a good parse by at least checking that the 
#   number of rows is exactly one fewer than the 
#   number of lines of text in the original file. The 
#   difference of one is because the column header 
#   counts as a line, but not as a data row. 

# example 2.3 of section 2.1.2 
# (example 2.3 of section 2.1.2)  : Loading data into R : Working with data from files : Using R on less-structured data 
# Title: Loading the credit dataset 

d <- read.table(paste('http://archive.ics.uci.edu/ml/',
   'machine-learning-databases/statlog/german/german.data',sep=''),
   stringsAsFactors=F,header=F)
print(d[1:3,])

# example 2.4 of section 2.1.2 
# (example 2.4 of section 2.1.2)  : Loading data into R : Working with data from files : Using R on less-structured data 
# Title: Setting column names 

colnames(d) <- c('Status.of.existing.checking.account',
   'Duration.in.month',  'Credit.history', 'Purpose',
   'Credit.amount', 'Savings account/bonds',
   'Present.employment.since',
   'Installment.rate.in.percentage.of.disposable.income',
   'Personal.status.and.sex', 'Other.debtors/guarantors',
   'Present.residence.since', 'Property', 'Age.in.years',
   'Other.installment.plans', 'Housing',
   'Number.of.existing.credits.at.this.bank', 'Job',
   'Number.of.people.being.liable.to.provide.maintenance.for',
   'Telephone', 'foreign.worker', 'Good.Loan')
d$Good.Loan <- as.factor(ifelse(d$Good.Loan==1,'GoodLoan','BadLoan'))
print(d[1:3,])

# example 2.5 of section 2.1.2 
# (example 2.5 of section 2.1.2)  : Loading data into R : Working with data from files : Using R on less-structured data 
# Title: Building a map to interpret loan use codes 

mapping <- list(
   'A40'='car (new)',
   'A41'='car (used)',
   'A42'='furniture/equipment',
   'A43'='radio/television',
   'A44'='domestic appliances',
   ...
  )

# example 2.6 of section 2.1.2 
# (example 2.6 of section 2.1.2)  : Loading data into R : Working with data from files : Using R on less-structured data 
# Title: Transforming the car data 

for(i in 1:(dim(d))[2]) {             	# Note: 1 
   if(class(d[,i])=='character') {
      d[,i] <- as.factor(as.character(mapping[d[,i]]))  	# Note: 2 
   }
}

# Note 1: 
#   (dim(d))[2] is the number of columns 
#   in the data frame d. 

# Note 2: 
#   Note that the indexing operator [] is vectorized. Each step in the for loop remaps an 
#   entire column of data through our list. 

# example 2.7 of section 2.1.2 
# (example 2.7 of section 2.1.2)  : Loading data into R : Working with data from files : Using R on less-structured data 
# Title: Summary of Good.Loan and Purpose 

table(d$Purpose,d$Good.Loan) 
                     
##                       BadLoan GoodLoan
##   business                 34       63
##   car (new)                89      145
##   car (used)               17       86
##   domestic appliances       4        8
##   education                22       28
##   furniture/equipment      58      123
##   others                    5        7
##   radio/television         62      218
##   repairs                   8       14
##   retraining                1        8

# example 2.11 of section 2.2.2 
# (example 2.11 of section 2.2.2)  : Loading data into R : Working with relational databases : Loading data from a database into R 
# Title: Loading data into R from a relational database 

options( java.parameters = "-Xmx2g" )  	# Note: 1 
library(RJDBC)
drv <- JDBC("org.h2.Driver", 	# Note: 2 
   "h2-1.3.176.jar", 	# Note: 3 
   identifier.quote="'") 	# Note: 4 
options<-";LOG=0;CACHE_SIZE=65536;LOCK_MODE=0;UNDO_LOG=0"
conn <- dbConnect(drv,paste("jdbc:h2:./H2DB",options,sep=''),"u","u")
dhus <- dbGetQuery(conn,"SELECT * FROM hus WHERE ORIGRANDGROUP<=1") 	# Note: 5 
dpus <- dbGetQuery(conn,"SELECT pus.* FROM pus WHERE pus.SERIALNO IN \
   (SELECT DISTINCT hus.SERIALNO FROM hus \
   WHERE hus.ORIGRANDGROUP<=1)") 	# Note: 6 
dbDisconnect(conn) 	# Note: 7 
save(dhus,dpus,file='phsample.RData') 	# Note: 8

# Note 1: 
#   Set Java option for extra memory before DB 
#   drivers are loaded. 

# Note 2: 
#   Specify the name of the database driver, same 
#   as in our XML database configuration. 

# Note 3: 
#   Specify where to find the implementation of 
#   the database driver. 

# Note 4: 
#   SQL column names with mixed-case 
#   capitalization, special characters, or that 
#   collide with reserved words must be quoted. We 
#   specify single-quote as the quote we’ll use when 
#   quoting column names, which may different than the 
#   quote we use for SQL literals. 

# Note 5: 
#   Create a data frame called dhus from * 
#   (everything) from the database table hus, taking 
#   only rows where ORGINRANGGROUP <= 1. The 
#   ORGINRANDGROUP column is a random integer from 0 
#   through 999 that SQL Screwdriver adds to the rows 
#   during data load to facilitate sampling. In this 
#   case, we’re taking 2/1000 of the data rows to get 
#   a small sample. 

# Note 6: 
#   Create a data frame called dpus from the 
#   database table pus, taking only records that have 
#   a household ID in the set of household IDs we 
#   selected from households table hus. 

# Note 7: 
#   Disconnect for the database. 

# Note 8: 
#   Save the two data frames into a file named 
#   phsample.RData, which can be read in with load(). 
#   Try help("save") or help("load") for more 
#   details. 

# example 2.12 of section 2.2.3 
# (example 2.12 of section 2.2.3)  : Loading data into R : Working with relational databases : Working with the PUMS data 
# Title: Selecting a subset of the Census data 

load('phsample.RData')
psub = subset(dpus,with(dpus,(PINCP>1000)&(ESR==1)&
   (PINCP<=250000)&(PERNP>1000)&(PERNP<=250000)&
   (WKHP>=40)&(AGEP>=20)&(AGEP<=50)&
   (PWGTP1>0)&(COW %in% (1:7))&(SCHL %in% (1:24)))) 	# Note: 1

# Note 1: 
#   Subset of data rows matching detailed 
#   employment conditions 

# example 2.13 of section 2.2.3 
# (example 2.13 of section 2.2.3)  : Loading data into R : Working with relational databases : Working with the PUMS data 
# Title: Recoding variables 

psub$SEX = as.factor(ifelse(psub$SEX==1,'M','F')) 	# Note: 1 
psub$SEX = relevel(psub$SEX,'M') 	# Note: 2 
cowmap <- c("Employee of a private for-profit",
   "Private not-for-profit employee",
   "Local government employee",
   "State government employee",
   "Federal government employee",
   "Self-employed not incorporated",
   "Self-employed incorporated")
psub$COW = as.factor(cowmap[psub$COW]) 	# Note: 3 
psub$COW = relevel(psub$COW,cowmap[1])
schlmap = c(  	# Note: 4 
   rep("no high school diploma",15),
  "Regular high school diploma",
  "GED or alternative credential",
  "some college credit, no degree",
  "some college credit, no degree",
  "Associate's degree",
  "Bachelor's degree",
  "Master's degree",
  "Professional degree",
  "Doctorate degree")
psub$SCHL = as.factor(schlmap[psub$SCHL])
psub$SCHL = relevel(psub$SCHL,schlmap[1])
dtrain = subset(psub,ORIGRANDGROUP >= 500)  	# Note: 5 
dtest = subset(psub,ORIGRANDGROUP < 500)    	# Note: 6

# Note 1: 
#   Reencode sex from 1/2 to M/F. 

# Note 2: 
#   Make the reference sex M, so F encodes a 
#   difference from M in models. 

# Note 3: 
#   Reencode class of worker info into a more 
#   readable form. 

# Note 4: 
#   Reencode education info into a more readable 
#   form and fewer levels (merge all levels below high 
#   school into same encoding). 

# Note 5: 
#   Subset of data rows used for model 
#   training. 

# Note 6: 
#   Subset of data rows used for model 
#   testing. 

# example 2.14 of section 2.2.3 
# (example 2.14 of section 2.2.3)  : Loading data into R : Working with relational databases : Working with the PUMS data 
# Title: Summarizing the classifications of work 

summary(dtrain$COW)
## Employee of a private for-profit      Federal government employee
##                              423                               21
##        Local government employee  Private not-for-profit employee
##                               39                               55
##       Self-employed incorporated   Self-employed not incorporated
##                               17                               16
##        State government employee
##                               24

