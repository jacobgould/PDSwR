# example 1.1 of section 1.2.3 
# (example 1.1 of section 1.2.3)  : The data science process : Stages of a data science project : Modeling 
# Title: Building a decision tree 

library('rpart')
load('GCDData.RData')
model <- rpart(Good.Loan ~
   Duration.in.month +
   Installment.rate.in.percentage.of.disposable.income +
   Credit.amount  +
   Other.installment.plans,
   data=d,
   control=rpart.control(maxdepth=4),
   method="class")

# example 1.2 of section 1.2.4 
# (example 1.2 of section 1.2.4)  : The data science process : Stages of a data science project : Model evaluation and critique 
# Title: Plotting the confusion matrix 

creditdata <- d
resultframe <- data.frame(Good.Loan=creditdata$Good.Loan,
                           pred=predict(model, type="class"))
rtab <- table(resultframe) 	# Note: 1 
rtab
##           pred
## Good.Loan  BadLoan GoodLoan
##   BadLoan       41      259
##   GoodLoan      13      687

sum(diag(rtab))/sum(rtab)  	# Note: 2 
## [1] 0.728
sum(rtab[1,1])/sum(rtab[,1]) 	# Note: 3 
## [1] 0.7592593
sum(rtab[1,1])/sum(rtab[1,]) 	# Note: 4 
## [1] 0.1366667
sum(rtab[2,1])/sum(rtab[2,]) 	# Note: 5 
## [1] 0.01857143

# Note 1: 
#   Create the confusion matrix. Rows represent 
#   actual loan status; columns represent predicted 
#   loan status. The diagonal entries represent 
#   correct predictions. 

# Note 2: 
#   accuracyconfusion matrixOverall model accuracy: 73% of the predictions 
#   were correct. 

# Note 3: 
#   precisionconfusion matrixModel precision: 76% of the applicants 
#   predicted as bad really did default. 

# Note 4: 
#   recallconfusion matrixModel recall: the model found 14% of the 
#   defaulting loans. 

# Note 5: 
#   false positive rateconfusion matrixFalse positive rate: 2% of the good applicants 
#   were mistakenly identified as bad. 

# example 1.3 of section 1.3.1 
# (example 1.3 of section 1.3.1)  : The data science process : Setting expectations : Determining lower and upper bounds on model performance 
# Title: Plotting the relation between disposable income and loan outcome 

tab1 <- as.table(matrix(data=c(50,6,0,44),nrow=2,ncol=2))
dimnames(tab1) <- list('loan.as.pct.disposable.income'=
      c('LT.15pct','GT.15pct'),
   'loan.quality.pop1'=
      c('goodloan','badloan'))
tab2 <- as.table(matrix(data=c(34,18,16,32),nrow=2,ncol=2))
dimnames(tab2) <- list('loan.as.pct.disposable.income'=
      c('LT.15pct','GT.15pct'),
   'loan.quality.pop2'=
      c('goodloan','badloan'))
tab1
##                              loan.quality.pop1 	# Note: 1 
## loan.as.pct.disposable.income goodloan badloan
##                      LT.15pct       50       0
##                      GT.15pct        6      44
sum(diag(tab1))/sum(tab1)                  	# Note: 2 
## [1] 0.94

tab2
##                              loan.quality.pop2  	# Note: 3 
## loan.as.pct.disposable.income goodloan badloan
##                      LT.15pct       34      16
##                      GT.15pct       18      32
sum(diag(tab2))/sum(tab2)
## [1] 0.66                                                        	# Note: 4

# Note 1: 
#   The count of correct predictions is on the 
#   diagonal of tab1. In this first population, all 
#   the loans that were less than 15% of disposable 
#   income were good loans, and all but six of the 
#   loans that were greater than 15% of disposable 
#   income defaulted. So you know that 
#   loan.as.pct.disposable.income models loan quality 
#   well in this population. Or as statisticians might 
#   say, loan.as.pct.disposable.income “explains” the 
#   output (loan quality). 

# Note 2: 
#   In fact, it’s 94% accurate. 

# Note 3: 
#   In the second population, about a third of 
#   the loans that were less than 15% of disposable 
#   income defaulted, and over half of the loans that 
#   were greater than 15% of disposable income were 
#   good. So you know that 
#   loan.as.pct.disposable.income doesn’t model loan 
#   quality well in this population. 

# Note 4: 
#   The rule of thumb is only 66% 
#   accurate. 

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

# example 3.1 of section 3.1 
# (example 3.1 of section 3.1)  : Exploring data : Using summary statistics to spot problems 
# Title: The summary() command 

custdata <- read.table('custdata.tsv',
   header=TRUE,sep='\t')
summary(custdata)
## custid        sex
## Min.   :   2068   F:440
## 1st Qu.: 345667   M:560
## Median : 693403
## Mean   : 698500
## 3rd Qu.:1044606
## Max.   :1414286
##
## is.employed         income     	# Note: 1 
## Mode :logical   Min.   : -8700
## FALSE:73        1st Qu.: 14600
## TRUE :599       Median : 35000
## NA's :328       Mean   : 53505
##                 3rd Qu.: 67000
##                 Max.   :615000
##
## marital.stat
## Divorced/Separated:155
## Married           :516
## Never Married     :233
## Widowed           : 96
##
## health.ins                    	# Note: 2 
## Mode :logical
## FALSE:159
## TRUE :841
## NA's :0
##
## housing.type                        	# Note: 3 
## Homeowner free and clear    :157
## Homeowner with mortgage/loan:412
## Occupied with no rent       : 11
## Rented                      :364
## NA's                        : 56
##
## recent.move      num.vehicles
## Mode :logical   Min.   :0.000
## FALSE:820       1st Qu.:1.000
## TRUE :124       Median :2.000
## NA's :56        Mean   :1.916
##                 3rd Qu.:2.000
##                 Max.   :6.000
##                 NA's   :56
##
## age              state.of.res       	# Note: 4 
## Min.   :  0.0   California  :100
## 1st Qu.: 38.0   New York    : 71
## Median : 50.0   Pennsylvania: 70
## Mean   : 51.7   Texas       : 56
## 3rd Qu.: 64.0   Michigan    : 52
## Max.   :146.7   Ohio        : 51
##                 (Other)     :600

# Note 1: 
#   The variable is.employed is missing for 
#   about a third of the data. The variable income has negative values, which are 
#   potentially invalid. 

# Note 2: 
#   About 84% of the customers have health 
#   insurance. 

# Note 3: 
#   The variables housing.type, recent.move, and 
#   num.vehicles are each missing 56 values. 

# Note 4: 
#   The average value of the variable age seems 
#   plausible, but the minimum and maximum values seem unlikely. The variable 
#   state.of.res is a categorical variable; summary() reports how many customers are in 
#   each state (for the first few states). 

# example 3.3 of section 3.1.1 
# (example 3.3 of section 3.1.1)  : Exploring data : Using summary statistics to spot problems : Typical problems revealed by data summaries 
# Title: Examples of invalid values and outliers 

summary(custdata$income)
##    Min. 1st Qu.  Median    Mean 3rd Qu.
##   -8700   14600   35000   53500   67000   	# Note: 1 
##    Max.
##  615000

summary(custdata$age)
##    Min. 1st Qu.  Median    Mean 3rd Qu.
##     0.0    38.0    50.0    51.7    64.0   	# Note: 2 
##    Max.
##   146.7

# Note 1: 
#   Negative values for income could indicate 
#   bad data. They might also have a special meaning, like “amount of 
#   debt.” Either way, you should check how prevalent the issue is, 
#   and decide what to do: Do you drop the data with negative income? Do you 
#   convert negative values to zero? 

# Note 2: 
#   Customers of age zero, or customers of an 
#   age greater than about 110 are outliers. They fall out of the range of 
#   expected customer values. Outliers could be data input errors. 
#   They could be special sentinel values: zero might mean “age unknown” or 
#   “refuse to state.” And some of your customers might be especially 
#   long-lived. 

# example 3.4 of section 3.1.1 
# (example 3.4 of section 3.1.1)  : Exploring data : Using summary statistics to spot problems : Typical problems revealed by data summaries 
# Title: Looking at the data range of a variable 

summary(custdata$income)
##    Min. 1st Qu.  Median    Mean 3rd Qu.
##   -8700   14600   35000   53500   67000   	# Note: 1 
##    Max.
##  615000

# Note 1: 
#   Income ranges from zero to over half a million 
#   dollars; a very wide range. 

# example 3.5 of section 3.1.1 
# (example 3.5 of section 3.1.1)  : Exploring data : Using summary statistics to spot problems : Typical problems revealed by data summaries 
# Title: Checking units sounds silly, but mistakes can lead to spectacular errors if not caught 

Income = custdata$income/1000
summary(Income)                                	# Note: 1 
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##    -8.7    14.6    35.0    53.5    67.0   615.0

# Note 1: 
#   The variable Income is defined as Income = custdata$income/1000. But suppose you didn’t know 
#   that. Looking only at the summary, the values could plausibly be 
#   interpreted to mean either “hourly wage” or “yearly income in units 
#   of $1000.” 

# example 3.6 of section 3.2.1 
# (example 3.6 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 
# Title: Plotting a histogram 

library(ggplot2)     	# Note: 1 

ggplot(custdata) +
   geom_histogram(aes(x=age),
   binwidth=5, fill="gray") 	# Note: 2

# Note 1: 
#   Load the ggplot2 library, if you haven’t 
#   already done so. 

# Note 2: 
#   binwidth parameterThe binwidth parameter tells the 
#   geom_histogram call how to make bins of five-year intervals (default is 
#   datarange/30). The fill parameter specifies the color of the histogram 
#   bars (default: black). 

# example 3.7 of section 3.2.1 
# (example 3.7 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 
# Title: Producing a density plot 

library(scales) 	# Note: 1 

ggplot(custdata) + geom_density(aes(x=income)) +
   scale_x_continuous(labels=dollar) 	# Note: 2

# Note 1: 
#   The scales package brings in the dollar 
#   scale notation. 

# Note 2: 
#   Set the x-axis labels to 
#   dollars. 

# example 3.8 of section 3.2.1 
# (example 3.8 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 
# Title: Creating a log-scaled density plot 

ggplot(custdata) + geom_density(aes(x=income)) +
   scale_x_log10(breaks=c(100,1000,10000,100000), labels=dollar) +  	# Note: 1 
   annotation_logticks(sides="bt")  	# Note: 2

# Note 1: 
#   Set the x-axis to be in log10 scale, with 
#   manually set tick points and labels as dollars. 

# Note 2: 
#   Add log-scaled tick marks to the top and 
#   bottom of the graph. 

# informalexample 3.2 of section 3.2.1 
# (informalexample 3.2 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 

ggplot(custdata) + geom_bar(aes(x=marital.stat), fill="gray")

# example 3.9 of section 3.2.1 
# (example 3.9 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 
# Title: Producing a horizontal bar chart 

ggplot(custdata) +
   geom_bar(aes(x=state.of.res), fill="gray") +  	# Note: 1 
   coord_flip() + 	# Note: 2 
   theme(axis.text.y=element_text(size=rel(0.8)))  	# Note: 3

# Note 1: 
#   Plot bar chart as before: state.of.res 
#   is on x axis, count is on y-axis. 

# Note 2: 
#   Flip the x and y axes: state.of.res is 
#   now on the y-axis. 

# Note 3: 
#   Reduce the size of the y-axis tick 
#   labels to 80% of default size for legibility. 

# example 3.10 of section 3.2.1 
# (example 3.10 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 
# Title: Producing a bar chart with sorted categories 

statesums <- table(custdata$state.of.res) 	# Note: 1 
statef <- as.data.frame(statesums) 	# Note: 2 
colnames(statef)<-c("state.of.res", "count") 	# Note: 3 
summary(statef)  	# Note: 4 
## state.of.res     count
## Alabama   : 1    Min.   :  1.00
## Alaska    : 1    1st Qu.:  5.00
## Arizona   : 1    Median : 12.00
## Arkansas  : 1    Mean   : 20.00
## California: 1    3rd Qu.: 26.25
## Colorado  : 1    Max.   :100.00
## (Other)   :44
statef <- transform(statef,
   state.of.res=reorder(state.of.res, count)) 	# Note: 5 
summary(statef)                       	# Note: 6 
## state.of.res     count
## Delaware    : 1    Min.   :  1.00
## North Dakota: 1    1st Qu.:  5.00
## Wyoming     : 1    Median : 12.00
## Rhode Island: 1    Mean   : 20.00
## Alaska      : 1    3rd Qu.: 26.25
## Montana     : 1    Max.   :100.00
## (Other)     :44
ggplot(statef)+ geom_bar(aes(x=state.of.res,y=count),
   stat="identity",              	# Note: 7 
   fill="gray") +
   coord_flip() +                                       	# Note: 8 
   theme(axis.text.y=element_text(size=rel(0.8)))

# Note 1: 
#   The table() command aggregates the data by state of residence—exactly the information the bar 
#   chart plots. 

# Note 2: 
#   Convert the table to a data frame. The default column names are Var1 and Freq. 

# Note 3: 
#   Rename the columns for readability. 

# Note 4: 
#   Notice that the default ordering for the 
#   state.of.res variable is alphabetical. 

# Note 5: 
#   Use the reorder() function to set the 
#   state.of.res variable to be count ordered. Use the transform() function 
#   to apply the transformation to the state.of.res data frame. 

# Note 6: 
#   The state.of.res variable is now count 
#   ordered. 

# Note 7: 
#   Since the data is being passed to 
#   geom_bar pre-aggregated, specify both the x and 
#   y variables, and use stat="identity" to plot the 
#   data exactly as given. 

# Note 8: 
#   Flip the axes and reduce the size of the 
#   label text as before. 

# example 3.11 of section 3.2.2 
# (example 3.11 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Producing a line plot 

x <- runif(100)   	# Note: 1 
y <- x^2 + 0.2*x   	# Note: 2 
ggplot(data.frame(x=x,y=y), aes(x=x,y=y)) + geom_line()  	# Note: 3

# Note 1: 
#   First, generate the data for this example. 
#   The x variable is uniformly randomly distributed 
#   between 0 and 1. 

# Note 2: 
#   The y variable is a 
#   quadratic function of x. 

# Note 3: 
#   Plot the line plot. 

# example 3.12 of section 3.2.2 
# (example 3.12 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Examining the correlation between age and income 

custdata2 <- subset(custdata,
   (custdata$age > 0 & custdata$age < 100
   & custdata$income > 0))                  	# Note: 1 

cor(custdata2$age, custdata2$income) 	# Note: 2 

## [1] -0.02240845 	# Note: 3

# Note 1: 
#   Only consider a subset of data with 
#   reasonable age and income values. 

# Note 2: 
#   Get correlation of age and income. 

# Note 3: 
#   Resulting correlation. 

# informalexample 3.3 of section 3.2.2 
# (informalexample 3.3 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 

ggplot(custdata2, aes(x=age, y=income)) +
   geom_point() + ylim(0, 200000)

# informalexample 3.4 of section 3.2.2 
# (informalexample 3.4 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 

ggplot(custdata2, aes(x=age, y=income)) + geom_point() +
  stat_smooth(method="lm") +
  ylim(0, 200000)

# informalexample 3.5 of section 3.2.2 
# (informalexample 3.5 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 

ggplot(custdata2, aes(x=age, y=income)) +
   geom_point() + geom_smooth() +
   ylim(0, 200000)

# example 3.13 of section 3.2.2 
# (example 3.13 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Plotting the distribution of health.ins as a function of age 

ggplot(custdata2, aes(x=age, y=as.numeric(health.ins))) + 	# Note: 1 
   geom_point(position=position_jitter(w=0.05, h=0.05)) +  	# Note: 2 
   geom_smooth() 	# Note: 3

# Note 1: 
#   The Boolean variable health.ins must be 
#   converted to a 0/1 variable using as.numeric. 

# Note 2: 
#   Since y values can 
#   only be 0 or 1, add a small jitter to get a sense of data 
#   density. 

# Note 3: 
#   Add smoothing curve. 

# example 3.14 of section 3.2.2 
# (example 3.14 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Producing a hexbin plot 

library(hexbin) 	# Note: 1 

ggplot(custdata2, aes(x=age, y=income)) +
   geom_hex(binwidth=c(5, 10000)) +   	# Note: 2 
   geom_smooth(color="white", se=F) +  	# Note: 3 
   ylim(0,200000)

# Note 1: 
#   Load hexbin library. 

# Note 2: 
#   Create hexbin with age binned into 5-year 
#   increments, income in increments of $10,000. 

# Note 3: 
#   Add smoothing curve in white; suppress 
#   standard error ribbon (se=F). 

# example 3.15 of section 3.2.2 
# (example 3.15 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Specifying different styles of bar chart 

ggplot(custdata) + geom_bar(aes(x=marital.stat,
   fill=health.ins)) 	# Note: 1 

ggplot(custdata) + geom_bar(aes(x=marital.stat,
   fill=health.ins),
   position="dodge")      	# Note: 2 

ggplot(custdata) + geom_bar(aes(x=marital.stat,
   fill=health.ins),
   position="fill")        	# Note: 3

# Note 1: 
#   Stacked bar chart, the 
#   default 

# Note 2: 
#   Side-by-side bar chart 

# Note 3: 
#   Filled bar chart 

# example 3.16 of section 3.2.2 
# (example 3.16 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Plotting data with a rug 

ggplot(custdata, aes(x=marital.stat)) +
   geom_bar(aes(fill=health.ins), position="fill") +
   geom_point(aes(y=-0.05), size=0.75, alpha=0.3, 	# Note: 1 
   position=position_jitter(h=0.01)) 	# Note: 2

# Note 1: 
#   Set the points just under the y-axis, 
#   three-quarters of default size, and make them slightly transparent with 
#   the alpha parameter. 

# Note 2: 
#   Jitter the points slightly for 
#   legibility. 

# example 3.17 of section 3.2.2 
# (example 3.17 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Plotting a bar chart with and without facets 

ggplot(custdata2) +                                          	# Note: 1 
   geom_bar(aes(x=housing.type, fill=marital.stat ),
      position="dodge") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))   	# Note: 2 

ggplot(custdata2) +                                          	# Note: 3 
   geom_bar(aes(x=marital.stat), position="dodge",
      fill="darkgray") +
   facet_wrap(~housing.type, scales="free_y") +               	# Note: 4 
   theme(axis.text.x = element_text(angle = 45, hjust = 1))   	# Note: 5

# Note 1: 
#   Side-by-side bar chart. 

# Note 2: 
#   coord_flip commandTilt the x-axis labels so they 
#   don’t overlap. You can also use coord_flip() to rotate the graph, as we 
#   saw previously. Some prefer coord_flip() because the theme() layer is 
#   complicated to use. 

# Note 3: 
#   The faceted bar chart. 

# Note 4: 
#   Facet the graph by housing.type. The scales="free_y" argument specifies that each facet has 
#   an independently scaled y-axis (the default is that all facets have 
#   the same scales on both axes). The argument free_x would free the 
#   x-axis scaling, and the argument free frees both axes. 

# Note 5: 
#   As of this writing, 
#   facet_wrap is incompatible with coord_flip, so we have to tilt the 
#   x-axis labels. 

# example 4.1 of section 4.1.1 
# (example 4.1 of section 4.1.1)  : Managing data : Cleaning data : Treating missing values (NAs) 
# Title: Checking locations of missing data 

custdata <- read.table('custdata.tsv',
   header=TRUE,sep='\t')
summary(custdata[is.na(custdata$housing.type), 	# Note: 1 
                   c("recent.move","num.vehicles")]) 	# Note: 2 

##  recent.move     num.vehicles   	# Note: 3 
##  Mode:logical   Min.   : NA
##  NA's:56        1st Qu.: NA
##                 Median : NA
##                 Mean   :NaN
##                 3rd Qu.: NA
##                 Max.   : NA
##                 NA's   :56

# Note 1: 
#   Restrict to the rows where housing.type is 
#   NA. 

# Note 2: 
#   Look only at the columns recent.move and 
#   num.vehicles. 

# Note 3: 
#   The output: all NAs. All the missing data 
#   comes from the same rows. 

# example 4.2 of section 4.1.1 
# (example 4.2 of section 4.1.1)  : Managing data : Cleaning data : Treating missing values (NAs) 
# Title: Remapping NA to a level 

custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),  	# Note: 1 
                                   "missing",                    	# Note: 2 
                                   ifelse(custdata$is.employed==T, 	# Note: 3 
                                          "employed",
                                          "not employed"))  	# Note: 4 

summary(as.factor(custdata$is.employed.fix)) 	# Note: 5 

##     employed      missing not employed
##          599          328           73

# Note 1: 
#   If is.employed value is missing... 

# Note 2: 
#   ...assign the value "missing". 
#   Otherwise... 

# Note 3: 
#   ...if is.employed==TRUE, assign the value 
#   "employed"... 

# Note 4: 
#   ...or the value "not employed". 

# Note 5: 
#   The transformation has turned the variable 
#   type from factor to string. You can change it back 
#   with the as.factor() function. 

# informalexample 4.1 of section 4.1.1 
# (informalexample 4.1 of section 4.1.1)  : Managing data : Cleaning data : Treating missing values (NAs) 

custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),
                  "not in active workforce",
                   ifelse(custdata$is.employed==T,
                                   "employed",
                                    "not employed"))

# informalexample 4.2 of section 4.1.1 
# (informalexample 4.2 of section 4.1.1)  : Managing data : Cleaning data : Treating missing values (NAs) 

summary(custdata$Income)

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
##       0   25000   45000   66200   82000  615000     328

# informalexample 4.3 of section 4.1.1 
# (informalexample 4.3 of section 4.1.1)  : Managing data : Cleaning data : Treating missing values (NAs) 

meanIncome <- mean(custdata$Income, na.rm=T) 	# Note: 1 
Income.fix <- ifelse(is.na(custdata$Income),
                       meanIncome,
                       custdata$Income)
summary(Income.fix)

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##       0   35000   66200   66200   66200  615000

# Note 1: 
#   Don’t forget the argument "na.rm=T"! 
#   Otherwise, the mean() function will include the 
#   NAs by default, and meanIncome will be NA. 

# example 4.3 of section 4.1.1 
# (example 4.3 of section 4.1.1)  : Managing data : Cleaning data : Treating missing values (NAs) 
# Title: Converting missing numeric data to a level 

breaks <-c(0, 10000, 50000, 100000, 250000, 1000000)           	# Note: 1 

Income.groups <- cut(custdata$income,
                      breaks=breaks, include.lowest=T)  	# Note: 2 

summary(Income.groups)                                        	# Note: 3 

##  [0,1e+04] (1e+04,5e+04] (5e+04,1e+05] (1e+05,2.5e+05] (2.5e+05,1e+06]
##         63           312           178              98              21
##       NA's
##        328

Income.groups <- as.character(Income.groups)                   	# Note: 4 

Income.groups <- ifelse(is.na(Income.groups),                  	# Note: 5 
                      "no income", Income.groups)

summary(as.factor(Income.groups))

##  (1e+04,5e+04] (1e+05,2.5e+05] (2.5e+05,1e+06]  (5e+04,1e+05]  [0,1e+04]
##            312              98              21            178         63
##      no income
##            328

# Note 1: 
#   Select some income ranges of interest. To 
#   use the cut() function, the upper and lower bounds 
#   should encompass the full income range of the 
#   data. 

# Note 2: 
#   Cut the data into income ranges. The 
#   include.lowest=T argument makes sure that zero 
#   income data is included in the lowest income range 
#   category. By default it would be excluded. 

# Note 3: 
#   The cut() function produces factor 
#   variables. Note the NAs are preserved. 

# Note 4: 
#   To preserve the category names before adding 
#   a new category, convert the variables to strings. 

# Note 5: 
#   Add the "no income" category to replace the 
#   NAs. 

# example 4.4 of section 4.1.1 
# (example 4.4 of section 4.1.1)  : Managing data : Cleaning data : Treating missing values (NAs) 
# Title: Tracking original NAs with an extra categorical variable 

missingIncome <- is.na(custdata$Income)  	# Note: 1 
Income.fix <- ifelse(is.na(custdata$Income), 0, custdata$Income) 	# Note: 2

# Note 1: 
#   The missingIncome variable lets you 
#   differentiate the two kinds of zeros in the data: 
#   the ones that you are about to add, and the ones 
#   that were already there. 

# Note 2: 
#   Replace the NAs with zeros. 

# example 4.5 of section 4.1.2 
# (example 4.5 of section 4.1.2)  : Managing data : Cleaning data : Data transformations 
# Title: Normalizing income by state 

medianincome <- aggregate(income~state.of.res,custdata,FUN=median)
colnames(medianincome) <- c('State','Median.Income')
summary(medianincome)  	# Note: 1 

##         State    Median.Income
##            : 1   Min.   :37427
##  Alabama   : 1   1st Qu.:47483
##  Alaska    : 1   Median :52274
##  Arizona   : 1   Mean   :52655
##  Arkansas  : 1   3rd Qu.:57195
##  California: 1   Max.   :68187
##  (Other)   :46

        
custdata <- merge(custdata, medianincome,
                   by.x="state.of.res", by.y="State")  	# Note: 2 

summary(custdata[,c("state.of.res", "income", "Median.Income")]) 	# Note: 3 

##        state.of.res     income       Median.Income
##  California  :100   Min.   : -8700   Min.   :37427
##  New York    : 71   1st Qu.: 14600   1st Qu.:44819
##  Pennsylvania: 70   Median : 35000   Median :50977
##  Texas       : 56   Mean   : 53505   Mean   :51161
##  Michigan    : 52   3rd Qu.: 67000   3rd Qu.:55559
##  Ohio        : 51   Max.   :615000   Max.   :68187
##  (Other)     :600

custdata$income.norm <- with(custdata, income/Median.Income) 	# Note: 4 
summary(custdata$income.norm)

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## -0.1791  0.2729  0.6992  1.0820  1.3120 11.6600

# Note 1: 
#   medianincome is a data frame of median 
#   income by state. 

# Note 2: 
#   Merge median income information into the 
#   custdata data frame by matching the column 
#   custdata$state.of.res to the column 
#   medianincome$State. 

# Note 3: 
#   Median.Income is now part of custdata. 

# Note 4: 
#   Normalize income by Median.Income. 

# informalexample 4.4 of section 4.1.2 
# (informalexample 4.4 of section 4.1.2)  : Managing data : Cleaning data : Data transformations 

custdata$income.lt.20K <- custdata$income < 20000
summary(custdata$income.lt.20K)
##    Mode   FALSE    TRUE    NA's
## logical     678     322       0

# example 4.6 of section 4.1.2 
# (example 4.6 of section 4.1.2)  : Managing data : Cleaning data : Data transformations 
# Title: Converting age into ranges 

brks <- c(0, 25, 65, Inf)  	# Note: 1 
custdata$age.range <- cut(custdata$age,
    breaks=brks, include.lowest=T) 	# Note: 2 
summary(custdata$age.range) 	# Note: 3 
        
##   [0,25]  (25,65] (65,Inf]
##       56      732      212

# Note 1: 
#   Select the age ranges of interest. The upper 
#   and lower bounds should encompass the full range 
#   of the data. 

# Note 2: 
#   Cut the data into age ranges. The 
#   include.lowest=T argument makes sure that zero age 
#   data is included in the lowest age range category. 
#   By default it would be excluded. 

# Note 3: 
#   The output of cut() is a factor variable. 

# example 4.7 of section 4.1.2 
# (example 4.7 of section 4.1.2)  : Managing data : Cleaning data : Data transformations 
# Title: Centering on mean age 

summary(custdata$age)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##     0.0    38.0    50.0    51.7    64.0   146.7
meanage <- mean(custdata$age)
custdata$age.normalized <- custdata$age/meanage
summary(custdata$age.normalized)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##  0.0000  0.7350  0.9671  1.0000  1.2380  2.8370

# example 4.8 of section 4.1.2 
# (example 4.8 of section 4.1.2)  : Managing data : Cleaning data : Data transformations 
# Title: Summarizing age 

summary(custdata$age)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##     0.0    38.0    50.0    51.7    64.0   146.7
meanage <- mean(custdata$age)  	# Note: 1 
stdage <- sd(custdata$age)     	# Note: 2 
meanage
## [1] 51.69981
stdage
## [1] 18.86343
custdata$age.normalized <- (custdata$age-meanage)/stdage 	# Note: 3 
summary(custdata$age.normalized)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
## -2.74100 -0.72630 -0.09011  0.00000  0.65210  5.03500

# Note 1: 
#   Take the mean. 

# Note 2: 
#   Take the standard deviation. 

# Note 3: 
#   Use the mean value as the origin (or 
#   reference point) and rescale the distance from the 
#   mean by the standard deviation. 

# informalexample 4.5 of section 4.1.2 
# (informalexample 4.5 of section 4.1.2)  : Managing data : Cleaning data : Data transformations 

signedlog10 <- function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

# example 4.9 of section 4.2.2 
# (example 4.9 of section 4.2.2)  : Managing data : Sampling for modeling and validation : Creating a sample group column 
# Title: Splitting into test and training using a random group mark 

custdata$gp <- runif(dim(custdata)[1])  	# Note: 1 
testSet <- subset(custdata, custdata$gp <= 0.1) 	# Note: 2 
trainingSet <- subset(custdata, custdata$gp > 0.1) 	# Note: 3 
dim(testSet)[1]
## [1] 93
dim(trainingSet)[1]
## [1] 907

# Note 1: 
#   dim(custdata) returns the number of rows and 
#   columns of the data frame as a vector, so 
#   dim(custdata)[1] returns the number of rows. 

# Note 2: 
#   Here we generate a test set of about 10% of 
#   the data (93 customers—a little over 9%, actually) 
#   and train on the remaining 90%. 

# Note 3: 
#   Here we generate a training using the 
#   remaining data. 

# example 4.10 of section 4.2.3 
# (example 4.10 of section 4.2.3)  : Managing data : Sampling for modeling and validation : Record grouping 
# Title: Ensuring test/train split doesn’t split inside a household 

hh <- unique(hhdata$household_id) 	# Note: 1 
households <- data.frame(household_id = hh, gp = runif(length(hh))) 	# Note: 2 
hhdata <- merge(hhdata, households, by="household_id") 	# Note: 3

# Note 1: 
#   Get all unique household IDs from your data 
#   frame. 

# Note 2: 
#   Create a temporary data frame of household IDs 
#   and a uniformly random number from 0 to 1. 

# Note 3: 
#   Merge new random sample group column back into 
#   original data frame. 

# example 5.1 of section 5.2.1 
# (example 5.1 of section 5.2.1)  : Choosing and evaluating models : Evaluating models : Evaluating classification models 
# Title: Building and applying a logistic regression spam model 

spamD <- read.table('spamD.tsv',header=T,sep='\t')
spamTrain <- subset(spamD,spamD$rgroup>=10)
spamTest <- subset(spamD,spamD$rgroup<10)
spamVars <- setdiff(colnames(spamD),list('rgroup','spam'))
spamFormula <- as.formula(paste('spam=="spam"',
   paste(spamVars,collapse=' + '),sep=' ~ '))
spamModel <- glm(spamFormula,family=binomial(link='logit'),
   data=spamTrain)
spamTrain$pred <- predict(spamModel,newdata=spamTrain,
   type='response')
spamTest$pred <- predict(spamModel,newdata=spamTest,
   type='response')
print(with(spamTest,table(y=spam,glmPred=pred>0.5)))
##           glmPred
## y          FALSE TRUE
##   non-spam   264   14
##   spam        22  158

# example 5.2 of section 5.2.1 
# (example 5.2 of section 5.2.1)  : Choosing and evaluating models : Evaluating models : Evaluating classification models 
# Title: Spam classifications 

sample <- spamTest[c(7,35,224,327),c('spam','pred')]
print(sample)
##          spam         pred
## 115      spam 0.9903246227
## 361      spam 0.4800498077
## 2300 non-spam 0.0006846551
## 3428 non-spam 0.0001434345

# example 5.3 of section 5.2.1 
# (example 5.3 of section 5.2.1)  : Choosing and evaluating models : Evaluating models : Evaluating classification models 
# Title: Spam confusion matrix 

cM <- table(truth=spamTest$spam,prediction=spamTest$pred>0.5)
print(cM)
##          prediction
## truth      FALSE TRUE
##   non-spam   264   14
##   spam        22  158

# example 5.4 of section 5.2.1 
# (example 5.4 of section 5.2.1)  : Choosing and evaluating models : Evaluating models : Evaluating classification models 
# Title: Entering data by hand 

t <- as.table(matrix(data=c(288-1,17,1,13882-17),nrow=2,ncol=2))
rownames(t) <- rownames(cM)
colnames(t) <- colnames(cM)
print(t)
##          FALSE  TRUE
## non-spam   287     1
## spam        17 13865

# example 5.5 of section 5.2.2 
# (example 5.5 of section 5.2.2)  : Choosing and evaluating models : Evaluating models : Evaluating scoring models 
# Title: Plotting residuals 

d <- data.frame(y=(1:10)^2,x=1:10)
model <- lm(y~x,data=d)
d$prediction <- predict(model,newdata=d)
library('ggplot2')
ggplot(data=d) + geom_point(aes(x=x,y=y)) +
    geom_line(aes(x=x,y=prediction),color='blue') +
    geom_segment(aes(x=x,y=prediction,yend=y,xend=x)) +
    scale_y_continuous('')

# example 5.6 of section 5.2.3 
# (example 5.6 of section 5.2.3)  : Choosing and evaluating models : Evaluating models : Evaluating probability models 
# Title: Making a double density plot 

ggplot(data=spamTest) +
   geom_density(aes(x=pred,color=spam,linetype=spam))

# example 5.7 of section 5.2.3 
# (example 5.7 of section 5.2.3)  : Choosing and evaluating models : Evaluating models : Evaluating probability models 
# Title: Plotting the receiver operating characteristic curve 

library('ROCR')
eval <- prediction(spamTest$pred,spamTest$spam)
plot(performance(eval,"tpr","fpr"))
print(attributes(performance(eval,'auc'))$y.values[[1]])
## [1] 0.9660072

# example 5.8 of section 5.2.3 
# (example 5.8 of section 5.2.3)  : Choosing and evaluating models : Evaluating models : Evaluating probability models 
# Title: Calculating log likelihood 

sum(ifelse(spamTest$spam=='spam',
   log(spamTest$pred),
   log(1-spamTest$pred)))
## [1] -134.9478
sum(ifelse(spamTest$spam=='spam',
   log(spamTest$pred),
   log(1-spamTest$pred)))/dim(spamTest)[[1]]
## [1] -0.2946458

# example 5.9 of section 5.2.3 
# (example 5.9 of section 5.2.3)  : Choosing and evaluating models : Evaluating models : Evaluating probability models 
# Title: Computing the null model’s log likelihood 

pNull <- sum(ifelse(spamTest$spam=='spam',1,0))/dim(spamTest)[[1]]
sum(ifelse(spamTest$spam=='spam',1,0))*log(pNull) +
   sum(ifelse(spamTest$spam=='spam',0,1))*log(1-pNull)
## [1] -306.8952

# example 5.10 of section 5.2.3 
# (example 5.10 of section 5.2.3)  : Choosing and evaluating models : Evaluating models : Evaluating probability models 
# Title: Calculating entropy and conditional entropy 

entropy <- function(x) { 	# Note: 1 
    xpos <- x[x>0]
    scaled <- xpos/sum(xpos)
    sum(-scaled*log(scaled,2))
  }

print(entropy(table(spamTest$spam))) 	# Note: 2 
## [1] 0.9667165

conditionalEntropy <- function(t) { 	# Note: 3 
    (sum(t[,1])*entropy(t[,1]) + sum(t[,2])*entropy(t[,2]))/sum(t)
  }

print(conditionalEntropy(cM)) 	# Note: 4 
## [1] 0.3971897

# Note 1: 
#   Define function that computes the entropy 
#   from list of outcome counts 

# Note 2: 
#   Calculate entropy of spam/non-spam 
#   distribution 

# Note 3: 
#   Function to calculate conditional or 
#   remaining entropy of spam distribution (rows) 
#   given prediction (columns) 

# Note 4: 
#   Calculate conditional or remaining entropy 
#   of spam distribution given prediction 

# example 5.11 of section 5.2.5 
# (example 5.11 of section 5.2.5)  : Choosing and evaluating models : Evaluating models : Evaluating clustering models 
# Title: Clustering random data in the plane 

set.seed(32297)
d <- data.frame(x=runif(100),y=runif(100))
clus <- kmeans(d,centers=5)
d$cluster <- clus$cluster

# example 5.12 of section 5.2.5 
# (example 5.12 of section 5.2.5)  : Choosing and evaluating models : Evaluating models : Evaluating clustering models 
# Title: Plotting our clusters 

library('ggplot2'); library('grDevices')
h <- do.call(rbind,
   lapply(unique(clus$cluster),
      function(c) { f <- subset(d,cluster==c); f[chull(f),]}))
ggplot() +
 geom_text(data=d,aes(label=cluster,x=x,y=y,
   color=cluster),size=3)  +
 geom_polygon(data=h,aes(x=x,y=y,group=cluster,fill=as.factor(cluster)),
   alpha=0.4,linetype=0) +
 theme(legend.position = "none")

# example 5.13 of section 5.2.5 
# (example 5.13 of section 5.2.5)  : Choosing and evaluating models : Evaluating models : Evaluating clustering models 
# Title: Calculating the size of each cluster 

table(d$cluster)

##  1  2  3  4  5
## 10 27 18 17 28

# example 5.14 of section 5.2.5 
# (example 5.14 of section 5.2.5)  : Choosing and evaluating models : Evaluating models : Evaluating clustering models 
# Title: Calculating the typical distance between items in every pair of clusters 

library('reshape2')
n <- dim(d)[[1]]
pairs <- data.frame(
   ca = as.vector(outer(1:n,1:n,function(a,b) d[a,'cluster'])),
   cb = as.vector(outer(1:n,1:n,function(a,b) d[b,'cluster'])),
   dist = as.vector(outer(1:n,1:n,function(a,b)
           sqrt((d[a,'x']-d[b,'x'])^2 + (d[a,'y']-d[b,'y'])^2)))
   )
dcast(pairs,ca~cb,value.var='dist',mean)
##   ca         1         2         3         4         5
## 1  1 0.1478480 0.6524103 0.3780785 0.4404508 0.7544134
## 2  2 0.6524103 0.2794181 0.5551967 0.4990632 0.5165320
## 3  3 0.3780785 0.5551967 0.2031272 0.6122986 0.4656730
## 4  4 0.4404508 0.4990632 0.6122986 0.2048268 0.8365336
## 5  5 0.7544134 0.5165320 0.4656730 0.8365336 0.2221314

# example 6.1 of section 6.1.1 
# (example 6.1 of section 6.1.1)  : Memorization methods : KDD and KDD Cup 2009 : Getting started with KDD Cup 2009 data 
# Title: Preparing the KDD data for analysis 

d <- read.table('orange_small_train.data.gz',  	# Note: 1 
   header=T,
   sep='\t',
   na.strings=c('NA','')) 	# Note: 2 
churn <- read.table('orange_small_train_churn.labels.txt',
   header=F,sep='\t') 	# Note: 3 
d$churn <- churn$V1 	# Note: 4 
appetency <- read.table('orange_small_train_appetency.labels.txt',
   header=F,sep='\t')
d$appetency <- appetency$V1 	# Note: 5 
upselling <- read.table('orange_small_train_upselling.labels.txt',
   header=F,sep='\t')
d$upselling <- upselling$V1 	# Note: 6 
set.seed(729375) 	# Note: 7 
d$rgroup <- runif(dim(d)[[1]])
dTrainAll <- subset(d,rgroup<=0.9)
dTest <- subset(d,rgroup>0.9) 	# Note: 8 
outcomes=c('churn','appetency','upselling')
vars <- setdiff(colnames(dTrainAll),
   c(outcomes,'rgroup'))
catVars <- vars[sapply(dTrainAll[,vars],class) %in%
   c('factor','character')] 	# Note: 9 
numericVars <- vars[sapply(dTrainAll[,vars],class) %in%
   c('numeric','integer')] 	# Note: 10 
rm(list=c('d','churn','appetency','upselling')) 	# Note: 11 
outcome <- 'churn' 	# Note: 12 
pos <- '1' 	# Note: 13 
useForCal <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0 	# Note: 14 
dCal <- subset(dTrainAll,useForCal)
dTrain <- subset(dTrainAll,!useForCal)

# Note 1: 
#   Read the file of independent variables. All 
#   data from 
#   https://github.com/WinVector/zmPDSwR/tree/master/KDD2009. 

# Note 2: 
#   Treat both NA and the empty string as missing 
#   data. 

# Note 3: 
#   Read churn dependent variable. 

# Note 4: 
#   Add churn as a new column. 

# Note 5: 
#   Add appetency as a new column. 

# Note 6: 
#   Add upselling as a new column. 

# Note 7: 
#   By setting the seed to the pseudo-random 
#   number generator, we make our work reproducible: 
#   someone redoing it will see the exact same 
#   results. 

# Note 8: 
#   Split data into train and test subsets. 

# Note 9: 
#   Identify which features are categorical 
#   variables. 

# Note 10: 
#   Identify which features are numeric 
#   variables. 

# Note 11: 
#   Remove unneeded objects from workspace. 

# Note 12: 
#   Choose which outcome to model (churn). 

# Note 13: 
#   Choose which outcome is considered 
#   positive. 

# Note 14: 
#   Further split training data into training and 
#   calibration. 

# example 6.2 of section 6.2.1 
# (example 6.2 of section 6.2.1)  : Memorization methods : Building single-variable models : Using categorical features 
# Title: Plotting churn grouped by variable 218 levels 

table218 <- table(
   Var218=dTrain[,'Var218'], 	# Note: 1 
   churn=dTrain[,outcome], 	# Note: 2 
   useNA='ifany') 	# Note: 3 
print(table218)
##       churn
## Var218    -1     1
##   cJvF 19245  1220
##   UYBR 17860  1618
##   <NA>   423   152
# Note this listing was updated: 10-14-2014 as some of results in the book were
# accidentally from older code.  Will update later listings as we go forward.

# Note 1: 
#   Tabulate levels of Var218. 

# Note 2: 
#   Tabulate levels of churn outcome. 

# Note 3: 
#   Include NA values in tabulation. 

# example 6.3 of section 6.2.1 
# (example 6.3 of section 6.2.1)  : Memorization methods : Building single-variable models : Using categorical features 
# Title: Churn rates grouped by variable 218 codes 

print(table218[,2]/(table218[,1]+table218[,2]))
##       cJvF       UYBR       <NA>
## 0.05994389 0.08223821 0.26523297

# example 6.4 of section 6.2.1 
# (example 6.4 of section 6.2.1)  : Memorization methods : Building single-variable models : Using categorical features 
# Title: Function to build single-variable models for categorical variables 

mkPredC <- function(outCol,varCol,appCol) { 	# Note: 1 
   pPos <- sum(outCol==pos)/length(outCol) 	# Note: 2 
   naTab <- table(as.factor(outCol[is.na(varCol)]))
   pPosWna <- (naTab/sum(naTab))[pos] 	# Note: 3 
   vTab <- table(as.factor(outCol),varCol)
   pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3) 	# Note: 4 
   pred <- pPosWv[appCol] 	# Note: 5 
   pred[is.na(appCol)] <- pPosWna 	# Note: 6 
   pred[is.na(pred)] <- pPos 	# Note: 7 
   pred 	# Note: 8 
}

# Note 1: 
#   Given a vector of training outcomes (outCol), 
#   a categorical training variable (varCol), and a 
#   prediction variable (appCol), use outCol and 
#   varCol to build a single-variable model and then 
#   apply the model to appCol to get new 
#   predictions. 

# Note 2: 
#   Get stats on how often outcome is positive 
#   during training. 

# Note 3: 
#   Get stats on how often outcome is positive for 
#   NA values of variable during training. 

# Note 4: 
#   Get stats on how often outcome is positive, 
#   conditioned on levels of training variable. 

# Note 5: 
#   Make predictions by looking up levels of 
#   appCol. 

# Note 6: 
#   Add in predictions for NA levels of 
#   appCol. 

# Note 7: 
#   Add in predictions for levels of appCol that 
#   weren’t known during training. 

# Note 8: 
#   Return vector of predictions. 

# example 6.5 of section 6.2.1 
# (example 6.5 of section 6.2.1)  : Memorization methods : Building single-variable models : Using categorical features 
# Title: Applying single-categorical variable models to all of our datasets 

for(v in catVars) {
  pi <- paste('pred',v,sep='')
  dTrain[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTrain[,v])
  dCal[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dCal[,v])
  dTest[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTest[,v])
}

# example 6.6 of section 6.2.1 
# (example 6.6 of section 6.2.1)  : Memorization methods : Building single-variable models : Using categorical features 
# Title: Scoring categorical variables by AUC 

library('ROCR')

calcAUC <- function(predcol,outcol) {
    perf <- performance(prediction(predcol,outcol==pos),'auc')
    as.numeric(perf@y.values)
 }

for(v in catVars) {
   pi <- paste('pred',v,sep='')
   aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
   if(aucTrain>=0.8) {
      aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
      print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
        pi,aucTrain,aucCal))
   }
 }
## [1] "predVar200, trainAUC: 0.828 calibrationAUC: 0.527"
## [1] "predVar202, trainAUC: 0.829 calibrationAUC: 0.522"
## [1] "predVar214, trainAUC: 0.828 calibrationAUC: 0.527"
## [1] "predVar217, trainAUC: 0.898 calibrationAUC: 0.553"

# example 6.7 of section 6.2.2 
# (example 6.7 of section 6.2.2)  : Memorization methods : Building single-variable models : Using numeric features 
# Title: Scoring numeric variables by AUC 

mkPredN <- function(outCol,varCol,appCol) {
   cuts <- unique(as.numeric(quantile(varCol,
      probs=seq(0, 1, 0.1),na.rm=T)))
   varC <- cut(varCol,cuts)
   appC <- cut(appCol,cuts)
   mkPredC(outCol,varC,appC)
}
for(v in numericVars) {
   pi <- paste('pred',v,sep='')
   dTrain[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTrain[,v])
   dTest[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTest[,v])
   dCal[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dCal[,v])
   aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
   if(aucTrain>=0.55) {
      aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
      print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
        pi,aucTrain,aucCal))
   }
 }
## [1] "predVar6, trainAUC: 0.557 calibrationAUC: 0.554"
## [1] "predVar7, trainAUC: 0.555 calibrationAUC: 0.565"
## [1] "predVar13, trainAUC: 0.568 calibrationAUC: 0.553"
## [1] "predVar73, trainAUC: 0.608 calibrationAUC: 0.616"
## [1] "predVar74, trainAUC: 0.574 calibrationAUC: 0.566"
## [1] "predVar81, trainAUC: 0.558 calibrationAUC: 0.542"
## [1] "predVar113, trainAUC: 0.557 calibrationAUC: 0.567"
## [1] "predVar126, trainAUC: 0.635 calibrationAUC: 0.629"
## [1] "predVar140, trainAUC: 0.561 calibrationAUC: 0.560"
## [1] "predVar189, trainAUC: 0.574 calibrationAUC: 0.599"

# example 6.8 of section 6.2.2 
# (example 6.8 of section 6.2.2)  : Memorization methods : Building single-variable models : Using numeric features 
# Title: Plotting variable performance 

library('ggplot2')
ggplot(data=dCal) +
   geom_density(aes(x=predVar126,color=as.factor(churn)))

# example 6.9 of section 6.2.3 
# (example 6.9 of section 6.2.3)  : Memorization methods : Building single-variable models : Using cross-validation to estimate effects of overfitting 
# Title: Running a repeated cross-validation experiment 

var <- 'Var217'
aucs <- rep(0,100)
for(rep in 1:length(aucs)) {   	# Note: 1 
   useForCalRep <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0  	# Note: 2 
   predRep <- mkPredC(dTrainAll[!useForCalRep,outcome],  	# Note: 3 
      dTrainAll[!useForCalRep,var],
      dTrainAll[useForCalRep,var])
   aucs[rep] <- calcAUC(predRep,dTrainAll[useForCalRep,outcome])  	# Note: 4 
 }
mean(aucs)
## [1] 0.5556656
sd(aucs)
## [1] 0.01569345

# Note 1: 
#   For 100 iterations... 

# Note 2: 
#   ...select a random subset of about 10% of the training data as hold-out set,... 

# Note 3: 
#   ...use the random 90% of training data to train model and evaluate that model on hold-out 
#   set,... 

# Note 4: 
#   ...calculate resulting model’s AUC using hold-out set; store that value and repeat. 

# example 6.10 of section 6.2.3 
# (example 6.10 of section 6.2.3)  : Memorization methods : Building single-variable models : Using cross-validation to estimate effects of overfitting 
# Title: Empirically cross-validating performance 

fCross <- function() {
   useForCalRep <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0
   predRep <- mkPredC(dTrainAll[!useForCalRep,outcome],
      dTrainAll[!useForCalRep,var],
      dTrainAll[useForCalRep,var])
   calcAUC(predRep,dTrainAll[useForCalRep,outcome])
}
aucs <- replicate(100,fCross())

# example 6.11 of section 6.3.1 
# (example 6.11 of section 6.3.1)  : Memorization methods : Building models using many variables : Variable selection 
# Title: Basic variable selection 

#    Each variable we use represents a chance of explaining
# more of the outcome variation (a chance of building a better
# model) but also represents a possible source of noise and
# overfitting. To control this effect, we often preselect
# which subset of variables we’ll use to fit. Variable
# selection can be an important defensive modeling step even
# for types of models that “don’t need it” (as seen with
# decision trees in section 6.3.2).  Listing 6.11 shows a
# hand-rolled variable selection loop where each variable is
# scored according to a deviance inspired score, where a
# variable is scored with a bonus proportional to the change
# in in scaled log likelihood of the training data.  We could
# also try an AIC (Akaike information criterion) by
# subtracting a penalty proportional to the complexity of the
# variable (which in this case is 2^entropy for categorical
# variables and a stand-in of 1 for numeric variables).  The
# score is a bit ad hoc, but tends to work well in selecting
# variables. Notice we’re using performance on the calibration
# set (not the training set) to pick variables. Note that we
# don’t use the test set for calibration; to do so lessens the
# reliability of the test set for model quality confirmation.

logLikelyhood <- function(outCol,predCol) { 	# Note: 1 
  sum(ifelse(outCol==pos,log(predCol),log(1-predCol)))
}

selVars <- c()
minStep <- 5
baseRateCheck <- logLikelyhood(dCal[,outcome],
   sum(dCal[,outcome]==pos)/length(dCal[,outcome]))

for(v in catVars) {  	# Note: 2 
  pi <- paste('pred',v,sep='')
  liCheck <- 2*((logLikelyhood(dCal[,outcome],dCal[,pi]) -
      baseRateCheck))
  if(liCheck>minStep) {
     print(sprintf("%s, calibrationScore: %g",
        pi,liCheck))
     selVars <- c(selVars,pi)
  }
}

for(v in numericVars) { 	# Note: 3 
  pi <- paste('pred',v,sep='')
  liCheck <- 2*((logLikelyhood(dCal[,outcome],dCal[,pi]) -
      baseRateCheck))
  if(liCheck>=minStep) {
     print(sprintf("%s, calibrationScore: %g",
        pi,liCheck))
     selVars <- c(selVars,pi)
  }
}

# Note 1: 
#   Define a convenience function to compute log 
#   likelihood. 

# Note 2: 
#   Run through categorical variables and pick 
#   based on a deviance improvement (related to 
#   difference in log likelihoods; see chapter 
#   3). 

# Note 3: 
#   Run through numeric variables and pick 
#   based on a deviance improvement. 

# example 6.13 of section 6.3.2 
# (example 6.13 of section 6.3.2)  : Memorization methods : Building models using many variables : Using decision trees 
# Title: Building a bad decision tree 

library('rpart')
fV <- paste(outcome,'>0 ~ ',
   paste(c(catVars,numericVars),collapse=' + '),sep='')
tmodel <- rpart(fV,data=dTrain)
print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
## [1] 0.9241265
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
## [1] 0.5266172
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))
## [1] 0.5126917

# example 6.14 of section 6.3.2 
# (example 6.14 of section 6.3.2)  : Memorization methods : Building models using many variables : Using decision trees 
# Title: Building another bad decision tree 

tVars <- paste('pred',c(catVars,numericVars),sep='')
fV2 <- paste(outcome,'>0 ~ ',paste(tVars,collapse=' + '),sep='')
tmodel <- rpart(fV2,data=dTrain)
print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
## [1] 0.928669
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
## [1] 0.5390648
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))
## [1] 0.5384152

# example 6.15 of section 6.3.2 
# (example 6.15 of section 6.3.2)  : Memorization methods : Building models using many variables : Using decision trees 
# Title: Building yet another bad decision tree 

tmodel <- rpart(fV2,data=dTrain,
   control=rpart.control(cp=0.001,minsplit=1000,
      minbucket=1000,maxdepth=5)
 )
print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
## [1] 0.9421195
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
## [1] 0.5794633
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))
## [1] 0.547967

# example 6.16 of section 6.3.2 
# (example 6.16 of section 6.3.2)  : Memorization methods : Building models using many variables : Using decision trees 
# Title: Building a better decision tree 

f <- paste(outcome,'>0 ~ ',paste(selVars,collapse=' + '),sep='')
tmodel <- rpart(f,data=dTrain,
   control=rpart.control(cp=0.001,minsplit=1000,
      minbucket=1000,maxdepth=5)
 )
print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
## [1] 0.6906852
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
## [1] 0.6843595
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))
## [1] 0.6669301

# example 6.17 of section 6.3.2 
# (example 6.17 of section 6.3.2)  : Memorization methods : Building models using many variables : Using decision trees 
# Title: Printing the decision tree 

print(tmodel)
## n= 40518 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
##  1) root 40518 2769.3550 0.07379436  
##    2) predVar126< 0.07366888 18188  726.4097 0.04167583  
##      4) predVar126< 0.04391312 8804  189.7251 0.02203544 *
##      5) predVar126>=0.04391312 9384  530.1023 0.06010230  
##       10) predVar189< 0.08449448 8317  410.4571 0.05206204 *
##       11) predVar189>=0.08449448 1067  114.9166 0.12277410 *
##    3) predVar126>=0.07366888 22330 2008.9000 0.09995522  
##      6) predVar212< 0.07944508 8386  484.2499 0.06153112  
##       12) predVar73< 0.06813291 4084  167.5012 0.04285015 *
##       13) predVar73>=0.06813291 4302  313.9705 0.07926546 *
##      7) predVar212>=0.07944508 13944 1504.8230 0.12306370  
##       14) predVar218< 0.07134103 6728  580.7390 0.09542212  
##         28) predVar126< 0.1015407 3901  271.8426 0.07536529 *
##         29) predVar126>=0.1015407 2827  305.1617 0.12309870  
##           58) predVar73< 0.07804522 1452  110.0826 0.08264463 *
##           59) predVar73>=0.07804522 1375  190.1935 0.16581820 *
##       15) predVar218>=0.07134103 7216  914.1502 0.14883590  
##         30) predVar74< 0.0797246 2579  239.3579 0.10352850 *
##         31) predVar74>=0.0797246 4637  666.5538 0.17403490  
##           62) predVar189< 0.06775545 1031  102.9486 0.11251210 *
##           63) predVar189>=0.06775545 3606  558.5871 0.19162510 *

# example 6.18 of section 6.3.2 
# (example 6.18 of section 6.3.2)  : Memorization methods : Building models using many variables : Using decision trees 
# Title: Plotting the decision tree 

par(cex=0.7)
plot(tmodel)
text(tmodel)

# example 6.19 of section 6.3.3 
# (example 6.19 of section 6.3.3)  : Memorization methods : Building models using many variables : Using nearest neighbor methods 
# Title: Running k-nearest neighbors 

library('class')
nK <- 200
knnTrain <- dTrain[,selVars]  	# Note: 1 
knnCl <- dTrain[,outcome]==pos 	# Note: 2 
knnPred <- function(df) { 	# Note: 3 
    knnDecision <- knn(knnTrain,df,knnCl,k=nK,prob=T)
    ifelse(knnDecision==TRUE, 	# Note: 4 
       attributes(knnDecision)$prob,
       1-(attributes(knnDecision)$prob))
}
print(calcAUC(knnPred(dTrain[,selVars]),dTrain[,outcome]))
## [1] 0.7443927
print(calcAUC(knnPred(dCal[,selVars]),dCal[,outcome]))
## [1] 0.7119394
print(calcAUC(knnPred(dTest[,selVars]),dTest[,outcome]))
## [1] 0.718256

# Note 1: 
#   Build a data frame with only the variables we 
#   wish to use for classification. 

# Note 2: 
#   Build a vector with the known training 
#   outcomes. 

# Note 3: 
#   Bind the knn() training function with our data 
#   in a new function. 

# Note 4: 
#   Convert knn’s unfortunate convention of 
#   calculating probability as “proportion of the 
#   votes for the winning class” into the more useful 
#   “calculated probability of being a positive 
#   example.” 

# example 6.20 of section 6.3.3 
# (example 6.20 of section 6.3.3)  : Memorization methods : Building models using many variables : Using nearest neighbor methods 
# Title: Platting 200-nearest neighbor performance 

dCal$kpred <- knnPred(dCal[,selVars])
ggplot(data=dCal) +
   geom_density(aes(x=kpred,
      color=as.factor(churn),linetype=as.factor(churn)))

# example 6.21 of section 6.3.3 
# (example 6.21 of section 6.3.3)  : Memorization methods : Building models using many variables : Using nearest neighbor methods 
# Title: Plotting the receiver operating characteristic curve 

plotROC <- function(predcol,outcol) {
   perf <- performance(prediction(predcol,outcol==pos),'tpr','fpr')
   pf <- data.frame(
      FalsePositiveRate=perf@x.values[[1]],
      TruePositiveRate=perf@y.values[[1]])
   ggplot() +
      geom_line(data=pf,aes(x=FalsePositiveRate,y=TruePositiveRate)) +
      geom_line(aes(x=c(0,1),y=c(0,1)))
}
print(plotROC(knnPred(dTest[,selVars]),dTest[,outcome]))

# example 6.22 of section 6.3.3 
# (example 6.22 of section 6.3.3)  : Memorization methods : Building models using many variables : Using nearest neighbor methods 
# Title: Plotting the performance of a logistic regression model 

gmodel <- glm(as.formula(f),data=dTrain,family=binomial(link='logit'))
print(calcAUC(predict(gmodel,newdata=dTrain),dTrain[,outcome]))
## [1] 0.7309537
print(calcAUC(predict(gmodel,newdata=dTest),dTest[,outcome]))
## [1] 0.7234645
print(calcAUC(predict(gmodel,newdata=dCal),dCal[,outcome]))
## [1] 0.7170824

# example 6.23 of section 6.3.4 
# (example 6.23 of section 6.3.4)  : Memorization methods : Building models using many variables : Using Naive Bayes 
# Title: Building, applying, and evaluating a Naive Bayes model 

pPos <- sum(dTrain[,outcome]==pos)/length(dTrain[,outcome])
nBayes <- function(pPos,pf) { 	# Note: 1 
   pNeg <- 1 - pPos
   smoothingEpsilon <- 1.0e-5
   scorePos <- log(pPos + smoothingEpsilon) + 
      rowSums(log(pf/pPos + smoothingEpsilon)) 	# Note: 2 
   scoreNeg <- log(pNeg + smoothingEpsilon) +
      rowSums(log((1-pf)/(1-pPos) + smoothingEpsilon)) 	# Note: 3 
   m <- pmax(scorePos,scoreNeg)
   expScorePos <- exp(scorePos-m)
   expScoreNeg <- exp(scoreNeg-m) 	# Note: 4 
   expScorePos/(expScorePos+expScoreNeg) 	# Note: 5 
}
pVars <- paste('pred',c(numericVars,catVars),sep='')
dTrain$nbpredl <- nBayes(pPos,dTrain[,pVars])
dCal$nbpredl <- nBayes(pPos,dCal[,pVars])
dTest$nbpredl <- nBayes(pPos,dTest[,pVars]) 	# Note: 6 
print(calcAUC(dTrain$nbpredl,dTrain[,outcome]))
## [1] 0.9757348
print(calcAUC(dCal$nbpredl,dCal[,outcome]))
## [1] 0.5995206
print(calcAUC(dTest$nbpredl,dTest[,outcome]))
## [1] 0.5956515 	# Note: 7

# Note 1: 
#   Define a function that performs the Naive 
#   Bayes prediction. 

# Note 2: 
#   For each row, compute (with a smoothing term) 
#   the sum of log(P[positive & 
#   evidence_i]/P[positive]) across all columns. This 
#   is equivalent to the log of the product of 
#   P[evidence_i | positive] up to terms that don’t 
#   depend on the positive/negative outcome. 

# Note 3: 
#   For each row, compute (with a smoothing term) 
#   the sum of log(P[negative & 
#   evidence_i]/P[negative]) across all columns. This 
#   is equivalent to the log of the product of 
#   P[evidence_i | negative] up to terms that don’t 
#   depend on the positive/negative outcome. 

# Note 4: 
#   Exponentiate to turn sums back into products, 
#   but make sure we don’t cause a floating point 
#   overflow in doing so. 

# Note 5: 
#   Use the fact that the predicted positive 
#   probability plus the predicted negative 
#   probability should sum to 1.0 to find and 
#   eliminate Z. Return the correctly scaled predicted 
#   odds of being positive as our forecast. 

# Note 6: 
#   Apply the function to make the predictions. 

# Note 7: 
#   Calculate the AUCs. Notice the 
#   overfit—fantastic performance on the training 
#   set that isn’t repeated on the calibration or test 
#   sets. 

# example 6.24 of section 6.3.4 
# (example 6.24 of section 6.3.4)  : Memorization methods : Building models using many variables : Using Naive Bayes 
# Title: Using a Naive Bayes package 

library('e1071')
lVars <- c(catVars,numericVars)
ff <- paste('as.factor(',outcome,'>0) ~ ',
   paste(lVars,collapse=' + '),sep='')
nbmodel <- naiveBayes(as.formula(ff),data=dTrain)
dTrain$nbpred <- predict(nbmodel,newdata=dTrain,type='raw')[,'TRUE']
dCal$nbpred <- predict(nbmodel,newdata=dCal,type='raw')[,'TRUE']
dTest$nbpred <- predict(nbmodel,newdata=dTest,type='raw')[,'TRUE']
calcAUC(dTrain$nbpred,dTrain[,outcome])
## [1] 0.4643591
calcAUC(dCal$nbpred,dCal[,outcome])
## [1] 0.5544484
calcAUC(dTest$nbpred,dTest[,outcome])
## [1] 0.5679519

# example 7.1 of section 7.1.1 
# (example 7.1 of section 7.1.1)  : Linear and logistic regression : Using linear regression : Understanding linear regression 
# Title: Loading the PUMS data 

load("psub.RData")
dtrain <- subset(psub,ORIGRANDGROUP >= 500)
dtest <- subset(psub,ORIGRANDGROUP < 500)
model <- lm(log(PINCP,base=10) ~ AGEP + SEX + COW + SCHL,data=dtrain)
dtest$predLogPINCP <- predict(model,newdata=dtest)
dtrain$predLogPINCP <- predict(model,newdata=dtrain)

# example 7.2 of section 7.1.3 
# (example 7.2 of section 7.1.3)  : Linear and logistic regression : Using linear regression : Making predictions 
# Title: Plotting log income as a function of predicted log income 

library('ggplot2')
ggplot(data=dtest,aes(x=predLogPINCP,y=log(PINCP,base=10))) +
   geom_point(alpha=0.2,color="black") +
   geom_smooth(aes(x=predLogPINCP,
      y=log(PINCP,base=10)),color="black") +
   geom_line(aes(x=log(PINCP,base=10),
      y=log(PINCP,base=10)),color="blue",linetype=2) +
   scale_x_continuous(limits=c(4,5)) +
   scale_y_continuous(limits=c(3.5,5.5))

# example 7.3 of section 7.1.3 
# (example 7.3 of section 7.1.3)  : Linear and logistic regression : Using linear regression : Making predictions 
# Title: Plotting residuals income as a function of predicted log income 

ggplot(data=dtest,aes(x=predLogPINCP,
                     y=predLogPINCP-log(PINCP,base=10))) +
  geom_point(alpha=0.2,color="black") +
  geom_smooth(aes(x=predLogPINCP,
                  y=predLogPINCP-log(PINCP,base=10)),
                  color="black")

# example 7.4 of section 7.1.3 
# (example 7.4 of section 7.1.3)  : Linear and logistic regression : Using linear regression : Making predictions 
# Title: Computing R-squared 

rsq <- function(y,f) { 1 - sum((y-f)^2)/sum((y-mean(y))^2) }
rsq(log(dtrain$PINCP,base=10),predict(model,newdata=dtrain))
rsq(log(dtest$PINCP,base=10),predict(model,newdata=dtest))

# example 7.5 of section 7.1.3 
# (example 7.5 of section 7.1.3)  : Linear and logistic regression : Using linear regression : Making predictions 
# Title: Calculating root mean square error 

rmse <- function(y, f) { sqrt(mean( (y-f)^2 )) }
rmse(log(dtrain$PINCP,base=10),predict(model,newdata=dtrain))
rmse(log(dtest$PINCP,base=10),predict(model,newdata=dtest))

# example 7.6 of section 7.1.5 
# (example 7.6 of section 7.1.5)  : Linear and logistic regression : Using linear regression : Reading the model summary and characterizing coefficient quality 
# Title: Summarizing residuals 

summary(log(dtrain$PINCP,base=10) - predict(model,newdata=dtrain))
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
## -1.29200 -0.14150  0.02458  0.00000  0.17630  0.62530
summary(log(dtest$PINCP,base=10) - predict(model,newdata=dtest))
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
## -1.494000 -0.165300  0.018920 -0.004637  0.175500  0.868100

# informalexample 7.9 of section 7.1.5 
# (informalexample 7.9 of section 7.1.5)  : Linear and logistic regression : Using linear regression : Reading the model summary and characterizing coefficient quality 

df <- dim(dtrain)[1] - dim(summary(model)$coefficients)[1]

# informalexample 7.10 of section 7.1.5 
# (informalexample 7.10 of section 7.1.5)  : Linear and logistic regression : Using linear regression : Reading the model summary and characterizing coefficient quality 

modelResidualError <- sqrt(sum(residuals(model)^2)/df)

# example 7.7 of section 7.2.1 
# (example 7.7 of section 7.2.1)  : Linear and logistic regression : Using logistic regression : Understanding logistic regression 
# Title: Loading the CDC data 

load("NatalRiskData.rData")
train <- sdata[sdata$ORIGRANDGROUP<=5,]
test <- sdata[sdata$ORIGRANDGROUP>5,]

# example 7.8 of section 7.2.2 
# (example 7.8 of section 7.2.2)  : Linear and logistic regression : Using logistic regression : Building a logistic regression model 
# Title: Building the model formula 

complications <- c("ULD_MECO","ULD_PRECIP","ULD_BREECH")
 riskfactors <- c("URF_DIAB", "URF_CHYPER", "URF_PHYPER",
                "URF_ECLAM")
y <- "atRisk"
x <- c("PWGT",
      "UPREVIS",
      "CIG_REC",
      "GESTREC3",
      "DPLURAL",
      complications,
      riskfactors)
fmla <- paste(y, paste(x, collapse="+"), sep="~")

# example 7.9 of section 7.2.2 
# (example 7.9 of section 7.2.2)  : Linear and logistic regression : Using logistic regression : Building a logistic regression model 
# Title: Fitting the logistic regression model 

print(fmla)
## [1] "atRisk ~ PWGT+UPREVIS+CIG_REC+GESTREC3+DPLURAL+ULD_MECO+ULD_PRECIP+
##                    ULD_BREECH+URF_DIAB+URF_CHYPER+URF_PHYPER+URF_ECLAM"

model <- glm(fmla, data=train, family=binomial(link="logit"))

# example 7.10 of section 7.2.3 
# (example 7.10 of section 7.2.3)  : Linear and logistic regression : Using logistic regression : Making predictions 
# Title: Applying the logistic regression model 

train$pred <- predict(model, newdata=train, type="response")
test$pred <- predict(model, newdata=test, type="response")

# example 7.11 of section 7.2.3 
# (example 7.11 of section 7.2.3)  : Linear and logistic regression : Using logistic regression : Making predictions 
# Title: Plotting distribution of prediction score grouped by known outcome 

library('ggplot2')
ggplot(train, aes(x=pred, color=atRisk, linetype=atRisk)) +
       geom_density()

# example 7.12 of section 7.2.3 
# (example 7.12 of section 7.2.3)  : Linear and logistic regression : Using logistic regression : Making predictions 
# Title: Exploring modeling trade-offs 

library(ROCR)                                      	# Note: 1 
library(grid)                                      	# Note: 2 

predObj <- prediction(train$pred, train$atRisk)     	# Note: 3 
precObj <- performance(predObj, measure="prec")     	# Note: 4 
recObj <- performance(predObj, measure="rec")       	# Note: 5 

precision <- (precObj@y.values)[[1]]                	# Note: 6 
prec.x <- (precObj@x.values)[[1]]                   	# Note: 7 
recall <- (recObj@y.values)[[1]]

rocFrame <- data.frame(threshold=prec.x, precision=precision,
                      recall=recall)               	# Note: 8 

nplot <- function(plist) {                          	# Note: 9 
  n <- length(plist)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(n,1)))
  vplayout=function(x,y) {viewport(layout.pos.row=x, layout.pos.col=y)}
  for(i in 1:n) {
    print(plist[[i]], vp=vplayout(i,1))
  }
}

pnull <- mean(as.numeric(train$atRisk))             	# Note: 10 

p1 <- ggplot(rocFrame, aes(x=threshold)) +          	# Note: 11 
  geom_line(aes(y=precision/pnull)) +
  coord_cartesian(xlim = c(0,0.05), ylim=c(0,10) )

p2 <- ggplot(rocFrame, aes(x=threshold)) +          	# Note: 12 
  geom_line(aes(y=recall)) +
  coord_cartesian(xlim = c(0,0.05) )

nplot(list(p1, p2))                                	# Note: 13

# Note 1: 
#   Load ROCR library. 

# Note 2: 
#   Load grid library (you’ll need this for the 
#   nplot function below). 

# Note 3: 
#   Create ROCR prediction object. 

# Note 4: 
#   Create ROCR object to calculate precision as 
#   a function of threshold. 

# Note 5: 
#   Create ROCR object to calculate recall as a 
#   function of threshold. 

# Note 6: 
#   at ( @ ) symbol@ (at) symbolROCR objects are what R calls S4 objects; 
#   the slots (or fields) of an S4 object are stored 
#   as lists within the object. You extract the slots 
#   from an S4 object using @ notation. 

# Note 7: 
#   The x values (thresholds) are the same in 
#   both predObj and recObj, so you only need to 
#   extract them once. 

# Note 8: 
#   Build data frame with thresholds, precision, 
#   and recall. 

# Note 9: 
#   Function to plot multiple plots on one page 
#   (stacked). 

# Note 10: 
#   Calculate rate of at-risk births in the 
#   training set. 

# Note 11: 
#   Plot enrichment rate as a function of 
#   threshold. 

# Note 12: 
#   Plot recall as a function of 
#   threshold. 

# Note 13: 
#   Show both plots simultaneously. 

# example 7.13 of section 7.2.3 
# (example 7.13 of section 7.2.3)  : Linear and logistic regression : Using logistic regression : Making predictions 
# Title: Evaluating our chosen model 

ctab.test <- table(pred=test$pred>0.02, atRisk=test$atRisk) 	# Note: 1 
ctab.test                                                      	# Note: 2 
##        atRisk
## pred    FALSE TRUE
##   FALSE  9487   93
##   TRUE   2405  116
precision <- ctab.test[2,2]/sum(ctab.test[2,])
precision
## [1] 0.04601349
recall <- ctab.test[2,2]/sum(ctab.test[,2])
recall
## [1] 0.5550239
enrich <- precision/mean(as.numeric(test$atRisk))
enrich
## [1] 2.664159

# Note 1: 
#   Build confusion matrix. 

# Note 2: 
#   Rows contain predicted negatives and 
#   positives; columns contain actual negatives and 
#   positives. 

# example 7.14 of section 7.2.4 
# (example 7.14 of section 7.2.4)  : Linear and logistic regression : Using logistic regression : Finding relations and extracting advice from logistic models 
# Title: The model coefficients 

coefficients(model)
##              (Intercept)                     PWGT
##              -4.41218940               0.00376166
##                  UPREVIS              CIG_RECTRUE
##              -0.06328943               0.31316930
##       GESTREC3< 37 weeks DPLURALtriplet or higher
##               1.54518311               1.39419294
##              DPLURALtwin             ULD_MECOTRUE
##               0.31231871               0.81842627
##           ULD_PRECIPTRUE           ULD_BREECHTRUE
##               0.19172008               0.74923672
##             URF_DIABTRUE           URF_CHYPERTRUE
##              -0.34646672               0.56002503
##           URF_PHYPERTRUE            URF_ECLAMTRUE
##               0.16159872               0.49806435

# example 7.15 of section 7.2.5 
# (example 7.15 of section 7.2.5)  : Linear and logistic regression : Using logistic regression : Reading the model summary and characterizing coefficients 
# Title: The model summary 

summary(model)

## Call:
## glm(formula = fmla, family = binomial(link = "logit"), data = train)
## 
## Deviance Residuals:
##     Min       1Q   Median       3Q      Max
## -0.9732  -0.1818  -0.1511  -0.1358   3.2641
## 
## Coefficients:
##                           Estimate Std. Error z value Pr(>|z|)
## (Intercept)              -4.412189   0.289352 -15.249  < 2e-16 ***
## PWGT                      0.003762   0.001487   2.530 0.011417 *
## UPREVIS                  -0.063289   0.015252  -4.150 3.33e-05 ***
## CIG_RECTRUE               0.313169   0.187230   1.673 0.094398 .
## GESTREC3< 37 weeks        1.545183   0.140795  10.975  < 2e-16 ***
## DPLURALtriplet or higher  1.394193   0.498866   2.795 0.005194 **
## DPLURALtwin               0.312319   0.241088   1.295 0.195163
## ULD_MECOTRUE              0.818426   0.235798   3.471 0.000519 ***
## ULD_PRECIPTRUE            0.191720   0.357680   0.536 0.591951
## ULD_BREECHTRUE            0.749237   0.178129   4.206 2.60e-05 ***
## URF_DIABTRUE             -0.346467   0.287514  -1.205 0.228187
## URF_CHYPERTRUE            0.560025   0.389678   1.437 0.150676
## URF_PHYPERTRUE            0.161599   0.250003   0.646 0.518029
## URF_ECLAMTRUE             0.498064   0.776948   0.641 0.521489
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##    Null deviance: 2698.7  on 14211  degrees of freedom
## Residual deviance: 2463.0  on 14198  degrees of freedom
## AIC: 2491
## 
## Number of Fisher Scoring iterations: 7

# example 7.16 of section 7.2.5 
# (example 7.16 of section 7.2.5)  : Linear and logistic regression : Using logistic regression : Reading the model summary and characterizing coefficients 
# Title: Calculating deviance residuals 

pred <- predict(model, newdata=train, type="response") 	# Note: 1 
llcomponents <- function(y, py) {                      	# Note: 2 
  y*log(py) + (1-y)*log(1-py)
}

edev <- sign(as.numeric(train$atRisk) - pred) *        	# Note: 3 
  sqrt(-2*llcomponents(as.numeric(train$atRisk), pred))

summary(edev)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## -0.9732 -0.1818 -0.1511 -0.1244 -0.1358  3.2640

# Note 1: 
#   Create vector of predictions for training 
#   data. 

# Note 2: 
#   Function to return the log likelihoods for 
#   each data point. Argument y is the true outcome 
#   (as a numeric variable, 0/1); argument py is the 
#   predicted probability. 

# Note 3: 
#   Calculate deviance residuals. 

# example 7.17 of section 7.2.5 
# (example 7.17 of section 7.2.5)  : Linear and logistic regression : Using logistic regression : Reading the model summary and characterizing coefficients 
# Title: Computing deviance 

loglikelihood <- function(y, py) {                                 	# Note: 1 
  sum(y * log(py) + (1-y)*log(1 - py))
}

pnull <- mean(as.numeric(train$atRisk))                            	# Note: 2 
null.dev <- -2*loglikelihood(as.numeric(train$atRisk), pnull)      	# Note: 3 

pnull
## [1] 0.01920912
null.dev
## [1] 2698.716
model$null.deviance                                                 	# Note: 4 
## [1] 2698.716

pred <- predict(model, newdata=train, type="response")   	# Note: 5 
resid.dev <- -2*loglikelihood(as.numeric(train$atRisk), pred)      	# Note: 6 

resid.dev
## [1] 2462.992
model$deviance                                                      	# Note: 7 
## [1] 2462.992

testy <- as.numeric(test$atRisk)                                   	# Note: 8 
testpred <- predict(model, newdata=test,
                        type="response")
pnull.test <- mean(testy)
null.dev.test <- -2*loglikelihood(testy, pnull.test)
resid.dev.test <- -2*loglikelihood(testy, testpred)

pnull.test
## [1] 0.0172713
null.dev.test
## [1] 2110.91
resid.dev.test
## [1] 1947.094

# Note 1: 
#   Function to calculate the log likelihood of 
#   a dataset. Variable y is the outcome 
#   in numeric form (1 for positive examples, 0 for 
#   negative). Variable py is the 
#   predicted probability that 
#   y==1. 

# Note 2: 
#   Calculate rate of positive examples in 
#   dataset. 

# Note 3: 
#   Calculate null deviance. 

# Note 4: 
#   For training data, the null deviance is 
#   stored in the slot model$null.deviance. 

# Note 5: 
#   Predict probabilities for training 
#   data. 

# Note 6: 
#   Calculate deviance of model for training 
#   data. 

# Note 7: 
#   For training data, model deviance is stored 
#   in the slot model$deviance. 

# Note 8: 
#   Calculate null deviance and residual 
#   deviance for test data. 

# example 7.18 of section 7.2.5 
# (example 7.18 of section 7.2.5)  : Linear and logistic regression : Using logistic regression : Reading the model summary and characterizing coefficients 
# Title: Calculating the significance of the observed fit 

df.null <- dim(train)[[1]] - 1                            	# Note: 1 
df.model <- dim(train)[[1]] - length(model$coefficients)  	# Note: 2 

df.null
## [1] 14211
df.model
## [1] 14198

delDev <- null.dev - resid.dev                            	# Note: 3 
deldf <- df.null - df.model
p <- pchisq(delDev, deldf, lower.tail=F)                  	# Note: 4 

delDev
## [1] 235.724
deldf
## [1] 13
p
## [1] 5.84896e-43

# Note 1: 
#   Null model has (number of data points - 1) 
#   degrees of freedom. 

# Note 2: 
#   Fitted model has (number of data points - 
#   number of coefficients) degrees of freedom. 

# Note 3: 
#   Compute difference in deviances and 
#   difference in degrees of freedom. 

# Note 4: 
#   Estimate probability of seeing the observed 
#   difference in deviances under null model (the 
#   p-value) using chi-squared distribution. 

# example 7.19 of section 7.2.5 
# (example 7.19 of section 7.2.5)  : Linear and logistic regression : Using logistic regression : Reading the model summary and characterizing coefficients 
# Title: Calculating the pseudo R-squared 

pr2 <- 1-(resid.dev/null.dev)

print(pr2)
## [1] 0.08734674
pr2.test <- 1-(resid.dev.test/null.dev.test)
print(pr2.test)
## [1] 0.07760427

# example 7.20 of section 7.2.5 
# (example 7.20 of section 7.2.5)  : Linear and logistic regression : Using logistic regression : Reading the model summary and characterizing coefficients 
# Title: Calculating the Akaike information criterion 

aic <- 2*(length(model$coefficients) -
         loglikelihood(as.numeric(train$atRisk), pred))
aic
## [1] 2490.992

# example 8.1 of section 8.1.2 
# (example 8.1 of section 8.1.2)  : Unsupervised methods : Cluster analysis : Preparing the data 
# Title: Reading the protein data 

protein <- read.table("protein.txt", sep="\t", header=TRUE)
summary(protein)
##            Country      RedMeat         WhiteMeat           Eggs
##  Albania       : 1   Min.   : 4.400   Min.   : 1.400   Min.   :0.500
##  Austria       : 1   1st Qu.: 7.800   1st Qu.: 4.900   1st Qu.:2.700
##  Belgium       : 1   Median : 9.500   Median : 7.800   Median :2.900
##  Bulgaria      : 1   Mean   : 9.828   Mean   : 7.896   Mean   :2.936
##  Czechoslovakia: 1   3rd Qu.:10.600   3rd Qu.:10.800   3rd Qu.:3.700
##  Denmark       : 1   Max.   :18.000   Max.   :14.000   Max.   :4.700
##  (Other)       :19
##       Milk            Fish           Cereals          Starch
##  Min.   : 4.90   Min.   : 0.200   Min.   :18.60   Min.   :0.600
##  1st Qu.:11.10   1st Qu.: 2.100   1st Qu.:24.30   1st Qu.:3.100
##  Median :17.60   Median : 3.400   Median :28.00   Median :4.700
##  Mean   :17.11   Mean   : 4.284   Mean   :32.25   Mean   :4.276
##  3rd Qu.:23.30   3rd Qu.: 5.800   3rd Qu.:40.10   3rd Qu.:5.700
##  Max.   :33.70   Max.   :14.200   Max.   :56.70   Max.   :6.500
##
##       Nuts           Fr.Veg
##  Min.   :0.700   Min.   :1.400
##  1st Qu.:1.500   1st Qu.:2.900
##  Median :2.400   Median :3.800
##  Mean   :3.072   Mean   :4.136
##  3rd Qu.:4.700   3rd Qu.:4.900
##  Max.   :7.800   Max.   :7.900

# example 8.2 of section 8.1.2 
# (example 8.2 of section 8.1.2)  : Unsupervised methods : Cluster analysis : Preparing the data 
# Title: Rescaling the dataset 

vars.to.use <- colnames(protein)[-1]       	# Note: 1 
pmatrix <- scale(protein[,vars.to.use])    	# Note: 2 
pcenter <- attr(pmatrix, "scaled:center")  	# Note: 3 
pscale <- attr(pmatrix, "scaled:scale")

# Note 1: 
#   Use all the columns except the first 
#   (Country). 

# Note 2: 
#   The output of scale() is a matrix. For the 
#   purposes of this chapter, you can think of a 
#   matrix as a data frame with all numeric columns 
#   (this isn’t strictly true, but it’s close enough). 

# Note 3: 
#   The scale() function annotates its output 
#   with two attributes—scaled:center returns the mean 
#   values of all the columns, and scaled:scale 
#   returns the standard deviations. You’ll store 
#   these away so you can “unscale” the data 
#   later. 

# example 8.3 of section 8.1.3 
# (example 8.3 of section 8.1.3)  : Unsupervised methods : Cluster analysis : Hierarchical clustering with hclust 
# Title: Hierarchical clustering 

d <- dist(pmatrix, method="euclidean")   	# Note: 1 
pfit <- hclust(d, method="ward.D")         	# Note: 2 
plot(pfit, labels=protein$Country)      	# Note: 3

# Note 1: 
#   Create the distance matrix. 

# Note 2: 
#   Do the clustering. 

# Note 3: 
#   Plot the dendrogram. 

# informalexample 8.5 of section 8.1.3 
# (informalexample 8.5 of section 8.1.3)  : Unsupervised methods : Cluster analysis : Hierarchical clustering with hclust 

rect.hclust(pfit, k=5)

# example 8.4 of section 8.1.3 
# (example 8.4 of section 8.1.3)  : Unsupervised methods : Cluster analysis : Hierarchical clustering with hclust 
# Title: Extracting the clusters found by hclust() 

groups <- cutree(pfit, k=5)

print_clusters <- function(labels, k) {             	# Note: 1 
  for(i in 1:k) {
    print(paste("cluster", i))
    print(protein[labels==i,c("Country","RedMeat","Fish","Fr.Veg")])
  }
}

print_clusters(groups, 5)
## [1] "cluster 1"
##       Country RedMeat Fish Fr.Veg
## 1     Albania    10.1  0.2    1.7
## 4    Bulgaria     7.8  1.2    4.2
## 18    Romania     6.2  1.0    2.8
## 25 Yugoslavia     4.4  0.6    3.2
## [1] "cluster 2"
##        Country RedMeat Fish Fr.Veg
## 2      Austria     8.9  2.1    4.3
## 3      Belgium    13.5  4.5    4.0
## 9       France    18.0  5.7    6.5
## 12     Ireland    13.9  2.2    2.9
## 14 Netherlands     9.5  2.5    3.7
## 21 Switzerland    13.1  2.3    4.9
## 22          UK    17.4  4.3    3.3
## 24   W Germany    11.4  3.4    3.8
## [1] "cluster 3"
##           Country RedMeat Fish Fr.Veg
## 5  Czechoslovakia     9.7  2.0    4.0
## 7       E Germany     8.4  5.4    3.6
## 11        Hungary     5.3  0.3    4.2
## 16         Poland     6.9  3.0    6.6
## 23           USSR     9.3  3.0    2.9
## [1] "cluster 4"
##    Country RedMeat Fish Fr.Veg
## 6  Denmark    10.6  9.9    2.4
## 8  Finland     9.5  5.8    1.4
## 15  Norway     9.4  9.7    2.7
## 20  Sweden     9.9  7.5    2.0
## [1] "cluster 5"
##     Country RedMeat Fish Fr.Veg
## 10   Greece    10.2  5.9    6.5
## 13    Italy     9.0  3.4    6.7
## 17 Portugal     6.2 14.2    7.9
## 19    Spain     7.1  7.0    7.2

# Note 1: 
#   A convenience function for printing out the 
#   countries in each cluster, along with the values 
#   for red meat, fish, and fruit/vegetable 
#   consumption. We’ll use this function throughout 
#   this section. Note that the function is hardcoded 
#   for the protein dataset. 

# example 8.5 of section 8.1.3 
# (example 8.5 of section 8.1.3)  : Unsupervised methods : Cluster analysis : Hierarchical clustering with hclust 
# Title: Projecting the clusters on the first two principal components 

library(ggplot2)
princ <- prcomp(pmatrix)    	# Note: 1 
nComp <- 2
project <- predict(princ, newdata=pmatrix)[,1:nComp]      	# Note: 2 
project.plus <- cbind(as.data.frame(project),             	# Note: 3 
                     cluster=as.factor(groups),
                     country=protein$Country)
ggplot(project.plus, aes(x=PC1, y=PC2)) +                	# Note: 4 
  geom_point(aes(shape=cluster)) +
  geom_text(aes(label=country),
            hjust=0, vjust=1)

# Note 1: 
#   Calculate the principal components of the 
#   data. 

# Note 2: 
#   The predict() function will rotate the data 
#   into the space described by the principal 
#   components. We only want the projection on the 
#   first two axes. 

# Note 3: 
#   Create a data frame with the transformed 
#   data, along with the cluster label and country 
#   label of each point. 

# Note 4: 
#   Plot it. 

# example 8.6 of section 8.1.3 
# (example 8.6 of section 8.1.3)  : Unsupervised methods : Cluster analysis : Hierarchical clustering with hclust 
# Title: Running clusterboot() on the protein data 

library(fpc)                                  	# Note: 1 
kbest.p<-5                                                   	# Note: 2 
cboot.hclust <- clusterboot(pmatrix,clustermethod=hclustCBI, 	# Note: 3 
                           method="ward.D", k=kbest.p)

summary(cboot.hclust$result)                              	# Note: 4 
##               Length Class  Mode
## result         7     hclust list
## noise          1     -none- logical
## nc             1     -none- numeric
## clusterlist    5     -none- list
## partition     25     -none- numeric
## clustermethod  1     -none- character
## nccl           1     -none- numeric

groups<-cboot.hclust$result$partition                      	# Note: 5 
print_clusters(groups, kbest.p)                           	# Note: 6 
## [1] "cluster 1"
##       Country RedMeat Fish Fr.Veg
## 1     Albania    10.1  0.2    1.7
## 4    Bulgaria     7.8  1.2    4.2
## 18    Romania     6.2  1.0    2.8
## 25 Yugoslavia     4.4  0.6    3.2
## [1] "cluster 2"
##        Country RedMeat Fish Fr.Veg
## 2      Austria     8.9  2.1    4.3
## 3      Belgium    13.5  4.5    4.0
## 9       France    18.0  5.7    6.5
## 12     Ireland    13.9  2.2    2.9
## 14 Netherlands     9.5  2.5    3.7
## 21 Switzerland    13.1  2.3    4.9
## 22          UK    17.4  4.3    3.3
## 24   W Germany    11.4  3.4    3.8
## [1] "cluster 3"
##           Country RedMeat Fish Fr.Veg
## 5  Czechoslovakia     9.7  2.0    4.0
## 7       E Germany     8.4  5.4    3.6
## 11        Hungary     5.3  0.3    4.2
## 16         Poland     6.9  3.0    6.6
## 23           USSR     9.3  3.0    2.9
## [1] "cluster 4"
##    Country RedMeat Fish Fr.Veg
## 6  Denmark    10.6  9.9    2.4
## 8  Finland     9.5  5.8    1.4
## 15  Norway     9.4  9.7    2.7
## 20  Sweden     9.9  7.5    2.0
## [1] "cluster 5"
##     Country RedMeat Fish Fr.Veg
## 10   Greece    10.2  5.9    6.5
## 13    Italy     9.0  3.4    6.7
## 17 Portugal     6.2 14.2    7.9
## 19    Spain     7.1  7.0    7.2
cboot.hclust$bootmean                                   	# Note: 7 
## [1] 0.7905000 0.7990913 0.6173056 0.9312857 0.7560000
cboot.hclust$bootbrd                                    	# Note: 8 
## [1] 25 11 47  8 35

# Note 1: 
#   Load the fpc package. You may have to 
#   install it first. We’ll discuss installing R 
#   packages in appendix . 

# Note 2: 
#   Set the desired number of clusters. 

# Note 3: 
#   Run clusterboot() with hclust 
#   ('clustermethod=hclustCBI') using Ward’s method 
#   ('method="ward.D"') and kbest.p clusters 
#   ('k=kbest.p'). Return the results in an object 
#   called cboot.hclust. 

# Note 4: 
#   The results of the clustering are in 
#   cboot.hclust$result. The output of the hclust() 
#   function is in cboot.hclust$result$result. 

# Note 5: 
#   cboot.hclust$result$partition returns a 
#   vector of clusterlabels. 

# Note 6: 
#   The clusters are the same as those produced 
#   by a direct call to hclust(). 

# Note 7: 
#   The vector of cluster stabilities. 

# Note 8: 
#   The count of how many times each cluster was 
#   dissolved. By default clusterboot() runs 100 
#   bootstrap iterations. 

# example 8.7 of section 8.1.3 
# (example 8.7 of section 8.1.3)  : Unsupervised methods : Cluster analysis : Hierarchical clustering with hclust 
# Title: Calculating total within sum of squares 

sqr_edist <- function(x, y) {             	# Note: 1 
  sum((x-y)^2)
}

wss.cluster <- function(clustermat) {     	# Note: 2 
  c0 <- apply(clustermat, 2, FUN=mean)    	# Note: 3 
  sum(apply(clustermat, 1, FUN=function(row){sqr_edist(row,c0)}))     	# Note: 4 
}

wss.total <- function(dmatrix, labels) {                               	# Note: 5 
  wsstot <- 0
  k <- length(unique(labels))
  for(i in 1:k)
    wsstot <- wsstot + wss.cluster(subset(dmatrix, labels==i))         	# Note: 6 
  wsstot
}

# Note 1: 
#   Function to calculate squared distance 
#   between two vectors. 

# Note 2: 
#   Function to calculate the WSS for a single 
#   cluster, which is represented as a matrix (one row 
#   for every point). 

# Note 3: 
#   Calculate the centroid of the cluster (the 
#   mean of all the points). 

# Note 4: 
#   Calculate the squared difference of every 
#   point in the cluster from the centroid, and sum 
#   all the distances. 

# Note 5: 
#   Function to compute the total WSS from a set 
#   of data points and cluster labels. 

# Note 6: 
#   Extract each cluster, calculate the 
#   cluster’s WSS, and sum all the values. 

# example 8.8 of section 8.1.3 
# (example 8.8 of section 8.1.3)  : Unsupervised methods : Cluster analysis : Hierarchical clustering with hclust 
# Title: The Calinski-Harabasz index 

totss <- function(dmatrix) {                 	# Note: 1 
  grandmean <- apply(dmatrix, 2, FUN=mean)
  sum(apply(dmatrix, 1, FUN=function(row){sqr_edist(row, grandmean)}))
}


ch_criterion <- function(dmatrix, kmax, method="kmeans") {     	# Note: 2 
  if(!(method %in% c("kmeans", "hclust"))) {
    stop("method must be one of c('kmeans', 'hclust')")
  }
  npts <- dim(dmatrix)[1]  # number of rows.

  totss <- totss(dmatrix)                                       	# Note: 3 

  wss <- numeric(kmax)
  crit <- numeric(kmax)
  wss[1] <- (npts-1)*sum(apply(dmatrix, 2, var))                	# Note: 4 
  for(k in 2:kmax) {                                           	# Note: 5 
    if(method=="kmeans") {
      clustering<-kmeans(dmatrix, k, nstart=10, iter.max=100)
      wss[k] <- clustering$tot.withinss
    }else {  # hclust                                          	# Note: 6 
      d <- dist(dmatrix, method="euclidean")
      pfit <- hclust(d, method="ward.D")
      labels <- cutree(pfit, k=k)
      wss[k] <- wss.total(dmatrix, labels)
    }
  }
  bss <- totss - wss                                            	# Note: 7 
  crit.num <- bss/(0:(kmax-1))                                  	# Note: 8 
  crit.denom <- wss/(npts - 1:kmax)                             	# Note: 9 
  list(crit = crit.num/crit.denom, wss = wss, totss = totss)   	# Note: 10 
}

# Note 1: 
#   Convenience function to calculate the total 
#   sum of squares. 

# Note 2: 
#   A function to calculate the CH index for a 
#   number of clusters from 1 to kmax. 

# Note 3: 
#   The total sum of squares is independent of 
#   the clustering. 

# Note 4: 
#   Calculate WSS for k=1 (which is really just 
#   total sum of squares). 

# Note 5: 
#   Calculate WSS for k from 2 to kmax. kmeans() 
#   returns the total WSS as one of its 
#   outputs. 

# Note 6: 
#   For hclust(), calculate total WSS by 
#   hand. 

# Note 7: 
#   Calculate BSS for k from 1 to kmax. 

# Note 8: 
#   Normalize BSS by k-1. 

# Note 9: 
#   Normalize WSS by npts - k. 

# Note 10: 
#   Return a vector of CH indices and of WSS for 
#   k from 1 to kmax. Also return total sum of 
#   squares. 

# example 8.9 of section 8.1.3 
# (example 8.9 of section 8.1.3)  : Unsupervised methods : Cluster analysis : Hierarchical clustering with hclust 
# Title: Evaluating clusterings with different numbers of clusters 

library(reshape2)                                         	# Note: 1 
clustcrit <- ch_criterion(pmatrix, 10, method="hclust")     	# Note: 2 
critframe <- data.frame(k=1:10, ch=scale(clustcrit$crit),   	# Note: 3 
                       wss=scale(clustcrit$wss))
critframe <- melt(critframe, id.vars=c("k"),                	# Note: 4 
                 variable.name="measure",
                 value.name="score")
ggplot(critframe, aes(x=k, y=score, color=measure)) +     	# Note: 5 
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
  scale_x_continuous(breaks=1:10, labels=1:10)

# Note 1: 
#   Load the reshape2 package (for the melt() 
#   function). 

# Note 2: 
#   Calculate both criteria for 1–10 
#   clusters. 

# Note 3: 
#   Create a data frame with the number of 
#   clusters, the CH criterion, and the WSS criterion. 
#   We’ll scale both the CH and WSS criteria to 
#   similar ranges so that we can plot them both on 
#   the same graph. 

# Note 4: 
#   Use the melt() function to put the data 
#   frame in a shape suitable for ggplot 

# Note 5: 
#   Plot it. 

# example 8.10 of section 8.1.4 
# (example 8.10 of section 8.1.4)  : Unsupervised methods : Cluster analysis : The k-means algorithm 
# Title: Running k-means with k=5 

pclusters <- kmeans(pmatrix, kbest.p, nstart=100, iter.max=100)   	# Note: 1 
summary(pclusters)                                               	# Note: 2 
##              Length Class  Mode
## cluster      25     -none- numeric
## centers      45     -none- numeric
## totss         1     -none- numeric
## withinss      5     -none- numeric
## tot.withinss  1     -none- numeric
## betweenss     1     -none- numeric
## size          5     -none- numeric

pclusters$centers                                                	# Note: 3 
##        RedMeat  WhiteMeat        Eggs       Milk       Fish
## 1 -0.807569986 -0.8719354 -1.55330561 -1.0783324 -1.0386379
## 2  0.006572897 -0.2290150  0.19147892  1.3458748  1.1582546
## 3 -0.570049402  0.5803879 -0.08589708 -0.4604938 -0.4537795
## 4  1.011180399  0.7421332  0.94084150  0.5700581 -0.2671539
## 5 -0.508801956 -1.1088009 -0.41248496 -0.8320414  0.9819154
##      Cereals     Starch       Nuts      Fr.Veg
## 1  1.7200335 -1.4234267  0.9961313 -0.64360439
## 2 -0.8722721  0.1676780 -0.9553392 -1.11480485
## 3  0.3181839  0.7857609 -0.2679180  0.06873983
## 4 -0.6877583  0.2288743 -0.5083895  0.02161979
## 5  0.1300253 -0.1842010  1.3108846  1.62924487
pclusters$size                                                  	# Note: 4 
## [1] 4 4 5 8 4

groups <- pclusters$cluster                                      	# Note: 5 
print_clusters(groups, kbest.p)                                 	# Note: 6 
## [1] "cluster 1"
##       Country RedMeat Fish Fr.Veg
## 1     Albania    10.1  0.2    1.7
## 4    Bulgaria     7.8  1.2    4.2
## 18    Romania     6.2  1.0    2.8
## 25 Yugoslavia     4.4  0.6    3.2
## [1] "cluster 2"
##    Country RedMeat Fish Fr.Veg
## 6  Denmark    10.6  9.9    2.4
## 8  Finland     9.5  5.8    1.4
## 15  Norway     9.4  9.7    2.7
## 20  Sweden     9.9  7.5    2.0
## [1] "cluster 3"
##           Country RedMeat Fish Fr.Veg
## 5  Czechoslovakia     9.7  2.0    4.0
## 7       E Germany     8.4  5.4    3.6
## 11        Hungary     5.3  0.3    4.2
## 16         Poland     6.9  3.0    6.6
## 23           USSR     9.3  3.0    2.9
## [1] "cluster 4"
##        Country RedMeat Fish Fr.Veg
## 2      Austria     8.9  2.1    4.3
## 3      Belgium    13.5  4.5    4.0
## 9       France    18.0  5.7    6.5
## 12     Ireland    13.9  2.2    2.9
## 14 Netherlands     9.5  2.5    3.7
## 21 Switzerland    13.1  2.3    4.9
## 22          UK    17.4  4.3    3.3
## 24   W Germany    11.4  3.4    3.8
## [1] "cluster 5"
##     Country RedMeat Fish Fr.Veg
## 10   Greece    10.2  5.9    6.5
## 13    Italy     9.0  3.4    6.7
## 17 Portugal     6.2 14.2    7.9
## 19    Spain     7.1  7.0    7.2

# Note 1: 
#   Run kmeans() with five clusters (kbest.p=5), 
#   100 random starts, and 100 maximum iterations per 
#   run. 

# Note 2: 
#   kmeans() returns all the sum of squares 
#   measures. 

# Note 3: 
#   pclusters$centers is a matrix whose rows are 
#   the centroids of the clusters. Note that 
#   pclusters$centers is in the scaled coordinates, 
#   not the original protein coordinates. 

# Note 4: 
#   pclusters$size returns the number of points 
#   in each cluster. Generally (though not always) a 
#   good clustering will be fairly well balanced: no 
#   extremely small clusters and no extremely large 
#   ones. 

# Note 5: 
#   pclusters$cluster is a vector of cluster 
#   labels. 

# Note 6: 
#   In this case, kmeans() and hclust() returned 
#   the same clustering. This won’t always be 
#   true. 

# example 8.11 of section 8.1.4 
# (example 8.11 of section 8.1.4)  : Unsupervised methods : Cluster analysis : The k-means algorithm 
# Title: Plotting cluster criteria 

clustering.ch <- kmeansruns(pmatrix, krange=1:10, criterion="ch")   	# Note: 1 
clustering.ch$bestk                                                	# Note: 2 
##  [1] 2
clustering.asw <- kmeansruns(pmatrix, krange=1:10, criterion="asw") 	# Note: 3 
clustering.asw$bestk
##  [1] 3

clustering.ch$crit                                                 	# Note: 4 
##  [1]  0.000000 14.094814 11.417985 10.418801 10.011797  9.964967
##  [7]  9.861682  9.412089  9.166676  9.075569
clustcrit$crit                                                     	# Note: 5 
##  [1]       NaN 12.215107 10.359587  9.690891 10.011797  9.964967
##  [7]  9.506978  9.092065  8.822406  8.695065

critframe <- data.frame(k=1:10, ch=scale(clustering.ch$crit),        	# Note: 6 
             asw=scale(clustering.asw$crit))
critframe <- melt(critframe, id.vars=c("k"),
                 variable.name="measure",
                  value.name="score")
ggplot(critframe, aes(x=k, y=score, color=measure)) +
   geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
   scale_x_continuous(breaks=1:10, labels=1:10)
summary(clustering.ch)                                            	# Note: 7 
##              Length Class  Mode
## cluster      25     -none- numeric
## centers      18     -none- numeric
## totss         1     -none- numeric
## withinss      2     -none- numeric
## tot.withinss  1     -none- numeric
## betweenss     1     -none- numeric
## size          2     -none- numeric
## crit         10     -none- numeric
## bestk         1     -none- numeric

# Note 1: 
#   Run kmeansruns() from 1–10 clusters, and the 
#   CH criterion. By default, kmeansruns() uses 100 
#   random starts and 100 maximum iterations per 
#   run. 

# Note 2: 
#   The CH criterion picks two clusters. 

# Note 3: 
#   Run kmeansruns() from 1–10 clusters, and the 
#   average silhouette width criterion. Average 
#   silhouette width picks 3 clusters. 

# Note 4: 
#   The vector of criterion values is called 
#   crit. 

# Note 5: 
#   Compare the CH values for kmeans() and 
#   hclust(). They’re not quite the same, because the 
#   two algorithms didn’t pick the same 
#   clusters. 

# Note 6: 
#   Plot the values for the two criteria. 

# Note 7: 
#   kmeansruns() also returns the output of 
#   kmeans for k=bestk. 

# example 8.12 of section 8.1.4 
# (example 8.12 of section 8.1.4)  : Unsupervised methods : Cluster analysis : The k-means algorithm 
# Title: Running clusterboot() with k-means 

kbest.p<-5
cboot<-clusterboot(pmatrix, clustermethod=kmeansCBI,
            runs=100,iter.max=100,
            krange=kbest.p, seed=15555)               	# Note: 1 

groups <- cboot$result$partition
print_clusters(cboot$result$partition, kbest.p)
## [1] "cluster 1"
##       Country RedMeat Fish Fr.Veg
## 1     Albania    10.1  0.2    1.7
## 4    Bulgaria     7.8  1.2    4.2
## 18    Romania     6.2  1.0    2.8
## 25 Yugoslavia     4.4  0.6    3.2
## [1] "cluster 2"
##    Country RedMeat Fish Fr.Veg
## 6  Denmark    10.6  9.9    2.4
## 8  Finland     9.5  5.8    1.4
## 15  Norway     9.4  9.7    2.7
## 20  Sweden     9.9  7.5    2.0
## [1] "cluster 3"
##           Country RedMeat Fish Fr.Veg
## 5  Czechoslovakia     9.7  2.0    4.0
## 7       E Germany     8.4  5.4    3.6
## 11        Hungary     5.3  0.3    4.2
## 16         Poland     6.9  3.0    6.6
## 23           USSR     9.3  3.0    2.9
## [1] "cluster 4"
##        Country RedMeat Fish Fr.Veg
## 2      Austria     8.9  2.1    4.3
## 3      Belgium    13.5  4.5    4.0
## 9       France    18.0  5.7    6.5
## 12     Ireland    13.9  2.2    2.9
## 14 Netherlands     9.5  2.5    3.7
## 21 Switzerland    13.1  2.3    4.9
## 22          UK    17.4  4.3    3.3
## 24   W Germany    11.4  3.4    3.8
## [1] "cluster 5"
##    Country RedMeat Fish Fr.Veg
## 10   Greece    10.2  5.9    6.5
## 13    Italy     9.0  3.4    6.7
## 17 Portugal     6.2 14.2    7.9
## 19    Spain     7.1  7.0    7.2
cboot$bootmean
## [1] 0.8670000 0.8420714 0.6147024 0.7647341 0.7508333
cboot$bootbrd
## [1] 15 20 49 17 32

# Note 1: 
#   We’ve set the seed for the random generator 
#   so the results are reproducible. 

# example 8.13 of section 8.1.5 
# (example 8.13 of section 8.1.5)  : Unsupervised methods : Cluster analysis : Assigning new points to clusters 
# Title: A function to assign points to a cluster 

assign_cluster <- function(newpt, centers, xcenter=0, xscale=1) { 	# Note: 1 
   xpt <- (newpt - xcenter)/xscale                                	# Note: 2 
   dists <- apply(centers, 1, FUN=function(c0){sqr_edist(c0, xpt)})  	# Note: 3 
   which.min(dists)                                                 	# Note: 4 
 }

# Note 1: 
#   A function to assign a new data point newpt to 
#   a clustering described by centers, a matrix where 
#   each row is a cluster centroid. If the data was 
#   scaled (using scale()) before clustering, then 
#   xcenter and xscale are the scaled:center and 
#   scaled:scale attributes, respectively. 

# Note 2: 
#   Center and scale the new data point. 

# Note 3: 
#   Calculate how far the new data point is from 
#   each of the cluster centers. 

# Note 4: 
#   Return the cluster number of the closest 
#   centroid. 

# example 8.14 of section 8.1.5 
# (example 8.14 of section 8.1.5)  : Unsupervised methods : Cluster analysis : Assigning new points to clusters 
# Title: An example of assigning points to cluster 

rnorm.multidim <- function(n, mean, sd, colstr="x") {    	# Note: 1 
   ndim <- length(mean)
   data <- NULL
   for(i in 1:ndim) {
     col <- rnorm(n, mean=mean[[i]], sd=sd[[i]])
     data<-cbind(data, col)
   }
   cnames <- paste(colstr, 1:ndim, sep='')
   colnames(data) <- cnames
   data
 }

mean1 <- c(1, 1, 1)                    	# Note: 2 
sd1 <- c(1, 2, 1)

mean2 <- c(10, -3, 5)
sd2 <- c(2, 1, 2)

mean3 <- c(-5, -5, -5)
sd3 <- c(1.5, 2, 1)

clust1 <- rnorm.multidim(100, mean1, sd1)           	# Note: 3 
clust2 <- rnorm.multidim(100, mean2, sd2)
clust3 <- rnorm.multidim(100, mean3, sd3)
toydata <- rbind(clust3, rbind(clust1, clust2))

tmatrix <- scale(toydata)                          	# Note: 4 
tcenter <- attr(tmatrix, "scaled:center")        	# Note: 5 
tscale<-attr(tmatrix, "scaled:scale")
kbest.t <- 3
tclusters <- kmeans(tmatrix, kbest.t, nstart=100, iter.max=100)   	# Note: 6 

tclusters$size              	# Note: 7 
## [1] 100 101  99

unscale <- function(scaledpt, centervec, scalevec) {    	# Note: 8 
   scaledpt*scalevec + centervec
}

unscale(tclusters$centers[1,], tcenter, tscale)   	# Note: 9 
##        x1        x2        x3
##  9.978961 -3.097584  4.864689
mean2
## [1] 10 -3  5

unscale(tclusters$centers[2,], tcenter, tscale)   	# Note: 10 
##        x1        x2        x3
## -4.979523 -4.927404 -4.908949
mean3
## [1] -5 -5 -5

unscale(tclusters$centers[3,], tcenter, tscale)    	# Note: 11 
##        x1        x2        x3
## 1.0003356 1.3037825 0.9571058
mean1
## [1] 1 1 1

assign_cluster(rnorm.multidim(1, mean1, sd1),  	# Note: 12 
                tclusters$centers,
                tcenter, tscale)
## 3                                                 	# Note: 13 
## 3

assign_cluster(rnorm.multidim(1, mean2, sd1),   	# Note: 14 
                tclusters$centers,
                tcenter, tscale)
## 1                                                	# Note: 15 
## 1

assign_cluster(rnorm.multidim(1, mean3, sd1),     	# Note: 16 
                tclusters$centers,
                tcenter, tscale)
## 2                                          	# Note: 17 
## 2

# Note 1: 
#   A function to generate n points drawn from a 
#   multidimensional Gaussian distribution with 
#   centroid mean and standard deviation sd. The 
#   dimension of the distribution is given by the 
#   length of the vector mean. 

# Note 2: 
#   The parameters for three Gaussian 
#   distributions. 

# Note 3: 
#   Create a dataset with 100 points each drawn 
#   from the above distributions. 

# Note 4: 
#   Scale the dataset. 

# Note 5: 
#   Store the centering and scaling parameters for 
#   future use. 

# Note 6: 
#   Cluster the dataset, using k-means with three 
#   clusters. 

# Note 7: 
#   The resulting clusters are about the right 
#   size. 

# Note 8: 
#   A function to “unscale” data points (put them 
#   back in the coordinates of the original 
#   dataset). 

# Note 9: 
#   Unscale the first centroid. It corresponds to 
#   our original distribution 2. 

# Note 10: 
#   The second centroid corresponds to the 
#   original distribution 3. 

# Note 11: 
#   The third centroid corresponds to the original 
#   distribution 1. 

# Note 12: 
#   Generate a random point from the original 
#   distribution 1 and assign it to one of the 
#   discovered clusters. 

# Note 13: 
#   It’s assigned to cluster 3, as we would 
#   expect. 

# Note 14: 
#   Generate a random point from the original 
#   distribution 2 and assign it. 

# Note 15: 
#   It’s assigned to cluster 1. 

# Note 16: 
#   Generate a random point from the original 
#   distribution 3 and assign it. 

# Note 17: 
#   It’s assigned to cluster 2. 

# example 8.15 of section 8.2.3 
# (example 8.15 of section 8.2.3)  : Unsupervised methods : Association rules : Mining association rules with the arules package 
# Title: Reading in the book data 

library(arules)  	# Note: 1 
bookbaskets <- read.transactions("bookdata.tsv.gz", format="single",  	# Note: 2 
                                 sep="\t",                    	# Note: 3 
                                 cols=c("userid", "title"),    	# Note: 4 
                                 rm.duplicates=T)       	# Note: 5

# Note 1: 
#   Load the arules package. 

# Note 2: 
#   Specify the file and the file format. 

# Note 3: 
#   Specify the column separator (a tab). 

# Note 4: 
#   Specify the column of transaction IDs and of 
#   item IDs, respectively. 

# Note 5: 
#   Tell the function to look for and remove 
#   duplicate entries (for example, multiple entries 
#   for “The Hobbit” by the same user). 

# example 8.16 of section 8.2.3 
# (example 8.16 of section 8.2.3)  : Unsupervised methods : Association rules : Mining association rules with the arules package 
# Title: Examining the transaction data 

class(bookbaskets)             	# Note: 1 
## [1] "transactions"
## attr(,"package")
## [1] "arules"
bookbaskets                    	# Note: 2 
## transactions in sparse format with
##  92108 transactions (rows) and
##  220447 items (columns)
dim(bookbaskets)               	# Note: 3 
## [1]  92108 220447
colnames(bookbaskets)[1:5]     	# Note: 4 
## [1] " A Light in the Storm:[...]"
## [2] " Always Have Popsicles"
## [3] " Apple Magic"
## [4] " Ask Lily"
## [5] " Beyond IBM: Leadership Marketing and Finance for the 1990s"
rownames(bookbaskets)[1:5]        	# Note: 5 
## [1] "10"     "1000"   "100001" "100002" "100004"

# Note 1: 
#   The object is of class transactions. 

# Note 2: 
#   Printing the object tells you its 
#   dimensions. 

# Note 3: 
#   You can also use dim() to see the dimensions 
#   of the matrix. 

# Note 4: 
#   The columns are labeled by book 
#   title. 

# Note 5: 
#   The rows are labeled by customer. 

# informalexample 8.7 of section 8.2.3 
# (informalexample 8.7 of section 8.2.3)  : Unsupervised methods : Association rules : Mining association rules with the arules package 

basketSizes <- size(bookbaskets)
summary(basketSizes)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##     1.0     1.0     1.0    11.1     4.0 10250.0

# example 8.17 of section 8.2.3 
# (example 8.17 of section 8.2.3)  : Unsupervised methods : Association rules : Mining association rules with the arules package 
# Title: Examining the size distribution 

quantile(basketSizes, probs=seq(0,1,0.1))     	# Note: 1 
##    0%   10%   20%   30%   40%   50%   60%   70%   80%   90%  100%
##     1     1     1     1     1     1     2     3     5    13 10253
library(ggplot2)                              	# Note: 2 
ggplot(data.frame(count=basketSizes)) +
  geom_density(aes(x=count), binwidth=1) +
  scale_x_log10()

# Note 1: 
#   Look at the basket size distribution, in 10% 
#   increments. 

# Note 2: 
#   Plot the distribution to get a better 
#   look. 

# informalexample 8.8 of section 8.2.3 
# (informalexample 8.8 of section 8.2.3)  : Unsupervised methods : Association rules : Mining association rules with the arules package 

bookFreq <- itemFrequency(bookbaskets)
## summary(bookFreq)
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
## 1.086e-05 1.086e-05 1.086e-05 5.035e-05 3.257e-05 2.716e-02

sum(bookFreq)
## [1] 11.09909

# example 8.18 of section 8.2.3 
# (example 8.18 of section 8.2.3)  : Unsupervised methods : Association rules : Mining association rules with the arules package 
# Title: Finding the ten most frequent books 

bookCount <- (bookFreq/sum(bookFreq))*sum(basketSizes)     	# Note: 1 
summary(bookCount)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
##    1.000    1.000    1.000    4.637    3.000 2502.000
orderedBooks <- sort(bookCount, decreasing=T)   	# Note: 2 
orderedBooks[1:10]
##                                     Wild Animus
##                                            2502
##                       The Lovely Bones: A Novel
##                                            1295
##                               She's Come Undone
##                                             934
##                               The Da Vinci Code
##                                             905
##           Harry Potter and the Sorcerer's Stone
##                                             832
##                      The Nanny Diaries: A Novel
##                                             821
##                                 A Painted House
##                                             819
##                           Bridget Jones's Diary
##                                             772
##                         The Secret Life of Bees
##                                             762
## Divine Secrets of the Ya-Ya Sisterhood: A Novel
##                                             737
orderedBooks[1]/dim(bookbaskets)[1]                 	# Note: 3 
## Wild Animus
##  0.02716376

# Note 1: 
#   Get the absolute count of book 
#   occurrences. 

# Note 2: 
#   Sort the count and list the 10 most popular 
#   books. 

# Note 3: 
#   The most popular book in the dataset 
#   occurred in fewer than 3% of the baskets. 

# informalexample 8.9 of section 8.2.3 
# (informalexample 8.9 of section 8.2.3)  : Unsupervised methods : Association rules : Mining association rules with the arules package 

bookbaskets_use <- bookbaskets[basketSizes > 1]
dim(bookbaskets_use)
## [1]  40822 220447

# example 8.19 of section 8.2.3 
# (example 8.19 of section 8.2.3)  : Unsupervised methods : Association rules : Mining association rules with the arules package 
# Title: Finding the association rules 

rules <- apriori(bookbaskets_use,                                  	# Note: 1 
                parameter =list(support = 0.002, confidence=0.75))

summary(rules)
## set of 191 rules                            	# Note: 2 
##
## rule length distribution (lhs + rhs):sizes         	# Note: 3 
##   2   3   4   5
##  11 100  66  14
##
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##   2.000   3.000   3.000   3.435   4.000   5.000
##
## summary of quality measures:                           	# Note: 4 
##     support           confidence          lift
##  Min.   :0.002009   Min.   :0.7500   Min.   : 40.89
##  1st Qu.:0.002131   1st Qu.:0.8113   1st Qu.: 86.44
##  Median :0.002278   Median :0.8468   Median :131.36
##  Mean   :0.002593   Mean   :0.8569   Mean   :129.68
##  3rd Qu.:0.002695   3rd Qu.:0.9065   3rd Qu.:158.77
##  Max.   :0.005830   Max.   :0.9882   Max.   :321.89
##
## mining info:                                           	# Note: 5 
##             data ntransactions support confidence
##  bookbaskets_use         40822   0.002       0.75

# Note 1: 
#   Call apriori() with a minimum support of 
#   0.002 and a minimum confidence of 0.75. 

# Note 2: 
#   The summary of the apriori() output reports 
#   the number of rules found;... 

# Note 3: 
#   ...the distribution of rule lengths (in this 
#   example, most rules contain 3 items—2 on the left 
#   side, X (lhs), and one on the right side, Y 
#   (rhs));... 

# Note 4: 
#   ...a summary of rule quality measures, 
#   including support and confidence;... 

# Note 5: 
#   ...and some information on how apriori() was 
#   called. 

# example 8.20 of section 8.2.3 
# (example 8.20 of section 8.2.3)  : Unsupervised methods : Association rules : Mining association rules with the arules package 
# Title: Scoring rules 

measures <- interestMeasure(rules,                            	# Note: 1 
                 measure=c("coverage", "fishersExactTest"),    	# Note: 2 
                 transactions=bookbaskets_use)                	# Note: 3 
summary(measures)
##     coverage        fishersExactTest
##  Min.   :0.002082   Min.   : 0.000e+00
##  1st Qu.:0.002511   1st Qu.: 0.000e+00
##  Median :0.002719   Median : 0.000e+00
##  Mean   :0.003039   Mean   :5.080e-138
##  3rd Qu.:0.003160   3rd Qu.: 0.000e+00
##  Max.   :0.006982   Max.   :9.702e-136

# Note 1: 
#   The call to interestMeasure() takes as 
#   arguments the discovered rules,... 

# Note 2: 
#   ...a list of interest measures to 
#   apply,... 

# Note 3: 
#   ...and a dataset to evaluate the interest 
#   measures over. This is usually the same set used 
#   to mine the rules, but it needn’t be. For 
#   instance, you can evaluate the rules over the full 
#   dataset, bookbaskets, to get coverage estimates 
#   that reflect all the customers, not just the ones 
#   who showed interest in more than one book. 

# informalexample 8.10 of section 8.2.3 
# (informalexample 8.10 of section 8.2.3)  : Unsupervised methods : Association rules : Mining association rules with the arules package 

inspect(head((sort(rules, by="confidence")), n=5))

# example 8.21 of section 8.2.3 
# (example 8.21 of section 8.2.3)  : Unsupervised methods : Association rules : Mining association rules with the arules package 
# Title: Finding rules with restrictions 

brules <- apriori(bookbaskets_use,
                parameter =list(support = 0.001,    	# Note: 1 
                                confidence=0.6),
                appearance=list(rhs=c("The Lovely Bones: A Novel"),  	# Note: 2 
                                default="lhs"))                      	# Note: 3 
summary(brules)
## set of 46 rules
##
## rule length distribution (lhs + rhs):sizes
##  3  4
## 44  2
##
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##   3.000   3.000   3.000   3.043   3.000   4.000
##
## summary of quality measures:
##     support           confidence          lift
##  Min.   :0.001004   Min.   :0.6000   Min.   :21.81
##  1st Qu.:0.001029   1st Qu.:0.6118   1st Qu.:22.24
##  Median :0.001102   Median :0.6258   Median :22.75
##  Mean   :0.001132   Mean   :0.6365   Mean   :23.14
##  3rd Qu.:0.001219   3rd Qu.:0.6457   3rd Qu.:23.47
##  Max.   :0.001396   Max.   :0.7455   Max.   :27.10
##
## mining info:
##             data ntransactions support confidence
##  bookbaskets_use         40822   0.001        0.6

# Note 1: 
#   Relax the minimum support to 0.001 and the 
#   minimum confidence to 0.6. 

# Note 2: 
#   Only The Lovely Bones 
#   is allowed to appear on the right side of the 
#   rules. 

# Note 3: 
#   By default, all the books can go into the 
#   left side of the rules. 

# example 8.22 of section 8.2.3 
# (example 8.22 of section 8.2.3)  : Unsupervised methods : Association rules : Mining association rules with the arules package 
# Title: Inspecting rules 

brulesConf <- sort(brules, by="confidence")  	# Note: 1 

inspect(head(lhs(brulesConf), n=5))      	# Note: 2 
##   items
## 1 {Divine Secrets of the Ya-Ya Sisterhood: A Novel,
##    Lucky : A Memoir}
## 2 {Lucky : A Memoir,
##    The Notebook}
## 3 {Lucky : A Memoir,
##    Wild Animus}
## 4 {Midwives: A Novel,
##    Wicked: The Life and Times of the Wicked Witch of the West}
## 5 {Lucky : A Memoir,
##    Summer Sisters}

# Note 1: 
#   Sort the rules by confidence. 

# Note 2: 
#   Use the lhs() function to get the left 
#   itemsets of each rule; then inspect the top 
#   five. 

# example 8.23 of section 8.2.3 
# (example 8.23 of section 8.2.3)  : Unsupervised methods : Association rules : Mining association rules with the arules package 
# Title: Inspecting rules with restrictions 

brulesSub <- subset(brules, subset=!(lhs %in% "Lucky : A Memoir"))  	# Note: 1 
brulesConf <- sort(brulesSub, by="confidence")

inspect(head(lhs(brulesConf), n=5))
##   items
## 1 {Midwives: A Novel,
##    Wicked: The Life and Times of the Wicked Witch of the West}
## 2 {She's Come Undone,
##    The Secret Life of Bees,
##    Wild Animus}
## 3 {A Walk to Remember,
##    The Nanny Diaries: A Novel}
## 4 {Beloved,
##    The Red Tent}
## 5 {The Da Vinci Code,
##    The Reader}

# Note 1: 
#   Restrict to the subset of rules where 
#   Lucky is not in the left 
#   side. 

# example 9.1 of section 9.1.1 
# (example 9.1 of section 9.1.1)  : Exploring advanced methods : Using bagging and random forests to reduce training variance : Using bagging to improve prediction 
# Title: Preparing Spambase data and evaluating the performance of decision trees 

spamD <- read.table('spamD.tsv',header=T,sep='\t')  	# Note: 1 
spamTrain <- subset(spamD,spamD$rgroup>=10)
spamTest <- subset(spamD,spamD$rgroup<10)

spamVars <- setdiff(colnames(spamD),list('rgroup','spam'))
spamFormula <- as.formula(paste('spam=="spam"',   	# Note: 2 
                          paste(spamVars,collapse=' + '),sep=' ~ '))

loglikelihood <- function(y, py) {   	# Note: 3 
  pysmooth <- ifelse(py==0, 1e-12,
                  ifelse(py==1, 1-1e-12, py))

  sum(y * log(pysmooth) + (1-y)*log(1 - pysmooth))
}


accuracyMeasures <- function(pred, truth, name="model") {  	# Note: 4 
  dev.norm <- -2*loglikelihood(as.numeric(truth), pred)/length(pred)  	# Note: 5 
  ctable <- table(truth=truth,
                 pred=(pred>0.5))                                    	# Note: 6 
  accuracy <- sum(diag(ctable))/sum(ctable)
  precision <- ctable[2,2]/sum(ctable[,2])
  recall <- ctable[2,2]/sum(ctable[2,])
  f1 <- 2*precision*recall/(precision+recall)
  data.frame(model=name, accuracy=accuracy, f1=f1, dev.norm)
}


library(rpart)                                                  	# Note: 7 
treemodel <- rpart(spamFormula, spamTrain)

accuracyMeasures(predict(treemodel, newdata=spamTrain),  	# Note: 8 
                 spamTrain$spam=="spam",
                 name="tree, training")


accuracyMeasures(predict(treemodel, newdata=spamTest),
                 spamTest$spam=="spam",
                 name="tree, test")

# Note 1: 
#   Load the data and split into training (90% of data) 
#   and test (10% of data) sets. 

# Note 2: 
#   Use all the features and do binary classification, 
#   where TRUE corresponds to spam documents. 

# Note 3: 
#   A function to calculate log likelihood (for 
#   calculating deviance). 

# Note 4: 
#   A function to calculate and return various measures 
#   on the model: normalized deviance, prediction accuracy, and f1, which is the 
#   harmonic mean of precision and recall. 

# Note 5: 
#   Normalize the deviance by the number of data points 
#   so that we can compare the deviance across training and test 
#   sets. 

# Note 6: 
#   Convert the class probability estimator into a 
#   classifier by labeling documents that score greater than 0.5 as 
#   spam. 

# Note 7: 
#   Load the rpart library and fit a decision tree 
#   model. 

# Note 8: 
#   Evaluate the decision tree model against the 
#   training and test sets. 

# example 9.2 of section 9.1.1 
# (example 9.2 of section 9.1.1)  : Exploring advanced methods : Using bagging and random forests to reduce training variance : Using bagging to improve prediction 
# Title: Bagging decision trees 

ntrain <- dim(spamTrain)[1]
n <- ntrain                  	# Note: 1 
ntree <- 100

samples <- sapply(1:ntree,      	# Note: 2 
                 FUN = function(iter)
                   {sample(1:ntrain, size=n, replace=T)})

treelist <-lapply(1:ntree,       	# Note: 3 
                  FUN=function(iter)
                  {samp <- samples[,iter];
                   rpart(spamFormula, spamTrain[samp,])})

predict.bag <- function(treelist, newdata) {  	# Note: 4 
  preds <- sapply(1:length(treelist),
                 FUN=function(iter) {
                   predict(treelist[[iter]], newdata=newdata)})
  predsums <- rowSums(preds)
  predsums/length(treelist)
}

accuracyMeasures(predict.bag(treelist, newdata=spamTrain),  	# Note: 5 
                 spamTrain$spam=="spam",
                 name="bagging, training")


accuracyMeasures(predict.bag(treelist, newdata=spamTest),
                 spamTest$spam=="spam",
                 name="bagging, test")

# Note 1: 
#   Use bootstrap samples the same size as the training 
#   set, with 100 trees. 

# Note 2: 
#   Build the bootstrap samples by sampling the row indices of spamTrain with replacement. Each 
#   column of the matrix samples represents the row indices into spamTrain 
#   that comprise the bootstrap sample. 

# Note 3: 
#   Train the individual decision trees and return them 
#   in a list. Note: this step can take a few minutes. 

# Note 4: 
#   predict.bag assumes the underlying classifier returns decision probabilities, not 
#   decisions. 

# Note 5: 
#   Evaluate the bagged decision trees against the 
#   training and test sets. 

# example 9.3 of section 9.1.2 
# (example 9.3 of section 9.1.2)  : Exploring advanced methods : Using bagging and random forests to reduce training variance : Using random forests to further improve prediction 
# Title: Using random forests 

library(randomForest)           	# Note: 1 
set.seed(5123512) 	# Note: 2 
fmodel <- randomForest(x=spamTrain[,spamVars], 	# Note: 3 
        y=spamTrain$spam,
        ntree=100, 	# Note: 4 
        nodesize=7, 	# Note: 5 
        importance=T) 	# Note: 6 
accuracyMeasures(predict(fmodel, 	# Note: 7 
   newdata=spamTrain[,spamVars],type='prob')[,'spam'],
   spamTrain$spam=="spam",name="random forest, train")
##                  model  accuracy        f1  dev.norm
## 1 random forest, train 0.9884142 0.9706611 0.1428786
accuracyMeasures(predict(fmodel,
   newdata=spamTest[,spamVars],type='prob')[,'spam'],
   spamTest$spam=="spam",name="random forest, test")
##                 model  accuracy        f1  dev.norm
## 1 random forest, test 0.9541485 0.8845029 0.3972416

# Note 1: 
#   Load the randomForest package. 

# Note 2: 
#   Set the pseudo-random seed to a known value to try 
#   and make the random forest run repeatable. 

# Note 3: 
#   Call the randomForest() function to build the model 
#   with explanatory variables as x and the category to be predicted as 
#   y. 

# Note 4: 
#   Use 100 trees to be compatible with our bagging 
#   example. The default is 500 trees. 

# Note 5: 
#   Specify that each node of a tree must have a minimum 
#   of 7 elements, to be compatible with the default minimum node size that rpart() 
#   uses on this training set. 

# Note 6: 
#   Tell the algorithm to save information to be used for 
#   calculating variable importance (we’ll see this later). 

# Note 7: 
#   Report the model quality. 

# example 9.4 of section 9.1.2 
# (example 9.4 of section 9.1.2)  : Exploring advanced methods : Using bagging and random forests to reduce training variance : Using random forests to further improve prediction 
# Title: randomForest variable importances 

varImp <- importance(fmodel)              	# Note: 1 

varImp[1:10, ]                           	# Note: 2 
##                     non-spam       spam MeanDecreaseAccuracy
## word.freq.make      2.096811  3.7304353             4.334207
## word.freq.address   3.603167  3.9967031             4.977452
## word.freq.all       2.799456  4.9527834             4.924958
## word.freq.3d        3.000273  0.4125932             2.917972
## word.freq.our       9.037946  7.9421391            10.731509
## word.freq.over      5.879377  4.2402613             5.751371
## word.freq.remove   16.637390 13.9331691            17.753122
## word.freq.internet  7.301055  4.4458342             7.947515
## word.freq.order     3.937897  4.3587883             4.866540
## word.freq.mail      5.022432  3.4701224             6.103929

varImpPlot(fmodel, type=1)                       	# Note: 3

# Note 1: 
#   Call importance() on the spam 
#   model. 

# Note 2: 
#   The importance() function returns a matrix of 
#   importance measures (larger values = more important). 

# Note 3: 
#   Plot the variable importance as measured by 
#   accuracy change. 

# example 9.5 of section 9.1.2 
# (example 9.5 of section 9.1.2)  : Exploring advanced methods : Using bagging and random forests to reduce training variance : Using random forests to further improve prediction 
# Title: Fitting with fewer variables 

selVars <- names(sort(varImp[,1], decreasing=T))[1:25] 	# Note: 1 

fsel <- randomForest(x=spamTrain[,selVars],y=spamTrain$spam, 	# Note: 2 
                           ntree=100,
                           nodesize=7,
                           importance=T)
                           
accuracyMeasures(predict(fsel,
   newdata=spamTrain[,selVars],type='prob')[,'spam'],
   spamTrain$spam=="spam",name="RF small, train")
##             model  accuracy        f1  dev.norm
## 1 RF small, train 0.9876901 0.9688546 0.1506817

accuracyMeasures(predict(fsel,
   newdata=spamTest[,selVars],type='prob')[,'spam'],
   spamTest$spam=="spam",name="RF small, test")
##            model  accuracy        f1 dev.norm
## 1 RF small, test 0.9497817 0.8738142 0.400825

# Note 1: 
#   Sort the variables by their importance, as 
#   measured by accuracy change. 

# Note 2: 
#   Build a random forest model using only the 25 
#   most important variables. 

# example 9.6 of section 9.2.2 
# (example 9.6 of section 9.2.2)  : Exploring advanced methods : Using generalized additive models (GAMs) to learn non-monotone relationships : A one-dimensional regression example 
# Title: Preparing an artificial problem 

set.seed(602957)

x <- rnorm(1000)
noise <- rnorm(1000, sd=1.5)

y <- 3*sin(2*x) + cos(0.75*x) - 1.5*(x^2 ) + noise

select <- runif(1000)
frame <- data.frame(y=y, x = x)

train <- frame[select > 0.1,]
test <-frame[select <= 0.1,]

# example 9.7 of section 9.2.2 
# (example 9.7 of section 9.2.2)  : Exploring advanced methods : Using generalized additive models (GAMs) to learn non-monotone relationships : A one-dimensional regression example 
# Title: Linear regression applied to our artificial example 

lin.model <- lm(y ~ x, data=train)
summary(lin.model)
## Call:
## lm(formula = y ~ x, data = train)
##
## Residuals:
##     Min      1Q  Median      3Q     Max
## -17.698  -1.774   0.193   2.499   7.529
##
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  -0.8330     0.1161  -7.175 1.51e-12 ***
## x             0.7395     0.1197   6.180 9.74e-10 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Residual standard error: 3.485 on 899 degrees of freedom
## Multiple R-squared:  0.04075,   Adjusted R-squared:  0.03968
## F-statistic: 38.19 on 1 and 899 DF,  p-value: 9.737e-10

#
# calculate the root mean squared error (rmse)
#
resid.lin <- train$y-predict(lin.model)
sqrt(mean(resid.lin^2))
## [1] 3.481091

# example 9.8 of section 9.2.2 
# (example 9.8 of section 9.2.2)  : Exploring advanced methods : Using generalized additive models (GAMs) to learn non-monotone relationships : A one-dimensional regression example 
# Title: GAM applied to our artificial example 

library(mgcv)                             	# Note: 1 
glin.model <- gam(y~s(x), data=train)  	# Note: 2 
glin.model$converged                      	# Note: 3 
## [1] TRUE

summary(glin.model)

## Family: gaussian                                	# Note: 4 
## Link function: identity
##
## Formula:
## y ~ s(x)
##
## Parametric coefficients:                       	# Note: 5 
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept) -0.83467    0.04852   -17.2   <2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Approximate significance of smooth terms:       	# Note: 6 
##        edf Ref.df     F p-value
## s(x) 8.685  8.972 497.8  <2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## R-sq.(adj) =  0.832   Deviance explained = 83.4%          	# Note: 7 
## GCV score =  2.144  Scale est. = 2.121     n = 901

#
# calculate the root mean squared error (rmse)
#
resid.glin <- train$y-predict(glin.model)
sqrt(mean(resid.glin^2))
## [1] 1.448514

# Note 1: 
#   Load the mgcv package. 

# Note 2: 
#   Build the model, specifying that x should be 
#   treated as a nonlinear variable. 

# Note 3: 
#   The converged parameter tells you if the algorithm 
#   converged. You should only trust the output if this is TRUE. 

# Note 4: 
#   Setting family=gaussian and link=identity tells you that the model was treated with the same 
#   distributions assumptions as a standard linear regression. 

# Note 5: 
#   The parametric coefficients are the linear terms (in this example, only the constant term). 
#   This section of the summary tells you which linear terms were 
#   significantly different from 0. 

# Note 6: 
#   The smooth terms are the nonlinear terms. This section of the summary tells you which 
#   nonlinear terms were significantly different from 0. It also tells you 
#   the effective degrees of freedom (edf) used up to build each smooth 
#   term. An edf near 1 indicates that the variable has an approximately 
#   linear relationship to the output. 

# Note 7: 
#   “R-sq (adj)” is the adjusted R-squared. “Deviance 
#   explained” is the raw R-squared (0.834). 

# example 9.9 of section 9.2.2 
# (example 9.9 of section 9.2.2)  : Exploring advanced methods : Using generalized additive models (GAMs) to learn non-monotone relationships : A one-dimensional regression example 
# Title: Comparing linear regression and GAM performance 

actual <- test$y
pred.lin <- predict(lin.model, newdata=test)  	# Note: 1 
pred.glin <- predict(glin.model, newdata=test)
resid.lin <- actual-pred.lin
resid.glin <- actual-pred.glin

sqrt(mean(resid.lin^2))   	# Note: 2 
## [1] 2.792653
sqrt(mean(resid.glin^2))
## [1] 1.401399

cor(actual, pred.lin)^2   	# Note: 3 
## [1] 0.1543172
cor(actual, pred.glin)^2
## [1] 0.7828869

# Note 1: 
#   Call both models on the test 
#   data. 

# Note 2: 
#   Compare the RMSE of the linear model and the GAM 
#   on the test data. 

# Note 3: 
#   Compare the R-squared of the linear model and the 
#   GAM on test data. 

# example 9.10 of section 9.2.3 
# (example 9.10 of section 9.2.3)  : Exploring advanced methods : Using generalized additive models (GAMs) to learn non-monotone relationships : Extracting the nonlinear relationships 
# Title: Extracting a learned spline from a GAM 

sx <- predict(glin.model, type="terms")
summary(sx)
##       s(x)
##  Min.   :-17.527035
##  1st Qu.: -2.378636
##  Median :  0.009427
##  Mean   :  0.000000
##  3rd Qu.:  2.869166
##  Max.   :  4.084999

xframe <- cbind(train, sx=sx[,1])

ggplot(xframe, aes(x=x)) + geom_point(aes(y=y), alpha=0.4) +
                             geom_line(aes(y=sx))

# example 9.11 of section 9.2.4 
# (example 9.11 of section 9.2.4)  : Exploring advanced methods : Using generalized additive models (GAMs) to learn non-monotone relationships : Using GAM on actual data 
# Title: Applying linear regression (with and without GAM) to health data 

library(mgcv)
library(ggplot2)
load("NatalBirthData.rData")
train <- sdata[sdata$ORIGRANDGROUP<=5,]
test <- sdata[sdata$ORIGRANDGROUP>5,]
form.lin <- as.formula("DBWT ~ PWGT + WTGAIN + MAGER + UPREVIS")
linmodel <- lm(form.lin, data=train)  	# Note: 1 
summary(linmodel)

## Call:
## lm(formula = form.lin, data = train)
##
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -3155.43  -272.09    45.04   349.81  2870.55 
##
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2419.7090    31.9291  75.784  < 2e-16 ***
## PWGT           2.1713     0.1241  17.494  < 2e-16 ***
## WTGAIN         7.5773     0.3178  23.840  < 2e-16 ***
## MAGER          5.3213     0.7787   6.834  8.6e-12 ***
## UPREVIS       12.8753     1.1786  10.924  < 2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Residual standard error: 562.7 on 14381 degrees of freedom
## Multiple R-squared:  0.06596, Adjusted R-squared:  0.0657 	# Note: 2 
## F-statistic: 253.9 on 4 and 14381 DF,  p-value: < 2.2e-16

form.glin <- as.formula("DBWT ~ s(PWGT) + s(WTGAIN) +
                        s(MAGER) + s(UPREVIS)")
glinmodel <- gam(form.glin, data=train)                    	# Note: 3 
glinmodel$converged                                          	# Note: 4 
## [1] TRUE
summary(glinmodel)

## Family: gaussian 
## Link function: identity 
##
## Formula:
## DBWT ~ s(PWGT) + s(WTGAIN) + s(MAGER) + s(UPREVIS)
##
## Parametric coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 3276.948      4.623   708.8   <2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Approximate significance of smooth terms:
##              edf Ref.df       F  p-value    
## s(PWGT)    5.374  6.443  68.981  < 2e-16 ***
## s(WTGAIN)  4.719  5.743 102.313  < 2e-16 ***
## s(MAGER)   7.742  8.428   6.959 1.82e-09 ***
## s(UPREVIS) 5.491  6.425  48.423  < 2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## R-sq.(adj) =  0.0927   Deviance explained = 9.42% 	# Note: 5 
## GCV score = 3.0804e+05  Scale est. = 3.0752e+05  n = 14386

# Note 1: 
#   Build a linear model with four 
#   variables. 

# Note 2: 
#   The model explains about 7% of the variance; all 
#   coefficients are significantly different from 0. 

# Note 3: 
#   Build a GAM with the same 
#   variables. 

# Note 4: 
#   Verify that the model has 
#   converged. 

# Note 5: 
#   The model explains just under 10% of the variance; 
#   all variables have a nonlinear effect significantly different from 
#   0. 

# example 9.12 of section 9.2.4 
# (example 9.12 of section 9.2.4)  : Exploring advanced methods : Using generalized additive models (GAMs) to learn non-monotone relationships : Using GAM on actual data 
# Title: Plotting GAM results 

terms <- predict(glinmodel, type="terms")       	# Note: 1 
tframe <- cbind(DBWT = train$DBWT, as.data.frame(terms))   	# Note: 2 
colnames(tframe) <- gsub('[()]', '', colnames(tframe))     	# Note: 3 
pframe <- cbind(tframe, train[,c("PWGT", "WTGAIN",
                                       "MAGER", "UPREVIS")])        	# Note: 4 

p1 <- ggplot(pframe, aes(x=PWGT)) +
   geom_point(aes(y=scale(sPWGT, scale=F))) +  	# Note: 5 
   geom_smooth(aes(y=scale(DBWT, scale=F))) # +   	# Note: 6 
# [...]  	# Note: 7

# Note 1: 
#   Get the matrix of s() 
#   functions. 

# Note 2: 
#   Bind in birth weight; convert to data 
#   frame. 

# Note 3: 
#   Make the column names reference-friendly 
#   (“s(PWGT)” is converted to “sPWGT”, etc.). 

# Note 4: 
#   Bind in the input variables. 

# Note 5: 
#   Plot s(PWGT) shifted to be zero mean versus PWGT (mother’s weight) as points. 

# Note 6: 
#   Plot the smoothing curve of DWBT (birth weight) shifted to be zero mean versus PWGT (mother’s 
#   weight). 

# Note 7: 
#   Repeat for remaining variables (omitted for 
#   brevity). 

# example 9.13 of section 9.2.4 
# (example 9.13 of section 9.2.4)  : Exploring advanced methods : Using generalized additive models (GAMs) to learn non-monotone relationships : Using GAM on actual data 
# Title: Checking GAM model performance on hold-out data 

pred.lin <- predict(linmodel, newdata=test)  	# Note: 1 
pred.glin <- predict(glinmodel, newdata=test)

cor(pred.lin, test$DBWT)^2           	# Note: 2 
# [1] 0.0616812
cor(pred.glin, test$DBWT)^2
# [1] 0.08857426

# Note 1: 
#   Run both the linear model and the GAM on the test 
#   data. 

# Note 2: 
#   Calculate R-squared for both 
#   models. 

# example 9.14 of section 9.2.5 
# (example 9.14 of section 9.2.5)  : Exploring advanced methods : Using generalized additive models (GAMs) to learn non-monotone relationships : Using GAM for logistic regression 
# Title: GLM logistic regression 

form <- as.formula("DBWT < 2000 ~ PWGT + WTGAIN + MAGER + UPREVIS")
logmod <- glm(form, data=train, family=binomial(link="logit"))

# example 9.15 of section 9.2.5 
# (example 9.15 of section 9.2.5)  : Exploring advanced methods : Using generalized additive models (GAMs) to learn non-monotone relationships : Using GAM for logistic regression 
# Title: GAM logistic regression 

form2 <- as.formula("DBWT<2000~s(PWGT)+s(WTGAIN)+
                                              s(MAGER)+s(UPREVIS)")
glogmod <- gam(form2, data=train, family=binomial(link="logit"))

glogmod$converged
## [1] TRUE

summary(glogmod)
## Family: binomial 
## Link function: logit 
##
## Formula:
## DBWT < 2000 ~ s(PWGT) + s(WTGAIN) + s(MAGER) + s(UPREVIS)
##
## Parametric coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -3.94085    0.06794     -58   <2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Approximate significance of smooth terms: 
##              edf Ref.df  Chi.sq  p-value    
## s(PWGT)    1.905  2.420   2.463  0.36412    	# Note: 1 
## s(WTGAIN)  3.674  4.543  64.426 1.72e-12 ***
## s(MAGER)   1.003  1.005   8.335  0.00394 ** 
## s(UPREVIS) 6.802  7.216 217.631  < 2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## R-sq.(adj) =  0.0331   Deviance explained = 9.14% 	# Note: 2 
## UBRE score = -0.76987  Scale est. = 1         n = 14386

# Note 1: 
#   Note that there’s no proof that the mother’s weight (PWGT) has a significant effect on 
#   outcome. 

# Note 2: 
#   “Deviance explained” is the pseudo R-squared: 1 - 
#   (deviance/null.deviance). 

# example 9.16 of section 9.3.1 
# (example 9.16 of section 9.3.1)  : Exploring advanced methods : Using kernel methods to increase data separation : Understanding kernel functions 
# Title: An artificial kernel example 

u <- c(1,2)
v <- c(3,4)
k <- function(u,v) { 	# Note: 1 
     u[1]*v[1] + u[2]*v[2] +
        u[1]*u[1]*v[1]*v[1] + u[2]*u[2]*v[2]*v[2] +
        u[1]*u[2]*v[1]*v[2]
  }
phi <- function(x) { 	# Note: 2 
     x <- as.numeric(x)
     c(x,x*x,combn(x,2,FUN=prod))
  }
print(k(u,v)) 	# Note: 3 
## [1] 108
print(phi(u))
## [1] 1 2 1 4 2
print(phi(v))
## [1]  3  4  9 16 12
print(as.numeric(phi(u) %*% phi(v))) 	# Note: 4 
## [1] 108

# Note 1: 
#   Define a function of two vector variables 
#   (both two dimensional) as the sum of various products of terms. 

# Note 2: 
#   Define a function of a single vector variable 
#   that returns a vector containing the original entries plus all products of 
#   entries. 

# Note 3: 
#   Example evaluation of k(,). 

# Note 4: 
#   Confirm phi() agrees with k(,). phi() is the certificate that shows k(,) is in fact a 
#   kernel. 

# example 9.17 of section 9.3.2 
# (example 9.17 of section 9.3.2)  : Exploring advanced methods : Using kernel methods to increase data separation : Using an explicit kernel on a problem 
# Title: Applying stepwise linear regression to PUMS data 

dtrain <- subset(psub,ORIGRANDGROUP >= 500)
dtest <- subset(psub,ORIGRANDGROUP < 500)  	# Note: 1 
m1 <- step( 	# Note: 2 
   lm(log(PINCP,base=10) ~ AGEP + SEX + COW + SCHL,
      data=dtrain), 	# Note: 3 
   direction='both')
rmse <- function(y, f) { sqrt(mean( (y-f)^2 )) } 	# Note: 4 
print(rmse(log(dtest$PINCP,base=10),
   predict(m1,newdata=dtest))) 	# Note: 5 
# [1] 0.2752171

# Note 1: 
#   Split data into test and training. 

# Note 2: 
#   Ask that the linear regression model we’re building be 
#   stepwise improved, which is a powerful automated procedure for 
#   removing variables that don’t seem to have significant impacts 
#   (can improve generalization performance). 

# Note 3: 
#   Build the basic linear regression model. 

# Note 4: 
#   Define the RMSE function. 

# Note 5: 
#   Calculate the RMSE between the prediction and the 
#   actuals. 

# example 9.18 of section 9.3.2 
# (example 9.18 of section 9.3.2)  : Exploring advanced methods : Using kernel methods to increase data separation : Using an explicit kernel on a problem 
# Title: Applying an example explicit kernel transform 

phi <- function(x) { 	# Note: 1 
     x <- as.numeric(x)
     c(x,x*x,combn(x,2,FUN=prod))
  }
phiNames <- function(n) { 	# Note: 2 
     c(n,paste(n,n,sep=':'),
        combn(n,2,FUN=function(x) {paste(x,collapse=':')}))
  }
modelMatrix <- model.matrix(~ 0 + AGEP + SEX + COW + SCHL,psub) 	# Note: 3 
colnames(modelMatrix) <- gsub('[^a-zA-Z0-9]+','_',
   colnames(modelMatrix)) 	# Note: 4 
pM <- t(apply(modelMatrix,1,phi)) 	# Note: 5 
vars <- phiNames(colnames(modelMatrix))
vars <- gsub('[^a-zA-Z0-9]+','_',vars)
colnames(pM) <- vars 	# Note: 6 
pM <- as.data.frame(pM)
pM$PINCP <- psub$PINCP
pM$ORIGRANDGROUP <- psub$ORIGRANDGROUP
pMtrain <- subset(pM,ORIGRANDGROUP >= 500)
pMtest <- subset(pM,ORIGRANDGROUP < 500) 	# Note: 7

# Note 1: 
#   Define our primal kernel function: map a 
#   vector to a copy of itself plus all square terms and cross-multiplied 
#   terms. 

# Note 2: 
#   Define a function similar to our primal 
#   kernel, but working on variable names instead of values. 

# Note 3: 
#   Convert data to a matrix where all 
#   categorical variables are encoded as multiple numeric indicators. 

# Note 4: 
#   Remove problematic characters from matrix 
#   column names. 

# Note 5: 
#   Apply the primal kernel function to every 
#   row of the matrix and transpose results so they’re written as rows (not as a 
#   list as returned by apply()). 

# Note 6: 
#   Extend names from original matrix to 
#   names for compound variables in new matrix. 

# Note 7: 
#   Add in outcomes, test/train split 
#   columns, and prepare new data for modeling. 

# example 9.19 of section 9.3.2 
# (example 9.19 of section 9.3.2)  : Exploring advanced methods : Using kernel methods to increase data separation : Using an explicit kernel on a problem 
# Title: Modeling using the explicit kernel transform 

formulaStr2 <- paste('log(PINCP,base=10)',
   paste(vars,collapse=' + '),
   sep=' ~ ')
m2 <- lm(as.formula(formulaStr2),data=pMtrain)
coef2 <- summary(m2)$coefficients
interestingVars <- setdiff(rownames(coef2)[coef2[,'Pr(>|t|)']<0.01],
                              '(Intercept)')
interestingVars <- union(colnames(modelMatrix),interestingVars) 	# Note: 1 
formulaStr3 <- paste('log(PINCP,base=10)',
                    paste(interestingVars,collapse=' + '),
                    sep=' ~ ')
m3 <- step(lm(as.formula(formulaStr3),data=pMtrain),direction='both') 	# Note: 2 
print(rmse(log(pMtest$PINCP,base=10),predict(m3,newdata=pMtest))) 	# Note: 3 
# [1] 0.2735955

# Note 1: 
#   Select a set of interesting variables by building an initial model using all of the new 
#   variables and retaining an interesting subset. This is an ad hoc 
#   move to speed up the stepwise regression by trying to quickly 
#   dispose of many useless derived variables. By introducing many new 
#   variables, the primal kernel method also introduces many new degrees 
#   of freedom, which can invite overfitting. 

# Note 2: 
#   Stepwise regress on subset of variables to 
#   get new model. 

# Note 3: 
#   Calculate the RMSE between the prediction and the actuals. 

# example 9.20 of section 9.3.2 
# (example 9.20 of section 9.3.2)  : Exploring advanced methods : Using kernel methods to increase data separation : Using an explicit kernel on a problem 
# Title: Inspecting the results of the explicit kernel model 

print(summary(m3))

## Call:
## lm(formula = log(PINCP, base = 10) ~ AGEP + SEXM +
##     COWPrivate_not_for_profit_employee +
##     SCHLAssociate_s_degree + SCHLBachelor_s_degree +
##     SCHLDoctorate_degree +
##     SCHLGED_or_alternative_credential + SCHLMaster_s_degree +
##     SCHLProfessional_degree + SCHLRegular_high_school_diploma +
##     SCHLsome_college_credit_no_degree + AGEP_AGEP, data = pMtrain)
##
## Residuals:
##      Min       1Q   Median       3Q      Max
## -1.29264 -0.14925  0.01343  0.17021  0.61968
##
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)                   2.9400460  0.2219310  13.248  < 2e-16 ***
## AGEP                          0.0663537  0.0124905   5.312 1.54e-07 ***
## SEXM                          0.0934876  0.0224236   4.169 3.52e-05 ***
## COWPrivate_not_for_profit_em -0.1187914  0.0379944  -3.127  0.00186 **
## SCHLAssociate_s_degree        0.2317211  0.0509509   4.548 6.60e-06 ***
## SCHLBachelor_s_degree         0.3844459  0.0417445   9.210  < 2e-16 ***
## SCHLDoctorate_degree          0.3190572  0.1569356   2.033  0.04250 *
## SCHLGED_or_alternative_creden 0.1405157  0.0766743   1.833  0.06737 .
## SCHLMaster_s_degree           0.4553550  0.0485609   9.377  < 2e-16 ***
## SCHLProfessional_degree       0.6525921  0.0845052   7.723 5.01e-14 ***
## SCHLRegular_high_school_diplo 0.1016590  0.0415834   2.445  0.01479 *
## SCHLsome_college_credit_no_de 0.1655906  0.0416345   3.977 7.85e-05 ***
## AGEP_AGEP                    -0.0007547  0.0001704  -4.428 1.14e-05 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Residual standard error: 0.2649 on 582 degrees of freedom
## Multiple R-squared:  0.3541,    Adjusted R-squared:  0.3408
## F-statistic: 26.59 on 12 and 582 DF,  p-value: < 2.2e-16

# example 9.21 of section 9.4.2 
# (example 9.21 of section 9.4.2)  : Exploring advanced methods : Using SVMs to model complicated decision boundaries : Trying an SVM on artificial example data 
# Title: Setting up the spirals data as an example classification problem 

library('kernlab')
data('spirals')  	# Note: 1 
sc <- specc(spirals, centers = 2) 	# Note: 2 
s <- data.frame(x=spirals[,1],y=spirals[,2],
   class=as.factor(sc)) 	# Note: 3 
library('ggplot2')
ggplot(data=s) +
   geom_text(aes(x=x,y=y,
      label=class,color=class)) +
   coord_fixed() + 
   theme_bw() + theme(legend.position='none') 	# Note: 4

# Note 1: 
#   Load the kernlab kernel and support vector 
#   machine package and then ask that the included example "spirals" be made 
#   available. 

# Note 2: 
#   Use kernlab’s spectral clustering routine 
#   to identify the two different spirals in the example dataset. 

# Note 3: 
#   Combine the spiral coordinates and the 
#   spiral label into a data frame. 

# Note 4: 
#   Plot the spirals with class labels. 

# example 9.22 of section 9.4.2 
# (example 9.22 of section 9.4.2)  : Exploring advanced methods : Using SVMs to model complicated decision boundaries : Trying an SVM on artificial example data 
# Title: SVM with a poor choice of kernel 

set.seed(2335246L)
s$group <- sample.int(100,size=dim(s)[[1]],replace=T)
sTrain <- subset(s,group>10)
sTest <- subset(s,group<=10) 	# Note: 1 
# mSVMV <- ksvm(class~x+y,data=sTrain,kernel='vanilladot') 
# had been using ksvm, but it seems to keep bad state in some cases
library('e1071')
mSVMV <- svm(class~x+y,data=sTrain,kernel='linear',type='nu-classification') 	# Note: 2 
sTest$predSVMV <- predict(mSVMV,newdata=sTest,type='response') 	# Note: 3 
ggplot() +
   geom_text(data=sTest,aes(x=x,y=y,
      label=predSVMV),size=12) +
   geom_text(data=s,aes(x=x,y=y,
      label=class,color=class),alpha=0.7) +
   coord_fixed() + 
   theme_bw() + theme(legend.position='none') 	# Note: 4

# Note 1: 
#   Prepare to try to learn spiral class label 
#   from coordinates using a support vector machine. 

# Note 2: 
#   Build the support vector model using a 
#   vanilladot kernel (not a very good kernel). 

# Note 3: 
#   Use the model to predict class on held-out 
#   data. 

# Note 4: 
#   Plot the predictions on top of a grey copy 
#   of all the data so we can see if predictions agree with the original 
#   markings. 

# example 9.23 of section 9.4.2 
# (example 9.23 of section 9.4.2)  : Exploring advanced methods : Using SVMs to model complicated decision boundaries : Trying an SVM on artificial example data 
# Title: SVM with a good choice of kernel 

# mSVMG <- ksvm(class~x+y,data=sTrain,kernel='rbfdot')
# had been using ksvm, but it seems to be keeping bad state in some cases
mSVMG <- svm(class~x+y,data=sTrain,kernel='radial',type='nu-classification') 	# Note: 1 
sTest$predSVMG <- predict(mSVMG,newdata=sTest,type='response')
ggplot() +
   geom_text(data=sTest,aes(x=x,y=y,
      label=predSVMG),size=12) +
   geom_text(data=s,aes(x=x,y=y,
      label=class,color=class),alpha=0.7) +
   coord_fixed() + 
   theme_bw() + theme(legend.position='none')

# Note 1: 
#   This time use the "radial" or 
#   Gaussian kernel, which is a nice geometric similarity measure. 

# example 9.24 of section 9.4.3 
# (example 9.24 of section 9.4.3)  : Exploring advanced methods : Using SVMs to model complicated decision boundaries : Using SVMs on real data 
# Title: Revisiting the Spambase example with GLM 

spamD <- read.table('spamD.tsv',header=T,sep='\t')
spamTrain <- subset(spamD,spamD$rgroup>=10)
spamTest <- subset(spamD,spamD$rgroup<10)
spamVars <- setdiff(colnames(spamD),list('rgroup','spam'))
spamFormula <- as.formula(paste('spam=="spam"',
   paste(spamVars,collapse=' + '),sep=' ~ '))
spamModel <- glm(spamFormula,family=binomial(link='logit'),
   data=spamTrain)
spamTest$pred <- predict(spamModel,newdata=spamTest,
   type='response')
print(with(spamTest,table(y=spam,glPred=pred>=0.5)))
##           glPred
## y          FALSE TRUE
##   non-spam   264   14
##   spam        22  158

# example 9.25 of section 9.4.3 
# (example 9.25 of section 9.4.3)  : Exploring advanced methods : Using SVMs to model complicated decision boundaries : Using SVMs on real data 
# Title: Applying an SVM to the Spambase example 

library('kernlab')
spamFormulaV <- as.formula(paste('spam',
   paste(spamVars,collapse=' + '),sep=' ~ '))
# may want to switch to library('e1071') svm() as had some state holding problems in some examles
svmM <- ksvm(spamFormulaV,data=spamTrain, 	# Note: 1 
        kernel='rbfdot', 	# Note: 2 
        C=10, 	# Note: 3 
        prob.model=T,cross=5, 	# Note: 4 
        class.weights=c('spam'=1,'non-spam'=10) 	# Note: 5 
        )
spamTest$svmPred <- predict(svmM,newdata=spamTest,type='response')
print(with(spamTest,table(y=spam,svmPred=svmPred)))
##           svmPred
## y          non-spam spam
##   non-spam      269    9
##   spam           27  153

# Note 1: 
#   Build a support vector model for the Spambase 
#   problem. 

# Note 2: 
#   Ask for the radial dot or Gaussian kernel (in 
#   fact the default kernel). 

# Note 3: 
#   Set the “soft margin penalty” high; prefer not moving training examples over getting a wider 
#   margin. Prefer a complex model that applies weakly to all the data 
#   over a simpler model that applies strongly on a subset of the 
#   data. 

# Note 4: 
#   Ask that, in addition to a predictive model, an estimate of a model estimating class 
#   probabilities also be built. Not all SVM libraries support this 
#   operation, and the probabilities are essentially built after the 
#   model (through a cross-validation procedure) and may not be as high-quality 
#   as the model itself. 

# Note 5: 
#   Explicitly control the trade-off between 
#   false positive and false negative errors. In this case, we say non-spam 
#   classified as spam (a false positive) should be considered an expensive 
#   mistake. 

# example 9.26 of section 9.4.3 
# (example 9.26 of section 9.4.3)  : Exploring advanced methods : Using SVMs to model complicated decision boundaries : Using SVMs on real data 
# Title: Printing the SVM results summary 

print(svmM)
## Support Vector Machine object of class "ksvm" 
##
## SV type: C-svc  (classification) 
##  parameter : cost C = 10 
##
## Gaussian Radial Basis kernel function. 
##  Hyperparameter : sigma =  0.0299836801848002 
##
## Number of Support Vectors : 1118 
##
## Objective Function Value : -4642.236 
## Training error : 0.028482 
## Cross validation error : 0.076998 
## Probability model included.

# example 9.27 of section 9.4.3 
# (example 9.27 of section 9.4.3)  : Exploring advanced methods : Using SVMs to model complicated decision boundaries : Using SVMs on real data 
# Title: Shifting decision point to perform an apples-to-apples comparison 

sameCut <- sort(spamTest$pred)[length(spamTest$pred)-162] 	# Note: 1 
print(with(spamTest,table(y=spam,glPred=pred>sameCut))) 	# Note: 2 
##           glPred
## y          FALSE TRUE
##   non-spam   267   11
##   spam        29  151

# Note 1: 
#   Find out what GLM score threshold has 162 
#   examples above it. 

# Note 2: 
#   Ask the GLM model for its predictions that 
#   are above the threshold. We’re essentially asking the model for its 162 best 
#   candidate spam prediction results. 

# informalexample 10.1 of section 10.2.1 
# (informalexample 10.1 of section 10.2.1)  : Documentation and deployment : Using knitr to produce milestone documentation : What is knitr? 

library(knitr)
knit('simple.Rmd')

# informalexample 10.2 of section 10.2.1 
# (informalexample 10.2 of section 10.2.1)  : Documentation and deployment : Using knitr to produce milestone documentation : What is knitr? 

echo "library(knitr); knit('add.Rnw')" | R --vanilla 	# Note: 1 
pdflatex add.tex 	# Note: 2

# Note 1: 
#   Use R in batch mode to create add.tex from 
#   add.Rnw. 

# Note 2: 
#   Use LaTeX to create add.pdf from 
#   add.tex. 

# example 10.7 of section 10.3.1 
# (example 10.7 of section 10.3.1)  : Documentation and deployment : Using comments and version control for running documentation : Writing effective comments 
# Title: Example code comment 

#    Return the pseudo logarithm of x, which is close to
# sign(x)*log10(abs(x)) for x such that abs(x) is large
# and doesn't "blow up" near zero.  Useful
# for transforming wide-range variables that may be negative
# (like profit/loss).
# See: http://www.win-vector.com/blog
#  /2012/03/modeling-trick-the-signed-pseudo-logarithm/
#    NB: This transform has the undesirable property of making most
# signed distributions appear bimodal around the origin, no matter
# what the underlying distribution really looks like.
# The argument x is assumed be numeric and can be a vector.
pseudoLog10 <- function(x) { asinh(x/2)/log(10) }

# example 10.8 of section 10.3.1 
# (example 10.8 of section 10.3.1)  : Documentation and deployment : Using comments and version control for running documentation : Writing effective comments 
# Title: Useless comment 

#######################################
# Function: addone
# Author: John Mount
# Version: 1.3.11
# Location: RSource/helperFns/addone.R
# Date: 10/31/13
# Arguments: x
# Purpose: Adds one
#######################################
addone <- function(x) { x + 1 }

# example 10.9 of section 10.3.1 
# (example 10.9 of section 10.3.1)  : Documentation and deployment : Using comments and version control for running documentation : Writing effective comments 
# Title: Worse than useless comment 

# adds one
addtwo <- function(x) { x + 2 }

# example 10.16 of section 10.4.1 
# (example 10.16 of section 10.4.1)  : Documentation and deployment : Deploying models : Deploying models as R HTTP services 
# Title: Buzz model as an R-based HTTP service 

library(Rook)  	# Note: 1 
load('thRS500.Rdata') 	# Note: 2 
library(randomForest) 	# Note: 3 
numericPositions <- sapply(buzztrain[,varslist],is.numeric) 	# Note: 4 

modelFn <- function(env) { 	# Note: 5 
   errors <- c()
   warnings <- c()
   val <- c()
   row <- c()
   tryCatch(
      {
         arg <- Multipart$parse(env) 	# Note: 6 
         row <- as.list(arg[varslist])
         names(row) <- varslist
         row[numericPositions] <- as.numeric(row[numericPositions])
         frame <- data.frame(row)
         val <- predict(fmodel,newdata=frame)
      },
      warning = function(w) { message(w)
         warnings <<- c(warnings,as.character(w)) },
      error = function(e) { message(e)
         errors <<- c(errors,as.character(e)) }
   )
   body <- paste( 	# Note: 7 
      'val=',val,'\n',
      'nerrors=',length(errors),'\n',
      'nwarnings=',length(warnings),'\n',
      'query=',env$QUERY_STRING,'\n',
      'errors=',paste(errors,collapse=' '),'\n',
      'warnings=',paste(warnings,collapse=' '),'\n',
      'data row','\n',
      paste(capture.output(print(row)),collapse='\n'),'\n',
      sep='')
   list(
      status=ifelse(length(errors)<=0,200L,400L),
      headers=list('Content-Type' = 'text/text'),
      body=body )
}


s <- Rhttpd$new() 	# Note: 8 
s$add(name="modelFn",app=modelFn) 	# Note: 9 
s$start() 	# Note: 10 
print(s)
## Server started on 127.0.0.1:20714
## [1] modelFn http://127.0.0.1:20714/custom/modelFn 	# Note: 11 
## 
## Call browse() with an index number or name to run an application.

# Note 1: 
#   Load the rook HTTP server library. 

# Note 2: 
#   Load the saved buzz workspace (includes the 
#   random forest model). 

# Note 3: 
#   Load the random forest library (loading the 
#   workspace doesn’t load the library). 

# Note 4: 
#   Determine which variables are numeric (in the 
#   rook server, everything defaults to 
#   character). 

# Note 5: 
#   Declare the modeling service. 

# Note 6: 
#   This block does the actual work: parse data 
#   and apply the model. 

# Note 7: 
#   Format results, place in a list, and 
#   return. 

# Note 8: 
#   Start a new rook HTTP service. 

# Note 9: 
#   Register our model function as an HTTP 
#   service. 

# Note 10: 
#   Start the HTTP server. 

# Note 11: 
#   This is the URL where the service is 
#   running. 

# example 10.17 of section 10.4.1 
# (example 10.17 of section 10.4.1)  : Documentation and deployment : Deploying models : Deploying models as R HTTP services 
# Title: Calling the buzz HTTP service 

rowAsForm <- function(url,row) { 	# Note: 1 
   s <- paste('<HTML><HEAD></HEAD><BODY><FORM action="',url,
      '" enctype="multipart/form-data" method="POST">\n',sep='')
   s <- paste(s,'<input type="submit" value="Send"/>',sep='\n')
   qpaste <- function(a,b) {
      paste('<p> ',a,' <input type="text" name="',a,
         '" value="',b,'"/> </p>',sep='') }
   assignments <- mapply('qpaste',varslist,as.list(row)[varslist])
   s <- paste(s,paste(assignments,collapse='\n'),sep='\n')
   s <- paste(s,'</FORM></BODY></HTML>',sep='\n')
   s
}

url <- 'http://127.0.0.1:20714/custom/modelFn' 	# Note: 2 
cat(rowAsForm(url,buzztest[7,]),file='buzztest7.html') 	# Note: 3

# Note 1: 
#   Function to convert a row of dataset into a 
#   huge HTML form that transmits all of the variable 
#   values to HTTP server on submit (when the Send 
#   button is clicked). 

# Note 2: 
#   The URL we started the rook HTTP server on; 
#   you’ll have to copy the URL address and port from 
#   what’s printed when you started the Rook 
#   service. 

# Note 3: 
#   Write the form representing the variables for 
#   the seventh test example to a file. 

# example 10.18 of section 10.4.2 
# (example 10.18 of section 10.4.2)  : Documentation and deployment : Deploying models : Deploying models by export 
# Title: Exporting the random forest model 

load('thRS500.Rdata') 	# Note: 1 
library(randomForest) 	# Note: 2 

extractTrees <- function(rfModel) { 	# Note: 3 
   ei <- function(i) {
      ti <- getTree(rfModel,k=i,labelVar=T)
      ti$nodeid <- 1:dim(ti)[[1]]
      ti$treeid <- i
      ti
   }
   nTrees <- rfModel$ntree
   do.call('rbind',sapply(1:nTrees,ei,simplify=F))
}

write.table(extractTrees(fmodel), 	# Note: 4 
   file='rfmodel.tsv',row.names=F,sep='\t',quote=F)

# Note 1: 
#   Load the saved buzz workspace (includes the 
#   random forest model). 

# Note 2: 
#   Load the random forest library (loading the 
#   workspace doesn’t load the library). 

# Note 3: 
#   Define a function that joins the tree tables 
#   from the random forest getTree() method into one 
#   large table of trees. 

# Note 4: 
#   Write the table of trees as a tab-separated 
#   values table (easy for other software to 
#   read). 

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

# example B.1 of section B.1.1 
# (example B.1 of section B.1.1)  : Important statistical concepts : Distributions : Normal distribution 
# Title: Plotting the theoretical normal density 

library(ggplot2)

x <- seq(from=-5, to=5, length.out=100) # the interval [-5 5]
f <- dnorm(x)                           # normal with mean 0 and sd 1
ggplot(data.frame(x=x,y=f), aes(x=x,y=y)) + geom_line()

# example B.2 of section B.1.1 
# (example B.2 of section B.1.1)  : Important statistical concepts : Distributions : Normal distribution 
# Title: Plotting an empirical normal density 

library(ggplot2)

# draw 1000 points from a normal with mean 0, sd 1
u <- rnorm(1000)

# plot the distribution of points,
# compared to normal curve as computed by dnorm() (dashed line)
ggplot(data.frame(x=u), aes(x=x)) + geom_density() +
   geom_line(data=data.frame(x=x,y=f), aes(x=x,y=y), linetype=2)

# example B.3 of section B.1.1 
# (example B.3 of section B.1.1)  : Important statistical concepts : Distributions : Normal distribution 
# Title: Working with the normal cdf 

# --- estimate probabilities (areas) under the curve ---

# 50% of the observations will be less than the mean
pnorm(0)
# [1] 0.5

# about 2.3% of all observations are more than 2 standard
# deviations below the mean
pnorm(-2)
# [1] 0.02275013

# about 95.4% of all observations are within 2 standard deviations
# from the mean
pnorm(2) - pnorm(-2)
# [1] 0.9544997

# example B.4 of section B.1.1 
# (example B.4 of section B.1.1)  : Important statistical concepts : Distributions : Normal distribution 
# Title: Plotting x < qnorm(0.75) 

# --- return the quantiles corresponding to specific probabilities ---

# the median (50th percentile) of a normal is also the mean
qnorm(0.5)
# [1] 0

# calculate the 75th percentile
qnorm(0.75)
# [1] 0.6744898
pnorm(0.6744898)
# [1] 0.75

# --- Illustrate the 75th percentile ---

# create a graph of the normal distribution with mean 0, sd 1
x <- seq(from=-5, to=5, length.out=100)
f <- dnorm(x)
nframe <- data.frame(x=x,y=f) 

# calculate the 75th percentile
line <- qnorm(0.75)
xstr <- sprintf("qnorm(0.75) = %1.3f", line)

# the part of the normal distribution to the left
# of the 75th percentile
nframe75 <- subset(nframe, nframe$x < line)

# Plot it. 
# The shaded area is 75% of the area under the normal curve
ggplot(nframe, aes(x=x,y=y)) + geom_line() +
  geom_area(data=nframe75, aes(x=x,y=y), fill="gray") + 
  geom_vline(aes(xintercept=line), linetype=2) +
  geom_text(x=line, y=0, label=xstr, vjust=1)

# example B.5 of section B.1.3 
# (example B.5 of section B.1.3)  : Important statistical concepts : Distributions : Lognormal distribution 
# Title: Demonstrating some properties of the lognormal distribution 

# draw 1001 samples from a lognormal with meanlog 0, sdlog 1
u <- rlnorm(1001)

# the mean of u is higher than the median
mean(u)
# [1] 1.638628
median(u)
# [1] 1.001051

# the mean of log(u) is approx meanlog=0
mean(log(u))
# [1] -0.002942916

# the sd of log(u) is approx sdlog=1
sd(log(u))
# [1] 0.9820357

# generate the lognormal with meanlog=0, sdlog=1
x <- seq(from=0, to=25, length.out=500)
f <- dlnorm(x)

# generate a normal with mean=0, sd=1
x2 <- seq(from=-5,to=5, length.out=500)
f2 <- dnorm(x2)

# make data frames
lnormframe <- data.frame(x=x,y=f)
normframe <- data.frame(x=x2, y=f2)
dframe <- data.frame(u=u)

# plot densityplots with theoretical curves superimposed
p1 <- ggplot(dframe, aes(x=u)) + geom_density() +
  geom_line(data=lnormframe, aes(x=x,y=y), linetype=2)

p2 <- ggplot(dframe, aes(x=log(u))) + geom_density() +
  geom_line(data=normframe, aes(x=x,y=y), linetype=2)

# functions to plot multiple plots on one page
library(grid)
nplot <- function(plist) {
  n <- length(plist)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(n,1)))
  vplayout<-function(x,y) {viewport(layout.pos.row=x, layout.pos.col=y)}
  for(i in 1:n) {
    print(plist[[i]], vp=vplayout(i,1))
  }
}

# this is the plot that leads this section.
nplot(list(p1, p2))

# example B.6 of section B.1.3 
# (example B.6 of section B.1.3)  : Important statistical concepts : Distributions : Lognormal distribution 
# Title: Plotting the lognormal distribution 

# the 50th percentile (or median) of the lognormal with
# meanlog=0 and sdlog=10
qlnorm(0.5)
# [1] 1
# the probability of seeing a value x less than 1
plnorm(1)
# [1] 0.5

# the probability of observing a value x less than 10:
plnorm(10)
# [1] 0.9893489

# -- show the 75th percentile of the lognormal 

# use lnormframe from previous example: the 
# theoretical lognormal curve

line <- qlnorm(0.75)
xstr <- sprintf("qlnorm(0.75) = %1.3f", line)

lnormframe75 <- subset(lnormframe, lnormframe$x < line)

# Plot it 
# The shaded area is 75% of the area under the lognormal curve
ggplot(lnormframe, aes(x=x,y=y)) + geom_line() +
  geom_area(data=lnormframe75, aes(x=x,y=y), fill="gray") + 
  geom_vline(aes(xintercept=line), linetype=2) +
  geom_text(x=line, y=0, label=xstr, hjust= 0, vjust=1)

# example B.7 of section B.1.4 
# (example B.7 of section B.1.4)  : Important statistical concepts : Distributions : Binomial distribution 
# Title: Plotting the binomial distribution 

library(ggplot2)
#
# use dbinom to produce the theoretical curves
#

numflips <- 50
# x is the number of heads that we see
x <- 0:numflips

# probability of heads for several different coins
p <- c(0.05, 0.15, 0.5, 0.75)
plabels <- paste("p =", p)

# calculate the probability of seeing x heads in numflips flips
# for all the coins. This probably isn't the most elegant
# way to do this, but at least it's easy to read

flips <- NULL
for(i in 1:length(p)) {
  coin <- p[i]
  label <- plabels[i]
  tmp <- data.frame(number.of.heads=x,
                   probability = dbinom(x, numflips, coin),
                   coin.type = label)
  flips <- rbind(flips, tmp)
}


# plot it
# this is the plot that leads this section
ggplot(flips, aes(x=number.of.heads, y=probability)) +
  geom_point(aes(color=coin.type, shape=coin.type)) +
  geom_line(aes(color=coin.type))

# example B.8 of section B.1.4 
# (example B.8 of section B.1.4)  : Important statistical concepts : Distributions : Binomial distribution 
# Title: Working with the theoretical binomial distribution 

p = 0.5 # the percentage of females in this student population
class.size <- 20 # size of a classroom
numclasses <- 100 # how many classrooms we observe

# what might a typical outcome look like?
numFemales <- rbinom(numclasses, class.size, p) 	# Note: 1 

# the theoretical counts (not necessarily integral)
probs <- dbinom(0:class.size, class.size, p)
tcount <- numclasses*probs

# the obvious way to plot this is with histogram or geom_bar
# but this might just look better

zero <- function(x) {0} # a dummy function that returns only 0

ggplot(data.frame(number.of.girls=numFemales, dummy=1),
  aes(x=number.of.girls, y=dummy)) + 
  # count the number of times you see x heads
  stat_summary(fun.y="sum", geom="point", size=2) + 	# Note: 2 
  stat_summary(fun.ymax="sum", fun.ymin="zero", geom="linerange") + 
  # superimpose the theoretical number of times you see x heads
  geom_line(data=data.frame(x=0:class.size, y=probs),
            aes(x=x, y=tcount), linetype=2) +
  scale_x_continuous(breaks=0:class.size, labels=0:class.size) +
  scale_y_continuous("number of classrooms")

# Note 1: 
#   Because we didn’t call set.seed, we 
#   expect different results each time we run this line. 

# Note 2: 
#   stat_summary is one of the ways to 
#   control data aggregation during plotting. In this case, we’re using it to 
#   place the dot and bar measured from the empirical data in with the 
#   theoretical density curve. 

# example B.9 of section B.1.4 
# (example B.9 of section B.1.4)  : Important statistical concepts : Distributions : Binomial distribution 
# Title: Simulating a binomial distribution 

# use rbinom to simulate flipping a coin of probability p N times

p75 <- 0.75 # a very unfair coin (mostly heads)
N <- 1000  # flip it several times
flips_v1 <- rbinom(N, 1, p75)

# Another way to generate unfair flips is to use runif:
# the probability that a uniform random number from [0 1)
# is less than p is exactly p. So "less than p" is "heads".
flips_v2 <- as.numeric(runif(N) < p75) 

prettyprint_flips <- function(flips) {
  outcome <- ifelse(flips==1, "heads", "tails")
  table(outcome)
}

prettyprint_flips(flips_v1)
# outcome
# heads tails 
# 756   244 
prettyprint_flips(flips_v2)
# outcome
# heads tails 
# 743   257

# example B.10 of section B.1.4 
# (example B.10 of section B.1.4)  : Important statistical concepts : Distributions : Binomial distribution 
# Title: Working with the binomial distribution 

# pbinom example

nflips <- 100
nheads <- c(25, 45, 50, 60)  # number of heads

# what are the probabilities of observing at most that 
# number of heads on a fair coin?
left.tail <- pbinom(nheads, nflips, 0.5)
sprintf("%2.2f", left.tail)
# [1] "0.00" "0.18" "0.54" "0.98"

# the probabilities of observing more than that
# number of heads on a fair coin?
right.tail <- pbinom(nheads, nflips, 0.5, lower.tail=F)
sprintf("%2.2f", right.tail)
# [1] "1.00" "0.82" "0.46" "0.02"

# as expected:
left.tail+right.tail
#  [1] 1 1 1 1 

# so if you flip a fair coin 100 times,
# you are guaranteed to see more than 10 heads, 
# almost guaranteed to see fewer than 60, and
# probably more than 45.

# qbinom example

nflips <- 100

# what's the 95% "central" interval of heads that you
# would expect to observe on 100 flips of a fair coin?

left.edge <- qbinom(0.025, nflips, 0.5)
right.edge <- qbinom(0.025, nflips, 0.5, lower.tail=F)
c(left.edge, right.edge)
# [1] 40 60

# so with 95% probability you should see between 40 and 60 heads

# example B.11 of section B.1.4 
# (example B.11 of section B.1.4)  : Important statistical concepts : Distributions : Binomial distribution 
# Title: Working with the binomial cdf 

# because this is a discrete probability distribution, 
# pbinom and qbinom are not exact inverses of each other

# this direction works
pbinom(45, nflips, 0.5)
# [1] 0.1841008
qbinom(0.1841008, nflips, 0.5)
# [1] 45

# this direction won't be exact
qbinom(0.75, nflips, 0.5)
# [1] 53
pbinom(53, nflips, 0.5)
# [1] 0.7579408

# example B.12 of section B.2.2 
# (example B.12 of section B.2.2)  : Important statistical concepts : Statistical theory : A/B tests 
# Title: Building simulated A/B test data 

set.seed(123515)
d <- rbind( 	# Note: 1 
   data.frame(group='A',converted=rbinom(100000,size=1,p=0.05)), 	# Note: 2 
   data.frame(group='B',converted=rbinom(10000,size=1,p=0.055)) 	# Note: 3 
)

# Note 1: 
#   Build a data frame to store simulated 
#   examples. 

# Note 2: 
#   Add 100,000 examples from the A group 
#   simulating a conversion rate of 5%. 

# Note 3: 
#   Add 10,000 examples from the B group 
#   simulating a conversion rate of 5.5%. 

# example B.13 of section B.2.2 
# (example B.13 of section B.2.2)  : Important statistical concepts : Statistical theory : A/B tests 
# Title: Summarizing the A/B test into a contingency table 

tab <- table(d)
print(tab)
##      converted
## group     0     1
##     A 94979  5021
##     B  9398   602

# example B.14 of section B.2.2 
# (example B.14 of section B.2.2)  : Important statistical concepts : Statistical theory : A/B tests 
# Title: Calculating the observed A and B rates 

aConversionRate <- tab['A','1']/sum(tab['A',])
print(aConversionRate)
## [1] 0.05021
bConversionRate <- tab['B','1']/sum(tab['B',])
print(bConversionRate)
## [1] 0.0602
commonRate <- sum(tab[,'1'])/sum(tab)
print(commonRate)
## [1] 0.05111818

# example B.15 of section B.2.2 
# (example B.15 of section B.2.2)  : Important statistical concepts : Statistical theory : A/B tests 
# Title: Calculating the significance of the observed difference in rates 

fisher.test(tab)

## 	Fisher's Exact Test for Count Data
##
## data:  tab
## p-value = 2.469e-05
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  1.108716 1.322464
## sample estimates:
## odds ratio 
##   1.211706

# example B.16 of section B.2.2 
# (example B.16 of section B.2.2)  : Important statistical concepts : Statistical theory : A/B tests 
# Title: Computing frequentist significance 

print(pbinom( 	# Note: 1 
   lower.tail=F, 	# Note: 2 
   q=tab['B','1']-1, 	# Note: 3 
   size=sum(tab['B',]), 	# Note: 4 
   prob=commonRate 	# Note: 5 
   )) 
## [1] 3.153319e-05

# Note 1: 
#   Use the pbinom() call to calculate how 
#   likely different observed counts are. 

# Note 2: 
#   Signal we want the probability of being 
#   greater than a given q. 

# Note 3: 
#   Ask for the probability of seeing at least as many conversions as our observed B groups 
#   did. 

# Note 4: 
#   Specify the total number of trials as 
#   equal to what we saw in our B group. 

# Note 5: 
#   Specify the conversion probability at the 
#   estimated common rate. 

# example B.17 of section B.2.2 
# (example B.17 of section B.2.2)  : Important statistical concepts : Statistical theory : A/B tests 
# Title: Bayesian estimate of the posterior tail mass 

print(pbeta( 	# Note: 1 
   aConversionRate, 	# Note: 2 
   shape1=commonRate+tab['B','1'], 	# Note: 3 
   shape2=(1-commonRate)+tab['B','0'])) 	# Note: 4 
## [1] 4.731817e-06

# Note 1: 
#   pbeta() functionUse pbeta() to estimate how likely 
#   different observed conversion rates are. 

# Note 2: 
#   Ask for the probability of seeing a 
#   conversion rate no larger than aConversionRate. 

# Note 3: 
#   Estimate conversion count as prior 
#   commonRate plus the B observations. 

# Note 4: 
#   Estimate nonconversion count as prior 
#   1-commonRate plus the B observations. 

# example B.18 of section B.2.2 
# (example B.18 of section B.2.2)  : Important statistical concepts : Statistical theory : A/B tests 
# Title: Plotting the posterior distribution of the B group 

library('ggplot2')
plt <- data.frame(x=seq(from=0.04,to=0.07,length.out=301))
plt$density <- dbeta(plt$x,
   shape1=commonRate+tab['B','1'],
   shape2=(1-commonRate)+tab['B','0'])
ggplot(dat=plt) + 
   geom_line(aes(x=x,y=density)) + 
   geom_vline(aes(xintercept=bConversionRate)) +
   geom_vline(aes(xintercept=aConversionRate),linetype=2)

# example B.19 of section B.2.3 
# (example B.19 of section B.2.3)  : Important statistical concepts : Statistical theory : Power of tests 
# Title: Sample size estimate 

estimate <- function(targetRate,difference,errorProb) {
    ceiling(-log(errorProb)*targetRate/(difference^2))
}

est <- estimate(0.045,0.004,0.05)
print(est)
## [1] 8426

# example B.20 of section B.2.3 
# (example B.20 of section B.2.3)  : Important statistical concepts : Statistical theory : Power of tests 
# Title: Exact binomial sample size calculation 

errorProb <- function(targetRate,difference,size) { 	# Note: 1 
   pbinom(ceiling((targetRate-difference)*size),
      size=size,prob=targetRate) 
}

print(errorProb(0.045,0.004,est)) 	# Note: 2 
## [1] 0.04153646

binSearchNonPositive <- function(fEventuallyNegative) { 	# Note: 3 
  low <- 1
  high <- low+1
  while(fEventuallyNegative(high)>0) {
    high <- 2*high
  }
  while(high>low+1) {
    m <- low + (high-low) %/% 2
    if(fEventuallyNegative(m)>0) {
       low <- m
    } else {
       high <- m
    }
  }
  high
}

actualSize <- function(targetRate,difference,errorProb) {
   binSearchNonPositive(function(n) {
       errorProb(targetRate,difference,n) - errorProb })
}

size <- actualSize(0.045,0.004,0.05) 	# Note: 4 
print(size) 
## [1] 7623
print(errorProb(0.045,0.004,size))
## [1] 0.04983659

# Note 1: 
#   Define a function that calculates the 
#   probability of seeing a low number of conversions, assuming the actual 
#   conversion rate is targetRate and the size of the experiment is size. Low is 
#   considered be a count that’s at least difference*size below the expected value 
#   targetRate*size. 

# Note 2: 
#   Calculate probability of a bad experiment using 
#   estimated experiment size. The failure odds are around 4% (under the 5% we’re 
#   designing for), which means the estimate size was slightly high. 

# Note 3: 
#   Define a binary search that finds a non-positive 
#   value of a function that’s guaranteed to be eventually negative. This search 
#   works around the minor non-monotonicity in errorProb() (due to rounding 
#   issues). 

# Note 4: 
#   Calculate the required sample size for our B 
#   experiment. 

# example B.21 of section B.2.4 
# (example B.21 of section B.2.4)  : Important statistical concepts : Statistical theory : Specialized statistical tests 
# Title: Building synthetic uncorrelated income example 

set.seed(235236) 	# Note: 1 
d <- data.frame(EarnedIncome=100000*rlnorm(100),
                 CapitalGains=100000*rlnorm(100))  	# Note: 2 
print(with(d,cor(EarnedIncome,CapitalGains)))
# [1] -0.01066116 	# Note: 3

# Note 1: 
#   Set the pseudo-random seed to a known 
#   value so the demonstration is repeatable. 

# Note 2: 
#   Generate our synthetic data. 

# Note 3: 
#   The correlation is -0.01, which is very near 0—indicating (as designed) no relation. 

# example B.22 of section B.2.4 
# (example B.22 of section B.2.4)  : Important statistical concepts : Statistical theory : Specialized statistical tests 
# Title: Calculating the (non)significance of the observed correlation 

with(d,cor(EarnedIncome,CapitalGains,method='spearman'))
# [1] 0.03083108
with(d,cor.test(EarnedIncome,CapitalGains,method='spearman'))
#
#       Spearman's rank correlation rho
#
#data:  EarnedIncome and CapitalGains
#S = 161512, p-value = 0.7604
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#       rho
#0.03083108

# example B.23 of section B.3.1 
# (example B.23 of section B.3.1)  : Important statistical concepts : Examples of the statistical view of data : Sampling bias 
# Title: Misleading significance result from biased observations 

veryHighIncome <- subset(d, EarnedIncome+CapitalGains>=500000)
print(with(veryHighIncome,cor.test(EarnedIncome,CapitalGains,
    method='spearman')))
#
#       Spearman's rank correlation rho
#
#data:  EarnedIncome and CapitalGains
#S = 1046, p-value < 2.2e-16
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#       rho
#-0.8678571

# example B.24 of section B.3.1 
# (example B.24 of section B.3.1)  : Important statistical concepts : Examples of the statistical view of data : Sampling bias 
# Title: Plotting biased view of income and capital gains 

library(ggplot2)
ggplot(data=d,aes(x=EarnedIncome,y=CapitalGains)) +
   geom_point() + geom_smooth(method='lm') +
   coord_cartesian(xlim=c(0,max(d)),ylim=c(0,max(d))) 	# Note: 1 
ggplot(data=veryHighIncome,aes(x=EarnedIncome,y=CapitalGains)) +
   geom_point() + geom_smooth(method='lm') +
   geom_point(data=subset(d,EarnedIncome+CapitalGains<500000),
         aes(x=EarnedIncome,y=CapitalGains),
      shape=4,alpha=0.5,color='red') +
   geom_segment(x=0,xend=500000,y=500000,yend=0,
      linetype=2,alpha=0.5,color='red') +
   coord_cartesian(xlim=c(0,max(d)),ylim=c(0,max(d))) 	# Note: 2 
print(with(subset(d,EarnedIncome+CapitalGains<500000),
    cor.test(EarnedIncome,CapitalGains,method='spearman'))) 	# Note: 3 
#
#        Spearman's rank correlation rho
#
#data:  EarnedIncome and CapitalGains
#S = 107664, p-value = 0.6357
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#        rho
#-0.05202267

# Note 1: 
#   Plot all of the income data with linear 
#   trend line (and uncertainty band). 

# Note 2: 
#   Plot the very high income data and linear 
#   trend line (also include cut-off and portrayal of suppressed data). 

# Note 3: 
#   Compute correlation of suppressed 
#   data. 

# example B.25 of section B.3.2 
# (example B.25 of section B.3.2)  : Important statistical concepts : Examples of the statistical view of data : Omitted variable bias 
# Title: Summarizing our synthetic biological data 

load('synth.RData')
print(summary(s))
##       week         Caco2A2BPapp       FractionHumanAbsorption
##  Min.   :  1.00   Min.   :6.994e-08   Min.   :0.09347        
##  1st Qu.: 25.75   1st Qu.:7.312e-07   1st Qu.:0.50343        
##  Median : 50.50   Median :1.378e-05   Median :0.86937        
##  Mean   : 50.50   Mean   :2.006e-05   Mean   :0.71492        
##  3rd Qu.: 75.25   3rd Qu.:4.238e-05   3rd Qu.:0.93908        
##  Max.   :100.00   Max.   :6.062e-05   Max.   :0.99170
head(s)
##   week Caco2A2BPapp FractionHumanAbsorption
## 1    1 6.061924e-05              0.11568186
## 2    2 6.061924e-05              0.11732401
## 3    3 6.061924e-05              0.09347046
## 4    4 6.061924e-05              0.12893540
## 5    5 5.461941e-05              0.19021858
## 6    6 5.370623e-05              0.14892154
# View(s) 	# Note: 1

# Note 1: 
#   Display a date in spreadsheet like 
#   window. View is one of the commands that has a much better implementation in 
#   RStudio than in basic R. 

# example B.26 of section B.3.2 
# (example B.26 of section B.3.2)  : Important statistical concepts : Examples of the statistical view of data : Omitted variable bias 
# Title: Building data that improves over time 

set.seed(2535251)
s <- data.frame(week=1:100)
s$Caco2A2BPapp <- sort(sample(d$Caco2A2BPapp,100,replace=T),
   decreasing=T)
sigmoid <- function(x) {1/(1+exp(-x))}
s$FractionHumanAbsorption <- 	# Note: 1 
 sigmoid(
   7.5 + 0.5*log(s$Caco2A2BPapp) + 	# Note: 2 
   s$week/10 - mean(s$week/10) + 	# Note: 3 
   rnorm(100)/3 	# Note: 4 
   )
write.table(s,'synth.csv',sep=',',
   quote=F,row.names=F)

# Note 1: 
#   Build synthetic examples. 

# Note 2: 
#   Add in Caco2 to absorption relation learned from original dataset. Note the relation is 
#   positive: better Caco2 always drives better absorption in our 
#   synthetic dataset. We’re log transforming Caco2, as it has over 3 
#   decades of range. 

# Note 3: 
#   Add in a mean-0 term that depends on time to simulate the effects of improvements as the 
#   project moves forward. 

# Note 4: 
#   Add in a mean-0 noise term. 

# example B.27 of section B.3.2 
# (example B.27 of section B.3.2)  : Important statistical concepts : Examples of the statistical view of data : Omitted variable bias 
# Title: A bad model (due to omitted variable bias) 

print(summary(glm(data=s,
   FractionHumanAbsorption~log(Caco2A2BPapp),
   family=binomial(link='logit'))))
## Warning: non-integer #successes in a binomial glm!
## 
## Call:
## glm(formula = FractionHumanAbsorption ~ log(Caco2A2BPapp), 
##    family = binomial(link = "logit"), 
##     data = s)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -0.609  -0.246  -0.118   0.202   0.557  
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -10.003      2.752   -3.64  0.00028 ***
## log(Caco2A2BPapp)   -0.969      0.257   -3.77  0.00016 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 43.7821  on 99  degrees of freedom
## Residual deviance:  9.4621  on 98  degrees of freedom
## AIC: 64.7
## 
## Number of Fisher Scoring iterations: 6

# example B.28 of section B.3.2 
# (example B.28 of section B.3.2)  : Important statistical concepts : Examples of the statistical view of data : Omitted variable bias 
# Title: A better model 

print(summary(glm(data=s,
   FractionHumanAbsorption~week+log(Caco2A2BPapp),
   family=binomial(link='logit'))))
## Warning: non-integer #successes in a binomial glm!
## 
## Call:
## glm(formula = FractionHumanAbsorption ~ week + log(Caco2A2BPapp), 
##     family = binomial(link = "logit"), data = s)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.3474  -0.0568  -0.0010   0.0709   0.3038  
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)   
## (Intercept)         3.1413     4.6837    0.67   0.5024   
## week                0.1033     0.0386    2.68   0.0074 **
## log(Caco2A2BPapp)   0.5689     0.5419    1.05   0.2938   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 43.7821  on 99  degrees of freedom
## Residual deviance:  1.2595  on 97  degrees of freedom
## AIC: 47.82
## 
## Number of Fisher Scoring iterations: 6

