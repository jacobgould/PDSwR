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

