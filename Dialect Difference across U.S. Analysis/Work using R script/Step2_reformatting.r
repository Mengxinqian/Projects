library(RUnit)
errMsg <- function(err) print(err)
load('reformatting-tests.rda')

# Implement the makeBinary function.
# args:
# <response.row>: a vector of integers giving the response values for each
#   question 
# <n.responses>: a vector of integers (same length as <response.row>)
#   indicating the number of possible responses for each question
#
# returns:
# a binary vector that reformats the responses of <response.row> as
# described in project1.pdf

makeBinary <- function(response.row, n.responses) {
	v <- vector(mode="integer", length=0)
	for (i in 1:length(response.row)){
		x = rep(0, n.responses[i])
		n= response.row[i]
		x[n]=1
		v = c(v,x)		
	}
	return(v)
}


tryCatch(checkEquals(make.binary.test1, makeBinary(make.binary.rr1, make.binary.nr)), error=function(err) errMsg(err))

tryCatch(checkEquals(make.binary.test2, makeBinary(make.binary.rr2, make.binary.nr)), error=function(err) errMsg(err))

# use your "makeBinary" function to reformat your "ling-data-clean.data"
# dataset. Store this as a dataframe with variable names and order **as
# indicated in project1.pdf**. Save this dataframe as the file
# "binary-ling-data.data".

ling.clean.data = read.table("ling-data-clean.data",header=TRUE)
new1.data = subset(ling.clean.data, select=c(ID:ZIP,lat:long))
conduct.data = subset(ling.clean.data, select=c(Q050:Q121))
n.responses.vector = as.vector(sapply(conduct.data,max))
new2.data = data.frame(t(apply(conduct.data,1,function(x)(makeBinary(x,n.responses.vector)))))
col.names = colnames(conduct.data)
col.names.new = c()
for(i in 1:length(n.responses.vector)){
    if (n.responses.vector[i] > 0) {
	    col.names.new = c(col.names.new, paste(col.names[i], 1:n.responses.vector[i], sep = "."))
    }
}
colnames(new2.data) = col.names.new
new.data = cbind(new1.data,new2.data)
#attributes(new.data)$row.names = row.names(new.data)
write.table(new.data, file = "binary-ling-data.data", row.names=F)

test.data = read.table("binary-ling-data.data",header=TRUE)
checkEquals(test.data[1:10,], head.binary.data)


