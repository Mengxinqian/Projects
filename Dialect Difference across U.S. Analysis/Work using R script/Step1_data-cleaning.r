# create a subset of the data in lingData.txt where all observations that
# omitted every question have been removed. Store the **number** of
# observations that you omitted as the variable <n.no.response>

survey.data <- read.table("lingData.txt", header=TRUE)
numric.data <- subset(survey.data, select=c(Q050:Q121))
no.response <- survey.data[apply(numric.data,1,function(row){sum(row)==0}),]
n.no.response <- nrow(no.response)

# plot a histogram of the number of omitted responses for each observation
# after removing observations that omitted all questions

clean.data <- survey.data[apply(numric.data,1,function(row){sum(row)!=0}),]
numric.clean.data <- subset(clean.data, select=c(Q050:Q121))
is.resp.data = numric.clean.data == 0
omitted.num = apply(is.resp.data, 1, sum)
hist(omitted.num, main="Number Omitted Questions", xlab="questions omitted")


# using your subset (with observations that responded to no questions
# removed) find the 99th percentile cutoff for number of questions
# omitted. Remove all observations with more than this number of omitted
# questions.

non.response.cutoff = quantile(omitted.num, 0.99)
cutoff.data = clean.data[omitted.num <= non.response.cutoff,]

# save the subset of remaining observations in a file named
# "ling-data-clean.data" 
write.table(cutoff.data, file = "ling-data-clean.data", row.names=F)


### START PART 2 ###

if (!require("maps")) {
  install.packages("maps")
}
if (!require("mapdata")) {
  install.packages("mapdata")
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
}
#if (!require("fpc")) {
#  install.packages("fpc")
#}

library(RColorBrewer)
library(maps)
library(mapdata)
#library(fpc)

#see if some questions are omitted more than others
num.omitted.by.question = sapply(numric.data, function(x) (sum(x==0)))
most.omitted.questions = sort(num.omitted.by.question, decreasing=T)[1:4]

#plot removed observations 
set1 = brewer.pal(length(most.omitted.questions), "Set1")
#map(database="state")
for(i in 1:length(most.omitted.questions)) {
    omitted.question = survey.data[, names(most.omitted.questions)[i]]
    omitted.question.observations = survey.data[which(omitted.question == 0),]
 #   points(omitted.question.observations$long, omitted.question.observations$lat, pch=20, col=set1[i])
}
#legend(x="bottomleft", legend=names(most.omitted.questions), pch=20, col=set1)

set.seed(47)
n.clusters = length(most.omitted.questions)
subset.omitted = survey.data[,c(names(most.omitted.questions), "lat", "long")]
cluster.labels.k = kmeans(subset.omitted[,1:n.clusters], centers=n.clusters, iter.max=5)$cluster
set1 = brewer.pal(n.clusters, "Set1")
map(database="state")
points(subset.omitted$long, subset.omitted$lat, pch=20, col=set1[as.factor(cluster.labels.k)])

#create vector of valid two letter state abbreviations
valid.states = "AL,AK,AZ,AR,CA,CO,CT,DE,FL,GA,HI,ID,IL,IN,IA,KS,KY,LA,ME,MD,MA,MI,MN,MS,MO,MT,NE,NV,NH,NJ,NM,NY,NC,ND,OH,OK,OR,PA,RI,SC,SD,TN,TX,UT,VT,VA,WA,WV,WI,WY"
valid.states = unlist(strsplit(valid.states, ","))

#count number of removed observations and number of total observations with respect to state
removed = rbind(no.response, clean.data[omitted.num > non.response.cutoff,])
removed.by.state = table(removed$STATE)
total.by.state = table(survey.data$STATE)

#clean data of invalid states
removed.by.state =removed.by.state[which(names(removed.by.state) %in% valid.states)] 
total.by.state =total.by.state[which(names(total.by.state) %in% valid.states)]

#calculate ratio of removed observations by state
ratio.removed.by.state = removed.by.state / total.by.state

#perform chi-square goodness-of-fit test to test if removed observations were sampled uniformly (in proportion to total number of observations per state) with respect to geography(state).
test.uniform = chisq.test(x=removed.by.state, p=total.by.state/sum(total.by.state))

#since p < 0.01, we can strongly reject the null hypothesis that the number of removed observations per state were sampled uniformly from each state.
p.value = test.uniform$p.value

#cells(states) with standardized residuals greater than 2 can be considered to have disproportionately large number of removed observations
non.uniform = removed.by.state[which(test.uniform$residuals > 2)]

