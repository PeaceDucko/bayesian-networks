# Install packages. Commend this out when not.
#install.packages("dagitty")
library( dagitty )

# Local data file location.
file_location <- "C:/Users/ylja0/OneDrive/Documents/school/Radboud/Master/KW1-2/Bayesian networking/bayesian-networks/data.csv"

# The DAG we are going to test
dag_string = '
dag {
"Contraceptive_method" [pos="-1.148,-0.509"]
"Husband_education" [pos="-0.530,-1.205"]
"Husband_occupation" [pos="-0.221,-1.205"]
"Media_exposure" [pos="-0.731,-0.506"]
"Number_children" [pos="-1.633,-0.527"]
"Standard_of_living" [pos="-0.723,-0.845"]
"Wife_age" [pos="-1.681,-1.208"]
"Wife_education" [pos="-1.146,-1.208"]
"Wife_religion" [pos="-1.488,-1.199"]
"Wife_working" [pos="-0.891,-1.208"]
"Husband_education" -> "Standard_of_living"
"Husband_occupation" -> "Standard_of_living"
"Media_exposure" -> "Contraceptive_method"
"Number_children" -> "Contraceptive_method"
"Standard_of_living" -> "Contraceptive_method"
"Standard_of_living" -> "Media_exposure"
"Wife_age" -> "Contraceptive_method"
"Wife_age" -> "Number_children"
"Wife_education" -> "Contraceptive_method"
"Wife_education" -> "Number_children"
"Wife_education" -> "Standard_of_living"
"Wife_religion" -> "Contraceptive_method"
"Wife_religion" -> "Number_children"
"Wife_working" -> "Contraceptive_method"
"Wife_working" -> "Standard_of_living"
}
'

# Read the dataset
d <- read.csv(file_location, sep=";", header=TRUE)

### Start replacing names with their abbreviated version
# Note that replacement happens based on the indices of the two arrays
old_names = c(
  "Contraceptive_method",
  "Husband_education","Husband_occupation",
  "Media_exposure","Number_children","Standard_of_living",
  "Wife_age","Wife_education","Wife_religion","Wife_working"
)
new_names = c("Cm","He","Ho","Me","Nc","Sl","Wa","We","Wr","Ww")

for(i in seq_along(old_names)){
  colnames(d)[colnames(d) == old_names[i]] <- new_names[i]
  dag_string <- gsub(old_names[i],new_names[i],dag_string)
}
###

# Create the graph
g <- dagitty(dag_string)
plot(g)

# Look at all implied conditional independencies of the DAG
impliedConditionalIndependencies(g)

# Check whether the names in the graph match those in the column headers
setdiff(names(g),colnames(d))

# Execute the Chi-square tests and plot the results
r = localTests(g, d, type='cis.chisq')
plotLocalTestResults( r )

plot_histogram <- function(variable,readable_name="variable"){
  hist_name = paste("Histogram for", readable_name)
  print(hist_name)
  
  dens = density(variable)
  plot(dens)
  polygon(dens, col="red", border="blue")
  
  h<-hist(variable, main=hist_name, xlab=readable_name)
  xfit<-seq(min(variable),max(variable))
  yfit<-dnorm(xfit,mean=mean(variable),sd=sd(variable))
  yfit<- yfit*diff(h$mids[1:2])*length(variable)
  lines(xfit,yfit,col="blue",lwd=2)
  
  print(paste("Mean:", mean(variable))) # Mean
  print(paste("Standard deviation:", sd(variable))) # Standard deviation
}

Wife_age=d$Wa # Would be d$Wife_age when not abbreviating

# Plot a histogram
plot_histogram(Wife_age, "Wife age")
abline(v=32, col="red") # Vertical line at the mean, age=32
abline(h=85, col="red") # Horizontal line at the (eyeballed) average frequency, f=75

# Binning, could alternatively be done with cut()
Wife_age[Wife_age>=38]<-3
Wife_age[Wife_age>=27]<-2
Wife_age[Wife_age>3]<-1
plot_histogram(Wife_age, "Wife age")

r = localTests(g, d, type='cis.chisq')
plotLocalTestResults( r ) # This prints the top 16 rows
plotLocalTestResults( r[5:10,] )

