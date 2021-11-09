# Install packages. Comment this out when they are already installed.
#install.packages("dagitty")
library( dagitty )

# Local data file location.
file_location <- "C:/Users/ylja0/OneDrive/Documents/school/Radboud/Master/KW1-2/Bayesian networking/bayesian-networks/data.csv"

# The DAG we are going to test
dag_string = '
dag {
"Contraceptive_method" [pos="-1.216,0.013"]
"Husband_education" [pos="-0.614,-1.330"]
"Husband_occupation" [pos="-0.234,-1.322"]
"Media_exposure" [pos="-1.218,-0.544"]
"Number_children" [pos="-1.748,-0.455"]
"Standard_of_living" [pos="-0.475,-0.787"]
"Wife_age" [pos="-1.767,-1.344"]
"Wife_education" [pos="-1.234,-1.109"]
"Wife_religion" [pos="-1.545,-1.347"]
"Wife_working" [pos="-0.929,-1.327"]
"Husband_education" -> "Standard_of_living"
"Husband_occupation" -> "Standard_of_living"
"Media_exposure" -> "Contraceptive_method"
"Number_children" -> "Contraceptive_method"
"Standard_of_living" -> "Contraceptive_method"
"Wife_age" -> "Number_children"
"Wife_education" -> "Media_exposure"
"Wife_religion" -> "Media_exposure"
"Wife_religion" -> "Wife_education"
"Wife_working" -> "Media_exposure"
"Wife_education" -> "Husband_education"
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

# Binning, could alternatively be done with cut()
Wife_age=d$Wa # Would be d$Wife_age when not abbreviating
Wife_age[Wife_age>=38]<-3
Wife_age[Wife_age>=27]<-2
Wife_age[Wife_age>3]<-1

# Execute the Chi-square tests and print+plot the results
r = localTests(g, d, type='cis.chisq')
r # Print table
plotLocalTestResults( r ) # This shows all rows of the table, but only 15 labels
plotLocalTestResults( r[0:5,] )
