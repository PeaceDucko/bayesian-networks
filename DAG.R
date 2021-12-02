# Install packages. Comment this out when they are already installed.
#install.packages("dagitty")
#install.packages("xtable")
#install.packages("bnlearn")
#install.packages("lavaan")

library( dagitty )
library( xtable )
library( bnlearn )
library( lavaan )

# Local data file location.
file_location <- "C:/Users/ylja0/OneDrive/Documents/school/Radboud/Master/KW1-2/Bayesian networking/bayesian-networks/data.csv"

# The DAG we are going to test
dag_string = '
dag {
"Contraceptive_method" [pos="0,0"]
"Husband_education" [pos="0.3,-0.75"]
"Husband_occupation" [pos="0.5,-0.75"]
"Media_exposure" [pos="0,-0.25"]
"Number_children" [pos="-0.5,-0.25"]
"Standard_of_living" [pos="0.4,-0.5"]
"Wife_age" [pos="-0.5,-1"]
"Wife_education" [pos="0,-1"]
"Wife_working" [pos="0.1,-0.75"]
"Husband_education" -> "Standard_of_living"
"Husband_occupation" -> "Standard_of_living"
"Media_exposure" -> "Contraceptive_method"
"Number_children" -> "Contraceptive_method"
"Standard_of_living" -> "Contraceptive_method"
"Wife_age" -> "Number_children"
"Wife_age" -> "Contraceptive_method"
"Wife_education" -> "Media_exposure"
"Wife_working" -> "Media_exposure"
"Wife_education" -> "Husband_education"
"Wife_education" -> "Wife_working"
"Standard_of_living" -> "Media_exposure"
"Wife_education" -> "Husband_occupation"
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

Number_children=d$Nc # Would be d$Number_children when not abbreviating
Number_children[Number_children<=1]<-1
Number_children[Number_children>1 & Number_children<4]<-2
Number_children[Number_children>=4]<-3

# Execute the Chi-square tests and print+plot the results
r = localTests(g, d, type='cis.chisq')
r # Print table
plotLocalTestResults( r ) # This shows all rows of the table, but only 15 labels

print(xtable(r, type = "latex", digits=c(0,3,2,0,5,3,3)), file = "dag.tex")

# List of all abbreviated columns, excluding Wife_religion
cols = c("Cm","He","Ho","Me","Nc","Sl","Wa","We","Ww")
# Scale the data to standard deviation 1 in order to compare the coefficients
data_scaled <- as.data.frame(scale(d[,cols]))

# Testing with bnlearn
net <- model2network(toString(g,"bnlearn"))
bn.fit( net, data_scaled )

# Testing with lavaan
lvsem <- toString(g,"lavaan")
lvsem.fit <- sem(lvsem,data_scaled)
summary(lvsem.fit)
