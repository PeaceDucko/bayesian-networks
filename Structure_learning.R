# Install packages. Comment this out when they are already installed.
#install.packages("bnlearn")

library( bnlearn )

# Local data file location.
file_location <- "data.csv"

# Read the dataset
d <- read.csv(file_location, sep=";", header=TRUE, colClasses=rep("double",4))

# Constraint-based Pearson's Correlation conditional independence test
net <- pc.stable(d)
net
plot(net)

# Score-based Hill-Climbing
net2 <- hc(d)
net2
plot(net2)

# Hybrid PC + HC
net3 <- h2pc(d)
net3
plot(net3)

score(net2,d,type="bic") # Find fix for this, only works with discrete data