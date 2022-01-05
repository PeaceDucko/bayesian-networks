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

# pc.stable returns a CPDAG, so use cextend to get a DAG
cnet <- cextend(net)
cnet
plot(cnet)

# Score-based Hill-Climbing
net2 <- hc(d)
net2
plot(net2)

# Hybrid PC + HC
net3 <- h2pc(d) # MMHC and RSMAX2 also work
net3
plot(net3)

# TABU
net4 <- tabu(d)
net4
plot(net4)

type <- "bge"
score(cnet,d,type=type)
score(net2,d,type=type)
score(net3,d,type=type)
score(net4,d,type=type)
