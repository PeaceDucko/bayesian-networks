# Install packages. Comment this out when they are already installed.
#install.packages("bnlearn")
#install.packages("dagitty")

library( bnlearn )
library( dagitty )

# Local data file location
file_location <- "data.csv"

# Use the given method for structure learning on the data
# Returns the resulting network as a bn object
learn_structure <- function(method, method_name="variable", print_net=TRUE, plot_net=TRUE){
  print(paste("Learning structure using", method_name))
  
  net <- method(d)
  
  if(print_net){
    print(net)
  }
  
  print("Executing cextend()")
  net <- cextend(net)
  
  if(print_net){
    print(net)
  }
  if(plot_net){
    plot(net)
  }
  
  return(net)
}

# Use the given method to compute the score for a given network
# Returns the score
score_net <- function(net, score_type){
  print(paste("Calculating score using", score_type))
  
  score <- score(net, d, type=score_type)
  
  return(score)
}

# Convert the given bn network into a DAG
# Returns a DAGitty DAG
bn_to_dag <- function(net){
  m <- modelstring(net)
  
  # For DAG A->B<-C, we have modelstring m = [A][C][B|A:C]
  
  m <- strsplit(m,         "^\\[")     # remove leading [
  m <- strsplit(m[[1]][2], "\\]$")     # remove trailing ]
  m <- strsplit(m[[1]],    "\\]\\[")   # actually split on ][
  
  # Now we have an actual list where every entry is of format Variable|Parent1:Parent2
  # The target is to get dag_string = 'dag{A->B C->B}'
  
  dag_string <- 'dag{ '
  
  for(entry in m[[1]]){
    # If this entry contains parents
    if(any(i <- grep("\\|", entry))){
      entry   <- strsplit(entry, "\\|")
      var     <- entry[[1]][1]         # Single variable
      parents <- entry[[1]][2]         # List of parent variables
      
      # Now we can add Parent->Variable to the dag_string for every parent
      parents <- strsplit(parents, ":")
      for(parent in parents[[1]]){
        dag_string <- paste(dag_string, parent, "->", var, " ", sep="")
      }
    }
  }
  
  dag_string <- paste(dag_string, '}', sep="")
  
  return(dagitty(dag_string))
}

# Compare the entries in the given list of DAGs
# Returns nothing, but prints the edges that differ between the DAGs
compare_dags <- function(dags){
  amount_dags <- length(dags)
  
  if(amount_dags < 2){
    return()
  }
  
  print(paste("Comparing",amount_dags,"DAGs"))
  
  # Get rid of every leading "{" and trailing "}"
  for(i in seq_along(dags)){
    dags[[i]] <- gsub("\\{", "", dags[[i]])
    dags[[i]] <- gsub("\\}", "", dags[[i]])
  }
  
  # For every DAG
  for(i in seq_along(dags)){
    current_dag <- dags[[i]]
    missing_found = list()
    # Loop over all DAGs (we only care about the others)
    for(j in seq_along(dags)){
      other_dag <- dags[[j]]
      # For every entry in the current DAG
      for(line in strsplit(current_dag, "\n")[[1]]){
        # If this entry does not appear in the other DAG, and this line has not been printed yet
        if(!any(k <- grep(line, other_dag)) && !is.element(line, missing_found)){
          missing_found <- c(missing_found, line)
          print(paste("DAG", i, "contains", line))
        }
      }
    }
  }
}

# Use the given method to compute the score for every entry in the given list of DAGs
# Returns the score (which is the same for every equivalent DAG apparently)
score_dags <- function(dags, score_type){
  print(paste("Calculating score using", score_type))
  
  compare_dags(dags)
  
  for(i in seq_along(dags)){
    dag <- dags[[i]]
    net <- model2network(toString(dag, "bnlearn"))
    score <- score(net, d, type=score_type)
    print(paste(paste("Score for DAG ", i, ":", sep=""), score))
  }
  
  return(score)
}

# Read the data from the csv file and process it
# Returns the processed dataframe
get_processed_data <- function(binning=TRUE, create_factors=TRUE, read_as_continuous=FALSE){
  
  if(read_as_continuous){
    d <- read.csv(file_location, sep=";", header=TRUE, colClasses=rep("double",4))
  }
  else{
    d <- read.csv(file_location, sep=";", header=TRUE)
  }
  
  # Binning
  if(binning){
    d$Wife_age[d$Wife_age>=38]<-3
    d$Wife_age[d$Wife_age>=27]<-2
    d$Wife_age[d$Wife_age>3]<-1
    
    d$Number_children[d$Number_children<=1]<-1
    d$Number_children[d$Number_children>1 & d$Number_children<4]<-2
    d$Number_children[d$Number_children>=4]<-3
  }
  
  # Create factors to discretize the data
  if(create_factors){
    d$Contraceptive_method <- as.factor(d$Contraceptive_method)
    d$Husband_education    <- as.factor(d$Husband_education)
    d$Husband_occupation   <- as.factor(d$Husband_occupation)
    d$Media_exposure       <- as.factor(d$Media_exposure)
    d$Number_children      <- as.factor(d$Number_children)
    d$Standard_of_living   <- as.factor(d$Standard_of_living)
    d$Wife_age             <- as.factor(d$Wife_age)
    d$Wife_education       <- as.factor(d$Wife_education)
    d$Wife_religion        <- as.factor(d$Wife_religion)
    d$Wife_working         <- as.factor(d$Wife_working)
  }

  return(d)
}

##    Start main program   ##

# Read the dataset
d <- get_processed_data() # Categorical data: read as-is, apply binning and factorizing
#d <- get_processed_data(binning=FALSE, create_factors=FALSE, read_as_continuous=TRUE) # Continuous data

score_type = "bic" # Bayesian Information Criterion (categorical data)
#score_type = "bge" # Bayesian Gaussian (continuous data)

# Constraint-based Pearson's Correlation conditional independence test
structure <- learn_structure(pc.stable, "PC.stable")
dag <- bn_to_dag(structure)
score_dags(equivalentDAGs(dag), score_type)

# Score-based Hill-Climbing
structure <- learn_structure(hc, "hc")
dag <- bn_to_dag(structure)
score_dags(equivalentDAGs(dag), score_type)

# Hybrid PC + HC
structure <- learn_structure(h2pc, "h2pc") # MMHC and RSMAX2 also work
dag <- bn_to_dag(structure)
score_dags(equivalentDAGs(dag), score_type)

# TABU
structure <- learn_structure(tabu, "tabu")
dag <- bn_to_dag(structure)
score_dags(equivalentDAGs(dag), score_type)
