# Local data file location.
file_location <- "C:/Users/ylja0/OneDrive/Documents/school/Radboud/Master/KW1-2/Bayesian networking/bayesian-networks/data.csv"

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

# Read the dataset
d <- read.csv(file_location,sep=";",header=TRUE)

## Wife age
Wife_age=d$Wife_age
plot_histogram(Wife_age, "Wife age")
abline(v=32, col="red") # Vertical line at the mean, age=32
abline(h=85, col="red") # Horizontal line at the (eyeballed) average frequency, f=75
# Binning
Wife_age[Wife_age>=38]<-3
Wife_age[Wife_age>=27]<-2
Wife_age[Wife_age>3]<-1
plot_histogram(Wife_age, "Wife age")

## Number of children
Number_children=d$Number_children
plot_histogram(Number_children, "Number of children")
# Binning
Number_children[Number_children<=1]<-1
Number_children[Number_children>1 & Number_children<4]<-2
Number_children[Number_children>=4]<-3
plot_histogram(Number_children, "Number of children")
