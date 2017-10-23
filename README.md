# R1-code
############### Part 1: Simulating node splitting in decision tree ###########

# IMP!!!:load the data "entropy_data.RData" that will be used for this part of exercise
# data is loaded as entropy.data
# last column "buys" is a class label
load("entropy_data.RData")
head(entropy.data)

# class frequencies (distribution)
table(entropy.data$buys)

## ----------------------------------- ##
## Function to calculate entropy
## ----------------------------------- ##
# This function takes class distribution and calculates entropy
# Argument: class.freq, number of observations belonging to differnt class categories 
# example: for class attribute "buy", class.freq would be
#         no yes 
#         5   9 
# returns: entropy of a node (assuming that all observations are members of a tree node)

getEntropy <- function(class.freq){
  
  # First convert class frequency into class probabilities (p) by dividing 
  # frequencies by total number of observations
  p <- class.freq/sum(class.freq)
  
  # Use formula to compute entropy using class probabilities "p"
  
  # *** ATTENTION !!! *** #
  entropy <- -1 * p * log2(p)
  
  # Probability p = 0 creates NaN value
  # we need to remove such value before computing sum
  # is.nan() returns TRUE if value is NaN
  entropy_final <- sum(entropy[!is.nan(entropy)])
  
  return(entropy_final)
}

# call the function to compute entropy, using class distribution of given data
getEntropy(table(entropy.data$buys))
# 0.940286

# ------------------------------------------------------------------- #
# This function computes information gain when a node is split on a given attribute
# Arguments: split_var = attribute column to be evaluated to split examples of a node
#            class_var = vector of class labels
# returns:   information_gain
# ------------------------------------------------------------------- #
getInfoGain <- function(split_var, class_var){
  
  class.freq <- as.matrix(table(class_var))
  
  # Calculate entropy of this parent node using class.freq
  entropy_parent <- getEntropy(class.freq)
  
  # We want to split this parent node using an attribute (e.g. income).
  # Create contingency table using the attribute and class label
  attr.class.freq <- as.matrix(table(split_var, class_var))
  
  # For example: attr.class.freq for attribute "income" and class "buys"
  #         no yes
  # high    2   2
  # low     1   3
  # medium  2   4
  
  # We investigate the split with value of income as "high", "low" and "medium"
  # First row of attr.class.freq in example above gives class distribution of examples that have high income
  
  # Intialize the overall entropy of split to 0.
  entropy_split <- 0
  num_observations <- length(split_var)
  
  # First, calculate entropy of each child node
  # e.g. "high", "low" and "medium" income results in three child nodes that contain 
  # observations with respective income type.
  for(i in 1:nrow(attr.class.freq)){
    
    # each row in attr.class.freq is class frequency of one node
    # For example: class distribution of child node with "high" income
    #         no yes
    # high    2   2 
    
    # *** ATTENTION !!! *** #
    child.node.freq <- sum(attr.class.freq[j,])
    
    # In order to find entropy of the split, 
    # first calculate entropy in each child node
    child.node.entropy <- getEntropy(child.node.freq)
    
    
    # number of examples in this child node
    num_obs_child.node <- sum(child.node.freq)
    # Add child node entropies weighted by fraction of examples they receive from parent node.
    
    # *** ATTENTION !!! *** #
    entropy_split <- entropy_split + ((child.node.freq/num_obs_child.node)*child.node.entropy)
  }
  # print(entropy_split)
  # Information gain is difference between parent node entropy and split entropy
  information_gain <- (entropy_parent - entropy_split)
  
  return(information_gain)
}


# test the function on categorical attributes, income, student and credit
getInfoGain(entropy.data$income, entropy.data$buys)
# 0.02922257
getInfoGain(entropy.data$student, entropy.data$buys)
# 0.1518355
getInfoGain(entropy.data$credit, entropy.data$buys)
# 0.04812703



# ---------------------------------------------------------------------- #
# Function that finds best attribute to split a given tree node
# Numeric attribute needs to be discretized and changed to categorical 
# Follow algorithm in instruction file to see how to do discretize

# Arguments: data.X = dataframe without class attribute
#            data.Y = vector of class labels

# Attribute that gives highest information gain on split is selected
# --------------------------------------------------------------------- #

findBestAttribute <- function(data.X, data.Y){
  
  number_of_columns <- ncol(data.X)
  attribute_names <- colnames(data.X)
  info_gains <- numeric(number_of_columns)
  best_cutoffs <- rep(NA, number_of_columns)
  
  for(i in 1:number_of_columns){
    
    # If an attribute is numeric, it has to be discretized (changed into categorical)
    if(class(data.X[,i]) == "numeric"){
      
      # find all the numeric values, order them in ascending order and select unique values
      x_sort_unique <- unique(sort(data.X[,i]))
      
      # for any two consecutive numbers, find the midpoint and use this midpoint to discretize
      # All observations with value for this attribute less than this midpoint gets "YES" category while rest gets "NO" 
      # Once all observations have value either "YES" or "NO", it can be treated as categorical
      
      max_info_gain_this_var <- 0
      best_cutoff_point <- NA
      
      for(j in 1:(length(x_sort_unique)-1)){
        
        # Calculate midpoint betweent two consecutive numeric values of numeric variable
        
        # *** ATTENTION !!! *** # ERROR!!!!!!!!!!!!!!!!!!!!!!11
        cutoff_point <- (x_sort_unique[j, 1] + x_sort_unique[j+1, 1])/2
        
        # Categorize all observations with numeric value less than cutoff as "YES" else "NO" 
        lessthan_cutoff <- ifelse(data.X[,i] <= cutoff_point, "YES", "NO")
        # Compute information gain on descritized numeric variable (now categorical) "lessthan_cutoff"
        
        # *** ATTENTION !!! *** #
        info_gain_for_this_split <- getEntropy (data.Y) - getInfoGain(lessthan_cutoff, data.Y)
        
        # If information gain is bettr than previous information gain, set it as new best
        if(info_gain_for_this_split > max_info_gain_this_var){
          max_info_gain_this_var <- info_gain_for_this_split
          best_cutoff_point <- cutoff_point
        }
      }
      info_gains[i] <- max_info_gain_this_var
      best_cutoffs[i] <- best_cutoff_point
      
    }else{ 
      # If attribute is categorical, simply use contingency table to calculate info gain calling getInfoGain function
      info_gains[i] <- getInfoGain(data.X[,i], data.Y)
    }
  }
  
  return_df <- data.frame("Attributes"=attribute_names, "Info.Gain" = info_gains, "Cutoff.at" = best_cutoffs)
  
  return(return_df) 
}


# Call the function that calculates information gain for each attribute
# ---------------------------------------------------------------------
attr_info_gains <- findBestAttribute(data.X = entropy.data[,1:4], data.Y = entropy.data[,5])
print(attr_info_gains)




############# Part 2: Perceptron Learning Algorithm ###############

# In this part, we write our own function that implements perceptron algorithm
# IMPORTANT!!! Download "Perceptron_data.RData" file and load it in R environment
# We will train perceptron algorithm on this data (perceptron.data)

load("Perceptron_data.RData")
head(perceptron.data)

# plot the perceptron.data
# we need to find a plane that separates green from red points
plot(perceptron.data[,1:2], xlim = c(-1,1), ylim = c(-1,1), col = ifelse(perceptron.data$y == -1, "red", "green"))


# Function that implements Perceptron learning algorithm 
# Arguments: data.X is a dataset with all attributes but class attribute
#            data.Y is a vector of class labels
#            max.iter is a maximum number of allowed iterations
# -------------------------------------------------------------
my.perceptron <- function(data.X, data.Y, max.iter = 500){
  
  # Add X0, which is 1 for all examples
  X.new <- cbind(1, data.X) 
  
  # Initial weight vector with all 0 elements, note that the first element is w0
  w <- matrix(0, 1, ncol(X.new))
  
  # track itertaions, counter for number of iterations
  iterations <- 0
  
  # matrix to keep track of weight changes
  weight.trace <- X.new[numeric(0),]
  
  while(TRUE){
    # use matrix multiplication of X.new and w to calculate the hypothesis.
    # Hint: Make sure both are matrix and try to see how to get cross product of two matrices 
    
    # *** ATTENTION !!! *** #
    hypothesis <- as.matrix(X.new[1:3]) %*% t(w)
    
    # use the sign of hypothesis to predict new class label for each observation
    # if hypothesis is positive (greater or equal to zero) assign it a class 1 else -1
    label.new <- ifelse(hypothesis >= 0, 1, -1) 
    
    # if the new class labels from hypothesis are same as the true class labels (data.Y) 
    # for all observations, then stop iterating
    if(all(label.new == data.Y)){ 
      
      #return(list("final.weight" = w, "weight.trace" = weight.trace))
      # return(w)
      break
      
      # if number of iterations exceeds maximum limit, stop iterating
    }else if(iterations >= max.iter){
      
      # return(w)
      break 
      
    }else{ 
      # if the new class labels from hypothesis are not the same with the true class label, 
      # update the weight vector and continue the iteration. 
      
      # index for those that had incorrect predictions
      row.index.wrong.pred <- (label.new != data.Y)
      
      # randomly select one misclassified point
      set.seed(5)
      wrong.predicted.idx <- which(row.index.wrong.pred == TRUE)
      rand.sel.index <- sample(wrong.predicted.idx, 1)
      
      # update the weight vector using this randomly selected misclassified point
      iterations <- iterations + 1
      
      # *** ATTENTION !!! *** #
      w <- (w + ( as.matrix(X.new[1:3])[rand.sel.index,] * data.Y[rand.sel.index]))
      
      # save the intermediate weights to see the changes in weight vector after each iteration 
      weight.trace <- rbind(weight.trace, w)
    }   
  }
  
  # remove the row names
  rownames(weight.trace) <- NULL
  
  return(list("final.weight" = w, "iterations" = iterations, "weight.trace" = weight.trace))
} 

# use our function that implements perceptron algorithm 
model <- my.perceptron(data.X = perceptron.data[,1:2], data.Y = perceptron.data[,3])

# see the final value of weight vector
print(model$final.weight)
#  Values for final weight should be, (1, -5.829599, 8.167969)

# how many iterations it took to converge
print(model$iterations)

# plot the final splitting hyperplane discovered by the algorithm
plot(perceptron.data[,1:2], xlim = c(-1,1), ylim = c(-1,1), col = ifelse(perceptron.data$y == -1, "red", "green"), pch = 20)
intercept.final <- -(model$final.weight[1])/model$final.weight[3]
slope.final <- -(model$final.weight[2])/model$final.weight[3]
abline(as.numeric(intercept.final), as.numeric(slope.final), lwd = 2)


# plot split planes and see how the plane adjust at each iteration
# and gets closer and closer to the correct spliting hyperplane
plot(perceptron.data[,1:2], xlim = c(-1,1), ylim = c(-1,1), col = ifelse(perceptron.data$y == -1, "red", "green"), pch = 20)
for(i in 1:model$iterations){
  intercept.i <- -(model$weight.trace[i,1])/model$weight.trace[i,3]
  slope.i <- -(model$weight.trace[i,2])/model$weight.trace[i,3]
  abline(intercept.i, slope.i, col = "blue")
  Sys.sleep(1)
  if(i == model$iterations)
    abline(intercept.i, slope.i,lwd = 3)
}



############# Part 3: Cross Validation ###############
# Read the data from file
# Download "Heart Disease" dataset from UCI Machine Learning Repository
# URL https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data 
# Find details of data set in https://archive.ics.uci.edu/ml/datasets/Heart+Disease 

heart.disease.db <- read.table(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",
                               header = FALSE, sep = ",", na.strings = "?", strip.white = TRUE, 
                               stringsAsFactors = FALSE)

head(heart.disease.db)
# details of the dataset features
# -------------------------------- #
# sex       = (0 is female, 1 is male)
# cp        = chest pain type (1 -> typical angina,  2 -> atypical angina,  3 -> non-anginal, 4 -> asymptomatic)
# trestbps  = resting blood pressure
# chol      = serum cholestral in mg/dl
# fbs       = fasting blood sugar > 120 mg/dl is 1 otherwise 0
# restecg   = resting electrocardiographic result, 0 -> normal, 1 -> St-T wave abnormality, 2 -> probable or definite hypertropy
# thalach   = maximum heart rate achieved
# exang     = exercise induced angina (1 = yes, 0 = no)
# oldpeak   = ST depression induced by exercise relative to rest
# slope     = the slope of the peak exercise ST segment (1 -> upslopping, 2 -> flat, 3 -> downslopping)
# ca        = number of major vessels (0-3) covered by flourosopy
# thal      = (3 -> normal, 6 -> fixed defect, 7 -> reversible defect)
# class     = diagnosis of heart disease
# ---------------------------------- #

colnames(heart.disease.db) <- c("age", "sex", "cp", "trestbps", "chol",
                                "fbs", "restecg", "thalach", "exang",
                                "oldpeak", "slope", "ca", "thal", "class")


# Check for missing values in each column and remove the examples with missing values
apply(heart.disease.db, 2, function(x) sum(is.na(x)))
heart.disease.db <- na.omit(heart.disease.db)

# Format data properly
heart.disease.db$sex <- ifelse(heart.disease.db$sex == 1, "Male", "Female")
heart.disease.db$sex <- as.factor(heart.disease.db$sex)

heart.disease.db$cp <- as.factor(heart.disease.db$cp)

heart.disease.db$fbs <- ifelse(heart.disease.db$fbs == 1, ">120", "<120")
heart.disease.db$fbs <- as.factor(heart.disease.db$fbs)

heart.disease.db$exang <- ifelse(heart.disease.db$exang == 1, "yes", "no")
heart.disease.db$exang <- as.factor(heart.disease.db$exang)

heart.disease.db$slope <- as.factor(heart.disease.db$slope)

heart.disease.db$thal <- as.factor(heart.disease.db$thal)

# for all the patients with heart diseases, change the class label to 1 (combine 1,2,3 and 4 to 1)
# 1 = Heart Disease
# 0 = No heart Disease
index.heart.disease <- (heart.disease.db$class != 0) 
heart.disease.db$class[index.heart.disease] <- 1
heart.disease.db$class <- as.factor(heart.disease.db$class)
