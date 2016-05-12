#  Script to create the cancer detection lesson in Machine Learning with R 
# 

# needed functon for use later on to normalize 
# used to normalize some outliers in the data

# needed packages....make sure you have these installed. 
library(class)
library(gmodels)



normalize <- function(x) {
  
  return (  (x - min(x)) / ( max(x) - min(x) ) )
  
}


wbcd <- read.csv("/home/alexander/prog/r/data/wisc_bc_data.csv", stringsAsFactors = FALSE)

# Remove the id column. 
wbcd <- wbcd[-1]


wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))

print(   round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1 )   )


print(   summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])  )

print("***** Notice the area_mean has outliers.....we will normalize them with the function from above *****")
cat("\n\n\n")


wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))



print(   summary(wbcd_n[c("radius_mean", "area_mean", "smoothness_mean")])   )
cat("\n\n\n")

print("Summary")
print( summary(wbcd_n$area_mean)   )
cat("\n\n")



# Create the training data 
wbcd_train <- wbcd_n[1:469, ]


# Create the test data
wbcd_test <- wbcd_n[470 : 569,  ]


# create the labels.
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels  <- wbcd[470 : 569, 1 ]



# Now for the good stuff....all the magic happens here.

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)


# Cross Table 
CrossTable( x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

