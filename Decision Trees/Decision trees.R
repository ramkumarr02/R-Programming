# Working Directory and other Initial setup
    setwd("F:/Ram/Data Science/GitHub/M17/R/Decision Trees")
    
    # install.packages("tree")
    # install.packages("ISLR")
    
    rm(list = ls())
    ls()
    
    library(tree)
    library(ISLR)

    
    
# View Dataframe Carseats
    head(Carseats)
    str(Carseats)


        
# Create temp dataset to avoid messing up CarSeats    
    data_carseats = Carseats
    summary(data_carseats$Sales)
    attach(data_carseats)



# Convert Sales to Sales binary and attach it to dataframe        
    data_carseats$sales_binary = ifelse(Sales>mean(Sales),"High","Low")
    data_carseats$sales_binary = as.factor(sales_binary)
    table(sales_binary)




# Tree modelling
    tree_carseats = tree(sales_binary~. -Sales, data_carseats, method = "class")
    
    plot(tree_carseats)
    text(tree_carseats)
    
    tree_carseats
    summary(tree_carseats)



# Cross validation of the created tree
    cv_carseats =  cv.tree(tree_carseats,FUN = prune.misclass)
    cv_carseats
