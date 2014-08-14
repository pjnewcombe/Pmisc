require(ROCR)
require(Pmisc)

n.vars <- 10
n.folds <- 10

# Example predictions and outcomes (in matrix format)
predictions.list <- list()
predictions.list[[1]] <- matrix(rnorm(100,0,1),n.vars,n.folds)
predictions.list[[2]] <- matrix(rnorm(100,0,1),n.vars,n.folds)
true.outcomes <- matrix(rbinom(n=100,size=1,prob=0.5),n.vars,n.folds)

# Use function to construct ROCR objects for plotting
rocs <- FormatRocrObjects(predictions.list, true.outcomes)

# Plot
RocPlots(rocs)
