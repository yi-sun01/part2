#' Random forest cross-validation function
#'
#' This function performs a random forest cross-validation.
#'   predicts "body_mass_g" using covariates bill_length_mm, bill_depth_mm,
#'   flipper_length_mm.
#'
#' @param k number of folds.
#' @keywords prediction, inference
#'
#' @return A numeric with the cross-validation error.
#' 
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {

  # omit NA values of my_penguins data
  penguins_02 <- na.omit(my_penguins)
  # extract "body_mass_g"
  body_mass_outcome <- dplyr::pull(penguins_02, var = "body_mass_g")
  # only keep the the four covariates we want:
  # bill_length_mm, bill_depth_mm, flipper_length_mm, and body_mass_g
  penguins_02 <- dplyr::select(penguins_02, 3:6)

  # define the fold
  fold <- base::sample(base::rep(1:k, length = nrow(penguins_02)))

  # initialize a vector to store the prediction of each iteration
  pred <-  base::rep(NA, length(k))

  for (i in 1:k) {

    # split "penguins_02" into train and test set
    new_fold <- base::which(fold != i)
    data_train <- penguins_02[new_fold,]
    data_test <- penguins_02[-new_fold,]

    # split the true "body_mass_g" into train and test set
    body_mass_train <- body_mass_outcome[new_fold]
    body_mass_test <- body_mass_outcome[-new_fold]

    # train a random forest model with 100 trees
    MODEL <- randomForest::randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
                          data = data_train,
                          ntree = 100)

    # make predictions of "body_mass_g"
    PREDICTIONS <- stats::predict(MODEL, data_test[, -4])

    # compute the MSE
    pred[i] <- mean((body_mass_test - PREDICTIONS)^2)

  }

  return(sum(pred) / k)
}
