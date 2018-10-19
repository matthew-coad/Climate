## -------------------------
## Algortihms
## -------------------------

# Get a data frame listing all caret algorithms
m_get_algorithms <- function(uninstalled = FALSE) {
    
    core_algorithms <- 
        c("lm", "glm", "Penalized Linear Model" = "glmnet", "rlm", "pls", "pcr", "enet", "lars", "lda", "plr", "polr", 
          "nnet", "earth", "svmLinear", "svmPoly", "svmRadial", "knn", "qda", "nb", 
          "rpart", "M5", "M5Rules",  "treebag", "rf", "cubist", "ada", "eXtreme Gradient Boosting - Linear" = "xgbLinear", "eXtreme Gradient Boosting - Tree" = "xgbTree",
          "null", "OneR")
    
    core_names <- names(core_algorithms) %>% purrr::keep(~ . != "")
    label_overrides <- core_names %>% set_names(core_names %>% purrr::map(~ core_algorithms[.]))
    overridden_label <- function(algorithm, label) { 
        if_else(!is.na(label_overrides[algorithm]),label_overrides[algorithm],label) %>% set_names(NULL)
    }
    
    installed_packages <- installed.packages() %>% as_tibble() %>% select(Package) %>% .[[1]]
    model_installed <- function(info) {
        sum(info$library %in% installed_packages) == length(info$library)
    }

    infos <- caret::getModelInfo()
    algorithms <- names(infos)
    installed <- infos %>% purrr::map_lgl(model_installed)
    
    labels <- infos %>% purrr::map_chr("label")
    overridden_labels <- purrr::map2_chr(algorithms, labels, overridden_label)
    regression <- infos %>% purrr::map_lgl(~ "Regression" %in% .$type)
    classification <- infos %>% purrr::map_lgl(~ "Classification" %in% .$type)
    core <- algorithms %in% core_algorithms
    linear <- infos %>% purrr::map_lgl(~ "Linear Regression" %in% .$tags || "Linear Classifier" %in% .$tags)
    two_class_only <- infos %>% purrr::map_lgl(~ "Two Class Only" %in% .$tags)
    two_class <- classification
    multi_class <- classification & !two_class_only
    handle_missing <- infos %>% purrr::map_lgl(~ "Handle Missing Predictor Data" %in% .$tags)
    tree_based <- infos %>% purrr::map_lgl(~ "Tree-Based Model" %in% .$tags)
    rule_based <- infos %>% purrr::map_lgl(~ "Rule-Based Model" %in% .$tags)
    support_vector_machine <- infos %>% purrr::map_lgl(~ "Support Vector Machines" %in% .$tags)
    neural_network <- infos %>% purrr::map_lgl(~ "Neural Network" %in% .$tags)
    case_weights <- infos %>% purrr::map_lgl(~ "Accepts Case Weights" %in% .$tags)
    feature_selection <- infos %>% purrr::map_lgl(~ "Implicit Feature Selection" %in% .$tags)
    
    boosting <- infos %>% purrr::map_lgl(~ "Boosting" %in% .$tags)
    ensemble <- infos %>% purrr::map_lgl(~ "Ensemble Model" %in% .$tags)
    bagging <- infos %>% purrr::map_lgl(~ "Bagging" %in% .$tags)
    generalized_linear_model <- infos %>% purrr::map_lgl(~ "Generalized Linear Model" %in% .$tags)
    ordinal_outcomes <- infos %>% purrr::map_lgl(~ "Ordinal Outcomes" %in% .$tags)
    
    mars <- infos %>% purrr::map_lgl(~ "Multivariate Adaptive Regression Splines" %in% .$tags)
    discriminant_analysis  <- infos %>% purrr::map_lgl(~ "Discriminant Analysis" %in% .$tags)
    model_tree  <- infos %>% purrr::map_lgl(~ "Model Tree" %in% .$tags)
    bayesian_model <- infos %>% purrr::map_lgl(~ "Bayesian Model" %in% .$tags)
    logistic_regression <- infos %>% purrr::map_lgl(~ "Logistic Regression" %in% .$tags)
    L1_regularization <- infos %>% purrr::map_lgl(~ "L1 Regularization" %in% .$tags)
    L2_regularization <- infos %>% purrr::map_lgl(~ "L2 Regularization" %in% .$tags)
    
    robust_model <- infos %>% purrr::map_lgl(~ "Robust Model" %in% .$tags)
    partial_least_squares <- infos %>% purrr::map_lgl(~ "Partial Least Squares" %in% .$tags)
    random_forest <- infos %>% purrr::map_lgl(~ "Random Forest" %in% .$tags)
    gaussian_process <- infos %>% purrr::map_lgl(~ "Gaussian Process" %in% .$tags)

    quick <- !(boosting | ensemble | bagging | model_tree | neural_network | support_vector_machine | gaussian_process)

    known_tags <- c("Linear Regression", "Linear Classifier", "Two Class Only", 
                    "Handle Missing Predictor Data",  "Accepts Case Weights", "Implicit Feature Selection",
                    "Tree-Based Model", "Rule-Based Model", "Support Vector Machines", "Neural Network", "Boosting",
                    "Ensemble Model", "Generalized Linear Model", "Multivariate Adaptive Regression Splines", "Discriminant Analysis",
                    "Model Tree", "Bayesian Model", "Logistic Regression", 
                    "L1 Regularization", "L2 Regularization", "Ordinal Outcomes", "Bagging", "Robust Model", "Partial Least Squares", "Random Forest",
                    "Gaussian Process")

    tags <- infos %>% purrr::map_chr(~ paste(.$tags[!.$tags %in% known_tags], collapse = ", "))

    r <- tibble(
        algorithm = algorithms,
        label = overridden_labels,
        installed = installed,
        regression = regression,
        classification = classification,
        two_class = two_class,
        multi_class = multi_class,
        core = core,
        quick = quick,
        ordinal_outcomes = ordinal_outcomes,
        feature_selection = feature_selection,
        case_weights  = case_weights,
        handle_missing = handle_missing,
        linear = linear,
        tree_based  = tree_based,
        rule_based = rule_based,
        boosting = boosting,
        ensemble = ensemble,
        bagging = bagging,
        L1_regularization = L1_regularization,
        L2_regularization  = L2_regularization,
        robust_model = robust_model,

        model_tree = model_tree,
        logistic_regression = logistic_regression,
        partial_least_squares = partial_least_squares,
        random_forest = random_forest,
        svm = support_vector_machine,
        neural_net = neural_network,
        discriminant_analysis = discriminant_analysis,
        bayesian_model = bayesian_model,
        generalized_linear_model = generalized_linear_model,
        mars = mars,
        gaussian_process = gaussian_process,
        tags = tags
    )
    
    if (!uninstalled) {
        r <- r %>% filter(installed)
        
    }
    r
}

m_get_algorithm_tags <- function(algorithms) {
    infos <- caret::getModelInfo()
    info_names <- names(infos)
    info_tags <- function(info, name) {
        info$tags %>% purrr::map(~ list(algorithm = name, tag = .)) %>% bind_rows()
    }
    tags <- purrr::map2(infos, info_names, info_tags) %>% bind_rows()
    algorithms %>% select(algorithm) %>% inner_join(tags, by = "algorithm")
}

m_get_algorithm_libraries <- function(algorithms, uninstalled_only = TRUE) {
    
    installed_packages <- installed.packages() %>% as_tibble() %>% select(Package) %>% .[[1]]

    algorithm_libraries <- function(code) {
        libraries <- caret::getModelInfo(code, regex = FALSE)[[1]]$library
    }
    libraries <- algorithms$algorithm %>% purrr::map(algorithm_libraries) %>% unlist() %>% unique()
    if (uninstalled_only) {
        libraries <- libraries[!libraries %in% installed_packages]
    }
    libraries
}

m_get_install_algorithms_script <- function(algorithms, uninstalled_only = TRUE) {
    libraries <- get_algorithm_libraries(algorithms, uninstalled_only = uninstalled_only)
    if (length(libraries) == 0) {
        stop("Found no packages to install")
    }
    sprintf('install.packages(c("%s"))', paste(libraries, collapse = '", "'))
}

m_algorithm <- function(m, algorithm) {
    m_set(m, algorithm = algorithm)
}

m_cross_algorithms <- function(m, algorithms) {
    m %>%
        m_set_parameter("algorithm") %>%
        m_cross_apply(x = algorithms$algorithm %>% set_names(algorithms$algorithm), f = ~ .m %>% m_algorithm(.x))
}

## -------------------------
## Core Algortihms
## -------------------------

## -- Linear Regression ----

m_linear_regression_algorithm <- function(m) {
    m_algorithm(m, "lm")
}

m_generalized_linear_model_algorithm <- function(m)
    m_algorithm(m, "glm")

m_penalized_linear_model_algorithm <- function(m)
    m_algorithm(m, "glmnet")

m_robust_linear_regression_algorithm <- function(m)
    m_algorithm(m, "rlm")

m_partial_least_squares_algorithm <- function(m)
    m_algorithm(m, "pls")

m_principal_component_regression_algorithm <- function(m)
    m_algorithm(m, "pcr")

m_elastic_net_algorithm <- function(m)
    m_algorithm(m, "enet")

m_least_angle_regression_algorithm <- function(m)
    m_algorithm(m, "lars")

## -- Linear Classification Algorithms --

m_linear_discriminant_analysis_algorithm <- function(m)
    m_algorithm(m, "lda")

m_penalized_logistic_regression_algorithm <- function(m)
    m_algorithm(m, "plr")

## Ordered Logistic or Probit Regression
m_ordered_logistic_regression_algorithm <- function(m)
    m_algorithm(m, "polr")

m_logistic_regression_algorithm <- m_generalized_linear_model_algorithm


## --- Non Linear Algorithms ---

m_neural_network_regression_algorithm <- function(m)
    m_algorithm(m, "nnet")

m_multivariate_adaptive_regression_spline_algorithm <- function(m)
    m_algorithm(m, "earth")

m_support_vector_machine_linear_kernel_algorithm <- function(m)
    m_algorithm(m, "svmLinear")

m_support_vector_machine_polynomial_kernel_algorithm <- function(m)
    m_algorithm(m, "svmPoly")

m_support_vector_machine_radial_basis_kernel_algorithm <- function(m)
    m_algorithm(m, "svmRadial")

m_k_nearest_neighbors_algorithm <- function(m)
    m_algorithm(m, "knn")

## --- Non Linear Classification Algorithms ---

## Quadratic Discriminant Analysis
m_quadratic_discriminant_analysis_algorithm <- function(m)
    m_algorithm(m, "qda")

m_naive_bayes_algorithm <- function(m) {
    m_algorithm(m, "nb")
}

## --- Regression Trees ---

## CART
m_cart_algorithm <- function(m)
    m_algorithm(m, "rpart")

## Model trees
m_model_trees_algorithm <- function(m)
    m_algorithm(m, "M5")

## Model rules
m_model_rules_algorithm <- function(m)
    m_algorithm(m, "M5Rules")

## Ensembles

m_bagged_cart_algorithm <- function(m)
    m_algorithm(m, "treebag")

m_random_forest_algorithm <- function(m)
    m_algorithm(m, "rf")

m_cubist_algorithm <- function(m)
    m_algorithm(m, "cubist")

m_boosted_classification_trees_algorithm <- function(m) {
    m_algorithm(m, "ada")
}

m_extreme_gradient_boosting_linear_algorithm <- function(m) {
    m_algorithm(m, "xgbLinear")
}     

m_extreme_gradient_boosting_tree_algorithm <- function(m) {
    m_algorithm(m, "xgbTree")
}     

## --------------
## Validation
## --------------

m_no_validation <- function(m) {
    m_set(m, validation = list(method = "none"))
}

m_bootstrap_validation <- function(m, number = 10) {
    m_set(m, validation = list(method = "boot", number = number))
}

m_cross_validation <- function(m, number = 10) {
    m_set(m, validation = list(method = "cv", number = number))
}

m_repeated_cross_validation <- function(m, number = 10, repeats = 3) {
    m_set(m, validation = list(method = "repeatedcv", number = number, repeats = repeats))
}

m_loocv_validation <- function(m) {
    m_set(m, validation = list(method = "loocv"))
}

m_validation <- function(m) {
    m_default(m, validation = list(method = "none"))
}

## --------------
## Performance
## --------------

m_performance <- function(m) {
    m_default(m, performance  = "defaultSummary")
}

m_default_performance <- function(m) {
    m_set(m, performance  = "defaultSummary")
}

m_two_class_performance <- function(m) {
    m_set(m, performance = "twoClassSummary", class_probability = TRUE)
}

m_multi_class_performance <- function(m) {
    m_set(m, performance  = "multiClassSummary", class_probability = TRUE)
}

m_log_loss_performance <- function(m) {
    m_set(m, performance  = "mnLogLoss", class_probability = TRUE)
}

m_precision_recall_performance <- function(m) {
    m_set(m, performance  = "prSummary")
}

## --------------
## Train control
## --------------

m_train_control <- function(m) {

    build <- function(validation, class_probability = FALSE, summary_function = "defaultSummary") {
        validation_method <- validation$method
        validation_args <- validation[names(validation) != "method"]
        
        train_control_args <- list()
        if (!is_empty(validation_method)) {
            train_control_args <- train_control_args %>% append(list(method = validation_method)) %>% append(validation_args)
        }
        summaryFunction <- get(summary_function)
        train_control_args <- append(train_control_args, list(classProbs = class_probability, summaryFunction = summaryFunction)) %>% purrr::compact()
        train_control <- do.call(caret::trainControl, train_control_args)
        train_control
    }
    m %>% m_validation() %>% m_set(train_control = build)
}

## ---------------
## Metric
## ---------------

m_rmse_metric <- function(m) {
    m_metric(m, "RMSE")
}

m_accuracy_metric <- function(m) {
    m_metric(m, "Accuracy")
}

m_metric <- function(m, metric = NULL) {
    build <- function(training_data, response) {
        training_y <- training_data %>% dplyr::select_(response)
        y <- training_y[[1]]
        if (is.numeric(y)) {
            return("RMSE")
        } else if (is.factor(y)) {
            return("Accuracy")
        }
        return(NULL)
    }
    if (!is.null(metric)) {
        m %>% m_set(metric = metric)
    } else {
        m %>% m_split() %>% m_default(metric = build)
    }
}

#m_roc_metric <- function(m = NULL)
    #m_update_value(
            #"metric",
            #m,
            #value_f = ~"ROC",
            #m_name_f = ~"roc") %>%
                #m_controller("roc", classProbs = TRUE, summaryFunction = caret::twoClassSummary)

#m_log_loss_metric <- function(m = NULL)
    #m_update_value(
            #"metric",
            #m,
            #value_f = ~"logLoss",
            #m_name_f = ~"log_loss") %>%
#m_controller("log_loss", classProbs = TRUE, summaryFunction = caret::mnLogLoss)


## ---------------
## Tuning/Parameters
## ---------------

m_tune_parameters <- function(m, tune_parameters) {
    m_set(m, tune_grid = tune_parameters %>% bind_cols())
}

m_tune_grid <- function(m, tune_grid) {
    m_set(m, tune_grid = tune_grid)
}

m_tune_length <- function(m, tune_length) {
    m_set(m, tune_length = tune_length)
}

## ---------------
## Train
## ---------------

# Request model training
m_train <- function(m, silent = FALSE) {

    build <- function(name, training_data, seed, algorithm, metric, train_control, response, tune_parameters = NULL, tune_length = NULL, tune_grid = NULL, engineered_data = NULL, pre_processed_data = NULL, cache = NULL) {
        
        if (!is.null(pre_processed_data)) {
            training_data <- pre_processed_data
        }
        else if (!is.null(engineered_data)) {
            training_data <- engineered_data
        }
        divide <- m_divide(training_data, response)
        training_x <- as.data.frame(divide$x)
        training_y <- divide$y
        
        train_args <- list(
            x = training_x,
            y = training_y[[1]],
            method = algorithm, 
            metric = metric, 
            tuneGrid = tune_grid,
            tuneLength = tune_length,
            trControl = train_control
        ) %>% purrr::compact()
        
        operation <- sprintf("Train %s", name)
        evaluator <- function(seed, train_args) {
            set.seed(seed)
            do.call(caret::train, train_args)
        } 
        recording <- record_value(operation, evaluator, seed = seed, train_args = train_args, cache = cache, silent = silent)
        recording
    }
    
    
    m %>%
        m_seed() %>%
        m_split() %>%
        m_engineer() %>%
        m_pre_processed_data() %>%
        m_validation() %>%
        m_performance() %>%
        m_metric() %>%
        m_train_control() %>%
        m_default(
            train_recording = build,
            train = function(train_recording) train_recording$result,
            tune_parameters = function(train = NULL) {
                if (!is.null(train)) {
                    train$bestTune %>% transpose() %>% flatten()
                }
                else {
                    NULL
                }
            },
            model = function(train_recording) train_recording$result$finalModel
        )
}
