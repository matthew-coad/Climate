## ---------------
## Evaluation
## ---------------

# Get data relevant to evaluation for a single task
m_get_task_data <- function(m) {
    
    stopifnot(m_is_task(m))
    stopifnot(!m_is_multi(m))
    stopifnot(!m_is_dirty(m))
    
    if (!is.null(m$pre_processed_data)) {
        data <- m$pre_processed_data
    }
    else if (!is.null(m$engineered_data)) {
        data <- m$engineered_data
    }
    else if (!is.null(m$training_data)) {
        data <- m$training_data
    }
    else {
        data <- NULL
    }
    stopifnot(!is.null(data))
    data
}


## Status of training tasks
m_get_train_status <- function(m) {
    
    success <- function(m) {
        r <- list(
            succeded = m$train_recording$succeded,
            error = ifelse(!is.null(m$train_recording$error), conditionMessage(m$train_recording$error), ""),
            errors = ifelse(!is.null(m$train_recording$error), 1, 0),
            messages = m$train_recording$messages,
            warnings = m$train_recording$warnings
        )
        r
    }
    timing <- function(m) {
        list(
            started = ifelse(!is.null(m$train_recording$started), format(m$train_recording$started), ""),
            finished = ifelse(!is.null(m$train_recording$finished), format(m$train_recording$finished), ""),
            duration = ifelse(!is.null(m$train_recording$started) && !is.null(m$train_recording$finished), format(m$train_recording$finished - m$train_recording$started), ""),
            seconds = ifelse(!is.null(m$train_recording$started) && !is.null(m$train_recording$finished), difftime(m$train_recording$finished, m$train_recording$started, units = "secs"), NA)
        )
    } 
    m %>% 
        m_as_task_list() %>% 
        purrr::keep(~ !is.null(.$train_recording)) %>% 
        m_as_task_list() %>% 
        m_query(success, timing)
}


## Training performance

m_get_performance <- function(m) {
    
    performance  <- function(l_i) {
        
        response <- l_i$response
        algorithm <- l_i$algorithm
        train <- l_i$train
        performance <- l_i$performance
        
        data <- m_get_evaluation_data(l_i)

        observed <- data %>% select(response) %>% .[[1]]
        predictions <- predict(train)
        if (is.matrix(predictions)) {
            predictions <- predictions[, 1]
        }
        performance_data <- data.frame(obs = observed, pred = predictions)
        
        evaluator <- 
            switch(performance,
                   "defaultSummary" = caret::defaultSummary, 
                   "twoClassSummary" = caret::twoClassSummary,
                   "multiClassSummary" = caret::multiClassSummary, 
                   "mnLogLoss" =  caret::mnLogLoss,
                   "prSummary" = caret::prSummary
            )
        performance <- evaluator(performance_data, NULL, algorithm) %>% as.list(r)
        performance
    }
    
    m %>% m_query(performance)
}


## Training resamples performance

m_get_resample_performance <- function(m) {
    l <- m %>%
        m_as_task_list() %>%
        purrr::keep(~ .$train_recording$succeded && !is.null(.$train$resample)) %>%
        m_as_task_list()
    resamples_builder <- function(l_i) {
        resamples <- as_tibble(l_i$train$resample)
        names <- list(name = rep(m_name(l_i), nrow(resamples)))
        bind_cols(names, resamples)
    }
    
    base <- l %>% m_query()
    resamples <- l %>% map_df(resamples_builder)
    base %>% dplyr::inner_join(resamples, by = "name")
}

## Training tuning performance

m_get_tune_performance <- function(m) {
    l <- m %>%
        m_as_task_list() %>%
        purrr::keep(~ .$train_recording$succeded && !is.null(.$train$resample)) %>%
        m_as_task_list()
    builder <- function(l_i) {
        performance <- l_i$train$results %>% tibble::rownames_to_column(var = "index") %>% mutate(index = as.integer(index))
        names <- list(name = rep(m_name(l_i), nrow(performance)))
        bind_cols(names, performance)
    }
    
    base <- l %>% m_query()
    performance <- l %>% map_df(builder)
    base %>% dplyr::inner_join(performance, by = "name")
}


## Variable Importance

m_get_variable_importance <- function(m) {

    l <- m %>%
        m_as_task_list() %>%
        purrr::keep(~ .$train_recording$succeded) %>%
        m_as_task_list()
    importance_builder <- function(l_i) {
        importance <- caret::varImp(l_i$train) %>% .$importance %>% tibble::rownames_to_column(var = "predictor")
        names <- list(name = rep(m_name(l_i), nrow(importance)))
        bind_cols(names, importance)
    }
    
    base <- m %>% m_query()
    results <- l %>% map_df(importance_builder)
    base %>% dplyr::inner_join(results, by = "name")
}

## Confusion Matrix

# m_confusion_matrix <- function(m) {
#     m <- m %>% m_prediction() %>% m_run()
#     augmented <- m_augment(m)
#     caret::confusionMatrix(dplyr::select(augmented, m$prediction)[[1]], dplyr::select_(augmented, m$response)[[1]])
# }
# 
# m_rank_correlation <- function(m) {
# 
#     build <- function(predicted_y, observed_y) {
#         m_as_evaluation(cor(predicted_y, observed_y, method = "spearman"))
#     }
# 
#     m %>% m_predicted_y() %>% m_observed_y() %>% m_default(rank_correlation = build)
# }
# 
# ## Calibration
# 
# m_calibration <- function(m) {
#     
#     build <- function(response, engineered_validation_x, validation_y, train = NULL) {
#         if (is.null(train)) {
#             return(NULL)
#         }
#         
#         training_probabilities <- predict(train, engineered_validation_x, type="prob")
#         lhs_name <- colnames(validation_y)[1]
#         rhs_name <- colnames(training_probabilities)[1]
#         formula <- f_new(as.name(rhs_name),lhs = as.name(lhs_name))
#         result <- caret::calibration(formula, data = bind_cols(validation_y, training_probabilities), cuts = 10)
#         result
#     }
#     
#     m %>% m_engineered_validation_x() %>% m_validation_y() %>% m_train() %>% m_default(calibration = build)
# }
# 
# m_calibrationplot <- function(m) {
#     
#     build <- function(calibration, title = NULL) {
#         ggplot(calibration) + geom_line() + ggtitle(title)
#     }
# 
#     m %>% m_calibration() %>% m_default(calibrationplot = build)
# }
