## ----------------------------------------------------
## Prediction
## ----------------------------------------------------

m_predict <- function(m, data = NULL) {
    if (m_is_multi(m)) {
        stop("Not supported for multi tasks")
    }
    
    m <- m %>% m_split() %>% m_train() %>% m_run()
    
    if (!is.null(m$pre_process_param$error)) {
        stop(m$pre_process_param$error)
    }
    
    if (!is.null(m$train_recording$error)) {
        stop(m$train_recording$error)
    }
    
    if (is.null(data)) {
        data <- m$training_data
    }
    
    if (!is.null(m$engineer_params)) {
        data <- m_engineer_data(data, m$response, m$engineer_params)
    }
    if (!is.null(m$pre_process_param$result)) {
        data <- m_pre_process(data, m$response, m$pre_process_param$result)
    }

    divided <- m_divide(data, m$response)    
    train <- m$train
    predictions <- predict(train, newdata = divided$x)
    if (is.matrix(predictions)) {
        predictions <- predictions[, 1]
    }
    predictions
}

m_seperator <- function(m, seperator = NULL) {
    if (!is.null(seperator))
        m_set(m, seperator = seperator)
    else
        m_default(m, seperator = "_")
}

m_prediction <- function(m, prediction = NULL) {
    build <- function(response, seperator) {
        as.formula(paste0("~", lazyeval::as_name(response), seperator, "Prediction"))
    }
    m <- m %>% m_seperator()
    if (!is.null(prediction))
        m %>% m_set_formula(prediction = prediction)
    else
        m %>% m_set_formula(prediction = build, .overwrite = FALSE)
}

m_residual <- function(m, residual = NULL) {
    build <- function(response, seperator) {
        response_name <- lazyeval::as_name(response)
        as.formula(paste0("~", response_name, seperator, "Residual"))
    }
    m <- m %>% m_seperator()
    if (!is.null(residual))
        m %>% m_set_formula(residual = residual)
    else
        m %>% m_set_formula(residual = build, .overwrite = FALSE)
}

m_augment <- function(m, data = NULL) {
    
    if (m_is_multi(m)) {
        stop("Not supported for multi tasks")
    }
    
    m <- m %>% m_split() %>% m_prediction() %>% m_residual() %>% m_run()
    
    if (is.null(m$response)) {
        stop("Response variable not set")
    }
    
    if (is.null(data)) {
        data <- m$training_data
    }
    
    response <- m$response
    prediction <- m_predict(m, data)
    prediction_f <- m$prediction
    prediction_name <- as.character(lazyeval::as_name(prediction_f))
    y <- dplyr::select(data, m$response)[[1]]
    
    data[[prediction_name]] <- prediction
    
    if (is.numeric(prediction)) {
        residual_f <- m$residual
        residual_name <- as.character(lazyeval::as_name(residual_f))
        data[residual_name] <- y - prediction
    }
    # if (is.factor(prediction)) {
    #     accurate_name <- paste0(response_name, "_Accurate")
    #     data[accurate_name] <- y == prediction
    # }
    data
}

## ----------------------------------------------------
## Diagnostics
## ----------------------------------------------------

m_predictions_scatter_plot <- function(m, condition = NULL) {
    
    plotter <- function(m, view_condition = NULL) {
        
        augmented_data <- m_augment(m)
        if (is.null(view_condition)) 
            mapping <- aes_(x = m$prediction, y = m$response)
        else 
            mapping <- aes_(x = m$prediction, y = m$response, color = view_condition)
        p <-
            ggplot(augmented_data, mapping) +
            geom_point(na.rm = TRUE) +
            geom_abline(intercept = 0, slope = 1, col = "darkgrey", linetype = "dashed") +
            coord_fixed(ratio = 1) +
            ggtitle(m_name(m)) +
            xlab("Predicted") +
            ylab("Observed")
        p
    }
    
    m <- m %>% m_prediction() %>% m_residual() %>% m_run()
    
    if (!m_is_multi(m)) {
        m <- list(m)
    }
    
    m %>% purrr::map(plotter, view_condition = condition)
}

m_residuals_scatter_plot <- function(m, condition = NULL) {
    
    plotter <- function(m, view_condition = NULL) {
        augmented_data <- m_augment(m)
        if (is.null(view_condition)) 
            mapping <- aes_(x = m$prediction, y = m$residual)
        else 
            mapping <- aes_(x = m$prediction, y = m$residual, color = view_condition)
        p <-
            ggplot(augmented_data, mapping) +
            geom_point(na.rm = TRUE) +
            geom_hline(yintercept = 0, col = "darkgrey", linetype = "dashed") +
            ggtitle(m_name(m)) +
            xlab("Predicted") +
            ylab("Residual")
        p
    }
    
    m <- m %>% m_prediction() %>% m_residual() %>% m_run()
    
    if (!m_is_multi(m)) {
        m <- list(m)
    }
    
    m %>% purrr::map(plotter, view_condition = condition)
}


m_diagnostics_plot <- function(m) {
    m %>% m_predictions_scatter_plot() %>% m_residuals_scatter_plot()
}
