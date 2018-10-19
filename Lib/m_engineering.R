## ----------------------------
## Engineering
## ----------------------------

## Frame work 

m_engineer_params <- function(m) {
    
    build <- function(training_data, response = NULL, engineers = NULL) {
        if (is.null(engineers)) {
            return (NULL)
        }
        
        # Make sure Y is the last column
        divided <- m_divide(training_data, response)
        engineered_data <- bind_cols(divided$x, divided$y)

        params <- list()
        for (i in 1:length(engineers)) {
            i_builder <- engineers[[i]]
            i_param <- i_builder(engineered_data, response)
            
            engineered_data <- i_param(engineered_data)
        
            params <- append(params, i_param)
        }
        params
    }
    
    m %>%
        m_split() %>%
        m_default(engineer_params = build)
}

m_engineer_data <- function(data, response, engineer_params) {
    
    if (is.null(engineer_params)) {
        return(data)
    }
    engineered_data <- data
    for (i in 1:length(engineer_params)) {
        i_param <- engineer_params[[i]]
        engineered_data <- i_param(engineered_data)
    }
    engineered_data
}

m_engineer <- function(m) {
    build <- function(training_data, response = NULL, engineer_params = NULL) {
        if (is.null(engineer_params)) {
            return (NULL)
        }
        m_engineer_data(training_data, response, engineer_params)
    }
    m %>%
        m_split() %>%
        m_engineer_params() %>%
        m_default(engineered_data = build)
}

## Filter engineers

# Engineers that work by filtering out certain records

# Filters out any incomplete cases
m_complete_cases_filter <- function(m) {
    builder <- function(training_data, response) {
        function (data) {
            divided <- m_divide(data, response)
            idx <- complete.cases(divided$x)
            data[idx,]
        }
    }
    m_set(m, engineers = append(m$engineers, list(builder)))
}

## Pass through to dplyr::filter
m_filter <- function(m, ...) {
    builder <- function(training_data, response) {
        function (data) dplyr::filter_(data, ...)
    }
    m_set(m, engineers = append(m$engineers, list(builder)))
}

## -------------------
## Selection engineers
## -------------------

## Manual selection engineer
m_select <- function(m, ...) {
    builder <- function(training_data, response) {
        function (data) dplyr::select_(data, ...)
    }
    m_set(m, engineers = append(m$engineers, list(builder)))
}


# Engineer that selects well correlated variables
m_select_well_correlated  <- function(m, cutoff = .9) {
    builder <- function(training_data, response) {
        divided <- m_divide(training_data, response)
        engineered_x <- divided$x
        engineered_y <- divided$y
        numeric_names <- engineered_x %>% map(class) %>% keep(~ "numeric" %in% .) %>% names()
        cor <- cor(engineered_x[, numeric_names])
        drop_names <- numeric_names[caret::findCorrelation(cor, cutoff, names = TRUE)]
        drop_cols <- which(colnames(engineered_x) %in% drop_names)
        if (!is_empty(drop_cols))
            r <- function(data) data[, -drop_cols]
        else
            r <- function(data) data
        r
            
    }
    m_set(m, engineers = append(m$engineers, list(builder)))
}

# Engineer that selects predictors that have signification variation
# freqCut - the cutoff for the ratio of the most common value to the second most common value
# uniqueCut	- the cutoff for the percentage of distinct values out of the number of total samples
m_select_significant_variation <- function(m, freqCut = 95/5, uniqueCut = 10) {
    builder <- function(training_data, response) {
        divided <- m_divide(training_data, response)
        engineered_x <- divided$x
        engineered_y <- divided$y
        nearZeroIdx <- caret::nearZeroVar(engineered_x, freqCut, uniqueCut)
        if (!is_empty(nearZeroIdx))
            r <- function(data) data[, -nearZeroIdx]
        else
            r <- function(data) data
        r
    }
    m_set(m, engineers = append(m$engineers, list(builder)))
}

# Engineer that creates dummary variables
m_select_dummy_variables <- function(m, formula = ~., sep = ".", levelsOnly = FALSE, fullRank = TRUE) {
    builder <- function(training_data, response) {
        divided <- m_divide(training_data, response)
        engineered_x <- divided$x
        engineered_y <- divided$y
        param <- caret::dummyVars(formula, data = engineered_x, sep = sep, levelsOnly = levelsOnly, fullRank = fullRank)
        function(data)  {
            divided <- m_divide(data, response)
            engineered_x <- divided$x
            engineered_y <- divided$y
            engineered_x <- predict(param, newdata = engineered_x) %>% as_tibble()
            if (!is.null(engineered_y))
                bind_cols(engineered_x, engineered_y)
            else
                engineered_x
        }
    }
    m_set(m, engineers = append(m$engineers, list(builder)))
}


## Imputation engineers

# Impute missing values by using the median value
m_impute_median <- function(m, predictor) {
    builder <- function(training_data, response) {
        predictor_name <- as.character(lazyeval::as_name(predictor))
        median_x <- median(training_data[[predictor_name]], na.rm = TRUE)
        function(data) {
            predictors <- data[[predictor_name]]
            rows <- which(is.na(predictors))
            if (length(rows) > 0) {
                data[rows, predictor_name] <- rep(median_x, length(rows))
            }
            data
        }
    }
    m_set(m, engineers = append(m$engineers, list(builder)))
}


# Impute missing values by using the most common value
m_impute_mode <- function(m, predictor) {
    builder <- function(training_data, response) {
        predictor_name <- as.character(lazyeval::as_name(predictor))
        x <- training_data[[predictor_name]]
        ux <- unique(x)
        mode <- ux[which.max(tabulate(match(x, ux)))]
        function(data) {
            predictors <- data[[predictor_name]]
            rows <- which(is.na(predictors))
            if (length(rows) > 0) {
                data[rows, predictor_name] <- rep(mode, length(rows))
            }
            data
        }
    }
    m_set(m, engineers = append(m$engineers, list(builder)))
}


# Impute missing values by predicting by creating a model
m_impute_model <- function(m, predictor, model_f) {
    builder <- function(training_data, response) {
        task_name <- sprintf("Impute.%s", lazyeval::f_text(predictor))
        predictor_name <- as.character(lazyeval::as_name(predictor))
        impute_m <- m_task(task_name) %>% m_input_data(training_data) %>% m_response(predictor)
        impute_m <- lazyeval::f_eval(model_f, data=list(m = impute_m, .m = impute_m))
        stopifnot(m_is_task(impute_m))
        impute_m <- impute_m %>% m_train() %>% m_run()
        function(data) {
            predictors <- data[[predictor_name]]
            rows <- which(is.na(predictors))
            predictions <- m_predict(impute_m, data[rows, ])
            data[rows, predictor_name] <- predictions
            data
        }
    }
    m_set(m, engineers = append(m$engineers, list(builder)))
}

# m_polynomial_variables <- function(m, degree, sep = ".") {
#     
#     formula_builder <- function(data, response) {
#         names <- engineered_data %>% map(class) %>% keep(~ "numeric" %in% .) %>% names() %>% make.names()
#         degrees <- seq(1, degree)
#         terms <- expand.grid(name = names, degree = degrees) %>% transmute(term = sprintf("I(%s^%i)", name, degree)) %>% .$term
#         formula <- sprintf("~ %s", paste0(terms, collapse = " + "))
#         formula
#     }
# 
#     builder <- function(engineered_data) {
#         formula <- formula_builder(engineered_data)
#         param <- caret::dummyVars(formula, data = engineered_data)
#         r <- function(engineered_data) predict(param, engineered_data)
#         r
#     }
#     m_set(m, engineers = append(m$engineers, list(builder)))
# }
# 

m_mutate <- function(m, formula) {
    builder <- function(training_data, response) {
        function(data) lazyeval::f_eval(formula, data = list(data = data))
    }
    m_set(m, engineers = append(m$engineers, list(builder)))
}

