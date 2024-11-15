## -- Input data --

m_input_data <- function(m, data = NULL) {
    name <- lazyeval::expr_text(m)
    if (!is.null(data)) {
        r <- m_set(m, input_data = data)
    } else {
        r <- m_task(name) %>% m_set(input_data = m)
    }
    r
}

## -- Seed --

# Random number seed used throughout the pipeline

# Set the random number 
m_seed <- function(m, seed = NULL) {
    if (!is.null(seed))
        m_set(m, seed = seed)
    else
        m_default(m, seed = 7)
}

## -- Divide --

## Dividing is the task of dividing the data into response and predictor portions

m_response <- function(m, response) {
    m %>% m_set_formula(response = response)
}

## The divide utility splits the input data into X and Y
m_divide <- function(data, response = NULL) {
    
    data_df <- as_tibble(data)
    if (!is.null(response)) {
        x <- dplyr::select(data_df, lazyeval::f_interp(~-uq(response)))
        y <- dplyr::select(data_df, response)
    } 
    else {
        x <- data_df
        y <- NULL
    }
    list(x = x, y = y)
}

## -- Split --

## Splitting is the task of dividing the data into training and validation sections

# Generates the test_index which is used to select testing data
# By default all data without a response variable set are considered test data
m_test_index <- function(m) {
    m_default(m,
        test_index = function(input_data, response = NULL) {
            if (is.null(response)) {
                return (NULL)
            }
            y <- dplyr::select(input_data, response) %>% .[[1]]
            r <- which(is.na(y))
            if (is_empty(r)) {
                return (NULL)
            }
            r
        }
    )
}

m_stratified_split <- function(m, p = 0.8) {
    m_seed(m) %>%
    m_test_index() %>%        
    m_set(
        split_p = p,
        data_index = function(input_data, response, seed, split_p, test_index = NULL) {
            if (!is.null(test_index)) {
                input_data <- input_data[-test_index,]
            }
            set.seed(seed)
            y <- input_data %>% dplyr::select(response)
            caret::createDataPartition(y[[1]], p = split_p, list = FALSE)[, 1]
        }
    )
}

m_split <- function(m) {
    
    build_training_data <- function(input_data, test_index = NULL, data_index = NULL) {
        if (!is.null(test_index)) {
            input_data <- input_data[-test_index,]
        }
        if (!is.null(data_index)) {
            input_data <- input_data[data_index,]
        }
        input_data %>% as_tibble()
    }
    build_validation_data <- function(input_data, test_index = NULL, data_index = NULL) {
        if (!is.null(test_index)) {
            input_data <- input_data[-test_index,]
        }
        if (is.null(data_index)) {
            return (NULL)
        }
        input_data[-data_index,] %>% as_tibble()
    }
    build_test_data <- function(input_data, test_index = NULL) {
        if (is.null(test_index)) {
            return (NULL)
        }
        input_data[test_index,] %>% as_tibble()
    }
    m %>% 
        m_test_index() %>% 
        m_default(
            training_data = build_training_data,
            validation_data = build_validation_data,
            test_data = build_test_data
        )
}

