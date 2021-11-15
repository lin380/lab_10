
# Functions --------------------------------------------------------------------

data_dic_starter <- function(data, file_path) {
  # Function:
  # Creates a .csv file with the basic information
  # to document a curated dataset

  tibble(variable_name = names(data), # column with existing variable names
         name = "", # column for human-readable names
         description = "") %>% # column for prose description
    write_csv(file = file_path) # write to disk
}

print_pretty_table <- function(data, caption, n = 10) {
  # Function
  # Print data frames as pretty tables with captions

  data %>% # dataset
    slice_head(n = n) %>% # first n observations
    knitr::kable(booktabs = TRUE, # bold headers
                 caption = caption) # caption
}

plot_indicative_features <- function(nb_model, top_n = 25) {
  # Function
  # Retrieve the top features from a NB model
  # and plot them

  library(tidyverse) # data manipulation and plotting

  nb_model %>% # nb model
    coef() %>% # get the probability coefficients
    as_tibble(rownames = "feature", # add feature to tibble
              .name_repair = janitor::make_clean_names) %>% # clean names
    pivot_longer(cols = 2:ncol(.), # get the columns
                 names_to = "class", # add column names to class column
                 values_to = "probability") %>% # add values to probability
    group_by(feature) %>% # grouping parameter
    slice_max(probability, n = 1) %>% # get highest probability for each feature
    ungroup() %>% # remove grouping
    group_by(class) %>% # grouping parameter
    slice_max(probability, n = top_n) %>% # get top_n for each class
    ungroup() %>% # remove grouping
    ggplot(aes(x = reorder(feature, probability),
               y = probability,
               fill = class)) + # mappings
    geom_col(show.legend = FALSE) + # bar plot without legend
    facet_wrap(~class, scales = "free_y") + # create separate plot for each class
    coord_flip() + # switch x/y axis
    labs(x = "Features", y = "Probability weights", title = paste("Top", top_n, "features"))
}


evaluate_training_model <- function(nb_model, classes) {
  # Function
  # Extract model performance metrics for training models
  # created by the textmodel_nb() function in the
  # quanteda.textmodels package

  library(tidyverse) # data manipulation

  model_predictions <-
    predict(nb_model, type = "prob") %>% # get the predicted document scores
    as.data.frame() %>% # convert to data frame
    mutate(document = rownames(.)) %>% # add the document names to the data frame
    as_tibble() %>% # convert to tibble
    pivot_longer(cols = all_of(classes), # convert from wide to long format
                 names_to = "prediction", # new column for ham/spam predictions
                 values_to = "probability") %>% # probability scores for each
    group_by(document) %>% # group parameter by document
    slice_max(probability, n = 1) %>% # keep the document row with highest probability
    slice_head(n = 1) %>% # for predictions that were 50/50
    ungroup() %>% # remove grouping parameter
    mutate(doc_id = str_remove(document, "text") %>% as.numeric) %>% # clean up document column so it matches doc_id in
    arrange(doc_id) # order by doc_id

  model_predictions_actual <-
    cbind(actual = nb_model$y, model_predictions) %>% # column-bind actual classes
    select(doc_id, document, actual, prediction, probability) # organize variables

  tab_class <-
    table(model_predictions_actual$actual, # actual class labels
          model_predictions_actual$prediction) # predicted class labels

  caret::confusionMatrix(tab_class, mode = "prec_recall") # model performance statistics
}
