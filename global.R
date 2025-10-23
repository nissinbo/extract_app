# Required packages
library(shiny)
library(tidyverse)
library(DT)
library(readxl)
library(openxlsx2)
library(ellmer)
library(shinyjs)

# Helper function: Generate ellmer schema from user input
generate_schema <- function(schema_df) {
  if (is.null(schema_df) || nrow(schema_df) == 0) {
    return(NULL)
  }
  
  schema_list <- list()
  
  for (i in seq_len(nrow(schema_df))) {
    col_name <- schema_df$column_name[i]
    var_type <- schema_df$var_type[i]
    enum_values <- schema_df$enum_values[i]
    
    schema_list[[col_name]] <- switch(
      var_type,
      "数値" = type_number(),
      "文字列" = type_string(),
      "因子" = {
        # Parse comma-separated enum values
        if (!is.na(enum_values) && nchar(trimws(enum_values)) > 0) {
          values <- strsplit(enum_values, ",")[[1]] %>% trimws()
          type_enum(values)
        } else {
          type_string()  # Fallback if no enum values provided
        }
      },
      type_string()  # Default
    )
  }
  
  do.call(type_object, schema_list)
}

# Helper function: Convert structured result to proper R types
convert_result_types <- function(result, schema_df) {
  if (is.null(result) || is.null(schema_df)) {
    return(result)
  }
  
  for (i in seq_len(nrow(schema_df))) {
    col_name <- schema_df$column_name[i]
    var_type <- schema_df$var_type[i]
    enum_values <- schema_df$enum_values[i]
    force_na <- schema_df$force_na[i]
    
    if (col_name %in% names(result)) {
      if (force_na) {
        # Force NA if checkbox is selected
        result[[col_name]] <- NA
      } else if (var_type == "因子") {
        # Convert to factor
        if (!is.na(enum_values) && nchar(trimws(enum_values)) > 0) {
          levels <- strsplit(enum_values, ",")[[1]] %>% trimws()
          result[[col_name]] <- factor(result[[col_name]], levels = levels)
        }
      } else if (var_type == "数値") {
        # Ensure numeric type
        result[[col_name]] <- as.numeric(result[[col_name]])
      }
    }
  }
  
  return(result)
}
