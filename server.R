server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    data = NULL,
    structured_data = NULL,
    schema_count = 0
  )

  # 1. File Upload and Preview

  observeEvent(input$file_upload, {
    req(input$file_upload)

    tryCatch(
      {
        file_path <- input$file_upload$datapath
        file_ext <- tools::file_ext(input$file_upload$name)

        # Read file based on extension
        rv$data <- if (file_ext == "csv") {
          read_csv(file_path, show_col_types = FALSE)
        } else if (file_ext %in% c("xlsx", "xls")) {
          read_excel(file_path)
        } else {
          NULL
        }

        if (!is.null(rv$data)) {
          showNotification("ファイルが正常に読み込まれました", type = "message")
        }
      },
      error = function(e) {
        showNotification(paste("エラー:", e$message), type = "error")
        rv$data <- NULL
      }
    )
  })

  # Display input data preview
  output$input_preview <- renderDT({
    req(rv$data)
    datatable(
      rv$data,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = "Bfrtip"
      ),
      class = "display nowrap"
    )
  })

  # 2. Target Column Selection

  output$target_column_ui <- renderUI({
    req(rv$data)
    selectInput(
      "target_column",
      "対象列を選択",
      choices = names(rv$data),
      selected = names(rv$data)[1]
    )
  })

  # 3. Dynamic Schema Definition Forms

  # Add default schema form on startup
  observe({
    if (rv$schema_count == 0) {
      rv$schema_count <- rv$schema_count + 1
      schema_id <- rv$schema_count

      insertUI(
        selector = "#schema_forms",
        where = "beforeEnd",
        ui = div(
          id = paste0("schema_", schema_id),
          class = "well well-sm",
          style = "background-color: #f9f9f9; margin-bottom: 10px; padding: 10px;",
          fluidRow(
            column(
              10,
              h5(paste("列", schema_id), style = "margin-top: 0;")
            ),
            column(
              2,
              actionButton(
                paste0("remove_", schema_id),
                icon("trash"),
                class = "btn-danger btn-xs pull-right",
                style = "margin-top: 0;"
              )
            )
          ),
          textInput(
            paste0("col_name_", schema_id),
            "列名",
            placeholder = "例: 年齢"
          ),
          selectInput(
            paste0("var_type_", schema_id),
            "変数の型",
            choices = c("文字列", "数値", "因子")
          ),
          conditionalPanel(
            condition = sprintf("input.var_type_%d == '因子'", schema_id),
            textInput(
              paste0("enum_values_", schema_id),
              "因子の水準（カンマ区切り）",
              placeholder = "例: 良い, 普通, 悪い"
            )
          ),
          textAreaInput(
            paste0("prompt_", schema_id),
            "抽出の指示（オプション）",
            placeholder = "例: テキストから年齢を数値で抽出してください",
            rows = 2
          ),
          checkboxInput(
            paste0("force_na_", schema_id),
            "NA を強制",
            value = FALSE
          )
        )
      )
    }
  })

  # Add new schema column form
  observeEvent(input$add_column, {
    rv$schema_count <- rv$schema_count + 1
    schema_id <- rv$schema_count

    insertUI(
      selector = "#schema_forms",
      where = "beforeEnd",
      ui = div(
        id = paste0("schema_", schema_id),
        class = "well well-sm",
        style = "background-color: #f9f9f9; margin-bottom: 10px; padding: 10px;",
        fluidRow(
          column(
            10,
            h5(paste("列", schema_id), style = "margin-top: 0;")
          ),
          column(
            2,
            actionButton(
              paste0("remove_", schema_id),
              icon("trash"),
              class = "btn-danger btn-xs pull-right",
              style = "margin-top: 0;"
            )
          )
        ),
        textInput(
          paste0("col_name_", schema_id),
          "列名",
          placeholder = "例: 年齢"
        ),
        selectInput(
          paste0("var_type_", schema_id),
          "変数の型",
          choices = c("文字列", "数値", "因子")
        ),
        conditionalPanel(
          condition = sprintf("input.var_type_%d == '因子'", schema_id),
          textInput(
            paste0("enum_values_", schema_id),
            "因子の水準（カンマ区切り）",
            placeholder = "例: 良い, 普通, 悪い"
          )
        ),
        textAreaInput(
          paste0("prompt_", schema_id),
          "抽出の指示（オプション）",
          placeholder = "例: テキストから年齢を数値で抽出してください",
          rows = 2
        ),
        checkboxInput(
          paste0("force_na_", schema_id),
          "NA を強制",
          value = FALSE
        )
      )
    )
  })

  # Remove schema column form
  observe({
    lapply(1:rv$schema_count, function(schema_id) {
      observeEvent(input[[paste0("remove_", schema_id)]],
        {
          removeUI(selector = paste0("#schema_", schema_id))
        },
        ignoreInit = TRUE
      )
    })
  })

  # Collect schema data from inputs
  get_schema_data <- reactive({
    schema_list <- list()

    for (i in 1:rv$schema_count) {
      if (!is.null(input[[paste0("col_name_", i)]])) {
        schema_list[[i]] <- list(
          id = i,
          column_name = input[[paste0("col_name_", i)]],
          var_type = input[[paste0("var_type_", i)]],
          enum_values = ifelse(
            is.null(input[[paste0("enum_values_", i)]]),
            NA_character_,
            input[[paste0("enum_values_", i)]]
          ),
          system_prompt = ifelse(
            is.null(input[[paste0("prompt_", i)]]),
            "",
            input[[paste0("prompt_", i)]]
          ),
          force_na = isTRUE(input[[paste0("force_na_", i)]])
        )
      }
    }

    if (length(schema_list) == 0) {
      return(NULL)
    }

    bind_rows(schema_list)
  })

  # 4. Execute Structured Data Extraction

  observeEvent(input$execute_btn, {
    req(rv$data)
    req(input$target_column)

    schema_df <- get_schema_data()

    # Validation
    if (is.null(schema_df) || nrow(schema_df) == 0) {
      showNotification("構造化列を少なくとも1つ定義してください", type = "error")
      return()
    }

    if (any(schema_df$column_name == "" | is.na(schema_df$column_name))) {
      showNotification("すべての列に名前を指定してください", type = "error")
      return()
    }

    # Check for duplicate column names with existing data
    existing_columns <- names(rv$data)
    duplicate_cols <- schema_df$column_name[schema_df$column_name %in% existing_columns]

    if (length(duplicate_cols) > 0) {
      showNotification(
        paste0(
          "既に存在する列名が指定されています: ", paste(duplicate_cols, collapse = ", "),
          "\n異なる列名を指定してください。"
        ),
        type = "error",
        duration = 10
      )
      return()
    }

    # Check for duplicate column names within schema
    if (any(duplicated(schema_df$column_name))) {
      dup_names <- schema_df$column_name[duplicated(schema_df$column_name)]
      showNotification(
        paste0("構造化列で重複した列名があります: ", paste(unique(dup_names), collapse = ", ")),
        type = "error",
        duration = 10
      )
      return()
    }

    # Show processing notification
    showNotification("構造化処理を開始しています...", type = "message", duration = NULL, id = "processing")

    tryCatch(
      {
        # Generate schema
        schema <- generate_schema(schema_df)

        # Prepare data
        target_data <- rv$data

        # Get prompts from target column
        prompts_text <- target_data[[input$target_column]]

        # Add system prompt if provided
        if (!is.null(input$system_prompt) && nchar(trimws(input$system_prompt)) > 0) {
          prompts_text <- paste(input$system_prompt, prompts_text, sep = "\n\n")
        }

        # Execute structured data extraction
        showNotification("LLMによる構造化処理を実行中...", type = "message", duration = NULL, id = "llm_processing")

        result <- parallel_chat_structured(
          chat = chat_google_gemini(),
          prompts = as.list(prompts_text),
          type = schema
        )

        # Convert result types
        result <- convert_result_types(result, schema_df)

        # Bind with original data
        rv$structured_data <- bind_cols(target_data, result)

        # Switch to result tab
        updateTabsetPanel(session, "main_tabs", selected = "構造化結果")

        removeNotification(id = "processing")
        removeNotification(id = "llm_processing")
        showNotification("構造化処理が完了しました", type = "message")
      },
      error = function(e) {
        removeNotification(id = "processing")
        removeNotification(id = "llm_processing")
        showNotification(paste("エラーが発生しました:", e$message), type = "error", duration = 10)

        # Display error in processing info
        output$processing_info <- renderPrint({
          cat("エラー詳細:\n")
          cat(e$message)
        })
      }
    )
  })

  # Display structured result
  output$result_table <- renderDT({
    req(rv$structured_data)
    datatable(
      rv$structured_data,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = "Bfrtip"
      ),
      class = "display nowrap"
    )
  })

  # Display processing information
  output$processing_info <- renderPrint({
    req(rv$structured_data)

    cat("処理情報:\n")
    cat("- 処理行数:", nrow(rv$structured_data), "\n")
    cat("- 元データ列数:", ncol(rv$data), "\n")
    cat("- 追加された列数:", ncol(rv$structured_data) - ncol(rv$data), "\n")

    # Check for NA values in structured columns
    schema_df <- get_schema_data()
    if (!is.null(schema_df)) {
      cat("\n構造化列のNA数:\n")
      for (col_name in schema_df$column_name) {
        if (col_name %in% names(rv$structured_data)) {
          na_count <- sum(is.na(rv$structured_data[[col_name]]))
          cat(sprintf(
            "  %s: %d (%.1f%%)\n",
            col_name,
            na_count,
            100 * na_count / nrow(rv$structured_data)
          ))
        }
      }
    }
  })

  # 5. Download Handlers

  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("structured_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(rv$structured_data)
      write_csv(rv$structured_data, file)
    }
  )

  output$download_xlsx <- downloadHandler(
    filename = function() {
      paste0("structured_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$structured_data)
      write_xlsx(rv$structured_data, file)
    }
  )
}
