ui <- fluidPage(
  useShinyjs(),
  titlePanel("フリーテキストから情報を抽出するアプリ"),
  sidebarLayout(
    # Sidebar Panel
    sidebarPanel(
      width = 4,

      # File upload
      h4("1. データアップロード"),
      fileInput(
        "file_upload",
        "CSV/XLSXファイルを選択",
        accept = c(".csv", ".xlsx")
      ),
      hr(),

      # Target column selection
      h4("2. 対象列選択"),
      uiOutput("target_column_ui"),
      hr(),

      # Structured columns definition
      h4("3. 構造化列の定義"),
      actionButton("add_column", "列を追加", icon = icon("plus"), class = "btn-primary btn-sm"),
      br(), br(),
      div(id = "schema_forms"),
      hr(),

      # System prompt
      h4("4. システムプロンプト"),
      textAreaInput(
        "system_prompt",
        NULL,
        value = "あなたはデータ分析の専門家です。以下のテキストから指定された情報を正確に抽出してください。",
        rows = 3,
        width = "100%"
      ),
      hr(),

      # Execute button
      actionButton(
        "execute_btn",
        "構造化を実行",
        icon = icon("play"),
        class = "btn-success btn-lg",
        width = "100%"
      )
    ),

    # Main Panel
    mainPanel(
      width = 8,
      tabsetPanel(
        id = "main_tabs",

        # Input data preview
        tabPanel(
          "入力データ",
          br(),
          h4("アップロードデータのプレビュー"),
          DTOutput("input_preview")
        ),

        # Structured result
        tabPanel(
          "構造化結果",
          br(),
          h4("構造化後のデータ"),

          # Download buttons
          fluidRow(
            column(
              6,
              downloadButton("download_csv", "CSV形式でダウンロード", class = "btn-info")
            ),
            column(
              6,
              downloadButton("download_xlsx", "XLSX形式でダウンロード", class = "btn-info")
            )
          ),
          br(),

          # Result table
          DTOutput("result_table"),
          br(),

          # Error/Info display
          verbatimTextOutput("processing_info")
        )
      )
    )
  )
)
