library(shiny)
library(bslib)
library(ggplot2)
library(DT)

for (file in c("config.R", "helpers.R", "filters_E.R", "tools_B.R", "usage_C.R", "study_D.R")) {
  source(file.path("R", file), local = TRUE)
}
survey <- read_survey(PROJECT$data_file)
response_fields <- setdiff(names(survey), c("is_ai_user", "tool", "switch_group", "purpose"))
full_users <- eligible_users(survey)
centre <- mean(full_users$weekly_ai_hours)

ui <- fluidPage(
  theme = bs_theme(version = 5, bg = "#FFFFFF", fg = "#172840", primary = "#17345A",
    base_font = "Arial", heading_font = "Georgia", border_radius = "12px"),
  tags$head(
    tags$title(paste(PROJECT$title, "STA304 Research")),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg")),
  div(class = "site-shell",
    tags$aside(class = "global-sidebar", `aria-label` = "Shared survey filters and chart settings",
      filtersUI("filters", survey)),
    tags$main(class = "main-content",
      tabsetPanel(id = "main_nav", type = "tabs", selected = "overview",
        tabPanel("Overview", value = "overview",
          section_header("01 / OVERVIEW", "How do students choose their AI?",
            "Explore which AI tools STA304H5 respondents use most for coursework."),
          div(class = "panel",
            div(class = "chart-topline", h3("The tools students reach for")),
            plotOutput("overview_chart", height = "420px"),
            div(class = "interpretation", span(class = "eyebrow", "READING THIS VIEW"),
              textOutput("overview_reading")))),
        tabPanel("Tools & tasks", value = "tools", toolsUI("tools", survey)),
        tabPanel("Time & future choices", value = "usage", usageUI("usage", survey)),
        tabPanel("Findings", value = "study", studyUI("study")),
        tabPanel("Data explorer", value = "data",
          section_header("05 / DATA EXPLORER", "Survey dataset",
            "Browse the survey responses with their original field names. Scroll horizontally to see all columns."),
          div(class = "panel", uiOutput("data_scope"), DTOutput("responses")))
      ))
  )
)

server <- function(input, output, session) {
  filters <- filtersServer("filters", survey, with_reference = TRUE,
    overview_active = reactive(identical(input$main_nav, "overview")))
  filtered_data <- filters$data
  figure_palette <- filters$palette
  toolsServer("tools", filtered_data, before_tool = filters$before_tool,
    palette = figure_palette, primary_tool = filters$primary_tool)
  usageServer("usage", filtered_data, palette = figure_palette, primary_tool = filters$primary_tool)
  studyServer("study", filtered_data, centre)
  overview_summary <- reactive(category_summary(filters$overview_data(), "primary_tool"))
  output$overview_chart <- renderPlot({
    s <- overview_summary()
    validate(need(nrow(s) > 0, "No responses in this view. Select a primary AI tool or adjust the other filters."))
    make_category_plot(s, axis_max = 100, fill = figure_palette()[["ChatGPT"]]) +
      labs(x = "Share of respondents in this view (%)")
  }, res = 110, bg = "white")
  output$overview_reading <- renderText({
    s <- overview_summary()
    req(nrow(s) > 0)
    leaders <- s$label[s$n == s$n[1]]
    share <- pct(s$n[1], s$denominator[1])
    if (length(leaders) == 1) {
      sprintf("The most common response in this view is %s (%d of %d respondents, %s).",
        leaders, s$n[1], s$denominator[1], share)
    } else {
      sprintf("The most common responses in this view are %s. Each accounts for %d of %d responses (%s).",
        paste(leaders, collapse = ", "), s$n[1], s$denominator[1], share)
    }
  })
  output$data_scope <- renderUI(div(class = "chart-topline", h3("Survey responses"),
    span(class = "sample-badge", paste(nrow(filtered_data()), "responses in this view"))))
  output$responses <- renderDT({
    shown <- filtered_data()[, response_fields, drop = FALSE]
    text_fields <- which(response_fields %in%
      c("academic_purpose", "tool_choice_reason", "outside_use", "switch_reason", "stay_reason"))
    datatable(shown, rownames = FALSE, filter = "top", selection = "none",
      options = list(pageLength = 8, lengthMenu = list(c(8, 15, 30, -1), c("8", "15", "30", "All")),
        order = list(), scrollX = TRUE, scrollY = "46vh", scrollCollapse = TRUE,
        language = list(search = "Search responses:", emptyTable = "No responses match these filters."),
        columnDefs = list(list(targets = "_all", defaultContent = "NA"),
          list(targets = text_fields - 1L, className = "response-text"))))
  }, server = TRUE)
}

shinyApp(ui, server)
