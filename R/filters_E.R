filtersUI <- function(id, data) {
  ns <- shiny::NS(id)
  max_hours <- max(data$weekly_ai_hours, na.rm = TRUE)
  demos <- DEMOGRAPHIC_COLUMNS[DEMOGRAPHIC_COLUMNS %in% names(data)]
  tools <- sort(unique(data$tool))
  shiny::tagList(
    shiny::h2("Explore the sample"),
    shiny::uiOutput(ns("scope")),
    shiny::tags$hr(),
    if (length(demos)) shiny::tagList(
      shiny::selectInput(ns("demographic"), "Demographic variable", choices = demos),
      shiny::uiOutput(ns("demographic_values"))
    ),
    shiny::conditionalPanel("input.main_nav === 'overview'", class = "overview-tool-filter",
      shiny::tags$label(class = "control-label", `for` = ns("overview_tool_menu"), "Primary AI tool"),
      shiny::div(class = "dropdown",
        shiny::tags$button(id = ns("overview_tool_menu"), type = "button", class = "form-select text-start",
          `data-bs-toggle` = "dropdown", `data-bs-auto-close` = "outside", `aria-expanded` = "false",
          shiny::textOutput(ns("overview_tool_label"), inline = TRUE)),
        shiny::div(class = "dropdown-menu overview-tool-menu",
          shiny::checkboxGroupInput(ns("overview_tools"), label = NULL, choices = tools, selected = tools),
          shiny::div(class = "overview-tool-actions",
            shiny::actionButton(ns("select_all_tools"), "Select all", class = "btn-link"),
            shiny::actionButton(ns("clear_tools"), "Clear all", class = "btn-link"))))),
    shiny::conditionalPanel("input.main_nav !== 'overview'",
      shiny::selectInput(ns("tool"), "Primary AI tool", choices = c("All", tools), selected = "All")),
    shiny::selectInput(ns("intention"), "Future tool choice",
      choices = c("All", "Stay", "Switch", "Not applicable")),
    shiny::sliderInput(ns("hours"), "Weekly AI use (hours)", min = 0,
      max = max_hours, value = c(0, max_hours), step = .5),
    shiny::checkboxInput(ns("keep_skipped"), "Include skipped usage questions", TRUE),
    shiny::selectInput(ns("figure_palette"), "Chart palette",
      choices = names(FIGURE_PALETTES), selected = DEFAULT_FIGURE_PALETTE),
    shiny::actionButton(ns("reset"), "Reset all filters", class = "reset-button")
  )
}

filtersServer <- function(id, data, with_reference = FALSE, overview_active = function() FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    demos <- DEMOGRAPHIC_COLUMNS[DEMOGRAPHIC_COLUMNS %in% names(data)]
    tools <- sort(unique(data$tool))
    output$demographic_values <- shiny::renderUI({
      shiny::req(input$demographic)
      values <- sort(unique(as.character(data[[input$demographic]])))
      values <- values[!is.na(values) & nzchar(values)]
      shiny::selectInput(session$ns("demographic_value"), "Include respondents", c("All", values))
    })
    # Figure 1 retains the comparison denominator before selecting a tool.
    before_tool <- shiny::reactive({
      apply_view_filters(data, "All", input$intention, input$hours,
        input$keep_skipped, input$demographic, input$demographic_value)
    })
    filtered <- shiny::reactive(apply_view_filters(before_tool(), tool = input$tool))
    overview_data <- shiny::reactive({
      d <- before_tool()
      d[d$tool %in% input$overview_tools, , drop = FALSE]
    })
    palette <- shiny::reactive({
      name <- input$figure_palette
      if (is.null(name)) name <- DEFAULT_FIGURE_PALETTE
      FIGURE_PALETTES[[name]]
    })
    output$overview_tool_label <- shiny::renderText({
      selected <- input$overview_tools
      switch(as.character(length(selected)),
        "0" = "No tools selected",
        "1" = selected,
        if (length(selected) == length(tools)) "All tools"
        else sprintf("%d tools selected", length(selected)))
    })
    output$scope <- shiny::renderUI({
      current <- if (overview_active()) overview_data() else filtered()
      shiny::div(class = "scope-counter", shiny::strong(nrow(current)),
        shiny::span(paste("of", nrow(data), "responses")))
    })
    shiny::observeEvent(input$select_all_tools,
      shiny::updateCheckboxGroupInput(session, "overview_tools", selected = tools))
    shiny::observeEvent(input$clear_tools,
      shiny::updateCheckboxGroupInput(session, "overview_tools", selected = character(0)))
    shiny::observeEvent(input$reset, {
      if (overview_active()) {
        shiny::updateCheckboxGroupInput(session, "overview_tools", selected = tools)
      } else {
        shiny::updateSelectInput(session, "tool", selected = "All")
      }
      shiny::updateSelectInput(session, "intention", selected = "All")
      shiny::updateSliderInput(session, "hours", value = c(0, max(data$weekly_ai_hours, na.rm = TRUE)))
      shiny::updateCheckboxInput(session, "keep_skipped", value = TRUE)
      shiny::updateSelectInput(session, "figure_palette", selected = DEFAULT_FIGURE_PALETTE)
      if (length(demos)) shiny::updateSelectInput(session, "demographic_value", selected = "All")
    })
    if (with_reference) {
      list(data = filtered, before_tool = before_tool, overview_data = overview_data,
        palette = palette, primary_tool = shiny::reactive(input$tool))
    } else {
      filtered
    }
  })
}
