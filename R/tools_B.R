toolsUI <- function(id, survey) {
  ns <- shiny::NS(id)
  purposes <- sort(unique(survey$purpose[survey$is_ai_user & !is.na(survey$purpose)]))
  shiny::tagList(
    section_header("02 / TOOLS & TASKS", "Primary AI tool by academic purpose",
      "Compare tool choices across the academic tasks reported by AI users."),
    shiny::div(class = "panel",
      shiny::div(class = "control-grid figure-controls",
        shiny::selectInput(ns("purpose"), "Academic purpose", c("All purposes" = "all", purposes)),
        shiny::radioButtons(ns("measure"), "Display values",
          c("Percentage" = "percent", "Count" = "count"), inline = TRUE)),
      shiny::div(class = "chart-topline", shiny::h3(shiny::textOutput(ns("title"), inline = TRUE)),
        shiny::uiOutput(ns("sample"))),
      shiny::div(class = "tool-chart-scroll", tabindex = "0", `aria-label` = "Figure 1 chart",
        shiny::plotOutput(ns("chart"), height = "420px")),
      shiny::div(class = "interpretation", shiny::span(class = "eyebrow", "READING THIS VIEW"),
        shiny::textOutput(ns("reading"))),
      shiny::checkboxInput(ns("show_table"), "Show the numbers behind this chart", FALSE),
      shiny::conditionalPanel("input.show_table", ns = ns,
        shiny::tableOutput(ns("summary_table")),
        shiny::downloadButton(ns("download"), "Download this summary", class = "secondary-button"))
    )
  )
}

purpose_tool_summary <- function(selected, reference, purpose = "") {
  eligible <- function(d) {
    d <- d[d$is_ai_user & !is.na(d$purpose), , drop = FALSE]
    if (nzchar(purpose) && purpose != "all") d <- d[d$purpose == purpose, , drop = FALSE]
    d
  }
  selected <- eligible(selected)
  reference <- eligible(reference)
  sizes <- table(reference$purpose)
  groups <- c("ChatGPT", "Claude", "Gemini", "Other AI")
  group <- ifelse(selected$tool %in% groups[1:3], selected$tool, "Other AI")
  shown_groups <- groups[groups %in% group]
  grid <- as.data.frame(table(
    Purpose = factor(selected$purpose, levels = rev(names(sizes))),
    Tool = factor(group, levels = shown_groups)), responseName = "Count")
  grid$Denominator <- as.integer(sizes[as.character(grid$Purpose)])
  grid$Percentage <- if (nrow(grid)) 100 * grid$Count / grid$Denominator else numeric(0)
  list(rows = grid, selected = nrow(selected), reference = nrow(reference))
}

purpose_tool_plot <- function(rows, percent, palette) {
  d <- rows[order(rows$Tool), , drop = FALSE]
  d$Value <- if (percent) d$Percentage else d$Count
  d$Label <- if (percent) paste0(round(d$Percentage, 1), "%") else as.character(d$Count)
  d$Midpoint <- ave(d$Value, d$Purpose, FUN = cumsum) - d$Value / 2
  limit <- if (percent) 100 else max(tapply(d$Value, d$Purpose, sum))
  label_min <- limit * (if (percent) .12 else .06)
  inside <- d[d$Value >= label_min, , drop = FALSE]
  outside <- d[d$Value > 0 & d$Value < label_min, , drop = FALSE]
  colours <- palette[c("ChatGPT", "Claude", "Gemini", "Other AI")]
  rgb <- grDevices::col2rgb(colours) / 255
  linear_rgb <- ifelse(rgb <= .04045, rgb / 12.92, ((rgb + .055) / 1.055)^2.4)
  luminance <- drop(c(.2126, .7152, .0722) %*% linear_rgb)
  names(luminance) <- names(colours)
  inside$Ink <- ifelse(luminance[as.character(inside$Tool)] > .179, "#000000", "#FFFFFF")

  plot <- ggplot2::ggplot(d, ggplot2::aes(Value, Purpose, fill = Tool)) +
    ggplot2::geom_col(orientation = "y", width = .65,
      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(data = inside,
      mapping = ggplot2::aes(x = Midpoint, y = Purpose, label = Label, colour = Ink),
      inherit.aes = FALSE, show.legend = FALSE, size = 3.2) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_x_continuous(
      breaks = if (percent) seq(0, 100, 25) else ggplot2::waiver(),
      labels = if (percent) function(x) paste0(x, "%") else ggplot2::waiver(),
      expand = ggplot2::expansion(mult = c(0, .04))) +
    ggplot2::coord_cartesian(xlim = c(0, limit), clip = "off") +
    ggplot2::labs(x = if (percent) "Share of AI users within each purpose" else "Number of students",
      y = "Primary academic purpose") +
    chart_theme()

  if (nrow(outside)) {
    outside$Label <- paste(outside$Tool, outside$Label)
    notes <- stats::aggregate(Label ~ Purpose, data = outside, FUN = paste, collapse = "\n")
    plot <- plot +
      ggplot2::geom_text(data = notes,
        mapping = ggplot2::aes(x = limit * 1.025, y = Purpose, label = Label),
        inherit.aes = FALSE, hjust = 0, size = 2.9, lineheight = 1, colour = "#172840") +
      ggplot2::theme(plot.margin = ggplot2::margin(12, 83, 12, 8))
  }
  plot
}

toolsServer <- function(id, data, before_tool = data, palette = function() FIGURE_PALETTES$Navy,
                        primary_tool = function() "All") {
  shiny::moduleServer(id, function(input, output, session) {
    summary <- shiny::reactive(purpose_tool_summary(data(), before_tool(),
      if (is.null(input$purpose)) "" else input$purpose))
    output$title <- shiny::renderText(figure_title("Figure 1. Primary AI Tool by Academic Purpose", primary_tool()))
    output$sample <- shiny::renderUI(shiny::span(class = "sample-badge",
      sprintf("n = %d eligible AI users", summary()$selected)))
    output$chart <- shiny::renderPlot({
      result <- summary()
      shiny::validate(shiny::need(result$selected > 0,
        if (only_non_users(data())) {
          "The selected respondents reported no AI use. The survey skips the academic-purpose question for this group."
        } else {
          "No eligible AI users match these filters. Try changing the tool or academic purpose."
        }))
      shiny::req(input$measure)
      purpose_tool_plot(result$rows, input$measure == "percent", palette())
    }, res = 110, bg = "white")
    output$reading <- shiny::renderText({
      s <- summary()
      if (!s$selected) return("No eligible AI users match these filters.")
      by_tool <- tapply(s$rows$Count, s$rows$Tool, sum)
      leaders <- names(by_tool)[by_tool == max(by_tool)]
      if (length(by_tool) == 1 && s$selected < s$reference) {
        return(sprintf("%d of the %d eligible AI users across the academic purposes shown are in the selected tool group.", s$selected, s$reference))
      }
      if (length(leaders) == 1) {
        sprintf("Among the %d eligible AI users shown, %s is the most common tool group (%d responses).",
          s$selected, leaders, max(by_tool))
      } else {
        sprintf("Among the %d eligible AI users shown, %s tie for the most responses (%d each).",
          s$selected, paste(leaders, collapse = ", "), max(by_tool))
      }
    })
    readable_summary <- shiny::reactive({
      d <- summary()$rows
      data.frame("Academic purpose" = as.character(d$Purpose), "Tool shown" = as.character(d$Tool),
        Count = d$Count, "Eligible AI users before tool filter" = d$Denominator,
        "Percentage (%)" = round(d$Percentage, 1), check.names = FALSE)
    })
    output$summary_table <- shiny::renderTable(readable_summary(), striped = TRUE, spacing = "m", digits = 1)
    output$download <- shiny::downloadHandler(filename = function() "STA304-tool-by-purpose.csv",
      content = function(file) write.csv(readable_summary(), file, row.names = FALSE))
  })
}
