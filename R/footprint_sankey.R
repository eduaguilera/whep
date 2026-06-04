#' Interactive footprint Sankey viewer
#'
#' @description
#' Create a browser-based Sankey viewer from a footprint table, such as the
#' output of [compute_footprint()]. The viewer includes stage filters, a
#' per-stage node limit controls, a minimum-flow threshold, hover tooltips,
#' click-to-highlight interactions, and SVG download.
#'
#' Each row is treated as one path through the selected `stages`, with
#' `value_col` giving the path size. Rows are summed before plotting, so
#' repeated paths are shown as one flow.
#'
#' The default stages are `origin_area`, `origin_item`, `target_item`, and
#' `target_area`; `target_fd` is appended automatically when present. If
#' columns such as `origin_area_name` or `target_item_name` are present,
#' they are used as display labels.
#'
#' @param footprints A data frame with footprint paths and a numeric value
#'   column.
#' @param stages Character vector of columns to use as Sankey stages.
#'   Defaults to the footprint origin and target columns.
#' @param value_col Name of the numeric column containing flow values.
#' @param label_cols Optional named character vector mapping stage columns
#'   to display-label columns, for example
#'   `c(origin_area = "origin_area_name")`. When omitted, `{stage}_name`
#'   columns are used where available.
#' @param max_nodes Maximum number of individual nodes to show per stage in
#'   the current filtered browser view. Less important nodes are grouped into
#'   `other_label`. Use `Inf` to show all nodes.
#' @param other_label Label used for grouped nodes when `max_nodes` is
#'   finite.
#' @param stage_max_nodes Optional named numeric vector overriding
#'   `max_nodes` for selected stages, for example
#'   `c(product = 75, target_area = 12)`.
#' @param stage_other_labels Optional named character vector overriding
#'   `other_label` for selected stages, for example
#'   `c(product = "Other products")`.
#' @param embed_max_nodes Maximum number of individual nodes to embed per
#'   stage before writing the viewer. Less important nodes are permanently
#'   grouped before serialization, reducing file size. Use `Inf` to keep all
#'   nodes available to the browser.
#' @param stage_embed_max_nodes Optional named numeric vector overriding
#'   `embed_max_nodes` for selected stages.
#' @param min_share Minimum visible path size as a percentage of the current
#'   filtered total. Set to 0 to keep diffuse trade links visible. Users can
#'   change this in the browser.
#' @param title Optional viewer title.
#' @param subtitle Optional viewer subtitle.
#' @param value_label Label used in tooltips and the summary line.
#' @param width Viewer width as a CSS value.
#' @param height Sankey SVG height in pixels.
#' @param file Optional path to write a standalone HTML viewer.
#' @param open If `TRUE` and `file` is supplied, open the written viewer in
#'   the default browser. If `TRUE` and `file` is `NULL`, a temporary HTML
#'   file is written and opened.
#'
#' @return A browsable HTML object when `file` is `NULL` and `open` is
#'   `FALSE`; otherwise invisibly returns the HTML file path.
#'
#' @export
#'
#' @examples
#' footprints <- tibble::tribble(
#'   ~origin_area, ~origin_item, ~target_item, ~target_area, ~value,
#'   "Brazil", "Soybeans", "Pigmeat", "China", 10,
#'   "Brazil", "Soybeans", "Milk", "China", 4,
#'   "Brazil", "Soybeans", "Soybean Oil", "France", 3
#' )
#'
#' if (
#'   interactive() &&
#'     requireNamespace("htmltools", quietly = TRUE) &&
#'     requireNamespace("jsonlite", quietly = TRUE)
#' ) {
#'   plot_footprint_sankey(footprints)
#' }
plot_footprint_sankey <- function(
  footprints,
  stages = NULL,
  value_col = "value",
  label_cols = NULL,
  max_nodes = 10,
  other_label = "Other",
  stage_max_nodes = NULL,
  stage_other_labels = NULL,
  embed_max_nodes = Inf,
  stage_embed_max_nodes = NULL,
  min_share = 0,
  title = "Footprint Sankey Viewer",
  subtitle = NULL,
  value_label = "footprint",
  width = "100%",
  height = 680,
  file = NULL,
  open = FALSE
) {
  .require_footprint_sankey_packages()

  stages <- .sankey_default_stages(footprints, stages)
  .validate_footprint_sankey_inputs(
    footprints,
    stages,
    value_col,
    label_cols,
    max_nodes,
    stage_max_nodes,
    stage_other_labels,
    embed_max_nodes,
    stage_embed_max_nodes,
    min_share,
    width,
    height,
    file,
    open
  )

  paths <- .sankey_prepare_paths(
    footprints,
    stages,
    value_col,
    label_cols,
    max_nodes,
    other_label
  )
  paths <- .sankey_group_small_nodes(
    paths,
    .sankey_stage_cols(stages),
    .sankey_stage_max_nodes(
      stages,
      embed_max_nodes,
      stage_embed_max_nodes
    ),
    .sankey_stage_other_labels(
      stages,
      other_label,
      stage_other_labels
    )
  )
  if (nrow(paths) == 0) {
    cli::cli_abort("No positive footprint flows to plot.")
  }

  model <- .sankey_model(
    paths,
    stages,
    max_nodes,
    other_label,
    stage_max_nodes,
    stage_other_labels,
    embed_max_nodes,
    stage_embed_max_nodes,
    min_share,
    title,
    subtitle,
    value_label,
    height
  )
  viewer <- .sankey_html(model, width)

  if (isTRUE(open) && is.null(file)) {
    file <- tempfile("whep-footprint-sankey-", fileext = ".html")
  }
  if (!is.null(file)) {
    htmltools::save_html(viewer, file = file)
    if (isTRUE(open)) {
      utils::browseURL(normalizePath(file, mustWork = TRUE))
    }
    return(invisible(file))
  }

  htmltools::browsable(viewer)
}

.require_footprint_sankey_packages <- function() {
  missing <- c(
    if (!requireNamespace("htmltools", quietly = TRUE)) "htmltools",
    if (!requireNamespace("jsonlite", quietly = TRUE)) "jsonlite"
  )
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.pkg {missing}} {?is/are} required to create the Sankey viewer."
    )
  }
}

.sankey_default_stages <- function(footprints, stages) {
  if (!is.null(stages)) {
    return(stages)
  }

  defaults <- c(
    "origin_area",
    "origin_item",
    "target_item",
    "target_area"
  )
  if ("target_fd" %in% names(footprints)) {
    defaults <- c(defaults, "target_fd")
  }
  defaults
}

.validate_footprint_sankey_inputs <- function(
  footprints,
  stages,
  value_col,
  label_cols,
  max_nodes,
  stage_max_nodes,
  stage_other_labels,
  embed_max_nodes,
  stage_embed_max_nodes,
  min_share,
  width,
  height,
  file,
  open
) {
  if (!is.data.frame(footprints)) {
    cli::cli_abort("{.arg footprints} must be a data frame.")
  }
  if (
    !is.character(stages) ||
      length(stages) < 2 ||
      any(is.na(stages)) ||
      any(!nzchar(stages))
  ) {
    cli::cli_abort("{.arg stages} must contain at least two column names.")
  }
  missing_stages <- setdiff(stages, names(footprints))
  if (length(missing_stages) > 0) {
    cli::cli_abort(
      "{.arg footprints} is missing stage column{?s}: {.field {missing_stages}}."
    )
  }
  if (!is.character(value_col) || length(value_col) != 1) {
    cli::cli_abort("{.arg value_col} must be one column name.")
  }
  if (!value_col %in% names(footprints)) {
    cli::cli_abort(
      "{.arg footprints} is missing value column {.field {value_col}}."
    )
  }
  if (!is.numeric(footprints[[value_col]])) {
    cli::cli_abort("{.arg value_col} must identify a numeric column.")
  }
  if (!is.null(label_cols)) {
    if (
      !is.character(label_cols) ||
        is.null(names(label_cols)) ||
        any(is.na(names(label_cols))) ||
        any(!nzchar(names(label_cols))) ||
        any(is.na(label_cols)) ||
        any(!nzchar(label_cols))
    ) {
      cli::cli_abort(
        "{.arg label_cols} must be a named character vector."
      )
    }
    missing_labels <- setdiff(unname(label_cols), names(footprints))
    if (length(missing_labels) > 0) {
      cli::cli_abort(
        "{.arg footprints} is missing label column{?s}: {.field {missing_labels}}."
      )
    }
  }
  .validate_sankey_max_nodes(max_nodes, "max_nodes")
  .validate_sankey_stage_max_nodes(stage_max_nodes, stages)
  .validate_sankey_stage_other_labels(stage_other_labels, stages)
  .validate_sankey_max_nodes(embed_max_nodes, "embed_max_nodes")
  .validate_sankey_stage_max_nodes(
    stage_embed_max_nodes,
    stages,
    "stage_embed_max_nodes"
  )
  if (
    !is.numeric(min_share) ||
      length(min_share) != 1 ||
      is.na(min_share) ||
      !is.finite(min_share) ||
      min_share < 0
  ) {
    cli::cli_abort("{.arg min_share} must be a non-negative number.")
  }
  if (
    !is.character(width) || length(width) != 1 || is.na(width) || !nzchar(width)
  ) {
    cli::cli_abort("{.arg width} must be a non-empty CSS width value.")
  }
  if (
    !is.numeric(height) ||
      length(height) != 1 ||
      is.na(height) ||
      !is.finite(height) ||
      height < 240
  ) {
    cli::cli_abort("{.arg height} must be at least 240 pixels.")
  }
  if (
    !is.null(file) &&
      (!is.character(file) || length(file) != 1 || is.na(file) || !nzchar(file))
  ) {
    cli::cli_abort("{.arg file} must be one file path.")
  }
  if (!is.logical(open) || length(open) != 1 || is.na(open)) {
    cli::cli_abort("{.arg open} must be `TRUE` or `FALSE`.")
  }
}

.validate_sankey_max_nodes <- function(value, arg) {
  if (!is.numeric(value) || length(value) != 1 || is.na(value) || value < 1) {
    cli::cli_abort("{.arg {arg}} must be a positive number or Inf.")
  }
}

.validate_sankey_stage_max_nodes <- function(
  stage_max_nodes,
  stages,
  arg = "stage_max_nodes"
) {
  if (is.null(stage_max_nodes)) {
    return(invisible(NULL))
  }
  if (
    !is.numeric(stage_max_nodes) ||
      is.null(names(stage_max_nodes)) ||
      any(is.na(names(stage_max_nodes))) ||
      any(!nzchar(names(stage_max_nodes))) ||
      any(is.na(stage_max_nodes)) ||
      any(stage_max_nodes < 1)
  ) {
    cli::cli_abort(
      "{.arg {arg}} must be a named numeric vector of positive values."
    )
  }
  unknown <- setdiff(names(stage_max_nodes), stages)
  if (length(unknown) > 0) {
    cli::cli_abort(
      "{.arg {arg}} has unknown stage name{?s}: {.field {unknown}}."
    )
  }
  invisible(NULL)
}

.validate_sankey_stage_other_labels <- function(stage_other_labels, stages) {
  if (is.null(stage_other_labels)) {
    return(invisible(NULL))
  }
  if (
    !is.character(stage_other_labels) ||
      is.null(names(stage_other_labels)) ||
      any(is.na(names(stage_other_labels))) ||
      any(!nzchar(names(stage_other_labels))) ||
      any(is.na(stage_other_labels)) ||
      any(!nzchar(stage_other_labels))
  ) {
    cli::cli_abort(
      "{.arg stage_other_labels} must be a named character vector of non-empty labels."
    )
  }
  unknown <- setdiff(names(stage_other_labels), stages)
  if (length(unknown) > 0) {
    cli::cli_abort(
      "{.arg stage_other_labels} has unknown stage name{?s}: {.field {unknown}}."
    )
  }
  invisible(NULL)
}

.sankey_prepare_paths <- function(
  footprints,
  stages,
  value_col,
  label_cols,
  max_nodes,
  other_label
) {
  stage_cols <- .sankey_stage_cols(stages)
  paths <- tibble::tibble(value = as.numeric(footprints[[value_col]]))

  for (i in seq_along(stages)) {
    paths[[stage_cols[i]]] <- .sankey_stage_labels(
      footprints,
      stages[i],
      label_cols
    )
  }

  paths <- paths |>
    dplyr::filter(is.finite(.data$value), .data$value > 0)

  if (nrow(paths) == 0) {
    return(paths)
  }

  paths |>
    dplyr::group_by(dplyr::across(dplyr::all_of(stage_cols))) |>
    dplyr::summarise(value = sum(.data$value), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$value))
}

.sankey_stage_labels <- function(footprints, stage, label_cols) {
  label_col <- NULL
  if (!is.null(label_cols) && stage %in% names(label_cols)) {
    label_col <- unname(label_cols[[stage]])
  }
  if (is.null(label_col)) {
    candidate <- paste0(stage, "_name")
    if (candidate %in% names(footprints)) {
      label_col <- candidate
    } else {
      label_col <- stage
    }
  }

  labels <- as.character(footprints[[label_col]])
  fallback <- as.character(footprints[[stage]])
  missing <- is.na(labels) | labels == ""
  labels[missing] <- fallback[missing]
  labels[is.na(labels) | labels == ""] <- "(missing)"
  labels
}

.sankey_group_small_nodes <- function(
  paths,
  stage_cols,
  max_nodes,
  other_label
) {
  if (all(is.na(max_nodes) | is.infinite(max_nodes))) {
    return(paths)
  }

  for (i in seq_along(stage_cols)) {
    stage_col <- stage_cols[i]
    limit <- max_nodes[i]
    if (is.na(limit) || is.infinite(limit)) {
      next
    }
    limit <- as.integer(limit)
    other <- other_label[i]
    keep <- paths |>
      dplyr::group_by(.data[[stage_col]]) |>
      dplyr::summarise(total = sum(.data$value), .groups = "drop") |>
      dplyr::filter(.data[[stage_col]] != other) |>
      dplyr::arrange(dplyr::desc(.data$total), .data[[stage_col]]) |>
      dplyr::slice_head(n = limit) |>
      dplyr::pull(.data[[stage_col]])

    paths[[stage_col]] <- ifelse(
      paths[[stage_col]] %in% keep,
      paths[[stage_col]],
      other
    )
  }

  paths |>
    dplyr::group_by(dplyr::across(dplyr::all_of(stage_cols))) |>
    dplyr::summarise(value = sum(.data$value), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$value))
}

.sankey_stage_cols <- function(stages) {
  paste0("stage_", seq_along(stages))
}

.sankey_stage_max_nodes <- function(stages, max_nodes, stage_max_nodes) {
  values <- rep(max_nodes, length(stages))
  if (!is.null(stage_max_nodes)) {
    idx <- match(names(stage_max_nodes), stages)
    values[idx] <- stage_max_nodes
  }
  ifelse(is.infinite(values), NA_real_, values)
}

.sankey_stage_other_labels <- function(
  stages,
  other_label,
  stage_other_labels
) {
  values <- rep(other_label, length(stages))
  if (!is.null(stage_other_labels)) {
    idx <- match(names(stage_other_labels), stages)
    values[idx] <- stage_other_labels
  }
  values
}

.sankey_model <- function(
  paths,
  stages,
  max_nodes,
  other_label,
  stage_max_nodes,
  stage_other_labels,
  embed_max_nodes,
  stage_embed_max_nodes,
  min_share,
  title,
  subtitle,
  value_label,
  height
) {
  stage_cols <- .sankey_stage_cols(stages)
  stage_info <- tibble::tibble(
    id = stages,
    key = stage_cols,
    label = .sankey_pretty_stage(stages),
    max_nodes = .sankey_stage_max_nodes(
      stages,
      max_nodes,
      stage_max_nodes
    ),
    embed_max_nodes = .sankey_stage_max_nodes(
      stages,
      embed_max_nodes,
      stage_embed_max_nodes
    ),
    other_label = .sankey_stage_other_labels(
      stages,
      other_label,
      stage_other_labels
    )
  )

  list(
    stages = stage_info,
    paths = paths,
    options = list(
      title = title,
      subtitle = subtitle,
      value_label = value_label,
      max_nodes = max_nodes,
      other_label = other_label,
      min_share = min_share,
      height = height,
      total = sum(paths$value)
    )
  )
}

.sankey_pretty_stage <- function(stages) {
  stages |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title()
}

.sankey_html <- function(model, width) {
  id <- paste0(
    "whep-sankey-",
    paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = "")
  )
  json <- .sankey_json(model)

  htmltools::tagList(
    htmltools::tags$div(
      id = id,
      class = "whep-sankey",
      style = paste0("width:", width, ";"),
      htmltools::tags$div(
        class = "whep-sankey__header",
        htmltools::tags$div(
          htmltools::tags$h2(model$options$title),
          if (!is.null(model$options$subtitle)) {
            htmltools::tags$p(model$options$subtitle)
          },
          htmltools::tags$div(class = "whep-sankey__summary")
        ),
        htmltools::tags$div(
          class = "whep-sankey__actions",
          htmltools::tags$button(
            type = "button",
            class = "whep-sankey__reset",
            "Reset"
          ),
          htmltools::tags$button(
            type = "button",
            class = "whep-sankey__download",
            "Download SVG"
          )
        )
      ),
      htmltools::tags$div(class = "whep-sankey__controls"),
      htmltools::tags$div(
        class = "whep-sankey__chart",
        htmltools::tags$svg(
          class = "whep-sankey__svg",
          role = "img",
          `aria-label` = model$options$title
        ),
        htmltools::tags$div(class = "whep-sankey__tooltip")
      )
    ),
    htmltools::tags$style(htmltools::HTML(.sankey_css())),
    htmltools::tags$script(htmltools::HTML(sprintf(
      "(function(){\nconst root = document.getElementById(%s);\nconst model = %s;\n%s\n})();",
      jsonlite::toJSON(id, auto_unbox = TRUE),
      json,
      .sankey_js()
    )))
  )
}

.sankey_json <- function(model) {
  json <- jsonlite::toJSON(
    model,
    dataframe = "rows",
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  stringr::str_replace_all(json, "</", "<\\\\/")
}

.sankey_css <- function() {
  paste(
    c(
      ".whep-sankey{box-sizing:border-box;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;color:#1f2d3a;background:#fff;border:1px solid #d6dde3;border-radius:6px;overflow:hidden;}",
      ".whep-sankey *{box-sizing:border-box;}",
      ".whep-sankey__header{display:flex;justify-content:space-between;gap:16px;align-items:flex-start;padding:16px 18px 12px;background:#f7f9fa;border-bottom:1px solid #d6dde3;}",
      ".whep-sankey__header h2{margin:0 0 4px;font-size:20px;line-height:1.2;font-weight:650;}",
      ".whep-sankey__header p{margin:0 0 8px;color:#52606d;font-size:13px;}",
      ".whep-sankey__summary{font-size:14px;color:#2f3b46;}",
      ".whep-sankey__actions{display:flex;gap:8px;flex-wrap:wrap;justify-content:flex-end;}",
      ".whep-sankey button{border:1px solid #95a5ad;background:#f8fafb;color:#263641;border-radius:4px;padding:8px 10px;font-size:13px;cursor:pointer;}",
      ".whep-sankey button:hover{background:#e9eef1;}",
      ".whep-sankey__controls{display:grid;grid-template-columns:repeat(auto-fit,minmax(170px,1fr));gap:10px;padding:12px 18px;border-bottom:1px solid #e3e8ec;background:#fff;}",
      ".whep-sankey__control{display:flex;flex-direction:column;gap:4px;min-width:0;}",
      ".whep-sankey__control span{font-size:12px;font-weight:650;color:#31424f;}",
      ".whep-sankey__control select,.whep-sankey__control input{width:100%;min-width:0;border:1px solid #c5cdd3;border-radius:4px;background:#fff;color:#1f2d3a;padding:7px 8px;font-size:13px;}",
      ".whep-sankey__chart{position:relative;min-height:280px;background:#fff;}",
      ".whep-sankey__svg{display:block;width:100%;height:auto;}",
      ".whep-sankey__node rect{cursor:pointer;stroke:#2e3c45;stroke-width:.6;}",
      ".whep-sankey__node text{font-size:12px;fill:#1b252d;pointer-events:none;}",
      ".whep-sankey__stage-label{font-size:12px;font-weight:700;fill:#30424f;text-anchor:middle;}",
      ".whep-sankey__link path{cursor:pointer;stroke:rgba(40,40,40,.08);stroke-width:.35;}",
      ".whep-sankey__link.is-dim path,.whep-sankey__node.is-dim{opacity:.15;}",
      ".whep-sankey__link.is-active path{opacity:.85;stroke:#16232c;stroke-width:.75;}",
      ".whep-sankey__node.is-active rect{stroke:#101820;stroke-width:1.5;}",
      ".whep-sankey__empty{font-size:14px;fill:#53616d;text-anchor:middle;}",
      ".whep-sankey__tooltip{position:absolute;z-index:3;display:none;max-width:320px;padding:9px 10px;background:rgba(20,30,38,.94);color:#fff;border-radius:4px;font-size:12px;line-height:1.35;box-shadow:0 8px 20px rgba(0,0,0,.18);pointer-events:none;}",
      ".whep-sankey__tooltip strong{display:block;margin-bottom:4px;font-size:12px;}",
      "@media(max-width:700px){.whep-sankey__header{display:block;}.whep-sankey__actions{justify-content:flex-start;margin-top:10px;}.whep-sankey__controls{grid-template-columns:1fr;}}"
    ),
    collapse = "\n"
  )
}

.sankey_js <- function() {
  paste(
    c(
      "const defaultMinShare = Number(model.options.min_share || 0);",
      "const state = { filters: Object.create(null), minShare: defaultMinShare, maxNodes: Object.create(null), active: null };",
      "const palette = ['#5d7290','#2f9c85','#8b5c9e','#c78d22','#4f8bb8','#9c5c61','#4f965e','#7764ad','#be6f39','#3c7f90','#a05e89','#6f7d3f'];",
      "const svg = root.querySelector('.whep-sankey__svg');",
      "const controls = root.querySelector('.whep-sankey__controls');",
      "const summary = root.querySelector('.whep-sankey__summary');",
      "const tooltip = root.querySelector('.whep-sankey__tooltip');",
      "const chart = root.querySelector('.whep-sankey__chart');",
      "function esc(value){return String(value == null ? '' : value).replace(/[&<>\"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','\"':'&quot;',\"'\":'&#39;'}[c]));}",
      "function number(value){return Number(value || 0).toLocaleString(undefined,{maximumFractionDigits:2});}",
      "function pct(value,total){return total > 0 ? (100 * value / total).toLocaleString(undefined,{maximumFractionDigits:1}) + '%' : '0%';}",
      "function truncate(value,n){value = String(value); return value.length > n ? value.slice(0,n - 3) + '...' : value;}",
      "function colorFor(value){let h = 0; for (let i = 0; i < value.length; i++) h = ((h << 5) - h + value.charCodeAt(i)) | 0; return palette[Math.abs(h) % palette.length];}",
      "function hardMaxNodes(stage){return stage.embed_max_nodes == null ? null : Math.max(1, Math.floor(Number(stage.embed_max_nodes)));}",
      "function defaultMaxNodes(stage){const value = stage.max_nodes == null ? null : Math.max(1, Math.floor(Number(stage.max_nodes))); const hard = hardMaxNodes(stage); return value == null || hard == null ? value : Math.min(value, hard);}",
      "function parseMaxNodes(raw, stage){if (raw === '') return null; const parsed = Number(raw); if (!Number.isFinite(parsed)) return null; let value = Math.max(1, Math.floor(parsed)); const hard = hardMaxNodes(stage); if (hard != null) value = Math.min(value, hard); return value;}",
      "model.stages.forEach(stage => { state.maxNodes[stage.key] = defaultMaxNodes(stage); });",
      "function stageTotals(stage){const totals = new Map(); model.paths.forEach(p => totals.set(p[stage.key], (totals.get(p[stage.key]) || 0) + Number(p.value || 0))); return [...totals.entries()].sort((a,b) => b[1] - a[1] || String(a[0]).localeCompare(String(b[0])));}",
      "function buildControls(){controls.innerHTML = ''; model.stages.forEach(stage => { const label = document.createElement('label'); label.className = 'whep-sankey__control'; label.innerHTML = '<span>' + esc(stage.label) + '</span>'; const select = document.createElement('select'); select.dataset.stage = stage.key; select.innerHTML = '<option value=\"\">All</option>' + stageTotals(stage).map(([name,total]) => '<option value=\"' + esc(name) + '\">' + esc(name) + ' (' + pct(total, model.options.total) + ')</option>').join(''); select.addEventListener('change', () => { state.filters[stage.key] = select.value; state.active = null; render(); }); label.appendChild(select); controls.appendChild(label); const limit = document.createElement('label'); limit.className = 'whep-sankey__control whep-sankey__control--max-nodes'; limit.innerHTML = '<span>Max ' + esc(stage.label) + ' nodes</span>'; const input = document.createElement('input'); input.type = 'number'; input.min = '1'; input.step = '1'; input.placeholder = 'All'; input.dataset.maxStage = stage.key; const hard = hardMaxNodes(stage); if (hard != null) input.max = String(hard); const current = state.maxNodes[stage.key]; input.value = current == null ? '' : String(current); input.addEventListener('input', event => { const value = parseMaxNodes(event.target.value.trim(), stage); state.maxNodes[stage.key] = value; event.target.value = value == null ? '' : String(value); state.active = null; render(); }); limit.appendChild(input); controls.appendChild(limit); }); const threshold = document.createElement('label'); threshold.className = 'whep-sankey__control'; threshold.innerHTML = '<span>Hide paths smaller than (%)</span><input type=\"number\" min=\"0\" step=\"0.1\" value=\"' + esc(state.minShare) + '\" data-threshold=\"true\">'; threshold.querySelector('input').addEventListener('input', event => { state.minShare = Math.max(0, Number(event.target.value || 0)); state.active = null; render(); }); controls.appendChild(threshold); }",
      "function filteredPaths(){return model.paths.filter(p => model.stages.every(stage => !state.filters[stage.key] || p[stage.key] === state.filters[stage.key]));}",
      "function aggregatePaths(paths){const keys = model.stages.map(s => s.key); const grouped = new Map(); paths.forEach(p => { const id = keys.map(k => p[k]).join('\\u001f'); const old = grouped.get(id) || { value: 0 }; keys.forEach(k => old[k] = p[k]); old.value += Number(p.value || 0); grouped.set(id, old); }); return [...grouped.values()].sort((a,b) => Number(b.value || 0) - Number(a.value || 0));}",
      "function groupVisiblePaths(paths){let grouped = paths.map(p => ({ ...p })); model.stages.forEach(stage => { const rawMax = state.maxNodes[stage.key]; const maxNodes = rawMax == null ? Infinity : Number(rawMax); if (!Number.isFinite(maxNodes)) return; const n = Math.max(1, Math.floor(maxNodes)); const other = stage.other_label || model.options.other_label || 'Other'; const totals = new Map(); grouped.forEach(p => totals.set(p[stage.key], (totals.get(p[stage.key]) || 0) + Number(p.value || 0))); const keep = new Set([...totals.entries()].filter(([name]) => name !== other).sort((a,b) => b[1] - a[1] || String(a[0]).localeCompare(String(b[0]))).slice(0, n).map(([name]) => name)); grouped.forEach(p => { if (p[stage.key] !== other && !keep.has(p[stage.key])) p[stage.key] = other; }); }); return aggregatePaths(grouped);}",
      "function visiblePaths(){let paths = groupVisiblePaths(filteredPaths()); const selectedTotal = paths.reduce((sum,p) => sum + Number(p.value || 0), 0); state.selectedTotal = selectedTotal; state.hiddenTotal = 0; state.hiddenCount = 0; if (state.minShare > 0 && selectedTotal > 0) { const before = paths; paths = before.filter(p => (100 * Number(p.value || 0) / selectedTotal) >= state.minShare); const visibleTotal = paths.reduce((sum,p) => sum + Number(p.value || 0), 0); state.hiddenTotal = Math.max(0, selectedTotal - visibleTotal); state.hiddenCount = before.length - paths.length; } return paths;}",
      "function summaryText(visibleTotal,pathCount){const label = model.options.value_label; const base = number(visibleTotal) + ' visible ' + label + ' across ' + pathCount.toLocaleString() + ' path' + (pathCount === 1 ? '' : 's') + '.'; if ((state.hiddenCount || 0) > 0) return base + ' Hidden by threshold: ' + number(state.hiddenTotal) + ' ' + label + ' (' + pct(state.hiddenTotal, state.selectedTotal) + ' of selected total) across ' + state.hiddenCount.toLocaleString() + ' path' + (state.hiddenCount === 1 ? '' : 's') + '.'; return base;}",
      "function aggregate(paths, key){const totals = new Map(); paths.forEach(p => totals.set(p[key], (totals.get(p[key]) || 0) + Number(p.value || 0))); return [...totals.entries()].map(([label,value]) => ({ label, value }));}",
      "function makeLayout(paths, width, height){const margin = { top: 62, right: 140, bottom: 26, left: 140 }; if (width < 760) { margin.left = 82; margin.right = 82; } const keys = model.stages.map(s => s.key); const nodeWidth = width < 760 ? 12 : 18; const gapFrac = 0.026; const stageData = keys.map((key,stage) => aggregate(paths,key).sort((a,b) => b.value - a.value || String(a.label).localeCompare(String(b.label))).map((d,i) => ({ ...d, stage, index: i, id: stage + '::' + d.label }))); const total = Math.max(0, ...stageData.map(nodes => nodes.reduce((sum,n) => sum + n.value, 0))); const gap = total * gapFrac; let maxHeight = 0; stageData.forEach(nodes => { let y = 0; nodes.forEach((node,i) => { if (i > 0) y += gap; node.y0v = y; node.y1v = y + node.value; y = node.y1v; }); maxHeight = Math.max(maxHeight, y); }); stageData.forEach(nodes => { const stageHeight = nodes.length ? nodes[nodes.length - 1].y1v : 0; const offset = (maxHeight - stageHeight) / 2; nodes.forEach(node => { node.y0v += offset; node.y1v += offset; node.midv = (node.y0v + node.y1v) / 2; }); }); const scale = maxHeight > 0 ? (height - margin.top - margin.bottom) / maxHeight : 1; const plotWidth = width - margin.left - margin.right; const nodes = stageData.flat().map(node => { const x = margin.left + (keys.length === 1 ? 0 : node.stage * plotWidth / (keys.length - 1)); return { ...node, x, x0: x - nodeWidth / 2, x1: x + nodeWidth / 2, y0: margin.top + node.y0v * scale, y1: margin.top + node.y1v * scale, yMid: margin.top + node.midv * scale }; }); const nodeMap = new Map(nodes.map(n => [n.id,n])); const links = []; for (let stage = 0; stage < keys.length - 1; stage++) { const grouped = new Map(); paths.forEach(p => { const source = p[keys[stage]]; const target = p[keys[stage + 1]]; const id = stage + '::' + source + '->' + target; const old = grouped.get(id) || { id, stage, source, target, value: 0, sourceId: stage + '::' + source, targetId: (stage + 1) + '::' + target }; old.value += Number(p.value || 0); grouped.set(id, old); }); links.push(...grouped.values()); } links.forEach((link,i) => { link.index = i; link.sourceNode = nodeMap.get(link.sourceId); link.targetNode = nodeMap.get(link.targetId); }); const bySource = new Map(); links.slice().sort((a,b) => a.stage - b.stage || String(a.sourceId).localeCompare(String(b.sourceId)) || a.targetNode.yMid - b.targetNode.yMid || String(a.target).localeCompare(String(b.target))).forEach(link => { const offset = bySource.get(link.sourceId) || 0; link.sy0v = link.sourceNode.y0v + offset; link.sy1v = link.sy0v + link.value; bySource.set(link.sourceId, offset + link.value); }); const byTarget = new Map(); links.slice().sort((a,b) => a.stage - b.stage || String(a.targetId).localeCompare(String(b.targetId)) || a.sourceNode.yMid - b.sourceNode.yMid || String(a.source).localeCompare(String(b.source))).forEach(link => { const offset = byTarget.get(link.targetId) || 0; link.ty0v = link.targetNode.y0v + offset; link.ty1v = link.ty0v + link.value; byTarget.set(link.targetId, offset + link.value); }); links.forEach(link => { link.sy0 = margin.top + link.sy0v * scale; link.sy1 = margin.top + link.sy1v * scale; link.ty0 = margin.top + link.ty0v * scale; link.ty1 = margin.top + link.ty1v * scale; }); return { nodes, links, total, margin, height, width, nodeWidth }; }",
      "function ribbonPath(link){const x0 = link.sourceNode.x1; const x1 = link.targetNode.x0; const xm = x0 + (x1 - x0) * 0.52; return 'M' + x0 + ',' + link.sy1 + ' C' + xm + ',' + link.sy1 + ' ' + xm + ',' + link.ty1 + ' ' + x1 + ',' + link.ty1 + ' L' + x1 + ',' + link.ty0 + ' C' + xm + ',' + link.ty0 + ' ' + xm + ',' + link.sy0 + ' ' + x0 + ',' + link.sy0 + ' Z';}",
      "function showTip(html,event){tooltip.innerHTML = html; tooltip.style.display = 'block'; moveTip(event);}",
      "function moveTip(event){if (!event) return; const box = chart.getBoundingClientRect(); tooltip.style.left = Math.min(event.clientX - box.left + 14, box.width - tooltip.offsetWidth - 8) + 'px'; tooltip.style.top = Math.max(8, event.clientY - box.top + 14) + 'px';}",
      "function hideTip(){tooltip.style.display = 'none';}",
      "function setActive(type,id){state.active = state.active && state.active.type === type && state.active.id === id ? null : { type, id }; applyHighlight();}",
      "function applyHighlight(){const active = state.active; const activeNodes = new Set(); svg.querySelectorAll('.whep-sankey__link').forEach(g => { const keep = !active || (active.type === 'link' ? g.dataset.linkIndex === active.id : (g.dataset.sourceId === active.id || g.dataset.targetId === active.id)); g.classList.toggle('is-dim', !keep); g.classList.toggle('is-active', !!active && keep); if (keep && active) { activeNodes.add(g.dataset.sourceId); activeNodes.add(g.dataset.targetId); } }); if (active && active.type === 'node') activeNodes.add(active.id); svg.querySelectorAll('.whep-sankey__node').forEach(g => { const keep = !active || activeNodes.has(g.dataset.nodeId); g.classList.toggle('is-dim', !keep); g.classList.toggle('is-active', !!active && keep); });}",
      "function render(){",
      "const width = Math.max(root.clientWidth || 900, 520);",
      "const height = Number(model.options.height || 680);",
      "const paths = visiblePaths();",
      "const layout = makeLayout(paths, width, height);",
      "svg.setAttribute('viewBox','0 0 ' + width + ' ' + height);",
      "svg.setAttribute('height', height);",
      "svg.innerHTML = '';",
      "summary.textContent = summaryText(layout.total, paths.length);",
      "if (!paths.length || !layout.links.length) { svg.innerHTML = '<text class=\"whep-sankey__empty\" x=\"' + width / 2 + '\" y=\"' + height / 2 + '\">No flows match the current filters.</text>'; return; }",
      "model.stages.forEach((stage,i) => { const x = layout.margin.left + i * (width - layout.margin.left - layout.margin.right) / (model.stages.length - 1); const label = document.createElementNS('http://www.w3.org/2000/svg','text'); label.setAttribute('class','whep-sankey__stage-label'); label.setAttribute('x', x); label.setAttribute('y', 26); label.textContent = stage.label; svg.appendChild(label); });",
      "const linkLayer = document.createElementNS('http://www.w3.org/2000/svg','g');",
      "svg.appendChild(linkLayer);",
      "layout.links.forEach(link => { const g = document.createElementNS('http://www.w3.org/2000/svg','g'); g.setAttribute('class','whep-sankey__link'); g.dataset.linkIndex = String(link.index); g.dataset.sourceId = link.sourceId; g.dataset.targetId = link.targetId; const path = document.createElementNS('http://www.w3.org/2000/svg','path'); path.setAttribute('d', ribbonPath(link)); path.setAttribute('fill', colorFor(link.sourceId)); path.setAttribute('opacity', '0.42'); g.appendChild(path); g.addEventListener('mouseenter', event => showTip('<strong>' + esc(link.source) + ' -> ' + esc(link.target) + '</strong>' + number(link.value) + ' ' + esc(model.options.value_label) + '<br>' + pct(link.value, layout.total) + ' of visible total', event)); g.addEventListener('mousemove', moveTip); g.addEventListener('mouseleave', hideTip); g.addEventListener('click', event => { event.stopPropagation(); setActive('link', String(link.index)); }); linkLayer.appendChild(g); });",
      "const nodeLayer = document.createElementNS('http://www.w3.org/2000/svg','g');",
      "svg.appendChild(nodeLayer);",
      "layout.nodes.forEach(node => { const g = document.createElementNS('http://www.w3.org/2000/svg','g'); g.setAttribute('class','whep-sankey__node'); g.dataset.nodeId = node.id; const rect = document.createElementNS('http://www.w3.org/2000/svg','rect'); rect.setAttribute('x', node.x0); rect.setAttribute('y', node.y0); rect.setAttribute('width', node.x1 - node.x0); rect.setAttribute('height', Math.max(1, node.y1 - node.y0)); rect.setAttribute('rx', 1.5); rect.setAttribute('fill', colorFor(node.id)); g.appendChild(rect); const text = document.createElementNS('http://www.w3.org/2000/svg','text'); const side = node.stage === 0 ? 'left' : (node.stage === model.stages.length - 1 ? 'right' : 'middle'); text.setAttribute('x', side === 'left' ? node.x0 - 6 : (side === 'right' ? node.x1 + 6 : node.x1 + 5)); text.setAttribute('y', node.yMid + 4); text.setAttribute('text-anchor', side === 'left' ? 'end' : 'start'); text.textContent = truncate(node.label + ' (' + pct(node.value, layout.total) + ')', width < 760 ? 24 : 42); g.appendChild(text); g.addEventListener('mouseenter', event => showTip('<strong>' + esc(node.label) + '</strong>' + number(node.value) + ' ' + esc(model.options.value_label) + '<br>' + pct(node.value, layout.total) + ' of visible total', event)); g.addEventListener('mousemove', moveTip); g.addEventListener('mouseleave', hideTip); g.addEventListener('click', event => { event.stopPropagation(); setActive('node', node.id); }); nodeLayer.appendChild(g); });",
      "applyHighlight();",
      "}",
      "root.querySelector('.whep-sankey__reset').addEventListener('click', () => { state.active = null; Object.keys(state.filters).forEach(k => state.filters[k] = ''); state.minShare = defaultMinShare; model.stages.forEach(stage => { state.maxNodes[stage.key] = defaultMaxNodes(stage); }); root.querySelectorAll('select').forEach(select => select.value = ''); root.querySelectorAll('input[data-max-stage]').forEach(input => { const stage = model.stages.find(s => s.key === input.dataset.maxStage); const value = defaultMaxNodes(stage); input.value = value == null ? '' : String(value); }); const threshold = root.querySelector('input[data-threshold]'); if (threshold) threshold.value = String(defaultMinShare); render(); });",
      "root.querySelector('.whep-sankey__download').addEventListener('click', () => { const clone = svg.cloneNode(true); clone.setAttribute('xmlns','http://www.w3.org/2000/svg'); const blob = new Blob([new XMLSerializer().serializeToString(clone)], { type: 'image/svg+xml;charset=utf-8' }); const url = URL.createObjectURL(blob); const a = document.createElement('a'); a.href = url; a.download = 'footprint-sankey.svg'; a.click(); URL.revokeObjectURL(url); });",
      "svg.addEventListener('click', () => { state.active = null; applyHighlight(); });",
      "window.addEventListener('resize', () => render());",
      "buildControls();",
      "render();"
    ),
    collapse = "\n"
  )
}
