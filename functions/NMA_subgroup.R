# CLBP, CNSLBP, and CDLBP subgroup analysis.
# Scenario-specific data selection, model settings, and outputs are unchanged.
# Public entry point: run_subgroup_analysis().
# See README.md and DATA_INPUT_GUIDE.md before running the analysis.

# Shared-core bootstrap.
NMA_ENTRY_FILES <- unlist(
  lapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) character() else as.character(frame$ofile[[1]])
    }
  ),
  use.names = FALSE
)
NMA_ENTRY_FILES <- NMA_ENTRY_FILES[file.exists(NMA_ENTRY_FILES)]
NMA_ENTRY_FILE <- if (length(NMA_ENTRY_FILES) > 0L) {
  normalizePath(tail(NMA_ENTRY_FILES, 1L), winslash = "/", mustWork = TRUE)
} else {
  NA_character_
}
if (is.na(NMA_ENTRY_FILE)) {
  command_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(command_file) > 0L) {
    NMA_ENTRY_FILE <- normalizePath(
      sub("^--file=", "", command_file[[1]]),
      winslash = "/",
      mustWork = FALSE
    )
  }
}
NMA_ENTRY_DIR <- if (!is.na(NMA_ENTRY_FILE) && file.exists(NMA_ENTRY_FILE)) {
  dirname(NMA_ENTRY_FILE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
NMA_COMMON_CORE_FILE <- file.path(NMA_ENTRY_DIR, "NMA_common.R")
if (!file.exists(NMA_COMMON_CORE_FILE)) {
  stop("Shared core file not found: ", NMA_COMMON_CORE_FILE)
}
source(NMA_COMMON_CORE_FILE, local = environment(), encoding = "UTF-8")

NMA_NETWORK_LABEL_CEX <- 1.8

NMA_NETWORK_TITLE_CEX <- 2

NMA_SCRIPT_DIR <- detect_nma_script_dir()

# Check required packages. Packages are never installed automatically.
load_nma_packages <- function() {
    required_packages <- c("readxl", "dplyr", "tidyr", "stringr", "meta", "netmeta", "openxlsx", "ggsci", 
        "svglite", "systemfonts", "showtext", "sysfonts", "rsvg", "jpeg", "magick")
    missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing_packages) > 0) {
        stop("Missing required R packages: ", paste(missing_packages, collapse = ", "), "\nInstall them first with: install.packages(c(", 
            paste(sprintf("\"%s\"", missing_packages), collapse = ", "), "))")
    }
    suppressPackageStartupMessages({
        library(dplyr)
        library(tidyr)
        library(stringr)
    })
    verify_nma_font_installation()
    refresh_bmj_ggsci_palette()
    invisible(TRUE)
}

# Figure, table, and treatment-order configuration.
NMA_FONT_REGULAR_FACE <- 1L

NMA_FONT_BOLD_FACE <- 2L

NMA_FOREST_ROW_HEIGHT_IN <- 0.195

NMA_FOREST_HEADER_HEIGHT_IN <- 0.5

NMA_FOREST_HEADER_DATA_GAP_IN <- 0.27

NMA_FOREST_LAYOUT_LONGEST_LABEL <- "Biofeedback devices"

NMA_FOREST_LAYOUT_COUNT_REFERENCE <- "99/9999"

NMA_FOREST_PANEL_WIDTH_X <- 0.253

NMA_FOREST_WIDTH_IN <- 6.65

NMA_FOREST_XLIM <- NULL

NMA_FOREST_TICKS <- NULL

NMA_FOREST_TEXT_SCALE <- 1.15

NMA_FOREST_POINT_SCALE <- 1.15

NMA_LEAGUE_BODY_FONT_SIZE <- 11.5

NMA_LEAGUE_DIAG_FONT_SIZE <- 12

NMA_LEAGUE_COLUMN_WIDTH <- 19

NMA_LEAGUE_ROW_HEIGHT <- 48

NMA_FONT_BODY_NAME <- "InterFace"

NMA_FONT_TITLE_NAME <- "InterFace"

NMA_FONT_FAMILY_NAME <- "NMA_Interface_Word_Outline"

NMA_FONT_INSTALLED_REGULAR_NAME <- "InterFace"

NMA_FONT_INSTALLED_BOLD_NAME <- "InterFace Negreta"

NMA_FONT_BODY_DESCRIPTION <- "TT InterFace"

NMA_FONT_TITLE_DESCRIPTION <- "TT InterFace"

NMA_FONT_BODY_ALIAS <- "InterFace"

NMA_FONT_TITLE_ALIAS <- "InterFace"

NMA_FONT_LIGHT_NAME <- NMA_FONT_BODY_NAME

NMA_FONT_BOLD_NAME <- NMA_FONT_BODY_NAME

NMA_FONT_LIGHT_DESCRIPTION <- NMA_FONT_BODY_DESCRIPTION

NMA_FONT_BOLD_DESCRIPTION <- NMA_FONT_BODY_DESCRIPTION

NMA_FONT_LIGHT_ALIAS <- NMA_FONT_BODY_ALIAS

NMA_FONT_BOLD_ALIAS <- NMA_FONT_BODY_ALIAS

NMA_WINDOWS_FONT_NAME <- NMA_FONT_BODY_NAME

NMA_WINDOWS_FONT_DESCRIPTION <- NMA_FONT_BODY_DESCRIPTION

NMA_WINDOWS_FONT_ALIAS <- NMA_FONT_BODY_ALIAS

NMA_INTERFACE_REGULAR_FILE_BASENAME <- "E30C90B06A6F693C6486C7040F7843D5.TTF"

NMA_INTERFACE_BOLD_FILE_BASENAME <- "7C569B540462B42E.TTF"

BMJ_GGSCI_PALETTE <- c("#2A6EBB", "#F0AB00", "#C50084", "#7D5CC6", "#E37222", "#69BE28", "#00B2A9", "#CD202C", 
    "#747678")

NMA_PRIMARY_HEX <- "#6A58A5"

BMJ_PRIMARY <- NMA_PRIMARY_HEX

BMJ_PRIMARY_LIGHT <- mix_with_white(BMJ_PRIMARY, 0.1)

BMJ_PRIMARY_MID <- mix_with_white(BMJ_PRIMARY, 0.25)

BMJ_TEXT_DARK <- "#232323"

export_nma_jpeg_from_svg <- function(svg_file, jpeg_file = sub("\\.svg$", ".jpg", svg_file)) {
    if (!file.exists(svg_file)) {
        stop("Cannot create JPEG because the SVG does not exist: ", svg_file)
    }
    png_file <- tempfile(fileext = ".png")
    on.exit(unlink(png_file), add = TRUE)
    rsvg::rsvg_png(svg_file, file = png_file, width = 3600)
    img <- magick::image_read(png_file)
    magick::image_write(img, path = jpeg_file, format = "jpeg", quality = 100)
    header <- readBin(jpeg_file, what = "raw", n = 3)
    if (!identical(header, as.raw(c(255, 216, 255)))) {
        stop("Invalid JPEG header; output is not a JPEG file: ", jpeg_file)
    }
    invisible(jpeg_file)
}

test_nma_windows_font <- test_nma_plot_font

NMA_NETWORK_NODE_ORDER <- c("Telemedicine", "Web-app", "Mobile-app", "Face to face visit", "Biofeedback device", 
    "Virtual Reality", "Control", "Exergaming")

NMA_NODE_DISPLAY_LABELS <- c(Telemedicine = "Telemedicine", `Web-app` = "Web-App", `Mobile-app` = "Mobile-App", 
    `Face to face visit` = "Face to face visit", `Biofeedback device` = "Biofeedback devices", `Virtual Reality` = "VR", 
    Control = "Control", Exergaming = "Exergaming")

NMA_NODE_CANONICAL_ALIASES <- c(telemedicine = "Telemedicine", webapp = "Web-app", mobileapp = "Mobile-app", 
    facetofacevisit = "Face to face visit", facetoface = "Face to face visit", biofeedbackdevice = "Biofeedback device", 
    biofeedbackdevices = "Biofeedback device", biofeedback = "Biofeedback device", virtualreality = "Virtual Reality", 
    vr = "Virtual Reality", control = "Control", exergaming = "Exergaming")

make_bmj_style_network_plot <- function(node_summary, edge_summary, sequence = NULL, file_prefix, plot_title = "", 
    bottom_node = "Control", bg_color = "white", node_color = BMJ_PRIMARY, edge_color = BMJ_PRIMARY, 
    edge_alpha = 1, anchor_angle = -pi/6, layout_radius = 0.82, node_radius_range = c(0.014, 0.115), 
    edge_width_range = c(1.2, 5.25), edge_width_step = 0.9, label_offset = 0.07, label_wrap_width = 16, 
    show_node_sample_size = FALSE, title_cex = NMA_NETWORK_TITLE_CEX, title_y_ndc = 0.925, label_cex = NMA_NETWORK_LABEL_CEX, 
    font_family = NMA_FONT_BODY_NAME) {
    font_roles <- get_nma_font_roles(font_family, vector_device = TRUE)
    light_font <- unname(font_roles[["light"]])
    title_font <- unname(font_roles[["title"]])
    nodes <- unique(as.character(node_summary$treatment))
    if (is.null(sequence)) {
        sequence <- nodes
    }
    else {
        sequence <- unique(c(sequence[sequence %in% nodes], setdiff(nodes, sequence)))
    }
    k <- length(sequence)
    if (k < 2) {
        stop("A network plot requires at least two treatment nodes.")
    }
    angles <- seq(from = pi/2, to = pi/2 - 2 * pi * (k - 1)/k, length.out = k)
    if (!is.null(bottom_node) && bottom_node %in% sequence) {
        current_angle <- angles[match(bottom_node, sequence)]
        angles <- angles + (anchor_angle - current_angle)
    }
    coords <- data.frame(treatment = sequence, angle = angles, x = layout_radius * cos(angles), y = layout_radius * 
        sin(angles), stringsAsFactors = FALSE)
    if (!"n_participants" %in% names(node_summary)) {
        stop("node_summary is missing the n_participants column.", "Check that node_summary was generated correctly.")
    }
    n_participants <- suppressWarnings(as.numeric(node_summary$n_participants[match(sequence, node_summary$treatment)]))
    valid_n <- n_participants[is.finite(n_participants) & n_participants > 0]
    if (length(valid_n) == 0) {
        stop("node_summary contains no valid participant counts.")
    }
    if (any(!is.finite(n_participants) | n_participants <= 0)) {
        warning("Some nodes have no valid participant count; ", "the smallest valid node count will be used for plotting.")
        n_participants[!is.finite(n_participants) | n_participants <= 0] <- min(valid_n)
    }
    node_radius_range <- sort(as.numeric(node_radius_range[1:2]))
    sqrt_n <- sqrt(n_participants)
    node_radius <- node_radius_range[2] * sqrt_n/max(sqrt_n)
    node_radius <- pmax(node_radius_range[1], pmin(node_radius_range[2], node_radius))
    node_radius <- setNames(node_radius, sequence)
    coords$n_participants <- n_participants
    coords$node_radius <- node_radius[coords$treatment]
    edge_summary <- as.data.frame(edge_summary, stringsAsFactors = FALSE)
    required_edge_columns <- c("from", "to", "n_studies")
    if (!all(required_edge_columns %in% names(edge_summary))) {
        stop("edge_summary is missing required columns: ", paste(setdiff(required_edge_columns, names(edge_summary)), 
            collapse = ", "))
    }
    edge_summary <- edge_summary %>% mutate(from = as.character(from), to = as.character(to), n_studies = as.numeric(n_studies))
    if (nrow(edge_summary) > 0) {
        edge_summary$n_studies[!is.finite(edge_summary$n_studies) | edge_summary$n_studies <= 0] <- 1
        edge_width_range <- sort(as.numeric(edge_width_range[1:2]))
        edge_summary$lwd <- pmin(edge_width_range[2], edge_width_range[1] + edge_width_step * (edge_summary$n_studies - 
            1))
        edge_summary$lwd <- pmax(edge_width_range[1], edge_summary$lwd)
        edge_summary <- edge_summary %>% arrange(desc(lwd))
    }
    edge_colour_plot <- grDevices::adjustcolor(edge_color, alpha.f = edge_alpha)
    display_labels <- display_nma_node(coords$treatment)
    wrap_label <- function(x) {
        paste(strwrap(x, width = label_wrap_width), collapse = "\n")
    }
    display_labels <- vapply(display_labels, wrap_label, character(1))
    if (isTRUE(show_node_sample_size)) {
        sample_size_text <- format(round(coords$n_participants), scientific = FALSE, trim = TRUE)
        display_labels <- paste0(display_labels, "\n(n=", sample_size_text, ")")
    }
    label_distance <- layout_radius + max(coords$node_radius) + label_offset
    coords$label_radius <- label_distance
    coords$label_x <- cos(coords$angle) * label_distance
    coords$label_y <- sin(coords$angle) * label_distance
    top_centre_ids <- which(coords$y > 0.56 & abs(coords$x) < 0.2)
    for (i in top_centre_ids) {
        single_line_label <- gsub("\n", " ", display_labels[i], fixed = TRUE)
        if (nchar(single_line_label) <= 24L) {
            display_labels[i] <- single_line_label
        }
    }
    coords$display_label <- display_labels
    make_network_title <- function(x) {
        x <- trimws(as.character(x))
        x <- gsub("[[:space:]]+", " ", x)
        title_parts <- strsplit(x, "\\s+[\u2014\u2013]\\s+", perl = TRUE)[[1]]
        if (length(title_parts) >= 2) {
            outcome_part <- trimws(title_parts[1])
            outcome_key <- gsub("[^a-z0-9]+", "", tolower(outcome_part))
            outcome_part <- make_display_outcome_name(outcome_part)
            time_part <- trimws(paste(title_parts[-1], collapse = " "))
            time_part <- sub("^post intervention$", "Post-intervention", time_part, ignore.case = TRUE)
            time_part <- sub("^short term$", "Short-term", time_part, ignore.case = TRUE)
            time_part <- sub("^mid term$", "Mid-term", time_part, ignore.case = TRUE)
            time_part <- sub("^long term$", "Long-term", time_part, ignore.case = TRUE)
            return(paste0(outcome_part, "\n", time_part))
        }
        x
    }
    title_display <- make_network_title(plot_title)
    title_display <- sub("^QoL(?=\\n|$)", "Health-related quality of life", title_display, ignore.case = TRUE, 
        perl = TRUE)
    title_display <- sub("^Pain[[:space:]]+fear[[:space:]]+avoidance(?=\\n|$)", "Pain-related fear avoidance", 
        title_display, ignore.case = TRUE, perl = TRUE)
    title_display <- sub("^Painfearavoindance(?=\\n|$)", "Pain-related fear avoidance", title_display, 
        ignore.case = TRUE, perl = TRUE)
    if (grepl("^(QoL|Pain[[:space:]]+fear[[:space:]]+avoidance|Painfearavoindance)(?=\\n|$)", title_display, 
        ignore.case = TRUE, perl = TRUE)) {
        stop("The previous network title was not replaced; SVG generation was stopped.")
    }
    message("Network plot title : ", gsub("\n", " / ", title_display, fixed = TRUE))
    network_file <- paste0(file_prefix, "_network.svg")
    if (file.exists(network_file)) {
        remove_status <- unlink(network_file, force = TRUE)
        if (remove_status != 0 || file.exists(network_file)) {
            stop("Unable to remove the existing network plot: ", network_file)
        }
    }
    open_nma_svg(filename = network_file, width_in = 7.2, height_in = 6.2, pointsize = 12, bg = bg_color)
    on.exit(abort_nma_svg(), add = TRUE)
    par(bg = bg_color, mar = c(0.05, 0.05, 0.05, 0.05), xpd = NA, family = light_font, font = NMA_FONT_REGULAR_FACE, 
        font.main = 1, font.lab = 1, font.axis = 1, lend = "round", ljoin = "round", lheight = 0.88)
    plot.new()
    plot.window(xlim = c(-1.58, 1.58), ylim = c(-1.23, 1.47), asp = 1)
    right_label_safe_x <- grconvertX(0.965, from = "ndc", to = "user")
    right_label_ids <- which(coords$x > 0.2)
    shifted_right_labels <- character()
    for (i in right_label_ids) {
        label_lines <- strsplit(coords$display_label[i], "\n", fixed = TRUE)[[1]]
        label_width_user <- suppressWarnings(max(strwidth(label_lines, cex = label_cex, font = NMA_FONT_REGULAR_FACE, 
            units = "user"), na.rm = TRUE))
        if (is.finite(label_width_user) && label_width_user > 0) {
            old_label_x <- coords$label_x[i]
            minimum_label_x <- coords$x[i] + coords$node_radius[i] + 0.025
            coords$label_x[i] <- max(minimum_label_x, min(old_label_x, right_label_safe_x - label_width_user))
            if (coords$label_x[i] < old_label_x) {
                shifted_right_labels <- c(shifted_right_labels, coords$display_label[i])
            }
        }
    }
    if (length(shifted_right_labels) > 0L) {
        message("Right network label shifted inside canvas: ", paste(unique(gsub("\n", " ", shifted_right_labels, 
            fixed = TRUE)), collapse = "; "))
    }
    title_y_ndc <- suppressWarnings(as.numeric(title_y_ndc)[1])
    if (!is.finite(title_y_ndc)) {
        title_y_ndc <- 0.925
    }
    title_y_ndc <- min(max(title_y_ndc, 0.8), 0.925)
    title_y_user <- grconvertY(title_y_ndc, from = "ndc", to = "user")
    title_height_user <- suppressWarnings(as.numeric(strheight(title_display, cex = title_cex, font = NMA_FONT_BOLD_FACE, 
        units = "user"))[1])
    if (!is.finite(title_height_user) || title_height_user <= 0) {
        title_height_user <- 0.2
    }
    title_bottom_user <- title_y_user - title_height_user/2
    title_label_gap_user <- 0.06
    top_label_ids <- which(coords$label_y > 0.56 & abs(coords$label_x) < 0.62)
    shifted_top_labels <- character()
    for (i in top_label_ids) {
        label_height_user <- suppressWarnings(as.numeric(strheight(coords$display_label[i], cex = label_cex, 
            font = NMA_FONT_REGULAR_FACE, units = "user"))[1])
        if (!is.finite(label_height_user) || label_height_user <= 0) {
            label_height_user <- 0.12 * length(strsplit(coords$display_label[i], "\n", fixed = TRUE)[[1]])
        }
        highest_safe_label_y <- title_bottom_user - title_label_gap_user - label_height_user
        lowest_safe_label_y <- coords$y[i] + coords$node_radius[i] + 0.025
        if (coords$label_y[i] > highest_safe_label_y) {
            coords$label_y[i] <- max(lowest_safe_label_y, highest_safe_label_y)
            shifted_top_labels <- c(shifted_top_labels, gsub("\n", " ", coords$display_label[i], fixed = TRUE))
        }
    }
    if (length(shifted_top_labels) > 0L) {
        message("Top network label shifted below title: ", paste(unique(shifted_top_labels), collapse = "; "))
    }
    text(x = 0, y = title_y_user, labels = title_display, adj = c(0.5, 0.5), family = title_font, font = NMA_FONT_BOLD_FACE, 
        cex = title_cex, col = BMJ_TEXT_DARK)
    for (i in seq_len(nrow(edge_summary))) {
        p1 <- coords[coords$treatment == edge_summary$from[i], ]
        p2 <- coords[coords$treatment == edge_summary$to[i], ]
        if (nrow(p1) == 1 && nrow(p2) == 1) {
            segments(x0 = p1$x, y0 = p1$y, x1 = p2$x, y1 = p2$y, lwd = edge_summary$lwd[i], col = edge_colour_plot, 
                lend = "round")
        }
    }
    symbols(x = coords$x, y = coords$y, circles = node_radius[coords$treatment], inches = FALSE, add = TRUE, 
        bg = node_color, fg = node_color)
    for (i in seq_len(nrow(coords))) {
        adj_x <- ifelse(coords$x[i] > 0.2, 0, ifelse(coords$x[i] < -0.2, 1, 0.5))
        adj_y <- ifelse(coords$y[i] > 0.56, 0, ifelse(coords$y[i] < -0.56, 1, 0.5))
        text(x = coords$label_x[i], y = coords$label_y[i], labels = coords$display_label[i], adj = c(adj_x, 
            adj_y), family = light_font, font = NMA_FONT_REGULAR_FACE, cex = label_cex, col = BMJ_TEXT_DARK)
    }
    close_nma_svg(network_file, verify_outline = TRUE)
    export_nma_jpeg_from_svg(network_file, sub("\\.svg$", ".jpg", network_file))
    on.exit(NULL, add = FALSE)
    if (!file.exists(network_file)) {
        stop("Network plot was not created: ", network_file)
    }
    invisible(list(svg_file = network_file, plot_data = coords, edge_plot_data = edge_summary))
}

make_bmj_style_forest_plot <- function(nma_vs_reference, node_summary = NULL, reference_group, file_prefix, 
    plot_title, treatment_order = NULL, effect_label = "Standardised mean\ndifferences (95% CI)", null_value = 0, 
    xlog = FALSE, forest_xlim = NMA_FOREST_XLIM, forest_ticks = NMA_FOREST_TICKS, show_truncation_arrows = TRUE, 
    digits = 2, favours_left = "Favours treatment", favours_right = NULL, bmj_blue = BMJ_PRIMARY, bmj_blue_light = BMJ_PRIMARY_LIGHT, 
    bg_color = "white", font_family = NMA_FONT_BODY_NAME, point_shape = 23, add_row_separators = FALSE, 
    row_sep_col = "#E3E3E6", row_sep_lwd = 0.6, row_height_in = NMA_FOREST_ROW_HEIGHT_IN, header_height_in = NMA_FOREST_HEADER_HEIGHT_IN, 
    header_data_gap_in = NMA_FOREST_HEADER_DATA_GAP_IN, add_forest_box = TRUE, left_header = "Treatment node", 
    count_header = "No of studies/\nNo of patients", outer_border = "#9AA0A6", show_plot_title = FALSE) {
    font_roles <- get_nma_font_roles(font_family, vector_device = TRUE)
    light_font <- unname(font_roles[["light"]])
    bold_font <- unname(font_roles[["bold"]])
    title_font <- unname(font_roles[["title"]])
    required_cols <- c("treatment", "SMD_vs_reference", "lower_95_CI", "upper_95_CI")
    if (!all(required_cols %in% names(nma_vs_reference))) {
        stop("nma_vs_reference is missing required columns: ", paste(required_cols, collapse = ", "))
    }
    tab <- nma_vs_reference %>% transmute(treatment = as.character(treatment), estimate = as.numeric(SMD_vs_reference), 
        lower = as.numeric(lower_95_CI), upper = as.numeric(upper_95_CI)) %>% filter(treatment != reference_group)
    if (is.null(treatment_order)) {
        treatment_order <- tab$treatment
    }
    treatment_order <- setdiff(treatment_order, reference_group)
    treatment_order <- c(treatment_order[treatment_order %in% tab$treatment], setdiff(tab$treatment, 
        treatment_order))
    tab <- tab %>% mutate(.order = match(treatment, treatment_order)) %>% arrange(is.na(.order), .order, 
        treatment) %>% select(-.order)
    plot_tab <- tab
    plot_tab$treatment_label <- display_nma_node(plot_tab$treatment)
    n_rows <- nrow(plot_tab)
    if (n_rows == 0) {
        stop("No non-reference treatment nodes are available for the forest plot.")
    }
    plot_tab$y <- rev(seq_len(n_rows))
    if (!is.finite(row_height_in) || row_height_in <= 0) {
        stop("row_height_in must be positive.")
    }
    if (!is.finite(header_height_in) || header_height_in <= 0) {
        stop("header_height_in must be positive.")
    }
    if (!is.finite(header_data_gap_in) || header_data_gap_in < 0) {
        stop("header_data_gap_in must be non-negative.")
    }
    if (!is.null(node_summary) && all(c("treatment", "n_studies", "n_participants") %in% names(node_summary))) {
        node_match <- match(plot_tab$treatment, as.character(node_summary$treatment))
        plot_tab$n_studies <- suppressWarnings(as.numeric(node_summary$n_studies[node_match]))
        plot_tab$n_participants <- suppressWarnings(as.numeric(node_summary$n_participants[node_match]))
    }
    else {
        plot_tab$n_studies <- NA_real_
        plot_tab$n_participants <- NA_real_
    }
    fmt_num <- function(x) {
        sprintf(paste0("%.", digits, "f"), x)
    }
    plot_tab$effect_txt <- ifelse(is.finite(plot_tab$estimate) & is.finite(plot_tab$lower) & is.finite(plot_tab$upper), 
        paste0(fmt_num(plot_tab$estimate), " (", fmt_num(plot_tab$lower), " to ", fmt_num(plot_tab$upper), 
            ")"), "")
    plot_tab$count_txt <- ifelse(is.finite(plot_tab$n_studies) & is.finite(plot_tab$n_participants), 
        paste0(round(plot_tab$n_studies), "/", round(plot_tab$n_participants)), "\u2014")
    if (is.null(favours_right)) {
        favours_right <- paste("Favours", reference_group)
    }
    if (xlog) {
        if (any(plot_tab$estimate[!is.na(plot_tab$lower)] <= 0, na.rm = TRUE) || any(plot_tab$lower[!is.na(plot_tab$lower)] <= 
            0, na.rm = TRUE) || null_value <= 0) {
            stop("When xlog = TRUE, estimate, lower, upper, and null_value must all be greater than zero.")
        }
        plot_tab$estimate_plot <- log(plot_tab$estimate)
        plot_tab$lower_plot <- log(plot_tab$lower)
        plot_tab$upper_plot <- log(plot_tab$upper)
        null_plot <- log(null_value)
        finite_vals <- c(plot_tab$lower_plot[is.finite(plot_tab$lower_plot)], plot_tab$upper_plot[is.finite(plot_tab$upper_plot)], 
            null_plot)
        pad <- max(0.12, diff(range(finite_vals)) * 0.08)
        xlim <- c(min(finite_vals) - pad, max(finite_vals) + pad)
        raw_ticks <- c(0.25, 0.5, 1, 2, 4)
        raw_ticks <- raw_ticks[raw_ticks > exp(xlim[1]) & raw_ticks < exp(xlim[2])]
        raw_ticks <- sort(unique(c(raw_ticks, null_value)))
        axis_at <- log(raw_ticks)
        axis_lab <- raw_ticks
    }
    else {
        plot_tab$estimate_plot <- plot_tab$estimate
        plot_tab$lower_plot <- plot_tab$lower
        plot_tab$upper_plot <- plot_tab$upper
        null_plot <- null_value
        if (!is.null(forest_xlim)) {
            if (length(forest_xlim) != 2L || any(!is.finite(forest_xlim)) || forest_xlim[1] >= forest_xlim[2]) {
                stop("forest_xlim must contain two finite values in increasing order.")
            }
            xlim <- as.numeric(forest_xlim)
            if (null_plot < xlim[1] || null_plot > xlim[2]) {
                stop("null_value must lie within forest_xlim.")
            }
        }
        else {
            finite_vals <- c(plot_tab$lower_plot[is.finite(plot_tab$lower_plot)], plot_tab$upper_plot[is.finite(plot_tab$upper_plot)], 
                null_plot)
            pad <- max(0.12, diff(range(finite_vals)) * 0.08)
            xlim <- c(min(finite_vals) - pad, max(finite_vals) + pad)
        }
        if (!is.null(forest_ticks)) {
            axis_at <- sort(unique(as.numeric(forest_ticks)))
            axis_at <- axis_at[is.finite(axis_at)]
            axis_at <- sort(unique(c(axis_at, null_plot)))
            axis_at <- axis_at[axis_at >= xlim[1] & axis_at <= xlim[2]]
        }
        else {
            pretty_all <- sort(unique(pretty(xlim, n = 4)))
            pretty_all <- pretty_all[is.finite(pretty_all)]
            axis_at <- pretty_all[pretty_all >= xlim[1] & pretty_all <= xlim[2]]
            axis_at <- sort(unique(c(axis_at, null_plot)))
            tick_diffs <- diff(pretty_all)
            tick_diffs <- tick_diffs[is.finite(tick_diffs) & tick_diffs > 0]
            tick_step <- if (length(tick_diffs) > 0L) {
                min(tick_diffs)
            }
            else {
                max(diff(xlim)/4, 0.1)
            }
            if (!any(axis_at < null_plot)) {
                left_candidates <- pretty_all[pretty_all < null_plot]
                left_tick <- if (length(left_candidates) > 0L) {
                  max(left_candidates)
                }
                else {
                  null_plot - tick_step
                }
                axis_at <- c(left_tick, axis_at)
            }
            if (!any(axis_at > null_plot)) {
                right_candidates <- pretty_all[pretty_all > null_plot]
                right_tick <- if (length(right_candidates) > 0L) {
                  min(right_candidates)
                }
                else {
                  null_plot + tick_step
                }
                axis_at <- c(axis_at, right_tick)
            }
            axis_at <- sort(unique(axis_at))
            xlim <- range(c(xlim, axis_at), finite = TRUE)
        }
        axis_lab <- axis_at
    }
    precision <- with(plot_tab, ifelse(is.finite(lower) & is.finite(upper) & upper > lower, 1/(upper - 
        lower), NA_real_))
    point_cex <- rep(1.02, n_rows)
    if (sum(is.finite(precision)) >= 2) {
        precision_finite <- precision[is.finite(precision)]
        if (diff(range(precision_finite)) > 0) {
            scaled_p <- (precision_finite - min(precision_finite))/(max(precision_finite) - min(precision_finite))
            point_cex[is.finite(precision)] <- 0.82 + (scaled_p^1.15) * 0.65
        }
    }
    point_cex <- point_cex * NMA_FOREST_POINT_SCALE
    svg_file <- paste0(file_prefix, "_forest.svg")
    header_rule_offset <- header_data_gap_in/row_height_in
    header_height_units <- header_height_in/row_height_in
    outer_top_offset <- header_rule_offset + header_height_units
    top_device_padding_units <- 0.05/row_height_in
    favours_y <- -1.34
    symmetric_text_padding_units <- header_height_units/2
    outer_bottom <- favours_y - symmetric_text_padding_units
    bottom_device_padding_units <- 0.05/row_height_in
    plot_y_min <- outer_bottom - bottom_device_padding_units
    plot_y_max <- n_rows + outer_top_offset + top_device_padding_units
    forest_height_in <- row_height_in * (plot_y_max - plot_y_min)
    open_nma_svg(filename = svg_file, width_in = NMA_FOREST_WIDTH_IN, height_in = forest_height_in, pointsize = 12, 
        bg = bg_color)
    on.exit(abort_nma_svg(), add = TRUE)
    par(mai = c(0, 0, 0, 0), bg = bg_color, xpd = NA, family = light_font, font = NMA_FONT_REGULAR_FACE, 
        font.main = 1, font.lab = 1, font.axis = 1, lend = "round", ljoin = "round", lheight = 0.88)
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(plot_y_min, plot_y_max))
    outer_left <- 0.025
    outer_right <- 0.975
    header_rule_y <- n_rows + header_rule_offset
    outer_top <- n_rows + outer_top_offset
    header_y <- mean(c(outer_top, header_rule_y))
    content_padding_x <- 0.013
    treatment_reference_width <- max(strwidth(c(NMA_FOREST_LAYOUT_LONGEST_LABEL, plot_tab$treatment_label),
        units = "user", cex = 0.76 * NMA_FOREST_TEXT_SCALE, family = light_font,
        font = NMA_FONT_REGULAR_FACE), na.rm = TRUE)
    count_reference_width <- max(strwidth(c(NMA_FOREST_LAYOUT_COUNT_REFERENCE, plot_tab$count_txt),
        units = "user", cex = 0.74 * NMA_FOREST_TEXT_SCALE, family = light_font,
        font = NMA_FONT_REGULAR_FACE), na.rm = TRUE)
    effect_body_width <- max(strwidth(plot_tab$effect_txt, units = "user", cex = 0.74 *
        NMA_FOREST_TEXT_SCALE, family = light_font, font = NMA_FONT_REGULAR_FACE), na.rm = TRUE)
    effect_header_width <- max(strwidth(strsplit(effect_label, "\n", fixed = TRUE)[[1]],
        units = "user", cex = 0.80 * NMA_FOREST_TEXT_SCALE, family = title_font,
        font = NMA_FONT_BOLD_FACE), na.rm = TRUE)
    content_left <- outer_left + content_padding_x
    content_right <- outer_right - content_padding_x
    equal_column_gap_x <- (content_right - content_left - treatment_reference_width -
        count_reference_width - NMA_FOREST_PANEL_WIDTH_X - effect_body_width) / 3
    if (!is.finite(equal_column_gap_x) || equal_column_gap_x <= 0) {
        stop("Forest-plot columns do not fit within the fixed outer frame.")
    }
    treatment_x <- content_left
    treatment_header_x <- treatment_x
    count_x <- treatment_x + treatment_reference_width + equal_column_gap_x + count_reference_width/2
    count_header_x <- count_x
    forest_left <- count_x + count_reference_width/2 + equal_column_gap_x
    forest_right <- forest_left + NMA_FOREST_PANEL_WIDTH_X
    effect_x <- forest_right + equal_column_gap_x
    effect_header_x <- content_right - effect_header_width
    panel_bottom <- 0.58
    panel_top <- n_rows + header_rule_offset * 0.48
    axis_label_y <- panel_bottom - 0.12
    draw_rounded_rect_base(xleft = outer_left, ybottom = outer_bottom, xright = outer_right, ytop = outer_top, 
        rx = 0.012, ry = 0.12, border = outer_border, fill = "white", lwd = 1.15)
    segments(x0 = outer_left, y0 = header_rule_y, x1 = outer_right, y1 = header_rule_y, col = outer_border, 
        lwd = 0.9)
    if (isTRUE(show_plot_title)) {
        text(x = 0.5, y = outer_top + 0.12, labels = plot_title, family = bold_font, font = NMA_FONT_BOLD_FACE, 
            cex = 0.86 * NMA_FOREST_TEXT_SCALE, col = BMJ_TEXT_DARK)
    }
    text(x = treatment_header_x, y = header_y, labels = left_header, adj = c(0, 0.5), family = title_font, 
        font = NMA_FONT_BOLD_FACE, cex = 0.86 * NMA_FOREST_TEXT_SCALE, col = BMJ_TEXT_DARK)
    text(x = count_header_x, y = header_y, labels = count_header, adj = c(0.5, 0.5), family = title_font, 
        font = NMA_FONT_BOLD_FACE, cex = 0.82 * NMA_FOREST_TEXT_SCALE, col = BMJ_TEXT_DARK)
    text(x = mean(c(forest_left, forest_right)), y = header_y, labels = effect_label, adj = c(0.5, 0.5), 
        family = title_font, font = NMA_FONT_BOLD_FACE, cex = 0.8 * NMA_FOREST_TEXT_SCALE, col = BMJ_TEXT_DARK)
    text(x = effect_header_x, y = header_y, labels = effect_label, adj = c(0, 0.5), family = title_font, font = NMA_FONT_BOLD_FACE, 
        cex = 0.8 * NMA_FOREST_TEXT_SCALE, col = BMJ_TEXT_DARK)
    text(x = treatment_x, y = plot_tab$y, labels = plot_tab$treatment_label, adj = c(0, 0.5), family = light_font, 
        font = NMA_FONT_REGULAR_FACE, cex = 0.76 * NMA_FOREST_TEXT_SCALE, col = BMJ_TEXT_DARK)
    text(x = count_x, y = plot_tab$y, labels = plot_tab$count_txt, adj = c(0.5, 0.5), family = light_font, 
        font = NMA_FONT_REGULAR_FACE, cex = 0.74 * NMA_FOREST_TEXT_SCALE, col = BMJ_TEXT_DARK)
    text(x = effect_x, y = plot_tab$y, labels = plot_tab$effect_txt, adj = c(0, 0.5), family = light_font, 
        font = NMA_FONT_REGULAR_FACE, cex = 0.74 * NMA_FOREST_TEXT_SCALE, col = BMJ_TEXT_DARK)
    if (isTRUE(add_row_separators) && n_rows > 1) {
        for (yy in seq(1.5, n_rows - 0.5, by = 1)) {
            segments(x0 = treatment_x, y0 = yy, x1 = outer_right - 0.02, y1 = yy, col = row_sep_col, 
                lwd = row_sep_lwd)
        }
    }
    if (isTRUE(add_forest_box)) {
        draw_rounded_rect_base(xleft = forest_left, ybottom = panel_bottom, xright = forest_right, ytop = panel_top, 
            rx = 0.012, ry = 0.13, border = outer_border, fill = "white", lwd = 1.05)
    }
    map_effect_x <- function(value) {
        forest_left + (value - xlim[1])/diff(xlim) * (forest_right - forest_left)
    }
    null_x <- map_effect_x(null_plot)
    segments(x0 = null_x, y0 = panel_bottom, x1 = null_x, y1 = panel_top, lwd = 0.85, col = "#A9A9AF")
    for (i in seq_len(n_rows)) {
        if (is.finite(plot_tab$lower_plot[i]) && is.finite(plot_tab$upper_plot[i]) && is.finite(plot_tab$estimate_plot[i])) {
            ci_left <- max(plot_tab$lower_plot[i], xlim[1])
            ci_right <- min(plot_tab$upper_plot[i], xlim[2])
            if (ci_left <= ci_right) {
                segments(x0 = map_effect_x(ci_left), y0 = plot_tab$y[i], x1 = map_effect_x(ci_right), 
                  y1 = plot_tab$y[i], lwd = 1.6, col = bmj_blue)
            }
            arrow_span <- diff(xlim) * 0.055
            if (isTRUE(show_truncation_arrows) && plot_tab$lower_plot[i] < xlim[1]) {
                arrows(x0 = map_effect_x(xlim[1] + arrow_span), y0 = plot_tab$y[i], x1 = map_effect_x(xlim[1]), 
                  y1 = plot_tab$y[i], length = 0.07, angle = 25, code = 2, lwd = 1.6, col = bmj_blue)
            }
            if (isTRUE(show_truncation_arrows) && plot_tab$upper_plot[i] > xlim[2]) {
                arrows(x0 = map_effect_x(xlim[2] - arrow_span), y0 = plot_tab$y[i], x1 = map_effect_x(xlim[2]), 
                  y1 = plot_tab$y[i], length = 0.07, angle = 25, code = 2, lwd = 1.6, col = bmj_blue)
            }
            if (plot_tab$estimate_plot[i] >= xlim[1] && plot_tab$estimate_plot[i] <= xlim[2]) {
                points(x = map_effect_x(plot_tab$estimate_plot[i]), y = plot_tab$y[i], pch = point_shape, 
                  bg = bmj_blue, col = bmj_blue, cex = point_cex[i], lwd = 0.9)
            }
        }
    }
    axis_x <- map_effect_x(axis_at)
    axis_label <- if (xlog) {
        axis_lab
    }
    else {
        format(axis_lab, trim = TRUE, scientific = FALSE)
    }
    segments(x0 = axis_x, y0 = panel_bottom, x1 = axis_x, y1 = panel_bottom - 0.065, col = "#222222", 
        lwd = 0.75)
    text(x = axis_x, y = axis_label_y, labels = axis_label, adj = c(0.5, 1), family = light_font, font = NMA_FONT_REGULAR_FACE, 
        cex = 0.7 * NMA_FOREST_TEXT_SCALE, col = BMJ_TEXT_DARK)
    format_favours <- function(x) {
        sub("^Favours[[:space:]]+", "Favours\n", x, ignore.case = TRUE)
    }
    text(x = mean(c(forest_left, null_x)), y = favours_y, labels = format_favours(favours_left), adj = c(0.5, 
        0.5), family = title_font, font = NMA_FONT_BOLD_FACE, cex = 0.76 * NMA_FOREST_TEXT_SCALE, col = BMJ_TEXT_DARK)
    text(x = mean(c(null_x, forest_right)), y = favours_y, labels = format_favours(favours_right), adj = c(0.5, 
        0.5), family = title_font, font = NMA_FONT_BOLD_FACE, cex = 0.76 * NMA_FOREST_TEXT_SCALE, col = BMJ_TEXT_DARK)
    close_nma_svg(svg_file, verify_outline = TRUE)
    export_nma_jpeg_from_svg(svg_file, sub("\\.svg$", ".jpg", svg_file))
    on.exit(NULL, add = FALSE)
    if (!file.exists(svg_file)) {
        stop("Forest plot was not created: ", svg_file)
    }
    invisible(list(plot_data = plot_tab, svg_file = svg_file))
}

# Apply the same NMA model after restricting studies to the prespecified pain
# types CLBP, CNSLBP, and CDLBP.
run_frequentist_nma <- function(arm_file, time_name = c("post_intervention", "short_term", "mid_term", 
    "long_term"), outcome_name = "Continuous outcome", output_dir = "results", arm_sheet = "Sheet1", 
    reference_group = NULL, exclude_studies = NULL, trial_map = NULL, reported_smd_data = NULL, node_order = NULL, 
    forest_order = NULL, small_values = "desirable", bmj_blue = BMJ_PRIMARY, bmj_blue_light = BMJ_PRIMARY_LIGHT, 
    bmj_background = "white", plot_font_family = NMA_FONT_BODY_NAME, plot_disconnected_network = TRUE) {
    load_nma_packages()
    time_name <- match.arg(time_name)
    outcome_display_name <- make_display_outcome_name(outcome_name)
    higher_scores_are_better <- is_higher_better_outcome(outcome_display_name)
    small_values <- small_values_for_outcome(outcome_display_name)
    direction_convention <- direction_convention_for_outcome(outcome_display_name)
    if (tolower(stringr::str_squish(outcome_name)) == "physical function") {
        outcome_name <- "Disability"
    }
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }
    file_prefix <- file.path(output_dir, analysis_file_stub(outcome_display_name, time_name))
    plot_title_clean <- make_readable_title(outcome_name = outcome_display_name, time_name = time_name)
    # Read the prespecified positional workbook layout documented in DATA_INPUT_GUIDE.md.
    raw <- readxl::read_excel(path = arm_file, sheet = arm_sheet, skip = 2, col_names = FALSE, .name_repair = "minimal")
    if (ncol(raw) < 9) {
        stop("The outcome sheet has fewer than nine columns, so the arm-level structure cannot be identified.", "Columns A:I are required: Study, note, Group, ", 
            "baseline n/Mean/SD, and post-intervention n/Mean/SD.")
    }
    if (ncol(raw) < 18) {
        original_ncol <- ncol(raw)
        n_missing_cols <- 18 - original_ncol
        for (i in seq_len(n_missing_cols)) {
            raw[[paste0(".empty_time_column_", i)]] <- rep(NA, nrow(raw))
        }
        message("The outcome sheet contains ", original_ncol, " columns; the final ", n_missing_cols, 
            " missing columns were filled with NA.")
    }
    raw <- raw[, 1:18]
    colnames(raw) <- c("Study", "note", "Group", "base_n", "base_mean", "base_sd", "post_n", "post_mean", 
        "post_sd", "short_n", "short_mean", "short_sd", "mid_n", "mid_mean", "mid_sd", "long_n", "long_mean", 
        "long_sd")
    raw <- raw %>% mutate(excel_row = row_number() + 2)
    measure_cols <- c("base_n", "base_mean", "base_sd", "post_n", "post_mean", "post_sd", "short_n", 
        "short_mean", "short_sd", "mid_n", "mid_mean", "mid_sd", "long_n", "long_mean", "long_sd")
    arm_all <- raw %>% mutate(Study = str_squish(as.character(Study)), note = str_squish(as.character(note)), 
        Group = canonicalise_nma_node(Group)) %>% mutate(across(all_of(measure_cols), ~na_if(str_squish(as.character(.x)), 
        ""))) %>% pivot_longer(cols = all_of(measure_cols), names_to = c("time_window", ".value"), names_pattern = "^(base|post|short|mid|long)_(n|mean|sd)$") %>% 
        mutate(time_window = recode(time_window, base = "baseline", post = "post_intervention", short = "short_term", 
            mid = "mid_term", long = "long_term"), n = suppressWarnings(as.numeric(n)), mean = suppressWarnings(as.numeric(mean)), 
            sd = suppressWarnings(as.numeric(sd))) %>% filter(time_window == time_name, !is.na(Study), 
        Study != "", !is.na(Group), Group != "")
    if (nrow(arm_all) == 0) {
        stop("No study arms are available for this time window.")
    }
    if (!is.null(trial_map)) {
        if (!all(c("Study", "trial_id") %in% names(trial_map))) {
            stop("trial_map must contain Study and trial_id columns.")
        }
        trial_map <- trial_map %>% transmute(Study = str_squish(as.character(Study)), trial_id = str_squish(as.character(trial_id))) %>% 
            distinct()
        arm_all <- arm_all %>% left_join(trial_map, by = "Study") %>% mutate(study_id = if_else(is.na(trial_id) | 
            trial_id == "", Study, trial_id))
    }
    else {
        arm_all <- arm_all %>% mutate(study_id = Study)
    }
    if (!is.null(exclude_studies)) {
        exclude_studies <- str_squish(as.character(exclude_studies))
        arm_all <- arm_all %>% filter(!(Study %in% exclude_studies), !(study_id %in% exclude_studies))
    }
    no_time_data <- arm_all %>% filter(is.na(n) & is.na(mean) & is.na(sd))
    if (nrow(no_time_data) > 0) {
        message("This time window has ", nrow(no_time_data), " study arms without n, Mean, and SD; they were excluded.")
    }
    arm_data <- arm_all %>% filter(!(is.na(n) & is.na(mean) & is.na(sd)))
    if (nrow(arm_data) == 0) {
        stop("No analysable study arms remain after excluding arms with no data for this time window.")
    }
    # Stop before modelling if a partially reported arm could bias the analysis.
    invalid_rows <- arm_data %>% filter(is.na(n) | is.na(mean) | is.na(sd) | !is.finite(n) | !is.finite(mean) | 
        !is.finite(sd) | n < 2 | sd <= 0)
    if (nrow(invalid_rows) > 0) {
        cat("\nInvalid data requiring correction:\n")
        print(invalid_rows %>% select(excel_row, Study, study_id, note, Group, time_window, n, mean, 
            sd), n = Inf)
        stop("Missing or invalid n, Mean, or SD values were found.", "Correct the source data using the Excel row numbers shown above.")
    }
    non_integer_n <- arm_data %>% filter(abs(n - round(n)) > 1e-08)
    if (nrow(non_integer_n) > 0) {
        warning("Non-integer sample sizes were found; check the n columns.")
    }
    # Harmonise all scales so that lower analysis values consistently indicate benefit.
    arm_data <- arm_data %>% mutate(outcome_direction = case_when(str_detect(note, fixed("\u2193")) ~ "lower_better", 
        str_detect(note, fixed("\u2191")) ~ "higher_better", TRUE ~ NA_character_))
    unknown_direction <- arm_data %>% filter(is.na(outcome_direction))
    if (nrow(unknown_direction) > 0) {
        cat("\nScales without an indicated direction (up or down):\n")
        print(unknown_direction %>% distinct(excel_row, Study, note, Group), n = Inf)
        stop("The direction of some scales cannot be determined.", "Mark each scale in note as higher-better or lower-better using the documented symbols.")
    }
    if (higher_scores_are_better && any(arm_data$outcome_direction != "higher_better")) {
        stop(outcome_display_name, " must use higher-is-better scales (note: ↑=better). ",
            "Correct the direction labels before analysis.")
    }
    arm_data <- arm_data %>% mutate(mean_original = mean, mean_analysis = if (isTRUE(higher_scores_are_better)) {
        mean
    } else {
        if_else(outcome_direction == "higher_better", -mean, mean)
    })
    scale_check <- arm_data %>% group_by(study_id) %>% summarise(n_notes = n_distinct(note), notes = paste(unique(note), 
        collapse = " | "), .groups = "drop") %>% filter(n_notes > 1)
    if (nrow(scale_check) > 0) {
        cat("\nTrials with multiple notes or scales in the same time window:\n")
        print(scale_check, n = Inf)
        stop("Different scale records were found within the same trial.", "Check whether multiple disability scales were extracted in error.")
    }
    duplicate_nodes <- arm_data %>% count(study_id, Group, name = "n_rows") %>% filter(n_rows > 1)
    if (nrow(duplicate_nodes) > 0) {
        duplicate_details <- arm_data %>% semi_join(duplicate_nodes, by = c("study_id", "Group")) %>% 
            arrange(study_id, Group, excel_row) %>% select(excel_row, Study, study_id, note, Group, n, 
            mean_original, sd)
        cat("\nDuplicate trial and Group combinations were found:\n")
        print(duplicate_details, n = Inf)
        stop("Determine whether these rows are duplicate entries, incorrect node names, ", "multiple reports from one trial, or separate arms assigned to the same node. ", 
            "Correct the workbook before rerunning the analysis.")
    }
    arm_counts <- arm_data %>% group_by(study_id) %>% summarise(n_arms = n_distinct(Group), .groups = "drop")
    single_arm_studies <- arm_counts %>% filter(n_arms < 2)
    single_arm_details <- arm_data %>% semi_join(single_arm_studies, by = "study_id")
    diagnostic_sheets <- list(all_arms_in_time_window = arm_all %>% select(excel_row, Study, study_id, 
        note, Group, time_window, n, mean, sd), no_time_data = no_time_data %>% select(excel_row, Study, 
        study_id, note, Group, time_window, n, mean, sd), valid_arm_data_before_single_arm_exclusion = arm_data %>% 
        select(excel_row, Study, study_id, note, Group, time_window, n, mean, sd), arm_counts = arm_counts, 
        single_arm_studies = single_arm_studies, single_arm_details = single_arm_details %>% select(excel_row, 
            Study, study_id, note, Group, time_window, n, mean, sd))
    if (nrow(single_arm_studies) > 0) {
        message("There are ", nrow(single_arm_studies), " studies with only one valid arm in this time window; they were excluded.")
        print(single_arm_studies, n = Inf)
    }
    arm_analysis <- arm_data %>% anti_join(single_arm_studies, by = "study_id")
    reported_smd_used <- prepare_reported_smd_data(reported_smd_data = reported_smd_data, outcome_name = outcome_name, 
        time_name = time_name)
    if (nrow(arm_analysis) > 0) {
        pw_arm <- meta::pairwise(studlab = study_id, treat = Group, n = n, mean = mean_analysis, sd = sd, 
            data = arm_analysis, sm = "SMD", append = TRUE)
        pairwise_arm <- as.data.frame(pw_arm) %>% mutate(source_type = "arm_level_mean_sd", effect_metric_original = "Cohen_d", 
            analysis_set_original = "main", scale = NA_character_, time_point = NA_character_, source_note = NA_character_)
    }
    else {
        pairwise_arm <- data.frame(studlab = character(), treat1 = character(), treat2 = character(), 
            TE = numeric(), seTE = numeric(), n1 = numeric(), n2 = numeric(), source_type = character(), 
            effect_metric_original = character(), analysis_set_original = character(), scale = character(), 
            time_point = character(), source_note = character(), stringsAsFactors = FALSE)
    }
    pairwise_data <- bind_rows(pairwise_arm, reported_smd_used) %>% mutate(studlab = as.character(studlab), 
        treat1 = as.character(treat1), treat2 = as.character(treat2), TE = suppressWarnings(as.numeric(TE)), 
        seTE = suppressWarnings(as.numeric(seTE))) %>% filter(!is.na(studlab), studlab != "", !is.na(treat1), 
        treat1 != "", !is.na(treat2), treat2 != "", is.finite(TE), is.finite(seTE), seTE > 0, treat1 != 
            treat2)
    if (nrow(pairwise_data) == 0) {
        stop("After combining arm-level Mean/SD data with directly reported SMD data, ", "no usable comparisons remain.")
    }
    mixed_source_trials <- pairwise_data %>% distinct(studlab, source_type) %>% count(studlab, name = "n_sources") %>% 
        filter(n_sources > 1)
    if (nrow(mixed_source_trials) > 0) {
        print(as.data.frame(mixed_source_trials), row.names = FALSE)
        stop("The studies shown above contain both Mean/SD data and directly reported SMD data.", "Retain only one data source per study to avoid double counting.")
    }
    duplicate_comparisons <- pairwise_data %>% mutate(comparison_a = pmin(treat1, treat2), comparison_b = pmax(treat1, 
        treat2)) %>% count(studlab, comparison_a, comparison_b, name = "n_rows") %>% filter(n_rows > 
        1)
    if (nrow(duplicate_comparisons) > 0) {
        print(as.data.frame(duplicate_comparisons), row.names = FALSE)
        stop("Duplicate study-comparison records were found; ", "check the SMD sheet for duplicate entries.")
    }
    if (n_distinct(pairwise_data$studlab) < 2) {
        stop("Fewer than two independent studies remain after combining SMD data.\n", "Outcome: ", outcome_name, 
            "\n", "Time window: ", time_name)
    }
    treatments <- sort(unique(c(pairwise_data$treat1, pairwise_data$treat2)))
    if (length(treatments) < 2) {
        stop("Fewer than two treatment nodes remain after combining SMD data.")
    }
    # A connected treatment network is required for a single standard NMA.
    connection <- netmeta::netconnection(treat1 = treat1, treat2 = treat2, studlab = studlab, data = pairwise_data)
    cat("\nNetwork connectivity check:\n")
    print(connection, details = TRUE)
    if (connection$n.subnets != 1) {
        if (!isTRUE(plot_disconnected_network)) {
            stop("The network contains ", connection$n.subnets, " disconnected components. ", "They cannot be combined in one standard NMA.")
        }
        network_only <- make_direct_evidence_network_plot(arm_analysis = arm_analysis, reported_smd_used = reported_smd_used, 
            pairwise_data = pairwise_data, file_prefix = file_prefix, plot_title = plot_title_clean, 
            node_order = node_order, font_family = plot_font_family)
        message("The network contains ", connection$n.subnets, " disconnected components, so a single standard NMA cannot be fitted; ", 
            "the direct-evidence network plot was still created: ", normalizePath(network_only$network_result$svg_file, 
                winslash = "/", mustWork = TRUE))
        return(invisible(list(analysis_status = "network_plot_only_disconnected", outcome = outcome_name, 
            time_window = time_name, n_subnets = connection$n.subnets, connection = connection, pairwise_effects = pairwise_data, 
            node_summary = network_only$node_summary, edge_summary = network_only$edge_summary, network_plot = network_only$network_result$svg_file)))
    }
    if (is.null(reference_group)) {
        possible_references <- c("Control", "Usual care", "Wait-list", "Waiting list", "Sham", "Placebo")
        available_reference <- intersect(possible_references, treatments)
        if (length(available_reference) > 0) {
            reference_group <- available_reference[1]
        }
        else {
            reference_group <- treatments[1]
        }
    }
    reference_group <- canonicalise_nma_node(reference_group)[1]
    node_order <- if (is.null(node_order)) 
        NULL
    else unique(canonicalise_nma_node(node_order))
    forest_order <- if (is.null(forest_order)) 
        NULL
    else unique(canonicalise_nma_node(forest_order))
    if (!(reference_group %in% treatments)) {
        stop("The specified reference_group is not present in the current network.\n", "Available nodes: ", paste(treatments, 
            collapse = ", "))
    }
    message("Reference group: ", reference_group)
    requested_network_order <- unique(c(NMA_NETWORK_NODE_ORDER, node_order, forest_order))
    unknown_order_nodes <- setdiff(requested_network_order, treatments)
    if (length(unknown_order_nodes) > 0) {
        message("The following prespecified nodes are absent from the network and were omitted from all outputs: ", paste(unknown_order_nodes, 
            collapse = ", "))
    }
    network_sequence <- c(requested_network_order[requested_network_order %in% treatments], setdiff(treatments, 
        requested_network_order))
    display_sequence <- network_sequence
    forest_sequence <- network_sequence
    # Fit the prespecified random-effects primary model and common-effect comparison model.
    nma_random <- netmeta::netmeta(TE = TE, seTE = seTE, treat1 = treat1, treat2 = treat2, studlab = studlab, 
        data = pairwise_data, sm = "SMD", common = FALSE, random = TRUE, prediction = TRUE, method.tau = "REML", 
        method.random.ci = "classic", reference.group = reference_group, small.values = small_values, 
        details.chkmultiarm = TRUE)
    nma_common <- netmeta::netmeta(TE = TE, seTE = seTE, treat1 = treat1, treat2 = treat2, studlab = studlab, 
        data = pairwise_data, sm = "SMD", common = TRUE, random = FALSE, prediction = FALSE, reference.group = reference_group, 
        small.values = small_values, details.chkmultiarm = TRUE)
    # Assess global and local inconsistency without hiding failures from the run log.
    global_inconsistency <- if (length(treatments) <= 2L) {
        message("Global inconsistency analysis skipped: the network has only two treatment nodes.")
        NULL
    }
    else {
        tryCatch(netmeta::decomp.design(nma_random), error = function(e) {
            message("The global inconsistency analysis could not be completed: ", conditionMessage(e))
            NULL
        })
    }
    local_inconsistency <- if (length(treatments) <= 2L) {
        message("Local inconsistency analysis skipped: direct and indirect evidence cannot be separated in a two-node network.")
        NULL
    }
    else {
        tryCatch(netmeta::netsplit(nma_random), error = function(e) {
            message("The local inconsistency analysis could not be completed: ", conditionMessage(e))
            NULL
        })
    }
    direct_results <- tryCatch(netmeta::netpairwise(nma_random), error = function(e) {
        message("The direct-comparison synthesis could not be completed: ", conditionMessage(e))
        NULL
    })
    global_consistency_table <- printout_to_table(object = global_inconsistency, outcome_name = outcome_name, 
        time_name = time_name, table_name = "Global inconsistency: design-by-treatment decomposition")
    local_consistency_table <- netsplit_to_dataframe(netsplit_object = local_inconsistency, outcome_name = outcome_name, 
        time_name = time_name, digits = 2)
    local_consistency_printout <- printout_to_table(object = local_inconsistency, outcome_name = outcome_name, 
        time_name = time_name, table_name = "Local inconsistency: netsplit direct versus indirect evidence")
    heterogeneity_summary <- make_heterogeneity_table(nma_object = nma_random, outcome_name = outcome_name, 
        time_name = time_name)
    nma_estimates_all <- extract_nma_estimates(nma_object = nma_random, outcome_name = outcome_name, 
        time_name = time_name, model = "random")
    nma_estimates_all_common <- extract_nma_estimates(nma_object = nma_common, outcome_name = outcome_name, 
        time_name = time_name, model = "common")
    nma_vs_reference <- make_reference_nma_table(nma_estimates_all = nma_estimates_all, reference_group = reference_group, 
        small_values = small_values, treatment_order = forest_sequence)
    nma_vs_reference_common <- make_reference_nma_table(nma_estimates_all = nma_estimates_all_common, 
        reference_group = reference_group, small_values = small_values, treatment_order = forest_sequence)
    random_vs_common_table <- make_random_vs_common_table(random_reference_table = nma_vs_reference, 
        common_reference_table = nma_vs_reference_common, digits = 2)
    direct_indirect_nma_results <- local_consistency_table
    pairwise_meta_summary <- make_pairwise_meta_summary(pairwise_data = pairwise_data, outcome_name = outcome_name, 
        time_name = time_name, digits = 2)
    pairwise_heterogeneity_table <- make_pairwise_heterogeneity_table(pairwise_meta_summary = pairwise_meta_summary, 
        outcome_name = outcome_name, time_name = time_name, digits = 3, i2_format = "percent")
    ranking <- netmeta::netrank(nma_random, small.values = small_values, method = "P-score")
    pscore_vector <- ranking$ranking.random
    pscore_table <- data.frame(treatment = names(pscore_vector), P_score = as.numeric(pscore_vector), 
        stringsAsFactors = FALSE)
    pscore_table <- pscore_table[pscore_table$treatment %in% network_sequence, , drop = FALSE]
    pscore_table <- pscore_table[order(-pscore_table$P_score), , drop = FALSE]
    rownames(pscore_table) <- NULL
    pscore_table$Rank <- seq_len(nrow(pscore_table))
    pscore_table$treatment <- display_nma_node(pscore_table$treatment)
    names(pscore_table)[1:2] <- c("Treatment node", "P-score")
    node_data <- bind_rows(arm_analysis %>% transmute(study_id = as.character(study_id), treatment = as.character(Group), 
        n = as.numeric(n), source_type = "arm_level_mean_sd"), reported_smd_used %>% transmute(study_id = as.character(studlab), 
        treatment = as.character(treat1), n = as.numeric(n1), source_type = "direct_reported_SMD"), reported_smd_used %>% 
        transmute(study_id = as.character(studlab), treatment = as.character(treat2), n = as.numeric(n2), 
            source_type = "direct_reported_SMD")) %>% filter(!is.na(study_id), study_id != "", !is.na(treatment), 
        treatment != "")
    node_n_conflict <- node_data %>% filter(is.finite(n)) %>% group_by(study_id, treatment) %>% summarise(n_values = n_distinct(n), 
        values = paste(sort(unique(n)), collapse = " | "), .groups = "drop") %>% filter(n_values > 1)
    if (nrow(node_n_conflict) > 0) {
        print(as.data.frame(node_n_conflict), row.names = FALSE)
        stop("Different sample sizes were found for the same treatment arm within a study.")
    }
    node_data <- node_data %>% arrange(study_id, treatment, desc(is.finite(n))) %>% distinct(study_id, 
        treatment, .keep_all = TRUE)
    if (nrow(reported_smd_used) > 0 && any(!is.finite(reported_smd_used$n1) | !is.finite(reported_smd_used$n2))) {
        warning("Some directly reported SMD records are missing n1/n2 and can enter the NMA, ", "but network node participant counts will be underestimated.")
    }
    node_summary <- node_data %>% group_by(treatment) %>% summarise(n_participants = sum(n, na.rm = TRUE), 
        n_studies = n_distinct(study_id), n_arms = n(), .groups = "drop")
    edge_summary <- pairwise_data %>% transmute(study_id = as.character(studlab), from = pmin(as.character(treat1), 
        as.character(treat2)), to = pmax(as.character(treat1), as.character(treat2))) %>% distinct(study_id, 
        from, to) %>% group_by(from, to) %>% summarise(n_studies = n_distinct(study_id), .groups = "drop")
    random_model_printout <- printout_to_table(nma_random, outcome_name, time_name, "Random-effects NMA model")
    common_model_printout <- printout_to_table(nma_common, outcome_name, time_name, "Common-effect NMA model")
    direct_results_printout <- printout_to_table(direct_results, outcome_name, time_name, "Direct pairwise meta-analysis")
    output_sheets <- list(arm_data_used = arm_analysis %>% select(excel_row, Study, study_id, note, Group, 
        time_window, n, mean_original, sd, outcome_direction, mean_analysis), reported_SMD_used = reported_smd_used, 
        pairwise_effects = pairwise_data, pairwise_meta_summary = pairwise_meta_summary, pairwise_heterogeneity_table = pairwise_heterogeneity_table, 
        node_summary = node_summary, edge_summary = edge_summary, heterogeneity_summary = heterogeneity_summary, 
        NMA_vs_reference_random = nma_vs_reference, NMA_vs_reference_common = nma_vs_reference_common, 
        random_vs_common = random_vs_common_table, NMA_all_pairwise_random = nma_estimates_all, NMA_all_pairwise_common = nma_estimates_all_common, 
        direct_indirect_NMA_results = direct_indirect_nma_results, global_consistency_table = global_consistency_table, 
        local_inconsistency_SIDE = local_consistency_table, local_consistency_printout = local_consistency_printout, 
        direct_results_printout = direct_results_printout, random_model_printout = random_model_printout, 
        common_model_printout = common_model_printout, all_arms_in_time_window = diagnostic_sheets$all_arms_in_time_window, 
        no_time_data = diagnostic_sheets$no_time_data, valid_arms_pre_exclusion = diagnostic_sheets$valid_arm_data_before_single_arm_exclusion, 
        arm_counts = diagnostic_sheets$arm_counts, single_arm_studies = diagnostic_sheets$single_arm_studies, 
        single_arm_details = diagnostic_sheets$single_arm_details)
    league_result <- NULL
    network_result <- NULL
    forest_result <- NULL
    plot_sequence <- network_sequence
    league_result <- make_styled_league_table(nma_object = nma_random, sequence = plot_sequence, digits = 2, 
        backtransf = FALSE, ci_separator = " to ")
    if (is.null(league_result) || is.null(league_result$league_matrix)) {
        stop("The league-table matrix was not generated; check netleague.")
    }
    network_display_nodes <- unname(display_nma_node(network_sequence))
    league_display_nodes <- unname(as.character(diag(league_result$league_matrix)))
    pscore_display_nodes <- as.character(pscore_table[["Treatment node"]])
    forest_display_nodes <- display_nma_node(as.character(nma_vs_reference$treatment))
    if (!identical(league_display_nodes, network_display_nodes)) {
        stop("League-table nodes or ordering do not match the network plot.")
    }
    if (!setequal(pscore_display_nodes, network_display_nodes)) {
        stop("P-score nodes do not match the network plot.")
    }
    if (!setequal(forest_display_nodes, setdiff(network_display_nodes, display_nma_node(reference_group)))) {
        stop("Forest-plot nodes do not match the non-reference nodes in the network plot.")
    }
    message("Node alignment passed: network, forest, league and P-score outputs agree.")
    network_result <- make_bmj_style_network_plot(node_summary = node_summary, edge_summary = edge_summary, 
        sequence = network_sequence, file_prefix = file_prefix, plot_title = plot_title_clean, bottom_node = NULL, 
        bg_color = "white", node_color = bmj_blue, edge_color = bmj_blue, edge_alpha = 1, layout_radius = 0.79, 
        node_radius_range = c(0.014, 0.115), edge_width_range = c(1.2, 5.25), edge_width_step = 0.9, 
        label_offset = 0.09, label_wrap_width = 16, show_node_sample_size = FALSE, title_cex = NMA_NETWORK_TITLE_CEX, 
        title_y_ndc = 0.925, label_cex = NMA_NETWORK_LABEL_CEX, font_family = plot_font_family)
    if (!file.exists(network_result$svg_file)) {
        stop("Network SVG was not created: ", network_result$svg_file)
    }
    message("Network plot saved: ", normalizePath(network_result$svg_file))
    forest_result <- make_bmj_style_forest_plot(nma_vs_reference = nma_vs_reference, node_summary = node_summary, 
        reference_group = reference_group, file_prefix = file_prefix, plot_title = plot_title_clean, 
        treatment_order = forest_sequence, effect_label = paste0("Standardised mean\n", "differences (95% CI)"), 
        null_value = 0, xlog = FALSE, forest_xlim = NULL, forest_ticks = NULL, show_truncation_arrows = FALSE, 
        digits = 2, favours_left = if (higher_scores_are_better) paste("Favours", reference_group) else "Favours treatment", 
        favours_right = if (higher_scores_are_better) "Favours treatment" else paste("Favours", reference_group), 
        bmj_blue = bmj_blue, bmj_blue_light = bmj_blue_light, bg_color = "white", font_family = plot_font_family, 
        point_shape = 23, add_row_separators = FALSE, row_sep_col = "#E3E3E6", row_sep_lwd = 0.6, row_height_in = NMA_FOREST_ROW_HEIGHT_IN, 
        header_height_in = NMA_FOREST_HEADER_HEIGHT_IN, header_data_gap_in = NMA_FOREST_HEADER_DATA_GAP_IN, 
        add_forest_box = TRUE, left_header = "Treatment node", count_header = paste0("No of studies/\n", 
            "No of patients"), outer_border = "#9AA0A6", show_plot_title = FALSE)
    if (!file.exists(forest_result$svg_file)) {
        stop("Forest-plot SVG was not created: ", forest_result$svg_file)
    }
    message("Forest plot saved: ", normalizePath(forest_result$svg_file))
    # Record model settings with each exported result workbook.
    analysis_settings <- data.frame(item = c("Outcome", "Time window", "Reference group", "Independent studies analysed", 
        "Treatment nodes", "Usable arm-level arms", "Direct SMD comparisons", "Arms without data in this window", 
        "Single-arm studies excluded", "Effect measure", "Primary model", "Heterogeneity estimator", 
        "Direction convention", "Figure colour palette", "League table lower triangle", "League table upper triangle", 
        "League table bold text", "meta version", "netmeta version", "ggsci version", "R version"), value = c(outcome_display_name, 
        time_name, reference_group, n_distinct(pairwise_data$studlab), length(treatments), nrow(arm_analysis), 
        nrow(reported_smd_used), nrow(no_time_data), nrow(single_arm_studies), "SMD (Cohen's d; arm-level and direct SMD combined)", 
        "Frequentist random-effects NMA", "REML", direction_convention, 
        "ggsci BMJ Purple (#7D5CC6)", "Random-effects network estimates", "Direct estimates", "95% CI does not include 0", 
        as.character(packageVersion("meta")), as.character(packageVersion("netmeta")), as.character(packageVersion("ggsci")), 
        R.version.string), stringsAsFactors = FALSE)
    output_sheets <- c(list(README = analysis_settings), output_sheets)
    final_excel_file <- paste0(file_prefix, "_results.xlsx")
    league_excel_file <- paste0(file_prefix, "_league.xlsx")
    pscore_excel_file <- paste0(file_prefix, "_pscore.xlsx")
    write_master_workbook(output_file = final_excel_file, sheets = output_sheets, league_matrix = NULL, 
        font_family = plot_font_family)
    if (!file.exists(final_excel_file)) {
        stop("Final Excel workbook was not created: ", final_excel_file)
    }
    message("All result tables saved in one Excel workbook: ", normalizePath(final_excel_file))
    write_league_workbook(output_file = league_excel_file, league_matrix = league_result$league_matrix, 
        font_family = plot_font_family)
    if (!file.exists(league_excel_file)) {
        stop("Standalone league-table workbook was not created: ", league_excel_file)
    }
    message("League table saved as a separate Excel workbook: ", normalizePath(league_excel_file))
    write_pscore_workbook(output_file = pscore_excel_file, pscore_table = pscore_table, font_family = plot_font_family)
    if (!file.exists(pscore_excel_file)) {
        stop("Standalone P-score workbook was not created: ", pscore_excel_file)
    }
    message("P-score table saved as a separate Excel workbook: ", normalizePath(pscore_excel_file))
    result_object <- list(arm_data = arm_analysis, reported_smd_used = reported_smd_used, excluded_no_time_data = no_time_data, 
        excluded_single_arm = single_arm_details, pairwise = pairwise_data, random_model = nma_random, 
        common_model = nma_common, connectivity = connection, global_inconsistency = global_inconsistency, 
        local_inconsistency = local_inconsistency, direct_results = direct_results, heterogeneity_summary = heterogeneity_summary, 
        nma_vs_reference = nma_vs_reference, nma_vs_reference_common = nma_vs_reference_common, nma_all_pairwise_estimates = nma_estimates_all, 
        nma_all_pairwise_estimates_common = nma_estimates_all_common, random_vs_common = random_vs_common_table, 
        pairwise_meta_summary = pairwise_meta_summary, pairwise_heterogeneity_table = pairwise_heterogeneity_table, 
        direct_indirect_nma_results = direct_indirect_nma_results, global_consistency_table = global_consistency_table, 
        local_consistency_table = local_consistency_table, local_consistency_printout = local_consistency_printout, 
        ranking = ranking, node_summary = node_summary, edge_summary = edge_summary, league_table = if (!is.null(league_result)) {
            league_result$league_matrix
        } else {
            NULL
        }, league_table_excel_file = league_excel_file, league_table_excel_sheet = "league_table", pscore_table = pscore_table, 
        pscore_table_excel_file = pscore_excel_file, network_plot_svg = if (!is.null(network_result)) {
            network_result$svg_file
        } else {
            NA_character_
        }, forest_plot_data = if (!is.null(forest_result)) {
            forest_result$plot_data
        } else {
            NULL
        }, forest_plot_svg = if (!is.null(forest_result)) {
            forest_result$svg_file
        } else {
            NA_character_
        }, excel_file = final_excel_file)
    cat("\n========================================\n")
    cat("Frequentist network meta-analysis completed\n")
    cat("========================================\n")
    cat("Outcome: ", outcome_display_name, "\n")
    cat("Time window: ", time_name, "\n")
    cat("Reference group: ", reference_group, "\n")
    cat("Independent studies: ", n_distinct(pairwise_data$studlab), "\n")
    cat("Treatment nodes: ", length(treatments), "\n")
    cat("Directly reported SMD comparisons: ", nrow(reported_smd_used), "\n")
    cat("Pairwise comparisons: ", nrow(pairwise_data), "\n")
    cat("Excel workbook: ", normalizePath(final_excel_file), "\n")
    cat("Network plot: ", normalizePath(network_result$svg_file), "\n")
    cat("Forest plot: ", normalizePath(forest_result$svg_file), "\n")
    cat("League table: ", normalizePath(league_excel_file), " (sheet: league_table)\n")
    cat("P-score table: ", normalizePath(pscore_excel_file), "\n")
    cat("========================================\n")
    invisible(result_object)
}

SUBGROUP_WORKBOOK_NAME <- "subgroup_analysis.xlsx"

SUBGROUP_WORKBOOK_CANDIDATES <- c(SUBGROUP_WORKBOOK_NAME, "50c43f05-8659-4a9c-b006-4c9d7a66a2b8.xlsx")

SUBGROUP_EXCLUDED_STUDIES <- c("10", "20", "70", "73")

SUBGROUP_SHEET_ALIASES <- list(`Physical function` = c("Physical function", "Disability"), `Pain-related fear avoidance` = c("Pain-related fear avoidance", 
    "Pain fear avoidance", "Painfearavoindance"), `Pain intensity` = c("Pain intensity"), `Health-related quality of life` = c("Health-related quality of life", 
    "Quality of Life", "QoL"))

resolve_subgroup_sheet <- function(workbook, outcome_name, arm_sheet = NULL) {
    available_sheets <- readxl::excel_sheets(workbook)
    normalise_sheet_name <- function(x) {
        gsub("[^a-z0-9]+", "", tolower(trimws(as.character(x))))
    }
    outcome_display <- make_display_outcome_name(outcome_name)
    candidate_sheets <- if (!is.null(arm_sheet) && length(arm_sheet) == 1L && !is.na(arm_sheet) && nzchar(trimws(arm_sheet))) {
        trimws(arm_sheet)
    }
    else {
        unique(c(outcome_display, SUBGROUP_SHEET_ALIASES[[outcome_display]]))
    }
    available_keys <- normalise_sheet_name(available_sheets)
    for (candidate in candidate_sheets) {
        hit <- which(available_keys == normalise_sheet_name(candidate))
        if (length(hit) > 0L) {
            return(available_sheets[hit[1]])
        }
    }
    stop("The outcome sheet could not be found in the subgroup-analysis workbook: ", outcome_display, "\nTried sheet names: ", paste(candidate_sheets, 
        collapse = ", "), "\nAvailable sheets: ", paste(available_sheets, collapse = ", "))
}

find_subgroup_workbook <- function(working_dir, subgroup_workbook = NULL) {
    working_dir <- normalizePath(working_dir, winslash = "/", mustWork = TRUE)
    if (!is.null(subgroup_workbook) && length(subgroup_workbook) == 1 && !is.na(subgroup_workbook) && 
        nzchar(trimws(subgroup_workbook))) {
        requested_file <- trimws(as.character(subgroup_workbook))
        is_absolute_path <- grepl("^[A-Za-z]:[/\\\\]|^[/\\\\]{2}|^/", requested_file)
        requested_file <- if (is_absolute_path) {
            requested_file
        }
        else {
            file.path(working_dir, requested_file)
        }
        if (file.exists(requested_file)) {
            return(normalizePath(requested_file, winslash = "/", mustWork = TRUE))
        }
    }
    exact_candidates <- file.path(working_dir, SUBGROUP_WORKBOOK_CANDIDATES)
    exact_candidates <- exact_candidates[file.exists(exact_candidates)]
    if (length(exact_candidates) >= 1) {
        return(normalizePath(exact_candidates[1], winslash = "/", mustWork = TRUE))
    }
    xlsx_candidates <- list.files(working_dir, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
    xlsx_candidates <- xlsx_candidates[!grepl("^~\\$", basename(xlsx_candidates)) & !(tolower(basename(xlsx_candidates)) %in% 
        "smd_data.xlsx")]
    if (length(xlsx_candidates) == 1) {
        message("The default subgroup-analysis workbook was not found; ", "using the only non-SMD workbook in the directory: ", 
            basename(xlsx_candidates))
        return(normalizePath(xlsx_candidates, winslash = "/", mustWork = TRUE))
    }
    if (length(xlsx_candidates) > 1) {
        stop("Multiple possible subgroup-analysis workbooks were found: ", paste(basename(xlsx_candidates), collapse = ", "), 
            "\nSpecify this argument when running the function: ", "subgroup_workbook = \"your_file.xlsx\"")
    }
    stop("The subgroup-analysis workbook was not found.\n", "Place the subgroup-analysis workbook in: ", working_dir)
}

filter_reported_smd_for_subgroup <- function(reported_smd_data, excluded_studies = SUBGROUP_EXCLUDED_STUDIES) {
    if (is.null(reported_smd_data) || nrow(reported_smd_data) == 0 || is.null(excluded_studies) || length(excluded_studies) == 
        0) {
        return(reported_smd_data)
    }
    dat <- as.data.frame(reported_smd_data, stringsAsFactors = FALSE)
    normalised_names <- trimws(names(dat))
    normalised_names <- gsub("[^A-Za-z0-9]+", "_", normalised_names)
    normalised_names <- gsub("^_|_$", "", normalised_names)
    normalised_names <- tolower(normalised_names)
    study_aliases <- c("studlab", "study", "study_id", "trial_id")
    study_hit <- which(normalised_names %in% study_aliases)
    if (length(study_hit) == 0) {
        stop("No study identifier column was found in the SMD workbook. Use studlab, study, study_id, or trial_id as the column name.")
    }
    study_column <- study_hit[1]
    study_id <- trimws(as.character(dat[[study_column]]))
    excluded_studies <- trimws(as.character(excluded_studies))
    remove_row <- !is.na(study_id) & study_id %in% excluded_studies
    if (any(remove_row)) {
        message("Studies excluded from the directly reported SMD data: ", paste(sort(unique(study_id[remove_row])), 
            collapse = ", "), " (", sum(remove_row), " rows)")
    }
    dat[!remove_row, , drop = FALSE]
}

NMA_MASTER_WORKBOOK_CANDIDATES <- "study_information.xlsx"

NMA_SEVEN_OUTCOMES <- c("Pain intensity", "Physical function", "Pain-related fear avoidance", "Health-related quality of life", 
    "Anxiety", "Depression", "Self-efficacy")

NMA_MASTER_SHEET_ALIASES <- list(`Pain intensity` = c("pain intensity", "Pain intensity"), `Physical function` = c("disability", 
    "Disability", "Physical function"), `Pain-related fear avoidance` = c("pain-related fear avoidance", 
    "Pain-related fear avoidance", "Pain fear avoidance", "Painfearavoindance"), `Health-related quality of life` = c("health-related quality of life", 
    "Health-related quality of life", "quality of life", "Quality of Life", "QoL"), Anxiety = c("Anxiety", 
    "anxiety"), Depression = c("Depression", "depression", "Dpression"), `Self-efficacy` = c("Self efficacy", 
    "Self-efficacy", "self efficacy"))

NMA_ALLOWED_PAIN_TYPES <- c("CLBP", "CNSLBP", "CDLBP")

read_target_pain_type_studies <- function(workbook) {
    sheets <- readxl::excel_sheets(workbook)
    sheet_keys <- normalise_nma_key(sheets)
    hit <- which(sheet_keys == "basicinformation")
    if (length(hit) == 0L) 
        stop("The Basic information sheet was not found in the study-information workbook.")
    dat <- as.data.frame(readxl::read_excel(workbook, sheet = sheets[hit[1]]), stringsAsFactors = FALSE)
    keys <- normalise_nma_key(names(dat))
    pain_hit <- which(keys == "paintype")
    if (length(pain_hit) == 0L) 
        stop("The Pain type column was not found in the Basic information sheet.")
    ids <- normalise_study_id(dat[[1]])
    pain_type <- toupper(stringr::str_squish(as.character(dat[[pain_hit[1]]])))
    allowed <- unique(ids[!is.na(ids) & nzchar(ids) & pain_type %in% NMA_ALLOWED_PAIN_TYPES])
    message("Pain-type subgroup filter: included ", length(allowed), " studies.")
    allowed
}

# Run one outcome/time window for the prespecified pain-type subgroup.
run_subgroup_outcome_time <- function(outcome_name = NMA_SEVEN_OUTCOMES, time_name = c("post_intervention", "short_term", 
    "mid_term", "long_term"), working_dir = locate_nma_working_dir(), master_workbook = NULL, arm_sheet = NULL, 
    reported_smd_sheet = "SMD", output_dir = NULL) {
    outcome_name <- canonicalise_requested_outcome(outcome_name[1])
    time_name <- match.arg(time_name)
    working_dir <- normalizePath(working_dir, winslash = "/", mustWork = TRUE)
    master_workbook <- find_master_workbook(working_dir, master_workbook)
    arm_sheet <- resolve_master_outcome_sheet(master_workbook, outcome_name, arm_sheet)
    allowed <- read_target_pain_type_studies(master_workbook)
    arm_ids <- read_arm_study_ids(master_workbook, arm_sheet)
    excluded <- setdiff(arm_ids, allowed)
    reported_smd_data <- read_master_smd(master_workbook, reported_smd_sheet)
    reported_smd_data <- filter_reported_smd_ids(reported_smd_data, include_ids = allowed, label = "SMD filter for the CLBP/CNSLBP/CDLBP subgroup")
    if (is.null(output_dir)) {
        output_dir <- file.path(working_dir, "subgroup", analysis_file_stub(outcome_name, time_name))
    }
    message("Subgroup analysis: ", arm_sheet, "; included Pain type = CLBP/CNSLBP/CDLBP only, with the same study set applied to SMD data.")
    run_frequentist_nma(arm_file = master_workbook, arm_sheet = arm_sheet, outcome_name = if (identical(outcome_name, 
        "Physical function")) 
        "Disability"
    else outcome_name, time_name = time_name, output_dir = output_dir, exclude_studies = excluded, reported_smd_data = reported_smd_data, 
        reference_group = "Control", node_order = NMA_NETWORK_NODE_ORDER, forest_order = NMA_NETWORK_NODE_ORDER, 
        small_values = small_values_for_outcome(outcome_name))
}

# Run one outcome at all four prespecified follow-up windows.
run_subgroup_outcome <- function(outcome_name = NMA_SEVEN_OUTCOMES, working_dir = locate_nma_working_dir(), 
    master_workbook = NULL, arm_sheet = NULL, reported_smd_sheet = "SMD", output_root = file.path(working_dir, 
        "subgroup"), continue_on_error = TRUE) {
    outcome_name <- canonicalise_requested_outcome(outcome_name[1])
    run_four_time_windows(outcome_name, working_dir, master_workbook, arm_sheet, output_root, continue_on_error, 
        run_one = function(current_time, current_output_dir) {
            run_subgroup_outcome_time(outcome_name = outcome_name, time_name = current_time, working_dir = working_dir, 
                master_workbook = master_workbook, arm_sheet = arm_sheet, reported_smd_sheet = reported_smd_sheet, 
                output_dir = current_output_dir)
        })
}

# Run all seven outcomes for the prespecified pain-type subgroup analysis.
run_subgroup_analysis <- function(working_dir = locate_nma_working_dir(), master_workbook = NULL, reported_smd_sheet = "SMD", 
    output_root = file.path(working_dir, "subgroup"), continue_on_error = TRUE) {
    out <- setNames(vector("list", length(NMA_SEVEN_OUTCOMES)), NMA_SEVEN_OUTCOMES)
    for (oo in NMA_SEVEN_OUTCOMES) {
        out[[oo]] <- run_subgroup_outcome(outcome_name = oo, working_dir = working_dir, master_workbook = master_workbook, 
            reported_smd_sheet = reported_smd_sheet, output_root = output_root, continue_on_error = continue_on_error)
    }
    invisible(out)
}

