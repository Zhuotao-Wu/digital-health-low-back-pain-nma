# Shared core functions for the four NMA workflows.
# This file is sourced by the main, sensitivity, and subgroup entry scripts.
# Statistical model choices remain in the scenario-specific entry scripts.

# -----------------------------------------------------------------------------
# Directly reported standardised mean differences
# -----------------------------------------------------------------------------
# Validate and harmonise comparison-level SMD records. Directly reported
# Hedges' g values are converted to Cohen's d so that they use the same effect
# metric as estimates derived from arm-level means and standard deviations.

prepare_reported_smd_data_core <- function(reported_smd_data, outcome_name, time_name) {
    empty <- data.frame(smd_excel_row = integer(), studlab = character(), treat1 = character(), treat2 = character(), 
        TE = numeric(), seTE = numeric(), lower_95_CI = numeric(), upper_95_CI = numeric(), n1 = numeric(), 
        n2 = numeric(), source_type = character(), effect_metric_original = character(), analysis_set_original = character(), 
        scale = character(), time_point = character(), source_note = character(), stringsAsFactors = FALSE)
    if (is.null(reported_smd_data) || nrow(reported_smd_data) == 0) {
        return(empty)
    }
    dat <- as.data.frame(reported_smd_data, stringsAsFactors = FALSE)
    normalised_names <- trimws(names(dat))
    normalised_names <- gsub("[^A-Za-z0-9]+", "_", normalised_names)
    normalised_names <- gsub("^_|_$", "", normalised_names)
    names(dat) <- tolower(normalised_names)
    dat$smd_excel_row <- seq_len(nrow(dat)) + 1L
    alias_to_standard <- function(data, aliases, standard) {
        if (!(standard %in% names(data))) {
            hit <- aliases[aliases %in% names(data)]
            if (length(hit) > 0) {
                names(data)[names(data) == hit[1]] <- standard
            }
        }
        data
    }
    dat <- alias_to_standard(dat, c("study", "study_id", "trial_id"), "studlab")
    dat <- alias_to_standard(dat, c("group1", "treatment1", "intervention1"), "treat1")
    dat <- alias_to_standard(dat, c("group2", "treatment2", "intervention2"), "treat2")
    dat <- alias_to_standard(dat, c("smd", "effect", "estimate"), "te")
    dat <- alias_to_standard(dat, c("se", "standard_error", "se_smd"), "sete")
    dat <- alias_to_standard(dat, c("lower", "lower_ci", "lower_95"), "lower_95_ci")
    dat <- alias_to_standard(dat, c("upper", "upper_ci", "upper_95"), "upper_95_ci")
    dat <- alias_to_standard(dat, c("metric", "smd_type", "reported_metric"), "effect_metric")
    dat <- alias_to_standard(dat, c("note", "source", "comments"), "source_note")
    if ("outcome" %in% names(dat)) {
        dat <- dat[trimws(tolower(as.character(dat$outcome))) == trimws(tolower(outcome_name)), , drop = FALSE]
    }
    if ("time_window" %in% names(dat)) {
        dat <- dat[trimws(tolower(as.character(dat$time_window))) == trimws(tolower(time_name)), , drop = FALSE]
    }
    if (nrow(dat) == 0) {
        return(empty)
    }
    required <- c("studlab", "treat1", "treat2", "te")
    missing_required <- setdiff(required, names(dat))
    if (length(missing_required) > 0) {
        stop("The SMD sheet is missing required columns: ", paste(missing_required, collapse = ", "), ". At minimum, studlab, treat1, treat2, and TE are required, ", 
            "together with either seTE or a 95% CI.")
    }
    if (!("sete" %in% names(dat))) {
        dat$sete <- NA_real_
    }
    if (!("lower_95_ci" %in% names(dat))) {
        dat$lower_95_ci <- NA_real_
    }
    if (!("upper_95_ci" %in% names(dat))) {
        dat$upper_95_ci <- NA_real_
    }
    if (!("n1" %in% names(dat))) {
        dat$n1 <- NA_real_
    }
    if (!("n2" %in% names(dat))) {
        dat$n2 <- NA_real_
    }
    effect_metric_column_missing <- !("effect_metric" %in% names(dat))
    if (effect_metric_column_missing) {
        dat$effect_metric <- "Cohen_d"
    }
    if (!("reverse_sign" %in% names(dat))) {
        dat$reverse_sign <- FALSE
    }
    if (!("analysis_set" %in% names(dat))) {
        dat$analysis_set <- "main"
    }
    if (!("scale" %in% names(dat))) {
        dat$scale <- NA_character_
    }
    if (!("time_point" %in% names(dat))) {
        dat$time_point <- NA_character_
    }
    if (!("source_note" %in% names(dat))) {
        dat$source_note <- NA_character_
    }
    n_before_blank_row_removal <- nrow(dat)
    dat <- dat %>% filter(!((is.na(studlab) | trimws(as.character(studlab)) == "") & (is.na(treat1) | 
        trimws(as.character(treat1)) == "") & (is.na(treat2) | trimws(as.character(treat2)) == "") & 
        (is.na(te) | trimws(as.character(te)) == "")))
    n_blank_rows_removed <- n_before_blank_row_removal - nrow(dat)
    if (n_blank_rows_removed > 0) {
        message("Ignored ", n_blank_rows_removed, " blank Excel placeholder rows with no core SMD values.")
    }
    if (nrow(dat) == 0) {
        return(empty)
    }
    if (effect_metric_column_missing) {
        message("The SMD sheet has no effect_metric column; ", "direct SMD values were treated as Cohen_d under the prespecified analysis convention.")
    }
    dat <- dat %>% mutate(smd_excel_row = as.integer(smd_excel_row), studlab = str_squish(as.character(studlab)), 
        treat1 = canonicalise_nma_node(treat1), treat2 = canonicalise_nma_node(treat2), te = suppressWarnings(as.numeric(te)), 
        sete = suppressWarnings(as.numeric(sete)), lower_95_ci = suppressWarnings(as.numeric(lower_95_ci)), 
        upper_95_ci = suppressWarnings(as.numeric(upper_95_ci)), n1 = suppressWarnings(as.numeric(n1)), 
        n2 = suppressWarnings(as.numeric(n2)), effect_metric_original = str_squish(as.character(effect_metric)), 
        effect_metric_key = gsub("[^a-z0-9]+", "", tolower(effect_metric_original)), reverse_sign = case_when(is.logical(reverse_sign) ~ 
            reverse_sign, tolower(as.character(reverse_sign)) %in% c("true", "t", "1", "yes", "y") ~ 
            TRUE, TRUE ~ FALSE), analysis_set_original = as.character(analysis_set), scale = as.character(scale), 
        time_point = as.character(time_point), source_note = as.character(source_note))
    if (effect_metric_column_missing) {
        dat$effect_metric_original <- "Cohen_d (default: effect_metric column absent)"
    }
    invalid_identity <- dat %>% filter(is.na(studlab) | studlab == "" | is.na(treat1) | treat1 == "" | 
        is.na(treat2) | treat2 == "" | treat1 == treat2)
    if (nrow(invalid_identity) > 0) {
        invalid_identity_to_print <- invalid_identity %>% select(any_of(c("smd_excel_row", "studlab", 
            "treat1", "treat2", "te", "outcome", "time_window", "source_note")))
        print(as.data.frame(invalid_identity_to_print), row.names = FALSE)
        stop("The SMD data contain missing study or treatment identifiers, ", "or treat1 is identical to treat2. ", "Check the SMD sheet using the smd_excel_row values shown above.")
    }
    dat <- dat %>% mutate(sete = if_else(is.finite(sete) & sete > 0, sete, (upper_95_ci - lower_95_ci)/(2 * 
        1.96)))
    invalid_precision <- dat %>% filter(!is.finite(te) | !is.finite(sete) | sete <= 0)
    if (nrow(invalid_precision) > 0) {
        print(as.data.frame(invalid_precision), row.names = FALSE)
        stop("The SMD data do not contain valid TE/seTE values, ", "and seTE cannot be recovered from the 95% CI.")
    }
    dat <- dat %>% mutate(te = if_else(reverse_sign, -te, te))
    cohen_keys <- c("cohend", "cohensd", "d", "cohen", "smd", "standardizedmeandifference", "standardisedmeandifference")
    hedges_keys <- c("hedgesg", "hedgeg", "g")
    missing_metric <- is.na(dat$effect_metric_key) | dat$effect_metric_key == ""
    if (any(missing_metric)) {
        dat$effect_metric_original[missing_metric] <- "Cohen_d (default: effect_metric blank)"
        dat$effect_metric_key[missing_metric] <- "cohend"
        message("The SMD data contain ", sum(missing_metric), " rows with a blank effect_metric; ", "they were treated as Cohen_d.")
    }
    unclear_metric <- dat %>% filter(!(effect_metric_key %in% c(cohen_keys, hedges_keys)))
    if (nrow(unclear_metric) > 0) {
        print(as.data.frame(unclear_metric), row.names = FALSE)
        stop("The effect_metric values in the SMD rows shown above are not recognised.", "Use Cohen_d, Cohen's d, SMD, or Hedges_g; ", 
            "check the SMD sheet using smd_excel_row.")
    }
    hedges_rows <- dat$effect_metric_key %in% hedges_keys
    if (any(hedges_rows)) {
        missing_n <- hedges_rows & (!is.finite(dat$n1) | !is.finite(dat$n2) | dat$n1 < 2 | dat$n2 < 2)
        if (any(missing_n)) {
            print(dat[missing_n, , drop = FALSE], n = Inf)
            stop("Converting Hedges_g to Cohen_d requires n1 and n2.")
        }
        df <- dat$n1[hedges_rows] + dat$n2[hedges_rows] - 2
        J <- 1 - 3/(4 * df - 1)
        dat$te[hedges_rows] <- dat$te[hedges_rows]/J
        dat$sete[hedges_rows] <- dat$sete[hedges_rows]/J
    }
    dat %>% transmute(smd_excel_row = smd_excel_row, studlab = studlab, treat1 = treat1, treat2 = treat2, 
        TE = te, seTE = sete, lower_95_CI = TE - 1.96 * seTE, upper_95_CI = TE + 1.96 * seTE, n1 = n1, 
        n2 = n2, source_type = "direct_reported_SMD", effect_metric_original = effect_metric_original, 
        analysis_set_original = analysis_set_original, scale = scale, time_point = time_point, source_note = source_note)
}

# -----------------------------------------------------------------------------
# Project paths and general formatting helpers
# -----------------------------------------------------------------------------
detect_nma_script_dir <- function() {
    source_files <- unlist(lapply(sys.frames(), function(frame) {
        if (is.null(frame$ofile)) {
            character()
        }
        else {
            as.character(frame$ofile[1])
        }
    }), use.names = FALSE)
    source_files <- source_files[!is.na(source_files) & nzchar(source_files)]
    if (length(source_files) > 0) {
        source_file <- tail(source_files, 1)
        if (file.exists(source_file)) {
            return(dirname(normalizePath(source_file, winslash = "/", mustWork = TRUE)))
        }
    }
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

make_safe_sheet_names <- function(x) {
    x <- as.character(x)
    x <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", x)
    x[x == "" | is.na(x)] <- "Sheet"
    out <- character(length(x))
    for (i in seq_along(x)) {
        base <- substr(x[i], 1, 31)
        candidate <- base
        suffix_id <- 1L
        while (candidate %in% out[seq_len(i - 1L)]) {
            suffix <- paste0("_", suffix_id)
            candidate <- paste0(substr(base, 1, 31 - nchar(suffix)), suffix)
            suffix_id <- suffix_id + 1L
        }
        out[i] <- candidate
    }
    out
}

make_ci <- function(est, lower, upper, digits = 2) {
    ifelse(is.na(est) | is.na(lower) | is.na(upper), NA_character_, paste0(sprintf(paste0("%.", digits, 
        "f"), est), " [", sprintf(paste0("%.", digits, "f"), lower), "; ", sprintf(paste0("%.", digits, 
        "f"), upper), "]"))
}

get_scalar <- function(x, candidates) {
    for (nm in candidates) {
        value <- tryCatch(x[[nm]], error = function(e) NULL)
        if (!is.null(value) && length(value) >= 1) {
            value <- suppressWarnings(as.numeric(value[1]))
            if (is.finite(value)) {
                return(value)
            }
        }
    }
    NA_real_
}

# -----------------------------------------------------------------------------
# Portable font selection and outlined SVG generation
# -----------------------------------------------------------------------------
# InterFace is preferred for continuity with the manuscript figures. If it is
# absent, a system sans-serif font is selected and the analysis continues.
NMA_SHOWTEXT_ACTIVE <- FALSE

find_nma_windows_user_font_file <- function(font_basename) {
    local_appdata <- Sys.getenv("LOCALAPPDATA", unset = "")
    candidate_dirs <- unique(c(if (nzchar(local_appdata)) {
        file.path(local_appdata, "Microsoft", "Windows", "Fonts")
    } else {
        character()
    }, file.path(path.expand("~"), "AppData", "Local", "Microsoft", "Windows", "Fonts"), if (nzchar(Sys.getenv("WINDIR", 
        unset = ""))) {
        file.path(Sys.getenv("WINDIR"), "Fonts")
    } else {
        character()
    }))
    candidate_dirs <- candidate_dirs[!is.na(candidate_dirs) & nzchar(candidate_dirs) & dir.exists(candidate_dirs)]
    if (length(candidate_dirs) == 0L) {
        return(NA_character_)
    }
    for (font_dir in candidate_dirs) {
        direct_path <- file.path(font_dir, font_basename)
        if (file.exists(direct_path)) {
            return(normalizePath(direct_path, winslash = "/", mustWork = TRUE))
        }
        listed <- list.files(font_dir, full.names = TRUE, all.files = FALSE)
        hit <- which(tolower(basename(listed)) == tolower(font_basename))
        if (length(hit) > 0L) {
            return(normalizePath(listed[hit[1]], winslash = "/", mustWork = TRUE))
        }
    }
    NA_character_
}

get_nma_interface_font_rows <- function(font_table) {
    n <- nrow(font_table)
    if (n == 0L) {
        return(font_table)
    }
    metadata_columns <- intersect(c("family", "style", "name", "postscript", "ps_name", "fullname", "full_name", 
        "weight", "path"), names(font_table))
    if (length(metadata_columns) == 0L) {
        return(font_table[FALSE, , drop = FALSE])
    }
    metadata_text <- rep("", n)
    for (nm in metadata_columns) {
        metadata_text <- paste(metadata_text, as.character(font_table[[nm]]))
    }
    font_table[grepl("interface", tolower(metadata_text), fixed = TRUE), , drop = FALSE]
}

resolve_nma_interface_faces <- function() {
    regular_direct <- find_nma_windows_user_font_file(NMA_INTERFACE_REGULAR_FILE_BASENAME)
    bold_direct <- find_nma_windows_user_font_file(NMA_INTERFACE_BOLD_FILE_BASENAME)
    font_table <- tryCatch(systemfonts::system_fonts(), error = function(e) NULL)
    if (is.null(font_table) || !"path" %in% names(font_table) || nrow(font_table) == 0L) {
        stop("Unable to read the system font registry. Check that systemfonts, svglite, showtext, and sysfonts are installed correctly.")
    }
    interface_fonts <- get_nma_interface_font_rows(font_table)
    face_from_path <- function(path_value, expected_label) {
        if (is.na(path_value) || !nzchar(path_value) || !file.exists(path_value)) {
            return(NULL)
        }
        norm_target <- normalizePath(path_value, winslash = "/", mustWork = TRUE)
        table_paths <- as.character(font_table$path)
        path_hit <- rep(FALSE, length(table_paths))
        for (i in seq_along(table_paths)) {
            if (!is.na(table_paths[i]) && nzchar(table_paths[i]) && file.exists(table_paths[i])) {
                current_norm <- tryCatch(normalizePath(table_paths[i], winslash = "/", mustWork = TRUE), 
                  error = function(e) NA_character_)
                path_hit[i] <- !is.na(current_norm) && identical(tolower(current_norm), tolower(norm_target))
            }
        }
        i <- if (any(path_hit)) 
            which(path_hit)[1]
        else NA_integer_
        value_or_blank <- function(column) {
            if (is.na(i) || !column %in% names(font_table)) 
                ""
            else as.character(font_table[[column]][i])
        }
        idx <- 0L
        if (!is.na(i) && "index" %in% names(font_table)) {
            idx_tmp <- suppressWarnings(as.integer(font_table$index[i]))
            if (!is.na(idx_tmp)) 
                idx <- idx_tmp
        }
        list(path = norm_target, index = idx, family = value_or_blank("family"), style = value_or_blank("style"), 
            weight = value_or_blank("weight"), name = value_or_blank("name"), source = paste0("confirmed Windows TTF (", 
                expected_label, ")"))
    }
    regular_face <- face_from_path(regular_direct, "Regular")
    bold_face <- face_from_path(bold_direct, "Negreta")
    if (is.null(regular_face) || is.null(bold_face)) {
        if (nrow(interface_fonts) == 0L) {
            stop("No InterFace font records were found. Install InterFace Regular and Bold, or use the automatic sans-serif fallback.")
        }
        n <- nrow(interface_fonts)
        get_col <- function(nm) {
            if (nm %in% names(interface_fonts)) 
                as.character(interface_fonts[[nm]])
            else rep("", n)
        }
        family_text <- get_col("family")
        style_text <- get_col("style")
        weight_text <- get_col("weight")
        name_text <- get_col("name")
        path_text <- get_col("path")
        italic <- if ("italic" %in% names(interface_fonts)) {
            suppressWarnings(as.logical(interface_fonts$italic))
        }
        else {
            rep(FALSE, n)
        }
        italic[is.na(italic)] <- FALSE
        descriptor <- tolower(paste(family_text, style_text, weight_text, name_text, basename(path_text)))
        bold_signal <- grepl("negreta|bold|demi|semi[ _-]?bold|semibold|heavy|black|extrabold|extra[ _-]?bold|700|800|900", 
            descriptor, perl = TRUE)
        if (is.null(regular_face)) {
            regular_candidates <- which(!italic & !bold_signal)
            regular_score <- rep(-Inf, n)
            regular_score[regular_candidates] <- 0
            if (length(regular_candidates) > 0L) {
                regular_score[regular_candidates] <- regular_score[regular_candidates] + 200 * (tolower(trimws(family_text[regular_candidates])) == 
                  "interface") + 160 * (tolower(trimws(name_text[regular_candidates])) == "interface") + 
                  120 * grepl("regular", tolower(style_text[regular_candidates]), fixed = TRUE) + 40 * 
                  grepl("normal|regular|book|roman|400", descriptor[regular_candidates], perl = TRUE)
            }
            if (any(is.finite(regular_score))) {
                i <- which.max(regular_score)
                candidate_path <- path_text[i]
                if (!is.na(candidate_path) && nzchar(candidate_path) && file.exists(candidate_path)) {
                  regular_face <- face_from_path(candidate_path, "Regular metadata fallback")
                }
            }
        }
        if (is.null(bold_face)) {
            bold_candidates <- which(!italic & bold_signal)
            bold_score <- rep(-Inf, n)
            bold_score[bold_candidates] <- 0
            if (length(bold_candidates) > 0L) {
                bold_score[bold_candidates] <- bold_score[bold_candidates] + 300 * grepl("interface[ _-]*negreta", 
                  descriptor[bold_candidates], perl = TRUE) + 240 * grepl("negreta", descriptor[bold_candidates], 
                  fixed = TRUE) + 160 * grepl("bold", descriptor[bold_candidates], fixed = TRUE) + 40 * 
                  grepl("700|800|900|heavy|black", descriptor[bold_candidates], perl = TRUE)
            }
            if (any(is.finite(bold_score))) {
                i <- which.max(bold_score)
                candidate_path <- path_text[i]
                if (!is.na(candidate_path) && nzchar(candidate_path) && file.exists(candidate_path)) {
                  bold_face <- face_from_path(candidate_path, "Negreta metadata fallback")
                }
            }
        }
    }
    if (is.null(regular_face) || is.null(bold_face)) {
        candidate_description <- if (nrow(interface_fonts) > 0L) {
            rows <- seq_len(nrow(interface_fonts))
            paste(vapply(rows, function(i) {
                vals <- vapply(intersect(c("family", "style", "weight", "name", "path"), names(interface_fonts)), 
                  function(nm) as.character(interface_fonts[[nm]][i]), character(1))
                paste(vals, collapse = " | ")
            }, character(1)), collapse = "\n  ")
        }
        else {
            "<none>"
        }
        stop("InterFace was detected, but both Regular and Bold font files could not be resolved.\n", 
            "Expected Windows font files:\n", "  Regular: ", NMA_INTERFACE_REGULAR_FILE_BASENAME, 
            "\n", "  Bold: ", NMA_INTERFACE_BOLD_FILE_BASENAME, "\n", "InterFace candidates reported by systemfonts:\n  ", 
            candidate_description)
    }
    list(regular = regular_face, bold = bold_face)
}

# Select a widely available sans-serif family when InterFace is unavailable.
# The fallback affects figure appearance only and never changes an analysis.
resolve_nma_fallback_faces <- function() {
    font_table <- tryCatch(systemfonts::system_fonts(), error = function(e) NULL)
    if (is.null(font_table) || !"path" %in% names(font_table) || nrow(font_table) == 0L) {
        return(NULL)
    }
    font_table <- font_table[file.exists(font_table$path), , drop = FALSE]
    if (nrow(font_table) == 0L) {
        return(NULL)
    }
    value_or_blank <- function(data, column) {
        if (column %in% names(data)) as.character(data[[column]]) else rep("", nrow(data))
    }
    family_text <- value_or_blank(font_table, "family")
    style_text <- value_or_blank(font_table, "style")
    weight_text <- value_or_blank(font_table, "weight")
    name_text <- value_or_blank(font_table, "name")
    descriptor <- tolower(paste(family_text, style_text, weight_text, name_text, basename(font_table$path)))
    italic <- if ("italic" %in% names(font_table)) suppressWarnings(as.logical(font_table$italic)) else rep(FALSE, nrow(font_table))
    italic[is.na(italic)] <- FALSE
    bold_signal <- grepl("bold|demi|semi[ _-]?bold|semibold|heavy|black|700|800|900", descriptor, perl = TRUE)
    preferred_families <- c("Arial", "Liberation Sans", "DejaVu Sans", "Nimbus Sans", "Noto Sans")
    choose_row <- function(bold = FALSE) {
        for (family_name in preferred_families) {
            candidates <- which(tolower(trimws(family_text)) == tolower(family_name) & !italic & bold_signal == bold)
            if (length(candidates) > 0L) return(candidates[1])
        }
        candidates <- which(!italic & bold_signal == bold)
        if (length(candidates) > 0L) candidates[1] else NA_integer_
    }
    regular_index <- choose_row(FALSE)
    bold_index <- choose_row(TRUE)
    if (is.na(regular_index)) regular_index <- 1L
    if (is.na(bold_index)) bold_index <- regular_index
    make_face <- function(i, label) {
        index_value <- if ("index" %in% names(font_table)) suppressWarnings(as.integer(font_table$index[i])) else 0L
        if (is.na(index_value)) index_value <- 0L
        list(
            path = normalizePath(font_table$path[i], winslash = "/", mustWork = TRUE),
            index = index_value,
            family = family_text[i],
            style = style_text[i],
            weight = weight_text[i],
            name = name_text[i],
            source = paste("portable sans-serif fallback", label)
        )
    }
    list(regular = make_face(regular_index, "regular"), bold = make_face(bold_index, "bold"))
}

resolve_nma_plot_faces <- function() {
    tryCatch(resolve_nma_interface_faces(), error = function(e) {
        fallback_faces <- resolve_nma_fallback_faces()
        if (!isTRUE(getOption("nma.font.fallback.warned", FALSE))) {
            warning(
                if (is.null(fallback_faces)) {
                    "InterFace Regular/Bold was not available; using the graphics-device sans font. "
                } else {
                    "InterFace Regular/Bold was not available; using a system sans-serif fallback. "
                },
                "Statistical results are unaffected. Original font message: ",
                conditionMessage(e),
                call. = FALSE
            )
            options(nma.font.fallback.warned = TRUE)
        }
        fallback_faces
    })
}

register_nma_showtext_fonts <- function(faces = NULL) {
    if (is.null(faces)) faces <- resolve_nma_plot_faces()
    if (is.null(faces)) return(NULL)
    registered <- tryCatch(sysfonts::font_families(), error = function(e) character())
    if (!NMA_FONT_FAMILY_NAME %in% registered) {
        sysfonts::font_add(family = NMA_FONT_FAMILY_NAME, regular = faces$regular$path, bold = faces$bold$path, 
            italic = faces$regular$path, bolditalic = faces$bold$path)
    }
    invisible(faces)
}

verify_nma_font_installation <- function() {
    faces <- resolve_nma_plot_faces()
    if (is.null(faces)) {
        message("No outline-capable font file was resolved; figures will use the graphics-device sans font.")
        return(invisible(NULL))
    }
    message("Plot font files selected:\n", "  Regular -> ", faces$regular$path, if (nzchar(faces$regular$family)) 
        paste0(" [family=", faces$regular$family, "]")
    else "", if (nzchar(faces$regular$style)) 
        paste0(" [style=", faces$regular$style, "]")
    else "", "\n  Negreta -> ", faces$bold$path, if (nzchar(faces$bold$family)) 
        paste0(" [family=", faces$bold$family, "]")
    else "", if (nzchar(faces$bold$style)) 
        paste0(" [style=", faces$bold$style, "]")
    else "")
    register_nma_showtext_fonts(faces)
    invisible(faces)
}

mix_with_white <- function(colour, colour_fraction) {
    colour_fraction <- max(0, min(1, as.numeric(colour_fraction)[1]))
    rgb_value <- grDevices::col2rgb(colour)[, 1]
    mixed_value <- round(colour_fraction * rgb_value + (1 - colour_fraction) * 255)
    grDevices::rgb(mixed_value[1], mixed_value[2], mixed_value[3], maxColorValue = 255)
}

refresh_bmj_ggsci_palette <- function() {
    palette_from_ggsci <- unname((ggsci::pal_bmj(palette = "default", alpha = 1))(9))
    palette_from_ggsci <- substr(palette_from_ggsci, 1, 7)
    BMJ_GGSCI_PALETTE <<- palette_from_ggsci
    BMJ_PRIMARY <<- NMA_PRIMARY_HEX
    BMJ_PRIMARY_LIGHT <<- mix_with_white(BMJ_PRIMARY, 0.1)
    BMJ_PRIMARY_MID <<- mix_with_white(BMJ_PRIMARY, 0.25)
    invisible(BMJ_GGSCI_PALETTE)
}

register_nma_windows_fonts <- function() {
    faces <- verify_nma_font_installation()
    if (is.null(faces)) {
        c(light = "sans", bold = "sans", title = "sans")
    } else {
        c(light = NMA_FONT_FAMILY_NAME, bold = NMA_FONT_FAMILY_NAME, title = NMA_FONT_FAMILY_NAME)
    }
}

safe_plot_font_family <- function(font_family = NMA_FONT_BODY_NAME) {
    if (is.null(font_family) || length(font_family) == 0 || is.na(font_family[1]) || !nzchar(trimws(as.character(font_family[1])))) {
        font_family <- NMA_FONT_FAMILY_NAME
    }
    requested_font <- trimws(as.character(font_family)[1])
    interface_requested <- tolower(requested_font) %in% tolower(c("InterFace", "Interface", NMA_FONT_FAMILY_NAME, 
        NMA_FONT_BODY_NAME, NMA_FONT_TITLE_NAME, NMA_FONT_BODY_ALIAS, NMA_FONT_TITLE_ALIAS))
    if (interface_requested) {
        faces <- verify_nma_font_installation()
        return(if (is.null(faces)) "sans" else NMA_FONT_FAMILY_NAME)
    }
    requested_font
}

get_nma_font_roles <- function(font_family = NMA_FONT_BODY_NAME, vector_device = FALSE) {
    requested_font <- if (is.null(font_family) || length(font_family) == 0 || is.na(font_family[1]) || 
        !nzchar(trimws(as.character(font_family[1])))) {
        NMA_FONT_FAMILY_NAME
    }
    else {
        trimws(as.character(font_family)[1])
    }
    interface_requested <- tolower(requested_font) %in% tolower(c("InterFace", "Interface", NMA_FONT_FAMILY_NAME, 
        NMA_FONT_BODY_NAME, NMA_FONT_TITLE_NAME, NMA_FONT_BODY_ALIAS, NMA_FONT_TITLE_ALIAS))
    if (interface_requested) {
        selected_font <- safe_plot_font_family(requested_font)
        return(c(light = selected_font, bold = selected_font, title = selected_font))
    }
    c(light = requested_font, bold = requested_font, title = requested_font)
}

assert_nma_svg_is_fully_outlined <- function(filename) {
    if (!file.exists(filename)) {
        stop("Outlined SVG was not created: ", filename)
    }
    svg_text <- paste(readLines(filename, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    has_live_text <- grepl("<text(?:\\s|>)|<tspan(?:\\s|>)", svg_text, ignore.case = TRUE, perl = TRUE)
    if (has_live_text) {
        stop("Outlined-SVG validation failed because live <text> or <tspan> elements remain.\n", "The file was rejected to prevent font substitution in Word.\n", 
            "Check that showtext and sysfonts can read the selected plot font:\n", filename)
    }
    invisible(TRUE)
}

open_nma_svg <- function(filename, width_in, height_in, pointsize = 12, bg = "white") {
    if (!is.finite(width_in) || width_in <= 0 || !is.finite(height_in) || height_in <= 0) {
        stop("SVG width_in and height_in must be positive.")
    }
    faces <- register_nma_showtext_fonts()
    svglite::svglite(filename = filename, width = width_in, height = height_in, pointsize = pointsize, 
        bg = bg, standalone = TRUE, fix_text_size = FALSE)
    if (is.null(faces)) {
        NMA_SHOWTEXT_ACTIVE <<- FALSE
        message("SVG opened with the graphics-device sans font; live text will be retained.")
        return(invisible(filename))
    }
    tryCatch(showtext::showtext_begin(), error = function(e) {
        if (grDevices::dev.cur() > 1) {
            try(grDevices::dev.off(), silent = TRUE)
        }
        stop("Unable to start showtext outline rendering: ", conditionMessage(e))
    })
    NMA_SHOWTEXT_ACTIVE <<- TRUE
    message("Outlined SVG fonts registered:\n", "  Regular -> ", basename(faces$regular$path), "\n", 
        "  Negreta -> ", basename(faces$bold$path))
    invisible(filename)
}

close_nma_svg <- function(filename, verify_outline = TRUE) {
    if (isTRUE(NMA_SHOWTEXT_ACTIVE)) try(showtext::showtext_end(), silent = TRUE)
    if (grDevices::dev.cur() > 1) {
        grDevices::dev.off()
    }
    if (isTRUE(verify_outline) && isTRUE(NMA_SHOWTEXT_ACTIVE)) {
        assert_nma_svg_is_fully_outlined(filename)
        message("Outlined SVG validation passed (no <text>/<tspan>): ", normalizePath(filename, 
            winslash = "/", mustWork = TRUE))
    } else if (isTRUE(verify_outline)) {
        message("SVG created with live text because no outline-capable font file was available.")
    }
    NMA_SHOWTEXT_ACTIVE <<- FALSE
    invisible(filename)
}

abort_nma_svg <- function() {
    if (isTRUE(NMA_SHOWTEXT_ACTIVE)) try(showtext::showtext_end(), silent = TRUE)
    if (grDevices::dev.cur() > 1) {
        try(grDevices::dev.off(), silent = TRUE)
    }
    NMA_SHOWTEXT_ACTIVE <<- FALSE
    invisible(NULL)
}

test_nma_plot_font <- function(output_file = file.path(getwd(), "NMA_plot_font_test.svg")) {
    faces <- verify_nma_font_installation()
    font_roles <- get_nma_font_roles(if (is.null(faces)) "sans" else NMA_FONT_FAMILY_NAME, vector_device = TRUE)
    open_nma_svg(filename = output_file, width_in = 7, height_in = 4.4, pointsize = 12, bg = "white")
    on.exit(abort_nma_svg(), add = TRUE)
    par(mar = c(0, 0, 0, 0), family = unname(font_roles[["light"]]), font = NMA_FONT_REGULAR_FACE, bg = "white")
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))
    text(x = 0.5, y = 0.76, labels = "Pain intensity \u2014 bold plot font", family = unname(font_roles[["title"]]), 
        font = NMA_FONT_BOLD_FACE, cex = 1.7)
    text(x = 0.5, y = 0.52, labels = "Control  Exergaming  Telemedicine", family = unname(font_roles[["light"]]), 
        font = NMA_FONT_REGULAR_FACE, cex = 1.7)
    text(x = 0.5, y = 0.3, labels = "Face to face  Biofeedback  Mobile-App", family = unname(font_roles[["light"]]), 
        font = NMA_FONT_REGULAR_FACE, cex = 1.7)
    text(x = 0.5, y = 0.11, labels = "Regular source: selected system font", family = unname(font_roles[["light"]]), 
        font = NMA_FONT_REGULAR_FACE, cex = 0.9)
    close_nma_svg(output_file, verify_outline = TRUE)
    on.exit(NULL, add = FALSE)
    if (is.null(faces)) {
        message("Plot-font diagnostic SVG saved with graphics-device sans: ", normalizePath(output_file, winslash = "/", mustWork = TRUE))
    } else {
        message("Plot-font diagnostic SVG saved: ", normalizePath(output_file, winslash = "/", 
            mustWork = TRUE), "\nRegular face: ", basename(faces$regular$path), "\nBold face: ", 
            basename(faces$bold$path))
    }
    invisible(output_file)
}

# -----------------------------------------------------------------------------
# Labels, file names, and treatment-node harmonisation
# -----------------------------------------------------------------------------
make_readable_title <- function(outcome_name, time_name) {
    outcome_label <- make_display_outcome_name(outcome_name)
    outcome_key <- gsub("[^a-z0-9]+", "", tolower(outcome_label))
    if (outcome_key %in% c("painfearavoidance", "painfearavoindance")) {
        outcome_label <- "Pain-related fear avoidance"
    }
    else if (outcome_key %in% c("qol", "qualityoflife", "healthrelatedqualityoflife")) {
        outcome_label <- "Health-related quality of life"
    }
    time_label <- dplyr::recode(time_name, post_intervention = "post intervention", short_term = "short term", 
        mid_term = "mid term", long_term = "long term", .default = gsub("_", " ", time_name))
    paste0(outcome_label, " \u2014 ", time_label)
}

make_safe_file_stub <- function(x) {
    x <- stringr::str_squish(as.character(x))
    x <- gsub("[<>:\"/\\\\|?*]", "_", x)
    x <- gsub("[[:cntrl:]]", "_", x)
    x <- gsub("[. ]+$", "", x)
    ifelse(is.na(x) | x == "", "NMA", x)
}

canonicalise_nma_node <- function(x) {
    x <- stringr::str_squish(as.character(x))
    node_key <- gsub("[^a-z0-9]+", "", tolower(x))
    matched <- match(node_key, names(NMA_NODE_CANONICAL_ALIASES))
    hit <- !is.na(matched)
    x[hit] <- unname(NMA_NODE_CANONICAL_ALIASES[matched[hit]])
    x
}

display_nma_node <- function(x) {
    canonical <- canonicalise_nma_node(x)
    matched <- match(canonical, names(NMA_NODE_DISPLAY_LABELS))
    hit <- !is.na(matched)
    canonical[hit] <- unname(NMA_NODE_DISPLAY_LABELS[matched[hit]])
    canonical
}

clean_league_text <- function(x) {
    x <- as.character(x)
    x[is.na(x) | x == "" | x == "NA"] <- "."
    x <- gsub("\\[", "(", x)
    x <- gsub("\\]", ")", x)
    x <- gsub("\\s*;\\s*", " to ", x)
    x <- gsub("[[:space:]]+\\(", "\n(", x)
    x
}

is_significant_league_cell <- function(x, null_value = 0) {
    if (length(x) == 0 || is.na(x[1]) || !nzchar(trimws(as.character(x[1]))) || trimws(as.character(x[1])) %in% 
        c(".", "NA")) {
        return(FALSE)
    }
    cell_text <- gsub("\u2212", "-", as.character(x[1]), fixed = TRUE)
    number_pattern <- paste0("[-+]?", "(?:[0-9]*\\.[0-9]+|[0-9]+\\.?[0-9]*)", "(?:[eE][-+]?[0-9]+)?")
    number_text <- regmatches(cell_text, gregexpr(number_pattern, cell_text, perl = TRUE))[[1]]
    if (length(number_text) < 3) {
        return(FALSE)
    }
    number_values <- suppressWarnings(as.numeric(number_text))
    ci_values <- tail(number_values, 2)
    if (length(ci_values) != 2 || any(!is.finite(ci_values))) {
        return(FALSE)
    }
    ci_lower <- min(ci_values)
    ci_upper <- max(ci_values)
    isTRUE(ci_lower > null_value || ci_upper < null_value)
}

get_random_league_matrix <- function(league_obj) {
    if (!is.null(league_obj$random)) {
        return(as.matrix(league_obj$random))
    }
    if (!is.null(league_obj$common)) {
        return(as.matrix(league_obj$common))
    }
    as.matrix(league_obj)
}

# -----------------------------------------------------------------------------
# Reproducible tabular outputs
# -----------------------------------------------------------------------------
write_master_workbook <- function(output_file, sheets, league_matrix = NULL, font_family = NMA_FONT_BODY_NAME) {
    wb <- openxlsx::createWorkbook()
    excel_light_font <- NMA_FONT_BODY_NAME
    excel_bold_font <- NMA_FONT_BODY_NAME
    excel_title_font <- NMA_FONT_TITLE_NAME
    header_style <- openxlsx::createStyle(fontName = excel_title_font, fontSize = 10, fontColour = "#FFFFFF", 
        fgFill = BMJ_PRIMARY, halign = "center", valign = "center", wrapText = TRUE, border = "TopBottomLeftRight", 
        borderColour = "#B7C9D6")
    body_style <- openxlsx::createStyle(fontName = excel_light_font, fontSize = 9, valign = "top", wrapText = TRUE, 
        border = "TopBottomLeftRight", borderColour = "#D9E2F3")
    safe_names <- make_safe_sheet_names(names(sheets))
    old_max_width <- getOption("openxlsx.maxWidth")
    options(openxlsx.maxWidth = 45)
    on.exit(options(openxlsx.maxWidth = old_max_width), add = TRUE)
    for (i in seq_along(sheets)) {
        sh <- safe_names[i]
        dat <- sheets[[i]]
        if (is.null(dat)) {
            dat <- data.frame(note = "Not available", stringsAsFactors = FALSE)
        }
        else if (!is.data.frame(dat)) {
            dat <- as.data.frame(dat, stringsAsFactors = FALSE)
        }
        dat[] <- lapply(dat, function(x) {
            if (is.list(x)) {
                vapply(x, function(z) paste(as.character(z), collapse = "; "), character(1))
            }
            else {
                x
            }
        })
        if (ncol(dat) == 0) {
            dat <- data.frame(note = "No rows available", stringsAsFactors = FALSE)
        }
        openxlsx::addWorksheet(wb, sh)
        openxlsx::showGridLines(wb, sh, showGridLines = FALSE)
        openxlsx::writeData(wb, sh, dat, withFilter = nrow(dat) > 0)
        openxlsx::addStyle(wb, sh, header_style, rows = 1, cols = seq_len(ncol(dat)), gridExpand = TRUE)
        if (nrow(dat) > 0) {
            openxlsx::addStyle(wb, sh, body_style, rows = 2:(nrow(dat) + 1), cols = seq_len(ncol(dat)), 
                gridExpand = TRUE)
        }
        openxlsx::freezePane(wb, sh, firstRow = TRUE)
        openxlsx::setColWidths(wb, sh, cols = seq_len(ncol(dat)), widths = "auto")
        openxlsx::setRowHeights(wb, sh, rows = 1, heights = 32)
    }
    if (!is.null(league_matrix)) {
        sh <- "league_table"
        league_matrix <- as.matrix(league_matrix)
        k <- nrow(league_matrix)
        openxlsx::addWorksheet(wb, sh, zoom = 100, paperSize = 9, orientation = "landscape")
        openxlsx::showGridLines(wb, sh, showGridLines = FALSE)
        openxlsx::writeData(wb, sh, x = as.data.frame(league_matrix, check.names = FALSE, stringsAsFactors = FALSE), 
            startRow = 1, startCol = 1, colNames = FALSE, rowNames = FALSE)
        league_body_style <- openxlsx::createStyle(fontName = excel_light_font, fontSize = NMA_LEAGUE_BODY_FONT_SIZE, 
            fgFill = "#FFFFFF", halign = "left", valign = "center", wrapText = TRUE, border = "TopBottomLeftRight", 
            borderColour = "#000000")
        openxlsx::addStyle(wb, sh, league_body_style, rows = seq_len(k), cols = seq_len(k), gridExpand = TRUE)
        diag_style <- openxlsx::createStyle(fontName = excel_bold_font, fontSize = NMA_LEAGUE_DIAG_FONT_SIZE, 
            textDecoration = "bold", fontColour = BMJ_TEXT_DARK, fgFill = BMJ_PRIMARY_MID, halign = "left", 
            valign = "center", wrapText = TRUE, border = "TopBottomLeftRight", borderColour = "#000000")
        lower_style <- openxlsx::createStyle(fontName = excel_light_font, fontSize = NMA_LEAGUE_BODY_FONT_SIZE, 
            fgFill = BMJ_PRIMARY_LIGHT, halign = "left", valign = "center", wrapText = TRUE, border = "TopBottomLeftRight", 
            borderColour = "#000000")
        significant_style <- openxlsx::createStyle(fontName = excel_bold_font, textDecoration = "bold")
        for (i in seq_len(k)) {
            openxlsx::addStyle(wb, sh, diag_style, rows = i, cols = i, stack = TRUE)
            if (i < k) {
                openxlsx::addStyle(wb, sh, lower_style, rows = (i + 1):k, cols = i, stack = TRUE)
            }
        }
        for (i in seq_len(k)) {
            for (j in seq_len(k)) {
                if (i != j && is_significant_league_cell(league_matrix[i, j], null_value = 0)) {
                  openxlsx::addStyle(wb, sh, significant_style, rows = i, cols = j, stack = TRUE)
                }
            }
        }
        openxlsx::setColWidths(wb, sh, cols = seq_len(k), widths = rep(NMA_LEAGUE_COLUMN_WIDTH, k))
        openxlsx::setRowHeights(wb, sh, rows = seq_len(k), heights = rep(NMA_LEAGUE_ROW_HEIGHT, k))
        openxlsx::pageSetup(wb, sh, orientation = "landscape", paperSize = 9, scale = 100, fitToWidth = FALSE, 
            fitToHeight = FALSE, left = 0.25, right = 0.25, top = 0.35, bottom = 0.35, header = 0.15, 
            footer = 0.15)
    }
    openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
    invisible(output_file)
}

write_league_workbook <- function(output_file, league_matrix, font_family = NMA_FONT_BODY_NAME) {
    if (is.null(league_matrix)) {
        stop("league_matrix must not be empty.")
    }
    write_master_workbook(output_file = output_file, sheets = list(), league_matrix = league_matrix, 
        font_family = font_family)
    invisible(output_file)
}

write_pscore_workbook <- function(output_file, pscore_table, font_family = NMA_FONT_BODY_NAME) {
    required_cols <- c("Treatment node", "P-score", "Rank")
    if (!is.data.frame(pscore_table) || !all(required_cols %in% names(pscore_table)) || nrow(pscore_table) == 
        0) {
        stop("pscore_table must contain Treatment node, P-score, and Rank columns, ", "and at least one row.")
    }
    pscore_table <- pscore_table[, required_cols, drop = FALSE]
    excel_font <- if (is.null(font_family) || length(font_family) == 0 || is.na(font_family[1]) || !nzchar(as.character(font_family[1]))) {
        NMA_FONT_BODY_NAME
    }
    else {
        as.character(font_family[1])
    }
    wb <- openxlsx::createWorkbook()
    sh <- "P_score_table"
    openxlsx::addWorksheet(wb, sh, gridLines = FALSE, zoom = 100, paperSize = 9, orientation = "portrait")
    openxlsx::writeData(wb, sh, x = pscore_table, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE, 
        withFilter = FALSE)
    header_style <- openxlsx::createStyle(fontName = excel_font, fontSize = 11, textDecoration = "bold", 
        halign = "center", valign = "center")
    body_text_style <- openxlsx::createStyle(fontName = excel_font, fontSize = 11, halign = "left", valign = "center")
    pscore_number_style <- openxlsx::createStyle(fontName = excel_font, fontSize = 11, halign = "center", 
        valign = "center", numFmt = "0.000")
    rank_style <- openxlsx::createStyle(fontName = excel_font, fontSize = 11, halign = "center", valign = "center", 
        numFmt = "0")
    top_rule_style <- openxlsx::createStyle(border = "Top", borderStyle = "medium", borderColour = "#000000")
    header_rule_style <- openxlsx::createStyle(border = "Bottom", borderStyle = "thin", borderColour = "#000000")
    bottom_rule_style <- openxlsx::createStyle(border = "Bottom", borderStyle = "medium", borderColour = "#000000")
    last_row <- nrow(pscore_table) + 1L
    openxlsx::addStyle(wb, sh, header_style, rows = 1, cols = 1:3, gridExpand = TRUE)
    openxlsx::addStyle(wb, sh, top_rule_style, rows = 1, cols = 1:3, gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, sh, header_rule_style, rows = 1, cols = 1:3, gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, sh, body_text_style, rows = 2:last_row, cols = 1, gridExpand = TRUE)
    openxlsx::addStyle(wb, sh, pscore_number_style, rows = 2:last_row, cols = 2, gridExpand = TRUE)
    openxlsx::addStyle(wb, sh, rank_style, rows = 2:last_row, cols = 3, gridExpand = TRUE)
    openxlsx::addStyle(wb, sh, bottom_rule_style, rows = last_row, cols = 1:3, gridExpand = TRUE, stack = TRUE)
    openxlsx::setColWidths(wb, sh, cols = 1:3, widths = c(28, 12, 10))
    openxlsx::setRowHeights(wb, sh, rows = 1, heights = 27)
    openxlsx::setRowHeights(wb, sh, rows = 2:last_row, heights = 23)
    openxlsx::pageSetup(wb, sh, orientation = "portrait", paperSize = 9, scale = 100, fitToWidth = FALSE, 
        fitToHeight = FALSE, left = 0.6, right = 0.6, top = 0.6, bottom = 0.6, header = 0.2, footer = 0.2)
    openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
    invisible(output_file)
}

draw_rounded_rect_base <- function(xleft, ybottom, xright, ytop, rx, ry, border = "#9AA0A6", fill = NA, 
    lwd = 1.2) {
    rx <- min(abs(rx), abs(xright - xleft)/2)
    ry <- min(abs(ry), abs(ytop - ybottom)/2)
    theta_br <- seq(-pi/2, 0, length.out = 18)
    theta_tr <- seq(0, pi/2, length.out = 18)
    theta_tl <- seq(pi/2, pi, length.out = 18)
    theta_bl <- seq(pi, 3 * pi/2, length.out = 18)
    x <- c(xright - rx + rx * cos(theta_br), xright - rx + rx * cos(theta_tr), xleft + rx + rx * cos(theta_tl), 
        xleft + rx + rx * cos(theta_bl))
    y <- c(ybottom + ry + ry * sin(theta_br), ytop - ry + ry * sin(theta_tr), ytop - ry + ry * sin(theta_tl), 
        ybottom + ry + ry * sin(theta_bl))
    polygon(x = x, y = y, border = border, col = fill, lwd = lwd)
    invisible(NULL)
}

# -----------------------------------------------------------------------------
# Network plots, league tables, and model-result extraction
# -----------------------------------------------------------------------------
make_styled_league_table <- function(nma_object, sequence = NULL, digits = 2, backtransf = FALSE, ci_separator = " to ") {
    if (is.null(sequence)) {
        sequence <- nma_object$trts
    }
    else {
        sequence <- c(sequence[sequence %in% nma_object$trts], setdiff(nma_object$trts, sequence))
    }
    league_obj <- netmeta::netleague(nma_object, common = FALSE, random = TRUE, seq = sequence, digits = digits, 
        text.NA = ".", bracket = "(", separator = ci_separator, backtransf = backtransf)
    league_mat <- get_random_league_matrix(league_obj)
    if (is.null(rownames(league_mat))) {
        rownames(league_mat) <- sequence[seq_len(nrow(league_mat))]
    }
    if (is.null(colnames(league_mat))) {
        colnames(league_mat) <- sequence[seq_len(ncol(league_mat))]
    }
    league_mat[] <- clean_league_text(league_mat)
    rownames(league_mat) <- display_nma_node(rownames(league_mat))
    colnames(league_mat) <- display_nma_node(colnames(league_mat))
    diag(league_mat) <- rownames(league_mat)
    invisible(list(league_matrix = league_mat))
}

make_direct_evidence_network_plot <- function(arm_analysis, reported_smd_used, pairwise_data, file_prefix, 
    plot_title, node_order = NULL, font_family = NMA_FONT_BODY_NAME) {
    node_data <- dplyr::bind_rows(arm_analysis %>% dplyr::transmute(study_id = as.character(study_id), 
        treatment = as.character(Group), n = as.numeric(n), source_type = "arm_level_mean_sd"), reported_smd_used %>% 
        dplyr::transmute(study_id = as.character(studlab), treatment = as.character(treat1), n = as.numeric(n1), 
            source_type = "direct_reported_SMD"), reported_smd_used %>% dplyr::transmute(study_id = as.character(studlab), 
        treatment = as.character(treat2), n = as.numeric(n2), source_type = "direct_reported_SMD")) %>% 
        dplyr::filter(!is.na(study_id), study_id != "", !is.na(treatment), treatment != "")
    node_n_conflict <- node_data %>% dplyr::filter(is.finite(n)) %>% dplyr::group_by(study_id, treatment) %>% 
        dplyr::summarise(n_values = dplyr::n_distinct(n), values = paste(sort(unique(n)), collapse = " | "), 
            .groups = "drop") %>% dplyr::filter(n_values > 1)
    if (nrow(node_n_conflict) > 0) {
        print(as.data.frame(node_n_conflict), row.names = FALSE)
        stop("Different sample sizes were found for the same treatment arm within a study; ", "network node sizes cannot be calculated reliably.")
    }
    node_data <- node_data %>% dplyr::arrange(study_id, treatment, dplyr::desc(is.finite(n))) %>% dplyr::distinct(study_id, 
        treatment, .keep_all = TRUE)
    if (nrow(reported_smd_used) > 0 && any(!is.finite(reported_smd_used$n1) | !is.finite(reported_smd_used$n2))) {
        warning("Some directly reported SMD records are missing n1/n2; network edges can still be drawn, ", "but participant counts for the corresponding nodes may be underestimated.")
    }
    node_summary <- node_data %>% dplyr::group_by(treatment) %>% dplyr::summarise(n_participants = sum(n, 
        na.rm = TRUE), n_studies = dplyr::n_distinct(study_id), n_arms = dplyr::n(), .groups = "drop")
    edge_summary <- pairwise_data %>% dplyr::transmute(study_id = as.character(studlab), from = pmin(as.character(treat1), 
        as.character(treat2)), to = pmax(as.character(treat1), as.character(treat2))) %>% dplyr::distinct(study_id, 
        from, to) %>% dplyr::group_by(from, to) %>% dplyr::summarise(n_studies = dplyr::n_distinct(study_id), 
        .groups = "drop")
    default_network_order <- NMA_NETWORK_NODE_ORDER
    if (!is.null(node_order)) {
        default_network_order <- c(default_network_order, canonicalise_nma_node(node_order))
    }
    available_nodes <- unique(as.character(node_summary$treatment))
    network_sequence <- c(unique(default_network_order)[unique(default_network_order) %in% available_nodes], 
        setdiff(available_nodes, unique(default_network_order)))
    network_result <- make_bmj_style_network_plot(node_summary = node_summary, edge_summary = edge_summary, 
        sequence = network_sequence, file_prefix = file_prefix, plot_title = plot_title, bottom_node = NULL, 
        bg_color = "white", node_color = BMJ_PRIMARY, edge_color = BMJ_PRIMARY, edge_alpha = 1, layout_radius = 0.82, 
        node_radius_range = c(0.014, 0.115), edge_width_range = c(1.2, 5.25), edge_width_step = 0.9, 
        label_offset = 0.07, label_wrap_width = 16, show_node_sample_size = FALSE, title_cex = NMA_NETWORK_TITLE_CEX, 
        title_y_ndc = 0.925, label_cex = NMA_NETWORK_LABEL_CEX, font_family = font_family)
    invisible(list(network_result = network_result, node_data = node_data, node_summary = node_summary, 
        edge_summary = edge_summary))
}

printout_to_table <- function(object, outcome_name, time_name, table_name) {
    if (is.null(object)) {
        return(data.frame(outcome = outcome_name, time_window = time_name, table = table_name, line_no = 1, 
            result_line = paste0(table_name, " was not available. The network may have only two treatments, ", 
                "no closed loops, or no comparisons with both direct and indirect evidence."), stringsAsFactors = FALSE))
    }
    warning_messages <- character()
    lines <- tryCatch(withCallingHandlers(capture.output(print(object)), warning = function(w) {
        warning_messages <<- c(warning_messages, conditionMessage(w))
        invokeRestart("muffleWarning")
    }), error = function(e) {
        warning_messages <<- c(warning_messages, conditionMessage(e))
        character(0)
    })
    if (length(warning_messages) > 0) {
        lines <- c(paste0("Warning: ", warning_messages), lines)
    }
    if (length(lines) == 0) {
        lines <- paste0(table_name, " produced no printable output. This usually means that the network ", 
            "has only two treatments, no closed loops, or no comparison with both ", "direct and indirect evidence.")
    }
    data.frame(outcome = rep(outcome_name, length(lines)), time_window = rep(time_name, length(lines)), 
        table = rep(table_name, length(lines)), line_no = seq_along(lines), result_line = lines, stringsAsFactors = FALSE)
}

make_heterogeneity_table <- function(nma_object, outcome_name, time_name) {
    tau_value <- get_scalar(nma_object, c("tau", "tau.random"))
    tau2_value <- get_scalar(nma_object, c("tau2", "tau2.random"))
    if (is.na(tau2_value) && !is.na(tau_value)) {
        tau2_value <- tau_value^2
    }
    I2_value <- get_scalar(nma_object, c("I2", "I2.random"))
    if (!is.na(I2_value) && I2_value <= 1) {
        I2_value <- I2_value * 100
    }
    data.frame(outcome = outcome_name, time_window = time_name, model = "random-effects frequentist NMA", 
        effect_measure = "SMD", tau = tau_value, tau_squared = tau2_value, I_squared_percent = I2_value, 
        Q = get_scalar(nma_object, c("Q", "Q.random")), df_Q = get_scalar(nma_object, c("df.Q", "df.Q.random")), 
        p_Q = get_scalar(nma_object, c("pval.Q", "pval.Q.random")), tau_estimator = "REML", prediction_interval = "prediction = TRUE in the random-effects NMA model", 
        stringsAsFactors = FALSE)
}

extract_nma_estimates <- function(nma_object, outcome_name, time_name, model = c("random", "common")) {
    model <- match.arg(model)
    te_name <- ifelse(model == "random", "TE.random", "TE.common")
    se_name <- ifelse(model == "random", "seTE.random", "seTE.common")
    lower_name <- ifelse(model == "random", "lower.random", "lower.common")
    upper_name <- ifelse(model == "random", "upper.random", "upper.common")
    TE <- nma_object[[te_name]]
    seTE <- nma_object[[se_name]]
    lower <- nma_object[[lower_name]]
    upper <- nma_object[[upper_name]]
    if (is.null(TE) || is.null(dim(TE))) {
        return(data.frame(outcome = character(), time_window = character(), model = character(), comparison = character(), 
            treat1 = character(), treat2 = character(), SMD = numeric(), SE = numeric(), lower_95_CI = numeric(), 
            upper_95_CI = numeric(), result = character(), stringsAsFactors = FALSE))
    }
    trt1 <- rownames(TE)
    trt2 <- colnames(TE)
    out <- list()
    k <- 1
    for (i in seq_len(nrow(TE))) {
        for (j in seq_len(ncol(TE))) {
            if (i < j && is.finite(suppressWarnings(as.numeric(TE[i, j])))) {
                out[[k]] <- data.frame(outcome = outcome_name, time_window = time_name, model = model, 
                  comparison = paste(trt1[i], "vs", trt2[j]), treat1 = trt1[i], treat2 = trt2[j], SMD = as.numeric(TE[i, 
                    j]), SE = if (!is.null(seTE)) 
                    as.numeric(seTE[i, j])
                  else NA_real_, lower_95_CI = if (!is.null(lower)) 
                    as.numeric(lower[i, j])
                  else NA_real_, upper_95_CI = if (!is.null(upper)) 
                    as.numeric(upper[i, j])
                  else NA_real_, result = make_ci(as.numeric(TE[i, j]), if (!is.null(lower)) 
                    as.numeric(lower[i, j])
                  else NA_real_, if (!is.null(upper)) 
                    as.numeric(upper[i, j])
                  else NA_real_), stringsAsFactors = FALSE)
                k <- k + 1
            }
        }
    }
    if (length(out) == 0) {
        return(data.frame(outcome = outcome_name, time_window = time_name, model = model, comparison = NA_character_, 
            treat1 = NA_character_, treat2 = NA_character_, SMD = NA_real_, SE = NA_real_, lower_95_CI = NA_real_, 
            upper_95_CI = NA_real_, result = NA_character_, stringsAsFactors = FALSE)[0, ])
    }
    do.call(rbind, out)
}

make_reference_nma_table <- function(nma_estimates_all, reference_group, small_values = "desirable", 
    treatment_order = NULL) {
    if (nrow(nma_estimates_all) == 0) {
        return(data.frame(treatment = character(), reference_group = character(), SMD_vs_reference = numeric(), 
            lower_95_CI = numeric(), upper_95_CI = numeric(), result = character(), interpretation = character(), 
            stringsAsFactors = FALSE))
    }
    tab <- nma_estimates_all %>% filter(treat1 == reference_group | treat2 == reference_group) %>% mutate(treatment = if_else(treat1 == 
        reference_group, treat2, treat1), SMD_original = SMD, lower_original = lower_95_CI, upper_original = upper_95_CI) %>% 
        mutate(SMD_vs_reference = if_else(treat1 == reference_group, -SMD_original, SMD_original), lower_95_CI_new = if_else(treat1 == 
            reference_group, -upper_original, lower_original), upper_95_CI_new = if_else(treat1 == reference_group, 
            -lower_original, upper_original)) %>% mutate(result = make_ci(SMD_vs_reference, lower_95_CI_new, 
        upper_95_CI_new), interpretation = case_when(lower_95_CI_new > 0 & small_values == "desirable" ~ 
        "Favours reference group", upper_95_CI_new < 0 & small_values == "desirable" ~ "Favours treatment", 
        lower_95_CI_new > 0 & small_values != "desirable" ~ "Favours treatment", upper_95_CI_new < 0 & 
            small_values != "desirable" ~ "Favours reference group", TRUE ~ "No clear difference")) %>% 
        transmute(treatment = treatment, reference_group = reference_group, SMD_vs_reference = SMD_vs_reference, 
            lower_95_CI = lower_95_CI_new, upper_95_CI = upper_95_CI_new, result = result, interpretation = interpretation)
    if (!is.null(treatment_order)) {
        treatment_order <- setdiff(treatment_order, reference_group)
        tab <- tab %>% mutate(.order = match(treatment, treatment_order)) %>% arrange(is.na(.order), 
            .order, treatment) %>% select(-.order)
    }
    else {
        tab <- tab %>% arrange(SMD_vs_reference)
    }
    tab
}

netsplit_to_dataframe <- function(netsplit_object, outcome_name, time_name, digits = 2) {
    if (is.null(netsplit_object)) {
        return(data.frame(outcome = outcome_name, time_window = time_name, note = paste0("netsplit was not available. The network may have no closed loops, ", 
            "or direct and indirect evidence could not be separated."), stringsAsFactors = FALSE))
    }
    ns <- unclass(netsplit_object)
    comparison <- NULL
    possible_comparison_names <- c("comparison", "comparisons", "Comparison")
    for (nm in possible_comparison_names) {
        if (!is.null(ns[[nm]])) {
            comparison <- as.character(ns[[nm]])
            break
        }
    }
    if (is.null(comparison) && !is.null(ns[["treat1"]]) && !is.null(ns[["treat2"]])) {
        comparison <- paste(as.character(ns[["treat1"]]), "vs", as.character(ns[["treat2"]]))
    }
    if (is.null(comparison)) {
        vector_lengths <- sapply(ns, function(x) {
            if (is.atomic(x) && !is.matrix(x)) {
                length(x)
            }
            else {
                NA_integer_
            }
        })
        vector_lengths <- vector_lengths[is.finite(vector_lengths) & vector_lengths > 1]
        if (length(vector_lengths) == 0) {
            return(data.frame(outcome = outcome_name, time_window = time_name, note = paste0("Failed to identify comparison-level results inside the netsplit object. ", 
                "Please use local_inconsistency_printout."), stringsAsFactors = FALSE))
        }
        n_rows <- as.integer(names(sort(table(vector_lengths), decreasing = TRUE))[1])
        comparison <- paste0("Comparison_", seq_len(n_rows))
    }
    else {
        n_rows <- length(comparison)
    }
    extracted <- list(outcome = rep(outcome_name, n_rows), time_window = rep(time_name, n_rows), comparison = comparison)
    for (nm in names(ns)) {
        x <- ns[[nm]]
        if (is.atomic(x) && !is.matrix(x) && length(x) == n_rows) {
            extracted[[nm]] <- x
        }
        if (is.matrix(x) && nrow(x) == n_rows) {
            for (j in seq_len(ncol(x))) {
                col_nm <- colnames(x)[j]
                if (is.null(col_nm) || col_nm == "") {
                  col_nm <- paste0("V", j)
                }
                extracted[[paste0(nm, "_", col_nm)]] <- x[, j]
            }
        }
    }
    tab <- as.data.frame(extracted, stringsAsFactors = FALSE, check.names = FALSE)
    tab <- tab[, !duplicated(names(tab)), drop = FALSE]
    rename_if_exists <- function(data, old, new) {
        if (old %in% names(data)) {
            names(data)[names(data) == old] <- new
        }
        data
    }
    tab <- tab %>% rename_if_exists("TE.nma", "NMA_SMD") %>% rename_if_exists("seTE.nma", "NMA_SE") %>% 
        rename_if_exists("lower.nma", "NMA_lower_95_CI") %>% rename_if_exists("upper.nma", "NMA_upper_95_CI") %>% 
        rename_if_exists("TE.random", "NMA_SMD") %>% rename_if_exists("seTE.random", "NMA_SE") %>% rename_if_exists("lower.random", 
        "NMA_lower_95_CI") %>% rename_if_exists("upper.random", "NMA_upper_95_CI") %>% rename_if_exists("TE.direct", 
        "Direct_SMD") %>% rename_if_exists("seTE.direct", "Direct_SE") %>% rename_if_exists("lower.direct", 
        "Direct_lower_95_CI") %>% rename_if_exists("upper.direct", "Direct_upper_95_CI") %>% rename_if_exists("TE.indirect", 
        "Indirect_SMD") %>% rename_if_exists("seTE.indirect", "Indirect_SE") %>% rename_if_exists("lower.indirect", 
        "Indirect_lower_95_CI") %>% rename_if_exists("upper.indirect", "Indirect_upper_95_CI") %>% rename_if_exists("statistic", 
        "z_or_test_statistic") %>% rename_if_exists("pval", "p_for_difference") %>% rename_if_exists("p.value", 
        "p_for_difference") %>% rename_if_exists("prop.direct", "proportion_direct_evidence")
    if (all(c("NMA_SMD", "NMA_lower_95_CI", "NMA_upper_95_CI") %in% names(tab))) {
        tab$NMA_result <- make_ci(as.numeric(tab$NMA_SMD), as.numeric(tab$NMA_lower_95_CI), as.numeric(tab$NMA_upper_95_CI))
    }
    if (all(c("Direct_SMD", "Direct_lower_95_CI", "Direct_upper_95_CI") %in% names(tab))) {
        tab$Direct_result <- make_ci(as.numeric(tab$Direct_SMD), as.numeric(tab$Direct_lower_95_CI), 
            as.numeric(tab$Direct_upper_95_CI))
    }
    if (all(c("Indirect_SMD", "Indirect_lower_95_CI", "Indirect_upper_95_CI") %in% names(tab))) {
        tab$Indirect_result <- make_ci(as.numeric(tab$Indirect_SMD), as.numeric(tab$Indirect_lower_95_CI), 
            as.numeric(tab$Indirect_upper_95_CI))
    }
    preferred_order <- c("outcome", "time_window", "comparison", "treat1", "treat2", "k", "proportion_direct_evidence", 
        "NMA_result", "Direct_result", "Indirect_result", "NMA_SMD", "NMA_SE", "NMA_lower_95_CI", "NMA_upper_95_CI", 
        "Direct_SMD", "Direct_SE", "Direct_lower_95_CI", "Direct_upper_95_CI", "Indirect_SMD", "Indirect_SE", 
        "Indirect_lower_95_CI", "Indirect_upper_95_CI", "z_or_test_statistic", "p_for_difference")
    preferred_order <- preferred_order[preferred_order %in% names(tab)]
    other_cols <- setdiff(names(tab), preferred_order)
    tab <- tab[, c(preferred_order, other_cols), drop = FALSE]
    tab
}

make_random_vs_common_table <- function(random_reference_table, common_reference_table, digits = 2) {
    if (nrow(random_reference_table) == 0) {
        return(data.frame(note = "No random-effects reference table available.", stringsAsFactors = FALSE))
    }
    random_tab <- random_reference_table %>% transmute(treatment = as.character(treatment), reference_group = as.character(reference_group), 
        random_SMD = as.numeric(SMD_vs_reference), random_lower_95_CI = as.numeric(lower_95_CI), random_upper_95_CI = as.numeric(upper_95_CI), 
        random_result = make_ci(random_SMD, random_lower_95_CI, random_upper_95_CI, digits = digits), 
        random_interpretation = interpretation)
    common_tab <- common_reference_table %>% transmute(treatment = as.character(treatment), common_SMD = as.numeric(SMD_vs_reference), 
        common_lower_95_CI = as.numeric(lower_95_CI), common_upper_95_CI = as.numeric(upper_95_CI), common_result = make_ci(common_SMD, 
            common_lower_95_CI, common_upper_95_CI, digits = digits), common_interpretation = interpretation)
    out <- random_tab %>% left_join(common_tab, by = "treatment") %>% mutate(random_statistically_clear = (random_lower_95_CI > 
        0 | random_upper_95_CI < 0), common_statistically_clear = (common_lower_95_CI > 0 | common_upper_95_CI < 
        0), direction_consistent = case_when(is.na(random_SMD) | is.na(common_SMD) ~ NA, random_SMD == 
        0 | common_SMD == 0 ~ TRUE, sign(random_SMD) == sign(common_SMD) ~ TRUE, TRUE ~ FALSE), significance_consistent = case_when(is.na(random_statistically_clear) | 
        is.na(common_statistically_clear) ~ NA, random_statistically_clear == common_statistically_clear ~ 
        TRUE, TRUE ~ FALSE), absolute_difference_common_minus_random = common_SMD - random_SMD, robustness_comment = case_when(direction_consistent == 
        TRUE & significance_consistent == TRUE ~ "Robust direction and statistical conclusion", direction_consistent == 
        TRUE & significance_consistent == FALSE ~ "Same direction but different statistical conclusion", 
        direction_consistent == FALSE ~ "Direction changed between models", TRUE ~ "Insufficient information"))
    out
}

make_pairwise_meta_summary <- function(pairwise_data, outcome_name, time_name, digits = 2) {
    required_cols <- c("studlab", "treat1", "treat2", "TE", "seTE")
    if (!all(required_cols %in% names(pairwise_data))) {
        return(data.frame(outcome = outcome_name, time_window = time_name, note = paste0("pairwise_data does not contain required columns: ", 
            paste(required_cols, collapse = ", ")), stringsAsFactors = FALSE))
    }
    dat <- pairwise_data %>% transmute(study_id = as.character(studlab), treat1_raw = as.character(treat1), 
        treat2_raw = as.character(treat2), TE_raw = suppressWarnings(as.numeric(TE)), seTE = suppressWarnings(as.numeric(seTE))) %>% 
        filter(!is.na(study_id), !is.na(treat1_raw), !is.na(treat2_raw), is.finite(TE_raw), is.finite(seTE), 
            seTE > 0) %>% mutate(treatment1 = pmin(treat1_raw, treat2_raw), treatment2 = pmax(treat1_raw, 
        treat2_raw), TE_aligned = if_else(treat1_raw == treatment1 & treat2_raw == treatment2, TE_raw, 
        -TE_raw), comparison = paste(treatment1, "vs", treatment2))
    if (nrow(dat) == 0) {
        return(data.frame(outcome = outcome_name, time_window = time_name, note = "No valid direct pairwise comparisons.", 
            stringsAsFactors = FALSE))
    }
    get_meta_value <- function(meta_object, candidates) {
        for (nm in candidates) {
            value <- tryCatch(meta_object[[nm]], error = function(e) NULL)
            if (!is.null(value) && length(value) >= 1) {
                value <- suppressWarnings(as.numeric(value[1]))
                if (is.finite(value)) {
                  return(value)
                }
            }
        }
        NA_real_
    }
    comparison_list <- split(dat, dat$comparison)
    out <- lapply(comparison_list, function(d) {
        k <- dplyr::n_distinct(d$study_id)
        if (k == 1) {
            TE_common <- d$TE_aligned[1]
            se_common <- d$seTE[1]
            lower_common <- TE_common - 1.96 * se_common
            upper_common <- TE_common + 1.96 * se_common
            return(data.frame(outcome = outcome_name, time_window = time_name, comparison = d$comparison[1], 
                treatment1 = d$treatment1[1], treatment2 = d$treatment2[1], effect_direction = "SMD = treatment1 - treatment2", 
                k = k, studies = paste(unique(d$study_id), collapse = "; "), common_SMD = TE_common, 
                common_SE = se_common, common_lower_95_CI = lower_common, common_upper_95_CI = upper_common, 
                common_result = make_ci(TE_common, lower_common, upper_common, digits = digits), random_SMD = TE_common, 
                random_SE = se_common, random_lower_95_CI = lower_common, random_upper_95_CI = upper_common, 
                random_result = make_ci(TE_common, lower_common, upper_common, digits = digits), tau_squared = NA_real_, 
                I_squared_percent = NA_real_, Q = NA_real_, df_Q = NA_real_, p_Q = NA_real_, note = "Single-study direct comparison; heterogeneity not estimable", 
                stringsAsFactors = FALSE))
        }
        m <- tryCatch(meta::metagen(TE = d$TE_aligned, seTE = d$seTE, studlab = d$study_id, sm = "SMD", 
            common = TRUE, random = TRUE, method.tau = "REML", method.random.ci = "classic"), error = function(e) e)
        if (inherits(m, "error")) {
            return(data.frame(outcome = outcome_name, time_window = time_name, comparison = d$comparison[1], 
                treatment1 = d$treatment1[1], treatment2 = d$treatment2[1], effect_direction = "SMD = treatment1 - treatment2", 
                k = k, studies = paste(unique(d$study_id), collapse = "; "), common_SMD = NA_real_, common_SE = NA_real_, 
                common_lower_95_CI = NA_real_, common_upper_95_CI = NA_real_, common_result = NA_character_, 
                random_SMD = NA_real_, random_SE = NA_real_, random_lower_95_CI = NA_real_, random_upper_95_CI = NA_real_, 
                random_result = NA_character_, tau_squared = NA_real_, I_squared_percent = NA_real_, 
                Q = NA_real_, df_Q = NA_real_, p_Q = NA_real_, note = paste0("Pairwise meta-analysis failed: ", 
                  conditionMessage(m)), stringsAsFactors = FALSE))
        }
        common_TE <- get_meta_value(m, c("TE.common", "TE.fixed"))
        common_SE <- get_meta_value(m, c("seTE.common", "seTE.fixed"))
        common_lower <- get_meta_value(m, c("lower.common", "lower.fixed"))
        common_upper <- get_meta_value(m, c("upper.common", "upper.fixed"))
        random_TE <- get_meta_value(m, c("TE.random"))
        random_SE <- get_meta_value(m, c("seTE.random"))
        random_lower <- get_meta_value(m, c("lower.random"))
        random_upper <- get_meta_value(m, c("upper.random"))
        I2_value <- get_meta_value(m, c("I2"))
        if (!is.na(I2_value) && I2_value <= 1) {
            I2_value <- I2_value * 100
        }
        data.frame(outcome = outcome_name, time_window = time_name, comparison = d$comparison[1], treatment1 = d$treatment1[1], 
            treatment2 = d$treatment2[1], effect_direction = "SMD = treatment1 - treatment2", k = k, 
            studies = paste(unique(d$study_id), collapse = "; "), common_SMD = common_TE, common_SE = common_SE, 
            common_lower_95_CI = common_lower, common_upper_95_CI = common_upper, common_result = make_ci(common_TE, 
                common_lower, common_upper, digits = digits), random_SMD = random_TE, random_SE = random_SE, 
            random_lower_95_CI = random_lower, random_upper_95_CI = random_upper, random_result = make_ci(random_TE, 
                random_lower, random_upper, digits = digits), tau_squared = get_meta_value(m, c("tau2")), 
            I_squared_percent = I2_value, Q = get_meta_value(m, c("Q")), df_Q = get_meta_value(m, c("df.Q")), 
            p_Q = get_meta_value(m, c("pval.Q")), note = "Traditional pairwise meta-analysis of direct evidence", 
            stringsAsFactors = FALSE)
    })
    bind_rows(out) %>% arrange(treatment1, treatment2)
}

make_pairwise_heterogeneity_table <- function(pairwise_meta_summary, outcome_name = NULL, time_name = NULL, 
    digits = 3, i2_format = c("percent", "proportion")) {
    i2_format <- match.arg(i2_format)
    required_cols <- c("comparison", "k", "Q", "p_Q", "tau_squared", "I_squared_percent")
    if (!all(required_cols %in% names(pairwise_meta_summary))) {
        stop("pairwise_meta_summary is missing required columns: ", paste(setdiff(required_cols, names(pairwise_meta_summary)), 
            collapse = ", "))
    }
    out <- pairwise_meta_summary %>% mutate(outcome_label = if (!is.null(outcome_name)) 
        outcome_name
    else as.character(outcome), time_label = if (!is.null(time_name)) 
        time_name
    else as.character(time_window), Q_statistic = as.numeric(Q), P_value = as.numeric(p_Q), tau_squared_value = as.numeric(tau_squared), 
        I_squared_value = as.numeric(I_squared_percent), I_squared_output = if (i2_format == "percent") {
            I_squared_value
        }
        else if (i2_format == "proportion") {
            I_squared_value/100
        }
        else {
            I_squared_value
        }, 
        potential_heterogeneity = case_when(k <= 1 ~ "Not estimable: single-study comparison", !is.na(P_value) & 
            P_value < 0.05 ~ "Yes", !is.na(I_squared_value) & I_squared_value > 50 ~ "Yes", TRUE ~ "No"), 
        heterogeneity_reason = case_when(k <= 1 ~ "Only one direct study; heterogeneity cannot be estimated", 
            !is.na(P_value) & P_value < 0.05 & !is.na(I_squared_value) & I_squared_value > 50 ~ "Q-test p < 0.05 and I\u00b2 > 50%", 
            !is.na(P_value) & P_value < 0.05 ~ "Q-test p < 0.05", !is.na(I_squared_value) & I_squared_value > 
                50 ~ "I\u00b2 > 50%", TRUE ~ "No statistical evidence of heterogeneity")) %>% transmute(Outcome = outcome_label, 
        Time_window = time_label, Comparisons = comparison, k = k, `Q statistic` = if_else(k <= 1, NA_real_, 
            Q_statistic), `P-value` = if_else(k <= 1, NA_real_, P_value), tau_squared = if_else(k <= 1, NA_real_, 
            tau_squared_value), I_squared = if_else(k <= 1, NA_real_, I_squared_output), `Potential heterogeneity` = potential_heterogeneity, 
        Reason = heterogeneity_reason)
    out
}

# -----------------------------------------------------------------------------
# Input workbook discovery and study-level filtering
# -----------------------------------------------------------------------------
normalise_nma_key <- function(x) {
    gsub("[^a-z0-9]+", "", tolower(trimws(as.character(x))))
}

normalise_study_id <- function(x) {
    out <- stringr::str_squish(as.character(x))
    out[is.na(x)] <- NA_character_
    out <- sub("\\.0+$", "", out)
    out
}

make_display_outcome_name <- function(outcome_name) {
    if (length(outcome_name) == 0L || is.na(outcome_name[1])) {
        return(NA_character_)
    }
    outcome_label <- stringr::str_squish(as.character(outcome_name)[1])
    outcome_key <- normalise_nma_key(outcome_label)
    if (outcome_key %in% c("disability", "physicalfunction")) {
        return("Physical function")
    }
    if (outcome_key %in% c("painfearavoidance", "painfearavoindance", "painrelatedfearavoidance")) {
        return("Pain-related fear avoidance")
    }
    if (outcome_key %in% c("qol", "qualityoflife", "healthrelatedqualityoflife")) {
        return("Health-related quality of life")
    }
    if (outcome_key == "painintensity") {
        return("Pain intensity")
    }
    if (outcome_key == "anxiety") {
        return("Anxiety")
    }
    if (outcome_key %in% c("depression", "dpression")) {
        return("Depression")
    }
    if (outcome_key %in% c("selfefficacy", "selfefficacyoutcome")) {
        return("Self-efficacy")
    }
    outcome_label
}

# Quality of life and self-efficacy retain their reported higher-is-better
# direction throughout the analysis. Other outcomes retain the established
# lower-is-better convention.
is_higher_better_outcome <- function(outcome_name) {
    make_display_outcome_name(outcome_name) %in% c(
        "Health-related quality of life",
        "Self-efficacy"
    )
}

small_values_for_outcome <- function(outcome_name) {
    if (is_higher_better_outcome(outcome_name)) "undesirable" else "desirable"
}

direction_convention_for_outcome <- function(outcome_name) {
    if (is_higher_better_outcome(outcome_name)) {
        "Higher values are better; original means and direct SMD signs retained"
    } else {
        "Lower values are better; higher-is-better scales are sign-reversed"
    }
}

canonicalise_requested_outcome <- function(outcome_name) {
    out <- make_display_outcome_name(outcome_name)
    if (is.na(out) || !(out %in% NMA_SEVEN_OUTCOMES)) {
        stop("Unsupported outcome: ", as.character(outcome_name)[1], "\nAllowed outcomes: ", paste(NMA_SEVEN_OUTCOMES, 
            collapse = ", "))
    }
    out
}

locate_nma_working_dir <- function() {
    candidate_dirs <- unique(c(NMA_SCRIPT_DIR, getwd(), file.path(path.expand("~"), "R", "Rworkdir"), 
        file.path(path.expand("~"), "Documents", "R", "Rworkdir"), file.path(path.expand("~"), "Rworkdir")))
    candidate_dirs <- candidate_dirs[!is.na(candidate_dirs) & nzchar(candidate_dirs) & dir.exists(candidate_dirs)]
    for (candidate_dir in candidate_dirs) {
        hits <- file.path(candidate_dir, NMA_MASTER_WORKBOOK_CANDIDATES)
        if (any(file.exists(hits))) {
            return(normalizePath(candidate_dir, winslash = "/", mustWork = TRUE))
        }
    }
    stop("The study-information workbook was not found.\nPlace the R scripts and study-information workbook in the same data directory.\n", 
        "Directories checked: ", paste(candidate_dirs, collapse = ", "))
}

find_master_workbook <- function(working_dir, master_workbook = NULL) {
    working_dir <- normalizePath(working_dir, winslash = "/", mustWork = TRUE)
    if (!is.null(master_workbook) && length(master_workbook) == 1L && !is.na(master_workbook) && nzchar(trimws(as.character(master_workbook)))) {
        requested <- trimws(as.character(master_workbook))
        is_abs <- grepl("^[A-Za-z]:[/\\\\]|^[/\\\\]{2}|^/", requested)
        requested <- if (is_abs) 
            requested
        else file.path(working_dir, requested)
        if (!file.exists(requested)) {
            stop("The specified study-information workbook does not exist: ", requested)
        }
        return(normalizePath(requested, winslash = "/", mustWork = TRUE))
    }
    exact <- file.path(working_dir, NMA_MASTER_WORKBOOK_CANDIDATES)
    exact <- exact[file.exists(exact)]
    if (length(exact) > 0L) {
        return(normalizePath(exact[1], winslash = "/", mustWork = TRUE))
    }
    available <- list.files(working_dir, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
    available <- available[!grepl("^~\\$", basename(available))]
    key <- normalise_nma_key(tools::file_path_sans_ext(basename(available)))
    hit <- which(grepl("yanjiujibenxinxihuizong", key, ignore.case = TRUE))
    if (length(hit) == 1L) {
        return(normalizePath(available[hit], winslash = "/", mustWork = TRUE))
    }
    stop("The study-information workbook could not be found.\nCandidate file names: ", paste(NMA_MASTER_WORKBOOK_CANDIDATES, 
        collapse = ", "), "\nCurrent directory: ", working_dir)
}

resolve_master_outcome_sheet <- function(workbook, outcome_name, arm_sheet = NULL) {
    available <- readxl::excel_sheets(workbook)
    outcome_name <- canonicalise_requested_outcome(outcome_name)
    candidates <- if (!is.null(arm_sheet) && length(arm_sheet) == 1L && !is.na(arm_sheet) && nzchar(trimws(as.character(arm_sheet)))) {
        trimws(as.character(arm_sheet))
    }
    else {
        NMA_MASTER_SHEET_ALIASES[[outcome_name]]
    }
    available_key <- normalise_nma_key(available)
    for (candidate in unique(candidates)) {
        hit <- which(available_key == normalise_nma_key(candidate))
        if (length(hit) > 0L) {
            return(available[hit[1]])
        }
    }
    stop("The outcome sheet could not be found in the study-information workbook: ", outcome_name, "\nTried: ", paste(candidates, 
        collapse = ", "), "\nAvailable sheets: ", paste(available, collapse = ", "))
}

read_arm_study_ids <- function(workbook, arm_sheet) {
    raw <- readxl::read_excel(workbook, sheet = arm_sheet, skip = 2, col_names = FALSE, .name_repair = "minimal")
    if (ncol(raw) < 1L) 
        return(character())
    ids <- normalise_study_id(raw[[1]])
    unique(ids[!is.na(ids) & nzchar(ids)])
}

read_master_smd <- function(workbook, smd_sheet = "SMD") {
    sheets <- readxl::excel_sheets(workbook)
    hit <- which(normalise_nma_key(sheets) == normalise_nma_key(smd_sheet))
    if (length(hit) == 0L) {
        message("No SMD sheet was found in the study-information workbook; only arm-level Mean/SD data will be used.")
        return(NULL)
    }
    readxl::read_excel(workbook, sheet = sheets[hit[1]])
}

filter_reported_smd_ids <- function(reported_smd_data, include_ids = NULL, exclude_ids = NULL, label = "SMD") {
    if (is.null(reported_smd_data) || nrow(reported_smd_data) == 0L) {
        return(reported_smd_data)
    }
    dat <- as.data.frame(reported_smd_data, stringsAsFactors = FALSE)
    keys <- normalise_nma_key(names(dat))
    hit <- which(keys %in% c("studlab", "study", "studyid", "trialid"))
    if (length(hit) == 0L) {
        stop("The SMD sheet has no studlab, study, study_id, or trial_id column.")
    }
    ids <- normalise_study_id(dat[[hit[1]]])
    keep <- rep(TRUE, nrow(dat))
    if (!is.null(include_ids)) {
        include_ids <- unique(normalise_study_id(include_ids))
        keep <- keep & !is.na(ids) & ids %in% include_ids
    }
    if (!is.null(exclude_ids)) {
        exclude_ids <- unique(normalise_study_id(exclude_ids))
        keep <- keep & (is.na(ids) | !(ids %in% exclude_ids))
    }
    message(label, ": retained ", sum(keep), " rows and excluded ", sum(!keep), " rows.")
    dat[keep, , drop = FALSE]
}

prepare_reported_smd_data <- function(reported_smd_data, outcome_name, time_name) {
    if (is.null(reported_smd_data) || nrow(reported_smd_data) == 0L) {
        return(prepare_reported_smd_data_core(reported_smd_data, make_display_outcome_name(outcome_name), 
            time_name))
    }
    dat <- as.data.frame(reported_smd_data, stringsAsFactors = FALSE)
    keys <- normalise_nma_key(names(dat))
    outcome_hit <- which(keys == "outcome")
    if (length(outcome_hit) > 0L) {
        dat[[outcome_hit[1]]] <- vapply(dat[[outcome_hit[1]]], function(x) make_display_outcome_name(x), 
            character(1))
    }
    numeric_keys <- c("te", "sete", "lower95ci", "upper95ci", "n1", "n2")
    for (i in which(keys %in% numeric_keys)) {
        x <- as.character(dat[[i]])
        x <- gsub("[\u2212\u2013\u2014]", "-", x)
        dat[[i]] <- x
    }
    target <- make_display_outcome_name(outcome_name)
    reverse_hit <- which(keys == "reversesign")
    if (is_higher_better_outcome(target)) {
        if (length(reverse_hit) == 0L) {
            dat$reverse_sign <- FALSE
        } else {
            dat[[reverse_hit[1]]] <- FALSE
        }
    }
    prepare_reported_smd_data_core(reported_smd_data = dat, outcome_name = target, time_name = time_name)
}

outcome_stub <- function(outcome_name) {
    outcome_name <- canonicalise_requested_outcome(outcome_name)
    unname(c(
        "Pain intensity" = "pain",
        "Physical function" = "function",
        "Pain-related fear avoidance" = "fear",
        "Health-related quality of life" = "qol",
        "Anxiety" = "anxiety",
        "Depression" = "depression",
        "Self-efficacy" = "selfeff"
    )[outcome_name])
}

time_window_stub <- function(time_window) {
    key <- normalise_nma_key(time_window)
    value <- unname(c(
        postintervention = "post",
        shortterm = "short",
        midterm = "mid",
        longterm = "long"
    )[key])
    if (length(value) == 0L || is.na(value)) {
        stop("Unsupported time window: ", as.character(time_window)[1])
    }
    value
}

analysis_file_stub <- function(outcome_name, time_window) {
    paste(outcome_stub(outcome_name), time_window_stub(time_window), sep = "_")
}

is_no_analysable_data_error <- function(message_text) {
    grepl(
        "No analysable study arms remain after excluding arms with no data for this time window.",
        as.character(message_text)[1],
        fixed = TRUE
    )
}

# -----------------------------------------------------------------------------
# Batch execution across the four prespecified follow-up windows
# -----------------------------------------------------------------------------
run_four_time_windows <- function(outcome_name, working_dir, master_workbook, arm_sheet, output_root, 
    continue_on_error, run_one) {
    outcome_name <- canonicalise_requested_outcome(outcome_name)
    time_windows <- c("post_intervention", "short_term", "mid_term", "long_term")
    if (!dir.exists(output_root)) 
        dir.create(output_root, recursive = TRUE)
    output_root <- normalizePath(output_root, winslash = "/", mustWork = TRUE)
    output_dirs <- setNames(file.path(output_root, vapply(time_windows, function(tt) {
        analysis_file_stub(outcome_name, tt)
    }, character(1))), time_windows)
    results <- setNames(vector("list", length(time_windows)), time_windows)
    errors <- setNames(rep(NA_character_, length(time_windows)), time_windows)
    skipped_no_data <- setNames(rep(FALSE, length(time_windows)), time_windows)
    for (current_time in time_windows) {
        message("\n====================================================", "\nStarting analysis: ", outcome_name, 
            " / ", current_time, "\n====================================================")
        f <- function() run_one(current_time, output_dirs[[current_time]])
        results[[current_time]] <- tryCatch(f(), error = function(e) {
            error_message <- conditionMessage(e)
            errors[[current_time]] <<- error_message
            if (is_no_analysable_data_error(error_message)) {
                skipped_no_data[[current_time]] <<- TRUE
                if (!dir.exists(output_dirs[[current_time]])) {
                    dir.create(output_dirs[[current_time]], recursive = TRUE)
                }
                writeLines(c(
                    paste("Outcome:", outcome_name),
                    paste("Time window:", current_time),
                    "Status: Skipped (no analysable data)",
                    paste("Reason:", error_message)
                ), file.path(output_dirs[[current_time]], "SKIPPED_NO_DATA.txt"))
                message("Skipped empty time window: ", outcome_name, " / ", current_time)
                return(structure(list(
                    analysis_status = "skipped_no_data",
                    outcome = outcome_name,
                    time_window = current_time,
                    reason = error_message
                ), class = "nma_skipped_no_data"))
            }
            if (!isTRUE(continue_on_error)) {
                stop(e)
            }
            message("This time window did not complete: ", current_time, "\nReason: ", error_message)
            NULL
        })
    }
    status <- vapply(time_windows, function(tt) {
        if (isTRUE(skipped_no_data[[tt]]))
            return("Skipped (no data)")
        if (!is.na(errors[[tt]]))
            return("Failed")
        rr <- results[[tt]]
        if (is.list(rr) && identical(rr$analysis_status, "network_plot_only_disconnected")) {
            return("Network plot only (disconnected)")
        }
        "Completed"
    }, character(1))
    status_table <- data.frame(outcome = outcome_name, time_window = time_windows, status = unname(status), 
        output_directory = unname(output_dirs), error_message = unname(errors), stringsAsFactors = FALSE)
    cat("\nStatus for the four time windows:\n")
    print(status_table, row.names = FALSE)
    attr(results, "status") <- status_table
    invisible(results)
}

