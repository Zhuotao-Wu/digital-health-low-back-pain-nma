# Portable one-click entry point.
# Open this project folder and source this file. All paths are resolved relative
# to the folder containing RUN_ANALYSIS.R; no setwd() or computer-specific path
# is required.

detect_run_directory <- function() {
    source_files <- unlist(
        lapply(sys.frames(), function(frame) {
            if (is.null(frame$ofile)) character() else as.character(frame$ofile[[1]])
        }),
        use.names = FALSE
    )
    source_files <- source_files[file.exists(source_files)]
    if (length(source_files) > 0L) {
        return(dirname(normalizePath(tail(source_files, 1L), winslash = "/", mustWork = TRUE)))
    }
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

NMA_RUN_DIR <- detect_run_directory()
NMA_FUNCTIONS_DIR <- file.path(NMA_RUN_DIR, "functions")

if (!dir.exists(NMA_FUNCTIONS_DIR)) {
    stop(
        "Workflow directory not found: ", NMA_FUNCTIONS_DIR,
        "\nKeep the functions folder beside RUN_ANALYSIS.R.",
        call. = FALSE
    )
}

assert_portable_output_path <- function(output_dir) {
    candidate <- file.path(
        normalizePath(output_dir, winslash = "/", mustWork = TRUE),
        "sens_rob", "selfeff_long", "selfeff_long_network.svg"
    )
    if (.Platform$OS.type == "windows" && nchar(candidate, type = "chars") > 240L) {
        stop(
            "The output path is too long. Move the project closer to the drive root, ",
            "for example D:/R/NMA_analysis.",
            call. = FALSE
        )
    }
    invisible(output_dir)
}

run_all_nma_analyses <- function(
    data_dir = file.path(NMA_RUN_DIR, "data"),
    output_dir = file.path(NMA_RUN_DIR, "results"),
    master_workbook = NULL,
    analyses = c("main", "excluding_high_risk", "mean_sd_only", "subgroup"),
    continue_on_error = TRUE
) {
    data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
    assert_portable_output_path(output_dir)

    workflow_plan <- list(
        main = list(
            script = "NMA_main.R",
            entry = "run_main_analysis",
            output = "main"
        ),
        excluding_high_risk = list(
            script = "NMA_sensitivity_excluding_high_risk.R",
            entry = "run_high_risk_sensitivity_analysis",
            output = "sens_rob"
        ),
        mean_sd_only = list(
            script = "NMA_sensitivity_mean_sd_only.R",
            entry = "run_mean_sd_sensitivity_analysis",
            output = "sens_meansd"
        ),
        subgroup = list(
            script = "NMA_subgroup.R",
            entry = "run_subgroup_analysis",
            output = "subgroup"
        )
    )

    analyses <- unique(as.character(analyses))
    unknown <- setdiff(analyses, names(workflow_plan))
    if (length(unknown) > 0L) {
        stop(
            "Unknown analyses: ", paste(unknown, collapse = ", "),
            ". Allowed values are: ", paste(names(workflow_plan), collapse = ", "),
            call. = FALSE
        )
    }

    results <- setNames(vector("list", length(analyses)), analyses)
    status <- data.frame(
        analysis = analyses,
        status = "pending",
        message = "",
        stringsAsFactors = FALSE
    )

    for (i in seq_along(analyses)) {
        analysis_name <- analyses[i]
        specification <- workflow_plan[[analysis_name]]
        script_file <- file.path(NMA_FUNCTIONS_DIR, specification$script)
        if (!file.exists(script_file)) {
            stop("Workflow script not found: ", script_file, call. = FALSE)
        }

        message("\nRunning workflow: ", analysis_name)
        workflow_environment <- new.env(parent = globalenv())
        run_result <- tryCatch({
            source(script_file, local = workflow_environment, encoding = "UTF-8")
            entry_function <- workflow_environment[[specification$entry]]
            if (!is.function(entry_function)) {
                stop("Entry function not found: ", specification$entry)
            }
            arguments <- list(
                working_dir = data_dir,
                output_root = file.path(output_dir, specification$output),
                continue_on_error = continue_on_error
            )
            if (!is.null(master_workbook)) arguments$master_workbook <- master_workbook
            value <- do.call(entry_function, arguments)
            status$status[i] <- "completed"
            value
        }, error = function(e) {
            status$status[i] <- "failed"
            status$message[i] <- conditionMessage(e)
            if (!isTRUE(continue_on_error)) stop(e)
            message("Workflow failed: ", analysis_name, "\nReason: ", conditionMessage(e))
            structure(list(error = conditionMessage(e)), class = "nma_workflow_error")
        })
        results[[analysis_name]] <- run_result
    }

    writeLines(capture.output(utils::sessionInfo()), file.path(output_dir, "sessionInfo.txt"))
    message("\nUnified run finished. Results: ", output_dir)
    invisible(list(results = results, status = status))
}

nma_results <- run_all_nma_analyses(
    data_dir = file.path(NMA_RUN_DIR, "data"),
    output_dir = file.path(NMA_RUN_DIR, "results"),
    continue_on_error = TRUE
)

message(
    "\nAnalysis run finished.\n",
    "Results: ", file.path(NMA_RUN_DIR, "results")
)
