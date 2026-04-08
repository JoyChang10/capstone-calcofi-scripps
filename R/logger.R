suppressPackageStartupMessages({
  library(logger)
})

# initialize logger
init_logger <- function() {
  log_appender(appender_console)
  log_layout(layout_glue_colors)
  log_threshold(INFO)
}

log_pipeline_start <- function() {
  log_info("🚀 Pipeline started at {Sys.time()}")
}

log_pipeline_end <- function() {
  log_info("✅ Pipeline finished at {Sys.time()}")
}

log_step_start <- function(step_name) {
  log_info("➡️ Starting step: {step_name}")
}

log_step_success <- function(step_name) {
  log_info("✅ Step success: {step_name}")
}

log_step_warning <- function(step_name, msg) {
  log_warn("⚠️ Step warning: {step_name} - {msg}")
}

log_step_error <- function(step_name, msg) {
  log_error("❌ Step failed: {step_name} - {msg}")
}

log_info_msg <- function(msg) {
  log_info("{msg}")
}

log_error_msg <- function(msg) {
  log_error("{msg}")
}