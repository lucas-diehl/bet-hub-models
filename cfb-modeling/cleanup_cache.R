# ============================================================================
# CLEANUP SCRIPT - Run this BEFORE running the main model
# ============================================================================

cat("\nDeleting old cache files...\n")

# Delete all RDS cache files
cache_files <- list.files("data_cache", pattern = "\\.rds$", full.names = TRUE)

if(length(cache_files) > 0) {
  for(file in cache_files) {
    unlink(file)
    cat(sprintf("  Deleted: %s\n", basename(file)))
  }
  cat(sprintf("\n✓ Deleted %d cache files\n\n", length(cache_files)))
} else {
  cat("  No cache files found\n\n")
}

cat("Now run: source('ultimate_cfb_model.R')\n\n")