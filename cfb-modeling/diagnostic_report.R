# ============================================================================
# DATA INVESTIGATION - Check what's actually being loaded
# ============================================================================

library(cfbfastR)
library(tidyverse)

# Set API key
cfbd_key <- Sys.getenv("CFB_API_KEY")
if(cfbd_key == "") {
  cfbd_key <- Sys.getenv("CFBD_API_KEY")
}
Sys.setenv(CFBD_API_KEY = cfbd_key)

cat("\n")
cat(strrep("=", 80), "\n")
cat("DATA INVESTIGATION REPORT\n")
cat(strrep("=", 80), "\n\n")

# Test loading betting lines directly
cat("Testing BETTING LINES data load:\n")
cat(strrep("-", 80), "\n\n")

for(year in 2023:2025) {
  cat(sprintf("Year %d:\n", year))
  
  tryCatch({
    lines <- cfbd_betting_lines(year = year, season_type = "regular")
    
    if(!is.null(lines) && nrow(lines) > 0) {
      cat(sprintf("  ✓ Loaded %d betting lines\n", nrow(lines)))
      cat(sprintf("  Columns: %s\n", paste(head(names(lines), 10), collapse = ", ")))
      
      # Show sample
      sample <- lines %>% 
        head(3) %>% 
        select(game_id, season, week, home_team, away_team, spread, over_under)
      cat("  Sample:\n")
      print(sample)
    } else {
      cat("  ✗ No data returned\n")
    }
    
  }, error = function(e) {
    cat(sprintf("  ✗ Error: %s\n", e$message))
  })
  
  cat("\n")
}

# Test game info
cat("\nTesting GAME INFO data load:\n")
cat(strrep("-", 80), "\n\n")

for(year in 2023:2025) {
  cat(sprintf("Year %d:\n", year))
  
  tryCatch({
    games <- cfbd_game_info(year = year, season_type = "regular")
    
    if(!is.null(games) && nrow(games) > 0) {
      cat(sprintf("  ✓ Loaded %d games\n", nrow(games)))
      
      # Check for scores
      with_scores <- games %>% filter(!is.na(home_points) & !is.na(away_points))
      cat(sprintf("  Games with scores: %d\n", nrow(with_scores)))
    } else {
      cat("  ✗ No data returned\n")
    }
    
  }, error = function(e) {
    cat(sprintf("  ✗ Error: %s\n", e$message))
  })
  
  cat("\n")
}

# Check what's in the cache
cat("\nChecking CACHED DATA:\n")
cat(strrep("-", 80), "\n\n")

cache_files <- list.files("data_cache", pattern = "\\.rds$", full.names = TRUE)

for(file in cache_files) {
  cat(sprintf("%s:\n", basename(file)))
  
  tryCatch({
    data <- readRDS(file)
    
    if(!is.null(data) && nrow(data) > 0) {
      cat(sprintf("  Rows: %d\n", nrow(data)))
      
      if("season" %in% names(data)) {
        years <- unique(data$season)
        cat(sprintf("  Years: %s\n", paste(years, collapse = ", ")))
      } else if("year" %in% names(data)) {
        years <- unique(data$year)
        cat(sprintf("  Years: %s\n", paste(years, collapse = ", ")))
      }
    }
  }, error = function(e) {
    cat(sprintf("  Error reading: %s\n", e$message))
  })
  
  cat("\n")
}

cat(strrep("=", 80), "\n")
cat("DIAGNOSIS:\n")
cat(strrep("=", 80), "\n\n")

cat("If betting lines show 0 rows for 2024-2025:\n")
cat("  → The cache is old and needs to be refreshed\n")
cat("  → Delete the data_cache folder and re-run the model\n\n")

cat("If game info shows games but no scores:\n")
cat("  → The season is scheduled but not yet played\n")
cat("  → This shouldn't happen since we're in 2026\n\n")

cat("RECOMMENDED FIX:\n")
cat("  1. Delete the data_cache folder\n")
cat("  2. Re-run the ultimate_cfb_model.R script\n")
cat("  3. This will force fresh data download from API\n\n")

cat(strrep("=", 80), "\n\n")