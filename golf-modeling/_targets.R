# _targets.R
library(targets)
library(data.table)

tar_option_set(
  packages = c("httr2", "data.table", "tidymodels", "rsample")
)

# -----------------------------
# Source project functions
# -----------------------------
source("dg_pull/dg_api.R")
source("dg_pull/dg_pull_raw.R")
source("dg_pull/dg_pull_event_list.R")
source("dg_pull/dg_pull_event_rounds.R")
source("dg_pull/dg_pull_odds.R")
source("dg_pull/dg_pull_betting_tools.R")

source("R/dg_safe.R")
source("R/build_labels.R")        # patched robustly earlier
source("R/reshape_rounds.R")      # patched robustly earlier
source("R/features_sg.R")
source("R/asof_join.R")

source("R/splits_year.R")
source("R/model_top20.R")         # patched safe-first earlier
source("R/platt_cv.R")            # patched robust fold fallback earlier

source("R/dk_edges_top20.R")      # patched to not hard-stop on 0 rows (recommended)
source("R/sim_top20.R")           # recommended patch: return empty if missing cols
source("R/sim_grid.R")
source("R/metrics_top20.R")
source("R/bankroll_sim.R")
source("R/live_helpers.R")
# -----------------------------
# Targets
# -----------------------------
list(
  # ---------------------------
  # Config
  # ---------------------------
  tar_target(season_range, 2015:2025),
  tar_target(train_years, 2015:2023),
  tar_target(test_years_hist, 2024:2024),
  tar_target(season_2025, 2025),
  
  # ---------------------------
  # Historical event index (event_id + start_date + year)
  # ---------------------------
  tar_target(
    pga_event_list_raw,
    dg_event_list(tour = "pga")
  ),
  
  tar_target(
    pga_event_index,
    {
      dt <- as.data.table(pga_event_list_raw)
      
      if (!("event_id" %in% names(dt))) stop("event-list missing event_id column")
      dt[, event_id := as.character(event_id)]
      
      date_cands <- c("start_date", "event_date", "date", "start", "start_dt", "tournament_start_date")
      date_col <- intersect(date_cands, names(dt))[1]
      
      if (is.na(date_col) || is.null(date_col)) {
        dt[, start_date := as.IDate(NA)]
      } else {
        dt[, start_date := as.IDate(get(date_col))]
      }
      
      # Optional season/year filter if present; else filter by start_date year
      if ("season" %in% names(dt)) dt[, season := as.integer(season)]
      if (!("season" %in% names(dt)) && "year" %in% names(dt)) dt[, season := as.integer(year)]
      
      if ("season" %in% names(dt)) {
        dt <- dt[season %in% season_range]
      } else {
        dt[, yr_tmp := as.integer(format(as.Date(start_date), "%Y"))]
        dt <- dt[yr_tmp %in% season_range]
        dt[, yr_tmp := NULL]
      }
      
      dt[, year := as.integer(format(as.Date(start_date), "%Y"))]
      dt <- dt[!is.na(event_id) & !is.na(year) & !is.na(start_date)]
      
      unique(dt[, .(event_id, start_date, year)])[order(start_date)]
    }
  ),
  
  tar_target(
    pga_event_params,
    {
      ev <- copy(pga_event_index)
      ev[, event_id := trimws(as.character(event_id))]
      ev[, year := as.integer(year)]
      ev[, start_date := as.Date(start_date)]
      ev <- ev[!is.na(event_id) & event_id != "" & !is.na(year)]
      unique(ev[, .(event_id, year, start_date)])[order(start_date)]
    }
  ),
  tar_target(
    pga_event_param_idx,
    seq_len(nrow(pga_event_params))
  ),
  #----------------------------
  # Pull historical rounds WIDE (event-by-event; YEAR REQUIRED)
  # ---------------------------
  tar_target(
    rounds_pga_by_event,
    {
      eid <- pga_event_params$event_id[pga_event_param_idx]
      yr  <- pga_event_params$year[pga_event_param_idx]
      
      dg_rounds_event(
        tour = "pga",
        event_id = eid,
        year = yr,
        sleep_sec = 0.8
      )
    },
    pattern = map(pga_event_param_idx),
    iteration = "list"
  ),
  tar_target(
    rounds_hist_all_wide,
    {
      lst <- rounds_pga_by_event
      lst <- lst[!vapply(lst, is.null, logical(1))]
      if (length(lst) == 0) stop("rounds_hist_all_wide: all event pulls failed (all NULL).")
      rbindlist(lst, use.names = TRUE, fill = TRUE)
    }
  ),
  # ---------------------------
  # Labels for historical seasons (Top20, finish_pos)
  # ---------------------------
  tar_target(
    results_all,
    build_results_from_wide_scores(rounds_hist_all_wide)
  ),
  
  tar_target(
    field_size_all,
    results_all[, .(field_size = .N), by = event_id]
  ),
  
  # ---------------------------
  # Events table for historical seasons (minimal + robust)
  # ---------------------------
  tar_target(
    events_all_min,
    {
      ev <- copy(pga_event_index)
      ev[, start_date := as.Date(start_date)]
      ev <- ev[!is.na(event_id) & !is.na(start_date)]
      ev <- ev[, .(start_date = min(start_date), year = min(year, na.rm = TRUE)), by = event_id]
      
      ev[, `:=`(
        sched_tour = "pga",
        is_alt = 0L,
        event_name = NA_character_,
        major_flag = 0L
      )]
      
      ev[field_size_all, on = "event_id", field_size := i.field_size]
      ev[, field_size := as.integer(field_size)]
      ev[]
    }
  ),
  
  # ---------------------------
  # Historical rounds LONG + rollups
  # ---------------------------
  tar_target(
    rounds_long_all,
    reshape_wide_scores_to_long(rounds_hist_all_wide, events_all_min)
  ),
  
  tar_target(
    rounds_feat_long_all,
    add_sg_rollups_long(rounds_long_all)
  ),
  
  # ---------------------------
  # Historical event-player roster + as-of join
  # ---------------------------
  tar_target(
    event_players_all,
    build_event_player_table(results_all, events_all_min)
  ),
  
  tar_target(
    features_by_event_all,
    join_asof_features(rounds_feat_long_all, event_players_all)
  ),
  
  # ---------------------------
  # Historical modeling table (features + labels + event context)
  # ---------------------------
  tar_target(
    model_table_all,
    {
      dt <- merge(
        features_by_event_all,
        results_all[, .(event_id, player_id, top20, finish_pos)],
        by = c("event_id", "player_id")
      )
      
      dt2 <- merge(
        dt,
        events_all_min[, .(event_id, start_date, year, field_size, sched_tour, is_alt, major_flag, event_name)],
        by = "event_id",
        all.x = TRUE,
        sort = FALSE
      )
      # ---- Field-relative normalization ----
      setDT(dt2)
      
      # event-level stats
      dt2[, field_mean_sg24 := mean(sg24, na.rm = TRUE), by = event_id]
      dt2[, field_sd_sg24   := sd(sg24, na.rm = TRUE), by = event_id]
      
      dt2[, field_mean_sg100 := mean(sg100, na.rm = TRUE), by = event_id]
      dt2[, field_sd_sg100   := sd(sg100, na.rm = TRUE), by = event_id]
      
      # z-scores
      dt2[, z_sg24  := (sg24  - field_mean_sg24)  / field_sd_sg24]
      dt2[, z_sg100 := (sg100 - field_mean_sg100) / field_sd_sg100]
      
      # percentile rank
      dt2[, pct_sg24  := frank(sg24,  ties.method="average") / .N, by = event_id]
      dt2[, pct_sg100 := frank(sg100, ties.method="average") / .N, by = event_id]
      
      # delta vs field
      dt2[, delta_sg24  := sg24  - field_mean_sg24]
      dt2[, delta_sg100 := sg100 - field_mean_sg100]
      
      # event base rate anchor
      dt2[, event_base_rate := 20 / field_size]
      dt2[]
    }
  ),
  
  # ---------------------------
  # Split historical events by year: train 2015-2023, test_hist 2024
  # ---------------------------
  tar_target(
    events_split_hist,
    split_events_by_year(events_all_min, train_years = train_years, test_years = test_years_hist)
  ),
  
  tar_target(
    model_table_labeled,
    {
      dt <- copy(model_table_all)
      dt[, event_id := as.character(event_id)]
      
      sp <- as.data.table(events_split_hist)
      sp[, event_id := as.character(event_id)]
      sp <- unique(sp[, .(event_id, split)], by = "event_id")
      
      out <- merge(dt, sp, by = "event_id", all.x = TRUE, sort = FALSE)
      out <- out[split %in% c("train", "test")]
      
      # ensure start_date exists (fallback to asof_date)
      if (!("start_date" %in% names(out))) {
        if ("asof_date" %in% names(out)) out[, start_date := as.Date(asof_date)] else out[, start_date := as.Date(NA)]
      }
      if (!("year" %in% names(out))) out[, year := as.integer(format(as.Date(start_date), "%Y"))]
      
      out[, `:=`(
        split = as.character(split),
        start_date = as.Date(start_date),
        year = as.integer(year),
        field_size = as.integer(field_size),
        is_alt = as.integer(is_alt),
        major_flag = as.integer(major_flag)
      )]
      out[]
    }
  ),
  
  # ---------------------------
  # ---- 2025 TEST BLOCK (schedule IDs) ----
  # Pull schedule + rounds + labels + features using schedule event_id namespace.
  # ---------------------------
  tar_target(
    schedule_pga_2025,
    dg_schedule("pga", season_2025)
  ),
  
  tar_target(
    pga_events_2025,
    {
      s <- as.data.table(schedule_pga_2025)
      if (!("event_id" %in% names(s))) {
        id_col <- intersect(names(s), c("id", "eventId", "event_key", "dg_event_id"))[1]
        if (is.na(id_col) || is.null(id_col)) stop("schedule_pga_2025: could not find event id column")
        setnames(s, id_col, "event_id")
      }
      s[, event_id := as.character(event_id)]
      unique(s[!is.na(event_id), event_id])
    }
  ),
  
  tar_target(
    rounds_pga_2025_events,
    dg_rounds_event_safe(tour = "pga", event_id = pga_events_2025, year = season_2025, sleep_sec = 0.6),
    pattern = map(pga_events_2025),
    iteration = "list"
  ),
  
  tar_target(
    rounds_pga_2025_wide,
    {
      lst <- rounds_pga_2025_events
      lst <- lst[!vapply(lst, is.null, logical(1))]
      data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)
    }
  ),
  
  tar_target(
    results_2025,
    build_results_from_wide_scores(rounds_pga_2025_wide)
  ),
  
  tar_target(
    field_size_2025,
    results_2025[, .(field_size = .N), by = event_id]
  ),
  
  tar_target(
    events_2025_min,
    {
      s <- as.data.table(schedule_pga_2025)
      
      if (!("event_id" %in% names(s))) {
        id_col <- intersect(names(s), c("id", "eventId", "event_key", "dg_event_id"))[1]
        if (is.na(id_col) || is.null(id_col)) stop("schedule_pga_2025: could not find event id column")
        setnames(s, id_col, "event_id")
      }
      if (!("start_date" %in% names(s))) {
        dt_col <- intersect(names(s), c("start_date", "date", "event_start_date", "start"))[1]
        if (is.na(dt_col) || is.null(dt_col)) stop("schedule_pga_2025: could not find start date column")
        setnames(s, dt_col, "start_date")
      }
      
      s[, event_id := as.character(event_id)]
      s[, start_date := as.Date(start_date)]
      
      ev <- unique(s[!is.na(event_id) & !is.na(start_date), .(event_id, start_date)])
      ev[, year := 2025L]
      ev[, `:=`(sched_tour = "pga", is_alt = 0L, major_flag = 0L, event_name = NA_character_)]
      
      ev[field_size_2025, on = "event_id", field_size := i.field_size]
      ev[, field_size := as.integer(field_size)]
      ev[]
    }
  ),
  
  tar_target(
    rounds_long_2025,
    reshape_wide_scores_to_long(rounds_pga_2025_wide, events_2025_min)
  ),
  
  tar_target(
    rounds_feat_long_2025,
    add_sg_rollups_long(rounds_long_2025)
  ),
  
  tar_target(
    rounds_feat_long_all_plus_2025,
    rbindlist(list(rounds_feat_long_all, rounds_feat_long_2025), use.names = TRUE, fill = TRUE)
  ),
  
  tar_target(
    event_players_2025,
    build_event_player_table(results_2025, events_2025_min)
  ),
  
  tar_target(
    features_by_event_2025,
    join_asof_features(rounds_feat_long_all_plus_2025, event_players_2025)
  ),
  
  tar_target(
    model_table_2025,
    {
      dt <- merge(
        features_by_event_2025,
        results_2025[, .(event_id, player_id, top20, finish_pos)],
        by = c("event_id", "player_id")
      )
      
      dt <- merge(
        dt,
        events_2025_min[, .(event_id, start_date, year, field_size, sched_tour, is_alt, major_flag, event_name)],
        by = "event_id",
        all.x = TRUE,
        sort = FALSE
      )
      # ---- Field-relative normalization (match historical) ----
      setDT(dt)
      
      dt[, field_mean_sg24 := mean(sg24, na.rm = TRUE), by = event_id]
      dt[, field_sd_sg24   := sd(sg24, na.rm = TRUE), by = event_id]
      dt[, field_mean_sg100 := mean(sg100, na.rm = TRUE), by = event_id]
      dt[, field_sd_sg100   := sd(sg100, na.rm = TRUE), by = event_id]
      
      # protect against sd==0
      dt[field_sd_sg24 == 0 | is.na(field_sd_sg24), field_sd_sg24 := NA_real_]
      dt[field_sd_sg100 == 0 | is.na(field_sd_sg100), field_sd_sg100 := NA_real_]
      
      dt[, z_sg24  := (sg24  - field_mean_sg24)  / field_sd_sg24]
      dt[, z_sg100 := (sg100 - field_mean_sg100) / field_sd_sg100]
      
      dt[, pct_sg24  := frank(sg24,  ties.method="average") / .N, by = event_id]
      dt[, pct_sg100 := frank(sg100, ties.method="average") / .N, by = event_id]
      
      dt[, delta_sg24  := sg24  - field_mean_sg24]
      dt[, delta_sg100 := sg100 - field_mean_sg100]
      
      # event base rate anchor (guard missing field_size)
      dt[, event_base_rate := fifelse(!is.na(field_size) & field_size > 0, 20 / field_size, NA_real_)]
      dt[, split := "test"]
      dt[]
    }
  ),
  
  # ---------------------------
  # Train/test sets (train: 2015-2023, test: 2024 + 2025)
  # ---------------------------
  tar_target(
    top20_sets,
    {
      dt_hist <- copy(model_table_labeled) # train + 2024 test
      tr <- dt_hist[split == "train"]
      te_2024 <- dt_hist[split == "test"]
      te_2025 <- copy(model_table_2025)
      
      list(
        train = tr,
        test  = rbindlist(list(te_2024, te_2025), use.names = TRUE, fill = TRUE)
      )
    }
  ),
  
  # Diagnostics (optional but useful)
  tar_target(
    top20_train_feature_coverage,
    {
      tr <- copy(top20_sets$train)
      cand <- c("sg12","sg24","sg50","sg100","sg_sd24",
                "sg_app_24","sg_ott_24","sg_arg_24","sg_putt_24",
                "field_size","is_alt","major_flag")
      cand <- intersect(cand, names(tr))
      data.table(
        feature = cand,
        pct_non_na = vapply(cand, function(x) mean(!is.na(tr[[x]])), numeric(1))
      )[order(pct_non_na)]
    }
  ),
  
  # ---------------------------
  # Fit model + Platt calibration (CV within train)
  # ---------------------------
  tar_target(
    top20_wf,
    fit_top20_glmnet(top20_sets$train)
  ),
  
  # ---------------------------
  # Predict test set (robust to dummy baseline)
  # ---------------------------
  tar_target(
    top20_pred_test_all,
    {
      te <- copy(top20_sets$test)
      
      # safe to always include dummy
      if (!("dummy" %in% names(te))) te[, dummy := 1]
      
      prob_tbl <- predict(top20_wf, new_data = te, type = "prob")
      p_raw <- if (".pred_1" %in% names(prob_tbl)) prob_tbl[[".pred_1"]] else prob_tbl[[ncol(prob_tbl)]]
      
      te[, p_raw := as.numeric(p_raw)]
      te[, p_top20 := p_raw]
      te[]
    }
  ),
  
  tar_target(top20_pred_2024, top20_pred_test_all[year == 2024]),
  tar_target(top20_pred_2025, top20_pred_test_all[year == 2025]),
  
  # ---------------------------
  # Metrics per season (probability quality)
  # ---------------------------
  tar_target(top20_metrics_2024, top20_prob_metrics(top20_pred_2024[, .(top20, p_top20)])),
  tar_target(top20_metrics_2025, top20_prob_metrics(top20_pred_2025[, .(top20, p_top20)])),
  
  # ---------------------------
  # Odds pulls (DraftKings top20) for 2024 & 2025
  # ---------------------------
  tar_target(
    dk_top20_odds_2024,
    dg_odds_outrights(
      year = 2024, market = "top_20",
      tour = "pga", book = "draftkings",
      event_id = "all", odds_format = "american"
    )
  ),
  
  tar_target(
    dk_top20_odds_2025,
    dg_odds_outrights(
      year = 2025, market = "top_20",
      tour = "pga", book = "draftkings",
      event_id = "all", odds_format = "american"
    )
  ),
  
  # ---------------------------
  # Join preds to DK odds
  # ---------------------------
  tar_target(
    dk_join_2024,
    {
      pred <- top20_pred_2024[, .(event_id, player_id, p_top20 = p_raw)]
      join_dk_open_close(pred, dk_top20_odds_2024)
    }
  ),
  
  tar_target(
    dk_join_2025,
    {
      pred <- top20_pred_2025[, .(event_id, player_id, p_top20 = p_raw)]
      join_dk_open_close(pred, dk_top20_odds_2025)
    }
  ),
  
  # ---------------------------
  # Bets + grading + summaries (safe if empty)
  # ---------------------------
  tar_target(
    bets_2024,
    select_bets_top20(
      dk_join_2024,
      ev_col = "ev_blend_per_1",
      ev_min = 0.00,
      max_bets_per_event = 3,
      odds_min = -500,
      odds_max = 1000
    )
  ),
  
  tar_target(
    bets_2025,
    select_bets_top20(
      dk_join_2025,
      ev_col = "ev_blend_per_1",
      ev_min = 0.00,
      max_bets_per_event = 3,
      odds_min = -500,
      odds_max = 1000
    )
  ),
  
  tar_target(
    graded_bets_2024,
    grade_bets_flat(bets_2024, top20_pred_2024[, .(event_id, player_id, top20)])
  ),
  
  tar_target(
    graded_bets_2025,
    grade_bets_flat(bets_2025, top20_pred_2025[, .(event_id, player_id, top20)])
  ),
  
  tar_target(bets_summary_2024, summarize_bets(graded_bets_2024)),
  tar_target(bets_summary_2025, summarize_bets(graded_bets_2025)),
  
  # Optional sweeps
  tar_target(
    sweep_2024,
    sweep_rules(
      dk_join_2024,
      top20_pred_2024[, .(event_id, player_id, top20)],
      ev_col = "ev_blend_per_1",
      ev_grid = c(0.00, 0.01, 0.02, 0.03),
      cap_grid = c(1, 2, 3),
      odds_min = -500,
      odds_max = 1000
    )
  ),
  
  tar_target(
    sweep_2025,
    sweep_rules(
      dk_join_2025,
      top20_pred_2025[, .(event_id, player_id, top20)],
      ev_col = "ev_blend_per_1",
      ev_grid = c(0.00, 0.01, 0.02, 0.03),
      cap_grid = c(1, 2, 3),
      odds_min = -500,
      odds_max = 1000
    )
  ),
  tar_target(
    dk_devig_check_2025,
    {
      dt <- data.table::as.data.table(dk_join_2025)
      
      if (nrow(dt) == 0) {
        return(data.table::data.table(
          event_id = character(),
          n = integer(),
          sum_open_imp = numeric(),
          sum_open_devig = numeric(),
          sum_close_devig = numeric()
        ))
      }
      
      dt[, .(
        n = .N,
        sum_open_imp = sum(p_open_imp, na.rm = TRUE),
        sum_open_devig = sum(p_open_devig, na.rm = TRUE),
        sum_close_devig = sum(p_close_devig, na.rm = TRUE)
      ), by = event_id][order(-abs(sum_open_devig - 20))]
    }
  ),
  tar_target(
    sim_2024_flat,
    simulate_bankroll(graded_bets_2024, stake_mode = "flat", flat_stake = 1, bankroll0 = 100)
  ),
  tar_target(
    sim_2025_flat,
    simulate_bankroll(graded_bets_2025, stake_mode = "flat", flat_stake = 1, bankroll0 = 100)
  ),
  
  tar_target(
    sim_2024_kelly25,
    simulate_bankroll(graded_bets_2024, stake_mode = "kelly", flat_stake = 100, kelly_mult = 0.25, p_col = "p_blend", bankroll0 = 100)
  ),
  tar_target(
    sim_2025_kelly25,
    simulate_bankroll(graded_bets_2025, stake_mode = "kelly", flat_stake = 100, kelly_mult = 0.25, p_col = "p_blend", bankroll0 = 100)
  ),
  
  tar_target(sim_summary_2024, rbindlist(list(sim_2024_flat$summary, sim_2024_kelly25$summary), fill = TRUE)),
  tar_target(sim_summary_2025, rbindlist(list(sim_2025_flat$summary, sim_2025_kelly25$summary), fill = TRUE)),
  
  tar_target(season_2026, 2026),
  
  tar_target(
    schedule_pga_2026,
    dg_schedule("pga", season_2026)
  ),
  
  tar_target(
    events_2026_min,
    {
      s <- as.data.table(schedule_pga_2026)
      
      if (!("event_id" %in% names(s))) {
        id_col <- intersect(names(s), c("id", "eventId", "event_key", "dg_event_id"))[1]
        if (is.na(id_col) || is.null(id_col)) stop("schedule_pga_2026: could not find event id column")
        setnames(s, id_col, "event_id")
      }
      if (!("start_date" %in% names(s))) {
        dt_col <- intersect(names(s), c("start_date", "date", "event_start_date", "start"))[1]
        if (is.na(dt_col) || is.null(dt_col)) stop("schedule_pga_2026: could not find start date column")
        setnames(s, dt_col, "start_date")
      }
      
      s[, event_id := as.character(event_id)]
      s[, start_date := as.Date(start_date)]
      
      ev <- unique(s[!is.na(event_id) & !is.na(start_date), .(event_id, start_date)])
      ev[, year := 2026L]
      ev[, `:=`(sched_tour = "pga", is_alt = 0L, major_flag = 0L, event_name = NA_character_)]
      ev[, field_size := as.integer(NA)]
      ev[]
    }
  ),
  
  # "today" as pipeline-stable value (you can change to Sys.Date() if you want non-deterministic)
  tar_target(run_date, as.Date("2026-02-25")),
  
  tar_target(
    pga_events_2026_past_ids,
    events_2026_min[start_date < run_date, event_id]
  ),
  
  tar_target(
    rounds_pga_2026_past_events,
    dg_rounds_event_safe(tour = "pga", event_id = pga_events_2026_past_ids, year = season_2026, sleep_sec = 0.6),
    pattern = map(pga_events_2026_past_ids),
    iteration = "list"
  ),
  
  tar_target(
    rounds_pga_2026_past_wide,
    {
      lst <- rounds_pga_2026_past_events
      lst <- lst[!vapply(lst, is.null, logical(1))]
      rbindlist(lst, use.names = TRUE, fill = TRUE)
    }
  ),
  
  tar_target(
    rounds_long_2026_past,
    reshape_wide_scores_to_long(rounds_pga_2026_past_wide, events_2026_min)
  ),
  
  tar_target(
    rounds_feat_long_2026_past,
    add_sg_rollups_long(rounds_long_2026_past)
  ),
  
  tar_target(
    rounds_feat_long_all_plus_2026,
    rbindlist(list(rounds_feat_long_all_plus_2025, rounds_feat_long_2026_past), use.names = TRUE, fill = TRUE)
  ),
  # ---------------------------
  # LIVE: current/next event (PGA, DK Top-20)
  # ---------------------------
  tar_target(live_alpha, 0.70),
  
  tar_target(
    dk_live_top20_pga,
    dg_betting_outrights(tour = "pga", market = "top_20", odds_format = "american")
  ),
  
  tar_target(
    live_event_id_pga,
    live_event_id_from_odds(dk_live_top20_pga, tour_tag = "PGA")
  ),
  tar_target(
    dk_live_top20_pga_stamped,
    stamp_live_event_id(dk_live_top20_pga, live_event_id_pga)
  ),
  tar_target(
    live_event_players_pga,
    {
      o <- as.data.table(dk_live_top20_pga_stamped)
      
      # dg_id is the typical player key in DG odds tables
      if (!("dg_id" %in% names(o))) {
        # sometimes it's player_id already
        if ("player_id" %in% names(o)) {
          o[, dg_id := player_id]
        } else {
          stop("live odds missing dg_id/player_id; inspect names(dk_live_top20_pga).")
        }
      }
      
      unique(o[, .(
        event_id,
        player_id = as.integer(dg_id)
      )])
    }
  ),
  
  tar_target(
    live_event_player_table_pga,
    {
      ep <- copy(live_event_players_pga)
      # Use today's date as the event as-of date for the rolling join.
      # You can replace this with schedule start_date later if you want.
      ep[, start_date := Sys.Date()]
      ep[]
    }
  ),
  
  tar_target(
    live_features_pga,
    join_asof_features(rounds_feat_long_all_plus_2025, live_event_player_table_pga)
  ),
  
  tar_target(
    live_pred_pga_top20,
    {
      te <- copy(live_features_pga)
      
      # match schema
      if (!("dummy" %in% names(te))) te[, dummy := 1]
      
      prob_tbl <- predict(top20_wf, new_data = te, type = "prob")
      p_raw <- if (".pred_1" %in% names(prob_tbl)) prob_tbl[[".pred_1"]] else prob_tbl[[ncol(prob_tbl)]]
      
      te[, p_top20 := as.numeric(p_raw)]
      te[, .(event_id, player_id, p_top20)]
    }
  ),
  
  tar_target(
    live_dk_join_pga,
    join_dk_open_close(
      pred_dt = live_pred_pga_top20,
      dk_odds_dt = dk_live_top20_pga_stamped,
      alpha = live_alpha,
      odds_min = -500,
      odds_max = 1000
    )
  ),
  
  tar_target(
    live_bets_pga,
    select_bets_top20(
      dk_join_dt = live_dk_join_pga,
      ev_min = 0.00,
      max_bets_per_event = 3,
      odds_min = -500,
      odds_max = 1000
    )
  ),
  
  tar_target(
    live_bets_card_pga,
    {
      b <- copy(live_bets_pga)
      if (nrow(b) == 0) return(b)
      
      # Make the bet card readable
      keep <- intersect(
        c("event_id","player_id","player_name","open_odds",
          "p_top20","p_open_imp","p_open_devig","p_blend",
          "ev_per_1","ev_blend_per_1",
          "edge_open_raw","edge_open_devig",
          "profit_per_1"),
        names(b)
      )
      b <- b[, ..keep]
      
      # Order by blended EV
      setorder(b, -ev_blend_per_1)
      b[]
    }
  )
)