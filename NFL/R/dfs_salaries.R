dfs_salary_schema <- function() {
  c(
    "season", "week", "site", "slate_type", "slate_name",
    "player_id_site", "player_name", "position", "team", "opponent",
    "home_away", "salary", "fantasy_points", "game_info",
    "source", "source_reference", "captured_at_utc"
  )
}

empty_dfs_salaries <- function() {
  tibble::tibble(
    season = integer(),
    week = integer(),
    site = character(),
    slate_type = character(),
    slate_name = character(),
    player_id_site = character(),
    player_name = character(),
    position = character(),
    team = character(),
    opponent = character(),
    home_away = character(),
    salary = double(),
    fantasy_points = double(),
    game_info = character(),
    source = character(),
    source_reference = character(),
    captured_at_utc = character()
  )
}

dfs_request <- function(url) {
  request <- httr2::request(url)
  proxy_values <- Sys.getenv(
    c("HTTP_PROXY", "HTTPS_PROXY", "ALL_PROXY"),
    unset = ""
  )
  if (any(grepl("127\\.0\\.0\\.1:9$", proxy_values))) {
    request <- httr2::req_options(request, proxy = "")
  }
  request
}

normalize_dfs_team <- function(x) {
  value <- toupper(trimws(as.character(x)))
  aliases <- c(
    ARZ = "ARI", ARI = "ARI",
    ATL = "ATL",
    BAL = "BAL", BLT = "BAL",
    BUF = "BUF",
    CAR = "CAR",
    CHI = "CHI",
    CIN = "CIN",
    CLE = "CLE", CLV = "CLE",
    DAL = "DAL",
    DEN = "DEN",
    DET = "DET",
    GNB = "GB", GBP = "GB", GB = "GB",
    HOU = "HOU", HST = "HOU",
    IND = "IND",
    JAC = "JAX", JAX = "JAX",
    KAN = "KC", KCC = "KC", KC = "KC",
    LAC = "LAC", SDG = "LAC", SD = "LAC",
    LAR = "LAR", RAM = "LAR", STL = "LAR",
    LVR = "LV", OAK = "LV", LV = "LV",
    MIA = "MIA",
    MIN = "MIN",
    NEP = "NE", NWE = "NE", NE = "NE",
    NOR = "NO", NOS = "NO", NO = "NO",
    NYG = "NYG",
    NYJ = "NYJ",
    PHI = "PHI",
    PIT = "PIT",
    SEA = "SEA",
    SFO = "SF", SF = "SF",
    TAM = "TB", TBB = "TB", TB = "TB",
    TEN = "TEN",
    WAS = "WAS", WSH = "WAS"
  )
  hit <- value %in% names(aliases)
  value[hit] <- unname(aliases[value[hit]])
  value
}

dfs_name_last_first_to_display <- function(x) {
  x <- trimws(as.character(x))
  vapply(strsplit(x, ",", fixed = TRUE), function(parts) {
    if (length(parts) < 2) return(trimws(parts[[1]]))
    paste(trimws(paste(parts[-1], collapse = " ")), trimws(parts[[1]]))
  }, character(1))
}

extract_html_pre <- function(content) {
  match <- stringr::str_match(
    content,
    stringr::regex("<pre[^>]*>(.*?)</pre>", ignore_case = TRUE, dotall = TRUE)
  )
  if (is.na(match[1, 2])) return("")
  value <- match[1, 2]
  value <- gsub("&amp;", "&", value, fixed = TRUE)
  value <- gsub("&lt;", "<", value, fixed = TRUE)
  value <- gsub("&gt;", ">", value, fixed = TRUE)
  value <- gsub("&quot;", "\"", value, fixed = TRUE)
  value <- gsub("&#39;", "'", value, fixed = TRUE)
  value
}

download_rotoguru_salary_week <- function(
    season,
    week,
    site = c("dk", "fd"),
    cache_directory = "data/raw/dfs_salaries/rotoguru",
    refresh = FALSE) {
  site <- match.arg(tolower(site), c("dk", "fd"))
  dir.create(cache_directory, recursive = TRUE, showWarnings = FALSE)
  cache_path <- file.path(
    cache_directory,
    sprintf("%s_%d_week_%02d.html", site, season, week)
  )
  url <- sprintf(
    "http://rotoguru1.com/cgi-bin/fyday.pl?week=%d&year=%d&game=%s&scsv=1",
    week, season, site
  )

  if (!file.exists(cache_path) || isTRUE(refresh)) {
    request <- dfs_request(url) |>
      httr2::req_user_agent(
        "nfl-dfs-salary-research/1.0 (one-time historical backfill)"
      ) |>
      httr2::req_timeout(30) |>
      httr2::req_retry(max_tries = 3)
    response <- httr2::req_perform(request)
    raw_content <- httr2::resp_body_raw(response)
    writeBin(raw_content, cache_path)
  }

  content <- readChar(
    cache_path,
    nchars = file.info(cache_path)$size,
    useBytes = TRUE
  )
  csv_text <- extract_html_pre(content)
  if (!nzchar(trimws(csv_text))) return(empty_dfs_salaries())

  raw <- suppressMessages(readr::read_delim(
    I(csv_text),
    delim = ";",
    trim_ws = TRUE,
    show_col_types = FALSE,
    name_repair = "minimal"
  ))
  if (nrow(raw) == 0L) return(empty_dfs_salaries())

  points_column <- if (site == "dk") "DK points" else "FD points"
  salary_column <- if (site == "dk") "DK salary" else "FD salary"
  required <- c(
    "Week", "Year", "GID", "Name", "Pos", "Team", "h/a", "Oppt",
    points_column, salary_column
  )
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    stop(
      "Unexpected RotoGuru schema for ", site, " ", season, " week ", week,
      ". Missing: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  tibble::tibble(
    season = as.integer(raw[["Year"]]),
    week = as.integer(raw[["Week"]]),
    site = toupper(site),
    slate_type = "FULL_WEEK",
    slate_name = paste(season, "Week", week),
    player_id_site = as.character(raw[["GID"]]),
    player_name = dfs_name_last_first_to_display(raw[["Name"]]),
    position = toupper(as.character(raw[["Pos"]])),
    team = normalize_dfs_team(raw[["Team"]]),
    opponent = normalize_dfs_team(raw[["Oppt"]]),
    home_away = toupper(as.character(raw[["h/a"]])),
    salary = suppressWarnings(as.numeric(raw[[salary_column]])),
    fantasy_points = suppressWarnings(as.numeric(raw[[points_column]])),
    game_info = NA_character_,
    source = "RotoGuru",
    source_reference = url,
    captured_at_utc = format(
      file.info(cache_path)$mtime,
      tz = "UTC",
      format = "%Y-%m-%dT%H:%M:%SZ"
    )
  ) |>
    dplyr::filter(is.finite(.data$salary), .data$salary > 0)
}

backfill_rotoguru_salaries <- function(
    seasons = 2011:2021,
    weeks = 1:18,
    sites = c("dk", "fd"),
    request_delay_seconds = 0.10,
    refresh = FALSE) {
  jobs <- tidyr::crossing(
    season = as.integer(seasons),
    week = as.integer(weeks),
    site = tolower(sites)
  )
  pieces <- vector("list", nrow(jobs))

  for (index in seq_len(nrow(jobs))) {
    job <- jobs[index, ]
    message(
      sprintf(
        "[%d/%d] %s %d week %d",
        index, nrow(jobs), toupper(job$site), job$season, job$week
      )
    )
    pieces[[index]] <- tryCatch(
      download_rotoguru_salary_week(
        job$season,
        job$week,
        job$site,
        refresh = refresh
      ),
      error = function(error) {
        warning(conditionMessage(error), call. = FALSE)
        empty_dfs_salaries()
      }
    )
    if (request_delay_seconds > 0) Sys.sleep(request_delay_seconds)
  }

  dplyr::bind_rows(pieces) |>
    dplyr::arrange(
      .data$site, .data$season, .data$week,
      .data$position, dplyr::desc(.data$salary), .data$player_name
    )
}

first_matching_column <- function(names_lower, candidates) {
  match <- match(candidates, names_lower)
  match <- match[!is.na(match)]
  if (!length(match)) NA_integer_ else match[[1]]
}

parse_manual_dfs_salary_file <- function(path, season = NA_integer_, week = NA_integer_) {
  raw <- suppressMessages(readr::read_csv(
    path,
    show_col_types = FALSE,
    name_repair = "minimal"
  ))
  names_lower <- tolower(trimws(names(raw)))

  dk_signature <- all(c("name + id", "salary") %in% names_lower)
  fd_signature <- all(c("nickname", "salary") %in% names_lower)
  site <- if (dk_signature) "DK" else if (fd_signature) "FD" else NA_character_
  if (is.na(site)) {
    stop(
      "Could not identify DraftKings or FanDuel schema in ", basename(path),
      call. = FALSE
    )
  }

  pick <- function(candidates, default = NA_character_) {
    index <- first_matching_column(names_lower, candidates)
    if (is.na(index)) rep(default, nrow(raw)) else raw[[index]]
  }
  player_name <- if (site == "DK") {
    pick(c("name", "name + id"))
  } else {
    pick(c("nickname", "name"))
  }

  inferred <- stringr::str_match(
    basename(path),
    stringr::regex("(20\\d{2}).*?(?:week|wk)[-_ ]?(\\d{1,2})", ignore_case = TRUE)
  )
  if (is.na(season) && !is.na(inferred[1, 2])) {
    season <- as.integer(inferred[1, 2])
  }
  if (is.na(week) && !is.na(inferred[1, 3])) {
    week <- as.integer(inferred[1, 3])
  }

  tibble::tibble(
    season = rep(as.integer(season), nrow(raw)),
    week = rep(as.integer(week), nrow(raw)),
    site = site,
    slate_type = "PLATFORM_EXPORT",
    slate_name = tools::file_path_sans_ext(basename(path)),
    player_id_site = as.character(pick(c("id", "player id", "name + id"))),
    player_name = as.character(player_name),
    position = toupper(as.character(pick(c("roster position", "position", "pos")))),
    team = normalize_dfs_team(pick(c("teamabbrev", "team", "team abbreviation"))),
    opponent = normalize_dfs_team(pick(c("opponent", "opp"))),
    home_away = NA_character_,
    salary = suppressWarnings(as.numeric(pick(c("salary")))),
    fantasy_points = suppressWarnings(
      as.numeric(pick(c("avgpointspergame", "fppg", "fpts")))
    ),
    game_info = as.character(pick(c("game info", "game"))),
    source = paste(site, "official salary CSV"),
    source_reference = normalizePath(path, winslash = "/", mustWork = FALSE),
    captured_at_utc = format(
      file.info(path)$mtime,
      tz = "UTC",
      format = "%Y-%m-%dT%H:%M:%SZ"
    )
  ) |>
    dplyr::filter(is.finite(.data$salary), .data$salary > 0)
}

ingest_manual_dfs_salary_directory <- function(
    directory = "data/raw/dfs_salary_uploads") {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  paths <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  if (!length(paths)) return(empty_dfs_salaries())
  purrr::map_dfr(paths, function(path) {
    tryCatch(
      parse_manual_dfs_salary_file(path),
      error = function(error) {
        warning(conditionMessage(error), call. = FALSE)
        empty_dfs_salaries()
      }
    )
  })
}

deduplicate_dfs_salaries <- function(data) {
  data |>
    dplyr::mutate(
      source_priority = dplyr::case_when(
        stringr::str_detect(.data$source, "official") ~ 1L,
        .data$source == "DraftKings API" ~ 2L,
        TRUE ~ 3L
      )
    ) |>
    dplyr::arrange(
      .data$source_priority,
      dplyr::desc(.data$captured_at_utc)
    ) |>
    dplyr::distinct(
      .data$season, .data$week, .data$site, .data$slate_name,
      .data$player_id_site, .data$player_name, .data$position,
      .keep_all = TRUE
    ) |>
    dplyr::select(-"source_priority")
}
