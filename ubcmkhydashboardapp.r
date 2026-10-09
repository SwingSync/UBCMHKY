# Detect whether the app is likely running in a hosted runtime.
# We use this to avoid pretending local file writes are persistent online.
is_hosted_runtime <- function() {
  nzchar(Sys.getenv("SHINY_PORT")) ||
    nzchar(Sys.getenv("RSTUDIO_CONNECT")) ||
    nzchar(Sys.getenv("RSCONNECT_PORT")) ||
    nzchar(Sys.getenv("R_CONFIG_ACTIVE"))
}

# Core packages used by the dashboard itself plus the live scraping layer.
required_packages <- c("shiny", "dplyr", "ggplot2", "DT", "rvest")

# Optional deployment helper package for publishing to shinyapps.io.
deploy_packages <- c("rsconnect")

# Install any missing packages when running locally so the live-data
# pipeline can start without extra manual setup. In hosted environments,
# package installation should happen during deployment instead.
install_missing_packages <- function(packages) {
  missing <- packages[!vapply(
    packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )]
  if (!length(missing)) {
    return(invisible(NULL))
  }

  if (is_hosted_runtime()) {
    warning(
      paste(
        "Missing required packages in hosted mode:",
        paste(missing, collapse = ", "),
        "Install them as deployment dependencies before publishing."
      )
    )
    return(invisible(missing))
  }

  tryCatch(
    {
      install.packages(missing, repos = "https://cloud.r-project.org")
      invisible(character(0))
    },
    error = function(e) {
      warning(
        paste(
          "Automatic package installation failed:",
          e$message,
          "Install the missing packages manually if live scraping",
          "does not load."
        )
      )
      invisible(missing)
    }
  )
}

install_missing_packages(required_packages)

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Install optional deployment tooling locally when you want to publish.
# This is skipped in hosted runtimes because rsconnect is not needed there.
install_deploy_tools <- function() {
  if (!is_hosted_runtime()) {
    install_missing_packages(deploy_packages)
  }
}

# Build a focused file list for deployment so temporary caches and editor
# artifacts are not bundled into the shinyapps.io upload.
deployment_app_files <- function(app_dir = ".") {
  base_files <- c(
    "app.r",
    "boxscore.csv",
    "check.csv",
    "playoff.csv",
    "reg.csv",
    "README.txt"
  )
  base_files <- base_files[file.exists(file.path(app_dir, base_files))]

  www_dir <- file.path(app_dir, "www")
  www_files <- character(0)
  if (dir.exists(www_dir)) {
    www_files <- list.files(
      www_dir,
      recursive = TRUE,
      full.names = FALSE
    )
    www_files <- file.path("www", www_files)
  }

  unique(c(base_files, www_files))
}

# Helper to publish this app to shinyapps.io from your local R session.
# Set the three SHINYAPPS_* environment variables first, or pass the values
# directly to the function arguments.
deploy_to_shinyapps <- function(
  account = Sys.getenv("SHINYAPPS_ACCOUNT"),
  token = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET"),
  app_dir = ".",
  app_name = NULL
) {
  install_deploy_tools()

  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop(
      paste(
        "Package 'rsconnect' is required for deployment.",
        "Run install.packages('rsconnect') if it was not installed",
        "automatically."
      )
    )
  }

  if (!nzchar(account) || !nzchar(token) || !nzchar(secret)) {
    stop(
      paste(
        "Set SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN, and SHINYAPPS_SECRET,",
        "or pass account, token, and secret directly to",
        "deploy_to_shinyapps()."
      )
    )
  }

  rsconnect::setAccountInfo(
    name = account,
    token = token,
    secret = secret
  )
  rsconnect::deployApp(
    appDir = app_dir,
    appName = app_name,
    appFiles = deployment_app_files(app_dir)
  )
}

utils::globalVariables(c(
  "A", "All shifts", "Assists", "Breakouts", "Category", "Entries",
  "Faceoffs", "Faceoffs won", "Faceoffs won, %", "Faceoffs lost",
  "Fenwick Against", "Fenwick For", "First Assist", "First assist",
  "G", "GA", "GP", "GP-GS", "Games Played", "Goals", "Hits",
  "Losses DZ", "Losses NZ", "Losses OZ", "Lost", "Metric", "MIN",
  "Minus", "PN-PIM", "PTS", "Penalties", "Penalties Drawn",
  "Penalty Time", "Player", "Plus", "Points", "Points / Game",
  "Pos", "Position", "Puck Losses", "Saves", "Scoring Chances",
  "Second Assist", "Second assist", "Shirt number", "Shots",
  "Shots Blocking", "Shots on Goal", "Takeaways", "Takeaways DZ",
  "Takeaways NZ", "Takeaways OZ", "TOI (min)", "TOI / Game",
  "TOI / Game (min)", "Time on ice (min)", "Value", "Won", "Win %",
  "all_shifts", "assists", "breakouts", "choices", "current_input",
  "current_val", "d", "entries", "faceoffs", "faceoffs_lost",
  "faceoffs_won", "faceoffs_won_pct", "fenwick_against", "fenwick_for",
  "filtered_players", "games_played", "goals", "gp", "hits", "img",
  "initials", "keep", "label", "max_val", "metric", "minutes",
  "n", "old", "out", "p", "penalties", "penalties_drawn",
  "penalty_time_minutes", "player", "player_name", "players",
  "plus", "plus_minus", "pm_text", "points", "points_per_game",
  "position", "positions", "prof", "puck_losses", "puck_losses_dz",
  "puck_losses_nz", "puck_losses_oz", "rng", "scoring_chances",
  "selected", "shirt_number", "shirt_number_numeric", "shots",
  "shots_blocking", "shots_on_goal", "tbl", "tbl_matrix",
  "time_on_ice", "time_on_ice_minutes", "toi_per_game_minutes",
  "total_assists", "transitions", "ts", "value"
))

# Core visual theme for the dashboard.
# Keeping these as top-level constants makes brand updates simple.
navy <- "#002145"
gold <- "#D6B22D"
white <- "#FFFFFF"
light <- "#F7F9FC"
border <- "#D9E1EC"
green <- "#16A34A"
red <- "#DC2626"

# Local image directory used in development.
# This works on a local machine, but hosted deployments like shinyapps.io
# should eventually use external storage for durability.
image_dir <- file.path("www", "player-images")
dir.create(image_dir, recursive = TRUE, showWarnings = FALSE)

# Convenience flag used throughout the app when deciding whether
# it is safe to save files to the local app directory.
can_persist_local_files <- !is_hosted_runtime()

# Canonical list of stat columns expected from uploaded CSV data.
expected_cols <- c(
  "Shirt number", "Player", "Position", "Time on ice",
  "Games played", "All shifts", "Goals", "First assist",
  "Second assist", "Points", "+/-", "Plus", "Minus", "Shots",
  "Shots on goal", "Scoring chances", "Hits", "Shots blocking",
  "Penalties", "Penalties drawn", "Penalty time", "Faceoffs",
  "Faceoffs won", "Faceoffs lost", "Faceoffs won, %",
  "Fenwick for", "Fenwick against", "Takeaways",
  "Takeaways in DZ", "Takeaways in NZ", "Takeaways in OZ",
  "Puck losses", "Puck losses in DZ", "Puck losses in NZ",
  "Puck losses in OZ", "Entries", "Breakouts"
)

rename_map <- c(
  "Shirt number" = "shirt_number",
  "Player" = "player",
  "Position" = "position",
  "Time on ice" = "time_on_ice",
  "Games played" = "games_played",
  "All shifts" = "all_shifts",
  "Goals" = "goals",
  "First assist" = "first_assist",
  "Second assist" = "second_assist",
  "Points" = "points",
  "+/-" = "plus_minus",
  "Plus" = "plus",
  "Minus" = "minus",
  "Shots" = "shots",
  "Shots on goal" = "shots_on_goal",
  "Scoring chances" = "scoring_chances",
  "Hits" = "hits",
  "Shots blocking" = "shots_blocking",
  "Penalties" = "penalties",
  "Penalties drawn" = "penalties_drawn",
  "Penalty time" = "penalty_time",
  "Faceoffs" = "faceoffs",
  "Faceoffs won" = "faceoffs_won",
  "Faceoffs lost" = "faceoffs_lost",
  "Faceoffs won, %" = "faceoffs_won_pct",
  "Fenwick for" = "fenwick_for",
  "Fenwick against" = "fenwick_against",
  "Takeaways" = "takeaways",
  "Takeaways in DZ" = "takeaways_dz",
  "Takeaways in NZ" = "takeaways_nz",
  "Takeaways in OZ" = "takeaways_oz",
  "Puck losses" = "puck_losses",
  "Puck losses in DZ" = "puck_losses_dz",
  "Puck losses in NZ" = "puck_losses_nz",
  "Puck losses in OZ" = "puck_losses_oz",
  "Entries" = "entries",
  "Breakouts" = "breakouts"
)

# Safe division helper so per-game metrics do not fail on zero denominators.
safe_div <- function(x, y) ifelse(is.na(y) | y == 0, 0, x / y)

# Convert a player name into a simple key suitable for filenames.
player_key <- function(x) {
  gsub(
    "(^-+|-+$)",
    "",
    gsub("[^a-z0-9]+", "-", tolower(trimws(x)))
  )
}

# Convert mm:ss text values into numeric seconds for analysis.
mmss_to_seconds <- function(x) {
  x <- trimws(as.character(x))
  out <- ifelse(
    is.na(x) | x == "" | x == "0" | x == "0:00" | x == "-",
    0,
    suppressWarnings(
      as.numeric(sub(":.*", "", x)) * 60 +
        as.numeric(sub(".*:", "", x))
    )
  )
  out[is.na(out)] <- 0
  out
}

# Find a matching player image in the local image folder if one exists.
find_player_image <- function(player_name) {
  files <- list.files(
    image_dir,
    pattern = paste0("^", player_key(player_name), "\\."),
    full.names = FALSE
  )
  if (length(files) == 0) NULL else file.path("player-images", files[1])
}

# Validate and standardize a stats data frame so every downstream plot,
# table, and filter can work from one consistent schema.
clean_stats <- function(raw) {
  names(raw) <- trimws(names(raw))
  missing <- setdiff(expected_cols, names(raw))
  if (length(missing) > 0) {
    stop(
      paste(
        "Missing required columns:",
        paste(missing, collapse = ", ")
      )
    )
  }
  data <- raw[, expected_cols, drop = FALSE]
  names(data) <- unname(rename_map[expected_cols])
  num_cols <- setdiff(
    names(data),
    c("player", "position", "time_on_ice", "penalty_time")
  )
  for (nm in num_cols) {
    data[[nm]] <- suppressWarnings(
      as.numeric(gsub(",", "", trimws(as.character(data[[nm]]))))
    )
  }
  data |>
    mutate(
      player = trimws(as.character(.data$player)),
      position = trimws(as.character(.data$position)),
      shirt_number = trimws(as.character(.data$shirt_number)),
      time_on_ice_seconds = mmss_to_seconds(.data$time_on_ice),
      penalty_time_seconds = mmss_to_seconds(.data$penalty_time),
      total_assists = .data$first_assist + .data$second_assist,
      shirt_number_numeric = suppressWarnings(
        as.numeric(.data$shirt_number)
      ),
      faceoffs_won_pct = ifelse(
        .data$faceoffs_won_pct <= 1,
        .data$faceoffs_won_pct * 100,
        .data$faceoffs_won_pct
      ),
      time_on_ice_minutes = .data$time_on_ice_seconds / 60,
      penalty_time_minutes = .data$penalty_time_seconds / 60,
      toi_per_game_minutes = safe_div(
        .data$time_on_ice_seconds / 60,
        .data$games_played
      ),
      goals_per_game = safe_div(.data$goals, .data$games_played),
      points_per_game = safe_div(.data$points, .data$games_played),
      fenwick_share = safe_div(
        .data$fenwick_for,
        .data$fenwick_for + .data$fenwick_against
      ) * 100
    ) |>
    filter(.data$player != "") |>
    arrange(desc(.data$points), desc(.data$goals), .data$player)
}

# Read CSV input while tolerating exports that include text above the
# header row.
read_stats_csv <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  start <- grep("Shirt number", lines, fixed = TRUE)[1]
  if (is.na(start)) start <- 1
  raw <- read.csv(
    text = paste(lines[start:length(lines)], collapse = "\n"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  clean_stats(raw)
}

# Build a blank stat row used while mapping partial live site data into the
# larger dashboard schema.
empty_live_row <- function() {
  row <- as.list(
    setNames(rep(NA_character_, length(expected_cols)), expected_cols)
  )
  numeric_like <- setdiff(
    expected_cols,
    c("Player", "Position", "Time on ice", "Penalty time")
  )
  row[numeric_like] <- lapply(row[numeric_like], function(x) 0)
  row[["Time on ice"]] <- "0:00"
  row[["Penalty time"]] <- "0:00"
  as.data.frame(row, stringsAsFactors = FALSE)
}

# Lightweight fallback helper for scraped values that may be missing.
coalesce_value <- function(value, fallback) {
  if (length(value) == 0 || is.na(value) || identical(value, "")) {
    fallback
  } else {
    value
  }
}

# Scrape the live current-season UBC page and map the exposed stats into the
# same structure used by uploaded CSV data.
scrape_live_stats <- function(
  url = "https://gothunderbirds.ca/sports/mens-ice-hockey/stats/2025"
) {
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop(
      paste(
        "Package 'rvest' is required for live data.",
        "Install it with install.packages('rvest')."
      )
    )
  }
  page <- rvest::read_html(url)
  tables <- rvest::html_table(rvest::html_elements(page, "table"), fill = TRUE)
  if (length(tables) == 0) stop("No tables were found on the live stats page.")

  normalize_names <- function(x) tolower(gsub("[^a-z0-9]+", "", x))

  find_table <- function(required_cols) {
    wanted <- normalize_names(required_cols)
    for (tbl in tables) {
      nm <- normalize_names(names(tbl))
      if (all(wanted %in% nm)) {
        return(tbl)
      }
    }
    NULL
  }

  skaters_raw <- find_table(
    c("#", "Player", "GP", "G", "A", "PTS", "Shots", "+/-", "PN-PIM")
  )
  goalies_raw <- find_table(
    c("#", "Player", "GP-GS", "MIN", "GA", "Saves", "SV%")
  )
  if (is.null(skaters_raw) && is.null(goalies_raw)) {
    stop(
      paste(
        "Could not match the live skater or goalie tables",
        "on the current season page."
      )
    )
  }

  parse_penalty_minutes <- function(x) {
    parts <- strsplit(as.character(x), "-", fixed = TRUE)
    sapply(parts, function(piece) {
      if (length(piece) >= 2) {
        suppressWarnings(as.numeric(piece[2]))
      } else {
        0
      }
    })
  }

  build_skater_row <- function(tbl) {
    names(tbl) <- gsub("\\.+", "", names(tbl))
    tbl <- tbl |>
      filter(
        !grepl(
          "Total|Opponents|TM|Team",
          .data$Player,
          ignore.case = TRUE
        )
      )
    rows <- lapply(seq_len(nrow(tbl)), function(i) {
      row <- empty_live_row()
      player_name <- trimws(as.character(tbl$Player[i]))
      number <- coalesce_value(
        as.character(
          tbl[[
            which(normalize_names(names(tbl)) == normalize_names("#"))
          ]][i]
        ),
        ""
      )
      gp <- suppressWarnings(
        as.numeric(
          tbl[[
            which(normalize_names(names(tbl)) == normalize_names("GP"))
          ]][i]
        )
      )
      goals <- suppressWarnings(
        as.numeric(
          tbl[[
            which(normalize_names(names(tbl)) == normalize_names("G"))
          ]][i]
        )
      )
      assists <- suppressWarnings(
        as.numeric(
          tbl[[
            which(normalize_names(names(tbl)) == normalize_names("A"))
          ]][i]
        )
      )
      points <- suppressWarnings(
        as.numeric(
          tbl[[
            which(normalize_names(names(tbl)) == normalize_names("PTS"))
          ]][i]
        )
      )
      shots <- suppressWarnings(
        as.numeric(
          tbl[[
            which(normalize_names(names(tbl)) == normalize_names("Shots"))
          ]][i]
        )
      )
      plus_minus <- suppressWarnings(
        as.numeric(
          gsub(
            "\\+",
            "",
            tbl[[
              which(normalize_names(names(tbl)) == normalize_names("+/-"))
            ]][i]
          )
        )
      )
      penalty_code <- tbl[[
        which(normalize_names(names(tbl)) == normalize_names("PN-PIM"))
      ]][i]
      pim <- parse_penalty_minutes(penalty_code)
      row[["Shirt number"]] <- number
      row[["Player"]] <- player_name
      row[["Position"]] <- "Skater"
      row[["Time on ice"]] <- "0:00"
      row[["Games played"]] <- coalesce_value(gp, 0)
      row[["All shifts"]] <- 0
      row[["Goals"]] <- coalesce_value(goals, 0)
      row[["First assist"]] <- coalesce_value(assists, 0)
      row[["Second assist"]] <- 0
      row[["Points"]] <- coalesce_value(points, 0)
      row[["+/-"]] <- coalesce_value(plus_minus, 0)
      row[["Plus"]] <- max(coalesce_value(plus_minus, 0), 0)
      row[["Minus"]] <- abs(min(coalesce_value(plus_minus, 0), 0))
      row[["Shots"]] <- coalesce_value(shots, 0)
      row[["Shots on goal"]] <- coalesce_value(shots, 0)
      row[["Scoring chances"]] <- 0
      row[["Hits"]] <- 0
      row[["Shots blocking"]] <- 0
      row[["Penalties"]] <- 0
      row[["Penalties drawn"]] <- 0
      row[["Penalty time"]] <- paste0(coalesce_value(pim[i], 0), ":00")
      row[["Faceoffs"]] <- 0
      row[["Faceoffs won"]] <- 0
      row[["Faceoffs lost"]] <- 0
      row[["Faceoffs won, %"]] <- 0
      row[["Fenwick for"]] <- 0
      row[["Fenwick against"]] <- 0
      row[["Takeaways"]] <- 0
      row[["Takeaways in DZ"]] <- 0
      row[["Takeaways in NZ"]] <- 0
      row[["Takeaways in OZ"]] <- 0
      row[["Puck losses"]] <- 0
      row[["Puck losses in DZ"]] <- 0
      row[["Puck losses in NZ"]] <- 0
      row[["Puck losses in OZ"]] <- 0
      row[["Entries"]] <- 0
      row[["Breakouts"]] <- 0
      row
    })
    bind_rows(rows)
  }

  build_goalie_row <- function(tbl) {
    names(tbl) <- gsub("\\.+", "", names(tbl))
    tbl <- tbl |>
      filter(
        !grepl(
          "Total|Opponents|TM|Team",
          .data$Player,
          ignore.case = TRUE
        )
      )
    rows <- lapply(seq_len(nrow(tbl)), function(i) {
      row <- empty_live_row()
      player_name <- trimws(as.character(tbl$Player[i]))
      number <- coalesce_value(
        as.character(
          tbl[[
            which(normalize_names(names(tbl)) == normalize_names("#"))
          ]][i]
        ),
        ""
      )
      gp_gs <- as.character(
        tbl[[
          which(normalize_names(names(tbl)) == normalize_names("GP-GS"))
        ]][i]
      )
      gp <- suppressWarnings(
        as.numeric(strsplit(gp_gs, "-", fixed = TRUE)[[1]][1])
      )
      minutes <- coalesce_value(
        as.character(
          tbl[[
            which(normalize_names(names(tbl)) == normalize_names("MIN"))
          ]][i]
        ),
        "0:00"
      )
      row[["Shirt number"]] <- number
      row[["Player"]] <- player_name
      row[["Position"]] <- "Goalie"
      row[["Time on ice"]] <- minutes
      row[["Games played"]] <- coalesce_value(gp, 0)
      row[["All shifts"]] <- 0
      row[["Penalty time"]] <- "0:00"
      row
    })
    bind_rows(rows)
  }

  skaters <- if (!is.null(skaters_raw)) {
    build_skater_row(skaters_raw)
  } else {
    NULL
  }
  goalies <- if (!is.null(goalies_raw)) {
    build_goalie_row(goalies_raw)
  } else {
    NULL
  }
  live_raw <- bind_rows(skaters, goalies)
  clean_stats(live_raw)
}

# Default dataset loaded at startup from the local CSV file.
box <- read_stats_csv("boxscore.csv")

# The live athletics page is now the primary dashboard source.
# CSV uploads remain available as a manual override and fallback.
default_data_source <- "live"

# Metrics exposed in filters and comparison controls.
metric_choices <- c(
  "Time on Ice (min)" = "time_on_ice_minutes",
  "Games played" = "games_played",
  "All shifts" = "all_shifts",
  "Goals" = "goals",
  "First assist" = "first_assist",
  "Second assist" = "second_assist",
  "Assists" = "total_assists",
  "Points" = "points",
  "+/-" = "plus_minus",
  "Plus" = "plus",
  "Minus" = "minus",
  "Shots" = "shots",
  "Shots on goal" = "shots_on_goal",
  "Scoring chances" = "scoring_chances",
  "Hits" = "hits",
  "Shots blocking" = "shots_blocking",
  "Penalties" = "penalties",
  "Penalties drawn" = "penalties_drawn",
  "Penalty time (min)" = "penalty_time_minutes",
  "Faceoffs" = "faceoffs",
  "Faceoffs won" = "faceoffs_won",
  "Faceoffs lost" = "faceoffs_lost",
  "Faceoffs won, %" = "faceoffs_won_pct",
  "Fenwick for" = "fenwick_for",
  "Fenwick against" = "fenwick_against",
  "Takeaways" = "takeaways",
  "Takeaways in DZ" = "takeaways_dz",
  "Takeaways in NZ" = "takeaways_nz",
  "Takeaways in OZ" = "takeaways_oz",
  "Puck losses" = "puck_losses",
  "Puck losses in DZ" = "puck_losses_dz",
  "Puck losses in NZ" = "puck_losses_nz",
  "Puck losses in OZ" = "puck_losses_oz",
  "Entries" = "entries",
  "Breakouts" = "breakouts",
  "Points / Game" = "points_per_game",
  "TOI / Game (min)" = "toi_per_game_minutes"
)

# Player filtering reuses the same metric set as the comparison tab.
player_filter_choices <- metric_choices

# Grouped chart views for the player profile panel.
player_profile_groups <- c(
  "Scoring" = "scoring",
  "Shooting & Chances" = "shooting",
  "Physical & Discipline" = "physical",
  "Faceoffs" = "faceoffs",
  "Puck Management" = "puck_management",
  "Transitions" = "transitions",
  "Usage" = "usage"
)

# Shared UI helper for summary cards at the top of the overview tab.
card_ui <- function(title, value, subtitle = NULL, accent = gold) {
  div(
    class = "stat-card", style = paste0("border-top:4px solid ", accent, ";"),
    div(class = "stat-label", title), div(class = "stat-value", value),
    if (!is.null(subtitle)) div(class = "stat-sub", subtitle)
  )
}

# nolint start: line_length_linter.
ui <- tagList(
  tags$head(tags$style(HTML(paste0("
    body{background:", light, ";font-family:Segoe UI,Arial,sans-serif;}
    .topbar{background:", navy, ";color:white;padding:22px 28px;border-bottom:3px solid ", gold, ";}
    .brand{font-size:30px;font-weight:800;}
    .season{opacity:.8;margin-top:4px;}
    .navbar{margin-bottom:14px;min-height:48px;}
    .navbar-default{background:white;border-color:", border, ";margin-bottom:14px;}
    .navbar-default .navbar-nav>.active>a{color:", navy, ";background:white;border-bottom:3px solid ", gold, ";font-weight:700;}
    .navbar-default .navbar-nav>li>a{color:", navy, ";}
    .container-fluid{padding-left:18px;padding-right:18px;}
    .panel-box{background:white;border:1px solid ", border, ";border-radius:18px;padding:16px;box-shadow:0 12px 28px rgba(0,33,69,.06);margin-bottom:16px;}
    .stat-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(200px,1fr));gap:16px;}
    .stat-card{background:white;border:1px solid ", border, ";border-radius:18px;padding:18px;box-shadow:0 10px 24px rgba(0,33,69,.05);}
    .stat-label{font-size:12px;text-transform:uppercase;letter-spacing:.08em;color:#5b6b84;font-weight:700;}
    .stat-value{font-size:38px;color:", navy, ";font-weight:800;line-height:1.1;margin-top:8px;}
    .stat-sub{margin-top:8px;color:#6b7a90;}
    .player-hero{background:linear-gradient(135deg,", navy, ",#0c3a6d);color:white;border-radius:22px;padding:22px;display:flex;gap:18px;align-items:flex-start;}
    .player-photo{width:170px;min-width:170px;height:170px;flex:0 0 170px;border-radius:18px;overflow:hidden;background:rgba(255,255,255,.08);display:flex;align-items:center;justify-content:center;}
    .player-photo img{width:100%;height:100%;object-fit:cover;}
    .player-placeholder{width:100%;height:100%;display:flex;align-items:center;justify-content:center;font-size:42px;font-weight:800;background:linear-gradient(135deg,", gold, ",rgba(255,255,255,.12));color:", navy, ";}
    .player-name{font-size:34px;font-weight:800;}
    .player-num{display:inline-flex;align-items:center;justify-content:center;width:56px;height:56px;border-radius:14px;background:", gold, ";color:", navy, ";font-size:28px;font-weight:800;}
    .player-tags span{display:inline-block;margin-right:8px;background:rgba(255,255,255,.14);padding:6px 10px;border-radius:999px;font-size:12px;text-transform:uppercase;}
    .player-kpis{display:grid;grid-template-columns:repeat(auto-fit,minmax(130px,1fr));gap:12px;margin-top:18px;}
    .player-kpi{background:rgba(255,255,255,.08);border-radius:16px;padding:12px;}
    .player-kpi .k{font-size:12px;text-transform:uppercase;opacity:.75;}
    .player-kpi .v{font-size:28px;font-weight:800;margin-top:6px;}
    .note{background:rgba(214,178,45,.12);border:1px solid rgba(214,178,45,.35);padding:12px 14px;border-radius:14px;color:", navy, ";margin-bottom:14px;}
    .source-strip{display:flex;justify-content:space-between;align-items:center;gap:12px;flex-wrap:wrap;margin:0 18px 10px 18px;}
    .source-badge{display:inline-flex;align-items:center;gap:8px;background:", navy, ";color:", white, ";padding:10px 14px;border-radius:999px;font-weight:700;box-shadow:0 10px 20px rgba(0,33,69,.12);}
    .source-badge.live{background:", gold, ";color:", navy, ";}
    .source-time{background:white;border:1px solid ", border, ";padding:10px 14px;border-radius:999px;color:", navy, ";font-weight:600;}
    .btn-primary{background:", navy, ";border-color:", navy, ";border-radius:12px;font-weight:700;}
    .form-control,.selectize-input{border-radius:12px !important;border-color:", border, " !important;}
    .dataTables_wrapper .dataTables_paginate .paginate_button{padding:.2em .7em !important;}
    .filter-row{margin-bottom:8px;}
    .compact-table .dataTables_wrapper{margin-top:4px;}
    table.dataTable.compare-bordered,
    table.dataTable.compare-bordered th,
    table.dataTable.compare-bordered td{border:1px solid ", border, " !important;}
    table.dataTable.compare-bordered thead th{background:", navy, ";color:", white, ";font-weight:700;}
    table.dataTable.compare-bordered tbody td{background:", white, ";}
  ")))),
  div(class = "topbar", div(class = "brand", "UBC MHKY"), div(class = "season", "Season 2025-26")),
  div(
    class = "panel-box", style = "margin: 14px 18px 8px 18px;",
    fluidRow(
      column(
        4,
        selectInput(
          "data_source",
          "Data source",
          choices = c(
            "Live current season" = "live",
            "Uploaded CSV" = "uploaded"
          ),
          selected = default_data_source
        )
      ),
      column(4, br(), actionButton("refresh_live", "Refresh Live Data", class = "btn-primary")),
      column(4, br(), uiOutput("data_source_status"))
    )
  ),
  div(class = "source-strip", uiOutput("source_badge"), uiOutput("source_time")),
  navbarPage(
    title = NULL, id = "tabs", collapsible = FALSE,
    tabPanel(
      "Team Overview",
      div(class = "panel-box", uiOutput("overview_cards")),
      fluidRow(
        column(6, div(class = "panel-box", h4("Top Scorers"), plotOutput("top_scorers_plot", height = 300))),
        column(6, div(class = "panel-box", h4("Season Summary"), DTOutput("team_summary")))
      ),
      fluidRow(
        column(6, div(class = "panel-box", h4("Roster Breakdown"), plotOutput("position_plot", height = 280))),
        column(6, div(class = "panel-box", h4("Team Profile"), plotOutput("team_profile_plot", height = 280)))
      )
    ),
    tabPanel(
      "Player Stats",
      div(
        class = "panel-box",
        fluidRow(
          column(4, selectizeInput("selected_player", "Select player", choices = sort(unique(box$player)), selected = box$player[1])),
          column(4, selectInput("player_position_filter", "Position filter", choices = c("All", sort(unique(box$position))), selected = "All")),
          column(4, fileInput("player_image", "Upload player image", accept = c(".png", ".jpg", ".jpeg", ".webp")))
        ),
        fluidRow(
          class = "filter-row",
          column(4, selectInput("player_stat_filter", "Filter by stat", choices = player_filter_choices, selected = "points")),
          column(4, sliderInput("player_stat_min", "Minimum value", min = 0, max = 100, value = 0, step = 1)),
          column(4, div(class = "note", "The player list above updates from the position and stat filters."))
        ),
        uiOutput("player_hero")
      ),
      fluidRow(
        column(6, div(class = "panel-box", h4("Player Statistics"), DTOutput("player_table"))),
        column(6, div(
          class = "panel-box",
          selectInput("player_profile_group", "Profile chart group", choices = player_profile_groups, selected = "scoring"),
          h4("Player Profile"),
          plotOutput("player_plot", height = 460),
          tags$details(
            style = "margin-top:12px;",
            tags$summary(
              style = paste0(
                "cursor:pointer;font-weight:700;color:", navy, ";"
              ),
              "How to read this profile"
            ),
            uiOutput("player_profile_explainer")
          )
        ))
      )
    ),
    tabPanel(
      "Compare Players",
      div(
        class = "panel-box",
        selectizeInput("compare_players", "Select up to 3 players", choices = sort(unique(box$player)), selected = head(box$player, 3), multiple = TRUE, options = list(maxItems = 3))
      ),
      div(
        class = "panel-box",
        selectInput("compare_metric", "Metric", choices = metric_choices, selected = "points"),
        h4("Comparison Chart"),
        plotOutput("compare_plot", height = 360)
      ),
      div(
        class = "panel-box compact-table",
        fluidRow(
          column(7, h4("Comparison Table")),
          column(5, checkboxInput("transpose_compare", "Flip table axes", value = TRUE))
        ),
        DTOutput("compare_table")
      )
    ),
    tabPanel(
      "Data Upload",
      div(
        class = "panel-box",
        div(class = "note", uiOutput("deployment_note")),
        fluidRow(
          column(4, downloadButton("download_template", "Download Template", class = "btn-primary")),
          column(8, fileInput("stats_upload", "Upload player statistics CSV", accept = ".csv"))
        ),
        br(),
        actionButton("apply_upload", "Apply CSV to Dashboard", class = "btn-primary"),
        br(), br(),
        uiOutput("upload_status")
      ),
      fluidRow(
        column(6, div(class = "panel-box", h4("Required Columns"), verbatimTextOutput("required_columns"))),
        column(6, div(class = "panel-box", h4("Current Data Preview"), DTOutput("upload_preview")))
      )
    )
  )
)
# nolint end

server <- function(input, output, session) {
  # Separate reactive stores for uploaded data and live-scraped data.
  # The active dashboard dataset is chosen later based on the source toggle.
  uploaded_data <- reactiveVal(box)
  live_data <- reactiveVal(NULL)
  status_text <- reactiveVal("Uploaded CSV source: boxscore.csv")
  live_status_text <- reactiveVal(
    paste(
      "Live source is set to the UBC 2025-26 statistics page.",
      "The app will refresh it when needed."
    )
  )
  uploaded_timestamp <- reactiveVal(Sys.time())
  live_timestamp <- reactiveVal(NULL)

  # Refresh the live season feed and record when that happened.
  fetch_live_data <- function() {
    live_status_text(
      "Refreshing live 2025-26 season data from gothunderbirds.ca ..."
    )
    new_live <- scrape_live_stats()
    live_data(new_live)
    live_timestamp(Sys.time())
    live_status_text(
      paste(
        "Live feed loaded from gothunderbirds.ca with",
        nrow(new_live),
        "players. Note: only stats exposed by the athletics site",
        "are populated; unavailable categories default to 0."
      )
    )
  }

  # Prime the live source once at session start so the dashboard opens with
  # current website data when available.
  observeEvent(
    TRUE,
    {
      if (identical(default_data_source, "live") && is.null(live_data())) {
        tryCatch(
          fetch_live_data(),
          error = function(e) {
            live_status_text(paste("Live refresh failed:", e$message))
          }
        )
      }
    },
    once = TRUE,
    ignoreInit = FALSE
  )

  observeEvent(input$refresh_live, {
    tryCatch(
      {
        fetch_live_data()
        showNotification("Live data refreshed.", type = "message")
      },
      error = function(e) {
        live_status_text(paste("Live refresh failed:", e$message))
        showNotification("Could not refresh live data.", type = "error")
      }
    )
  })

  # Central data-source switch used by the rest of the app.
  # Every tab reads from this reactive so the source toggle is global.
  current_source_data <- reactive({
    if (identical(input$data_source, "live")) {
      if (is.null(live_data())) {
        tryCatch(
          fetch_live_data(),
          error = function(e) {
            live_status_text(paste("Live refresh failed:", e$message))
          }
        )
      }
      if (!is.null(live_data())) {
        return(live_data())
      }
      live_status_text(
        paste(
          "Live refresh failed, so the dashboard is using the uploaded",
          "CSV fallback until the website can be reached again."
        )
      )
      uploaded_data()
    } else {
      uploaded_data()
    }
  })

  # Keep dynamic inputs aligned with whichever source is active.
  observeEvent(current_source_data(),
    {
      players <- sort(unique(current_source_data()$player))
      keep <- intersect(isolate(input$compare_players), players)
      updateSelectizeInput(
        session,
        "compare_players",
        choices = players,
        selected = if (length(keep)) keep else head(players, 3),
        server = TRUE
      )
      position_choices <- c("All", sort(unique(current_source_data()$position)))
      selected_position <- if (
        !is.null(input$player_position_filter) &&
          input$player_position_filter %in% position_choices
      ) {
        input$player_position_filter
      } else {
        "All"
      }
      updateSelectInput(
        session,
        "player_position_filter",
        choices = position_choices,
        selected = selected_position
      )
    },
    ignoreInit = FALSE
  )

  # Save a local player image only when the runtime can persist files.
  # On shinyapps.io we warn instead of pretending uploads are durable.
  observeEvent(input$player_image, {
    req(input$selected_player, input$player_image$datapath)
    if (!can_persist_local_files) {
      showNotification(
        paste(
          "Hosted mode detected: player image uploads are not",
          "persisted yet. Use external storage before publishing",
          "this feature."
        ),
        type = "warning"
      )
      return()
    }
    key <- player_key(input$selected_player)
    old <- list.files(
      image_dir,
      pattern = paste0("^", key, "\\."),
      full.names = TRUE
    )
    if (length(old)) unlink(old)
    ext <- tools::file_ext(input$player_image$name)
    file.copy(
      input$player_image$datapath,
      file.path(image_dir, paste0(key, ".", ext)),
      overwrite = TRUE
    )
    showNotification(
      paste("Saved image for", input$selected_player),
      type = "message"
    )
  })

  # Validate uploaded CSV data and load it into the app.
  # In local mode we also write it back to boxscore.csv; in hosted mode
  # the upload is kept only for the current app session.
  observeEvent(input$apply_upload, {
    req(input$stats_upload$datapath)
    tryCatch(
      {
        new_data <- read_stats_csv(input$stats_upload$datapath)
        uploaded_data(new_data)
        uploaded_timestamp(Sys.time())
        out <- new_data[, unname(rename_map[expected_cols]), drop = FALSE]
        names(out) <- expected_cols
        if (can_persist_local_files) {
          write.csv(out, "boxscore.csv", row.names = FALSE, na = "")
          status_text(
            paste(
              "Loaded", nrow(new_data), "players from",
              input$stats_upload$name,
              "and saved the cleaned file to boxscore.csv."
            )
          )
        } else {
          status_text(
            paste(
              "Loaded", nrow(new_data), "players from",
              input$stats_upload$name,
              "for this live app session. Hosted mode does not",
              "persist uploads to disk yet."
            )
          )
        }
        showNotification("Uploaded CSV data updated.", type = "message")
      },
      error = function(e) {
        status_text(paste("Upload failed:", e$message))
        showNotification(
          "Upload failed. See the message in this tab.",
          type = "error"
        )
      }
    )
  })

  # Active data used everywhere below.
  data <- reactive(current_source_data())

  # Filter the player list based on selected position and stat threshold.
  filtered_player_choices <- reactive({
    d <- data()
    req(input$player_stat_filter)
    if (
      !is.null(input$player_position_filter) &&
        input$player_position_filter != "All"
    ) {
      d <- d |> filter(.data$position == input$player_position_filter)
    }
    d |> filter(.data[[input$player_stat_filter]] >= input$player_stat_min)
  })

  # Update the player selector so it only shows players matching the
  # filter controls.
  observeEvent(filtered_player_choices(),
    {
      filtered_players <- filtered_player_choices()
      choices <- filtered_players$player
      if (!length(choices)) {
        choices <- sort(unique(data()$player))
      }
      current_selected <- isolate(input$selected_player)
      selected <- if (
        !is.null(current_selected) &&
          current_selected %in% choices
      ) {
        current_selected
      } else {
        choices[1]
      }
      freezeReactiveValue(input, "selected_player")
      updateSelectizeInput(
        session,
        "selected_player",
        choices = choices,
        selected = selected,
        server = TRUE
      )
    },
    ignoreInit = FALSE
  )

  # Rescale the threshold slider whenever the chosen filter stat changes.
  observeEvent(input$player_stat_filter,
    {
      rng <- range(data()[[input$player_stat_filter]], na.rm = TRUE)
      max_val <- ceiling(rng[2])
      if (!is.finite(max_val) || max_val < 1) max_val <- 1
      current_input <- input$player_stat_min
      if (is.null(current_input)) current_input <- 0
      current_val <- min(current_input, max_val)
      updateSliderInput(
        session,
        "player_stat_min",
        min = 0,
        max = max_val,
        value = current_val,
        step = if (max_val > 20) 1 else 0.1
      )
    },
    ignoreInit = FALSE
  )

  # Convenience reactive for the currently selected player row.
  player_row <- reactive({
    req(input$selected_player)
    data() |>
      filter(.data$player == input$selected_player) |>
      slice(1)
  })

  # Format values for the individual player stat table.
  format_stat_value <- function(metric, value) {
    if (metric == "time_on_ice") {
      return(as.character(value))
    }
    if (metric == "toi_per_game_minutes") {
      return(sprintf("%.1f min", value))
    }
    if (metric == "faceoffs_won_pct") {
      return(paste0(round(value, 1), "%"))
    }
    if (is.numeric(value)) {
      return(format(round(value, 2), trim = TRUE, nsmall = 0))
    }
    as.character(value)
  }

  # Compute a squad ranking label for a selected player and metric.
  squad_rank_label <- function(player_name, metric) {
    d <- data()
    rank_data <- d
    if (metric == "faceoffs_won_pct") {
      rank_data <- rank_data |> filter(.data$faceoffs > 0)
    }
    if (!metric %in% names(rank_data) || !nrow(rank_data)) {
      return("N/A")
    }
    ranked <- rank_data |>
      mutate(
        stat_rank = min_rank(desc(.data[[metric]]))
      )
    player_rank <- ranked |>
      filter(.data$player == player_name) |>
      pull(.data$stat_rank)
    if (!length(player_rank)) {
      return("N/A")
    }
    paste0(player_rank[1], " / ", nrow(ranked))
  }

  # Format profile metric values for the circular player graphic.
  format_profile_value <- function(metric_key, value) {
    if (metric_key == "faceoffs_won_pct") {
      return(paste0(round(value, 1), "%"))
    }
    if (metric_key %in% c("penalty_time_minutes", "toi_per_game_minutes")) {
      return(sprintf("%.1f", value))
    }
    format(round(value, 2), trim = TRUE, nsmall = 0)
  }

  # Metric definitions used by the player profile groups.
  profile_group_defs <- function(group_key) {
    switch(group_key,
      scoring = data.frame(
        metric = c(
          "Goals", "First Assist", "Second Assist",
          "Assists", "Points", "+/-"
        ),
        metric_key = c(
          "goals", "first_assist", "second_assist",
          "total_assists", "points", "plus_minus"
        ),
        stringsAsFactors = FALSE
      ),
      shooting = data.frame(
        metric = c(
          "Shots", "Shots on Goal", "Scoring Chances",
          "Fenwick For", "Fenwick Against"
        ),
        metric_key = c(
          "shots", "shots_on_goal", "scoring_chances",
          "fenwick_for", "fenwick_against"
        ),
        stringsAsFactors = FALSE
      ),
      physical = data.frame(
        metric = c(
          "Hits", "Shots Blocking", "Penalties",
          "Penalties Drawn", "Penalty Time"
        ),
        metric_key = c(
          "hits", "shots_blocking", "penalties",
          "penalties_drawn", "penalty_time_minutes"
        ),
        stringsAsFactors = FALSE
      ),
      faceoffs = data.frame(
        metric = c("Faceoffs", "Won", "Lost", "Win %"),
        metric_key = c(
          "faceoffs", "faceoffs_won", "faceoffs_lost", "faceoffs_won_pct"
        ),
        stringsAsFactors = FALSE
      ),
      puck_management = data.frame(
        metric = c(
          "Takeaways", "Takeaways DZ", "Takeaways NZ", "Takeaways OZ",
          "Puck Losses", "Losses DZ", "Losses NZ", "Losses OZ"
        ),
        metric_key = c(
          "takeaways", "takeaways_dz", "takeaways_nz", "takeaways_oz",
          "puck_losses", "puck_losses_dz", "puck_losses_nz",
          "puck_losses_oz"
        ),
        stringsAsFactors = FALSE
      ),
      transitions = data.frame(
        metric = c("Entries", "Breakouts", "Fenwick For", "Fenwick Against"),
        metric_key = c(
          "entries", "breakouts", "fenwick_for", "fenwick_against"
        ),
        stringsAsFactors = FALSE
      ),
      usage = data.frame(
        metric = c(
          "TOI (min)", "TOI / Game", "Games Played",
          "All Shifts", "Points / Game"
        ),
        metric_key = c(
          "time_on_ice_minutes", "toi_per_game_minutes", "games_played",
          "all_shifts", "points_per_game"
        ),
        stringsAsFactors = FALSE
      )
    )
  }

  # Build the selected player profile chart data for the chosen group.
  player_profile_data <- reactive({
    p <- player_row()
    d <- data()
    req(nrow(p) == 1, input$player_profile_group)

    defs <- profile_group_defs(input$player_profile_group)
    defs$value <- vapply(
      defs$metric_key,
      function(metric_key) {
        as.numeric(p[[metric_key]][[1]])
      },
      numeric(1)
    )
    defs$group_max <- vapply(
      defs$metric_key,
      function(metric_key) {
        vals <- d[[metric_key]]
        vals <- vals[is.finite(vals)]
        if (!length(vals)) {
          return(0)
        }
        max(vals, na.rm = TRUE)
      },
      numeric(1)
    )
    defs$scaled_value <- ifelse(
      defs$group_max > 0,
      defs$value / defs$group_max,
      0
    )
    defs$scaled_value <- pmax(defs$scaled_value, 0)
    defs$radius <- 0.28 + defs$scaled_value * 0.62
    defs$display_value <- vapply(
      seq_len(nrow(defs)),
      function(i) {
        format_profile_value(defs$metric_key[i], defs$value[i])
      },
      character(1)
    )
    defs$idx <- seq_len(nrow(defs))
    defs$label_radius <- 1.08
    defs$value_radius <- pmin(defs$radius + 0.08, 0.98)
    defs
  })

  # Overview cards summarizing the current active dataset.
  output$overview_cards <- renderUI({
    d <- data()
    gp <- max(d$games_played, na.rm = TRUE)
    div(
      class = "stat-grid",
      card_ui(
        "Win-style Snapshot",
        paste0(round(mean(d$points_per_game, na.rm = TRUE), 2), " P/GP"),
        "Roster average scoring pace",
        gold
      ),
      card_ui(
        "Total Goals",
        sum(d$goals, na.rm = TRUE),
        paste0(
          round(safe_div(sum(d$goals, na.rm = TRUE), gp), 2),
          " per game"
        ),
        navy
      ),
      card_ui(
        "Total Points",
        sum(d$points, na.rm = TRUE),
        paste0(nrow(d), " players tracked"),
        gold
      ),
      card_ui(
        "Faceoff Win %",
        paste0(
          round(mean(d$faceoffs_won_pct[d$faceoffs > 0], na.rm = TRUE), 1),
          "%"
        ),
        "Players with recorded draws",
        navy
      )
    )
  })

  # Top scorers bar chart for the overview tab.
  output$top_scorers_plot <- renderPlot({
    p <- data() |>
      slice_max(points, n = 8, with_ties = FALSE) |>
      mutate(player = factor(.data$player, levels = rev(.data$player)))
    ggplot(p, aes(.data$player, .data$points)) +
      geom_col(fill = gold, width = .65) +
      geom_text(
        aes(label = .data$points),
        hjust = -0.15,
        color = navy,
        fontface = "bold"
      ) +
      coord_flip(clip = "off") +
      labs(x = NULL, y = "Points") +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = navy),
        plot.margin = margin(10, 30, 10, 10)
      )
  })

  # Compact team-level summary table.
  output$team_summary <- renderDT({
    d <- data()
    gp <- max(d$games_played, na.rm = TRUE)
    tbl <- data.frame(
      Metric = c(
        "Goals per Game", "Points per Game", "Shots per Game",
        "Hits per Game", "Average TOI / Game", "Fenwick Share"
      ),
      Value = c(
        round(safe_div(sum(d$goals, na.rm = TRUE), gp), 2),
        round(safe_div(sum(d$points, na.rm = TRUE), gp), 2),
        round(safe_div(sum(d$shots, na.rm = TRUE), gp), 2),
        round(safe_div(sum(d$hits, na.rm = TRUE), gp), 2),
        paste0(round(mean(d$toi_per_game_minutes, na.rm = TRUE), 1), " min"),
        paste0(
          round(
            safe_div(
              sum(d$fenwick_for, na.rm = TRUE),
              sum(d$fenwick_for + d$fenwick_against, na.rm = TRUE)
            ) * 100,
            1
          ),
          "%"
        )
      )
    )
    datatable(tbl, rownames = FALSE, options = list(dom = "t", paging = FALSE))
  })

  # Roster composition by position.
  output$position_plot <- renderPlot({
    p <- data() |> count(.data$position)
    ggplot(p, aes(.data$position, .data$n, fill = .data$position)) +
      geom_col(width = .65, show.legend = FALSE) +
      geom_text(
        aes(label = .data$n),
        vjust = -0.4,
        color = navy,
        fontface = "bold"
      ) +
      scale_fill_manual(values = c("F" = gold, "D" = navy, "G" = "#6D95C4")) +
      labs(x = NULL, y = "Players") +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = navy)
      )
  })

  # Quick team profile comparison using core aggregate metrics.
  output$team_profile_plot <- renderPlot({
    d <- data()
    gp <- max(d$games_played, na.rm = TRUE)
    p <- data.frame(
      metric = c(
        "Goals / Game", "Shots / Game", "Hits / Game", "Faceoff Win %"
      ),
      value = c(
        safe_div(sum(d$goals), gp),
        safe_div(sum(d$shots), gp),
        safe_div(sum(d$hits), gp),
        mean(d$faceoffs_won_pct[d$faceoffs > 0], na.rm = TRUE)
      )
    )
    ggplot(p, aes(.data$metric, .data$value, fill = .data$metric)) +
      geom_col(show.legend = FALSE, width = .65) +
      scale_fill_manual(values = c(gold, navy, "#6D95C4", "#B89414")) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = navy, face = "bold")
      )
  })

  # Hero panel for the selected player, including image and headline KPIs.
  output$player_hero <- renderUI({
    p <- player_row()
    req(nrow(p) == 1)
    img <- find_player_image(p$player)
    initials <- paste(substr(strsplit(p$player, " ")[[1]], 1, 1), collapse = "")
    pm_text <- if (p$plus_minus > 0) {
      paste0("+", p$plus_minus)
    } else {
      as.character(p$plus_minus)
    }
    div(
      class = "player-hero",
      div(
        class = "player-photo",
        if (!is.null(img)) {
          tags$img(src = img)
        } else {
          div(class = "player-placeholder", initials)
        }
      ),
      div(
        style = "flex:1;",
        div(
          style = "display:flex;align-items:center;gap:14px;",
          div(class = "player-num", p$shirt_number),
          div(class = "player-name", p$player)
        ),
        div(
          class = "player-tags",
          span(p$position),
          span(paste0("#", p$shirt_number))
        ),
        div(
          class = "player-kpis",
          div(
            class = "player-kpi",
            div(class = "k", "Games Played"),
            div(class = "v", p$games_played)
          ),
          div(
            class = "player-kpi",
            div(class = "k", "Points"),
            div(class = "v", p$points)
          ),
          div(
            class = "player-kpi",
            div(class = "k", "Plus / Minus"),
            div(
              class = "v",
              style = paste0(
                "color:",
                if (p$plus_minus >= 0) green else red,
                ";"
              ),
              pm_text
            )
          ),
          div(
            class = "player-kpi",
            div(class = "k", "TOI / Game"),
            div(class = "v", sprintf("%.1f min", p$toi_per_game_minutes))
          )
        )
      )
    )
  })

  # Detailed stat table for the selected player.
  output$player_table <- renderDT({
    p <- player_row()
    player_name <- p$player[[1]]
    stat_defs <- data.frame(
      Category = c(
        "Time on ice", "Games played", "TOI / Game", "All shifts", "Goals",
        "First assist", "Second assist", "Points", "+/-", "Shots",
        "Shots on goal", "Scoring chances", "Hits", "Shots blocking",
        "Penalties", "Penalties drawn", "Faceoffs", "Faceoffs won",
        "Faceoffs lost", "Faceoffs won, %", "Fenwick for",
        "Fenwick against", "Takeaways", "Puck losses", "Entries",
        "Breakouts"
      ),
      Metric = c(
        "time_on_ice", "games_played", "toi_per_game_minutes",
        "all_shifts", "goals",
        "first_assist", "second_assist", "points", "plus_minus",
        "shots", "shots_on_goal", "scoring_chances", "hits",
        "shots_blocking", "penalties", "penalties_drawn", "faceoffs",
        "faceoffs_won", "faceoffs_lost", "faceoffs_won_pct",
        "fenwick_for", "fenwick_against", "takeaways", "puck_losses",
        "entries", "breakouts"
      ),
      stringsAsFactors = FALSE
    )
    tbl <- data.frame(
      Category = stat_defs$Category,
      Value = vapply(
        stat_defs$Metric,
        function(metric) {
          format_stat_value(metric, p[[metric]][[1]])
        },
        character(1)
      ),
      `Squad Ranking` = vapply(
        stat_defs$Metric,
        function(metric) {
          squad_rank_label(player_name, metric)
        },
        character(1)
      )
    )
    datatable(tbl, rownames = FALSE, options = list(dom = "t", paging = FALSE))
  })

  # Circular player profile graphic for the selected stat group.
  output$player_plot <- renderPlot({
    prof <- player_profile_data()
    p <- player_row()
    center_name <- paste(strwrap(p$player[[1]], width = 12), collapse = "\n")
    center_number <- paste0("#", p$shirt_number[[1]])
    palette_vals <- rep(
      c(
        gold, navy, "#B89414", "#6D95C4",
        "#7DA2C8", "#114A82", "#3B6EA8", "#8DA9CB"
      ),
      length.out = nrow(prof)
    )
    ring_breaks <- data.frame(
      ring = c(0.28, 0.48, 0.68, 0.9),
      x = 1
    )

    ggplot(prof, aes(.data$idx, .data$radius, group = 1)) +
      geom_hline(
        data = ring_breaks,
        aes(yintercept = .data$ring),
        color = border,
        linewidth = 0.35,
        inherit.aes = FALSE
      ) +
      geom_segment(
        aes(
          xend = .data$idx,
          y = 0.28,
          yend = .data$radius
        ),
        color = navy,
        linewidth = 2.8,
        alpha = 0.22,
        lineend = "round"
      ) +
      geom_polygon(
        fill = gold,
        alpha = 0.18,
        color = gold,
        linewidth = 1.2
      ) +
      geom_point(
        aes(fill = .data$metric),
        size = 5.6,
        shape = 21,
        stroke = 1.2,
        color = white
      ) +
      geom_text(
        aes(y = .data$label_radius, label = .data$metric),
        color = navy,
        fontface = "bold",
        size = 3.6,
        lineheight = 0.92
      ) +
      geom_label(
        aes(y = .data$value_radius, label = .data$display_value),
        size = 3.1,
        fill = white,
        color = navy,
        label.size = 0,
        fontface = "bold",
        label.r = grid::unit(0.18, "lines")
      ) +
      annotate(
        "text",
        x = mean(prof$idx),
        y = 0.2,
        label = center_number,
        color = gold,
        fontface = "bold",
        size = 7.2,
        lineheight = 1
      ) +
      annotate(
        "text",
        x = mean(prof$idx),
        y = 0.01,
        label = center_name,
        color = navy,
        fontface = "bold",
        size = 3.6,
        lineheight = 1.05
      ) +
      scale_fill_manual(values = setNames(palette_vals, prof$metric)) +
      coord_polar(clip = "off") +
      scale_y_continuous(limits = c(0, 1.14)) +
      labs(x = NULL, y = NULL) +
      theme_void(base_size = 13) +
      theme(
        legend.position = "none",
        plot.margin = margin(30, 34, 24, 34)
      )
  })

  # Explain how to read the player profile graphic.
  output$player_profile_explainer <- renderUI({
    p <- player_row()
    req(nrow(p) == 1, input$player_profile_group)
    group_explainer <- switch(input$player_profile_group,
      scoring = paste(
        "This view highlights scoring production and finishing support.",
        "Longer spokes mean", p$player[[1]],
        "is closer to the squad leader in goals, assists, points,",
        "or plus/minus."
      ),
      shooting = paste(
        "This view focuses on shot generation and offensive chance volume.",
        "Use it to see how", p$player[[1]],
        "compares in creating pressure and directing pucks toward goal."
      ),
      physical = paste(
        "This view summarizes physical play and discipline.",
        "It shows how", p$player[[1]],
        "stacks up in hits, blocks, penalties, and penalties drawn."
      ),
      faceoffs = paste(
        "This view isolates puck-drop performance.",
        "It compares", p$player[[1]],
        "in total draws, wins, losses, and win percentage against teammates."
      ),
      puck_management = paste(
        "This view looks at puck security and recovery in each zone.",
        "Longer spokes indicate stronger takeaway volume or higher puck-loss",
        "counts relative to the squad."
      ),
      transitions = paste(
        "This view shows transition impact.",
        "It helps explain how", p$player[[1]],
        "contributes to entries, breakouts, and overall territorial play."
      ),
      usage = paste(
        "This view summarizes workload and deployment.",
        "It shows how heavily", p$player[[1]],
        "is used through time on ice, shifts, games played, and scoring rate."
      ),
      "This circular profile compares the selected player to the squad."
    )
    div(
      class = "note",
      style = "margin-top:10px;margin-bottom:0;",
      paste(
        group_explainer,
        "Each spoke is one statistic, the label bubble shows the actual",
        "value, and a longer spoke means the player is closer to the",
        "best mark on the current squad for that stat."
      )
    )
  })

  # Comparison chart using the currently selected metric.
  output$compare_plot <- renderPlot({
    req(length(input$compare_players) > 0)
    p <- data() |>
      filter(.data$player %in% input$compare_players) |>
      mutate(player = factor(.data$player, levels = input$compare_players))
    ggplot(
      p,
      aes(
        .data$player,
        .data[[input$compare_metric]],
        fill = .data$player
      )
    ) +
      geom_col(width = .65, show.legend = FALSE) +
      geom_text(
        aes(label = round(.data[[input$compare_metric]], 2)),
        hjust = -0.15,
        color = navy,
        fontface = "bold"
      ) +
      scale_fill_manual(values = c(gold, navy, "#6D95C4")) +
      coord_flip(clip = "off") +
      labs(
        x = NULL,
        y = names(metric_choices[metric_choices == input$compare_metric])
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = navy, face = "bold"),
        plot.margin = margin(10, 30, 10, 10)
      )
  })

  # Comparison table for selected players, optionally transposed so
  # players appear as columns instead of rows.
  output$compare_table <- renderDT({
    req(length(input$compare_players) > 0)
    tbl <- data() |>
      filter(.data$player %in% input$compare_players) |>
      transmute(
        Player = .data$player,
        `Shirt number` = .data$shirt_number_numeric,
        Position = .data$position,
        `Time on ice (min)` = round(.data$time_on_ice_minutes, 1),
        `Games played` = .data$games_played,
        `All shifts` = .data$all_shifts,
        Goals = .data$goals,
        `First assist` = .data$first_assist,
        `Second assist` = .data$second_assist,
        Assists = .data$total_assists,
        Points = .data$points,
        `+/-` = .data$plus_minus,
        Plus = .data$plus,
        Minus = .data$minus,
        Shots = .data$shots,
        `Shots on goal` = .data$shots_on_goal,
        `Scoring chances` = .data$scoring_chances,
        Hits = .data$hits,
        `Shots blocking` = .data$shots_blocking,
        Penalties = .data$penalties,
        `Penalties drawn` = .data$penalties_drawn,
        `Penalty time (min)` = round(.data$penalty_time_minutes, 1),
        Faceoffs = .data$faceoffs,
        `Faceoffs won` = .data$faceoffs_won,
        `Faceoffs lost` = .data$faceoffs_lost,
        `Faceoffs won, %` = round(.data$faceoffs_won_pct, 1),
        `Fenwick for` = .data$fenwick_for,
        `Fenwick against` = .data$fenwick_against,
        Takeaways = .data$takeaways,
        `Takeaways in DZ` = .data$takeaways_dz,
        `Takeaways in NZ` = .data$takeaways_nz,
        `Takeaways in OZ` = .data$takeaways_oz,
        `Puck losses` = .data$puck_losses,
        `Puck losses in DZ` = .data$puck_losses_dz,
        `Puck losses in NZ` = .data$puck_losses_nz,
        `Puck losses in OZ` = .data$puck_losses_oz,
        Entries = .data$entries,
        Breakouts = .data$breakouts,
        `Points / Game` = round(.data$points_per_game, 2),
        `TOI / Game (min)` = round(.data$toi_per_game_minutes, 1)
      )
    if (isTRUE(input$transpose_compare)) {
      tbl_matrix <- as.data.frame(
        t(as.matrix(tbl[, -1, drop = FALSE])),
        stringsAsFactors = FALSE
      )
      colnames(tbl_matrix) <- tbl$Player
      tbl_matrix <- cbind(
        Metric = rownames(tbl_matrix),
        tbl_matrix,
        row.names = NULL
      )
      datatable(
        tbl_matrix,
        rownames = FALSE,
        options = list(dom = "tip", paging = FALSE, scrollX = TRUE),
        class = "display compare-bordered"
      )
    } else {
      datatable(
        tbl,
        rownames = FALSE,
        options = list(dom = "tip", paging = FALSE, scrollX = TRUE),
        class = "display compare-bordered"
      )
    }
  })

  # Status callout explaining the current source state.
  output$data_source_status <- renderUI({
    message_text <- if (identical(input$data_source, "live")) {
      live_status_text()
    } else {
      status_text()
    }
    div(class = "note", style = "margin-bottom:0;", message_text)
  })

  # Deployment note changes based on whether the app is local or hosted.
  output$deployment_note <- renderUI({
    if (can_persist_local_files) {
      HTML(
        paste(
          "Upload a new CSV to refresh the dashboard data.",
          "The app validates the columns, loads the file, and saves",
          "a clean version back to <code>boxscore.csv</code>."
        )
      )
    } else {
      HTML(
        paste(
          "Hosted deployment mode detected. CSV uploads update the",
          "current app session only, and player image uploads are",
          "not persisted yet. For public deployment, connect these",
          "features to external storage such as S3, Azure Blob",
          "Storage, or a database-backed file service."
        )
      )
    }
  })

  # Visual badge showing whether the dashboard is using uploaded or live data.
  output$source_badge <- renderUI({
    if (identical(input$data_source, "live")) {
      div(class = "source-badge live", "Live Data", "Current Season Feed")
    } else {
      div(class = "source-badge", "Uploaded Data", "CSV Source")
    }
  })

  # Last-refresh timestamp for the active data source.
  output$source_time <- renderUI({
    ts <- if (identical(input$data_source, "live")) {
      live_timestamp()
    } else {
      uploaded_timestamp()
    }
    label <- if (is.null(ts)) {
      "Not refreshed yet"
    } else {
      format(ts, "%Y-%m-%d %I:%M %p")
    }
    div(class = "source-time", paste("Last refreshed:", label))
  })

  # Upload tab helpers: current status, required schema, and a quick preview.
  output$upload_status <- renderUI(div(class = "note", status_text()))
  output$required_columns <- renderText(paste(expected_cols, collapse = "\n"))
  output$upload_preview <- renderDT({
    datatable(
      data() |> transmute(
        `#` = .data$shirt_number,
        Player = .data$player,
        Pos = .data$position,
        GP = .data$games_played,
        Goals = .data$goals,
        Assists = .data$total_assists,
        Points = .data$points
      ),
      rownames = FALSE, options = list(pageLength = 8)
    )
  })

  # Download a template built from the current schema.
  output$download_template <- downloadHandler(
    filename = function() "ubc-hockey-template.csv",
    content = function(file) {
      out <- data()[, unname(rename_map[expected_cols]), drop = FALSE]
      names(out) <- expected_cols
      write.csv(out, file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)
