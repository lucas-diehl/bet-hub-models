import fs from "node:fs/promises";
import path from "node:path";
import { SpreadsheetFile, Workbook } from "@oai/artifact-tool";

const root = process.cwd();
const outputDir = path.join(root, "outputs", "dfs_value_backtest");
await fs.mkdir(outputDir, { recursive: true });

function parseCSV(text) {
  const rows = [];
  let row = [];
  let field = "";
  let quoted = false;
  for (let i = 0; i < text.length; i += 1) {
    const c = text[i];
    if (quoted) {
      if (c === '"' && text[i + 1] === '"') {
        field += '"';
        i += 1;
      } else if (c === '"') {
        quoted = false;
      } else {
        field += c;
      }
    } else if (c === '"') {
      quoted = true;
    } else if (c === ",") {
      row.push(field);
      field = "";
    } else if (c === "\n") {
      row.push(field.replace(/\r$/, ""));
      rows.push(row);
      row = [];
      field = "";
    } else {
      field += c;
    }
  }
  if (field.length || row.length) {
    row.push(field.replace(/\r$/, ""));
    rows.push(row);
  }
  return rows.map((values, rowIndex) => values.map((value) => {
    if (rowIndex === 0) return value;
    if (value === "" || value === "NA") return null;
    if (value === "TRUE") return true;
    if (value === "FALSE") return false;
    if (/^-?(?:\d+\.?\d*|\.\d+)(?:e[+-]?\d+)?$/i.test(value)) {
      return Number(value);
    }
    return value;
  }));
}

async function csv(name) {
  return parseCSV(await fs.readFile(path.join(root, "outputs", name), "utf8"));
}

function records(rows) {
  const headers = rows[0];
  return rows.slice(1).map((values) => Object.fromEntries(
    headers.map((header, index) => [header, values[index] ?? null]),
  ));
}

function colLetter(indexZeroBased) {
  let n = indexZeroBased + 1;
  let result = "";
  while (n > 0) {
    result = String.fromCharCode(65 + ((n - 1) % 26)) + result;
    n = Math.floor((n - 1) / 26);
  }
  return result;
}

const comparisonRows = await csv("dfs_value_lineup_comparison.csv");
const seasonRows = await csv("dfs_value_season_comparison.csv");
const projectionRows = await csv("dfs_value_projection_metrics.csv");
const decileRows = await csv("dfs_value_by_decile.csv");
const positionRows = await csv("dfs_value_by_position.csv");
const stableRows = await csv("dfs_value_stable_interactions.csv");
const interactionRows = await csv("dfs_value_interactions.csv");
const interactionSeasonRows = await csv("dfs_value_interactions_by_season.csv");
const selectionRows = await csv("dfs_value_selection_comparison.csv");
const weeklyRows = await csv("dfs_value_weekly_lineups.csv");
const lineupPlayerRows = await csv("dfs_value_lineup_players.csv");
const matchRows = await csv("dfs_value_match_audit.csv");
const checkRows = await csv("dfs_value_checks.csv");
const boardRaw = await csv("dfs_value_player_board.csv");

const comparison = records(comparisonRows)[0];
const seasonRecords = records(seasonRows);
const projectionRecords = records(projectionRows);
const stableRecords = records(stableRows);

const boardKeep = [
  "season", "week", "game_date", "player_display_name", "position",
  "team", "opponent_team", "salary", "fantasy_points", "projected_ppr",
  "baseline_ppr", "salary_expected_points", "model_salary_edge",
  "baseline_salary_edge", "actual_salary_edge", "model_advantage",
  "model_points_per_1k", "actual_points_per_1k", "hit_3x", "hit_4x",
  "hit_5x", "salary_tier", "favorite_status", "total_tier",
  "weather_tier", "model_value_decile", "model_position_rank",
];
const boardHeaders = boardRaw[0];
const boardIndices = boardKeep.map((header) => boardHeaders.indexOf(header));
const boardRows = [
  boardKeep,
  ...boardRaw.slice(1).map((row) => boardIndices.map((index) => row[index])),
];

const navy = "#15324B";
const teal = "#197278";
const blue = "#2F6690";
const white = "#FFFFFF";
const ink = "#17212B";
const paleBlue = "#EAF2F7";
const paleGreen = "#E8F4EA";
const paleYellow = "#FFF4D6";
const paleRed = "#FBE9E7";
const lightGray = "#F3F5F7";
const border = "#CBD5DF";

const workbook = Workbook.create();
const dashboard = workbook.worksheets.add("Dashboard");
const lineup = workbook.worksheets.add("Lineup Results");
const value = workbook.worksheets.add("Value Signal");
const interactions = workbook.worksheets.add("Interactions");
const projection = workbook.worksheets.add("Projection Accuracy");
const weekly = workbook.worksheets.add("Weekly Lineups");
const lineupPlayers = workbook.worksheets.add("Lineup Players");
const playerBoard = workbook.worksheets.add("Player Value Board");
const audit = workbook.worksheets.add("Audit");
const methods = workbook.worksheets.add("Methods");

for (const sheet of [
  dashboard, lineup, value, interactions, projection, weekly,
  lineupPlayers, playerBoard, audit, methods,
]) {
  sheet.showGridLines = false;
}

function titleBand(sheet, lastCol, title, subtitle) {
  sheet.getRange(`A1:${lastCol}2`).merge();
  sheet.getRange(`A1:${lastCol}2`).values = [[title]];
  sheet.getRange(`A1:${lastCol}2`).format = {
    fill: navy,
    font: { color: white, bold: true, size: 20, name: "Aptos Display" },
    verticalAlignment: "center",
  };
  sheet.getRange(`A3:${lastCol}3`).merge();
  sheet.getRange(`A3:${lastCol}3`).values = [[subtitle]];
  sheet.getRange(`A3:${lastCol}3`).format = {
    fill: paleBlue,
    font: { color: ink, italic: true, size: 10, name: "Aptos" },
    verticalAlignment: "center",
  };
}

function section(sheet, range, text) {
  sheet.getRange(range).merge();
  sheet.getRange(range).values = [[text]];
  sheet.getRange(range).format = {
    fill: teal,
    font: { color: white, bold: true, size: 11 },
    verticalAlignment: "center",
  };
}

function header(sheet, range) {
  sheet.getRange(range).format = {
    fill: navy,
    font: { color: white, bold: true, size: 9 },
    verticalAlignment: "center",
    wrapText: true,
    borders: { bottom: { color: border, style: "continuous", weight: 1 } },
  };
}

function card(sheet, valueRange, labelRange, label, formula, format, fill = paleGreen) {
  sheet.getRange(labelRange).merge();
  sheet.getRange(labelRange).values = [[label]];
  sheet.getRange(labelRange).format = {
    fill: navy,
    font: { color: white, bold: true, size: 9 },
    horizontalAlignment: "center",
    verticalAlignment: "center",
  };
  sheet.getRange(valueRange).merge();
  sheet.getRange(valueRange).formulas = [[formula]];
  sheet.getRange(valueRange).format = {
    fill,
    font: { color: navy, bold: true, size: 18 },
    horizontalAlignment: "center",
    verticalAlignment: "center",
    numberFormat: format,
    borders: {
      top: { color: border, style: "continuous", weight: 1 },
      bottom: { color: border, style: "continuous", weight: 1 },
      left: { color: border, style: "continuous", weight: 1 },
      right: { color: border, style: "continuous", weight: 1 },
    },
  };
}

function writeRawSheet(sheet, rows, title, subtitle, widths = {}) {
  const lastCol = colLetter(rows[0].length - 1);
  titleBand(sheet, lastCol, title, subtitle);
  sheet.getRange(`A5:${lastCol}${rows.length + 4}`).values = rows;
  header(sheet, `A5:${lastCol}5`);
  sheet.getRange(`A6:${lastCol}${rows.length + 4}`).format = {
    font: { color: ink, size: 9, name: "Aptos" },
    borders: { bottom: { color: "#E8EDF1", style: "continuous", weight: 1 } },
  };
  Object.entries(widths).forEach(([columns, width]) => {
    sheet.getRange(columns).format.columnWidth = width;
  });
  sheet.freezePanes.freezeRows(5);
  sheet.getRange(`A1:${lastCol}${Math.min(rows.length + 3, 50)}`).format.font.name = "Aptos";
}

titleBand(
  dashboard,
  "N",
  "NFL DFS Value Backtest | 2017-2021",
  "Walk-forward player projections, reconstructed DraftKings Sunday main slates, and legal $50K lineup optimization",
);
card(dashboard, "A6:C8", "A5:C5", "MODEL AVG SCORE", "='Lineup Results'!B7", "0.00");
card(dashboard, "D6:F8", "D5:F5", "BASELINE AVG SCORE", "='Lineup Results'!C7", "0.00", paleBlue);
card(dashboard, "G6:I8", "G5:I5", "AVG WEEKLY LIFT", "='Lineup Results'!D7", "+0.00;-0.00");
card(dashboard, "J6:L8", "J5:L5", "MODEL WIN RATE", "='Lineup Results'!I7", "0.0%");
card(dashboard, "M6:N8", "M5:N5", "TEST SLATES", "='Lineup Results'!A7", "0", paleBlue);

section(dashboard, "A10:N10", "Decision for 2026");
dashboard.getRange("A11:N14").merge();
dashboard.getRange("A11:N14").values = [[
  "USE WITH GUARDRAILS. The model improved optimized lineup scoring across the full test and beat the rolling baseline in four of five seasons. It should be used as a projection and lineup-construction input, not as proof of contest ROI. The missing ownership, cash-line, entry-fee, and payout history prevents an honest historical dollar-ROI estimate.",
]];
dashboard.getRange("A11:N14").format = {
  fill: paleGreen,
  font: { color: "#245B33", bold: true, size: 11 },
  wrapText: true,
  verticalAlignment: "center",
  borders: {
    top: { color: border, style: "continuous", weight: 1 },
    bottom: { color: border, style: "continuous", weight: 1 },
    left: { color: border, style: "continuous", weight: 1 },
    right: { color: border, style: "continuous", weight: 1 },
  },
};

section(dashboard, "A16:G16", "Evidence that held up");
dashboard.getRange("A17:G22").values = [
  ["Finding", "Evidence", null, null, null, null, null],
  ["Projection quality", "Model MAE beat the rolling baseline in every test season.", null, null, null, null, null],
  ["Lineup construction", `+${comparison.average_lift.toFixed(2)} points/week; ${comparison.model_wins}-${comparison.baseline_wins} weekly record.`, null, null, null, null, null],
  ["Uncertainty", `Paired bootstrap 95% interval: +${comparison.lift_ci_low.toFixed(2)} to +${comparison.lift_ci_high.toFixed(2)} points.`, null, null, null, null, null],
  ["Stable value tie-breaker", "$4K-$5.4K WRs in the top 20% of model salary edge beat their position benchmark in 5/5 seasons.", null, null, null, null, null],
  ["Stable game-context tie-breaker", "RBs favored by 3+ in the top 20% of model salary edge beat their position benchmark in 5/5 seasons.", null, null, null, null, null],
];
for (let row = 17; row <= 22; row += 1) dashboard.getRange(`B${row}:G${row}`).merge();
header(dashboard, "A17:G17");
dashboard.getRange("A18:G22").format = {
  wrapText: true,
  verticalAlignment: "top",
  borders: { bottom: { color: "#E8EDF1", style: "continuous", weight: 1 } },
};

section(dashboard, "I16:N16", "Guardrails");
dashboard.getRange("I17:N22").values = [
  ["Risk", "Operating response", null, null, null, null],
  ["2021 regime failure", "Monitor model-vs-baseline lift weekly; reduce reliance when recent calibration deteriorates.", null, null, null, null],
  ["No contest ROI archive", "Archive ownership and contest results in 2026 before evaluating bankroll deployment.", null, null, null, null],
  ["Standalone edge ranking failed", "Do not blindly play the largest model salary edges; let the optimizer build the full lineup.", null, null, null, null],
  ["2017-2021 salary window", "Paper-test or use small stakes until the current-season process is validated.", null, null, null, null],
  ["Late news", "Regenerate after injury/inactive news and retain timestamped pre-lock files.", null, null, null, null],
];
for (let row = 17; row <= 22; row += 1) dashboard.getRange(`J${row}:N${row}`).merge();
header(dashboard, "I17:N17");
dashboard.getRange("I18:N22").format = {
  wrapText: true,
  verticalAlignment: "top",
  borders: { bottom: { color: "#E8EDF1", style: "continuous", weight: 1 } },
};

section(dashboard, "A24:N24", "Season stability");
dashboard.getRange("A25:I30").values = [
  seasonRows[0],
  ...seasonRows.slice(1),
];
header(dashboard, "A25:I25");
dashboard.getRange("C26:E30").format.numberFormat = "0.00";
dashboard.getRange("I26:I30").format.numberFormat = "0.0%";
dashboard.getRange("E26:E30").conditionalFormats.add("cellValue", {
  operator: "lessThan",
  formula: 0,
  format: { fill: paleRed, font: { color: "#8A1C1C", bold: true } },
});
dashboard.getRange("P25:R30").values = [
  ["Season", "Model", "Baseline"],
  ...seasonRecords.map((row) => [row.season, row.model_score, row.baseline_score]),
];
const seasonChart = dashboard.charts.add("column", dashboard.getRange("P25:R30"));
seasonChart.title = "Average lineup score by season";
seasonChart.hasLegend = true;
seasonChart.setPosition("J24", "N34");
dashboard.getRange("A:A").format.columnWidth = 18;
dashboard.getRange("B:B").format.columnWidth = 18;
dashboard.getRange("C:N").format.columnWidth = 12;
dashboard.getRange("A11:N14").format.rowHeight = 24;
dashboard.getRange("A17:N22").format.rowHeight = 34;
dashboard.freezePanes.freezeRows(4);

titleBand(
  lineup,
  "L",
  "Lineup Backtest Results",
  "Model versus rolling baseline; bootstrap resamples paired weekly score differences",
);
section(lineup, "A5:L5", "Overall comparison");
lineup.getRange("A6:L7").values = comparisonRows;
header(lineup, "A6:L6");
lineup.getRange("B7:E7").format.numberFormat = "0.00";
lineup.getRange("I7:L7").format.numberFormat = "0.0%";
section(lineup, "A9:I9", "By season");
lineup.getRange("A10:I15").values = seasonRows;
header(lineup, "A10:I10");
lineup.getRange("C11:F15").format.numberFormat = "0.00";
lineup.getRange("I11:I15").format.numberFormat = "0.0%";
lineup.getRange("N10:P15").values = [
  ["Season", "Model", "Baseline"],
  ...seasonRecords.map((row) => [row.season, row.model_score, row.baseline_score]),
];
const lineupChart = lineup.charts.add("column", lineup.getRange("N10:P15"));
lineupChart.title = "Model vs baseline lineup score";
lineupChart.hasLegend = true;
lineupChart.setPosition("A18", "H34");
lineup.getRange("A:A").format.columnWidth = 12;
lineup.getRange("B:L").format.columnWidth = 16;
lineup.freezePanes.freezeRows(5);

titleBand(
  value,
  "T",
  "Value Signal",
  "Decile 1 is the largest model salary edge within position and week",
);
section(value, "A5:L5", "Model salary-edge deciles");
value.getRange(`A6:L${decileRows.length + 5}`).values = decileRows;
header(value, "A6:L6");
value.getRange(`D7:H${decileRows.length + 5}`).format.numberFormat = "0.00";
value.getRange(`I7:K${decileRows.length + 5}`).format.numberFormat = "0.0%";
value.getRange(`L7:L${decileRows.length + 5}`).format.numberFormat = "0.00";
const decileRecords = records(decileRows);
value.getRange("O5:P15").values = [
  ["Value decile", "Actual points/$1K"],
  ...decileRecords.map((row) => [
    row.model_value_decile,
    row.actual_points_per_1k,
  ]),
];
const valueChart = value.charts.add("line", value.getRange("O5:P15"));
valueChart.title = "Actual points per $1K by model value decile";
valueChart.hasLegend = true;
valueChart.setPosition("L5", "T20");
section(value, "A19:M19", "Top-three value selections: model versus rolling baseline");
value.getRange(`A20:M${selectionRows.length + 19}`).values = selectionRows;
header(value, "A20:M20");
value.getRange(`E21:I${selectionRows.length + 19}`).format.numberFormat = "0.00";
value.getRange(`J21:L${selectionRows.length + 19}`).format.numberFormat = "0.0%";
value.getRange(`M21:M${selectionRows.length + 19}`).format.numberFormat = "0.00";
value.getRange("A:A").format.columnWidth = 20;
value.getRange("B:K").format.columnWidth = 15;
value.freezePanes.freezeRows(6);

titleBand(
  interactions,
  "L",
  "Interaction Evidence",
  "Groups below are restricted to the top 20% of model salary-edge candidates",
);
section(interactions, "A5:L5", "Stable interactions ranked by season consistency");
interactions.getRange(`A6:L${stableRows.length + 5}`).values = stableRows;
header(interactions, "A6:L6");
interactions.getRange(`G7:G${stableRows.length + 5}`).format.numberFormat = "0";
interactions.getRange(`H7:J${stableRows.length + 5}`).format.numberFormat = "0.000";
interactions.getRange(`K7:L${stableRows.length + 5}`).format.numberFormat = "0.0%";
section(interactions, `A${stableRows.length + 7}:L${stableRows.length + 7}`, "All aggregated interaction groups");
const allStart = stableRows.length + 8;
interactions.getRange(`A${allStart}:L${allStart + interactionRows.length - 1}`).values =
  interactionRows.map((row) => row.slice(0, 12));
header(interactions, `A${allStart}:L${allStart}`);
interactions.getRange("A:A").format.columnWidth = 20;
interactions.getRange("B:F").format.columnWidth = 16;
interactions.getRange("G:L").format.columnWidth = 15;
interactions.freezePanes.freezeRows(6);

titleBand(
  projection,
  "G",
  "Projection Accuracy",
  "Player-game walk-forward scoring: model and baseline are evaluated against realized DraftKings points",
);
projection.getRange(`A5:G${projectionRows.length + 4}`).values = projectionRows;
header(projection, "A5:G5");
projection.getRange(`C6:G${projectionRows.length + 4}`).format.numberFormat = "0.000";
projection.getRange("I5:K10").values = [
  ["Season", "Model MAE", "Baseline MAE"],
  ...projectionRecords.map((row) => [
    row.season,
    row.model_mae,
    row.baseline_mae,
  ]),
];
const projectionChart = projection.charts.add("line", projection.getRange("I5:K10"));
projectionChart.title = "MAE by season";
projectionChart.hasLegend = true;
projectionChart.setPosition("A13", "G29");
projection.getRange("A:B").format.columnWidth = 15;
projection.getRange("C:G").format.columnWidth = 22;
projection.freezePanes.freezeRows(5);

writeRawSheet(
  weekly,
  weeklyRows,
  "Weekly Lineup History",
  "Every reconstructed slate and strategy; use model vs baseline for honest forward comparison",
  { "A:B": 10, "C:C": 14, "D:G": 18 },
);
weekly.getRange(`D6:F${weeklyRows.length + 4}`).format.numberFormat = "0.00";

writeRawSheet(
  lineupPlayers,
  lineupPlayerRows,
  "Lineup Player Detail",
  "Nine legal roster slots for each model, baseline, and hindsight candidate-pool lineup",
  { "A:B": 10, "C:C": 14, "D:D": 23, "E:G": 12, "H:N": 16 },
);

writeRawSheet(
  playerBoard,
  boardRows,
  "Player Value Board",
  "Matched DraftKings Sunday main-slate player-games with projections, salary edge, context, and realized outcomes",
  { "A:C": 11, "D:D": 23, "E:G": 12, "H:P": 15, "Q:AA": 16 },
);
playerBoard.getRange(`H6:R${boardRows.length + 4}`).format.numberFormat = "0.00";
playerBoard.getRange(`S6:U${boardRows.length + 4}`).format.numberFormat = "0.0%";

titleBand(
  audit,
  "F",
  "Audit & Validation",
  "Salary coverage and optimizer constraints checked before conclusions are reported",
);
section(audit, "A5:D5", "Automated checks");
audit.getRange(`A6:D${checkRows.length + 5}`).values = checkRows;
header(audit, "A6:D6");
audit.getRange("D7:D14").conditionalFormats.add("containsText", {
  text: "PASS",
  format: { fill: paleGreen, font: { color: "#245B33", bold: true } },
});
section(audit, "A17:D17", "Salary match audit");
audit.getRange(`A18:D${matchRows.length + 17}`).values = matchRows;
header(audit, "A18:D18");
audit.getRange(`D19:D${matchRows.length + 17}`).format.numberFormat = "0.0%";
audit.getRange("A:A").format.columnWidth = 34;
audit.getRange("B:D").format.columnWidth = 18;
audit.freezePanes.freezeRows(5);

titleBand(
  methods,
  "H",
  "Method, Interpretation, and 2026 Operating Plan",
  "This workbook separates projection usefulness from claims that require contest-level payout data",
);
section(methods, "A5:H5", "Backtest design");
methods.getRange("A6:H13").values = [
  ["Element", "Implementation", null, null, null, null, null, null],
  ["Seasons", "Walk-forward tests for 2017-2021; only earlier seasons train each test season.", null, null, null, null, null, null],
  ["Slate", "Reconstructed DraftKings Sunday main slate: 1:00, 4:05, and 4:25 ET games.", null, null, null, null, null, null],
  ["Projection inputs", "Lagged player usage/performance, game-market context, and weather. No same-week outcomes are model inputs.", null, null, null, null, null, null],
  ["Salary expectation", "Position-specific quadratic salary curve trained only on earlier seasons.", null, null, null, null, null, null],
  ["Lineup", "$50K cap; 1 QB, 2-3 RB, 3-4 WR, 1-2 TE, 1 DST; exactly nine unique players.", null, null, null, null, null, null],
  ["DST", "Lagged five-game average DraftKings score.", null, null, null, null, null, null],
  ["Uncertainty", "5,000 paired bootstrap resamples of weekly model-minus-baseline lineup score.", null, null, null, null, null, null],
];
for (let row = 6; row <= 13; row += 1) methods.getRange(`B${row}:H${row}`).merge();
header(methods, "A6:H6");
methods.getRange("A7:H13").format = { wrapText: true, verticalAlignment: "top" };

section(methods, "A15:H15", "What the results do and do not say");
methods.getRange("A16:H20").values = [
  ["Question", "Answer", null, null, null, null, null, null],
  ["Are the projections useful?", "Yes. They improved player MAE in every test season and improved optimized lineup score over the full sample.", null, null, null, null, null, null],
  ["Can we claim historical DFS ROI?", "No. Salaries and fantasy results are available, but ownership, contest cash lines, entry fees, and payouts are not.", null, null, null, null, null, null],
  ["Should largest value edges be auto-played?", "No. Standalone top-edge selections did not beat the baseline list. Evidence is stronger for whole-lineup optimization.", null, null, null, null, null, null],
  ["What is the principal warning?", "2021 underperformed. Recent calibration and a live 2026 archive are required.", null, null, null, null, null, null],
];
for (let row = 16; row <= 20; row += 1) methods.getRange(`B${row}:H${row}`).merge();
header(methods, "A16:H16");
methods.getRange("A17:H20").format = { wrapText: true, verticalAlignment: "top" };

section(methods, "A22:H22", "2026 operating plan");
methods.getRange("A23:H29").values = [
  ["Step", "Action", null, null, null, null, null, null],
  [1, "Archive the official DraftKings main-slate salary CSV before lock.", null, null, null, null, null, null],
  [2, "Generate and preserve timestamped pre-lock projections after final injury and inactive news.", null, null, null, null, null, null],
  [3, "Optimize legal lineups; use stable WR salary and favorite-RB interactions only as tie-breakers.", null, null, null, null, null, null],
  [4, "Retain the rolling baseline and record weekly model-minus-baseline lift.", null, null, null, null, null, null],
  [5, "Archive contest results and ownership; record contest type, entry fee, cash line, payout, and total entries.", null, null, null, null, null, null],
  [6, "Paper-test or use small stakes until live calibration and contest-level ROI are demonstrated.", null, null, null, null, null, null],
];
for (let row = 23; row <= 29; row += 1) methods.getRange(`B${row}:H${row}`).merge();
header(methods, "A23:H23");
methods.getRange("A24:H29").format = { wrapText: true, verticalAlignment: "top" };

section(methods, "A31:H31", "Limitations");
methods.getRange("A32:H37").values = [
  ["Item", "Impact", null, null, null, null, null, null],
  ["Salary availability", "Complete free DraftKings history used here ends in 2021; 2022-2025 lineup validation is unavailable.", null, null, null, null, null, null],
  ["Scoring", "Player projection is standard PPR and does not explicitly model DraftKings three-point yardage bonuses.", null, null, null, null, null, null],
  ["Optimizer", "Candidate pruning makes optimization tractable; the oracle is a hindsight ceiling within that candidate pool.", null, null, null, null, null, null],
  ["Slate reconstruction", "Game times reconstruct the main slate; this is not an archived official contest-slate file.", null, null, null, null, null, null],
  ["ROI", "Without contest-level economics, point lift is a quality indicator, not a bankroll-return estimate.", null, null, null, null, null, null],
];
for (let row = 32; row <= 37; row += 1) methods.getRange(`B${row}:H${row}`).merge();
header(methods, "A32:H32");
methods.getRange("A33:H37").format = { wrapText: true, verticalAlignment: "top" };
methods.getRange("A:A").format.columnWidth = 23;
methods.getRange("B:H").format.columnWidth = 15;
methods.getRange("A6:H37").format.rowHeight = 31;
methods.freezePanes.freezeRows(5);

const inspect = await workbook.inspect({
  kind: "table",
  range: "Dashboard!A1:N34",
  include: "values,formulas",
  tableMaxRows: 34,
  tableMaxCols: 14,
  maxChars: 12000,
});
await fs.writeFile(
  path.join(outputDir, "dashboard_inspect.ndjson"),
  inspect.ndjson,
  "utf8",
);

const errors = await workbook.inspect({
  kind: "match",
  searchTerm: "#REF!|#DIV/0!|#VALUE!|#NAME\\?|#N/A",
  options: { useRegex: true, maxResults: 300 },
  summary: "final formula error scan",
});
await fs.writeFile(
  path.join(outputDir, "formula_error_scan.ndjson"),
  errors.ndjson,
  "utf8",
);

for (const [sheetName, range, fileName] of [
  ["Dashboard", "A1:N34", "dashboard_preview.png"],
  ["Lineup Results", "A1:L34", "lineup_preview.png"],
  ["Value Signal", "A1:T32", "value_preview.png"],
  ["Interactions", "A1:L24", "interactions_preview.png"],
  ["Projection Accuracy", "A1:G29", "projection_preview.png"],
  ["Audit", "A1:F31", "audit_preview.png"],
  ["Methods", "A1:H37", "methods_preview.png"],
]) {
  const preview = await workbook.render({
    sheetName,
    range,
    scale: 1,
    format: "png",
  });
  await fs.writeFile(
    path.join(outputDir, fileName),
    new Uint8Array(await preview.arrayBuffer()),
  );
}

const output = await SpreadsheetFile.exportXlsx(workbook);
await output.save(path.join(outputDir, "NFL_DFS_Value_Backtest_2017_2021.xlsx"));
console.log(
  `Saved DFS value backtest workbook with ${boardRows.length - 1} player-games.`,
);
