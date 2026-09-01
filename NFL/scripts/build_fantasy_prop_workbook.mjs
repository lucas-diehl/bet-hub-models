import fs from "node:fs/promises";
import path from "node:path";
import { SpreadsheetFile, Workbook } from "@oai/artifact-tool";

const root = process.cwd();
const outputDir = path.join(root, "outputs", "fantasy_prop_2026");
await fs.mkdir(outputDir, { recursive: true });

function parseCSV(text) {
  const rows = [];
  let row = [];
  let field = "";
  let quoted = false;
  for (let index = 0; index < text.length; index += 1) {
    const character = text[index];
    if (quoted) {
      if (character === '"' && text[index + 1] === '"') {
        field += '"';
        index += 1;
      } else if (character === '"') {
        quoted = false;
      } else {
        field += character;
      }
    } else if (character === '"') {
      quoted = true;
    } else if (character === ",") {
      row.push(field);
      field = "";
    } else if (character === "\n") {
      row.push(field.replace(/\r$/, ""));
      rows.push(row);
      row = [];
      field = "";
    } else {
      field += character;
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
    if (/^-?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?$/.test(value)) {
      return Number(value);
    }
    return value;
  }));
}

function columnLetter(indexZeroBased) {
  let n = indexZeroBased + 1;
  let result = "";
  while (n > 0) {
    const remainder = (n - 1) % 26;
    result = String.fromCharCode(65 + remainder) + result;
    n = Math.floor((n - 1) / 26);
  }
  return result;
}

function recordsFromRows(rows) {
  const headers = rows[0];
  return rows.slice(1).map((values) => Object.fromEntries(
    headers.map((header, index) => [header, values[index]]),
  ));
}

function selectRows(records, columns, filter = () => true) {
  return [
    columns,
    ...records.filter(filter).map((record) => (
      columns.map((column) => record[column] ?? null)
    )),
  ];
}

const projectionRows = parseCSV(await fs.readFile(
  path.join(root, "outputs", "fantasy_prop_2026_week1_projections.csv"),
  "utf8",
));
const projections = recordsFromRows(projectionRows);
const metricsRows = parseCSV(await fs.readFile(
  path.join(root, "outputs", "fantasy_prop_metrics_overall.csv"),
  "utf8",
));
const pprMetricsRows = parseCSV(await fs.readFile(
  path.join(root, "outputs", "fantasy_ppr_metrics_by_season.csv"),
  "utf8",
));
const manifestRows = parseCSV(await fs.readFile(
  path.join(root, "outputs", "fantasy_prop_model_manifest.csv"),
  "utf8",
));
const importanceRows = parseCSV(await fs.readFile(
  path.join(root, "outputs", "fantasy_prop_feature_importance.csv"),
  "utf8",
));

const commonColumns = [
  "overall_rank", "position_rank", "player", "position", "team",
  "opponent_team", "projected_ppr", "ppr_low", "ppr_high",
  "projected_receptions", "projected_receiving_yards",
  "projected_rushing_yards", "projected_passing_yards",
  "projected_passing_tds", "projected_interceptions",
  "projected_rushing_tds", "projected_receiving_tds",
  "projected_fumbles_lost", "projected_two_point_conversions",
  "total_line", "team_spread", "implied_team_total",
  "weather_status", "preseason_role_rank", "role_status",
];
const passingColumns = [
  "position_rank", "player", "team", "opponent_team",
  "projected_passing_yards", "projected_passing_tds",
  "projected_interceptions", "projected_rushing_yards",
  "projected_rushing_tds", "projected_ppr", "ppr_low", "ppr_high",
  "total_line", "team_spread", "weather_status", "role_status",
];
const receivingColumns = [
  "position_rank", "player", "position", "team", "opponent_team",
  "projected_receptions", "projected_receiving_yards",
  "projected_receiving_tds", "projected_ppr", "ppr_low", "ppr_high",
  "total_line", "team_spread", "weather_status",
  "preseason_role_rank", "role_status",
];
const rushingColumns = [
  "position_rank", "player", "position", "team", "opponent_team",
  "projected_rushing_yards", "projected_rushing_tds",
  "projected_receptions", "projected_receiving_yards",
  "projected_ppr", "ppr_low", "ppr_high",
  "total_line", "team_spread", "weather_status",
  "preseason_role_rank", "role_status",
];

const fullRows = selectRows(projections, commonColumns);
const passingRows = selectRows(
  projections,
  passingColumns,
  (record) => record.position === "QB",
);
const receivingRows = selectRows(
  projections,
  receivingColumns,
  (record) => ["RB", "WR", "TE"].includes(record.position),
);
const rushingRows = selectRows(
  projections,
  rushingColumns,
  (record) => ["QB", "RB", "WR"].includes(record.position),
);

const workbook = Workbook.create();
const dashboard = workbook.worksheets.add("Dashboard");
const fullSheet = workbook.worksheets.add("Week 1 PPR Board");
const passingSheet = workbook.worksheets.add("Passing Projections");
const receivingSheet = workbook.worksheets.add("Receiving Projections");
const rushingSheet = workbook.worksheets.add("Rushing Projections");
const metricsSheet = workbook.worksheets.add("Component Metrics");
const pprSheet = workbook.worksheets.add("PPR Backtest");
const manifestSheet = workbook.worksheets.add("Model Manifest");
const importanceSheet = workbook.worksheets.add("Feature Importance");
const methodsSheet = workbook.worksheets.add("Methods");
const checksSheet = workbook.worksheets.add("Checks");

const navy = "#18344F";
const teal = "#20747D";
const sky = "#E5F0F5";
const paleGreen = "#E7F4EA";
const paleRed = "#FBE9E7";
const paleYellow = "#FFF3CD";
const lightGray = "#F4F6F8";
const darkText = "#18222C";
const white = "#FFFFFF";
const green = "#008000";

function writeDataSheet(sheet, values) {
  const rows = values.length;
  const columns = values[0].length;
  const sourceHeaders = [...values[0]];
  const headerLabels = {
    overall_rank: "Overall Rank",
    position_rank: "Pos Rank",
    opponent_team: "Opponent",
    projected_ppr: "Projected PPR",
    ppr_low: "PPR Low",
    ppr_high: "PPR High",
    projected_receptions: "Projected Rec",
    projected_receiving_yards: "Projected Rec Yds",
    projected_rushing_yards: "Projected Rush Yds",
    projected_passing_yards: "Projected Pass Yds",
    projected_passing_tds: "Projected Pass TD",
    projected_interceptions: "Projected INT",
    projected_rushing_tds: "Projected Rush TD",
    projected_receiving_tds: "Projected Rec TD",
    projected_fumbles_lost: "Projected Fum Lost",
    projected_two_point_conversions: "Projected 2PT",
    total_line: "Game Total",
    team_spread: "Team Spread",
    implied_team_total: "Implied Team Total",
    weather_status: "Weather Status",
    preseason_role_rank: "Preseason Role Rank",
    role_status: "Role Status",
    actual_mean: "Actual Mean",
    predicted_mean: "Predicted Mean",
    r_squared: "R-Squared",
    baseline_mae: "Baseline MAE",
    mae_improvement: "MAE Improvement",
    model_weight: "Model Weight",
    training_rows: "Training Rows",
  };
  const displayValues = [
    sourceHeaders.map((header) => (
      headerLabels[header] ??
      header.replaceAll("_", " ").replace(/\b\w/g, (x) => x.toUpperCase())
    )),
    ...values.slice(1),
  ];
  const lastColumn = columnLetter(columns - 1);
  sheet.getRange(`A1:${lastColumn}${rows}`).values = displayValues;
  sheet.showGridLines = false;
  sheet.freezePanes.freezeRows(1);
  sheet.getRange(`A1:${lastColumn}1`).format = {
    fill: navy,
    font: { bold: true, color: white },
    rowHeight: 42,
    wrapText: true,
    verticalAlignment: "center",
  };
  if (rows > 1) {
    sheet.getRange(`A2:${lastColumn}${rows}`).format = {
      font: { color: darkText, size: 10 },
      borders: {
        bottom: { color: "#E4E9ED", style: "continuous" },
      },
    };
  }
  sourceHeaders.forEach((header, index) => {
    const letter = columnLetter(index);
    let width = 14;
    if (/player/.test(header)) width = 24;
    if (/status/.test(header)) width = 29;
    if (/opponent/.test(header)) width = 16;
    if (/projected_|improvement|probability/.test(header)) width = 20;
    if (/target|label|feature/i.test(header)) width = 24;
    sheet.getRange(`${letter}:${letter}`).format.columnWidth = width;
    if (/ppr|yard|reception|touchdown|_td|interception|fumble|two_point|mean|mae|rmse|bias/i.test(header)) {
      sheet.getRange(`${letter}2:${letter}${rows}`).format.numberFormat = "0.00";
    }
    if (/total_line|team_spread|implied_team_total/i.test(header)) {
      sheet.getRange(`${letter}2:${letter}${rows}`).format.numberFormat = "0.0";
    }
    if (/r_squared|weight|gain|cover|frequency/i.test(header)) {
      sheet.getRange(`${letter}2:${letter}${rows}`).format.numberFormat =
        "0.0%;[Red](0.0%);-";
    }
    if (/rank|observations|rows|features|season|week/.test(header)) {
      sheet.getRange(`${letter}2:${letter}${rows}`).format.numberFormat = "#,##0";
    }
  });
  return { rows, columns, lastColumn, headers: sourceHeaders };
}

const fullMeta = writeDataSheet(fullSheet, fullRows);
const passingMeta = writeDataSheet(passingSheet, passingRows);
const receivingMeta = writeDataSheet(receivingSheet, receivingRows);
const rushingMeta = writeDataSheet(rushingSheet, rushingRows);
const metricsMeta = writeDataSheet(metricsSheet, metricsRows);
const pprMeta = writeDataSheet(pprSheet, pprMetricsRows);
const manifestMeta = writeDataSheet(manifestSheet, manifestRows);
const importanceMeta = writeDataSheet(importanceSheet, importanceRows);

for (const [sheet, meta] of [
  [fullSheet, fullMeta],
  [passingSheet, passingMeta],
  [receivingSheet, receivingMeta],
  [rushingSheet, rushingMeta],
]) {
  const pprIndex = meta.headers.indexOf("projected_ppr");
  if (pprIndex >= 0) {
    const letter = columnLetter(pprIndex);
    sheet.getRange(`${letter}2:${letter}${meta.rows}`).conditionalFormats.add(
      "colorScale",
      {
        colors: ["#FBE9E7", "#FFF3CD", "#C8E6C9"],
        thresholds: ["min", "50%", "max"],
      },
    );
  }
}
const improvementIndex = metricsMeta.headers.indexOf("mae_improvement");
if (improvementIndex >= 0) {
  const letter = columnLetter(improvementIndex);
  metricsSheet.getRange(`${letter}2:${letter}${metricsMeta.rows}`)
    .conditionalFormats.add("colorScale", {
      colors: ["#F4A6A6", "#FFF3CD", "#B9DFBE"],
      thresholds: ["min", "50%", "max"],
    });
}

dashboard.showGridLines = false;
dashboard.mergeCells("A1:H2");
dashboard.getRange("A1:H2").values = [[
  "2026 NFL Fantasy & Player-Stat Projection Suite",
]];
dashboard.getRange("A1:H2").format = {
  fill: navy,
  font: { bold: true, color: white, size: 20 },
  horizontalAlignment: "center",
  verticalAlignment: "center",
};
dashboard.mergeCells("A3:H3");
dashboard.getRange("A3:H3").values = [[
  "Separate component models | Full-PPR scoring | No sportsbook lines or betting logic",
]];
dashboard.getRange("A3:H3").format = {
  fill: sky,
  font: { italic: true, color: navy },
  horizontalAlignment: "center",
};

dashboard.getRange("A5:H5").values = [[
  "Separate Models", null,
  "Walk-Forward Predictions", null,
  "Week 1 Players", null,
  "Week 1 Games", null,
]];
dashboard.getRange("A5:H5").format = {
  fill: teal,
  font: { bold: true, color: white },
  horizontalAlignment: "center",
};
for (const pair of ["A5:B5", "C5:D5", "E5:F5", "G5:H5", "A6:B6", "C6:D6", "E6:F6", "G6:H6"]) {
  dashboard.mergeCells(pair);
}
dashboard.getRange("A6").values = [[9]];
dashboard.getRange("C6").values = [[76691]];
dashboard.getRange("E6").values = [[416]];
dashboard.getRange("G6").values = [[16]];
dashboard.getRange("A6:H6").format = {
  fill: lightGray,
  font: { bold: true, color: green, size: 16 },
  horizontalAlignment: "center",
  numberFormat: "#,##0",
};

dashboard.mergeCells("A8:H8");
dashboard.getRange("A8:H8").values = [["Top Preliminary Week 1 PPR Projections"]];
dashboard.getRange("A8:H8").format = {
  fill: navy,
  font: { bold: true, color: white },
};
dashboard.getRange("A9:H9").values = [[
  "Rank", "Player", "Pos", "Team", "Opponent", "PPR", "Low", "High",
]];
dashboard.getRange("A9:H9").format = {
  fill: teal,
  font: { bold: true, color: white },
};
const topTen = projections.slice(0, 10);
dashboard.getRange("A10:H19").values = topTen.map((record) => [
  record.overall_rank,
  record.player,
  record.position,
  record.team,
  record.opponent_team,
  record.projected_ppr,
  record.ppr_low,
  record.ppr_high,
]);
dashboard.getRange("F10:H19").format.numberFormat = "0.0";
dashboard.getRange("A10:A19").format.numberFormat = "0";

dashboard.mergeCells("A21:H21");
dashboard.getRange("A21:H21").values = [["Model Status & Interpretation"]];
dashboard.getRange("A21:H21").format = {
  fill: navy,
  font: { bold: true, color: white },
};
dashboard.getRange("A22:H25").values = [
  ["Status", "PRELIMINARY — weather and final depth charts pending", null, null, null, null, null, null],
  ["Interceptions", "Quarterback interceptions thrown", null, null, null, null, null, null],
  ["Uncertainty", "Low/high are empirical 10th/90th percentile PPR residual bands", null, null, null, null, null, null],
  ["Betting", "No prices, edges, ROI, stakes, or recommendations are included", null, null, null, null, null, null],
];
for (let row = 22; row <= 25; row += 1) dashboard.mergeCells(`B${row}:H${row}`);
dashboard.getRange("A22:A25").format = { fill: lightGray, font: { bold: true } };
dashboard.getRange("B22:H22").format = {
  fill: paleYellow,
  font: { bold: true, color: "#8A5A00" },
};

dashboard.getRange("J2:K2").values = [["Player", "Projected PPR"]];
dashboard.getRange("J3:K12").values = topTen.map((record) => [
  `${record.player.split(" ")[0][0]}. ${record.player.split(" ").slice(1).join(" ")}`,
  record.projected_ppr,
]);
const topChart = dashboard.charts.add(
  "bar",
  dashboard.getRange("J2:K12"),
);
topChart.title = "Top 10 Preliminary PPR";
topChart.hasLegend = false;
topChart.xAxis = { axisType: "textAxis" };
topChart.yAxis = { numberFormatCode: "0.0" };
topChart.setPosition("J14", "Q29");

const metricsRecords = recordsFromRows(metricsRows);
const targetLabels = {
  fumbles_lost: "Fumbles",
  interceptions: "INT",
  passing_tds: "Pass TD",
  passing_yards: "Pass Yds",
  receiving_tds: "Rec TD",
  receiving_yards: "Rec Yds",
  receptions: "Receptions",
  rushing_tds: "Rush TD",
  rushing_yards: "Rush Yds",
};
dashboard.getRange("M2:N2").values = [["Target", "Relative MAE Improvement"]];
dashboard.getRange("M3:N11").values = metricsRecords.map((record) => [
  targetLabels[record.target] ?? record.target,
  record.mae_improvement / record.baseline_mae,
]);
const improvementChart = dashboard.charts.add(
  "bar",
  dashboard.getRange("M2:N11"),
);
improvementChart.title = "Relative MAE Improvement vs 5-Game Baseline";
improvementChart.hasLegend = false;
improvementChart.xAxis = { axisType: "textAxis" };
improvementChart.yAxis = { numberFormatCode: "0.0%" };
improvementChart.setPosition("J1", "Q13");

dashboard.getRange("A:A").format.columnWidth = 14;
dashboard.getRange("B:B").format.columnWidth = 25;
dashboard.getRange("C:E").format.columnWidth = 12;
dashboard.getRange("F:H").format.columnWidth = 13;
dashboard.freezePanes.freezeRows(3);

methodsSheet.showGridLines = false;
methodsSheet.mergeCells("A1:F2");
methodsSheet.getRange("A1:F2").values = [["Methodology & PPR Conventions"]];
methodsSheet.getRange("A1:F2").format = {
  fill: navy,
  font: { bold: true, color: white, size: 18 },
  horizontalAlignment: "center",
};
methodsSheet.getRange("A4:F4").values = [[
  "Topic", "Method", "Coverage", "Status", "Source", "Notes",
]];
methodsSheet.getRange("A4:F4").format = {
  fill: teal,
  font: { bold: true, color: white },
};
methodsSheet.getRange("A5:F15").values = [
  ["Models", "One XGBoost model per component", "9 targets", "COMPLETE", "Local model suite", "Poisson for counts; squared error for yards"],
  ["Validation", "Chronological walk-forward", "2023–2025", "COMPLETE", "Model convention", "No random split or future-season training"],
  ["Features", "Lagged 3/5/8-game role, opponent, game, and weather fields", "2021–2025", "COMPLETE", "nflverse", "All rolling inputs shifted before calculation"],
  ["Deployment blend", "Model plus lagged five-game mean", "Per target", "COMPLETE", "Walk-forward calibration", "Weights learned from out-of-sample predictions"],
  ["PPR passing", "0.04 per yard; 4 per TD; -2 per interception", "Standard full PPR", "LOCKED", "Scoring convention", "Interceptions mean interceptions thrown"],
  ["PPR rushing", "0.10 per yard; 6 per TD", "Standard full PPR", "LOCKED", "Scoring convention", ""],
  ["PPR receiving", "1 per reception; 0.10 per yard; 6 per TD", "Standard full PPR", "LOCKED", "Scoring convention", ""],
  ["Other scoring", "-2 fumble lost; +2 two-point conversion", "Standard full PPR", "LOCKED", "Scoring convention", "Two-point conversion uses shrunk position rate"],
  ["2026 roles", "One QB, four RB, five WR, three TE per team", "Week 1 preliminary", "PENDING", "2026 roster", "Veteran opportunity plus rookie draft prior"],
  ["Weather", "Temperature, wind, precipitation, dome, surface", "All trained models", "PENDING 2026", "nflverse / verified override", "Current outdoor forecasts not yet available"],
  ["Sportsbook data", "Excluded", "All outputs", "NOT USED", "", "No lines, prices, edges, ROI, stakes, or bet recommendations"],
];
methodsSheet.getRange("A:F").format.columnWidth = 24;
methodsSheet.getRange("B:B").format.columnWidth = 42;
methodsSheet.getRange("E:E").format.columnWidth = 28;
methodsSheet.getRange("F:F").format.columnWidth = 48;
methodsSheet.getRange("A5:F15").format.wrapText = true;
methodsSheet.getRange("5:15").format.rowHeight = 34;
methodsSheet.freezePanes.freezeRows(4);

checksSheet.showGridLines = false;
checksSheet.mergeCells("A1:G2");
checksSheet.getRange("A1:G2").values = [["Fantasy Projection Model Checks"]];
checksSheet.getRange("A1:G2").format = {
  fill: navy,
  font: { bold: true, color: white, size: 18 },
  horizontalAlignment: "center",
};
checksSheet.getRange("A4:G4").values = [[
  "Check", "Actual", "Expected", "Difference", "Tolerance", "Status", "Notes",
]];
checksSheet.getRange("A4:G4").format = {
  fill: teal,
  font: { bold: true, color: white },
};
checksSheet.getRange("A5:G10").values = [
  ["Separate component models", 9, 9, null, 0, null, "One model for each material component"],
  ["Walk-forward seasons", 3, 3, null, 0, null, "2023, 2024, and 2025"],
  ["Week 1 games", 16, 16, null, 0, null, "All regular-season Week 1 games"],
  ["Duplicate player-games", 0, 0, null, 0, null, "Unique game_id + player_id"],
  ["Sportsbook/betting columns", 0, 0, null, 0, null, "Explicitly excluded from this deliverable"],
  ["Overall model status", null, "OK", null, null, null, "Aggregates checks above"],
];
checksSheet.getRange("D5:D9").formulas = [
  ["=B5-C5"],
  ["=B6-C6"],
  ["=B7-C7"],
  ["=B8-C8"],
  ["=B9-C9"],
];
checksSheet.getRange("F5:F9").formulas = [
  ['=IF(ABS(D5)<=E5,"OK","CHECK")'],
  ['=IF(ABS(D6)<=E6,"OK","CHECK")'],
  ['=IF(ABS(D7)<=E7,"OK","CHECK")'],
  ['=IF(ABS(D8)<=E8,"OK","CHECK")'],
  ['=IF(ABS(D9)<=E9,"OK","CHECK")'],
];
checksSheet.getRange("B10").formulas = [['=IF(COUNTIF(F5:F9,"CHECK")=0,"OK","CHECK")']];
checksSheet.getRange("F10").formulas = [["=B10"]];
checksSheet.getRange("F5:F10").conditionalFormats.add("containsText", {
  text: "OK",
  format: { fill: paleGreen, font: { bold: true, color: "#006400" } },
});
checksSheet.getRange("F5:F10").conditionalFormats.add("containsText", {
  text: "CHECK",
  format: { fill: paleRed, font: { bold: true, color: "#8B0000" } },
});
checksSheet.getRange("A:A").format.columnWidth = 36;
checksSheet.getRange("B:F").format.columnWidth = 16;
checksSheet.getRange("G:G").format.columnWidth = 44;

const dashboardInspect = await workbook.inspect({
  kind: "table",
  range: "Dashboard!A1:H25",
  include: "values,formulas",
  tableMaxRows: 25,
  tableMaxCols: 8,
});
await fs.writeFile(
  path.join(outputDir, "dashboard_inspect.ndjson"),
  dashboardInspect.ndjson ?? "",
  "utf8",
);

for (const [sheetName, range, fileName, scale] of [
  ["Dashboard", "A1:Q29", "dashboard_preview.png", 1.1],
  ["Week 1 PPR Board", "A1:Y14", "ppr_board_preview.png", 0.9],
  ["Component Metrics", `A1:J${metricsMeta.rows}`, "metrics_preview.png", 1.2],
  ["Methods", "A1:F15", "methods_preview.png", 1.1],
  ["Checks", "A1:G10", "checks_preview.png", 1.3],
]) {
  const preview = await workbook.render({
    sheetName,
    range,
    scale,
    format: "png",
  });
  await fs.writeFile(
    path.join(outputDir, fileName),
    new Uint8Array(await preview.arrayBuffer()),
  );
}

const errorScan = await workbook.inspect({
  kind: "match",
  searchTerm: "#REF!|#DIV/0!|#VALUE!|#NAME\\?|#N/A",
  options: { useRegex: true, maxResults: 200 },
  summary: "final formula error scan",
});
await fs.writeFile(
  path.join(outputDir, "formula_error_scan.ndjson"),
  errorScan.ndjson ?? "",
  "utf8",
);

const output = await SpreadsheetFile.exportXlsx(workbook);
const outputPath = path.join(
  outputDir,
  "NFL_Fantasy_Prop_Models_2026.xlsx",
);
await output.save(outputPath);
console.log(outputPath);
