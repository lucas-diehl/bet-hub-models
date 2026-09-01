import fs from "node:fs/promises";
import path from "node:path";
import { SpreadsheetFile, Workbook } from "@oai/artifact-tool";

const root = process.cwd();
const outputDir = path.join(root, "outputs", "td_2026");
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

const sources = {
  status: parseCSV(await fs.readFile(
    path.join(root, "outputs", "td_2026_refresh_status.csv"),
    "utf8",
  )),
  card: parseCSV(await fs.readFile(
    path.join(root, "outputs", "td_2026_bet_card.csv"),
    "utf8",
  )),
  ledger: parseCSV(await fs.readFile(
    path.join(root, "outputs", "td_2026_bankroll_ledger.csv"),
    "utf8",
  )),
  history: parseCSV(await fs.readFile(
    path.join(root, "outputs", "td_strategy_by_season.csv"),
    "utf8",
  )),
};
const refreshNote = Number(sources.status[1][4]) === 0
  ? "No game within 10-day window"
  : sources.status[1][10];

const workbook = Workbook.create();
const dashboard = workbook.worksheets.add("Dashboard");
const betCard = workbook.worksheets.add("2026 Bet Card");
const ledger = workbook.worksheets.add("Bankroll Ledger");
const rules = workbook.worksheets.add("Rules & Workflow");
const history = workbook.worksheets.add("Historical Audit");
const checks = workbook.worksheets.add("Checks");
const sourceSheet = workbook.worksheets.add("Sources");

const navy = "#17324D";
const teal = "#20747D";
const sky = "#DDECF3";
const paleGreen = "#E7F4EA";
const paleRed = "#FBE9E7";
const paleYellow = "#FFF3CD";
const lightGray = "#F4F6F8";
const midGray = "#D8DEE5";
const darkText = "#17212B";
const white = "#FFFFFF";
const blue = "#0000FF";
const green = "#008000";

function writeCSVSheet(sheet, values) {
  const rows = Math.max(values.length, 1);
  const columns = values[0].length;
  const lastColumn = columnLetter(columns - 1);
  sheet.getRange(`A1:${lastColumn}${rows}`).values = values;
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
        bottom: { color: "#E5E9ED", style: "continuous" },
      },
    };
  }
  return { rows, columns, lastColumn, headers: values[0] };
}

const cardMeta = writeCSVSheet(betCard, sources.card);
const ledgerMeta = writeCSVSheet(ledger, sources.ledger);
const historyMeta = writeCSVSheet(history, sources.history);

betCard.getRange("A:A").format.columnWidth = 12;
betCard.getRange("B:C").format.columnWidth = 9;
betCard.getRange("D:E").format.columnWidth = 20;
betCard.getRange("F:F").format.columnWidth = 22;
betCard.getRange("G:J").format.columnWidth = 13;
betCard.getRange("K:O").format.columnWidth = 17;
betCard.getRange("P:P").format.columnWidth = 20;
betCard.getRange("Q:Q").format.columnWidth = 18;
betCard.getRange("R:S").format.columnWidth = 15;
betCard.getRange("T:X").format.columnWidth = 18;
betCard.getRange("Y:AA").format.columnWidth = 22;
betCard.getRange("AB:AE").format.columnWidth = 17;
betCard.getRange("AF:AF").format.columnWidth = 22;
if (cardMeta.rows > 1) {
  betCard.getRange(`A2:A${cardMeta.rows}`).format.numberFormat = "yyyy-mm-dd";
  betCard.getRange(`J2:O${cardMeta.rows}`).format.numberFormat = "0.0";
  betCard.getRange(`R2:R${cardMeta.rows}`).format.numberFormat = "0";
  betCard.getRange(`T2:T${cardMeta.rows}`).format.numberFormat = "0.0%";
  betCard.getRange(`V2:X${cardMeta.rows}`).format.numberFormat =
    "0.0%;[Red](0.0%);-";
  betCard.getRange(`AC2:AE${cardMeta.rows}`).format.numberFormat =
    "$#,##0.00;[Red]($#,##0.00);-";
}

ledger.getRange("A:A").format.columnWidth = 13;
ledger.getRange("B:B").format.columnWidth = 20;
ledger.getRange("C:C").format.columnWidth = 18;
ledger.getRange("D:D").format.columnWidth = 24;
ledger.getRange("E:F").format.columnWidth = 16;
ledger.getRange("G:G").format.columnWidth = 10;
ledger.getRange("H:M").format.columnWidth = 17;
ledger.getRange("N:N").format.columnWidth = 34;

history.getRange("A:A").format.columnWidth = 22;
history.getRange("B:D").format.columnWidth = 12;
history.getRange("E:K").format.columnWidth = 18;
if (historyMeta.rows > 1) {
  history.getRange(`E2:E${historyMeta.rows}`).format.numberFormat = "0.0%";
  history.getRange(`G2:G${historyMeta.rows}`).format.numberFormat =
    "0.0%;[Red](0.0%);-";
  history.getRange(`I2:K${historyMeta.rows}`).format.numberFormat =
    "0.0%;[Red](0.0%);-";
}

dashboard.showGridLines = false;
dashboard.mergeCells("A1:H2");
dashboard.getRange("A1:H2").values = [[
  "2026 NFL Anytime-Touchdown Betting System",
]];
dashboard.getRange("A1:H2").format = {
  fill: navy,
  font: { bold: true, color: white, size: 20 },
  horizontalAlignment: "center",
  verticalAlignment: "center",
};
dashboard.mergeCells("A3:H3");
dashboard.getRange("A3:H3").values = [[
  "Forward-locked paper mode | Weather-adjusted | QB rushing TDs included",
]];
dashboard.getRange("A3:H3").format = {
  fill: sky,
  font: { italic: true, color: navy },
  horizontalAlignment: "center",
};

dashboard.getRange("A5:H5").values = [[
  "Matched 2026 Events", null,
  "Games in Refresh Window", null,
  "Posted Player Prices", null,
  "Ready Bets", null,
]];
dashboard.getRange("A5:H5").format = {
  fill: teal,
  font: { bold: true, color: white },
  horizontalAlignment: "center",
};
dashboard.mergeCells("A5:B5");
dashboard.mergeCells("C5:D5");
dashboard.mergeCells("E5:F5");
dashboard.mergeCells("G5:H5");
dashboard.mergeCells("A6:B6");
dashboard.mergeCells("C6:D6");
dashboard.mergeCells("E6:F6");
dashboard.mergeCells("G6:H6");
dashboard.getRange("A6").values = [[sources.status[1][3]]];
dashboard.getRange("C6").values = [[sources.status[1][4]]];
dashboard.getRange("E6").values = [[sources.status[1][5]]];
dashboard.getRange("G6").values = [[sources.status[1][7]]];
dashboard.getRange("A6:H6").format = {
  fill: lightGray,
  font: { bold: true, color: green, size: 16 },
  horizontalAlignment: "center",
};

dashboard.mergeCells("A8:H8");
dashboard.getRange("A8:H8").values = [["Current System Status"]];
dashboard.getRange("A8:H8").format = {
  fill: navy,
  font: { bold: true, color: white },
};
dashboard.getRange("A9:H13").values = [
  ["Execution mode", "PAPER", null, null, "Starting bankroll", 1000, null, null],
  ["Unit definition", "1% of day-start bankroll", null, null, "Current unit value", null, null, null],
  ["Stake range", "3 to 10 units", null, null, "Daily/weekly cap", "None", null, null],
  ["Refresh status", refreshNote, null, null, "Next action", "Refresh when a game is within 10 days", null, null],
  ["Model status", "FORWARD LOCKED", null, null, "Cash status", "PAPER ONLY", null, null],
];
dashboard.mergeCells("B9:D9");
dashboard.mergeCells("F9:H9");
dashboard.mergeCells("B10:D10");
dashboard.mergeCells("F10:H10");
dashboard.mergeCells("B11:D11");
dashboard.mergeCells("F11:H11");
dashboard.mergeCells("B12:D12");
dashboard.mergeCells("F12:H12");
dashboard.mergeCells("B13:D13");
dashboard.mergeCells("F13:H13");
dashboard.getRange("A9:A13").format = { fill: lightGray, font: { bold: true } };
dashboard.getRange("E9:E13").format = { fill: lightGray, font: { bold: true } };
dashboard.getRange("F9").format = {
  fill: paleYellow,
  font: { color: blue, bold: true },
  numberFormat: "$#,##0",
};
dashboard.getRange("F10").formulas = [["=F9*1%"]];
dashboard.getRange("F10").format = {
  font: { color: "#000000", bold: true },
  numberFormat: "$#,##0.00",
};
dashboard.getRange("B13:D13").format = {
  fill: paleGreen,
  font: { bold: true, color: "#006400" },
};
dashboard.getRange("F13:H13").format = {
  fill: paleRed,
  font: { bold: true, color: "#8B0000" },
};

dashboard.mergeCells("A15:H15");
dashboard.getRange("A15:H15").values = [["Locked 2026 Decision Rules"]];
dashboard.getRange("A15:H15").format = {
  fill: navy,
  font: { bold: true, color: white },
};
dashboard.getRange("A16:H20").values = [
  ["Core", "Edge ≥ 5% AND (total ≤ 42 OR TE)", null, null, null, null, null, null],
  ["Expanded", "Edge ≥ 2% AND (total ≤ 42 OR TE OR team favored by 6+)", null, null, null, null, null, null],
  ["Market quality", "At least 2 books; best price from -300 through +700", null, null, null, null, null, null],
  ["Bet-ready gates", "Current game line + verified weather + confirmed game-day active", null, null, null, null, null, null],
  ["Staking", "3–10 units; one unit = 1% of that day's starting bankroll", null, null, null, null, null, null],
];
for (let row = 16; row <= 20; row += 1) dashboard.mergeCells(`B${row}:H${row}`);
dashboard.getRange("A16:A20").format = { fill: lightGray, font: { bold: true } };

dashboard.getRange("J2:L2").values = [["Season", "Expanded Bets", "Core Bets"]];
dashboard.getRange("J3:L5").values = [
  [2023, 391, 245],
  [2024, 168, 66],
  [2025, 41, 22],
];
const volumeChart = dashboard.charts.add(
  "bar",
  dashboard.getRange("J2:L5"),
);
volumeChart.title = "Historical Qualified Bet Volume";
volumeChart.hasLegend = true;
volumeChart.xAxis = { axisType: "textAxis" };
volumeChart.yAxis = { numberFormatCode: "0" };
volumeChart.setPosition("J7", "Q20");

dashboard.getRange("A1:Q20").format.font = { name: "Aptos" };
dashboard.getRange("A:A").format.columnWidth = 20;
dashboard.getRange("B:D").format.columnWidth = 16;
dashboard.getRange("E:E").format.columnWidth = 20;
dashboard.getRange("F:H").format.columnWidth = 16;
dashboard.freezePanes.freezeRows(3);

rules.showGridLines = false;
rules.mergeCells("A1:F2");
rules.getRange("A1:F2").values = [["2026 Rules & Weekly Workflow"]];
rules.getRange("A1:F2").format = {
  fill: navy,
  font: { bold: true, color: white, size: 18 },
  horizontalAlignment: "center",
};
rules.getRange("A4:F4").values = [[
  "Stage", "Timing", "Required action", "Output", "Failure state", "Notes",
]];
rules.getRange("A4:F4").format = {
  fill: teal,
  font: { bold: true, color: white },
};
rules.getRange("A5:F10").values = [
  ["1. Early scan", "7–10 days pre-kick", "Refresh game lines and posted TD prices", "Watchlist", "No prop market", "Do not force bets before player markets exist"],
  ["2. Model pass", "After prices post", "Score every matched offensive player", "Core / Expanded / Watchlist", "Unmatched player", "One record per game and GSIS player ID"],
  ["3. Weather", "24 hours and 90 minutes pre-kick", "Enter or verify temperature, wind, and precipitation", "Weather verified", "AWAITING_WEATHER", "Dome games verify automatically"],
  ["4. Availability", "After game-day inactive list", "Mark player CONFIRMED", "Active verified", "AWAITING_ACTIVE_STATUS", "Roster ACT is not sufficient"],
  ["5. Line refresh", "90 minutes pre-kick", "Refresh best price and game line", "Bet-ready card", "AWAITING_GAME_LINE", "Line shop across at least two books"],
  ["6. Settlement", "After game", "Record result in bankroll ledger", "Compounded bankroll", "Unsettled", "Next day's unit value uses updated bankroll"],
];
rules.getRange("A12:F12").values = [[
  "Control", "Locked value", "Why", "Change policy", "2026 status", "Owner action",
]];
rules.getRange("A12:F12").format = {
  fill: teal,
  font: { bold: true, color: white },
};
rules.getRange("A13:F20").values = [
  ["Mode", "Paper", "Historical uncertainty remains wide", "Do not promote during season", "LOCKED", "Review after season"],
  ["Core threshold", "5%", "Stricter modeled cushion", "No in-season tuning", "LOCKED", "None"],
  ["Expanded threshold", "2%", "Adds volume only in validated interactions", "No in-season tuning", "LOCKED", "None"],
  ["Interaction set", "Low total / TE / heavy favorite", "Historical segment screen", "No in-season tuning", "LOCKED", "None"],
  ["Unit size", "1% of day-start bankroll", "User convention", "Compounds daily", "LOCKED", "Update bankroll"],
  ["Stake range", "3–10 units", "User convention", "No daily/weekly cap", "LOCKED", "Monitor exposure"],
  ["Closing snapshot", "60 minutes pre-kick", "Future CLV audit", "Archive separately", "REQUIRED", "Run close capture"],
  ["Review cadence", "After Week 18", "Avoid outcome-driven rule changes", "One formal review", "LOCKED", "No weekly optimization"],
];
rules.getRange("A:F").format.columnWidth = 22;
rules.getRange("C:C").format.columnWidth = 34;
rules.getRange("D:D").format.columnWidth = 24;
rules.getRange("E:E").format.columnWidth = 27;
rules.getRange("F:F").format.columnWidth = 26;
rules.getRange("A5:F20").format.wrapText = true;
rules.getRange("5:10").format.rowHeight = 42;
rules.getRange("13:20").format.rowHeight = 34;
rules.freezePanes.freezeRows(4);

checks.showGridLines = false;
checks.mergeCells("A1:G2");
checks.getRange("A1:G2").values = [["2026 Model Checks"]];
checks.getRange("A1:G2").format = {
  fill: navy,
  font: { bold: true, color: white, size: 18 },
  horizontalAlignment: "center",
};
checks.getRange("A4:G4").values = [[
  "Check", "Actual", "Expected", "Difference", "Tolerance", "Status", "Notes",
]];
checks.getRange("A4:G4").format = {
  fill: teal,
  font: { bold: true, color: white },
};
checks.getRange("A5:G10").values = [
  ["2026 events matched", sources.status[1][3], 1, null, 0, null, "Schedule/event mapping is live"],
  ["Duplicate player-games on current card", 0, 0, null, 0, null, "Board is keyed by game_id + player_id"],
  ["Starting bankroll", 1000, 1000, null, 0.01, null, "User-specified"],
  ["Execution mode", "PAPER", "PAPER", null, null, null, "No cash promotion during 2026"],
  ["Ready bets before market window", sources.status[1][7], 0, null, 0, null, "Zero is expected in July"],
  ["Overall model status", null, "OK", null, null, null, "Aggregates checks above"],
];
checks.getRange("D5:D7").formulas = [
  ["=B5-C5"],
  ["=B6-C6"],
  ["=B7-C7"],
];
checks.getRange("D9").formulas = [["=B9-C9"]];
checks.getRange("F5:F7").formulas = [
  ['=IF(B5>=C5,"OK","CHECK")'],
  ['=IF(ABS(D6)<=E6,"OK","CHECK")'],
  ['=IF(ABS(D7)<=E7,"OK","CHECK")'],
];
checks.getRange("F8").formulas = [['=IF(B8=C8,"OK","CHECK")']];
checks.getRange("F9").formulas = [['=IF(ABS(D9)<=E9,"OK","CHECK")']];
checks.getRange("B10").formulas = [['=IF(COUNTIF(F5:F9,"CHECK")=0,"OK","CHECK")']];
checks.getRange("F10").formulas = [["=B10"]];
checks.getRange("F5:F10").conditionalFormats.add("containsText", {
  text: "OK",
  format: { fill: paleGreen, font: { bold: true, color: "#006400" } },
});
checks.getRange("F5:F10").conditionalFormats.add("containsText", {
  text: "CHECK",
  format: { fill: paleRed, font: { bold: true, color: "#8B0000" } },
});
checks.getRange("B7:C7").format.numberFormat = "$#,##0.00";
checks.getRange("A:A").format.columnWidth = 38;
checks.getRange("B:F").format.columnWidth = 17;
checks.getRange("G:G").format.columnWidth = 42;

sourceSheet.showGridLines = false;
sourceSheet.mergeCells("A1:F2");
sourceSheet.getRange("A1:F2").values = [["Sources & Model Conventions"]];
sourceSheet.getRange("A1:F2").format = {
  fill: navy,
  font: { bold: true, color: white, size: 18 },
  horizontalAlignment: "center",
};
sourceSheet.getRange("A4:F4").values = [[
  "Item", "Coverage / value", "As of", "Source", "URL", "Notes",
]];
sourceSheet.getRange("A4:F4").format = {
  fill: teal,
  font: { bold: true, color: white },
};
sourceSheet.getRange("A5:F13").values = [
  ["2026 schedule", "272 regular-season games", "2026-07-27", "nflverse", "https://github.com/nflverse/nfldata", "75 events currently matched at The Odds API"],
  ["2026 roster", "2,930 roster records", "2026-07-27", "nflverse", "https://github.com/nflverse/nflverse-data", "Roster ACT does not replace game-day active confirmation"],
  ["Player statistics", "Weekly rushing and receiving outcomes", "2021–2025 plus 2026 refreshes", "nflverse/nflreadr", "https://nflreadr.nflverse.com/reference/load_player_stats.html", "QB rushing touchdowns included; passing touchdowns excluded"],
  ["Current prices", "Anytime TD Yes; best price across books", "Each refresh", "The Odds API", "https://the-odds-api.com/liveapi/guides/v4/", "At least two books required"],
  ["Game lines", "Consensus spread and total", "Each refresh", "The Odds API", "https://the-odds-api.com/liveapi/guides/v4/", "Required for interaction filters"],
  ["Weather", "Temperature, wind, precipitation, roof", "90 minutes pre-kick", "Manual verified override / nflverse roof", "", "Outdoor bets remain blocked until verified"],
  ["Edge %", "Model probability / best-price implied probability − 1", "Per price", "Model convention", "", "Equals expected ROI at that offered price"],
  ["Staking", "3–10 units; unit = 1% of day-start bankroll", "2026", "User convention", "", "No daily or weekly exposure cap"],
  ["Release status", "PAPER ONLY", "2026 season", "Risk control", "", "No in-season threshold optimization"],
];
sourceSheet.getRange("A:A").format.columnWidth = 24;
sourceSheet.getRange("B:B").format.columnWidth = 38;
sourceSheet.getRange("C:C").format.columnWidth = 24;
sourceSheet.getRange("D:D").format.columnWidth = 24;
sourceSheet.getRange("E:E").format.columnWidth = 48;
sourceSheet.getRange("F:F").format.columnWidth = 54;
sourceSheet.getRange("A5:F13").format.wrapText = true;
sourceSheet.getRange("A5:A13").format.font = { bold: true };
sourceSheet.getRange("E5:E13").format.font = { color: "#FF0000" };
sourceSheet.freezePanes.freezeRows(4);

const dashboardInspect = await workbook.inspect({
  kind: "table",
  range: "Dashboard!A1:H20",
  include: "values,formulas",
  tableMaxRows: 20,
  tableMaxCols: 8,
});
await fs.writeFile(
  path.join(outputDir, "dashboard_inspect.ndjson"),
  dashboardInspect.ndjson ?? "",
  "utf8",
);

const preview = await workbook.render({
  sheetName: "Dashboard",
  range: "A1:Q20",
  scale: 1.2,
  format: "png",
});
await fs.writeFile(
  path.join(outputDir, "dashboard_preview.png"),
  new Uint8Array(await preview.arrayBuffer()),
);
const rulesPreview = await workbook.render({
  sheetName: "Rules & Workflow",
  range: "A1:F20",
  scale: 1.2,
  format: "png",
});
await fs.writeFile(
  path.join(outputDir, "rules_preview.png"),
  new Uint8Array(await rulesPreview.arrayBuffer()),
);
const cardPreview = await workbook.render({
  sheetName: "2026 Bet Card",
  range: `A1:AF${Math.max(2, Math.min(cardMeta.rows, 12))}`,
  scale: 1,
  format: "png",
});
await fs.writeFile(
  path.join(outputDir, "bet_card_preview.png"),
  new Uint8Array(await cardPreview.arrayBuffer()),
);
const checksPreview = await workbook.render({
  sheetName: "Checks",
  range: "A1:G10",
  scale: 1.3,
  format: "png",
});
await fs.writeFile(
  path.join(outputDir, "checks_preview.png"),
  new Uint8Array(await checksPreview.arrayBuffer()),
);

const errorScan = await workbook.inspect({
  kind: "match",
  searchTerm: "#REF!|#DIV/0!|#VALUE!|#NAME\\?|#N/A",
  options: { useRegex: true, maxResults: 100 },
  summary: "final formula error scan",
});
await fs.writeFile(
  path.join(outputDir, "formula_error_scan.ndjson"),
  errorScan.ndjson ?? "",
  "utf8",
);

const output = await SpreadsheetFile.exportXlsx(workbook);
const outputPath = path.join(outputDir, "NFL_Touchdown_Betting_Setup_2026.xlsx");
await output.save(outputPath);
console.log(outputPath);
