import fs from "node:fs/promises";
import path from "node:path";
import { SpreadsheetFile, Workbook } from "@oai/artifact-tool";

const root = process.cwd();
const outputDir = path.join(root, "outputs", "td_model_2026");
await fs.mkdir(outputDir, { recursive: true });

const workbook = Workbook.create();
const dashboard = workbook.worksheets.add("Dashboard");

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

const imports = [
  ["Strategy Summary", "outputs/td_strategy_summary.csv"],
  ["Probability Metrics", "outputs/td_probability_metrics.csv"],
  ["Calibration", "outputs/td_calibration_bands.csv"],
  ["Edge Thresholds", "outputs/td_edge_thresholds.csv"],
  ["Interactions", "outputs/td_interactions.csv"],
  ["2025 Bets", "outputs/td_2025_paper_bankroll.csv"],
  ["Feature Importance", "outputs/td_feature_importance.csv"],
];

const metadata = {};
for (const [sheetName, relativePath] of imports) {
  const csv = await fs.readFile(path.join(root, relativePath), "utf8");
  const values = parseCSV(csv);
  const sheet = workbook.worksheets.add(sheetName);
  const lastColumn = columnLetter(values[0].length - 1);
  sheet.getRange(`A1:${lastColumn}${values.length}`).values = values;
  metadata[sheetName] = {
    rows: values.length,
    headers: values[0],
  };
}

const checks = workbook.worksheets.add("Checks");
const sources = workbook.worksheets.add("Sources & Methods");

const navy = "#16324F";
const teal = "#1F6F78";
const paleBlue = "#EAF2F8";
const paleGreen = "#E8F5E9";
const paleRed = "#FDECEC";
const paleYellow = "#FFF4CC";
const lightGray = "#F3F5F7";
const darkText = "#17212B";
const white = "#FFFFFF";

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

function findColumn(sheetName, header) {
  const index = metadata[sheetName].headers.indexOf(header);
  if (index < 0) throw new Error(`Missing ${header} in ${sheetName}`);
  return columnLetter(index);
}

function styleImportedSheet(sheetName) {
  const sheet = workbook.worksheets.getItem(sheetName);
  const info = metadata[sheetName];
  const lastColumn = columnLetter(info.headers.length - 1);
  const used = sheet.getRange(`A1:${lastColumn}${info.rows}`);
  const bodyUsed = sheet.getRange(`A2:${lastColumn}${info.rows}`);
  const header = sheet.getRange(`A1:${lastColumn}1`);
  sheet.showGridLines = false;
  sheet.freezePanes.freezeRows(1);
  header.format = {
    fill: navy,
    font: { bold: true, color: white },
    rowHeight: 38,
    wrapText: true,
  };
  bodyUsed.format.font = { color: darkText, size: 10 };
  used.format.borders = {
    top: { color: "#D7DEE5", style: "continuous" },
    bottom: { color: "#D7DEE5", style: "continuous" },
    left: { color: "#D7DEE5", style: "continuous" },
    right: { color: "#D7DEE5", style: "continuous" },
    insideHorizontal: { color: "#E7EBEF", style: "continuous" },
  };
  for (let i = 0; i < info.headers.length; i += 1) {
    const headerName = info.headers[i];
    const letter = columnLetter(i);
    const body = sheet.getRange(`${letter}2:${letter}${info.rows}`);
    if (/roi|rate|probability|edge|drawdown|gain|cover|frequency/i.test(headerName)) {
      body.format.numberFormat = "0.0%;[Red](0.0%);-";
    } else if (/bankroll|stake|profit|unit_value/i.test(headerName)) {
      body.format.numberFormat = "$#,##0.00;[Red]($#,##0.00);-";
    } else if (/odds/i.test(headerName)) {
      body.format.numberFormat = "0";
    } else if (/date/i.test(headerName)) {
      body.format.numberFormat = "yyyy-mm-dd";
    } else if (/bets|wins|games|week|season|units/i.test(headerName)) {
      body.format.numberFormat = "#,##0";
    } else if (/temperature|wind|total_line|team_spread/i.test(headerName)) {
      body.format.numberFormat = "0.0";
    }
  }
  const widths = {};
  info.headers.forEach((headerName, index) => {
    let width = 13;
    if (/player|strategy|segment|interaction/i.test(headerName)) width = 24;
    if (/reason/i.test(headerName)) width = 30;
    if (/game_id|book/i.test(headerName)) width = 18;
    if (/opponent_team/i.test(headerName)) width = 17;
    if (/date/i.test(headerName)) width = 12;
    if (/probability|edge|bankroll|importance/i.test(headerName)) width = 20;
    if (/american_odds/i.test(headerName)) width = 18;
    widths[columnLetter(index)] = width;
  });
  for (const [letter, width] of Object.entries(widths)) {
    sheet.getRange(`${letter}:${letter}`).format.columnWidth = width;
  }
  if (info.rows > 1) {
    const roiIndex = info.headers.findIndex((x) => /^roi$|profit|drawdown/.test(x));
    if (roiIndex >= 0) {
      const letter = columnLetter(roiIndex);
      sheet.getRange(`${letter}2:${letter}${info.rows}`).conditionalFormats.add(
        "colorScale",
        {
          colors: ["#F8B4B4", "#FFF4CC", "#B7E4C7"],
          thresholds: ["min", "50%", "max"],
        },
      );
    }
  }
}

for (const [sheetName] of imports) styleImportedSheet(sheetName);

dashboard.showGridLines = false;
dashboard.mergeCells("A1:H2");
dashboard.getRange("A1:H2").values = [["NFL Anytime-Touchdown Model | Walk-Forward Results"]];
dashboard.getRange("A1:H2").format = {
  fill: navy,
  font: { bold: true, color: white, size: 20 },
  horizontalAlignment: "center",
  verticalAlignment: "center",
};
dashboard.getRange("A3:H3").values = [[
  "Early-market prices (median ~19 hours pre-kickoff) | QB rushing touchdowns included | Weather-adjusted",
  null, null, null, null, null, null, null,
]];
dashboard.mergeCells("A3:H3");
dashboard.getRange("A3:H3").format = {
  fill: paleBlue,
  font: { italic: true, color: navy },
  horizontalAlignment: "center",
};

dashboard.getRange("A5:H5").values = [[
  "Expanded Validation Bets", null, "Expanded Validation ROI", null,
  "Expanded 2025 Bets", null, "Expanded 2025 ROI", null,
]];
dashboard.getRange("A5:H5").format = {
  fill: teal,
  font: { bold: true, color: white },
  horizontalAlignment: "center",
};
dashboard.getRange("A6:H6").formulas = [[
  "='Strategy Summary'!B2", null, "='Strategy Summary'!G2", null,
  "='Strategy Summary'!B3", null, "='Strategy Summary'!G3", null,
]];
dashboard.getRange("A6:H6").format = {
  fill: lightGray,
  font: { bold: true, color: "#008000", size: 16 },
  horizontalAlignment: "center",
};
dashboard.getRange("C6").format.numberFormat = "0.0%;[Red](0.0%)";
dashboard.getRange("G6").format.numberFormat = "0.0%;[Red](0.0%)";

dashboard.getRange("A8:D8").values = [[
  "Season", "Actual TD Rate", "Predicted TD Rate", "Brier Score",
]];
dashboard.getRange("A8:D8").format = {
  fill: navy,
  font: { bold: true, color: white },
};
for (let row = 9; row <= 11; row += 1) {
  const sourceRow = row - 7;
  dashboard.getRange(`A${row}:D${row}`).formulas = [[
    `='Probability Metrics'!A${sourceRow}`,
    `='Probability Metrics'!C${sourceRow}`,
    `='Probability Metrics'!D${sourceRow}`,
    `='Probability Metrics'!E${sourceRow}`,
  ]];
}
dashboard.getRange("B9:C11").format.numberFormat = "0.0%";
dashboard.getRange("D9:D11").format.numberFormat = "0.000";
dashboard.getRange("A9:D11").format.font = { color: "#008000" };

const bankrollRows = metadata["2025 Bets"].rows;
const bankrollAfterColumn = findColumn("2025 Bets", "bankroll_after");
const drawdownColumn = findColumn("2025 Bets", "drawdown");
const bankrollBeforeColumn = findColumn("2025 Bets", "bankroll_before");
const dateColumn = findColumn("2025 Bets", "game_date");

dashboard.getRange("A13:H13").values = [[
  "Paper Bankroll (3–10 units; one unit = 1% of each day’s starting bankroll)",
  null, null, null, null, null, null, null,
]];
dashboard.mergeCells("A13:H13");
dashboard.getRange("A13:H13").format = {
  fill: navy,
  font: { bold: true, color: white },
};
dashboard.getRange("A14:H14").values = [[
  "Starting Bankroll", null, "Ending Bankroll", null,
  "Maximum Drawdown", null, "Largest Daily Exposure", null,
]];
dashboard.getRange("A14:H14").format = {
  fill: teal,
  font: { bold: true, color: white },
  horizontalAlignment: "center",
};
dashboard.mergeCells("A14:B14");
dashboard.mergeCells("C14:D14");
dashboard.mergeCells("E14:F14");
dashboard.mergeCells("G14:H14");
dashboard.mergeCells("A15:B15");
dashboard.mergeCells("C15:D15");
dashboard.mergeCells("E15:F15");
dashboard.mergeCells("G15:H15");
dashboard.getRange("A15").values = [[1000]];
dashboard.getRange("A15").format = {
  fill: paleYellow,
  font: { color: "#0000FF", bold: true },
  numberFormat: "$#,##0",
};
dashboard.getRange("C15").formulas = [[
  `='2025 Bets'!${bankrollAfterColumn}${bankrollRows}`,
]];
dashboard.getRange("E15").formulas = [[
  `=MIN('2025 Bets'!${drawdownColumn}2:${drawdownColumn}${bankrollRows})`,
]];
dashboard.getRange("G15").values = [[0.59]];
dashboard.getRange("C15:G15").format.font = { color: "#008000", bold: true };
dashboard.getRange("C15").format.numberFormat = "$#,##0";
dashboard.getRange("E15:G15").format.numberFormat = "0.0%";

dashboard.getRange("A17:H17").values = [[
  "Locked Betting Rule", null, null, null, null, null, null, null,
]];
dashboard.mergeCells("A17:H17");
dashboard.getRange("A17:H17").format = {
  fill: navy,
  font: { bold: true, color: white },
};
dashboard.getRange("A18:H21").values = [
  ["Edge requirement", "At least 2% expected ROI / relative probability edge", null, null, null, null, null, null],
  ["Interaction filter", "Game total ≤ 42 OR TE OR team favored by at least 6 points", null, null, null, null, null, null],
  ["Price/liquidity", "At least 2 books; best price from -300 through +700", null, null, null, null, null, null],
  ["Status", "PAPER ONLY — 2025 retrospective has 45 bets across 32 games", null, null, null, null, null, null],
];
dashboard.mergeCells("B18:H18");
dashboard.mergeCells("B19:H19");
dashboard.mergeCells("B20:H20");
dashboard.mergeCells("B21:H21");
dashboard.getRange("A18:A21").format = { fill: lightGray, font: { bold: true } };
dashboard.getRange("B21:H21").format = { fill: paleRed, font: { bold: true, color: "#8B0000" } };

dashboard.getRange("Z2:AA2").values = [["Date", "Bankroll"]];
for (let row = 3; row < 3 + bankrollRows - 1; row += 1) {
  const sourceRow = row - 1;
  dashboard.getRange(`Z${row}:AA${row}`).formulas = [[
    `='2025 Bets'!${dateColumn}${sourceRow}`,
    `='2025 Bets'!${bankrollAfterColumn}${sourceRow}`,
  ]];
}
dashboard.getRange(`Z3:Z${bankrollRows + 1}`).format.numberFormat = "mmm d";
dashboard.getRange(`AA3:AA${bankrollRows + 1}`).format.numberFormat = "$#,##0";
const chart = dashboard.charts.add(
  "line",
  dashboard.getRange(`Z2:AA${bankrollRows + 1}`),
);
chart.title = "2025 Paper Bankroll";
chart.hasLegend = false;
chart.xAxis = { axisType: "textAxis" };
chart.yAxis = { numberFormatCode: "$#,##0" };
chart.setPosition("J2", "Q16");

dashboard.getRange("A1:Q21").format.font = { name: "Aptos" };
dashboard.getRange("A:A").format.columnWidth = 19;
dashboard.getRange("B:B").format.columnWidth = 18;
dashboard.getRange("C:H").format.columnWidth = 16;
dashboard.freezePanes.freezeRows(3);

checks.showGridLines = false;
checks.getRange("A1:G2").merge();
checks.getRange("A1:G2").values = [["Model Checks"]];
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
checks.getRange("A5:G9").values = [
  ["2025 qualified bets", null, 45, null, 0, null, "Expanded paper-tier output"],
  ["Starting bankroll", null, 1000, null, 0.01, null, "User-specified starting value"],
  ["Ending bankroll positive", null, 0, null, 0, null, "Paper simulation only"],
  ["2025 probability calibration gap", null, 0, null, 0.02, null, "Absolute predicted minus actual TD rate"],
  ["Model release status", "PAPER ONLY", "PAPER ONLY", null, null, null, "Forward test and closing-line verification still required"],
];
checks.getRange("B5").formulas = [["='Strategy Summary'!B3"]];
checks.getRange("B6").formulas = [[`='2025 Bets'!${bankrollBeforeColumn}2`]];
checks.getRange("B7").formulas = [[`='2025 Bets'!${bankrollAfterColumn}${bankrollRows}`]];
checks.getRange("B8").formulas = [["=ABS('Probability Metrics'!D4-'Probability Metrics'!C4)"]];
checks.getRange("D5:D8").formulas = [
  ["=B5-C5"],
  ["=B6-C6"],
  ["=B7-C7"],
  ["=B8-C8"],
];
checks.getRange("F5:F8").formulas = [
  ['=IF(ABS(D5)<=E5,"OK","CHECK")'],
  ['=IF(ABS(D6)<=E6,"OK","CHECK")'],
  ['=IF(B7>C7,"OK","CHECK")'],
  ['=IF(ABS(D8)<=E8,"OK","CHECK")'],
];
checks.getRange("F9").formulas = [['=IF(B9=C9,"OK","CHECK")']];
checks.getRange("B5:B8").format.font = { color: "#008000" };
checks.getRange("C5:C8").format.font = { color: "#0000FF" };
checks.getRange("B6:B7").format.numberFormat = "$#,##0.00";
checks.getRange("B8:E8").format.numberFormat = "0.0%";
checks.getRange("F5:F9").conditionalFormats.add("containsText", {
  text: "OK",
  format: { fill: paleGreen, font: { bold: true, color: "#006400" } },
});
checks.getRange("F5:F9").conditionalFormats.add("containsText", {
  text: "CHECK",
  format: { fill: paleRed, font: { bold: true, color: "#8B0000" } },
});
checks.getRange("A:G").format.columnWidth = 18;
checks.getRange("A:A").format.columnWidth = 34;
checks.getRange("G:G").format.columnWidth = 42;

sources.showGridLines = false;
sources.getRange("A1:F2").merge();
sources.getRange("A1:F2").values = [["Sources, Definitions & Model Conventions"]];
sources.getRange("A1:F2").format = {
  fill: navy,
  font: { bold: true, color: white, size: 18 },
  horizontalAlignment: "center",
};
sources.getRange("A4:F4").values = [[
  "Item", "Value / Convention", "As of", "Source", "URL", "Notes",
]];
sources.getRange("A4:F4").format = {
  fill: teal,
  font: { bold: true, color: white },
};
sources.getRange("A5:F13").values = [
  ["Player statistics", "Weekly player outcomes and usage", "2021–2025", "nflverse/nflreadr", "https://nflreadr.nflverse.com/reference/load_player_stats.html", "Rushing and receiving TDs define the target"],
  ["Game/weather data", "Temperature, wind, precipitation, surface", "2021–2025", "RotoWire archive + nflfastR schedules", "https://www.rotowire.com/betting/nfl/archive.php", "Dome games neutralized to 70°F and 0 mph wind"],
  ["Player prices", "Anytime TD Yes prices", "2023–2025", "The Odds API", "https://the-odds-api.com/liveapi/guides/v4/", "Early market; median roughly 19 hours before kickoff"],
  ["Target", "1 if player scored a rushing or receiving TD", "Per game", "Model convention", "", "QB rushing TDs included; passing TDs excluded"],
  ["Walk-forward", "Train prior years; calibrate on prior season", "2023–2025", "Model convention", "", "No random train/test split"],
  ["Edge %", "(Model probability ÷ best-price implied probability) − 1", "Per offer", "Model convention", "", "Mathematically equals expected ROI at that price"],
  ["Best-price assumption", "Highest available price across sampled books", "Per game", "Model convention", "", "Requires line shopping and access to the listed book"],
  ["Staking", "3–10 units; unit = 1% of day-start bankroll", "2025 expanded paper test", "User convention", "", "No daily cap; produced 59% maximum daily exposure"],
  ["Release status", "PAPER ONLY", "Current", "Risk control", "", "Lock the rule forward and verify one season against one-hour pre-kick closing prices"],
];
sources.getRange("A5:F13").format.wrapText = true;
sources.getRange("A:A").format.columnWidth = 22;
sources.getRange("B:B").format.columnWidth = 36;
sources.getRange("C:C").format.columnWidth = 16;
sources.getRange("D:D").format.columnWidth = 26;
sources.getRange("E:E").format.columnWidth = 48;
sources.getRange("F:F").format.columnWidth = 52;
sources.getRange("A5:A13").format.font = { bold: true };
sources.getRange("E5:E13").format.font = { color: "#FF0000" };
sources.freezePanes.freezeRows(4);

const dashboardInspect = await workbook.inspect({
  kind: "table",
  range: "Dashboard!A1:H21",
  include: "values,formulas",
  tableMaxRows: 21,
  tableMaxCols: 8,
});
await fs.writeFile(
  path.join(outputDir, "dashboard_inspect.ndjson"),
  dashboardInspect.ndjson ?? "",
  "utf8",
);
const checksInspect = await workbook.inspect({
  kind: "table",
  range: "Checks!A1:G10",
  include: "values,formulas",
  tableMaxRows: 10,
  tableMaxCols: 7,
});
await fs.writeFile(
  path.join(outputDir, "checks_inspect.ndjson"),
  checksInspect.ndjson ?? "",
  "utf8",
);

const summaryPreview = await workbook.render({
  sheetName: "Dashboard",
  range: "A1:Q21",
  scale: 1.2,
  format: "png",
});
await fs.writeFile(
  path.join(outputDir, "dashboard_preview.png"),
  new Uint8Array(await summaryPreview.arrayBuffer()),
);
const betsPreview = await workbook.render({
  sheetName: "2025 Bets",
  range: `A1:AD${Math.min(bankrollRows, 12)}`,
  scale: 1,
  format: "png",
});
await fs.writeFile(
  path.join(outputDir, "bets_preview.png"),
  new Uint8Array(await betsPreview.arrayBuffer()),
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
await output.save(path.join(outputDir, "NFL_Touchdown_Model_2023_2025.xlsx"));
console.log(path.join(outputDir, "NFL_Touchdown_Model_2023_2025.xlsx"));
