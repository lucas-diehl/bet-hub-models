import fs from "node:fs/promises";
import path from "node:path";
import { SpreadsheetFile, Workbook } from "@oai/artifact-tool";

const root = process.cwd();
const outputDir = path.join(root, "outputs", "dfs_salary_history");
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
    if (/^-?(?:\d+\.?\d*|\.\d+)$/.test(value)) return Number(value);
    return value;
  }));
}

function recordsFromRows(rows) {
  const headers = rows[0];
  return rows.slice(1).map((values) => Object.fromEntries(
    headers.map((header, index) => [header, values[index] ?? null]),
  ));
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

const salaryRecords = recordsFromRows(parseCSV(await fs.readFile(
  path.join(root, "outputs", "dfs_salaries.csv"),
  "utf8",
)));
const coverageRecords = recordsFromRows(parseCSV(await fs.readFile(
  path.join(root, "outputs", "dfs_salary_coverage.csv"),
  "utf8",
)));
const checkRecords = recordsFromRows(parseCSV(await fs.readFile(
  path.join(root, "outputs", "dfs_salary_checks.csv"),
  "utf8",
)));

const navy = "#17324D";
const teal = "#237B7B";
const sky = "#E8F1F5";
const paleGreen = "#E8F4EA";
const paleYellow = "#FFF3CD";
const paleRed = "#FBE9E7";
const lightGray = "#F3F5F7";
const border = "#CBD5DF";
const darkText = "#17212B";
const white = "#FFFFFF";

const workbook = Workbook.create();
const dashboard = workbook.worksheets.add("Dashboard");
const coverage = workbook.worksheets.add("Coverage");
const dkSheet = workbook.worksheets.add("DraftKings Salaries");
const fdSheet = workbook.worksheets.add("FanDuel Salaries");
const checks = workbook.worksheets.add("Checks");
const methods = workbook.worksheets.add("Sources & Operations");

for (const sheet of [dashboard, coverage, dkSheet, fdSheet, checks, methods]) {
  sheet.showGridLines = false;
}

function titleBand(sheet, range, title, subtitle) {
  sheet.getRange(range).merge();
  sheet.getRange(range).values = [[title]];
  sheet.getRange(range).format = {
    fill: navy,
    font: { color: white, bold: true, size: 20 },
    horizontalAlignment: "left",
    verticalAlignment: "center",
  };
  const match = range.match(/^([A-Z]+)(\d+):([A-Z]+)(\d+)$/);
  const startRow = Number(match[2]);
  const subtitleRange = `${match[1]}${startRow + 2}:${match[3]}${startRow + 2}`;
  sheet.getRange(subtitleRange).merge();
  sheet.getRange(subtitleRange).values = [[subtitle]];
  sheet.getRange(subtitleRange).format = {
    fill: sky,
    font: { color: darkText, italic: true, size: 10 },
    verticalAlignment: "center",
  };
}

function sectionHeader(sheet, range, text) {
  sheet.getRange(range).merge();
  sheet.getRange(range).values = [[text]];
  sheet.getRange(range).format = {
    fill: teal,
    font: { color: white, bold: true, size: 11 },
    verticalAlignment: "center",
  };
}

function writeCard(sheet, range, labelRange, label, formula, fill = paleGreen) {
  sheet.getRange(range).merge();
  sheet.getRange(range).formulas = [[formula]];
  sheet.getRange(range).format = {
    fill,
    font: { color: navy, bold: true, size: 18 },
    horizontalAlignment: "center",
    verticalAlignment: "center",
    borders: {
      top: { color: border, style: "continuous", weight: 1 },
      bottom: { color: border, style: "continuous", weight: 1 },
      left: { color: border, style: "continuous", weight: 1 },
      right: { color: border, style: "continuous", weight: 1 },
    },
  };
  sheet.getRange(range).format.numberFormat = "#,##0";
  sheet.getRange(labelRange).merge();
  sheet.getRange(labelRange).values = [[label]];
  sheet.getRange(labelRange).format = {
    fill: navy,
    font: { color: white, bold: true, size: 9 },
    horizontalAlignment: "center",
    verticalAlignment: "center",
  };
}

titleBand(
  dashboard,
  "A1:N2",
  "NFL DFS Salary History & Collection Plan",
  "Verified free backfill, explicit coverage gaps, and a repeatable 2026 archive process",
);
writeCard(dashboard, "A5:C7", "A4:C4", "TOTAL SALARY ROWS", "=SUM(Coverage!F5:F34)");
writeCard(dashboard, "D5:F7", "D4:F4", "DRAFTKINGS ROWS", '=SUMIF(Coverage!A5:A34,"DK",Coverage!F5:F34)');
writeCard(dashboard, "G5:I7", "G4:I4", "FANDUEL ROWS", '=SUMIF(Coverage!A5:A34,"FD",Coverage!F5:F34)');
writeCard(dashboard, "J5:L7", "J4:L4", "VALID SITE-SEASONS", '=COUNTIF(Coverage!K5:K34,"Complete")');
writeCard(dashboard, "M5:N7", "M4:N4", "FAILED CHECKS", '=COUNTIF(Checks!C5:C12,"FAIL")', paleYellow);

sectionHeader(dashboard, "A9:N9", "What is usable now");
dashboard.getRange("A10:N14").values = [
  ["Area", "Decision", "Coverage / timing", "Why", null, null, null, null, null, null, null, null, null, null],
  ["Historical DK", "Use", "2014–2021, Weeks 1–17/18", "Complete free RotoGuru weekly player pools", null, null, null, null, null, null, null, null, null, null],
  ["Historical FD", "Use", "2011–2021, Weeks 1–17/18", "Complete free RotoGuru weekly player pools", null, null, null, null, null, null, null, null, null, null],
  ["2022–2025", "Gap", "No verified complete free archive", "Do not interpolate salaries or mislabel partial article tables as full slates", null, null, null, null, null, null, null, null, null, null],
  ["2026 onward", "Archive weekly", "Official platform CSV + optional DK capture", "The official CSV is the durable record; API capture is a convenience", null, null, null, null, null, null, null, null, null, null],
];
for (let row = 10; row <= 14; row += 1) {
  dashboard.getRange(`D${row}:N${row}`).merge();
}
dashboard.getRange("A10:N10").format = {
  fill: lightGray,
  font: { bold: true, color: navy },
  borders: { bottom: { color: border, style: "continuous", weight: 1 } },
};
dashboard.getRange("A11:N14").format = {
  font: { color: darkText, size: 10 },
  wrapText: true,
  borders: { bottom: { color: "#E6EBEF", style: "continuous", weight: 1 } },
};
dashboard.getRange("B13").format = { fill: paleYellow, font: { bold: true, color: "#7A5200" } };
dashboard.getRange("B14").format = { fill: paleGreen, font: { bold: true, color: "#246B35" } };

sectionHeader(dashboard, "A17:C17", "Rows by season");
const seasons = Array.from({ length: 11 }, (_, index) => 2011 + index);
dashboard.getRange("A18:C29").values = [
  ["Season", "DraftKings", "FanDuel"],
  ...seasons.map((season) => [
    season,
    coverageRecords.find((row) => row.site === "DK" && row.season === season)?.player_rows ?? 0,
    coverageRecords.find((row) => row.site === "FD" && row.season === season)?.player_rows ?? 0,
  ]),
];
dashboard.getRange("A18:C18").format = {
  fill: navy,
  font: { color: white, bold: true },
};
dashboard.getRange("B19:C29").format.numberFormat = "#,##0";
const chart = dashboard.charts.add("line", dashboard.getRange("A18:C29"));
chart.title = "Historical salary rows by platform";
chart.hasLegend = true;
chart.xAxis = { axisType: "textAxis" };
chart.yAxis = { numberFormatCode: "#,##0" };
chart.setPosition("E17", "N31");

dashboard.getRange("A1:N31").format.font.name = "Aptos";
dashboard.getRange("A1:N2").format.rowHeight = 28;
dashboard.getRange("A4:N7").format.rowHeight = 23;
dashboard.getRange("A10:N14").format.rowHeight = 28;
dashboard.getRange("A:A").format.columnWidth = 17;
dashboard.getRange("B:B").format.columnWidth = 17;
dashboard.getRange("C:C").format.columnWidth = 22;
dashboard.getRange("D:N").format.columnWidth = 12;
dashboard.freezePanes.freezeRows(3);

titleBand(
  coverage,
  "A1:J2",
  "Coverage Audit",
  "Complete means every regular-season week available for that platform-season is populated",
);
const coverageHeader = [
  "Site", "Season", "Weeks", "First Week", "Last Week", "Player Rows",
  "Unique Players", "Min Salary", "Median Salary", "Max Salary", "Status",
];
const coverageRows = [];
for (const site of ["DK", "FD"]) {
  for (let season = 2011; season <= 2025; season += 1) {
    const found = coverageRecords.find((row) => row.site === site && row.season === season);
    const expected = site === "DK" ? season >= 2014 && season <= 2021 : season <= 2021;
    let status = "Not offered in free source";
    if (found) status = "Complete";
    else if (season >= 2022) status = "Free archive gap";
    else if (expected) status = "Missing";
    coverageRows.push([
      site,
      season,
      found?.weeks ?? null,
      found?.first_week ?? null,
      found?.last_week ?? null,
      found?.player_rows ?? null,
      found?.unique_players ?? null,
      found?.minimum_salary ?? null,
      found?.median_salary ?? null,
      found?.maximum_salary ?? null,
      status,
    ]);
  }
}
coverage.getRange("A4:K34").values = [coverageHeader, ...coverageRows];
coverage.getRange("A4:K4").format = {
  fill: navy,
  font: { color: white, bold: true },
  verticalAlignment: "center",
};
coverage.getRange("A5:K34").format = {
  font: { color: darkText, size: 10 },
  borders: { bottom: { color: "#E6EBEF", style: "continuous", weight: 1 } },
};
coverage.getRange("F5:J34").format.numberFormat = "#,##0";
coverage.getRange("K5:K34").conditionalFormats.add("containsText", {
  text: "Complete",
  format: { fill: paleGreen, font: { color: "#246B35", bold: true } },
});
coverage.getRange("K5:K34").conditionalFormats.add("containsText", {
  text: "gap",
  format: { fill: paleYellow, font: { color: "#7A5200", bold: true } },
});
coverage.getRange("A4:K34").format.font.name = "Aptos";
coverage.getRange("A:A").format.columnWidth = 9;
coverage.getRange("B:E").format.columnWidth = 11;
coverage.getRange("F:J").format.columnWidth = 14;
coverage.getRange("K:K").format.columnWidth = 25;
coverage.freezePanes.freezeRows(4);
coverage.tables.add("A4:K34", true, "CoverageTable").style = "TableStyleMedium2";

function writeSalarySheet(sheet, site, title) {
  const columns = [
    "season", "week", "player_id_site", "player_name", "position", "team",
    "opponent", "home_away", "salary", "fantasy_points", "slate_type", "source",
  ];
  const records = salaryRecords.filter((row) => row.site === site);
  const values = [
    columns.map((column) => ({
      season: "Season",
      week: "Week",
      player_id_site: "Source Player ID",
      player_name: "Player",
      position: "Position",
      team: "Team",
      opponent: "Opponent",
      home_away: "H/A",
      salary: "Salary",
      fantasy_points: "Fantasy Points",
      slate_type: "Slate Type",
      source: "Source",
    }[column])),
    ...records.map((record) => columns.map((column) => record[column] ?? null)),
  ];
  titleBand(
    sheet,
    "A1:L2",
    title,
    `${records.length.toLocaleString()} normalized player-week rows; filterable by season, week, position, team, and salary`,
  );
  const lastRow = values.length + 3;
  sheet.getRange(`A4:L${lastRow}`).values = values;
  sheet.getRange("A4:L4").format = {
    fill: navy,
    font: { color: white, bold: true },
    verticalAlignment: "center",
  };
  sheet.getRange("A5:L24").format = {
    font: { color: darkText, size: 9 },
  };
  sheet.getRange(`A5:C${lastRow}`).format.numberFormat = "0";
  sheet.getRange(`I5:I${lastRow}`).format.numberFormat = "$#,##0";
  sheet.getRange(`J5:J${lastRow}`).format.numberFormat = "0.00";
  sheet.getRange("A:B").format.columnWidth = 9;
  sheet.getRange("C:C").format.columnWidth = 16;
  sheet.getRange("D:D").format.columnWidth = 25;
  sheet.getRange("E:H").format.columnWidth = 10;
  sheet.getRange("I:J").format.columnWidth = 15;
  sheet.getRange("K:L").format.columnWidth = 17;
  sheet.getRange("A1:L24").format.font.name = "Aptos";
  sheet.freezePanes.freezeRows(4);
  sheet.freezePanes.freezeColumns(2);
}

writeSalarySheet(dkSheet, "DK", "DraftKings Historical Salaries");
writeSalarySheet(fdSheet, "FD", "FanDuel Historical Salaries");

titleBand(
  checks,
  "A1:D2",
  "Data Quality Checks",
  "A failed check should block downstream DFS value analysis until resolved",
);
checks.getRange("A4:C12").values = [
  ["Check", "Value", "Status"],
  ...checkRecords.map((record) => [
    record.check,
    record.value,
    record.check === "rows" || record.value === 0 ? "PASS" : "FAIL",
  ]),
];
checks.getRange("A4:C4").format = {
  fill: navy,
  font: { color: white, bold: true },
};
checks.getRange("A5:C12").format = {
  font: { color: darkText },
  borders: { bottom: { color: "#E6EBEF", style: "continuous", weight: 1 } },
};
checks.getRange("B5:B12").format.numberFormat = "#,##0";
checks.getRange("C5:C12").conditionalFormats.add("containsText", {
  text: "PASS",
  format: { fill: paleGreen, font: { color: "#246B35", bold: true } },
});
checks.getRange("C5:C12").conditionalFormats.add("containsText", {
  text: "FAIL",
  format: { fill: paleRed, font: { color: "#9B1C1C", bold: true } },
});
checks.getRange("A:A").format.columnWidth = 28;
checks.getRange("B:C").format.columnWidth = 15;
checks.getRange("A1:D12").format.font.name = "Aptos";
checks.freezePanes.freezeRows(4);

titleBand(
  methods,
  "A1:H2",
  "Sources & 2026 Operating Procedure",
  "Use the free historical archive for research and official platform exports as the forward source of record",
);
sectionHeader(methods, "A4:H4", "Source assessment");
methods.getRange("A5:H9").values = [
  ["Source", "Use", "Verified coverage", "Reliability note", "URL", null, null, null],
  ["RotoGuru", "Historical DK/FD player-week salaries", "DK 2014–2021; FD 2011–2021", "Free and reproducible, but NFL tables stop after 2021", "http://rotoguru1.com/cgi-bin/fyday.pl", null, null, null],
  ["DraftKings salary CSV", "Weekly 2026 source of record", "Active slates", "Official export; save before the slate disappears", "https://www.draftkings.com/", null, null, null],
  ["DraftKings draftables endpoint", "Optional automated 2026 capture", "Active draft groups only", "Undocumented; can change without notice", "https://api.draftkings.com/draftgroups/v1/draftgroups/", null, null, null],
  ["DraftKings contest results CSV", "Ownership / lineup results", "10 days after contest", "Official support page states completed-contest CSVs are retained for 10 days", "https://support.draftkings.com/dk/en-us/how-does-draftkings-keep-fantasy-sports-contests-transparent?id=kb_article_view&sysparm_article=KB0010720", null, null, null],
];
methods.getRange("A5:H5").format = {
  fill: lightGray,
  font: { bold: true, color: navy },
};
methods.getRange("A6:H9").format = {
  wrapText: true,
  verticalAlignment: "top",
  borders: { bottom: { color: "#E6EBEF", style: "continuous", weight: 1 } },
};
sectionHeader(methods, "A11:H11", "Weekly 2026 procedure");
methods.getRange("A12:H18").values = [
  ["Step", "Timing", "Action", "Validation", null, null, null, null],
  [1, "When slate opens", "Download the official DK salary CSV for every slate you may play.", "Filename includes platform, season, week, and slate.", null, null, null, null],
  [2, "Same session", "Optionally run the current DK capture to preserve the API payload.", "Player count and salary range reconcile to the CSV.", null, null, null, null],
  [3, "Before lock", "Save the final salary file if the player pool changes.", "Captured timestamp is retained.", null, null, null, null],
  [4, "After games", "Archive contest results CSV within 10 days when ownership analysis is needed.", "Contest ID and slate name are recorded.", null, null, null, null],
  [5, "Pipeline run", "Place salary files in data/raw/dfs_salary_uploads and ingest them.", "No missing season/week, player, or salary fields.", null, null, null, null],
  [6, "Model join", "Join salary to the pre-lock fantasy projection using platform player ID first.", "Name/team fallback matches are separately audited.", null, null, null, null],
];
methods.getRange("A12:H12").format = {
  fill: lightGray,
  font: { bold: true, color: navy },
};
methods.getRange("A13:H18").format = {
  wrapText: true,
  verticalAlignment: "top",
  borders: { bottom: { color: "#E6EBEF", style: "continuous", weight: 1 } },
};
sectionHeader(methods, "A20:H20", "Commands");
methods.getRange("A21:H23").values = [
  ["Purpose", "PowerShell command", null, null, null, null, null, null],
  ["One-time free backfill", ".\\run_dfs_salaries.ps1 -Backfill", null, null, null, null, null, null],
  ["Ingest saved official files", ".\\run_dfs_salaries.ps1 -IngestFiles", null, null, null, null, null, null],
];
methods.getRange("A21:H21").format = {
  fill: lightGray,
  font: { bold: true, color: navy },
};
methods.getRange("B22:B23").format = {
  fill: "#EEF2F6",
  font: { name: "Consolas", color: darkText },
};
methods.getRange("A:A").format.columnWidth = 22;
methods.getRange("B:B").format.columnWidth = 24;
methods.getRange("C:C").format.columnWidth = 38;
methods.getRange("D:D").format.columnWidth = 42;
methods.getRange("E:E").format.columnWidth = 52;
methods.getRange("F:H").format.columnWidth = 4;
methods.getRange("A1:H23").format.font.name = "Aptos";
methods.getRange("A5:H9").format.rowHeight = 40;
methods.getRange("A12:H18").format.rowHeight = 37;
methods.freezePanes.freezeRows(4);

const dashboardInspect = await workbook.inspect({
  kind: "table",
  range: "Dashboard!A1:N31",
  include: "values,formulas",
  tableMaxRows: 31,
  tableMaxCols: 14,
  maxChars: 8000,
});
await fs.writeFile(
  path.join(outputDir, "dashboard_inspect.ndjson"),
  dashboardInspect.ndjson,
  "utf8",
);

const errorScan = await workbook.inspect({
  kind: "match",
  searchTerm: "#REF!|#DIV/0!|#VALUE!|#NAME\\?|#N/A",
  options: { useRegex: true, maxResults: 300 },
  summary: "final formula error scan",
});
await fs.writeFile(
  path.join(outputDir, "formula_error_scan.ndjson"),
  errorScan.ndjson,
  "utf8",
);

const renderTargets = [
  ["Dashboard", "A1:N31", "dashboard_preview.png"],
  ["Coverage", "A1:K34", "coverage_preview.png"],
  ["DraftKings Salaries", "A1:L24", "dk_preview.png"],
  ["FanDuel Salaries", "A1:L24", "fd_preview.png"],
  ["Checks", "A1:D12", "checks_preview.png"],
  ["Sources & Operations", "A1:H23", "methods_preview.png"],
];
for (const [sheetName, range, fileName] of renderTargets) {
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
await output.save(path.join(outputDir, "NFL_DFS_Salary_History.xlsx"));
console.log(`Saved workbook with ${salaryRecords.length.toLocaleString()} salary rows.`);
