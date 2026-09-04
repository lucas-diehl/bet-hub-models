#!/usr/bin/env node
// ============================================================================
// DFS ENGINE - simulator/engine parity test.
// Extracts the REAL scorer (gridAt / pctBeaten / activeContest / activeMult /
// simMetrics) from spine/assets/dashboard.html and runs it on the golden fixture
// produced by gen_fixture.R, asserting every metric matches the R reference within
// tolerance. Because it evals the functions straight out of the HTML, any edit to
// the in-browser math that diverges from grade_candidates() fails CI.
// Run (from dfs-engine/, after gen_fixture.R):  node tests/parity/parity.js
// ============================================================================
const fs = require("fs");
const path = require("path");

const here = __dirname;
const root = path.resolve(here, "..", "..");                 // dfs-engine/
const htmlPath = path.join(root, "spine", "assets", "dashboard.html");
const html = fs.readFileSync(htmlPath, "utf8");

// pull the contiguous scorer block: from `function gridAt` up to `function mbox`
const si = html.indexOf("function gridAt");
const ei = html.indexOf("function mbox");
if (si < 0 || ei < 0 || ei <= si) {
  console.error("PARITY: could not locate the scorer block in dashboard.html");
  process.exit(2);
}
const slice = html.slice(si, ei);

// load the extracted functions into a scope that supplies the globals they close over
function loadScorer(js) {
  const mean = (a) => { let s = 0; for (const x of a) s += x; return s / a.length; };
  const S = { simContest: 0 };                               // no contest selected -> uses sp.gpp_mult
  eval(js);                                                  // defines gridAt,pctBeaten,activeContest,activeMult,simMetrics
  return simMetrics;                                         // eslint-disable-line no-undef
}
const simMetrics = loadScorer(slice);

const fx = JSON.parse(fs.readFileSync(path.join(here, "_fixture.json"), "utf8"));
const exp = JSON.parse(fs.readFileSync(path.join(here, "_expected.json"), "utf8"));
const sp = {
  k: fx.k, field_size: fx.field_size, qlevels: fx.qlevels,
  players: fx.players, draws: fx.draws, fgrid: fx.fgrid,
  gpp_mult: fx.gpp_mult, contests: null,
};

const KEYS = ["proj", "cash", "top1", "win", "fin", "gppRoi", "roiStd", "dupe", "own"];
const TOL = 1e-6;
let fails = 0, checks = 0;
fx.lineups.forEach((sel, li) => {
  const m = simMetrics(sp, sel);
  const e = Array.isArray(exp) ? exp[li] : exp;
  KEYS.forEach((kk) => {
    checks++;
    const a = Number(m[kk]), b = Number(e[kk]), d = Math.abs(a - b);
    if (!(d <= TOL)) { fails++; console.log(`  FAIL  L${li} ${kk}: js=${a} r=${b} diff=${d.toExponential(2)}`); }
  });
  console.log(`  L${li}: gppRoi=${(m.gppRoi).toFixed(4)} cash=${(m.cash).toFixed(4)} top1=${(m.top1).toFixed(4)} dupe=${(m.dupe).toFixed(4)}`);
});
console.log(fails
  ? `\nPARITY FAIL: ${fails}/${checks} checks off (tol ${TOL})`
  : `\nPARITY OK: ${checks} checks across ${fx.lineups.length} lineups agree within ${TOL}`);
process.exit(fails ? 1 : 0);
