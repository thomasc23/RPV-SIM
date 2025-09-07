#!/usr/bin/env node
const { execFileSync } = require("child_process");
const fs = require("fs");
const path = require("path");

// ---- args ----
function arg(flag, def = null) {
  const i = process.argv.indexOf(flag);
  return i > -1 && process.argv[i + 1] ? process.argv[i + 1] : def;
}
const votesArg = arg("--votes");
const cvapArg  = arg("--cvap");
if (!votesArg || !cvapArg) {
  console.error("Required: --votes <precinct .shp> --cvap <CVAP .shp>");
  process.exit(1);
}

// Resolve to .../Simulation/data (no chdir)
const dataRoot = path.resolve(__dirname, "..", "data");
const toAbs = p => (path.isAbsolute(p) ? p : path.resolve(dataRoot, p));
const votesAbs = toAbs(votesArg);
const cvapAbs  = toAbs(cvapArg);

// State code from path
const normVotes = votesAbs.replace(/\\/g, "/");
const m = normVotes.match(/\/States\/([A-Z]{2})\//);
const stateCode = (m ? m[1] : "XX").toLowerCase();

// Vote year -> field names
function detectVoteYY(p) {
  const y4 = (p.match(/20(16|20|24)/) || [])[0];
  if (y4) return y4.slice(-2);
  const yy = (p.match(/(?:vest_|_)(16|20|24)\b/) || [])[1];
  return yy || "16";
}
const voteYY = detectVoteYY(normVotes);
const REP_FIELDS = { "16": "G16PRERTRU", "20": "G20PRERTRU", "24": "G24PRERTRU" };
const DEM_FIELDS = { "16": "G16PREDCLI", "20": "G20PREDBID", "24": "G24PREDHAR" };
const repField = REP_FIELDS[voteYY] || REP_FIELDS["16"];
const demField = DEM_FIELDS[voteYY] || DEM_FIELDS["16"];

// CVAP year/fields
const cvapBase  = path.basename(cvapAbs, ".shp");
const cvapYear4 = (cvapBase.match(/(\d{4})/) || [])[1] || "2020";
const cvapYY    = cvapYear4.slice(-2);

// Need: total, non-Hispanic total, Hispanic, and NH race-alone categories
const cvapFields = [
  `CVAP_TOT${cvapYY}`, `CVAP_NHS${cvapYY}`, `CVAP_HSP${cvapYY}`,
  `CVAP_AMI${cvapYY}`, `CVAP_ASI${cvapYY}`, `CVAP_BLA${cvapYY}`,
  `CVAP_NHP${cvapYY}`, `CVAP_2OM${cvapYY}`
];

// Output path (.shp)
const outDir  = path.join(dataRoot, "States", stateCode.toUpperCase(), "out");
fs.mkdirSync(outDir, { recursive: true });
const outFileBase = path.join(outDir, `${stateCode}_${voteYY}`);
const outFileShp  = outFileBase + ".shp";

// Mapshaper args
const precinctLayer = "precincts";
const cbgLayer      = "cbg";

// Shapefile-safe names (≤10 chars)
const renameList = [
  `rep_votes=${repField}`,
  `dem_votes=${demField}`,
  // raw CVAP interpolates (kept for auditing)
  `cvap_tot=CVAP_TOT${cvapYY}`,
  `cvap_nhs=CVAP_NHS${cvapYY}`,
  `cvap_hsp=CVAP_HSP${cvapYY}`,
  `cvap_ami=CVAP_AMI${cvapYY}`,
  `cvap_asi=CVAP_ASI${cvapYY}`,
  `cvap_bla=CVAP_BLA${cvapYY}`,
  `cvap_nhp=CVAP_NHP${cvapYY}`,
  `cvap_2om=CVAP_2OM${cvapYY}`
].join(",");

// Standardize ID/label fields across vintages
const makeIdAndLabel = [
  `prec_id = (typeof PCTKEY!=='undefined' ? PCTKEY :`,
  `          (typeof UNIQUE_ID!=='undefined' ? UNIQUE_ID :`,
  `          (typeof TX_VTD!=='undefined' ? TX_VTD :`,
  `          (typeof VTD!=='undefined' ? VTD :`,
  `          (typeof PREC!=='undefined' ? PREC : null)))));`,
  `prec_label = (typeof PREC!=='undefined' ? PREC :`,
  `             (typeof TX_VTD!=='undefined' ? TX_VTD :`,
  `             (typeof County!=='undefined' ? County :`,
  `             (typeof COUNTY!=='undefined' ? COUNTY : null))));`
].join(" ");

// Round CVAP to integers
const roundCvap = [
  `cvap_tot = (cvap_tot==null?0:Math.max(0, Math.round(cvap_tot)));`,
  `cvap_nhs = (cvap_nhs==null?0:Math.max(0, Math.round(cvap_nhs)));`,
  `cvap_hsp = (cvap_hsp==null?0:Math.max(0, Math.round(cvap_hsp)));`,
  `cvap_ami = (cvap_ami==null?0:Math.max(0, Math.round(cvap_ami)));`,
  `cvap_asi = (cvap_asi==null?0:Math.max(0, Math.round(cvap_asi)));`,
  `cvap_bla = (cvap_bla==null?0:Math.max(0, Math.round(cvap_bla)));`,
  `cvap_nhp = (cvap_nhp==null?0:Math.max(0, Math.round(cvap_nhp)));`,
  `cvap_2om = (cvap_2om==null?0:Math.max(0, Math.round(cvap_2om)));`
].join(" ");

// Build mutually-exclusive W/B/H
// - Non-Hispanic White alone = NH total minus NH non-white categories
// - Non-Hispanic Black alone = cvap_bla
// - Hispanic (any race)      = cvap_hsp
const buildExclusive = [
  `cvap_whtx = Math.max(0, cvap_nhs - (cvap_ami + cvap_asi + cvap_bla + cvap_nhp + cvap_2om));`,
  `cvap_blkx = Math.max(0, cvap_bla);`,
  `cvap_hspx = Math.max(0, cvap_hsp);`,
  // diagnostic ratio: should be ≤ 1, small >1 can happen from rounding
  `cvap_chk  = (cvap_tot>0 ? (cvap_whtx + cvap_blkx + cvap_hspx) / cvap_tot : 0);`
].join(" ");

// Final keep list (plus diagnostic)
const keepFields = [
  "prec_id","prec_label","rep_votes","dem_votes",
  "cvap_tot","cvap_whtx","cvap_blkx","cvap_hspx","cvap_chk"
].join(",");

// Snap/clean params in EPSG:3083 (meters)
const SNAP = "snap-interval=5";    // tweak if needed (2–10 m typical)

// Build command
const args = [
  "-i", votesAbs, `name=${precinctLayer}`,
  "-i", cvapAbs,  `name=${cbgLayer}`,

  // Reproject to EPSG:3083 (Texas Albers)
  "-proj", "EPSG:3083", `target=${precinctLayer}`,
  "-proj", `match=${precinctLayer}`, `target=${cbgLayer}`,

  // Pre-join snapping/cleaning
  "-clean", SNAP, `target=${precinctLayer}`,
  "-clean", SNAP, `target=${cbgLayer}`,

  // Area-weighted interpolation of required CVAP fields
  "-join", `source=${cbgLayer}`, `target=${precinctLayer}`,
    `interpolate=${cvapFields.join(",")}`,
    "fields=", "min-overlap-area=1e-6",

  // Post-join clean
  "-clean", SNAP, `target=${precinctLayer}`,

  // Rename to short names
  "-rename-fields", `target=${precinctLayer}`, renameList,

  // IDs
  "-each", `target=${precinctLayer}`, makeIdAndLabel,

  // Round CVAPs, then build exclusive W/B/H and diagnostic
  "-each", `target=${precinctLayer}`, roundCvap,
  "-each", `target=${precinctLayer}`, buildExclusive,

  // Keep tidy schema
  "-filter-fields", `target=${precinctLayer}`, keepFields,

  // Write Shapefile in EPSG:3083, quantized to 1 m grid
  "-o", "force", "format=shapefile", "precision=1",
        `target=${precinctLayer}`, outFileShp
];

console.log("votes:", votesAbs);
console.log("cvap :", cvapAbs);
console.log("out  :", outFileShp);
console.log("voteYY:", voteYY, "repField:", repField, "demField:", demField, "cvapYY:", cvapYY);

try {
  execFileSync("mapshaper", args, { stdio: "inherit" });
  console.log(`\n✅ Wrote ${outFileShp}`);
  console.log("   Fields: prec_id, dem_votes, rep_votes, cvap_tot, cvap_whtx (NH White), cvap_blkx (NH Black), cvap_hspx (Hispanic), cvap_chk");
  console.log("   Tip: summarize cvap_chk in R; values should be ≤ 1 (± tiny rounding).");
} catch (e) {
  console.error("\n❌ mapshaper failed.");
  process.exit(2);
}

