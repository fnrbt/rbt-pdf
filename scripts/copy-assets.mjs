// Copy the run-time assets the viewer loads (default font, Unicode data) into
// a directory. They come from the restored NuGet packages.
import fs from "node:fs";
import os from "node:os";
import path from "node:path";

const target = path.resolve(process.argv[2] ?? "assets");
const packages = process.env.NUGET_PACKAGES ?? path.join(os.homedir(), ".nuget", "packages");

const assets = [
  { file: "NotoSans-Regular.ttf", packageId: "rbt.renderer.core" },
  { file: "ucd.dat", packageId: "rbt.unicode.core" },
];

function fromPackage(packageId, file) {
  const dir = path.join(packages, packageId);
  if (!fs.existsSync(dir)) return undefined;
  // Newest restored version first.
  const versions = fs.readdirSync(dir).sort((a, b) => b.localeCompare(a, undefined, { numeric: true }));
  return versions.map((version) => path.join(dir, version, "assets", file)).find((candidate) => fs.existsSync(candidate));
}

fs.mkdirSync(target, { recursive: true });
for (const asset of assets) {
  const source = fromPackage(asset.packageId, asset.file);
  if (!source) {
    console.error(`${asset.file}: not found in the ${asset.packageId} package`);
    process.exit(1);
  }
  fs.copyFileSync(source, path.join(target, asset.file));
}
