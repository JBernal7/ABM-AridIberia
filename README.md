# ABM-AridIberia

Agent-based model of *Ailanthus altissima* expansion along Mediterranean road buffers (2008–2023), with baseline calibration and management scenarios.

## Layout
- `netlogo/` – NetLogo model(s).
- `netlogo/behaviorsearch/` – BehaviorSearch experiments.
- `R/` – nlrx runs, map comparison, plots.
- `data/obs/` – observed series/masks (placeholders, use LFS for rasters).
- `experiments/results/` – outputs (gitignored).
- `docs/` – ODD and notes.

## Quick start
```bash
git lfs install
git add .
git commit -m "init"
git branch -M main
git push -u origin main
