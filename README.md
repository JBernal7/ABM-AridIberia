# ABM-Ailanthus-Calibration

Reproducible repo for the *Ailanthus altissima* ABM calibration (baseline, 10 m buffer) and analysis.

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
