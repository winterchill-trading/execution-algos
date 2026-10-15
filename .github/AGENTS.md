# AGENTS.md: .github/

`workflows/ci.yml` builds and tests on the infra `ox-setup` action, then builds the odoc
docs (any warning fails) and runs the shared obj-magic job.

The `pins` list must match the org dependencies in `../dune-project`, in dependency
order: `fixed-decimal`, `exchange-core`, `parquet`, `market-data`.

See `../AGENTS.md`.
