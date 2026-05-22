# CUSUM RAP Test Suite

Run the required tests from the project root:

```r
source("tests/testthat.R")
```

The suite validates:

- `config.json` CUSUM settings.
- API-POP CSV import and column alias handling.
- Population being optional for CUSUM.
- Missing required-column messages.
- Multiple-file combine behavior and preparation logs.
- Missing-week filling in prepared data.
- Observed weekly coverage stop/warning logic.
- Empty location selection meaning all locations.
- Overlap-resolution behavior.
- Core CUSUM parameter and alarm calculations.

These tests are intended to be run before GitHub pushes and release builds.
