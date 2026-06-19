# User Manual

**CUSUM RAP Tool**  
Early-warning analysis for weekly case counts  
Documentation revision: June 2026  
Language: English

---

# 1. Purpose and analytical scope

The CUSUM RAP Tool applies a one-sided upper cumulative sum procedure to weekly case counts. It is designed to highlight sustained increases above an expected baseline for each selected analysis unit.

> **Interpretation.** A CUSUM alarm is an early-warning signal, not a diagnosis, causal conclusion, or confirmation of an outbreak. Review data quality, reporting changes, denominators, local context, and other surveillance evidence before acting.

- The analysis is performed separately for every selected geographic unit.
- The current workflow is count based. Population may be uploaded, but it is not required for the standard CUSUM calculation.
- The detection period controls the recent weekly window used for analysis; prepared-data coverage rules may require a longer underlying history.


# 2. Input data

Upload one or more API-POP-compatible CSV files. Comma- and semicolon-delimited files are supported. Each row should represent one geographic unit in one epidemiological week.

| Internal column | Required | Meaning | Accepted aliases or examples |
| --- | --- | --- | --- |
| year | Yes | Epidemiological year. | year, anio, año, epiyear |
| week | Yes | Epidemiological week, normally 1-53. | week, semana, epiweek, se |
| country | Recommended | Country or national unit. | Argentina |
| level1 | Depends on selected level | First administrative level. | province, region, Level1 |
| level2 | Depends on selected level | Second administrative level. | department, district, LAD, Level2 |
| level3 / level4 | Depends on selected level | More detailed administrative levels. | fraction, MSOA, Level3 |
| n_cases | Yes | Observed weekly cases or event count. | n_cases, cases, case_count, count, n |
| population | Optional | Not required by the current count-based CUSUM. | population, pop, denominator |

## Multiple uploaded files

All newly uploaded files are active by default. Use Manage files to include or exclude individual files, remove inactive files, or clear the session.

When more than one file is active, the tool asks how to resolve duplicate location/week records:

| Option | Effect |
| --- | --- |
| Keep Newest File Information | For an overlapping location/week, retain the record from the newest active file. |
| Keep Oldest File Information | Retain the record from the oldest active file. |
| Add Together (Sum) | Add overlapping case counts together. Use only when the records represent complementary data rather than duplicate exports. |

> **Data safety.** File management and overlap resolution affect only the in-memory analysis session. The original uploaded files are not modified.


# 3. Configure the analysis

1. Upload the CSV file or files.
2. Select a country when more than one country is enabled in config.json.
3. Choose the Geographic Level.
4. Optionally select specific Locations. Leave the field empty to include all available locations.
5. Set the Detection Period, expected frequency method, ARL0, relative risk, and threshold h.
6. Select Run Analysis.

| Control | Current behavior |
| --- | --- |
| Geographic Level | Defines the administrative level used to build independent analysis units. |
| Locations | Limits CUSUM to the selected units. An empty selection includes all units available at the chosen level. |
| Detection Period (Weeks) | UI range 4-260; default 52. The configured coverage validation currently requires at least the larger of this value and 52 prepared weeks. |
| Automatic expected frequency | Fits an expected weekly count separately for each analysis unit using the current Poisson-model workflow. |
| Manual expected frequency (mu0) | Uses the entered fixed expected weekly case count. Enter a count per unit per week, not a prevalence rate. |
| Target ARL0 | Desired average run length when the process is in control; used to display a recommended h. |
| Relative Risk (RR) | Increase the CUSUM is intended to detect. It is used with the expected count to calculate k. |
| Calculated k | Reference value shown by the app; it is calculated from RR and the expected count. |
| h Threshold | Decision threshold. A larger value generally produces fewer alarms and slower detection. |

> **Coverage validation.** Missing location/week combinations can be filled with zero cases in the prepared dataset, but the tool checks observed coverage first. It stops when the time span is too short or when too much of the series would be inferred.


# 4. Review the outputs

## Overview

The Overview heatmap summarizes alarms across locations and recent weeks. Use it to identify where and when the CUSUM crossed the threshold. The detection period controls the weeks displayed for alarm monitoring.

## Detailed View

Choose a location through the hierarchical selectors. The summary card reports the current administrative selection, analysis level, weeks analysed, date range, population when available, whether alarms occurred, and the most recent alarm week.

- Observed series plot: compares observed weekly values with the expected baseline.
- CUSUM process plot: displays the cumulative statistic and threshold over time.
- Download Bar Plot and Download Trends export the selected plots after results are available.

## Analysis Results

The Unit-level reference table contains expected weekly cases and expected rates once per unit. The Weekly results table contains the time-series values, CUSUM statistic, alarm state, and associated parameters. Use Search by to filter a chosen column.

## Prepared Data

This tab shows the cleaned, aggregated, overlap-resolved, gap-completed dataset actually used by the analysis. It includes a coverage assessment, a preparation/change log, summary information, the number of added rows, a filterable table, and a CSV download.

> **Display settings.** The Help tab can increase interface and plot font size and change the number of decimal places shown in tables. These settings do not round downloaded CSV values.


# 5. Interpretation guide

For each detection week, the tool standardizes the difference between observed and expected counts, subtracts k, and adds positive evidence to a one-sided cumulative statistic. The statistic is never allowed below zero. An alarm is marked when the statistic reaches or exceeds h; the current application resets the running statistic after an alarm while preserving the peak value in that week.

| Pattern | Suggested interpretation |
| --- | --- |
| Observed values repeatedly above expected | The CUSUM tends to accumulate and may reach h. |
| Observed values close to or below expected | The CUSUM tends to remain low or return to zero. |
| Single isolated high week | May or may not alarm, depending on magnitude and current accumulated evidence. |
| Many zero-filled weeks | Review coverage warnings and confirm that missing reports truly represent zero cases. |
| Alarm in several neighbouring units | Investigate shared reporting changes, common exposures, population movement, and broader epidemiological evidence. |

- Confirm that the expected baseline is plausible for each unit.
- Review the preparation log for overlap resolution and gap filling.
- Check whether coding, geography, or reporting practices changed during the selected period.
- Treat the result as one component of a broader surveillance assessment.


# 6. Downloads and reproducibility

| Download | Contents |
| --- | --- |
| Download Bar Plot | Observed and expected weekly series for the current detailed location. |
| Download Trends | CUSUM process plot for the current detailed location. |
| Download Results CSV | Complete analysis output for all analysed units and weeks. |
| Download Prepared Data CSV | Cleaned and aggregated input used by the analysis. |

On-screen table filters and decimal-place settings affect only the view. Downloads retain the underlying rows and full numeric precision produced by the analysis.


# 7. Troubleshooting

| Problem or message | What to check |
| --- | --- |
| Missing required columns | Confirm year, week, n_cases or an accepted alias, and the column needed for the selected geographic level. |
| No locations available | The chosen geographic level may not exist in the active data, or the values may be blank. |
| Insufficient prepared weeks | Upload a longer weekly history or reduce the detection period, noting that the current configured minimum is 52 prepared weeks. |
| Low observed coverage | Confirm whether absent weeks represent true zero cases. Upload a more complete series when they are missing reports. |
| Unexpectedly large counts after combining files | Review the active files and the selected overlap method, especially Add Together (Sum). |
| Automatic baseline appears implausible | Inspect the selected units and history, then compare with an appropriate manual expected weekly count. |

> **Escalation package.** When reporting a problem, include the active file names, selected country and level, location selection, parameter values, the preparation log, and the exact error text. Do not send identifiable or restricted data through an unapproved channel.


# 8. Glossary

| Term | Meaning |
| --- | --- |
| mu0 / expected frequency | Expected weekly case count for an analysis unit. |
| k | Reference value that controls how strongly deviations contribute to the CUSUM. |
| h | Decision threshold used to raise an alarm. |
| ARL0 | Average run length expected under the no-change condition. |
| RR | Relative risk increase the design is intended to detect. |
| Prepared week | A weekly row present after aggregation and optional zero filling. |
| Observed coverage | Proportion of prepared weeks originating from observed uploaded records rather than inserted zero rows. |

