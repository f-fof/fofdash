
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Economic Indicators Dashboard

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![shiny-deploy](https://github.com/aiti-flinders/aitidash/actions/workflows/shiny-deploy.yaml/badge.svg)](https://github.com/aiti-flinders/aitidash/actions/workflows/shiny-deploy.yaml)

<!-- badges: end -->

The Australian Industrial Transformation Institute Economic Indicators
dashboard enables users to generate comprehensive charts comparing
Australian employment data between states and territories, and across
demographic variables. It also shows changes to industry indicators over
time. These charts, and the data used to generate them, can then be
downloaded for use offline.

## Navigating the Dashboard

You can navigate through the different modules by selecting them from
the side menu. Any chart, as it appears on screen, as well as the data
behind it, can be downloaded in the Downloads section of each module.

## Development Notes

This dashboard is currently in beta, and may occasionally show errors to
the user. The most common cause for an error is the selection of a
combination of *Region*, *Indicator*, *Series Type*, and *Date* for
which data is not available. Selecting another combination of variables
should solve any problems.

Additional modules are currently in development, and will be added as
they are completed.

The Summary, and Employment Insights modules are updated on
approximately the third Thursday of the month when the ABS Labour Force
Survey is released. The Industry Insights module is updated every
quarter when the ABS Detailed Labour Force Survey is released. Check
back regularly for access to the latest data.

For any comments, requests, or issues, please contact
<aiti@flinders.edu.au> or file an issue here.

## Definitions

<table>
<colgroup>
<col style="width: 8%" />
<col style="width: 91%" />
</colgroup>
<thead>
<tr class="header">
<th>Term</th>
<th>Definition</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Indicator</td>
<td>A time series variable, measured and collected by the ABS</td>
</tr>
<tr class="even">
<td>Series Type</td>
<td><p>How the observed data has been processed by the ABS:</p>
<ul>
<li><p>original: the observed, unprocessed data</p></li>
<li><p>seasonally adjusted: observed data processed to remove influences
that are systematic and related to the calendar</p></li>
<li><p>trend: observed data processed to remove calendar related, and
other irregular effects, to show the long term movement of an
indicator.</p></li>
</ul></td>
</tr>
<tr class="odd">
<td>Region</td>
<td>States and Territories in Australia, or Australia itself.</td>
</tr>
</tbody>
</table>

## Notes on Data Availability

Data for this dashboard is sourced from the Australian Bureau of
Statistics. There is not universal coverage across regions, economic
indicators, or series types.
