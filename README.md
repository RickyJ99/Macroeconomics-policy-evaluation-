
# MACROECONOMIC POLICY EVALUATION - Assignment Report

## Dataset
The dataset "oil data.gdt" contains monthly time series data from 1973:1 to 2007:12, including:
- Percent change in global crude oil production (∆prodt)
- Index of real economic activity (reat) expressed in logs
- Real price of oil (rpot) expressed in logs

## 1. Testing Integration of reat
- Plotted the time series of reat and explored the correlogram.
- Conducted an Augmented Dickey-Fuller (ADF) test for testing the null hypothesis of reat being I(1).
- Chose lag order using BIC criterion.
- ADF regression results: ...
- Test result: ...
- Motivation for deterministic component: ...

## 2. ADF Test on ∆reat
- Conducted ADF test on the first difference of reat, ∆reat = reat - reat(-1).
- Motivated choice for deterministic component: ...

## 3. ARMA(p,q) Model Selection for ∆reat
- Selected best ARMA(p,q) specification using BIC criterion.
- Estimated ARMA models using conditional maximum likelihood with `bhhh_toler 0.0001`.
- Reported results of ARMA estimation.
- Plotted residuals and their ACF.
- Commented on white noise characteristics of residuals.

## 4. Reduced Form VAR
- Estimated a reduced form VAR for yt = [∆prodt;reat;rpot]′.
- Selected lag order using BIC criterion.
- Plotted VAR inverse roots and residuals.
- Concluded about the stationarity of the VAR.

## 5. Identification Strategy
- Proposed identification strategy based on given assumptions (a), (b), and (c) for C matrix coefficients.

## 6. Matrix C Estimation and Shock Responses
- Estimated matrix C using a VAR with 24 lags.
- Plotted responses of reat and rpot to specified shocks for a 25-period horizon.
- Commented on the results of the shock responses: (a) negative shock to oil supply, (b) positive shock to aggregate demand, (c) positive shock to oil-market specific demand.

## 7. Historical Decomposition of rpot
- Plotted Historical Decomposition for rpot using SVAR specification from point 5.
- Briefly commented on the results of the Historical Decomposition.

For detailed results and further analysis, refer to the complete report in the document.
