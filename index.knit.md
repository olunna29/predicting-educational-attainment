---
title: "Our Question and Our Data"
format:
  html:
    toc: true
    toc-depth: 2
---

::: {.hero-banner}
# Macroeconomic Dynamics & Educational Attainment {.unlisted}
<p class="hero-subtitle">Does a better economic environment lead to higher proportions of people pursuing post-secondary education?</p>
:::

## 1. Executive Motivation & Context

Understanding how macroeconomic momentum and public policy investment influence educational attainment is essential for state policymakers, academic leaders, and workforce planners. During periods of robust macroeconomic expansion, rising real median household incomes ($\Delta \text{Income}$) and expanding regional economic output ($\Delta \text{GDP}$) provide families and state budgets with greater financial liquidity to invest in human capital. Simultaneously, targeted state investments in higher education ($\Delta \text{Edu Investment}$) expand capacity and subsidize tuition costs.

Rather than asserting causal mechanisms—which require strict counterfactual identification assumptions—this data science framework focuses on **predictive modeling** and **conditional expected values**. We aim to forecast expected educational outcomes given observable macroeconomic indicators.

::: {.callout-note}
### Collaborative Note: Strategic Domain Context
State workforce development boards and university system enrollment managers can utilize these predictive conditional expectations for long-term budget allocation, facility planning, and tuition assistance strategy during shifting macroeconomic growth regimes.
:::

---

## 2. Central Predictive Research Question

To establish a quantitative data science framework, we formulate our core predictive research question centered on conditional expectations and statistically significant economic growth drivers:

::: {.callout-important}
### Central Predictive Question
> **"What is the expected increase in a state's population attaining a 4-year college degree when moving from a low-income growth environment ($< 1.0\%$) to a high-income growth environment ($\ge 3.0\%$), controlling for regional economic growth ($\Delta \text{GDP}$) and state higher education investment ($\Delta \text{Edu Investment}$)?"**
:::

### Predictive vs. Causal Framing
- **Causal Claim (Avoided)**: *"Higher median income growth directly causes state populations to obtain college degrees."*
- **Predictive Conditional Expectation (Adopted)**: *"Given that an economy transitions into a high-income growth regime ($\ge 3.0\%$), what is the updated expected value $\mathbb{E}[Y_{\text{degree}} \mid \Delta \text{Income} \ge 3.0\%, \Delta \text{GDP}, \Delta \text{Edu Investment}]$ compared to a low-growth baseline?"*

---

## 3. Dataset Architecture & Summary

The predictive framework draws upon a panel dataset spanning **50 US States** over **21 years (2004–2024)**, capturing macroeconomic growth indicators alongside state-level educational attainment statistics.



