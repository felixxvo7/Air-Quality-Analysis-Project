---
output:
  pdf_document: default
  html_document: default
---

# Approximate Formulas for PAN, HONO, and SAPRC Proxy

## 1. Peroxyacetyl Nitrate (PAN) Approximation

### Formula (Using **Polynomial Regression** for C6H6):

$$
PAN_{proxy} = a \times NOx + b\times NOx^2 + c \times C6H6 + d\times C6H6^2 + e \times O_3 + f \times T
$$ Where:

\- **NOx (NO + NO₂)** → Major precursor for PAN.

-   **C6H6 (Benzene)** → A proxy for VOCs that contribute to PAN formation.

\- **C6H6² (Quadratic Term)** → Captures nonlinear VOC effects.

\- **O₃ (Ozone sensor value)** → PAN is strongly associated with photochemical smog.

\- **T (Temperature)** → Higher temperatures accelerate PAN formation.

#### Sources:

1.  Singh, H. B. (1987). *Peroxyacetyl nitrate (PAN): its chemistry and role in the atmosphere*. Advances in Environmental Science and Technology.
2.  Grosjean, D. (1992). *Formation, fate, and impact of peroxyacetyl nitrates in the atmosphere*. Environmental Science & Technology.

------------------------------------------------------------------------

## 2. Nitrous Acid (HONO) Approximation

### Formula:

$$
HONO_{proxy} = f \times NOx + g \times RH + h \times (NOx\times Rh) + i \times T + j  \times(NO_x\times T)
$$ Where:

\- **NOx (NO + NO₂)** → HONO is formed from heterogeneous NO₂ reactions.

\- **RH (Relative Humidity)** → Surface chemistry enhances HONO formation under humid conditions.

\- **NOx Sensor (PT08.S3.NOx.)** → Used as an alternative for NO₂ levels.

\- **T (Temperature)** → Affects the equilibrium between HONO formation and photolysis.

|  |  |
|------------------------------------|------------------------------------|
| **NOx × RH** | More HONO forms under humid conditions because NO₂ reacts with water vapor. |

|  |  |
|------------------------------------|------------------------------------|
| **NOx × T** | At higher temperatures, HONO photolyzes faster, reducing its concentration. |

#### Sources:

1.  Finlayson-Pitts, B. J., & Pitts, J. N. (2000). *Chemistry of the upper and lower atmosphere*.
2.  Kleffmann, J. (2007). *Daytime sources of nitrous acid (HONO) in the atmospheric boundary layer*. ChemPhysChem.

------------------------------------------------------------------------

## 3. SAPRC Proxy Approximation

### Formula:

$$
SAPRC_{proxy} = k \times NOx+ p \times O_3 + q \times T + r \times RH
$$ Where:

\- **NOx (NO + NO₂)** → SAPRC models focus on NOx-related photochemistry.

\- **C6H6 (Benzene, VOC indicator)** → Represents hydrocarbon chemistry in SAPRC models.

\- **C6H6² (Quadratic Term)** → Captures nonlinear VOC effects.

\- **O₃ (Ozone sensor PT08.S5.O3.)** → Ozone is critical in photochemical cycles.

\- **T (Temperature)** → Affects reaction rates and photolysis rates.

\- **RH (Relative Humidity)** → Influences radical chemistry in SAPRC models.

\- **k, m, n, p, q, r** → Empirical coefficients derived from regression models.

#### Sources:

1.  Carter, W. P. L. (2010). *Development of the SAPRC-07 chemical mechanism*. Atmospheric Environment.
2.  Carter, W. P. L. (1990). *A detailed mechanism for the gas-phase atmospheric reactions of organic compounds*. Atmospheric Environment.

------------------------------------------------------------------------

To estimate PAN, HONO, and SAPRC Proxy:

1\. **Train a Linear Regression or Polynomial Regression model** using real-world NO₂ data.

2\. **Extract best-fit coefficients** to apply in forecasting models.

3\. **Use these approximations as exogenous variables in SARIMAX .**
