# State-wise Development Analysis Using R

## Project Overview

This project presents a data-driven analytical system developed in R to evaluate development disparities across Indian states using multiple socio-economic and infrastructure indicators.

The system integrates data preprocessing, composite score modeling, clustering analysis, regression analysis, database connectivity, visualization, and an interactive Shiny dashboard to identify development patterns and lagging states.

---

## Research Question

**How do selected socio-economic and infrastructure indicators contribute to the composite development score of Indian states, and which states exhibit comparatively lower development performance?**

---

## Features

- Data preprocessing and integration
- Missing value handling
- Min-Max normalization
- Composite Development Score calculation
- K-Means clustering analysis
- Multiple linear regression analysis
- SQLite database integration
- Interactive Shiny analytical dashboard
- Visualization of development patterns across states

---

## Indicators Used

The project uses the following indicators:

- Internet penetration
- Employment indicator (EUS)
- Urban-rural accessibility
- NABH-accredited hospitals

---

## Technologies Used

- R Programming Language
- Shiny
- ggplot2
- dplyr
- DBI
- RSQLite
- DT
- factoextra

---

## Database Integration

The processed dataset is stored in an SQLite database for structured storage and retrieval within the analytical system.

Database file.
```text
development_analysis.db
