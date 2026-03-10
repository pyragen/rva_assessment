# RVA Specialist Coding Assessment
**Candidate: Brandon Tran | Date: March 2026**

## Project Overview
This repository contains the completed technical assessment for the RVA Specialist position. The project demonstrates data manipulation using `tidyverse`, clinical reporting with `gtsummary` and interactive application development using `shiny`. Data comes from the CDISC ADaM test dataset (`pharmaverseadam`).

## Repository Structure
The project is organized into independent directories for each question:

* **`question_1/`**: R script and HTML output for a Treatment-Emergent Adverse Event (TEAE) Summary Table. 
* **`question_2/`**: R script and PNG output for a stacked bar chart of Unique Subjects by SOC and Severity. 
* **`question_3/`**: An interactive Shiny Dashboard.
    * **Live App**: [https://b2765h-pyragen.shinyapps.io/question3/]
    * **Source**: Contained within the `question_3/` directory.

---

### How to Run Locally
1.  **Clone the repository**:
    ```bash
    git clone https://github.com/pyragen/rva_assessment.git
    ```
2.  **Open the Project**: Launch `rva_assessment.Rproj` in RStudio.
3.  **Restore the Library**: In the R console, run:
    ```R
    renv::restore()
    ```
    Alternatively, install the packages from CRAN manually:
    ```R
    install.packages(c("pharmaverseadam", "tidyverse", "gtsummary", "shiny")
    ```
5.  **Execute**: Run scripts in `question1/question_1.R`, `question2/question_2.R`, and `question3/question_3.R` using the `source()` command.
