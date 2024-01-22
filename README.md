# Badminton

**Reports for presence statistics of members of BCT80 Thalwil**

This repo contains R Code for reading and reporting on the members' presence file (file is not public due to data privacy reasons).

## Prerequisites

File "BCT_Excel.xlsx" must be in relative Data folder. Newest tab in file has to be the one from last year (i.e. no incomplete tabs for current year). Excel export from Apple Nubers file **does not work**, must be saved by Excel application itself to avoid an abort in the `read_excel()` function. The presence list is not created inside this program, it must be included manually beforehand by saving it as a PNG file named "BCT_Excel.png" in the Data folder.

## Program flow

-   `data_import.R` extracts the data from multiple excel sheets and harmonizes them\
-   `data_analysis.R` calculates certain figures and prepares tibbles for plots. Relies on data_import\
-   `plots_page1.R` creates the plots for the first page of a pdf and arranges them accordingly in a Grob. Also loads an existing excel-png-output file to insert it into the newly created pdf.\
-   `plots_page2.R` creates the plots for the second page of a pdf and arranges them accordingly in a Grob\
-   `control_flow.R` provides the libraries, the generic code and sources all the other files before printing the 2 Grob's into one pdf file.
-   `jubilee.R` some additional charts for the 20th anniversary of statistics in BCT (not integrated into the PDF output)

Therefore, running `source "control_flow.R"` should render the whole output as described below.

## Output

PDF with 2 pages:\
First page contains mainly data of most current year (determined by the latest tab in the members' presence file). Second page contains some cumulative data since 2004 (beginning of statistics).

## Known issues

-   There is currrently no parameter to choose the current year
-   Code is not following functional programming guidelines
-   PNG must be created manually
-   Members file is an Apple Numbers file and must be converted with Excel to an xlsx file
