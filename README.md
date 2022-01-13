# Badminton
**Reports for presence statistics of members of BCT80 Thalwil**

Code for reading and reporting on members' presence file
(file not public)

- data_import.R extracts the data from multiple excel sheets and harmonizes them
- data_analysis.R calculates certain figures and prepares tibbles for plots. Relies on data_import  
- plots_page1.R creates the plots for the first page of a pdf and arranges them accordingly in a Grob. Also extracts from an existing excel-pdf-output file a PNG to insert into the newly created pdf.  
- plots_page2.R creates the plots for the second page of a pdf and arranges them accordingly in a Grob
- control_flow.R provides the libraries, the generic code and sources all the other files before printing the 2 Grob's into one pdf file.

First page contains mainly data of most actual year which contains already data in the members' presence file. There is currrently no parameter to choose the current year.
