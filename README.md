# Simpson-et-al.-aegerine-code
Repository for code associated with "Volatile saturation driving rare earth element enrichment in peralkaline magmas recorded by aegirine sector zoning"

This repository contains the code used to analyse the GEOROC clinopyroxene compilation.

To run these codes, you must install all packages and set your working directory appropriately. I have included basic descriptions of the codes below.

01_Simpson_GEOROC_Clinopyroxene_Formatting_Code:
  - This first script formats and filters the GEOROC file "2023-06-SGFTN_Clinopyroxenes_Simpson.csv".
  - To install the necessary packages, run this code: install. packages(c("tidyverse", "patchwork", "ggsci, "scico", "skimr", "ggtern"))
  - You must set your directory in line 11.
      - If you are using Windows, you can copy the file path from File Explorer ("C:\\Users\\...).
      - If you are using MacOS, right-click on the file in Finder, select "Get Info", and copy the file path from "Where".
  - The exported file "GEOROC_Clinopyroxene_Working_Simpson_Formatted.csv" is formatted so that it can be copied into the Excel sheet "Simpson_GEOROC_Clinopyroxene_Structural_Formulae", which performs the necessary stoichiometry calculations.
  - A .csv version of this file is provided here.

02_Simpson_GEOROC_Clinopyroxene_Plotting_Code:
 - This code performs the remaining formatting corrections, classifies the data, and generates plots used in the paper.
 - The input file is "2023-06-SGFTN_Clinopyroxenes_Simpson_CalcStructuralFormulae.csv".

Feel free to contact me (Jack Ward) at jack.ward@utas.edu.au if you have any questions!
