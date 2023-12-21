# Simpson-et-al.-aegerine-code
Repository for code associated with "Volatile saturation driving rare earth element enrichment in peralkaline magmas recorded by aegirine sector zoning"

This repository contains the code used to (1) format and filter the GEOROC clinopyroxene compilation (01_Simpson_GEOROC_Clinopyroxene_Formatting_Code), and (2) produce associated plots of clinopyroxene compositions (02_Simpson_GEOROC_Clinopyroxene_Plotting_Code).

To run these codes, you must install all packages and set your working directory appropriately. Feel free to contact me (Jack Ward) at jack.ward1@uqconnect.edu.au if you have any questions! I have included basic descriptions of the codes below.

01_Simpson_GEOROC_Clinopyroxene_Formatting_Code:
  - This first script formats and filters the GEOROC file "2023-06-SGFTN_Clinopyroxenes_Simpson.csv"
  - The exported file "GEOROC_Clinopyroxene_Working_Simpson_Formatted.csv" is formatted so that it can be copied into the Excel sheet "Simpson_GEOROC_Clinopyroxene_Structural_Formulae", which performs the necessary stoichiometry calculations.

02_Simpson_GEOROC_Clinopyroxene_Formatting_Code:
 - This code performs the remaining formatting corrections, classifies the data, and generates plots used in the paper.
 - The input file is "2023-06-SGFTN_Clinopyroxenes_Simpson_CalcStructuralFormulae.csv".
