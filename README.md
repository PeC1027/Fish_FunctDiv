# Fish_FunctDiv

This RScript analyzes a 40 years time series on scientific bottom-trawl survey data available from the International Council for the Exploration of the Sea (ICES) online database, specifically on one of the longest time series for North Sea demersal fish species abundance, the ICES coordinated International Bottom Trawl Survey (IBTS), available here: https://www.ices.dk/data/data-portals/Pages/DATRAS.aspx.

The file entitled "TOT_Fdiv_Lm.csv" gathers values of functional diversity calculated for each sample via the approach introduced by Ryabov et al. (2022) - DOI: 10.1073/pnas.2118156119-, and expanded by Carrasco et al. (2023) - DOI:10.3389/fevo.2023.1285115. The file entitled "Fd_NS" gathers functional diversity variation per year according to ICES SubArea.

In the RScript, "Fish_FunctDiv.R", I anlyze trends of functional diversity in the time series in both the teporal scale, and the spatial scale, using the ICES areas and ICES Subareas.
