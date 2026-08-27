# Data folder overview

* /data contains the raw data folder, **knb-lter-luq.20.4923064**, from McDowell and International Institute Of Tropical Forestry (IITF) (2024)

The csv files used to recreate Figure 3 from **/data/knb-lter-luq.20.4923064** include:

1. QuebradaCuenca1-Bisley.csv
2. QuebradaCuenca2-Bisley.csv
3. QuebradaCuenca3-Bisley.csv
4. RioMameyesPuenteRoto.csv

Each of the target csv files have the same structure. The columns needed to recreate figure 3 are:

1. **Sample_ID**   Site name (i.e., Q1, Q2, Q3, MPR)
2. **Sample_Date**   Sampling began on 1986-05-20 and ended on 2020-12-29
3. **NO3-N**   Concentration of nitrate in ug/L
4. **K**   Concentration of potassium in mg/L
5. **Mg**   Concentration of magnesium in mg/L
6. **Ca**   Concentration of calcium in mg/L
7. **NH4-N**   Concentration of ammonium in ug/L

* **1_clean_data.R** includes the code for cleaning the raw data, creating **fig3_long.csv** in /output, which is ready for analysis