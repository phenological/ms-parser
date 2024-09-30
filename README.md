# ms-parser
parsers for MS data


## sampleType:
|Name|Assay|Meaning|Notes|
|:--:|:--:|:--:|:--:|
|Check|BA, SCFA| Matrix-free solution of the standard (e.g., 17 BA) set to a certain concentration (e.g., 50 nM). This is used to check the system performance. Remove from any analysis||
|Condition|BA|LTRs injected at the beginning of the run to stabilise the column before the study sample begins. The value is often not stable. Remove from Analysis and also remove from LTR check||
|SB|SCFA|Single blank (blank that has undergone sample preparation)||
|Blank||||
|Calibration||prepared from parent liquid solution of each assay metabolites||
|LTR||||
|SLTR||||
|VLTR||||
|QC||Quality check solution (prepared from parent liquid solution of each assay metabolites)||
|PQC||Pooled quality check sample (pooled from samples taken from unhealthy volunteers)||

## Assays:

### Amino Acids

### Tryptophan

### Bile Acids
|Formula|Analyte|Abbrev|MW|
|:--:|:--:|:--:|:--:|
|C24H36D4O4|		|CDCA|	396.6|
|C24H36D4O5|	Cholic acid (2,2,4,4-D4, 98%)| 	CA	|412.6|
|C24D4H36O4|	Deoxycholic acid (2,2,4,4-D4, 98%)| 	DCA|	396.6|
|C24H36D4O3|	Lithocholic acid (2,2,4,4-D4, 98%)| 	LCA	|380.6|
|C24H35D5O5|	β-Muricholic acid (2,2,3,4,4-D5, 99%)| 	b-MCA	|413.6|
|C24H36D4O4|	Ursodeoxycholic acid (2,2,4,4-D4, 98%) CP 95%| 	UDCA|	396.6|
|C26H39D4NO5|	Glycochenodeoxycholic acid (2,2,4,4-D4, 98%) CP 97%| 	GCDCA	|453.65|
|C26H39D4NO6|	Glycocholic acid (2,2,4,4-D4, 98%) CP 96% 	|GCA	|469.65|
|C26H39D4NO5|	Glycodeoxycholic acid (2,2,4,4-D4, 98%) 	|GDCA|	453.65|
|C26H39D4NO4|	Glycolithocholic acid (2,2,4,4-D4, 98%) 	|GLCA|	437.65|
|C26H39D4NO5|	Glycoursodeoxycholic acid (2,2,4,4-D4, 98%) CP 97% 	|GUDCA	|453.65|
|C26H40D4NNaO6S|	Taurochenodeoxycholic acid, sodium salt (2,2,4,4-D4, 98%) CP 97% 	|TCDCA|	525.71|
|C26H40D4NaNO7S|	Taurocholic acid, sodium salt (2,2,4,4-D4, 98%) 	|TCA|	541.71|
|C26D4H40NNaO6S|	Taurodeoxycholic acid, sodium salt (2,2,4,4-D4, 98%) 	|TDCA|	525.71|
|C26H40D4NNaO5S|	Taurolithocholic acid, sodium salt (2,2,4,4-D4, 98%) 	|TLCA	|509.71|
|C26H40D4NaNO6S|	Tauroursodeoxycholic acid, sodium salt (2,2,4,4-D4, 98%) |	TUDCA	|525.71|

### SCFA

|Analyte|Standard|MW|Notes|
|:--:|:--:|:--:|:--:|
|Acetic acid|Acetic acid-d4 (1)|||
|Propionic acid (1)|Propionic acid 13C3 (1)|||
|Isobutyric acid (1)|Isobutyric acid-d7|||
|Butyric acid (1)|Butyric acid-d7 (1)|||
|2Methylbutyric acid (1)||||
|Isovaleric acid|Isovaleric acid-d9 (1)|||
|Valeric acid (1)||||
|3Methylvaleric acid (1)||||
|Hexanoic acid (1)|Hexanoic acid-d11 (1)|||

### Lipids

## plate Layout

### 48 glass vials rack layout:
||1|2|3|4|5|6|7|8|
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
|A|Sample|Sample|Sample|Sample|Sample|Sample|SB|QC2|
|B|Sample|Sample|Sample|Sample|Sample|Sample|Cal5|QC1|
|C|Sample|Sample|Sample|Sample|Sample|Sample|Cal4|PQC1|
|D|Sample|Sample|Sample|Sample|Sample|Sample|Cal3|PQC2|
|E|Sample|Sample|Sample|Sample|Sample|Sample|Cal2|PQC3|
|F|Sample|Sample|Sample|Sample|Sample|Sample|Cal1|PQC4|

### 96 well plate layout:
||1|2|3|4|5|6|7|8|9|10|11|12|
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
|A|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Calibration|QC1|
|B|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Calibration|QC2|
|C|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Calibration|QC3|
|D|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Calibration|QC4|
|E|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Calibration|LTR|
|F|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Calibration|LTR|
|G|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Calibration|LTR|
|H|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Sample|Calibration|LTR|

