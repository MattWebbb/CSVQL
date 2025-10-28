Grid p = read "P.csv";

Grid q = read "Q.csv";

Grid a = repeatRows(Int row = 0, numRows p) repeatRows(Int row2 = 0, numRows q) 
if ((getStr p row 0) == (getStr q row2 0)) 
then (addCols (getCell p row 0) (repeatCols(Int col = 1, 4) if ((getStr p row col) == "") then (getCell q row2 col) else (getCell p row col)))
else ([[]]);

output = a;