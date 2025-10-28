Grid p = read "P.csv";
Grid q = read "Q.csv";

Grid a = repeatRows(Int row = 0, numRows p) repeatRows(Int row2 = 0, numRows q)
if ((getStr p row 3) == (getStr q row2 3)) 
then (addCols (repeatCols(Int col = 0, 3) if ((getStr q row2 col) == "") then (getCell p row col) else (getCell q row2 col)) (getCell q row2 3))
else ([[]]);

output = a;


