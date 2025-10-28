Grid r = read "R.csv";

Int rows = numRows r;

Grid rr = repeatRows (Int row = 0, rows) repeatRows (Int row2 = 0, rows) repeatRows (Int row3 = 0, rows)
if (((getStr r row 1) == (getStr r row2 0)) && ((getStr r row2 1) == (getStr r row3 0))) then (addCols (getCell r row 0) (getCell r row3 1)) else ([[]]);

output = rr;



