Grid s = read "S.csv";
Grid t = read "T.csv";

Grid a = repeatRows (Int row = 0, numRows s) repeatRows (Int row2 = 0, numRows t)
if (((getStr s row 0) == (getStr s row 1)) && ((getStr t row2 1) == (getStr t row2 2)))
then (addCols (getCell s row 2) (getCell t row2 0))
else ([[]]);

output = a;


