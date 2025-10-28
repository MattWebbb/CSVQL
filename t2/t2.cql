Grid a = read "A.csv";

Grid b = repeatRows (Int row = 0, numRows a) if (getStr a row 0 == getStr a row 1) then (addCols (getCell a row 2) (getCell a row 0)) else ([[]]);

output = b;