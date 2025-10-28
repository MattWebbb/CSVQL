Grid p = read "P.csv";
Grid q = read "Q.csv";
Grid r = read "R.csv";
Grid s = read "S.csv";
Grid t = read "T.csv";

Grid pqrst = repeatRows (Int row = 0, numRows p) (repeatRows (Int row2 = 0, numRows q) (repeatRows (Int row3 = 0, numRows r) (repeatRows (Int row4 = 0, numRows s) (repeatRows (Int row5 = 0, numRows t) 
addCols (getRow p row) addCols (getRow q row2) addCols (getRow r row3) addCols (getRow s row4) (getRow t row5)))));

output = pqrst;