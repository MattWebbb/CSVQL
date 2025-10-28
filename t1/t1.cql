Grid a = read "a.csv";
Grid b = read "b.csv";

Grid c = repeatRows (Int row = 0, numRows a) (repeatRows (Int row2 = 0, numRows b) addCols (getRow a row) (getRow b row2));

output = c;