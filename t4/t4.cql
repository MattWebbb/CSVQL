Grid a = read "A.csv";

Grid b = repeatRows (Int row = 0, numRows a) [["foo"]];

Grid c = addCols a b;

Grid d = addCols c a;

output = d;