Grid f = read "F.csv";
Grid g = read "G.csv";

Grid a = repeatRows(Int row = 0, numRows f) repeatRows(Int row2 = 0, numRows g)
if ((if ((getStr f row 1) == "") then (false) else (true)) && (if ((getStr f row 2) == "") then (false) else (true)) && ((getStr f row 1) == (getStr g row2 0)) && ((getStr f row 2) == (getStr g row2 1)))
then (addCols (getCell f row 0) (getCell g row2 2))
else ([[]]);

output = a;


