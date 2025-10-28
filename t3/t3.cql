Grid a = read "A.csv";

Grid b = repeatRows (Int row = 0, (numRows a)) if ((getStr a row 1) == "") then ([[]]) else (getRow a row);

output = b;