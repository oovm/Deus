## Sudoku17
### Source

- http://staffhome.ecm.uwa.edu.au/~00013890/sudokumin.php

### Generation
```Mathematica
imp=Import["C:\\Users\\Aster\\Downloads\\Sudoku17.txt","Text"]
raw=StringPartition[StringDelete[imp,{"\n","\r"}],81]
Export["Sudoku17.wxf",Sort@raw,PerformanceGoal->"Size"]
```



## SudokuMega
### Source

- https://www.kaggle.com/bryanpark/sudoku/home

### Generation
```Mathematica
raw=Import["C:\\Users\\Aster\\Downloads\\sudoku.csv\\SudokuMega.csv", {"CSV", "RawData"}];
Export["SudokuMega.wxf",Sort@First@Transpose[Rest@raw],PerformanceGoal->"Size"]
```
