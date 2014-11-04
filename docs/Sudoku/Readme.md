# Functions

Sudoku 模块


::: tip Rule
约定 n 阶数独是一个 n^2×n^2 的矩阵
正数表示原本就存在的值, 也就是谜题
0 表示未知的值, 需要求解的部分
负数表示用户填入的部分, 或者求解函数返回的部分
其他非整数非数字都按填 0 处理
:::


- 模块未完成

- 模块可单独加载

```haskell
Import["https://deus.netlify.com/pkg/Sudoku.wl"];
```

## Magic

### 参数说明

::: tip Sudoku[n,dim:2]
- n 类型为 `Integer`, 表示幻方的阶
:::

### 可选项

待定, 支持其他种类幻方

### 返回值

`MagicObject`

### 标准示例

#### 测试代码:
```haskell
Import[]
```

#### 测试输出:


