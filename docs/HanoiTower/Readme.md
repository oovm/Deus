# Functions

HanoiTower 模块

- 模块可单独加载

```haskell
Import["https://deus.netlify.com/pkg/HanoiTower.wl"];
```

## HanoiGraph
### 参数说明

::: tip HanoiGraph[n]
- n 类型为 `Integer`, 汉诺塔的阶数
:::

### 可选项



### 返回值

给出n阶汉诺图

### 标准示例

**测试代码:**
```haskell
Import[]
```

**测试输出:**




## HanoiSteps
### 参数说明

::: tip HanoiSteps[n,p]
- n 类型为 `Integer`, 汉诺塔的层数
- p 类型为 `Integer`, 汉诺塔的柱数
:::

### 可选项



### 返回值

给出汉诺塔的最少移动步骤, n>4 的情况尚未证明.

### 标准示例

**测试代码:**
```haskell
Import[]
```

**测试输出:**

## HanoiMove
### 参数说明

::: tip HanoiMove[n,p]
- n 类型为 `Integer`, 汉诺塔的层数
- p 类型为 `Integer`, 汉诺塔的柱数
:::

### 可选项



### 返回值

给出汉诺塔状态间的最优移动方式

### 标准示例

**测试代码:**
```haskell
Import[]
```

**测试输出:**

## HanoiShow
### 参数说明

::: tip HanoiShow[states_]
- states 类型为
:::

### 可选项



### 返回值

可视化圆盘的移动过程

### 标准示例

**测试代码:**
```haskell
Import[]
```

**测试输出:**


