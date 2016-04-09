# Functions

NumberMaster 模块包含了一系列经典的数字谜题.

- 模块可单独加载

```haskell
Import["https://deus.netlify.com/pkg/NumberMaster.wl"];
```

## Poker24
### 参数说明

::: tip Poker24[pks]
- pks 类型为 `List`, 表示需要计算24点的卡牌
:::

### 可选项

- Number->24
	- 需要凑出的点数, 默认24点
- Extension -> Off
	- `Off`: 标准的四则运算
	- `Min`: 允许使用指数,对数,开根凑配
	- `Max`: 额外允许使用排列组合
	- 自定义**二元算符**
- FindInstance -> Off
	- 是否只返回找到的第一个解
	- 该功能未完成

### 返回值

经典谜题, 算24点

四则运算的情况下是高速且不重不漏的

::: warning 不重不漏
重复被定义为在因式分解算子下同构
比如 (-a)×(-b) 被认为和 a×b 同构
:::


### 标准示例

#### 测试代码:
```haskell
Import[]
```

#### 测试输出:
`{10, 8, 7, 3}` 在四则运算下无解
允许使用开方后有唯一解 $10+\sqrt[3]{8} 7$
允许使用排列组合有 x 个解

## Calculate100
### 参数说明

::: tip Calculate100[input]
- input 类型为 `List`, 表示需要凑配的数字列表
:::

::: tip Calculate100[input,target]
- input 类型为 `List`, 表示需要凑配的数字列表
- target 类型为 `Integer`, 表示需要凑出的数字
:::

### 返回值

经典谜题,9个数字添加符号计算100

::: warning 提取计算
完整计算时间极长, 可以直接 `Abort` 提取当前搜到的结果.
:::

`List`, 包含所有的解.

### 标准示例

#### 测试代码:
```haskell
Import[]
```

#### 测试输出:



## Proof1926
### 参数说明

::: tip Proof1926[num1,num2]
- num1 类型为 `Integer`, 表示需要凑配的数字之一
- num2 类型为 `Integer`, 表示需要凑配的数字之一
:::

### 可选项

- Number->2018
	- 指定两个数字都要凑配到这个数字
- TimeConstrained->1
	- 凑配允许的时间上限

### 返回值

经典谜题, 某两个人物生日的数字论证

函数返回第一个结果.

### 标准示例

#### 测试代码:
```haskell
Import[]
```

#### 测试输出:
