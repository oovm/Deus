# Functions

NumberMaster 模块包含了一系列经典的数字谜题.

- 模块可单独加载

```haskell
Import["https://deus.netlify.com/pkg/NumberMaster.wl"];
```

## Poker24

经典谜题, 24点

### 参数说明

::: tip Poker24[pks]
- pks 类型为 `List`, 表示需要计算24点的卡牌
:::

### 可选项

- Number->24
	- 需要凑出的点数, 默认24点
- Extension -> Off
	- `Off`: 标准的四则运算
	- `Min`: 允许使用阶乘,对数,开根凑配
	- `All`: 允许使用所有的二元运算凑配
- FindInstance -> Off
	- 是否只返回找到的第一个解
	- 该功能未完成

### 返回值

`List`, 包含所有的解.

四则运算的情况下是高速且不重不漏的

::: warning 不重不漏
重复被定义为在因式分解算子下同构
:::


### 标准示例

#### 测试代码:
```haskell
Import[]
```

#### 测试输出:


## Calculate100

经典谜题,9个数字添加符号计算100

### 参数说明

::: tip Calculate100[input]
- input 类型为 `List`, 表示需要凑配的数字列表
:::

::: tip Calculate100[input,target]
- input 类型为 `List`, 表示需要凑配的数字列表
- target 类型为 `Integer`, 表示需要凑出的数字
:::

### 返回值

`List`, 包含所有的解.

### 标准示例

#### 测试代码:
```haskell
Import[]
```

#### 测试输出:

## Proof1926

经典谜题, 某两个人物生日的数字论证

### 参数说明

::: tip Proof1926[num1,num2]
- num1 类型为 `Integer`, 表示需要凑配的数字之一
- num2 类型为 `Integer`, 表示需要凑配的数字之一
:::

### 可选项

- Number->2018
	- 指定两个数字都要凑配到这个数字

### 返回值

`List`, 包含所有的解.

### 标准示例

#### 测试代码:
```haskell
Import[]
```

#### 测试输出:
