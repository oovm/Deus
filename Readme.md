# Deus — Nogame Nolife

[![Build Status](https://travis-ci.org/GalAster/Deus.svg?branch=master)](https://travis-ci.org/GalAster/Deus)
[![Mathematica](https://img.shields.io/badge/Mathematica-%3E%3D10.0-brightgreen.svg)](https://www.wolfram.com/mathematica/)
[![Release Vision](https://img.shields.io/badge/release-v0.3.2-ff69b4.svg)](https://github.com/GalAster/Deus/releases)
[![Repo Size](https://img.shields.io/github/repo-size/badges/shields.svg)](https://github.com/GalAster/Deus)

![Background](https://raw.githubusercontent.com/GalAster/Deus/master/Resources/pic/MainPage.jpg)

## ![项目简介](https://raw.githubusercontent.com/GalAster/Deus/master/Resources/ico/board-game-blocks.png) Introduce



## ![安装方式](https://raw.githubusercontent.com/GalAster/Deus/master/Resources/ico/board-game-map.png) Install

#### 手动安装

- 点击release下载最新的安装包

  - release 版本不带说明文档

- 解压到任意`$Path`路径下

  - 如果选择手动安装那么升级也要手动升级

#### 自动安装

- 使用``BTools` ``的部署功能

## ![意见建议](https://raw.githubusercontent.com/GalAster/Deus/master/Resources/ico/board-game-box.png) Show Time!

### 三维幻方

### 汉诺塔动画

### 数织二维码

<img src="https://raw.githubusercontent.com/GalAster/Deus/master/Resources/pic/Nonograms.png" alt="CC协议" width = "400" align=center />



- 演示代码

```Mathematica
Needs["Deus`"]
In[1]:= MagicShow[Magic[3,3],3]
In[2]:=
In[3]:= NonogramGenerate["https://github.com/GalAster/Deus",Method->"QR"]
In[4]:=
```
## ![计划项目](https://raw.githubusercontent.com/GalAster/Deus/master/Resources/ico/battleship.png) Todo List

- [x] 汉诺塔
  - [x] 三柱汉诺塔任意解
  - [x] 多柱汉诺塔一般解
  - [ ] 多柱汉诺塔任意解
- [x] 幻方
  - [x] 任意维幻方
  - [ ] 特种幻方
  - [ ] 幻方判定
- [x] 数织
  - [x] 二维码数织
  - [ ] 非方阵数织
  - [ ] 多色数织
- [ ] DigitalMinato
  - [ ] 算法优化
- [ ] 猜拳
- [ ] n 皇后
- [ ] 约瑟夫游戏


## ![更新日志](https://raw.githubusercontent.com/GalAster/Deus/master/Resources/ico/hourglass.png) Change Log

| 版本号 |更新记录 |
|:-----:| --- 
| 0.4.0 | 加入并持续更新24点中!
| 0.3.2 | 加入数织包
| 0.2.1 | 移植幻方包
| 0.1.0 | 移植汉诺塔包
| Older | [查看完整记录](https://github.com/GalAster/Deus/blob/master/Resources/Change%20Log%20Full.md)

## ![意见建议](https://raw.githubusercontent.com/GalAster/Deus/master/Resources/ico/board-games-with-roles.png) Ideas

### 联系方式


|知乎主页|  2|
|:-:|:-:|
| [<img src="https://raw.githubusercontent.com/GalAster/Deus/master/Resources/pic/Logo_Zhihu.png" alt="知乎链接" width = "100" align=center />](https://www.zhihu.com/people/GalAster) | 2 |


## ![许可协议](https://raw.githubusercontent.com/GalAster/Deus/master/Resources/ico/board-gaming.png) License

该软件包遵从CC 4.0协议: <img src="https://raw.githubusercontent.com/GalAster/Deus/master/Resources/ico/CC40_BY+NC+SA.png" alt="CC协议" align=center />

**BY-NC-SA** (非商业性使用、相同方式共享）