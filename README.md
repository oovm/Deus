# Deus — Nogame Nolife

[![Build Status](https://travis-ci.org/GalAster/Deus.svg?branch=master)](https://travis-ci.org/GalAster/Deus)
[![Mathematica](https://img.shields.io/badge/Mathematica-%3E%3D10.0-brightgreen.svg)](https://www.wolfram.com/mathematica/)
[![Release Vision](https://img.shields.io/badge/release-v0.5.0-ff69b4.svg)](https://github.com/GalAster/Deus/releases)
[![Repo Size](https://img.shields.io/github/repo-size/GalAster/Deus.svg)](https://github.com/GalAster/Deus.git)

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

<div align=center>
<img src="https://raw.githubusercontent.com/GalAster/Deus/master/Resources/pic/Magic3D.gif" alt="3D幻方"/>
</div>

### 汉诺塔动画

<div align=center>
<img src="https://raw.githubusercontent.com/GalAster/Deus/master/Resources/pic/HanoiTower.gif" alt="3D幻方"/>
</div>

### 数织二维码

<div align=center>
<img src="https://raw.githubusercontent.com/GalAster/Deus/master/Resources/pic/Nonograms.png" alt="数织二维码" width = "400"/>
</div>

### 珠玑妙算

<div align=center>
<img src="https://raw.githubusercontent.com/GalAster/Deus/master/Resources/pic/DigitMaster.png" alt="数织二维码"/>
</div>

- 演示代码

```Mathematica
Needs["Deus`"]
In[1]:= MagicShow[Magic[3,3],3]
In[2]:= HanoiShow@HanoiMove[10, Pillar -> 4] // ListAnimate
In[3]:= NonogramGenerate["https://github.com/GalAster/Deus",Method->"QR"]
In[4]:= "如图"
```

- [查看完整演示](https://github.com/GalAster/Deus/blob/master/Resources/Examples%20Full.md)

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
- [ ] 珠玑妙算
  - [ ] 24点
  - [ ] 算法优化
- [ ] 猜拳
- [ ] n 皇后
- [ ] 约瑟夫游戏

- [查看完整计划](https://github.com/GalAster/Deus/blob/master/Resources/Todo%20List%20Full.md)


## ![更新日志](https://raw.githubusercontent.com/GalAster/Deus/master/Resources/ico/hourglass.png) Change Log

| 版本号 |最近更新|
|:-----:| --- 
| 0.5.0 | Bugfix Ver
| 0.4.3 | 增加1926数字验证
| 0.4.2 | 增加凑100游戏
| 0.4.1 | 优化24点算法
| Older | [查看完整记录](https://github.com/GalAster/Deus/blob/master/Resources/Change%20Log%20Full.md)

## ![意见建议](https://raw.githubusercontent.com/GalAster/Deus/master/Resources/ico/board-games-with-roles.png) Ideas

### 联系方式


|知乎主页|QQ群聊| 
|:-:|:-:|
|[<img src="https://raw.githubusercontent.com/GalAster/Deus/master/Resources/pic/Logo_Zhihu.png" alt="知乎链接" width = "100" align=center />](https://www.zhihu.com/people/GalAster)|[<img src="https://raw.githubusercontent.com/GalAster/Deus/master/Resources/pic/Logo_QQ.png" alt="QQ链接" width = "100" align=center />](https://jq.qq.com/?_wv=1027&k=5BqFya1)





©Copyright by Mozilla Public License Version 2.0