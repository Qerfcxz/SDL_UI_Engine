# Hsue - A Haskell UI Engine for Text-Based Roguelikes

> **Note:** This is a work-in-progress engine developed in one month by a senior undergraduate student. It is built from scratch using Haskell and SDL2 Raw Bindings, aiming to power a future text-based turn-based Roguelike game.

## 📖 Introduction

**Hsue** is an experimental yet hardcore UI engine written in **Haskell**. Unlike mainstream UI frameworks that rely on callbacks or MVVM, this engine adopts a unique **State-Machine Event Flow** and a **Pull-based** architecture.

It was born out of dissatisfaction with existing engines handling complex text rendering and state management in Roguelike games. It prioritizes control, precision, and text clarity over ease of use, making it a specialized tool for specific needs.

## ✨ Key Features

### 1. Finite State Machine Event Flow
The engine treats UI navigation as a state machine. It provides explicit instructions like `Goto` (jump to a widget ID) and `Back` (return to previous state) to control the event flow.
- **Why?** This is a game-changer for turn-based games with deep nested menus (e.g., Inventory -> Item -> Details -> Back), eliminating the need for manual stack management.

### 2. True Adaptive Rendering (No Scaling Artifacts)
Unlike engines that simply scale textures when the window resizes (causing blurriness), this engine recalculates layout and **re-rasterizes fonts** at the target resolution.
- **Result:** Text remains crisp and pixel-perfect at any window size or DPI, which is crucial for text-heavy games.

### 3. Functional Instruction System
Communication between parent and child widgets is handled via **Instructions**, **Event Transforms**, and **Request Transforms**.
- Parents can intercept and modify events before they reach children.
- Children send abstract requests that parents can transform (e.g., for implementing scroll views or coordinate mapping).

### 4. Dynamic Focus Strategy
There is no static "Main Widget". The engine determines which widget receives input (focus) via a pure function evaluated every frame based on the current engine state. This allows for highly dynamic and programmable focus logic.

### 5. Production-Grade Editor Widget
A built-in, fully featured text editor component that supports:
- **50+ Keyboard Shortcuts** (Navigation, Selection, Modification).
- **Clipboard Integration** (System Copy/Paste via SDL2).
- **Advanced Selection** (Dual-cursor range selection, paragraph selection).
- **Block-based Rendering** for high performance.

### 6. Decoupled Data & State
The **Data Widget Family** (`Bool_data`, `Int_data`, `List_char_data`, etc.) allows local state to be stored within the engine's global map but accessed via IDs. This effectively decouples game data from UI presentation, facilitating Save/Load functionality.

### 7. Deferred Rendering via Collectors
The `Collector` widget enables hierarchical rendering control. It collects render requests and allows the engine to sort or batch them (Z-ordering) before execution, solving complex layering issues.

### 8. "Pull" over "Push" Philosophy
Instead of the engine pushing context down to every widget, components are designed to **fetch (pull)** resources (renderers, data, configs) using IDs when needed. This reduces parameter passing overhead and decouples dependencies.

### 9. Infinite Nesting
Widgets are referenced by IDs stored in `IntMap`s, not by direct recursive data types. This allows for virtually infinite nesting depths without the typical performance penalties or type complexities found in some functional UI definitions.

## 🛠 Technical Stack

- **Language:** Haskell (GHC)
- **Backend:** SDL2 (via `SDL.Raw` bindings for maximum control)
- **Data Structures:** Heavily utilizes `Data.Sequence` (Finger Trees) for efficient text/event handling and `IntMap` for component storage.

## 🚀 Future Roadmap

- [ ] Implement a Layout Engine (Flexbox/Grid-like) to replace absolute positioning.
- [ ] Add a Markdown/XML parser for rich text styling in the `Text` widget.
- [ ] Integrate `binary` or `cereal` for full UI state serialization (Save/Load system).
- [ ] Develop the actual Roguelike game using this engine.

## 📝 Disclaimer

This project is currently in the **Alpha / Prototype** stage. The API is subject to radical changes. It serves as a proof-of-concept for building high-performance, low-level UI systems in Haskell without relying on high-level FRP libraries.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# Hsue - 专为文字类 Roguelike 打造的 Haskell UI 引擎

> **说明:** 这是一个由大四学生在单月内独立开发的原型引擎。它基于 Haskell 和 SDL2 Raw Bindings 从零构建，旨在支撑未来一款文字类回合制 Roguelike 游戏的开发。

## 📖 简介

**Hsue** 是一个实验性但硬核的纯 Haskell UI 引擎。与依赖回调或 MVVM 的主流框架不同，本引擎采用独特的 **状态机式事件流** 和 **“自取式”（Pull-based）** 架构。

该引擎的诞生源于对现有引擎处理复杂文本渲染和回合制状态管理的不满。相比易用性，它更优先考虑**控制力、渲染精度和逻辑解耦**，是为特定游戏类型量身定制的利器。

## ✨ 核心特性

### 1. 状态机式事件流转机制
引擎将 UI 导航视为状态机。提供了 `Goto`（跳转到控件ID）和 `Back`（回溯历史状态）等显式指令来控制事件流。
- **优势**: 完美契合回合制游戏中深层嵌套的菜单逻辑（如：主界面 -> 物品栏 -> 选中物品 -> 详情 -> 返回），无需开发者手动维护状态栈。

### 2. 真·自适应渲染（拒绝模糊）
当窗口大小改变时，引擎不仅仅是拉伸纹理，而是根据设计分辨率与实际分辨率的比例，重新计算布局并**重新光栅化字体**。
- **效果**: 无论窗口如何缩放，文字始终保持像素级清晰（Pixel-perfect），这对文字量巨大的游戏至关重要。

### 3. 函数式指令系统
父子控件之间通过 **Instruction（指令）**、**Event Transform** 和 **Request Transform** 进行通信。
- 父控件可在事件到达子控件前进行拦截或坐标变换。
- 子控件发出抽象请求，父控件可对其进行转换（例如实现滚动视图或相对坐标映射）。

### 4. 动态主控件逻辑
没有静态的“焦点控件”。引擎每帧通过一个基于当前状态的纯函数来动态决定哪个控件 ID 接收输入。这使得焦点切换逻辑高度可编程。

### 5. 生产级文本编辑器 (Editor)
内置功能完备的文本编辑组件，支持：
- **50+ 快捷键**（涵盖光标移动、选择、编辑）。
- **剪贴板集成**（通过 SDL2 原生支持复制/粘贴）。
- **高级选择功能**（双光标范围选择、段落选择）。
- **基于 Block 的渲染**，确保高性能。

### 6. 数据与状态解耦
通过 **Data 控件家族**（`Bool_data`, `Int_data` 等），组件的局部状态被存储在引擎的全局 Map 中，但通过 ID 进行访问。这有效地将游戏数据与 UI 表现分离，天然支持存读档（Save/Load）。

### 7. 基于 Collector 的层级渲染
`Collector` 控件支持延迟和分批处理渲染请求。它允许引擎对请求进行排序或批处理（Z-order），从而轻松解决复杂的 UI 遮挡和层级问题。

### 8. “自取” (Pull) 设计哲学
引擎不负责将所有上下文“推”给每个控件，而是让控件在需要时通过 ID 向引擎 **“索取” (Fetch/Pull)** 资源（如渲染器、数据、配置）。这种设计极大减少了函数参数传递的负担，降低了耦合度。

### 9. 无限嵌套能力
控件通过 `IntMap` 中的 ID 相互引用，而非直接的递归数据类型。这使得 UI 树可以几乎无限嵌套，且避免了某些函数式数据结构更新时的性能陷阱。

## 🛠 技术栈

- **语言:** Haskell (GHC)
- **后端:** SDL2 (使用 `SDL.Raw` 绑定以获得最大控制权)
- **核心数据结构:** 大量使用 `Data.Sequence` (Finger Trees) 处理文本序列，使用 `IntMap` 管理组件索引。

## 🚀以此为基础的计划

- [ ] 实现简易布局引擎 (类似 Flexbox/Grid)，替代绝对坐标定位。
- [ ] 为 `Text` 控件添加 Markdown/XML 解析器以支持富文本。
- [ ] 引入 `binary` 或 `cereal` 库实现全 UI 状态序列化 (存读档系统)。
- [ ] 正式开发基于此引擎的 Roguelike 游戏。

## 📝 免责声明

本项目目前处于 **Alpha / 原型** 阶段。API 可能会发生剧烈变化。它更多是作为一个概念验证（Proof-of-Concept），展示如何在不依赖高级 FRP 库的情况下，用 Haskell 构建高性能、底层的 UI 系统。
