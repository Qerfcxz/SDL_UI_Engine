# Hsue - A Haskell UI Engine for Text-Based Roguelikes

> 🚨 **Notice:** This repository has been refactored! The updated and active version of the project is now available at **[github.com/Qerfcxz/Hsue](https://github.com/Qerfcxz/Hsue)**. Please visit the new repository for the latest code and updates.
>
> **Note:** The main content of this engine was completed within one month by a senior undergraduate student. It is built entirely from scratch using Haskell and SDL2 Raw Bindings, designed specifically to power a future text-heavy, turn-based Roguelike game.

## 📖 Introduction

**Hsue** is an experimental yet hardcore UI engine written in **Haskell**. Diverging from mainstream UI frameworks that rely on callbacks, DOM trees, or MVVM, Hsue adopts a unique **State-Machine Event Flow**, **Sequence-Based Path Addressing**, and a **Pull/Request-Driven** architecture.

It was born out of dissatisfaction with existing engines when handling complex text rendering, deep menu navigation, and pure state management. It prioritizes absolute control, performance optimization (especially for text), and logical decoupling over immediate ease of use.

## ✨ Key Features

### 1. Finite State Machine Event Flow
The engine treats UI navigation as a state machine. Event handlers return explicit control IDs like `Goto` (jump to a specific widget flow) and `Back` (return to the previous state).
- **Why?** This perfectly handles deep, nested turn-based menus (e.g., Main Menu -> Inventory -> Item Details -> Back), eliminating manual state stack management.

### 2. Sequence-Based Path Addressing (`DS.Seq Int`)
Widgets are not directly nested in recursive data types. Instead, they are flattened into `IntMap`s and accessed via a strict path sequence (`Data.Sequence Int`). 
- **Result:** Infinite nesting depth without the typical performance penalties of functional tree updates. It enables O(log N) lookup and highly decoupled parent-child relationships.

### 3. Pure Functional UI Coroutines
Built-in support for a pure `Coroutine` widget (`Wait`, `Loop`, `Then`, `Fork`, `Emit`). 
- **Result:** You can program complex UI animations, sequenced events, or timed delays asynchronously without touching Haskell's `IO` or dealing with thread synchronization.

### 4. Instruction & Request-Driven Architecture
State mutation is entirely deferred. Widgets emit `Request`s (e.g., Create, Alter, Replace) combined with `Instruction`s (spatial transforms like `Move`, `Scale`, `Angle`, `Flip`).
- Parents can intercept, transform, or block requests and events from children. This makes features like Scroll Views or dynamic scaling completely math-driven and side-effect free.

### 5. Production-Grade Text & Editor Engine
The engine houses an incredibly complex and optimized text system:
- **Block-Based Font Caching:** Uses `IntMap` to cache SDL Textures at the character/block level, recalculating and re-rasterizing dynamically based on the window's adaptive scale.
- **Advanced Editor:** Supports 50+ programmable keybindings (`Editor_binding`), dual-cursor text selection, paragraph navigation, and native OS Clipboard integration (Copy/Paste).
- **Typesetting:** Native support for Left, Center, and Right text alignment with precise font-metric calculations (Ascent/Descent).

### 6. True Adaptive Rendering & Canvas (Off-screen)
- **Adaptive Window:** Automatically calculates the Greatest Common Divisor (GCD) to map design resolutions to actual window sizes. No blurry upscaling; fonts and shapes re-render precisely.
- **Canvas System:** Supports switching SDL Render Targets (`SDL_TEXTUREACCESS_TARGET`) via the `Canvas` widget, allowing complex off-screen compositing before presenting to the main window.

### 7. Decoupled Data Widgets
The **Data Widget Family** (`Bool_data`, `Int_data`, `List_char_data`, custom `Data a`) stores localized game state directly inside the UI map. Game logic can "pull" this data via IDs, perfectly decoupling the data layer from the presentation layer (ideal for Save/Load mechanics).

## 🛠 Technical Stack

- **Language:** Haskell (GHC)
- **Backend:** SDL2 (via `SDL.Raw` bindings for low-level pointer and memory control)
- **Data Structures:** Heavily utilizes `Data.Sequence` (Finger Trees) for paths/text and `Data.IntMap.Strict` for high-performance widget storage.

## 🚀 Future Roadmap

- [ ] Implement a Layout Engine (Flexbox/Grid-like) to replace absolute coordinate positioning.
- [ ] Add a Markdown/XML parser for rich text styling in the `Text` widget.
- [ ] Integrate `binary` or `cereal` for full UI state serialization (Save/Load system).
- [ ] Develop the actual Roguelike game utilizing this engine.

---

# Hsue - 专为文字类 Roguelike 打造的 Haskell UI 引擎

> 🚨 **重构公告：** 本项目已进行全面重构！全新的重构版本请移步至新项目仓库：**[github.com/Qerfcxz/Hsue](https://github.com/Qerfcxz/Hsue)**。建议前往新仓库查看最新的开发进度与代码。
>
> **说明:** 本引擎的主要内容由一名大四学生在一个月内独立开发完成。它基于 Haskell 和 SDL2 Raw Bindings 从零构建，旨在支撑未来一款文字量巨大、逻辑复杂的回合制 Roguelike 游戏的开发。

## 📖 简介

**Hsue** 是一个实验性但硬核的纯 Haskell UI 引擎。与依赖回调、DOM树或 MVVM 的主流框架不同，Hsue 采用了独特的 **状态机式事件流**、**基于序列的路径寻址** 以及 **请求驱动 (Request-Driven)** 架构。

该引擎的诞生源于对现有引擎在处理复杂文本光栅化、深层菜单路由和纯函数状态管理时的局限性的不满。相比开箱即用，它更追求对底层的绝对控制力、极限的文本渲染性能以及业务逻辑的彻底解耦。

## ✨ 核心特性

### 1. 状态机式事件流转
引擎将 UI 导航视为状态机。事件处理函数会返回显式的控制 ID，如 `Goto`（跳转到指定控件流）和 `Back`（回溯历史状态）。
- **优势**: 完美契合回合制游戏中深层嵌套的菜单逻辑（如：主界面 -> 物品栏 -> 选中物品 -> 详情 -> 返回），彻底免去开发者手动维护状态栈的痛苦。

### 2. 基于序列的路径寻址 (`DS.Seq Int`)
控件并非通过传统的递归数据类型直接嵌套。相反，它们被扁平化存储在 `IntMap` 中，并通过严格的路径序列 (`Data.Sequence Int`) 进行精确定位。
- **优势**: 允许几乎无限的 UI 嵌套深度，且避免了函数式树状结构在深层更新时的性能损耗。父子控件完全解耦，支持 O(log N) 级别的高效寻址。

### 3. 纯函数式 UI 协程 (Coroutine)
引擎原生提供了一个纯函数式的 `Coroutine` 控件（支持 `Wait`, `Loop`, `Then`, `Fork`, `Emit`）。
- **优势**: 开发者可以异步编写复杂的 UI 动画、序列事件或延时逻辑，而完全不需要触碰 Haskell 的 `IO` 机制，也无需处理多线程同步问题。

### 4. 指令与请求驱动架构
所有状态变更都被延迟执行。控件发出 `Request`（如创建、修改、替换控件），并可附带 `Instruction`（空间变换指令，如 `Move`, `Scale`, `Angle`, `Flip`）。
- 父控件可以在子控件的请求和事件向上传递时进行拦截和矩阵变换。诸如“滚动视图 (ScrollView)”或“动态缩放”等功能完全由纯数学变换实现，无副作用。

### 5. 生产级文本与编辑器引擎
引擎内部实现了一套极其复杂且高度优化的文本系统：
- **Block 级字体缓存**: 使用 `IntMap` 对 SDL 纹理进行字符/块级别的缓存 (`Block_font`)。配合自适应缩放机制，在窗口尺寸改变时动态重新光栅化。
- **高级编辑器**: 内置 `Editor` 控件，支持 50+ 可编程快捷键 (`Editor_binding`)、双光标高亮选区、段落级跳转，以及系统原生的剪贴板集成 (复制/粘贴)。
- **精准排版**: 原生支持左、中、右对齐排版，基于字体度量学 (Ascent/Descent) 进行极其精准的行高计算。

### 6. 真·自适应渲染与离屏画布 (Canvas)
- **自适应窗口**: 引擎通过计算最大公约数 (GCD)，自动将设计分辨率完美映射到物理窗口分辨率。拒绝模糊拉伸，所有的形状和字体都会在目标分辨率下重新精准绘制。
- **Canvas 系统**: 支持通过 `Canvas` 控件切换 SDL 渲染目标 (`SDL_TEXTUREACCESS_TARGET`)，允许在最终呈现到屏幕前进行复杂的离屏渲染和图像合成。

### 7. 数据与状态解耦
通过 **Data 控件家族**（`Bool_data`, `Int_data`, 自定义 `Data a` 等），组件的局部状态被直接保存在引擎的全局 Map 中。业务逻辑通过 ID "拉取 (Pull)" 这些数据，将数据层与渲染层完美剥离，天然支持游戏存读档 (Save/Load)。

## 🛠 技术栈

- **语言:** Haskell (GHC)
- **底层:** SDL2 (直接调用 `SDL.Raw` 绑定，进行底层的指针与内存操作，如 `alloca`, `poke`, `peek`)
- **核心数据结构:** 大量使用 `Data.Sequence` (Finger Trees) 处理文本/路径，使用 `Data.IntMap.Strict` 构建极速的组件存储树。

## 🚀 以此为基础的计划

- [ ] 实现简易布局引擎 (类似 Flexbox/Grid)，替代绝对坐标定位。
- [ ] 为 `Text` 控件添加 Markdown/XML 解析器以支持富文本排版。
- [ ] 引入 `binary` 或 `cereal` 库实现全 UI 状态序列化 (存读档系统)。
- [ ] 正式开发基于此引擎的回合制 Roguelike 游戏。
