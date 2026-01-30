# Functional UI Engine (Unnamed Project)

> **Note:** This is a personal experimental project developed from scratch in **Haskell** over the course of one month. It explores a novel architecture for handling UI state and rendering without relying on traditional OOP widgets.

> **注意：** 这是一个完全由 **Haskell** 从零构建的个人实验性项目（开发周期约一个月）。它探索了一种全新的架构，旨在不依赖传统 OOP 组件模式的情况下处理 UI 状态与渲染。

## 🌟 Introduction (简介)

This engine is an attempt to reimagine GUI development from "First Principles" using Functional Programming. Instead of managing mutable widget trees, this engine treats the UI as a stream of transformations. It decouples **Logical Intent** (Requests) from **Physical Execution** (Instructions), solving complex problems like coordinate mapping and context isolation recursively.

这个引擎是从“第一性原理”出发，利用函数式编程思想重构 GUI 开发的一次尝试。它不维护可变的组件树，而是将 UI 视为一系列变换的流。通过将**逻辑意图**（Requests）与**物理执行**（Instructions）解耦，递归地解决了坐标映射和上下文隔离等复杂问题。

## ✨ Key Features (核心特性)

### 1. State-Machine Based Navigation (状态机导航)
Unlike the traditional component tree, event consumption here is modeled as a graph traversal.
*   **Goto / Back:** Widgets return explicit navigation IDs (`Goto Int` or `Back Int`) after handling events.
*   **Logic Flow:** This makes UI flow (e.g., nested menus, wizards) strictly deterministic and historically traceable.

**状态机式导航**：不同于传统的组件树，这里的事件消费被建模为图的遍历。控件在处理事件后返回明确的导航 ID（`Goto` 或 `Back`），使得 UI 流程（如嵌套菜单）严格确定且可回溯。

### 2. Instruction Pipeline & Transforms (指令管线与变换)
A unique bi-directional transformation system:
*   **Downward (Event):** Containers use `event_transform` to modify events (e.g., converting global coordinates to local) before passing them to children.
*   **Upward (Request):** Children emit raw requests. Containers use `request_transform` to wrap these into a sequence of `Instructions` (e.g., `Move_widget`).
*   **Result:** True **Location Transparency**. A widget doesn't need to know where it is; the instruction stack determines its final render position.

**指令管线与变换**：独特的双向变换系统。容器通过 `event_transform` 向下转换事件（如坐标系转换），通过 `request_transform` 向上将子控件的请求包装为指令序列（`Instructions`）。这实现了真正的**位置无关性**。

### 3. High-Fidelity Adaptive Rendering (高保真自适应渲染)
The engine rejects simple texture scaling.
*   **Vector-like Adaptation:** Coordinates and font sizes are recalculated dynamically based on the window size using `adaptive_window` logic.
*   **Crisp Text:** Utilizing a custom `Block_font` system, text textures are regenerated at exact pixel densities, ensuring sharpness on any DPI.

**高保真自适应渲染**：拒绝简单的贴图缩放。引擎基于窗口尺寸动态重新计算参数和字号，配合自定义的 `Block_font` 系统，确保在任何 DPI 下文字都绝对清晰。

### 4. The "Data" Widget Family (Data 控件家族)
Storage is decoupled from the `Engine` struct. Data (Bool, Int, Lists) can be stored directly within the widget tree, effectively turning the UI structure into a functional database.

**Data 控件家族**：数据存储与引擎结构解耦。数据可以直接存储在组件树中，实现了“UI 即数据库”的设计。

### 5. Professional-Grade Editor Widget (专业级编辑器控件)
Includes a fully implemented text editor core supporting:
*   ~50 keyboard shortcuts (Vim/Emacs style logic).
*   Complex text selection, clipboard operations, and viewport tracing.
*   Custom typesetting and layout algorithms (`Text_From.txt`).

**专业级编辑器**：内置一个完整实现的文本编辑器核心，支持约 50 种快捷键操作、复杂的文本选择、剪贴板处理以及自定义排版算法。

## 🛠 Architecture Overview (架构概览)

The core follows a strict separation of concerns:

```haskell
-- Simplified conceptual model
data Engine a = ...
data Request a = ... -- The "Intent" (What to do)
data Instruction = ... -- The "Execution" (How to do it, e.g., Move, Clip)

-- The transformation flow
Node_widget :: (Engine -> Raw_request -> Seq Instruction -> Maybe (Seq Instruction)) -> ...
