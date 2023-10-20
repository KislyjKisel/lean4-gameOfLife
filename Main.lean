import Raymath
import Raylib

open Raymath
open Raylib

def cellSize : Vector2 := .mk 8 8
def gridW : Nat := 64
def gridH : Nat := 64
def tickDelay : Nat := 400
def windowWidth : UInt32 := 800
def windowHeight : UInt32 := 600

def Vector (α : Type) (n : Nat) : Type := { a : Array α // a.size = n }

def Vector.replicate {α n} (x : α) : Vector α n :=
  .mk (.mkArray n x) (Array.size_mkArray ..)

def Vector.get {α n} (v : Vector α n) (i : Nat) (h : i < n) : α :=
  v.1.get $ Fin.mk i $ v.2.symm ▸ h

def Vector.set {α n} (v : Vector α n) (i : Nat) (val : α) (h : i < n) : Vector α n :=
  .mk (v.1.set (Fin.mk i $ v.2.symm ▸ h) val) ((Array.size_set ..).trans v.2)

structure Grid where
  data : Vector (Vector Bool gridW) gridH

def Grid.get (grid : Grid) (i j : Nat) (h1 : i < gridH) (h2 : j < gridW) : Bool :=
  (grid.data.get i h1).get j h2

def Grid.set (grid : Grid) (i j : Nat) (val : Bool) (h1 : i < gridH) (h2 : j < gridW) : Grid :=
  let row := grid.data.get i h1
  let row := row.set j val h2
  .mk $ grid.data.set i row h1

def drawGrid (grid : Grid) : IO Unit := do
  for hi: i in [0:gridH] do
    let y := i.toFloat32 * cellSize.x
    for hj: j in [0:gridW] do
      let val := grid.get i j hi.2 hj.2
      let x := j.toFloat32 * cellSize.y
      drawRectangleV (.mk x y) cellSize (cond val .black .white)

def Grid.getOrFalse (grid : Grid) (i j : Nat) (di dj : Int) : Bool :=
  if i + di < 0 ∨ j + dj < 0
    then false
    else
      let i := (i + di).toNat
      let j := (j + dj).toNat
      if h: i < gridH ∧ j < gridW
        then grid.get i j h.1 h.2
        else false

def Grid.neighbours (grid : Grid) (i j : Nat) : Nat :=
  let offsets : Array (Int × Int) := #[
    (-1, -1), (0, -1), (1, -1), (1, 0),
    (1, 1), (0, 1), (-1, 1), (-1, 0)
  ]
  flip offsets.foldl 0 λ count (di, dj) ↦
    cond (grid.getOrFalse i j di dj) (count + 1) count

def cell (neighbours : Nat) : Bool → Bool
| true => neighbours ≥ 2 ∧ neighbours ≤ 3
| false => neighbours == 3

def step : StateT Grid Id Unit := do
  for h1: i in [0:gridH] do
    for h2: j in [0:gridW] do
      let grid ← get
      let val := grid.get i j h1.2 h2.2
      set $ grid.set i j (cell (grid.neighbours i j) val) h1.2 h2.2

def main : IO Unit := do
  let rlctx ← initWindow windowWidth windowHeight "Lean4 Game Of Life".toSubstring
  let mut grid : Grid := .mk $ .replicate (.replicate false)
  let mut nextTick : Nat := (← IO.monoMsNow) + tickDelay
  let mut pause : Bool := true
  let font ← getFontDefault rlctx
  repeat do
    let time ← IO.monoMsNow
    if ← isMouseButtonDown .left then
      let mp := (← getMousePosition) / cellSize
      let i := mp.y.toFloat.toUInt64.toNat
      let j := mp.x.toFloat.toUInt64.toNat
      if h: i < gridH ∧ j < gridW then
        grid := grid.set i j true h.1 h.2
    if time > nextTick then
      nextTick := nextTick + tickDelay
      if !pause then
        grid := (step.run grid).2
    beginDrawing
    clearBackground .raywhite
    drawGrid grid
    if pause then
      let text := "Paused, press `Space` to unpause"
      let fontSize := 20
      let spacing := 4
      let textSize := measureTextEx font text fontSize spacing
      let x := windowWidth.toFloat32 / 2 - textSize.x / 2
      let y := windowHeight.toFloat32 - 20 - textSize.y
      drawTextEx font text { x, y } fontSize spacing .black
    if ← isKeyPressed .space then
      pause := !pause
    endDrawing
    if ← windowShouldClose then break
  closeWindow rlctx
