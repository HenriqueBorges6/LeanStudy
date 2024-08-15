import LeanStudy

def main : IO Unit :=
  IO.println s!"Hello, {hello}!"


def joinStringsWith (a :String) (b :String ) (c :String) := (b.append a).append c
#check (joinStringsWith)
#eval joinStringsWith ", " "one" "and another"

#check (joinStringsWith ": ")

def volume (l : Nat ) (c : Nat ) (h : Nat ) : Nat := l*c*h
#check (volume)
#eval volume 2 3 4

structure Point where
  point ::
  x : Float
  y : Float
deriving Repr

def origin : Point := { x := 0.0 , y := 0.0 }

def addPoints (p1 : Point) (p2 : Point) : Point :=
  { x := p1.x + p2.x, y := p1.y + p2.y }

def distance (p1 : Point) (p2 : Point) : Float :=
  Float.sqrt (((p2.x - p1.x) ^ 2.0) + ((p2.y - p1.y) ^ 2.0))


structure Point3D where
  x : Float
  y : Float
  z : Float
deriving Repr

def origin3D : Point3D := { x := 0.0, y := 0.0, z := 0.0 }

def Point.modifyBoth (f : Float → Float) (p : Point) : Point :=
  { x := f p.x, y := f p.y }

structure RectangularPrism where
  RectangularPrism ::
  height : Float
  width : Float
  depth : Float
deriving Repr

def vol (Prism : RectangularPrism) : Float :=
  Prism.height * Prism.width * Prism.depth

#check (vol)

structure segment where
  start : Point
  final : Point
deriving Repr

def length (s: segment) : Float :=
  ((s.final.x - s.start.x) + (s.final.y - s.start.y)).sqrt

def v : Point := { x := 5, y:= 4}

def vec : segment := { start := origin , final :=  v}
#eval length vec

--- Perguntar as seguintes questões ao professor
-- Como faz comentário de várias linhas
--Which names are introduced by the declaration of RectangularPrism?
--Which names are introduced by the following declarations of Hamster and Book? What are their types?

structure Hamster where
  name : String
  fluffy : Bool

structure Book where
  makeBook ::
  title : String
  author : String
  price : Float

def isZero (n : Nat) : Bool :=
  match n with
  | Nat.zero => true
  | Nat.succ k => false


def pred (n : Nat) : Nat :=
  match n with
  | Nat.zero => Nat.zero
  | Nat.succ k => k

#eval pred 5
