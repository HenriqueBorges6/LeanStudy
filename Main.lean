import LeanStudy



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
  | Nat.succ _ => false


def pred (n : Nat) : Nat :=
  match n with
  | Nat.zero => Nat.zero
  | Nat.succ k => k

def even (n : Nat ): Bool :=
  match n with
    | Nat.zero => true
    | Nat.succ k => not (even k)

inductive Sign where
  | pos
  | neg

def posOrNegThree (s : Sign) : match s with | Sign.pos => Nat | Sign.neg => Int :=
  match s with
  | Sign.pos => (3 : Nat)
  | Sign.neg => (-3 : Int)

def sevens : String × (Int × Nat) := ("VII", (7, 4 + 3))

def primesUnder10 : List Nat := [2, 3,5 , 7]

def last_list {α : Type} (xs : List α) : Option α :=
  match xs with
    | n :: [] => n
    | _ :: t => last_list t
    | [] => none

#eval last_list primesUnder10

def List.findFirst? {α : Type} (xs : List α) (predicate : α → Bool) : Option α :=
  match xs with
    | [] => none
    | n :: h => if predicate n then n else List.findFirst? h predicate

def isPositive (n : Int ) : Bool :=
  n > 0
def numbers : List Int :=
  [-10, -5, 0, 3, 7, -2]

#eval List.findFirst? numbers isPositive


def Prod.swap {α β : Type} (pair : α × β) : β × α :=
  (pair.snd , pair.fst )

#eval Prod.swap ("world" , "hello")


inductive PetName : Type where
  | Dog : String → PetName
  | Cat : String → PetName

def animals : List PetName :=
  [PetName.Dog "Spot", PetName.Cat "Tiger", PetName.Dog "Fifi", PetName.Dog "Rex", PetName.Cat "Floof"]

def howManyDogs (pets : List PetName) : Nat :=
  match pets with
  | [] => 0
  | PetName.Dog _ :: morePets => howManyDogs morePets + 1
  | PetName.Cat _ :: morePets => howManyDogs morePets

def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) :=
  match xs, ys with
  | [] , _ => []
  | _ , [] => []
  | x :: xn , y :: yn => (x,y) :: zip xn yn

#eval zip [1, 2, 3, 4, 5] ["a", "b", "c", "d"]

def take { α : Type } ( xs : List α) (n : Nat): List α :=
  match xs, n with
    | [],_ => []
    | _ , 0 => []
    | x :: xn, n => x :: take xn n.pred

#eval take ["a", "b", "e", "l", "h", "a"] 4

--def distributive { α : Type } (p : α × (β ⊕ γ )) : α × (β ⊕ γ ) → (α × β) ⊕ (α × γ ) :=


-- Cap 2

def main : IO Unit := IO.println "Hello, world!"

def quest1a : 2 + 3 = 5 := by rfl
def quest1b : 15 - 8 = 7 := by rfl
def quest1c : "Hello ".append "world" = "Hello world" := by rfl
def quest1d : 5 < 18 := by simp

def quintaentrada {α : Type} (xs : List α ) ( ok : xs.length > 4) : α  := xs[4]

-- Cria um Type Pos dos números positivos
inductive Pos : Type where
  | one : Pos
  | succ : Pos → Pos

-- Cria uma clase
class Plus (α : Type) where
  plus : α → α → α

instance : Plus Nat where
  plus := Nat.add

open Plus (plus)

#eval plus 5 3

def Pos.plus : Pos → Pos → Pos
  | Pos.one, k => Pos.succ k
  | Pos.succ n, k => Pos.succ (n.plus k)

instance : Plus Pos where
  plus := Pos.plus

instance : Add Pos where
  add := Pos.plus

#eval plus 5 7


def posToString (atTop : Bool) (p : Pos) : String :=
  let paren s := if atTop then s else "(" ++ s ++ ")"
  match p with
  | Pos.one => "Pos.one"
  | Pos.succ n => paren s!"Pos.succ {posToString false n}"

instance : ToString Pos where
  toString := posToString true

def Pos.toNat : Pos → Nat
  | Pos.one => 1
  | Pos.succ n => n.toNat + 1

instance : ToString Pos where
  toString x := toString (x.toNat)

instance : OfNat Pos (n + 1) where
  ofNat :=
    let rec natPlusOne : Nat → Pos
      | 0 => Pos.one
      | k + 1 => Pos.succ (natPlusOne k)
    natPlusOne n

def eight : Pos := 8

-- Exercise:

structure PosNumber where
  succ ::
  pred : Nat

def PosNumber.Plus : PosNumber → PosNumber → PosNumber
  | a, b =>

instance : Add PosNumber where
  add := PosNumber.Plus
