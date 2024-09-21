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

-- Class de type cria um nome, alguns parâmetros e uma coleção de métodos
class Plus (α : Type) where
  plus : α → α → α
/- Nome da classe : Plus
   Argumentos: α : Type
   Métodos: plus : α → α → α
-/

-- Sobrecarga do método plus
instance : Plus Nat where -- Plus Nat é um tipo
  plus := Nat.add -- Método da classe Plus

open Plus (plus) -- Métodos de classe de tipo são definidos em um namespace

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
-- Create IntPos as a Struct
structure IntPos where
  succ ::
  pred : Nat

-- define sum operation
def IntPos.plus (x : IntPos) (y : IntPos) : IntPos :=
  { pred := x.pred + y.pred  + 1}

-- One example
def one : IntPos := { pred := 0 }
def two : IntPos := { pred := 1 }
#eval (IntPos.plus one two).pred

-- Overloading Add
instance : Add IntPos where
  add := IntPos.plus

-- Example using simbol " + "
#eval (one + two).pred

-- Create a product function of IntPos numbers
def IntPos.product (x : IntPos) (y : IntPos) : IntPos :=
  { pred := x.pred * y.pred + x.pred + y.pred }

-- Example
def seven : IntPos := { pred := 6 }
def forty_nine : IntPos := { pred := 48 }
#eval (IntPos.product seven forty_nine).pred

-- Overloading
instance : Mul IntPos where
  mul := IntPos.product

-- Example using " * " operator
#eval ( seven * forty_nine).pred

/--/
def IntPosToString (atTop : Bool) (p : IntPos) : String :=
    let paren s := if atTop then s else "(" ++ s ++ ")"
  match p.pred with
  | 0 => "succ 0"
  | .succ n => let k : IntPos := {pred := n} ;
                paren s!"Pos.succ {IntPosToString false k}"

instance : ToString IntPos where
  toString := IntPosToString true

#eval one

-- def IntPos.soma : IntPos → IntPos → IntPos :=
-/
-- Create a new Type
inductive Evens : Type where
  | zero : Evens
  | succ : Evens → Evens

-- Define some elements of this type
def zero : Evens := Evens.zero
def dois : Evens := Evens.succ (Evens.zero)

-- Define sum operation
def Evens.plus : Evens → Evens → Evens
  | Evens.zero, k => k
  | Evens.succ n , k => Evens.succ (n.plus k)

-- testing sum operation
def quatro : Evens := dois.plus dois

-- oveloading sum operation to Add
instance : Add Evens where
  add := Evens.plus

def seis : Evens := quatro + dois

def EvensToString (atTop : Bool) (p : Evens) : String :=
  let paren s := if atTop then s else "(" ++ s ++ ")"
  match p with
  | Evens.zero => "Evens.one"
  | Evens.succ n => paren s!"Evens.succ {EvensToString false n}"

instance : ToString Evens where
  toString := EvensToString true

#eval s!"{quatro}"

/- 4.2 Type Classes and Polymorphism -/

def List.sum [Add α] [OfNat α 0] : List α → α
  | [] => 0
  | x :: xs => x + xs.sum

def fourNats : List Nat := [1, 2, 3, 4]

#eval fourNats.sum

structure PPoint (α : Type) where
  x : α
  y : α
deriving Repr

instance [Add α] : Add (PPoint α) where
  add p1 p2 := { x := p1.x + p2.x, y := p1.y + p2.y }

/- Exercício
Escreva uma instância de OfNat para o tipo de dado de
número par dos exercícios da seção anterior que usa busca de
instância recursiva. Para a instância base, é necessário
escrever OfNat Even Nat.zero em vez de OfNat Even 0.
-/

instance : OfNat Evens Nat.zero where
  ofNat := Evens.zero

instance [OfNat Evens n] : OfNat Evens (Nat.succ (Nat.succ n)) where
  ofNat := Evens.succ (Evens.succ ( OfNat.ofNat n ))

def oito : Evens := 254
#eval s!"Oito em Evens: {oito}"

/- 4.3 -/

-- Permitir soma de ℕ + Pos e Pos + ℕ
def addNatPos : Nat → Pos → Pos
  | 0, p => p
  | n + 1, p => Pos.succ (addNatPos n p)

def addPosNat : Pos → Nat → Pos
  | p, 0 => p
  | p, n + 1 => Pos.succ (addPosNat p n)

-- Sobrcarregando

instance : HAdd Nat Pos Pos where
  hAdd := addNatPos

instance : HAdd Pos Nat Pos where
  hAdd := addPosNat

#eval (3 : Pos) + (5 : Nat)
#eval (3 : Nat) + (5 : Pos)

-- Classe para soma de ℕ + Pos = Pos + ℕ
class HPluss (α : Type) (β : Type) (γ : Type) where
  hPlus : α → β → γ

instance : HPluss Nat Pos Pos where
  hPlus := addNatPos

instance : HPluss Pos Nat Pos where
  hPlus := addPosNat

-- É necessário passar o tipo do retorno
#eval (HPluss.hPlus (3 : Pos) (5 : Nat) : Pos)

-- Evitamos definindo γ : Retorno como parâmetro
class HPlus (α : Type) (β : Type) (γ : outParam Type) where
  hPlus : α → β → γ

instance : HPlus Nat Pos Pos where
  hPlus := addNatPos

instance : HPlus Pos Nat Pos where
  hPlus := addPosNat

#eval HPlus.hPlus (3 : Pos) (5 : Nat)

instance [Add α] : HPlus α α α where
  hPlus := Add.add

-- Produz MetaVariáveis
#check HPlus.hPlus (5 : Nat)


/-
Defina uma instância de HMul (PPoint α) α (PPoint α) que multiplique
ambas as projeções pelo escalar. Deve funcionar para qualquer tipo α
para o qual haja uma Mul α instância.

Por exemplo:
#eval {x := 2.5, y := 3.7 : PPoint Float} * 2.0
deve render

{ x := 5.000000, y := 7.400000 }

-/
