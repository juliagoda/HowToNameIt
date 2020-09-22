{-# LANGUAGE MultiParamTypeClasses
            ,FlexibleContexts
            ,FlexibleInstances
            ,TypeSynonymInstances
            ,TemplateHaskell
            ,NoMonomorphismRestriction #-}
            
module Pokoje
( First'(..)
, Optional(..)
, Wizytowka(..)
, BokiKorytarza(..)
, Person(..)
, Tree'(..)
, testTreeInt
, testTree
, testTreeList
, tellAboutPerson
, tellPerson
, jg
, tr
, Vector(..)
, Pair(..)
, vplus
, vectMult
, scalarMult
, YearsOld(..)
, obliczDwaPola
, dodajBokiData
, obliczPowierzchnie
, czyMozeLazienke
, ileOsob
, cenaZaDobe
, standardPok
, ascMonoid
, intersperse'
, intercalate'
, transpose'
, concat'
, io
, io2
, io3
, io4
, io5
, io6
, io7
, io8
, io9
, io10
, io11
, io12
, io13
, io14
, io15
, msumFoldableMonad
, msumFoldableMonadTreeList
, foldmFoldableMonadTree
) where
    
    
    
import Data.List as L
import qualified Data.Map as Map
import System.IO
import System.Environment
import System.IO.Error
import System.Directory
import Data.Char
import qualified Data.Monoid
import qualified Data.Foldable
import qualified Control.Monad
import Test.QuickCheck
import qualified Data.Semigroup as S
import Data.List.NonEmpty as N



data Powierzchnia = Powierzchnia Float Float deriving (Show, Read, Eq)
data PomieszczeniaPow = Lazienka Float | Pokoj Powierzchnia Powierzchnia | Pomieszczenie Powierzchnia  deriving (Show, Read, Eq) 

data Wizytowka a b = Wizytowka { nazwaHotelu :: String
						       , adresHotelu :: String 
						       , numerTel :: a
						       , iloscGwiazdek :: b
                                                     } deriving (Show, Eq)

data BokiKorytarza a = BokiKorytarza a a deriving (Show)


		

obliczDwaPola :: (Num a) => BokiKorytarza a -> BokiKorytarza a -> a
obliczDwaPola (BokiKorytarza w h) (BokiKorytarza w2 h2) = (w * h) + (w2 * h2)

dodajBokiData :: (Num a) => BokiKorytarza a -> BokiKorytarza a -> BokiKorytarza a
dodajBokiData (BokiKorytarza w h) (BokiKorytarza w2 h2) = BokiKorytarza (w + w2) (h + h2)

-- moze jest to male naduzycie, ale w przypadku dlugich typow, warto rozwazyc napisanie takiego aliasu, zeby kazdy wiedzial, co to reprezentuje
-- 

type Dlugosc = Float
type Szerokosc = Float
type PoleKwadratowe = Float
type IloscOsob = Int
type Cena = Int

obliczPowierzchnie :: Dlugosc -> Szerokosc -> Float
obliczPowierzchnie dl sz = dl * sz * 400.0

czyMozeLazienke :: PoleKwadratowe -> Bool
czyMozeLazienke powierzchnia = if powierzchnia > 36000000.0 then True else False

ileOsob :: IloscOsob -> Float
ileOsob osob
    | osob == 1 = 36000000.0
    | osob == 2 = 64000000
    | osob == 4 = 100000000.0
    | otherwise = 0

cenaZaDobe :: Float -> Bool -> Int
cenaZaDobe powierzchnia lazienka
    | (powierzchnia <= 36000000.0) && (lazienka == False) = 35
    | (powierzchnia > 36000000.0) && (powierzchnia < 100000000.0) && (lazienka == False) = 45
    | (powierzchnia > 36000000.0) && (powierzchnia < 100000000.0) && (lazienka == True) = 50
    | (powierzchnia >= 100000000.0)  && (lazienka == False) = 60
    | (powierzchnia >= 100000000.0)  && (lazienka == True) = 75
    | otherwise = 0

standardPok :: Cena -> String
standardPok cena
    | cena < 46 = "niski" 
    | (cena > 39) && (cena < 60) = "średni" 
    | cena > 59 = "wysoki"
    | otherwise = "brak danych"

   
sayHello :: String -> IO ()
sayHello str = putStrLn $ "Czesc, " ++ "tutaj mieszka " ++ str ++ "?"

dateFromNow :: (Integral a) => a -> a
dateFromNow x = mod (4 + x) 7


-- funkcja h z lambdą i sygnaturą, która zawiera typy polimforiczne (Num a) => a -> a -> a
-- funkcja oczekuje dwóch argumentów numerycznych
-- Num jest klasą, natomiast a jest dowolnym istniejącym typem, dla którego stworzono instancję dla Num (niebezpośrednio też, np Num -> Integral -> inny typ, który także będzie posiadał odziedziczone zachowanie superklasy Num) 
h :: (Num a) => a -> a -> a
h = \x y -> x + y

-- funkcja hh z lambdą i sygnaturą, która zawiera jasno określone typy (zamiast a, jest Int)
-- funkcja oczekuje dwóch argumentów typu Int
hh :: Int -> Int -> Int
hh = \x y -> x + y

example = L.length [1,2] > L.length "123"

x = print
y = print "woohoo"
z = x "hello world"


-- podkreślnik oznacza, że nie jest ważna reszta listy w tym przypadku
-- (x:xs) - x to pierwszy element listy, a xs pozostala czesc listy
-- (x:y:xs) - x to pierwszy element, y to drugi element, a xs pozostala czesc listy
-- al@(x:xs) - al to cała lista (rekurencja na liście nie zmienia tej wartości, czasem taka stała wartość jest potrzebna) 
functionH (x:_) = x

-- tutaj niepotrzebna jest sygnatura funkcji, bo x jak i y to dowolne typy, które można między sobą porównywać
-- nie ma sensu okraniczać porównywania do konkretnego typu, by funkcja była reużywalna dla innych typów
functionC x y = if (x > y) then True else False

-- wyciąganie elementu z tuple, tutaj jest to drugi element
functionS (x, y) = y

--data Person = Person Bool deriving Show

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)


applyTimes 0 f b = b
applyTimes times f b = f (applyTimes (times - 1) f b)
-- applyTimes 5 (+1) 5
--  ((+1) ((+1) ((+1) ((+1) ((+1) 5)))))

ff :: Bool -> Int
ff False = 0


fibbonacci 0 = 0
fibbonacci 1 = 1
fibbonacci x = fibbonacci (x - 1) + fibbonacci (x - 2)
-- fibbonacci 6
-- ((5 - 1) ((6 - 1) + (6 - 2)) (5 - 2))


cattyConny x s = x ++ " kitty, " ++ s

flippy = flip cattyConny
appedCatty = cattyConny "woops"
frappe = flippy "haha"


-- gdy sie tworzy wlasna rekurencje, warto wypisac wszystkie kroki ewaluacji dla jakiegos przykladu. Patrzac na kroki ewaluacji latwiej odgadnac, co jest potrzebne
-- granice dla pattern matching lepiej wypisywac najwyżej, aby mogly zostac dostrzezone przez kompilator. W innym razie kroki wykonywania moga sie nigdy nie skonczyc
sumAll 0 = 0
sumAll n = sumAll (n - 1) + n
-- 5
-- (((1 + 2) + 3) + 4) + 5
-- dlaczego rozpisane tal jak powyżej (a po której stronie jest "n"? - po prawej w "sumAll (n - 1) + n")


multiplyAll 0 = 0
multiplyAll 1 = 1
multiplyAll n = multiplyAll (n - 1) * n
-- multiplyAll - podobnie jak poprzednio


data DividedResult = Result (Integer, Integer) | DividedByZero deriving (Show, Eq, Ord)

-- pierwszy wynik z getLine jest zapisany w "name"
-- wynik z drugieo getLine jest zapisany w "age"
-- mówi się, że ">>" ignoruje akcje monadyczne po lewej stronie i skupia się na prawej stronie, by zwrócić ją jako wynik (to prawda, "imię proszę" i "wiek prosze" są wyswietlane w konsoli, ale nie maja wpływu na wynik ostateczny
-- nawiasy zostały dodane dla ułatwienia zrozumienia
twoBinds' :: IO ()
twoBinds' = 
    putStrLn "imie prosze:" >>
    getLine >>=
    \name ->
    (putStrLn "wiek prosze:" >>
    getLine >>=
    \age ->
    (putStrLn ("Sie masz: " ++ name ++ ". Masz: " ++ age ++ " lat?")))




data Optional a = Nada | Only a deriving (Eq, Show)

-- teraz trzeba uwzgledniac w tworzeniu instancji takze semigrupy
instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    
instance Semigroup a => Semigroup (Optional a) where
    (Only a) <> (Only b) = Only (a <> b)
    Nada <> (Only b) = Only b
    (Only a) <> Nada = Only a
    Nada <> Nada = Nada
    
-- aliasy    
type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String


madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj = e <> "! powiedzial " <> adv <> " gdy wskoczyl do swojego auta " <> noun <> " i odjechal z jego " <> adj <> " zona."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat [e, "! powiedzial ", adv, " gdy wskoczyl do swojego auta ", noun, " i odjechal z jego ", adj, " zona."]


asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c = a <> (b <> c) == (a <> b) <> c

ascMonoid :: (Eq a, Monoid a) => a -> a -> a -> Bool
ascMonoid a b c = (a <> (b <> c)) == ((a <> b) <> c)


newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)
-- przykładowo:
-- Prelude> First' (Only 5)
-- Wynik: First' {getFirst' = Only 5}
-- Prelude> getFirst' (First' (Only 5))
-- Wynik: Only 5

-- nazwy w record syntax pozwalaja pozbywac sie typu wrappera
-- tutaj pozbywany jest typ First'

-- w przypadku newtype niepotrzebny jest constraint Monoid a => Monoid (First' a), ponieważ działa on jak wrapper na kod
instance Monoid (First' a) where
    mempty = First' Nada
    mappend = (<>)
    
instance Semigroup (First' a) where
    (First' (Only a)) <> _ = First' (Only a)
    _ <> (First' (Only b)) = First' (Only b)
    _ <> _ = First' Nada
    
firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = frequency [(1, return Nada), (3, Only <$> arbitrary)]
    
instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = First' <$> arbitrary
    
    
-- Semigroups
-- lista nieposiadajaca pustego elementu []
semi = 1 :| [2,3]

xs = 1 :| [2,3]
ys = 4 :| [5,6]
    
resultSemis = xs <> ys
-- 1 :| [2,3,4,5,6]



-- record syntax
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , email :: String  
                     } deriving (Show, Eq) 
                     
jg = Person {firstName="Jagoda", lastName="Gorska", age=26, height=1.64, email="juliagoda.pl@protonmail.com"} 
    
tellAboutPerson :: Person -> String
tellAboutPerson person = "Mam na imie: " ++ firstName person ++ " " ++ lastName person ++ ". Mam " ++ (show . age) person ++ " i mam " ++ (show . height) person ++ " m wzrostu. Jesli masz pytania napisz na: " ++ email person 

tellPerson :: Person -> String
tellPerson (Person {firstName = f, lastName = l, age = a, height = h, email = p}) = "Mam na imie: " ++ f ++ " " ++ l ++ ". Mam " ++ show a ++ " i mam " ++ show h ++ " m wzrostu. Jesli masz pytania napisz na: " ++ p


data Vector a = Vector a a a deriving (Show, Eq, Ord)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n  


ghci> Vector 3 5 8 `vplus` Vector 9 2 8  
-- Vector 12 7 16  
ghci> Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3  
-- Vector 12 9 19  
ghci> Vector 3 9 7 `vectMult` 10  
-- Vector 30 90 70  
ghci> Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0  
-- 74.0  
ghci> Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)  
-- Vector 148 666 222  

haskellPeople = [jg]  
ifPersonExists x = x `elem` haskellPeople
ifPersonsEqual p1 p2 = p1 == p2


data YearsOld = Joschi | Jagoda | Piotrek    
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
           
ghci> Joschi == Joschi  
-- True  
ghci> Jagoda > Joschi  
-- True  
ghci> Jagoda `compare` Piotrek  
-- LT  
ghci> minBound :: YearsOld  
-- Joschi  
ghci> maxBound :: YearsOld  
-- Piotrek


data LockerState = Taken | Free deriving (Show, Eq)  
  
type Code = String  
  
-- alias dla długiego typu  
type LockerMap = Map.Map Int (LockerState, Code)  


lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]
    
lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing -> Left $ "Taki numer do schowka " ++ show lockerNumber ++ " nie istnieje!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Ten klucz do schowka " ++ show lockerNumber ++ " już ktoś zabrał!"
                                
                                
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)  

ghci> Empty  
-- Empty  

ghci> 5 `Cons` Empty  
-- Cons 5 Empty  

ghci> 4 `Cons` (5 `Cons` Empty)  
-- Cons 4 (Cons 5 Empty)  

ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))  
-- Cons 3 (Cons 4 (Cons 5 Empty)) 


infixr 5 :-:  
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord) 


ghci> 3 :-: 4 :-: 5 :-: Empty  
-- (:-:) 3 ((:-:) 4 ((:-:) 5 Empty))  

ghci> let a = 3 :-: 4 :-: 5 :-: Empty  
ghci> 100 :-: a  
-- (:-:) 100 ((:-:) 3 ((:-:) 4 ((:-:) 5 Empty)))  


-- własny operator

infixr 5  .++  
(.++) :: List a -> List a -> List a   
Empty .++ ys = ys  
(x :-: xs) .++ ys = x :-: (xs .++ ys) 


ghci> let a = 3 :-: 4 :-: 5 :-: Empty  
ghci> let b = 6 :-: 7 :-: Empty  
ghci> a .++ b  
-- (:-:) 3 ((:-:) 4 ((:-:) 5 ((:-:) 6 ((:-:) 7 Empty)))) 


-- różnica między foldl a foldl' oraz między foldr a foldr'

-- foldl, foldl', foldr i foldr' wymagają podania wartości początkowej (w miejsce drugiego argument funkcji)
-- foldl' i foldr' to funkcje tak zwane restrykcyjne, co znaczy, że skupiają się na niezbędnych elementach, zamiast od razu na całości
-- w tym wypadku te niezbędne elementy to dwa sąsiednie elementy na liście (np. dwie dodawane liczby z których powstaje jedna, taką redukcja elementów cechuje się właśnie "folding")
-- w sytuacji "leniwej" czyli dla foldl, foldr, foldl1, foldr1 wszystkie kroki obliczania są rozpisywane, zanim zostaną redukowane, co zajmuje dużo więcej czasu, ale działają przez to dla list nieskończonych (taką listą nieskończoną jest np. [1..])
-- foldr1 i foldl1 są takie same jak foldl i foldr, ale podanie wartości początkowej nie jest wymagane (wynik w foldr1 i w foldr może się różnić)


Prelude Data.Foldable> foldr (-) 0 [1,2,3]
2

-- 1 krok: 1 -
-- 2 krok: 1 - (2 -
-- 3 krok: 1 - (2 - (3 - 0))
-- 4 krok: 1 - (2 - (3))
-- 5 krok: 1 - (- 1)
-- 6 krok: 2

Prelude Data.Foldable> foldl (-) 0 [1,2,3]
-6

-- 1 krok: (0 - 1) 
-- 2 krok: (0 - 1) - 2
-- 3 krok: ((0 - 1) - 2) - 3
-- 4 krok: ((-1) - 2) - 3
-- 5 krok: (-3) - 3
-- 6 krok: -6

-- z kolei restrykcyjne funkcje fold działają tak:

Prelude Data.Foldable> foldl' (-) 0 [1,2,3]
-6

-- 1 krok: (0 - 1) 
-- 2 krok: (-1) - 2
-- 3 krok: (-3) - 3
-- 4 krok: -6

Prelude Data.Foldable> foldr1 (-) [1,2,3]
2

-- 1 krok: (1 -
-- 2 krok: (1 - (2 -
-- 3 krok: (1 - (2 - 3))
-- 4 krok: (1 - (- 1))
-- 5 krok: 2

Prelude Data.Foldable> foldl1 (-) [1,2,3]
-4

-- 1 krok: (1 - 
-- 2 krok: (1 - 2)
-- 3 krok: ((1 - 2) - 3)
-- 4 krok: (- 1) - 3
-- 5 krok: -4

-- powyższy wynik różni się od foldl (-) 0 [1,2,3]


-- inny przykład związany z różnicą między restrict a lazy

plus :: Int -> Int -> Int
plus _ 0 = 0 -- gdy bedzie po drodze cos innego niz liczba, zwroc 0
plus a b = a * b

list = [1,2,3,undefined,4,5]

-- wersja rozpisywana
-- dalsze spojrzenie na wszystkie kroki ewaluacji pozwala uwzglednic undefined w gornej granicy funkcji plus bez problemu (czyli w plus _ 0 = 0, gdzie undefined dopasowuje sie w _ )
Prelude GHC.Err Data.Foldable> foldl plus 0 list
0

-- wersja "po drodze"
-- nie wie co robic, widzi tylko aktualny element i kolejny, ktorym jest w koncu undefined, liczba i undefined nie moga zostac polaczone
Prelude GHC.Err Data.Foldable> foldl' plus 0 list
*** Exception: Prelude.undefined


plus2 :: Int -> Int -> Int
plus2 _ b = b -- gdy bedzie po drodze cos innego niz liczba, zwroc ostatnia liczbe z listy
plus2 a b = a * b

list = [1,2,3,undefined,4,5]

Prelude GHC.Err Data.Foldable> foldl plus2 0 list
5

Prelude GHC.Err Data.Foldable> foldl' plus2 0 list
*** Exception: Prelude.undefined


-- Jeśli nie ma potrzeby pracowania przy listach nieskończonych leniwych i chce się po prostu uzyskać zwyklą wartość, 
-- szczególnie gdy mowa o dużej ilości elementów, to warto użyć foldl' zamiast foldr'
-- można zawsze samemu wywołać np. foldr' (+) 0 [1..1000000]
-- oraz foldl' (+) 0 [1..1000000] i zobaczyć, który wywołuje się szybciej

"foldl' (+) 0 [1..1000000]" szybciej od "foldr (+) 0 [1..1000000]" oraz "foldr' (+) 0 [1..1000000]" szybciej od "foldl (+) 0 [1..1000000]"

"foldl1 (+) [1..1000000]" oraz "foldr1 (+) [1..1000000]" wykonują się w podobnym czasie, choć i tak wolniej niż foldl' w tym przypadku


-- Jeśli już mowa o leniwych funkcjach, to można to zaobserwować oczywiście w innych funkcjach w języku
-- na przykładach choćby w słynnych mapperach i filtrach

Prelude Data.Foldable> map (+3) [1,2,3,undefined,4,5]
[4,5,6,*** Exception: Prelude.undefined

-- powyżej widać część wyników, zanim mapper napotkał "undefined"
-- a że nie posiadał żadnych informacji na temat tego, co znajduje się na liście za "undefined", zwrócił od razu wyjątek (do uzyskania takiego rezultatu jak z funkcjami fold, potrzebna by byla kolejna funkcja pomocnicza jak np. plus, czy plus2)
-- mimo to "leniwość" i tak widać po wypisywanych elementach



data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  


singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  

treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right) 

    
treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right


nums = [8,6,4,1,7,3,5]  
numsTree = foldr treeInsert EmptyTree nums

ghci> numsTree  
-- Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree)) 

ghci> 8 `treeElem` numsTree  
-- True  
ghci> 100 `treeElem` numsTree  
-- False


data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False 
    
instance Show TrafficLight where  
    show Red = "Czerwone swiatlo"  
    show Yellow = "Zolte swiatlo"  
    show Green = "Zielone swiatlo" 
    
--instance (Eq m) => Eq (Maybe m) where  
--    Just x == Just y = x == y  
--    Nothing == Nothing = True  
--    _ == _ = False 
    
    
class YesNo a where  
    yesno :: a -> Bool 
    
instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True 
    
instance YesNo [a] where  
    yesno [] = False  
    yesno _ = True 
    
instance YesNo (Maybe a) where  
    yesno (Just _) = True  
    yesno Nothing = False  
    
instance YesNo (Tree a) where  
    yesno EmptyTree = False  
    yesno _ = True
    
instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True 
    
--instance Functor Maybe where  
--    fmap f (Just x) = Just (f x)  
--    fmap f Nothing = Nothing  
    
ghci> fmap (*2) (Just 200)  
-- Just 400  
ghci> fmap (*2) Nothing  
-- Nothing

instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub) 
    
ghci> fmap (*2) EmptyTree  
-- EmptyTree  
ghci> fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])  
-- Node 28 (Node 4 EmptyTree (Node 8 EmptyTree (Node 12 EmptyTree (Node 20 EmptyTree EmptyTree)))) EmptyTree 

--instance Functor (Either a) where  
--    fmap f (Right x) = Right (f x)  
--    fmap f (Left x) = Left x 
    
    
data Barry t k p = Barry { yabba :: p, dabba :: t k }  
ghci> :k Barry  
-- Barry :: (* -> *) -> * -> * -> *  
-- (* -> *) to t

instance Functor (Barry a b) where  
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}  
    
    
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode  
--import System.IO  

-- hGetContents otwiera dostęp do pliku
doWithHandle = do  
    handle <- openFile "girlfriend.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle 

    
-- withFile przejmuje ścieżkę do pliku, kodu IOMode, a następnie przejmuje funkcję, która przejmuje handle i zwraca jakieś działanie I/O. To, co zwraca, to czynność We/Wy, która otworzy ten plik, zrobi coś, czego chcemy z plikiem, a następnie go zamknie.
doWithFile = do     
    withFile "girlfriend.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle     
        putStr contents)


readAllFromFile = do  
    contents <- readFile "girlfriend.txt"  
    putStr contents 


--import System.IO     
--import Data.Char  
    
overwriteFile = do     
    contents <- readFile "girlfriend.txt"     
    writeFile "girlfriendcaps.txt" (map toUpper contents) 


-- appendFile posiada tą samą sygnaturę co writeFile, ale appendFile nie usuwa danych z pliku, ale dodaje nowe na sam koniec treści.

appendToFile = do     
    todoItem <- getLine  
    appendFile "todo.txt" (todoItem ++ "\n") 

-- zawartość <- hGetContents nie powoduje jednoczesnego odczytania całego pliku i zapisania go w pamięci. Jest to I/O lazy. To tak jakby podłączyć potok z pliku do wyjścia. Tak jak możesz myśleć o listach jako strumieniach, możesz również myśleć o plikach jako strumieniach. 

-- putStrLn jest funkcją, która bierze łańcuch i zwraca akcję I/O, która wypisze ten łańcuch do terminala i nową linię po nim. hPutStrLn bierze handle i łańcuch i zwraca akcję I/O, która zapisze ten łańcuch do pliku powiązanego z handle, a następnie umieści nową linię po nim. hGetLine bierze handle i zwraca akcję I/O, która odczytuje linię ze swojego pliku.


data Tree' a = EmptyNode | Leaf a | TreeNode (Tree' a) a (Tree' a) deriving (Show, Eq)

--newtype NewTree a = NewTree { getTree :: Tree' a } deriving (Show, Eq)


-- singleton :: a -> Tree' a  
-- singleton x = TreeNode EmptyNode x EmptyNode  
  

-- treeInsert :: (Ord a) => a -> Tree' a -> Tree' a  
-- treeInsert x EmptyNode = singleton x 
-- treeInsert x (Leaf a) 
--    | x > a = TreeNode (Leaf  -- trzeba dokonczyc
-- treeInsert x (TreeNode left a right)   
--    | x == a = TreeNode left x right  -- jesli element wkladany jest rowny korzeniowi, wstaw
--    | x < a  = TreeNode (treeInsert x left) a right  -- jesli element wkladany jest mniejszy niz korzen, to wstaw to dalej w strone lewej drzewa (z definicji drzewa binarnego). Kolejna rekurencja bedzie robila tak samo z danymi zaktualizowanymi
--    | x > a  = TreeNode left a (treeInsert x right) -- jesli element wkladany jest wiekszy niz korzen, to wstaw to dalej w strone prawej drzewa (z definicji drzewa binarnego). Kolejna rekurencja bedzie robila tak samo z danymi zaktualizowanymi

-- najpierw trzeba stworzyc instancje dla monoidu
-- instance Foldable NewTree where
--    foldMap f $ NewTree EmptyNode = 
   
   
-- fold :: Monoid m => t m -> m
-- foldMap :: Monoid m => (a -> m) -> t a -> m

-- foldr :: (a -> b -> b) -> b -> t a -> b
-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)

-- foldl :: (b -> a -> b) -> b -> t a -> b
-- foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn

-- foldr' :: (a -> b -> b) -> b -> t a -> b
-- foldl' :: (b -> a -> b) -> b -> t a -> b
-- foldr1 :: (a -> a -> a) -> t a -> a
-- foldl1 :: (a -> a -> a) -> t a -> a

-- elem :: Eq a => a -> t a -> Bool
-- maximum :: forall a. Ord a => t a -> a
-- minimum :: forall a. Ord a => t a -> a

-- maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
-- minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a


-- asum :: (Foldable t, Alternative f) => t (f a) -> f a
sumFoldables = asum [Just "Hello", Nothing, Just "World"]
-- Just "Hello"

-- działa bo [] jest istniejąca instancją dla Alternative 
sumFoldables2 = asum testTreeList
-- [1,5,6,5,8,7,11,10,12]

-- działa bo Maybe jest istniejąca instancją dla Alternative 
sumFoldables3 = asum testTreeInt
-- Just 1


*Main Pokoje Control.Monad Control.Applicative Data.Foldable> toList testTreeInt
-- [Just 1,Just 5,Just 6,Just 5,Just 8,Just 7,Just 11,Just 10,Just 12]
*Main Pokoje Control.Monad Control.Applicative Data.Foldable> toList testTreeList
-- [[1],[5],[6],[5],[8],[7],[11],[10],[12]]
*Main Pokoje Control.Monad Control.Applicative Data.Foldable> toList testTree
-- [1,5,6,5,8,7,11,10,12]


*Main Pokoje Control.Monad Control.Applicative Data.Foldable> null testTree
-- False


*Main Pokoje Control.Monad Control.Applicative Data.Foldable> length testTree
-- 9


*Main Pokoje Control.Monad Control.Applicative Data.Foldable> elem 5 testTree
-- True
*Main Pokoje Control.Monad Control.Applicative Data.Foldable> elem (Just 5) testTreeInt
-- True
*Main Pokoje Control.Monad Control.Applicative Data.Foldable> elem [5] testTreeList
-- True


*Main Pokoje Control.Monad Control.Applicative Data.Foldable> maximum testTree
-- 12
*Main Pokoje Control.Monad Control.Applicative Data.Foldable> maximum testTreeInt
-- Just 12
*Main Pokoje Control.Monad Control.Applicative Data.Foldable> maximum testTreeList
-- [12]


*Main Pokoje Control.Monad Control.Applicative Data.Foldable> minimum testTree
-- 1
*Main Pokoje Control.Monad Control.Applicative Data.Foldable> minimum testTreeInt
-- Just 1
*Main Pokoje Control.Monad Control.Applicative Data.Foldable> minimum testTreeList
-- [1]


*Main Pokoje Control.Monad Control.Applicative Data.Foldable> sum testTree
-- 65
*Main Pokoje Control.Monad Control.Applicative Data.Foldable> product testTree
-- 11088000


-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
*Main Pokoje Control.Monad Control.Applicative Data.Foldable> concatMap (\x -> [x + 1]) testTree
-- [2,6,7,6,9,8,12,11,13]


*Main Pokoje Control.Monad Control.Applicative Data.Foldable> any (> 5) testTree
-- True
*Main Pokoje Control.Monad Control.Applicative Data.Foldable> all (> 5) testTree
-- False

-- szuka elementu, nie indeksu
*Main Pokoje Control.Monad Control.Applicative Data.Foldable> find (> 5) testTree
-- Just 6



        
instance Foldable Tree' where
    foldMap f EmptyNode = mempty
    foldMap f (Leaf x) = f x
    foldMap f (TreeNode leftSide x rightSide) = foldMap f leftSide `mappend` f x `mappend` foldMap f rightSide 
    foldr f z EmptyNode = z
    foldr f z (Leaf x) = f x z -- (liść - 0, gdzie 0 to wartość początkowa z) - od prawej strony węzła
    foldr f z (TreeNode leftSide x rightSide) = foldr f (f x (foldr f z rightSide)) leftSide
    foldl f z EmptyNode = z
    foldl f z (Leaf x) = f z x -- (0 - liść, gdzie 0 to wartość początkowa z) - od lewej strony węzła
    foldl f z (TreeNode leftSide x rightSide) = foldl f (f (foldl f z leftSide) x) rightSide
 
 
 -- <*> Functor jeszcze nie zawiera <*>
instance Functor Tree' where
    fmap f EmptyNode = EmptyNode
    fmap f (Leaf x) = Leaf $ f x
    fmap f (TreeNode leftSide x rightSide) = TreeNode (fmap f leftSide) (f x) (fmap f rightSide)

    
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
instance Traversable Tree' where
    traverse f EmptyNode = pure EmptyNode
    traverse f (Leaf x) = Leaf <$> f x
    traverse f (TreeNode leftSide x rightSide) = TreeNode <$> traverse f leftSide <*> f x <*> traverse f rightSide
   
   
testTree :: Num a => Tree' a
testTree = TreeNode 
            (TreeNode  
                (TreeNode EmptyNode 1 EmptyNode)
                5
                (TreeNode EmptyNode 6 EmptyNode)  
            ) 
            5
            (TreeNode  
                (TreeNode EmptyNode 8 EmptyNode)
                7
                (TreeNode (Leaf 11) 10 (Leaf 12))  
            )  
            
            
testTreeInt :: Num a => Tree' (Maybe a)
testTreeInt = TreeNode
            (TreeNode  
                (TreeNode EmptyNode (Just 1) EmptyNode)
                (Just 5) 
                (TreeNode EmptyNode (Just 6) EmptyNode)  
            ) 
            (Just 5)
            (TreeNode  
                (TreeNode EmptyNode (Just 8) EmptyNode)
                (Just 7)
                (TreeNode (Leaf (Just 11)) (Just 10) (Leaf (Just 12)))  
            ) 
            
            
testTreeList :: Num a => Tree' [a]
testTreeList = TreeNode
            (TreeNode  
                (TreeNode EmptyNode [1] EmptyNode)
                [5] 
                (TreeNode EmptyNode [6] EmptyNode)  
            ) 
            [5]
            (TreeNode  
                (TreeNode EmptyNode [8] EmptyNode)
                [7]
                (TreeNode (Leaf [11]) [10] (Leaf [12]))  
            ) 
            
            
Main> :m +Data.Functor.Identity
Main Data.Functor.Identity> traverse (Identity . (+1)) [1,2,3]
-- Identity [2,3,4]

Main Data.Functor.Identity> runIdentity $ traverse (Identity . (+1)) [1,2,3]
-- [2,3,4]          

Main Data.Functor.Identity> traverse (Just . (+10)) testTree
-- lub traverse (Just <$> (+10)) testTree
-- Just (TreeNode (TreeNode (TreeNode EmptyNode 11 EmptyNode) 15 (TreeNode EmptyNode 16 EmptyNode)) 15 (TreeNode (TreeNode EmptyNode 18 EmptyNode) 17 (TreeNode (Leaf 21) 20 (Leaf 22))))


Main Data.Functor.Identity> mapM (return . (+5)) testTree
-- TreeNode (TreeNode (TreeNode EmptyNode 6 EmptyNode) 10 (TreeNode EmptyNode 11 EmptyNode)) 10 (TreeNode (TreeNode EmptyNode 13 EmptyNode) 12 (TreeNode (Leaf 16) 15 (Leaf 17)))

-- mapM :: Monad m => (a -> m b) -> t a -> m (t b)
Main Data.Functor.Identity> mapM (print . (+5)) testTree
--6
--10
--11
--10
--13
--12
--16
--15
--17
-- TreeNode (TreeNode (TreeNode EmptyNode () EmptyNode) () (TreeNode EmptyNode () EmptyNode)) () (TreeNode (TreeNode EmptyNode () EmptyNode) () (TreeNode (Leaf ()) () (Leaf ())))

Main Data.Functor.Identity> mapM_ (print . (+5)) testTree
--6
--10
--11
--10
--13
--12
--16
--15
--17


-- sequenceA :: Applicative f => t (f a) -> f (t a)
Main Data.Functor.Identity Control.Monad> sequence (Just . (+10) <$> testTree)
-- Just (TreeNode (TreeNode (TreeNode EmptyNode 11 EmptyNode) 15 (TreeNode EmptyNode 16 EmptyNode)) 15 (TreeNode (TreeNode EmptyNode 18 EmptyNode) 17 (TreeNode (Leaf 21) 20 (Leaf 22))))

-- sequenceA :: Applicative f => t (f a) -> f (t a)
Main Data.Functor.Identity Control.Monad> sequenceA (Just . (+10) <$> testTree)
-- Just (TreeNode (TreeNode (TreeNode EmptyNode 11 EmptyNode) 15 (TreeNode EmptyNode 16 EmptyNode)) 15 (TreeNode (TreeNode EmptyNode 18 EmptyNode) 17 (TreeNode (Leaf 21) 20 (Leaf 22))))


-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)            
tr = traverse (\x -> print x) testTree


intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' el (x:xs) = el : x : intersperse' el xs

intercalate' :: [a] -> [[a]] -> [a]
intercalate' inter alls@(x:xs) = x ++ (concat $ intersperse' inter alls)

transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' ([] : _) = []
transpose' ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose' (xs : [t | (_:t) <- xss]) 
-- gdzie x to pierwszy element na liscie
-- gdzie xs to reszta elementow na liscie
-- gdzie xss to pozostale listy na liscie dwuwymiarowej


concat' :: [[a]] -> [a]
concat' [[a]] = [a]
concat' (xs : ss) = xs ++ (concat' ss) 
concat' _ = []

--newtype Pair b a = Pair { getPair :: (a,b) }  

--instance Functor (Pair c) where  
--    fmap f (Pair (x,y)) = Pair (f x, y)  

-- przy tworzeniu instancji ostatni element jest brany pod uwage do zmian, wiec tu jest to c (nie b a)
-- ale we wnetrzu newtype widac c na drugim miejscu elementow tuple
-- wiec tylko tu wolno potraktowac miejsce funkcją
-- naniesienie funkcji w innych miejscach wywola blad przy kompilacji
newtype Pair b a c = Pair { getPair :: (a,c,b) }  


instance Functor (Pair c h) where  
    fmap f (Pair (x,y,z)) = Pair (x, f y,z) 


newtype First a = First { getFirst :: Maybe a }  
    deriving (Eq, Ord, Read, Show)  
    
    
instance Monoid (First a) where  
    mempty = First Nothing  
    
    
instance Semigroup (First a) where
    First (Just x) <> _ = First (Just x)  
    First Nothing <> x = x  
    
    
retFirst = getFirst $ First (Just 'a') `mappend` First (Just 'b') 
-- Just 'a'  

ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]  
-- Just 9  


io = do  
    putStrLn "Czesc, jak masz na imie?"  
    name <- getLine  
    putStrLn ("Czesc " ++ name ++ "!") 
    

-- strzalka "<-" w bloku "do" wyciaga wartosc z typu monadycznego. 
-- odpowiednik "<-", jest ">>=", w tym że:
-- funkcja zwracajaca wartosc o typie monadycznym (nazwijmy tę funkcję "sfg") >>= funkcja przyjmujaca rezultat funkcji sfg w miejsce ostatniego parametru i zwracajaca wartosc typu monadycznego, ">>=" przenosi rezultat z lewej strony na prawą stronę bez typu monadycznego 
io2 = do  
    foo <- putStrLn "Czesc, jak masz na imie?"  
    name <- getLine  
    putStrLn ("Czesc " ++ name ++ "!") 
    
    
io3 = do  
    putStrLn "Jak masz na imie?"  
    firstName <- getLine  
    putStrLn "Jak masz na nazwisko?"  
    lastName <- getLine  
    let bigFirstName = L.map toUpper firstName  
        bigLastName = L.map toUpper lastName  
    putStrLn $ "Hej " ++ bigFirstName ++ " " ++ bigLastName ++ ", co u ciebie?" 
   
   
io4 = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
              
  
reverseWords :: String -> String  
reverseWords = unwords . L.map L.reverse . words 

io5 = do  
    a <- return "hell"  
    b <- return "yeah!"  
    putStrLn $ a ++ " " ++ b  
    
io6 = do  
    let a = "hell"  
        b = "yeah"  
    putStrLn $ a ++ " " ++ b  

Main> putStr "jhhj" >> putStr "fgdsf" >> putStrLn "qweqw"
Main> putChar 'A' >> putChar 'B' >> putChar 'C' >> putStrLn ""

io7 = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
        else return ()

-- when :: Bool -> IO () -> IO b
io8 = do  
    c <- getChar  
    Control.Monad.when (c /= ' ') $ do  
        putChar c  

-- sequence :: [IO a] -> IO [a]
io9 = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs 

-- putStrLn "hehe" zwraca wartość IO (), czyli (), dlatego nie jest ukazywany
-- getLine zwraca wartość IO String, czyli String, dlatego jest ukazany wynik
-- w przypadku sequence (map print [1,2,3,4,5]) wynikiem jest:
-- 1
-- 2
-- 3
-- 4
-- 5
-- [(),(),(),(),()]
-- więc jak map print [1,2,3,4,5] :: [IO ()]
-- to sequence :: [IO a] -> IO [a]
-- więc jak rezultatem print jest IO () a nie IO String
-- to rezultatem jest IO [(),(),(),(),()], bo bylo 5 elementow na liscie
-- mapM_ nie dba o wynik, dba jedynie o skutki uboczne jak np. wyświetlanie na monitorze, zapisywanie danych do plików


io10 = Control.Monad.forever $ do  
    putStr "Napisz cos i zatwierdz: "  
    l <- getLine  
    putStrLn $ L.map toUpper l  

Main> io10
-- Napisz cos i zatwierdz: JJ
-- JJ
-- Napisz cos i zatwierdz: JJ
-- JJ
-- Napisz cos i zatwierdz: KK
-- KK
-- Napisz cos i zatwierdz: UU
-- UU
-- Napisz cos i zatwierdz: PP
-- PP
-- Napisz cos i zatwierdz: LL
-- LL

-- Control.Monad.forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
io11 = do   
    colors <- Control.Monad.forM [1,2,3,4] (\a -> do  
        putStrLn $ "Jaki kolor przypisałbyś do liczby " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "Kolory, które przypisałeś do 1, 2, 3 i 4 to: "  
    mapM putStrLn colors  
    
Main> io11
-- Which color do you associate with the number 1?
-- Red
-- Which color do you associate with the number 2?
-- Blue
-- Which color do you associate with the number 3?
-- Yellow
-- Which color do you associate with the number 4?
-- Green
-- The colors that you associate with 1, 2, 3 and 4 are: 
-- Red
-- Blue
-- Yellow
-- Green
-- [(),(),(),()]

-- Mogliśmy to zrobić bez forM, tylko z forM jest bardziej czytelne. 
-- Normalnie piszemy forM, gdy chcemy zmapować i uporządkować niektóre działania, 
-- które definiujemy przy użyciu notacji "do". 
-- Na tej samej zasadzie mogliśmy zastąpić ostatnią linię forM colors putStrLn.

-- getContents jest akcją I/O, która odczytuje wszystko od standardowego wejścia aż do momentu, gdy napotka znak końca pliku. 
-- Jego typem jest getContents :: IO String. Fajne w getContents jest to, że odpowiada za leniwe obsługiwanie I/O. 
-- Kiedy robimy foo <- getContents, nie odczytuje wszystkich danych wejściowych naraz, nie przechowuje ich w pamięci, 
-- a następnie wiąże je z foo. Jeśli dalszy kod będzie wymagał danych z foo, wtedy zostaną podjęte w tle odpowiednie kroki.

io12 = do  
    contents <- getContents  
    putStr (L.map toUpper contents) 

Main Pokoje> io12
-- pPlLjJ  KKmMnN
-- od razu literę przekształca w dużą bez zatwierdzania enterem


io13 = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  
    
    
-- interact :: (String -> String) -> IO ()
-- interact przejmuje funkcję typu String -> String jako parametr i zwraca akcję I/O, 
-- która wykona na niej jakąś czynność wejściową, uruchomi tę funkcję i wydrukuje wynik funkcji.
io14 = interact shortLinesOnly

-- krótsza wersja io14
io14shorter = interact $ unlines . filter ((<10) . length) . lines  

-- odsiewa linijki tekstu o większej ilości znaków niż 10 (nie wyświetli ich)
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = L.filter (\line -> L.length line < 10) allLines  
        result = unlines shortLines  
    in  result 
    
 
io15 = interact respondPalindromes  

-- point-free pattern
respondPalindromes = unlines . L.map (\xs -> if isPalindrome xs then "to palindrom" else "to nie jest palindrom") . lines  
    where   isPalindrome xs = xs == L.reverse xs  

-- dla io12 i io13, io14 i io15 możnaby po zbudowaniu i skompilowaniu przydzielic tresc z pliku zewnetrznego z pomoca piping
-- cat IO.txt | ./pokoje
-- albo
-- cat IO.txt | runhaskell Pokoje.hs , o ile jest w pliku main


-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode  

-- openFile :: FilePath -> IOMode -> IO Handle
io16 = do  
    handle <- openFile "io.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle  
    
-- Ta funkcja jest bardzo podobna do getContents. Jedyna różnica polega na tym, że getContents będzie automatycznie odczytywane z standardowego wejścia (czyli z terminala), podczas gdy hGetContents pobiera "uchwyt" pliku (czyli handle), który mówi mu, z którego pliku ma być odczytany. We wszystkich innych aspektach, działają one tak samo. I tak jak getContents, hGetContents nie będzie próbował odczytywać pliku od razu i przechowywać go w pamięci, ale będzie go czytać w miarę potrzeb. To naprawdę fajne, ponieważ możemy traktować zawartość jako całą zawartość pliku, ale tak naprawdę nie jest on załadowany do pamięci. Więc gdyby to był naprawdę duży plik, robienie hGetContents nie przeładowałoby naszej pamięci, ale przeczytałoby tylko to, czego potrzebowałoby z pliku, kiedy byłoby to konieczne.


-- WSZYSTKIE FUNKCJE FUNCTOR

-- Gotowymi już funktorami są Maybe, [], IO, Product, Sum, Dual, First, Last, Identity, ZipList, Max, Min, (Either a)

-- fmap :: (a -> b) -> f a -> f b
fmapFunc = (+3) $ Just 7
-- Just 10

-- (<$) :: Functor f => a -> f b -> f a
-- zastępuje wartość funktora z prawej strony wartością z lewej strony, czyniąc ją też funktorem
leftFunc = Just 1 <$ Just 2
-- Just 1

-- ($>) :: Functor f => f a -> b -> f b 
-- zastępuje wartość funktora z lewej strony wartością z prawej strony, czyniąc ją też funktorem
rightFunc = Just 1 $> Just 2
-- Just 2

-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- operator będący aliasem dla fmap
synonymFmap = (+3) <$> Just 7
-- Just 10

-- (<&>) :: Functor f => f a -> (a -> b) -> f b 
-- miejsca argumentów <$> są zamienione
flippedFmap = Just 8 <&> (+5)
-- Just 13

-- void :: Functor f => f a -> f ()
-- odrzuca lub ignoruje wyniki
voidFunctor = void (1,2)
-- (1,())
voidFunctor1 = void (Left 8675309)
-- Left 8675309
voidFunctor2 = void (Right 8675309)
-- Right ()



-- WSZYSTKIE FUNKCJE SEMIGROUP

-- gotowymi semigrupami są Ordering, (), Any, All, Void, (Maybe a), (IO a), (Product a), (Sum a), (Dual a), (Endo a), (Last a), (First a), (Identity a), (Max a), (Min a), (Either a b)

-- Data.Monoid.Last x === Maybe (Data.Semigroup.Last x)

-- (<>) :: a -> a -> a 
-- działa jak mappend Monoidów, to operacja asocjacyjna (zmiana kolejnosci argumentow dla (<>) nie zmiena ostatecznego wyniku)
semiProduct = getProduct (Product 3 <> Product 4 <> mempty)
-- 12

semiSum = getSum (Sum 1 <> Sum 2 <> mempty)
-- 3

semiAny = getAny (Any True <> mempty <> Any False)
-- True
semiAny2 = getAny (mconcat (map (\x -> Any (even x)) [2,4,6,7,8]))
-- True

semiAll = getAll (All True <> mempty <> All False)
-- False
semiAll2 = getAll (mconcat (map (\x -> All (even x)) [2,4,6,7,8]))
-- False

-- appEndo :: a -> a
computation = Endo ("Hello, " ++) <> Endo (++ "!")
semiEndo = appEndo computation "Haskell"
-- "Hello, Haskell!"

-- getDual :: a
semiDual = getDual (mappend (Dual "Hello") (Dual "World"))
-- "WorldHello"

-- getOption :: Maybe a
-- Option jest typem jak Maybe ale z lepiej wykonaną instancją dla Monoidów (Monoid jest superklasą dla typu Option)
-- option :: b -> (a -> b) -> Option a -> b
semiOption = 5 (+5) (Option Nothing)


-- WSZYSTKIE FUNKCJE MONOID

-- Gotowymi monoidami są wszystkie typy, które występują już w Semigroup

-- mempty :: a
-- wartość neutralna monoidów, uwzględnianie jej nie zmienia wyniku
emptyMonoid = mempty :: [a]
-- []
emptyMaybeMonoid = mempty :: Semigroup a => Maybe a
-- Nothing

-- mappend :: a -> a -> a
-- to samo co (<>) semigrupy
mappendMonoid = [1,2,3] `mappend` [4,5,6]
-- [1,2,3,4,5,6]


-- mconcat :: [a] -> a
-- wykonuje folding na monoidach
-- map Sum [1,2,3]
concatMonoids = mconcat [Sum {getSum = 1}, Sum {getSum = 2}, Sum {getSum = 3}]
-- Sum {getSum = 6}
returnValFromConcat = getSum $ Sum {getSum = 6}
-- 6


-- WSZYSTKIE FUNKCJE APPLICATIVE

-- Gotowymi funktorami z aplikacją funkcji są [], Maybe, IO, Product, Sum, Dual, Last, First, Identity, ZipList, Option, Max, Min

-- pure :: a -> f a
-- zmienia zwykłą wartość w typ applicative
pureApp = pure 5
-- :t pureApp 5
-- pure 5 :: (Applicative f, Num a) => f a

-- (<*>) :: f (a -> b) -> f a -> f b 
-- dodaje funkcję w kontekście (obindowany w typ) do wartości w kontekście (w typie)
addFuncContext = Just (+3) <*> Just 5
-- Just 8

-- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
-- zastępuje zapis (+) <$> [1,2] <*> [3,4], który jest bardziej pochłaniający ewaluacyjnie
liftApp2 = liftA2 (+) [1,2] [3,4]
-- [4,5,5,6]

-- (*>) :: f a -> f b -> f b 
-- działanie sekwencyjne, gdzie wynik pierwszej sekwencji jest ignorowany
ignoreSecSeq = Just 3 *> Just 5
-- Just 5

-- (<*) :: f a -> f b -> f a
-- działanie sekwencyjne, gdzie wynik drugiej sekwencji jest ignorowany
ignoreFirstSeq = Just 3 <* Just 5
-- Just 3

-- (<**>) :: Applicative f => f a -> f (a -> b) -> f b 
-- zachowuje się jak <*> w tym, że jego argumenty są poprzestawiane miejscami
addFlippedFuncContext = Just 5 <**> Just (+3)

-- liftA :: Applicative f => (a -> b) -> f a -> f b
-- działa dokładnie jak fmap
liftApp = liftA (+3) (Just 5)
-- Just 8

-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- łączy 3 argumenty i tworzy nowy applicative
liftApp3 = liftA3 (\x y z -> x + y + z) (Just 5) (Just 5) (Just 5)
-- Just 15

-- optional :: Alternative f => f a -> f (Maybe a)
-- "obindowuje" wynik maybe w alternative
optionalAlter = optional Nothing
-- just Nothing
optionalAlter2 = optional (Just 5)
-- Just (Just 5)

-- przykład dla ZipList
zipListApp = (+) <$> ZipList [1,2,3] <*> ZipList [4,5,6]
-- ZipList {getZipList = [5,7,9]}


-- WSZYSTKIE FUNKCJE MONAD

-- Gotowymi monadami są wszystkie typy, które znajdowały się w Applicative, bo monady to aplikujące funktory z dodatkowymi cechami rozbierania wartości z typów, zanim zostaną przeliczone w kolejnej funkcji

-- (>>=) :: forall a b. m a -> (a -> m b) -> m b 
-- Sekwencyjnie zestawia dwie akcje, przekazując każdą wartość wytworzoną przez pierwszą jako argument do drugiej
monadSeqRight = Just 5 >>= return . (+5)
-- Just 10
monadSeqRight1 = [5] >>= return . (+5)
-- [10]

-- (>>) :: forall a b. m a -> m b -> m b 
-- sekwencyjnie wykonuje się najpierw pierwsza rzecz potem druga, ale to drugiej sekwencji wykorzystywany jest wynik w dalszym ewentualnym kodzie programu
twoSeqsMonad = print 5 >> print 6
-- 5
-- 6
twoSeqsMonad2 = getLine >> getLine
-- twoSeqsMonad >>= return . (++ "5")
-- gg
-- aa
-- "aa5"

-- return :: a -> m a
-- zmienia prostą wartość na monad
returnMonad = return 5
-- :t return 5
-- return 5 :: (Monad m, Num a) => m a

-- fail :: String -> m a
-- błąd wyrzucany z własnym tekstem
failMonad = fail "45"
-- *** Exception: user error (45)


-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
-- zmienia elementy struktury innymi wartościami, zmienia ich typ na typ monadów a potem całość obindowuje w monad. Cała struktura znajduje się wewnątrz typu monadycznego
mapMonad = mapM (Just . (+5)) [1,2,3,4]
-- Just [6,7,8,9]

mapMonad2 = mapM (return . (+5)) [1,2,3,4]
-- [6,7,8,9]

mapMonad3 = mapM (Just . (+5)) []
-- Just []

-- powyższe przykłady zadziałały po [] posiada instancję z Traversable


-- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
-- jak mapM ale ignorowany jest wynik ewaluwoany, więc dzieją się w tle tylko skutki uboczne jak wyświetlanie danych na ekranie
mapMonad_ = mapM_ (Just . (+5)) [1,2,3,4]
-- Just ()

mapMonad2_ = mapM_ (Just . (+5)) []
-- Just ()


-- forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
-- to samo co mapM, ale ma poprzestawiane miejscami argumenty
forMonad = forM [1,2,3,4] (Just . (+5))
-- Just [6,7,8,9]

-- forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
-- to samo co mapM_, ale ma poprzestawiane miejscami argumenty
forMonad_ = forM_ [1,2,3,4] (Just . (+5)) 

-- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
-- wykonuje zbiór akcji monadycznych od lewej do prawej strony i przechowuje zbiór monad wyników Traversable
sequenceMonads = sequence [getLine,getLine,getLine]
-- A
-- B
-- C
-- ["A","B","C"]

-- to zadziałało bo na starcie [] był typem Traversable, a getLine był monadycznym IO
-- następnie [] został Monad (bo może być przez wbudowaną już instację z Applicative, a Applicative jest superklasą Monad, co znaczy ze zachowanie Monad jest dziedziczone z klasy Applicative)
-- Liczne String natomiast czyli [Char] zostały Traversable wewnątrz jednego typu Monad []

-- sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
-- to samo co sequence, ale nie obchodzi go wynik, który miałby przekazać dalej
sequenceMonads_ = sequence_ [getLine,getLine,getLine]
-- A
-- B 
-- C

-- (=<<) :: Monad m => (a -> m b) -> m a -> m b 
-- to samo co >>=, ale miejsca argumentow sa poprzestawiane
monadSeqLeft = return . (+5) =<< [5]
-- [10]


-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c 
-- wartość najpierw zmienia się w pierwszej sekwencji, potem w drugiej, na koniec wyświetlany jest ostateczny wynik
kleisliMonadRight = (Just . (5-) >=> Just . (+2)) 5
-- Just 2, bo (5 - 5 + 2), ze względu na zapis (5-) zamiast (-5)

-- (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c 
-- wartość najpierw zmienia się w drugiej sekwencji, potem w pierwszej, na koniec wyświetlany jest ostateczny wynik
kleisliMonadLeft = (Just . (5-) <=< Just . (+2)) 5
-- Just (-2), bo 5 - (5 + 2) = 5 - 7 = -2


-- forever :: Applicative f => f a -> f b
-- powtarza w nieskończoność akcję
foreverMonad = Control.Monad.forever $ do  
    putStr "Napisz cos i zatwierdz: "  
    l <- getLine  
    putStrLn $ L.map toUpper l 
    
-- Napisz cos i zatwierdz: JJ
-- JJ
-- Napisz cos i zatwierdz: JJ
-- JJ
-- Napisz cos i zatwierdz: KK
-- KK
-- Napisz cos i zatwierdz: UU
-- UU
-- Napisz cos i zatwierdz: PP
-- PP
-- Napisz cos i zatwierdz: LL
-- LL
-- ...


-- join :: Monad m => m (m a) -> m a
-- wyrównuje dwa poziomy monad w jeden. Można to wykorzystać do ewaluacji w dalszej części jeśli chodzi o zgodność typów, ale także może się przydać, gdy chcemy wyrównać potok danych IO tak, aby nie zanikały po drodze, a były zwracane na końcu. Więc jeśli nie wyskakuje nam błąd podczas pracy nad monad, ale zarazem nie widzimy wyniku, warto spróbować użyć join
-- :t putStrLn <$> getLine :: IO (IO ())
joinMonadBefore = putStrLn <$> getLine
-- przyjmie wartosc ale nic nie zwroci i nic nie wyswietli pomimo putStrLn. putStrLn jak i getLine działają na własnych poziomach IO i nie ma między nimi potoku danych

-- :t join $ putStrLn <$> getLine :: IO ()
joinMonadAfter = join $ putStrLn <$> getLine
-- wyświetli się to co napiszemy w getLine, następnie dane te czyli String zostanie przekazany putStrLn, który wyświetli wynik na monitorze. Tym razem działają na tej samej płaszczyźnie, przez co dane z getLine trafiają do putStrLn


-- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
-- jest taki sam jak foldl, ale zwraca wynik monadyczny
foldmFoldableMonadTree = foldM (return . (+)) 0 testTreeList
-- foldmFoldableMonadTree 5
-- 45
-- foldmFoldableMonadTree 1
-- 9


-- msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
-- sumuje akcje i łaczy je concatem. Sumowanie następuje na zasadzie monoidów, ponieważ MonadPlus jest monoidycznym monadem
-- poniższe przykłady zadziałają bo zarówno Maybe a jak i [] mają stworzone odpowiednie instancje pod MonadPlus
-- msum zwraca ten sam wynik co asum dla tych samych przykładów bo domyślna definicja mplus jest - mplus = (<|>)
msumFoldableMonad = msum testTreeInt
-- Just 1

msumFoldableMonadTreeList = msum testTreeList
-- [1,5,6,5,8,7,11,10,12]

-- ap :: Monad m => m (a -> b) -> m a -> m b
-- return f `ap` x1 `ap` ... `ap` xn
-- odpowiednik <*>
apMonad = Just (+4) `ap` Just 6
-- Just 10
apMonad1 = (ap $ Just (+10)) $ (ap $ Just (+5)) $ Just (+4) `ap` Just 6
-- Just 25
apMonad2 = Just (+5) `ap` (Just (+4) `ap` Just 6)
-- Just 15

-- (<$!>) :: Monad m => (a -> b) -> m a -> m b 
-- restrykcyjna wersja <$>
strictMonad = (+3) <$!> [6]
-- [9]

strictMonad2 = (+3) <$!> Just 6
-- Just 9

