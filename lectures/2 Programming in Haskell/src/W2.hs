-- W2.hs (a comment!)
-- Edit this with a text editor (VS Code is great!)

myList :: [Int]   -- myList is a list of Ints
myList = [1..100] -- types can generally be left off

sumList :: [Int] -> Int
sumList []     = 0
sumList (n:ns) = n + sumList ns -- a recursive function
--                   ^^^^^^^       defined using itself!


str1 :: String
str1 = "Simon" -- Strings go in double-quotes

str2 :: [Char]
str2 = str1 -- Strings are just lists of characters

char1 :: Char
char1 = 'S' -- Single characters are in single quotes


addTwoLam = \n -> n + 2
addTwo n = n + 2


swap :: (a, b) -> (b, a)
swap (x, y) = (y, x) -- equivalent to
                     -- swap p = (snd p, fst p)
-- *Main> swap (True, 3.14)
-- (3.14, True)

-- These both work the same:
swap' pair = let (x, y) = pair in (y, x)
swap'' = \(x, y) -> (y, x)


is3or4 x = case x of 3 -> True
                     4 -> True
                     _ -> False


data Hand = Rock | Paper | Scissors
  deriving Eq 

playRock p2 = case p2 of Scissors -> "Rock wins!"
                         _        -> "Rock doesn't win!"


firstOdd :: [Int] -> Bool
firstOdd []     = False
firstOdd (x:xs) = odd x
-- *Main> firstOdd [5..100]
-- True

-- an equivalent definition:
firstOdd' xs = case xs of []     -> False
                          (x:xs) -> odd x



-- A Natural number is either Zero,
-- or the Successor of a Natural number

data Nat = Z | S Nat
  deriving (Eq, Show) -- don't worry about this

-- With this data type, we can roll our own numbers:
one   = S Z
two   = S one -- S (S Z)
three = S two -- S (S (S Z))

-- We can also perform sanity checks:
-- *Main> two == S (S Z)
-- True


addOne :: Nat -> Nat
addOne n = S n

subOne :: Nat -> Nat
subOne Z     = Z
subOne (S n) = n


toInt :: Nat -> Int
toInt Z     = 0           -- Rule TI0
toInt (S n) = 1 + toInt n -- Rule TIR
--                ^^^^^ recursion!


double :: Nat -> Nat
double Z     = Z                -- call this Rule 0
double (S n) = S (S (double n)) -- call this Rule R
--                   ^^^^^^ recursion!


isEven :: Nat -> Bool
isEven Z     = True     -- Rule E0
isEven (S n) = isOdd n  -- Rule ER
--             ^^^^^ mutual

isOdd :: Nat -> Bool
isOdd Z     = False     -- Rule O0
isOdd (S n) = isEven n  -- Rule OR
--            ^^^^^^ recursion


data IntList = Empty | Cons Int IntList
--                     ^^^^ 'Cons' for 'Construct'

myIntList = Cons 2 (Cons 5 (Cons 3 Empty))

sumIntList :: IntList -> Int
sumIntList Empty       = 0
sumIntList (Cons n ns) = n + sumIntList ns

-- *Main> sumIntList myIntList
-- 10


x3 = [ (x,y) | x <- [1..10], y <- [1..1000], y == x^3 ]
-- *Main> x3
-- [(1,1),(2,8),(3,27),(4,64),(5,125),(6,216),(7,343),
--  (8,512),(9,729),(10,1000)]


myListEvens = [n | n <- [1..100], even n]


myListOdds = [n | n <- [1..100], odd n]


muls17 = [ n | n <- [1..], n `mod` 17 == 0 ]


pyTriples = [ (a,b,c) | c <- [1..], b <- [1..c],
                        a <- [1..b], a*a + b*b == c*c ]


