import           Data.List

-- This is a Haskell file. The .hs extension is how you know. Comments are
-- preceded by two dashes (they're here for you and me, and are ignored by the
-- computer). You can also make long comments by using {- and -} like this:

{- This file is in plain text, and can be edited in any text editor. You'll
find one that knows how to highlight Haskell syntax very helpful. Microsoft's
Visual Studio (VS) Code is free and awesome, and what I'd suggest starting
with (it'll recommend you install a Haskell extension when you first open a
.hs file; I use and like Haskell Syntax Highlighting by Justus Adam).

The purpose of this homework is to ensure sure that you've successfully
installed the Haskell platform, and to collect some info on you, your
background, and your interests.

If you have correctly installed the Haskell platform, you should be able to
load this file as follows:

1. Make a folder for your work in this course.
2. Move this file, and great-expectations.txt, into that folder.
3. Using the command prompt, navigate to that folder. Generally, this is done
   with the `cd` command (for 'change directory').
4. Once in the directory, type `ghci HW1.hs`. This starts ghci, the Haskell
   interpreter, and loads HW1.hs. If everything works, you should see
   something like the following:

   [1 of 1] Compiling Main             ( HW1.hs, interpreted )
   Ok, one module loaded.
   *Main>

If that didn't work, make sure that you've followed the steps to install
Haskell (does `ghci` on its own work?), and that you're in the right
directory. An easy way to check that you're in the right directory is typing
`ls` in the command prompt (`dir` is this command's name in Windows), which
will list the files in the current directory. HW1.hs should be among them.

If you're using VS Code, you can streamline this process by Open-ing your
homework folder in VS Code (File > Open; make sure to open the *folder* with
the hs file inside, rather than the hs file) and bringing up the command line
from within VS Code (View > Terminal). This automatically puts you in the
right directory.

If you got this file loaded into ghci, congratulations: you have completed the
bulk of the homework! ^_^ -}

-- To demonstrate that everything's up and working, type `main` (no quotes)
-- after calling `ghci HW1.hs`. You should get a number (if you're very
-- curious, you might poke around at the end of this file to see if you can
-- figure out what this number represents). Report the result here by
-- replacing `undefined` with that number.

myAnswer = undefined

-- Ok, that's it for the hard stuff! Last, tell me some details about you by
-- replacing all the ""s below with answers. Your answers must be in quotes so
-- that they're interpreted as strings/text, rather than as Haskell code. So
-- for my last name, I'd need to write "Charlow" (with quotes).

-- Your last name:
myLastName = ""

-- Your first name:
myFirstName = ""

-- Your major (declared or assumed):
myMajor = ""

-- Prior linguistics courses:
priorLing = ""

-- Prior computer science courses:
priorCS = ""

-- Any other programming experience?
priorProg = ""

-- What drew you to Comp Ling?
clInterest = ""

-- Once you're done with this, refresh the interpreter by typing `:r` into
-- ghci (this loads up your new changes). Then type `main` once more (you'll
-- see the same number you did before). You should now find a .csv file in the
-- directory with the other homework files, storing the info you entered here.
-- Upload that to Canvas, together with your updated HW1.hs file.

-- Below is the code that makes everything work with some hints as to how/why it
-- works.
main :: IO () -- a bit mysterious, but this type signature can be left out;
main = do     -- in most cases, ghci infers types on its own
  s <- readFile "great-expectations.txt" -- load the book as a string, name it s
  print (f "Havisham" s)                 -- display the result of calling `f "Havisham" s` (f is defined below)
  writeFile (myLastName ++ myFirstName ++ ".csv") -- some clunky code for generating the csv file
    (intercalate "," $ map (map $ \x -> if x == ',' then ';' else x)
    [myLastName, myFirstName, myMajor, priorLing, priorCS, priorProg, clInterest])

f :: String -> String -> Int -- f takes two strings and produces a number
f word "" = 0                -- can you figure out how f works?
f word xs = let n = length word in
   if take n xs == word
      then 1 + f word (drop n xs)
      else f word (tail xs)
