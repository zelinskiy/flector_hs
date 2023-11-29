import Data.List (isSuffixOf)
import System.Exit
--import Data.List.Split (splitOn)

data Persona = P1
             | P2
             | P3
             deriving (Show, Read, Enum, Bounded)

data Numerus = Sg
             | Pl
             deriving (Show, Read, Enum, Bounded)

data FormaGrammatica = MkFormaGrammatica {
  persona :: Persona,
  numerus :: Numerus
} deriving (Show)

data Conjugatio = Con1
                | Con2
                | Con3
                | Con4
                deriving (Show, Read, Enum, Bounded)

type Thema = String


ending :: Conjugatio -> FormaGrammatica -> String
-- Praesens Indicativi Activi Con. 1
ending Con1 MkFormaGrammatica {numerus=Sg, persona=P1} = "o"
ending Con1 MkFormaGrammatica {numerus=Sg, persona=P2} = "as"
ending Con1 MkFormaGrammatica {numerus=Sg, persona=P3} = "at"
ending Con1 MkFormaGrammatica {numerus=Pl, persona=P1} = "amus"
ending Con1 MkFormaGrammatica {numerus=Pl, persona=P2} = "atis"
ending Con1 MkFormaGrammatica {numerus=Pl, persona=P3} = "ant"

-- Praesens Indicativi Activi Con. 2
ending Con2 MkFormaGrammatica {numerus=Sg, persona=P1} = "eo"
ending Con2 MkFormaGrammatica {numerus=Sg, persona=P2} = "es"
ending Con2 MkFormaGrammatica {numerus=Sg, persona=P3} = "et"
ending Con2 MkFormaGrammatica {numerus=Pl, persona=P1} = "emus"
ending Con2 MkFormaGrammatica {numerus=Pl, persona=P2} = "etis"
ending Con2 MkFormaGrammatica {numerus=Pl, persona=P3} = "ent"

-- Praesens Indicativi Activi
ending Con3 MkFormaGrammatica {numerus=Sg, persona=P1} = "o"
ending Con3 MkFormaGrammatica {numerus=Sg, persona=P2} = "is"
ending Con3 MkFormaGrammatica {numerus=Sg, persona=P3} = "it"
ending Con3 MkFormaGrammatica {numerus=Pl, persona=P1} = "imus"
ending Con3 MkFormaGrammatica {numerus=Pl, persona=P2} = "itis"
ending Con3 MkFormaGrammatica {numerus=Pl, persona=P3} = "unt"

-- Praesens Indicativi Activi
ending Con4 MkFormaGrammatica {numerus=Sg, persona=P1} = "o"
ending Con4 MkFormaGrammatica {numerus=Sg, persona=P2} = "s"
ending Con4 MkFormaGrammatica {numerus=Sg, persona=P3} = "t"
ending Con4 MkFormaGrammatica {numerus=Pl, persona=P1} = "mus"
ending Con4 MkFormaGrammatica {numerus=Pl, persona=P2} = "itis"
ending Con4 MkFormaGrammatica {numerus=Pl, persona=P3} = "unt"

-- Undefined
ending con fg = "-"


allForms :: Conjugatio -> Thema -> [(FormaGrammatica, String)]
allForms con thema = _allForms _forms []
  where
    -- reversed order to achieve
    _forms = [
      MkFormaGrammatica {numerus=n, persona=p}
      | n <- [Pl, Sg], p <- [P3, P2, P1]
      ]
    _allForms (f:fs) res = _allForms fs ((f, thema ++ ending con f):res)
    _allForms [] res = res

-- allForms con thema = error "Not defined"

printForms ((MkFormaGrammatica {numerus=n, persona=p}, v):fs)
  = unwords [show n, show p, v, "\n"] ++ printForms fs
printForms [] = ""

detectConjugatio :: (String, String) -> Maybe Conjugatio
detectConjugatio (p1, inf)
  | "are" `isSuffixOf` inf = Just Con1
  | "ere" `isSuffixOf` inf && "eo" `isSuffixOf` p1 = Just Con2
  | "ere" `isSuffixOf` inf = Just Con3
  | "ire" `isSuffixOf` inf = Just Con4
  | otherwise = Nothing

getThema inf = take (length inf - 3) inf

mainLoop :: IO ()
mainLoop = do
  putStrLn "Enter: (e.g. amo amare; exit=q)"
  input <- words <$> getLine
  case input of
    ["q"] -> die "Bye!"
    ["h"] -> do
      putStrLn "input=amo amare;\nexit=q"
      main
    p1:inf:_ ->
      case (detectConjugatio (p1, inf)) of
        (Just con) -> do
          putStrLn $ printForms $ allForms con (getThema inf)
          main
        Nothing -> do
          putStrLn "Can't detect conjugation"
          main
    _ -> putStrLn "Can't recognize input" >> main

main :: IO ()
main = do
  putStrLn $ printForms $ allForms Con1 "am"
--  putStrLn $ printForms $ allForms Con2 "mone"
--  putStrLn $ printForms $ allForms Con3 "leg"
--  putStrLn $ printForms $ allForms Con4 "audi"
--  print $ detectConjugatio ("amo", "are")
