-- 1) Construa uma gramática e implemente em uma linguagem de
-- programação à sua escolha para reconhecer a linguagem {a*b*} e {a*b+}
reconhecer1 :: String -> Bool
reconhecer1 [] =
  True
reconhecer1 str =
  let
    shouldHaveOnlyB = dropWhile ((==) 'a') str
  in
    all ((==) 'b') shouldHaveOnlyB


reconhecer2 :: String -> Bool
reconhecer2 str =
  let
    shouldHaveOnlyB = dropWhile ((==) 'a') str
  in
    all ((==) 'b') shouldHaveOnlyB && length shouldHaveOnlyB >= 1


-- 2) Construa e implemente em uma linguagem de programação
-- à sua escolha um AFD que aceite:


-- a) Todas as sentenças (0, 1)* que apresentem cada 1 seguido
-- imediatamente de dois 0.
auto1 :: String -> Bool
auto1 =
  auto1' 'F'

auto1' :: Char -> String -> Bool
auto1' 'F' [] =
  True
auto1' _ [] =
  False
auto1' 'F' (x:xs) =
  case x of
    '0' -> auto1' 'F' xs
    '1' -> auto1' 'A' xs
auto1' 'A' (x:xs) =
  case x of
    '0' -> auto1' 'B' xs
    '1' -> False
auto1' 'B' (x:xs) =
  case x of
    '0' -> auto1' 'F' xs
    '1' -> False

-- b) Todas as sentenças de (0, 1)* de modo que a palavra começa
-- por 1 e termina por 0
auto2 :: String -> Bool
auto2 str =
  auto2' 'I' str

auto2' :: Char -> String -> Bool
auto2' 'F' [] =
  True
auto2' _ [] =
  False
auto2' 'I' (x:xs) =
  case x of
    '0' -> False
    '1' -> auto2' 'A' xs
auto2' 'A' (x:xs) =
  case x of
    '0' -> auto2' 'F' xs
    '1' -> auto2' 'A' xs
auto2' 'F' (x:xs) =
  case x of
    '0' -> auto2' 'F' xs
    '1' -> auto2' 'A' xs

-- c) Todas as sentenças de (a, b)* de modo que o último símbolo
-- seja "b" e o número de símbolos "a" seja par
auto3 :: String -> Bool
auto3 str =
  auto3' 'I' str

auto3' :: Char -> String -> Bool
auto3' 'F' [] =
  True
auto3' _ [] =
  False
auto3' 'I' (x:xs) =
  case x of
    'a' -> auto3' 'A' xs
    'b' -> auto3' 'F' xs
auto3' 'A' (x:xs) =
  case x of
    'a' -> auto3' 'I' xs
    'b' -> auto3' 'A' xs
auto3' 'F' (x:xs) =
  case x of
    'a' -> auto3' 'A' xs
    'b' -> auto3' 'F' xs


-- EXTRA


-- Não usando o conceito de AFD, as seguintes duas linhas resolveriam
-- a questão (2b):
-- auto2 str =
--  (head str) == '1' && (last str) == '0' && all (\x -> elem x ['0', '1']) str

