{-# LANGUAGE EmptyCase #-}
{-

  01: Введение

  Добро пожаловать на курс "Введение в функциональное программирование"!

  В этом курсе мы познакомимся с функциональным программированием,
  используя функциональный язык программирования Haskell.

  Курс ориентирован на студентов уже знакомых с другими
  языками программирования (с 2 курса и выше).

  В курсе будет много практики — написание программ и решение
  упражнений. Также будет немного теории.

  Вам необходимо настроить среду для прохождения курса. 
  Понадобится компилятор ghc и менеджер пакетов cabal.
  Подробности по установке есть в README.md.

-}

{-
  Для удобства написания программ, как и в любом взрослом языке,
  существует механизм модулей.

  Модуль декларируется специальным синтаксисом:
-}
module Lecture01 where
-- Позже мы обсудим модули подробнее

-- Термы и типы

{-
  Как правило, функциональный язык состоит из термов и типов.

  Примеры термов:
  
    - числа: 2, 2.3, 324234, 1.2
    - строки: "hello", "i'm just a term"
    - списки: [1,2,3], ["yes, i'm inside this list"]
    - функции: \x -> x + 1, div
    - и другие

  Примеры типов:

    - Привычные типы данных: Int, String, Double, Float
    - Списки: [Int], [String]
    - Полиморфные типы: Maybe a, [a]
    - и другие

  Начнем мы с синтаксиса объявления термов.
-}

-- Целые числа со знакомыми операциями:
a, b, m :: Int -- такая запись означает, что a, b, m имеют тип Int.
--         ^ все типы начинаются с большой буквы, так построен синтаксис языка
--      ^ два двоеточия используются для декларации типа: 2 :: Int, "heh" :: String
-- ^ термы одного типа можно объявлять через запятую.
a = 50
b = 564 + 123
m = 34 * 50
-- ^ термы определяются через уравнения

{-
  Выше мы определили, что:
    - a равно значению 50
    - b равно сумме чисел
    - m равно произведению чисел

  Имена констант и функций должны состоять из латинских букв, цифр, подчеркиваний
  и апострофа и начинаться со строчной буквы или с подчёркивания. Имя _,
  состоящее из одного подчёркивания, является специальным, обсудим это чуть позже.

  Для целочисленного деления нужно использовать функцию `div`.
-}
c :: Int
c = div 60 2

{-
  Особенностью является унарный минус. Нужно запомнить, что его
  обязательно нужно закрывать в скобки. Иначе парсер языка подумает
  что это бинарный минус. Например:
  
  > 35 * -1
  <interactive>:1:1: error:
    Precedence parsing error
      cannot mix ‘*’ [infixl 7] and prefix `-' [infixl 6] in the same infix expression
-}
d :: Int
d = 50 * (-123)

-- В языке есть числа с плавающей точкой
pi :: Double
pi = 3.141592653589793238462643

e :: Float
e = 2.71828

-- Символьный тип
firstLetter :: Char
firstLetter = 'A'

-- Строки
name :: String
name = "John"

-- Конкатенация строк происходит с помощью оператора (++)
greeting :: String
greeting = "Hello, " ++ name ++ "!"

-- Булевый тип:
usuallySnowIsWhite :: Bool
usuallySnowIsWhite = True

goodEcology :: Bool
goodEcology = False

snowIsDirty :: Bool
snowIsDirty = usuallySnowIsWhite && not goodEcology

-- Помимо && и not есть привычные логические операции == и ||
-- Отличается знак неравенства: /= (похож на зачеркнутое равенство в математике)

{-
  В языке присутствует на первый взгляд привычный `if _ then _ else`
-}
whatSalary :: Int
whatSalary = if isGoodWorker then 100000 else 10000
  where
    isGoodWorker = True

{-
  Важно, что в Haskell у if всегда должен быть else.

  Так написать нельзя:

  >if 3 > 10 then 5

  Будет ошибка компиляции:

  parse error (possibly incorrect indentation or mismatched brackets).

  Конструкция `if` в императивных языках — это управляющая конструкция (statement).
  Это значит, что она "управляет" последовательностью выполнения кода.

  В Haskell все конструкции языка - выражения, включая `if _ then _ else`.
  Грубо говоря, выражение - это кусок кода, который возвращает значение.
  Например, 2 + 3 - выражение. "Hello, world!" - тоже выражение.
  
  Поскольку `if_then_else` — это выражение, то у него должен быть тип.

  Рассмотрим выражение: (if a then 3 else c) :: X
  и попробуем порассуждать что какой тип должен быть на месте X.

  Мы знаем, что if сначала ожидает булевое значние, то есть
  терм `a` должен иметь тип `Bool`. Затем мы видим, что если `a`
  истинно (то есть равно `True`), то вернется `3`.
  Из этого следует, что терм `c` обязательно должен иметь тип `Int`.
  Потому что наше выражение может иметь только один тип,
  и это тип `X`. Тогда получается следующий набор уравнений:

  - `a :: Bool`
  - `c :: Int`
  - `(if a then 3 else c) :: Int`

-}

-- Функции:

{-
  До :: стоит имя функции, после - тип. Тип Int -> Int означает,
  что функция принимает один аргумент типа Int и возращает Int.
  Это похоже на Func<int, int> в C#.
-}
double :: Int -> Int
double n = n * n

-- Так можно объявить функцию с несколькими аргументами:
add :: Int -> Int -> Int
add a b = a + b 

{-
  Чтобы вызвать функцию, необходимо передать ей аргументы через пробел.
  Вы можете попробовать вызвать ее в ghci:

  > add 3 5
  8
-}

{-
  Допустим мы хотим вычислить 123 * 123 + 2435:

  > add 2435 double 123

  получаем следующую ошибку:

  <interactive>:13:1: error:
    • Non type-variable argument in the constraint: Num (a -> a)
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall a. (Num a, Num (a -> a)) => a

  Почему так получилось? Вызов функции имеет самый высокий приоритет.
  Из-за этого выражение `add 2435 double 123` интерпретируется
  как `(add 2435 double) 123`. А так как `add` ожидает вторым
  аргументом `Int`, а не функцию, то возвращается очень страшная
  и непонятная ошибка.

  Как получить то что хочется? Необходимо обернуть выражение в скобки

    add 2345 (double 123)

  или использовать оператор `($)`:

    > add 2345 $ double 123
    17474

  Оператор `($)` просто принимает на вход функцию `f` и аргумент `x`,
  а затем возвращает `f x`. Он имеет самый низкий приоритет и на практике
  его используют просто для красивой замены скобочек.
-}

{-
  Во время написания функций иногда хочется сохранять результаты вычислений.
  Для этого есть выражения `let` и `where`:
-}
someArithmeticCalculations :: Int
someArithmeticCalculations =
  let a = double 4 in
  let b = add a 2 in
    a - b + c
  where
    c = 556

{-
  Важно, что в таком выражении нельзя изменить значения у `a`, `b` и `c`.
  В Haskell переменная это не область памяти, которой дали имя, а просто синоним.
  Другими словами, запись `let a = 2` в Haskell похожа на запись `const int = 2` в C#.
-}

-- <Задачи для самостоятельного решения>

{-
  Функция `tellSign` должна описывать целое число `n`, возвращая строку:
    - если n = 0, то "zero"
    - если n > 0, то "positive"
    - если n < 0, то "negative"
-}
tellSign :: Int -> String 
tellSign 0 = "zero"
tellSign n = if n > 0 then "positive" else "negative"


{-
  `howManyDigits` возвращает количество цифр целого числа `n`:
    - если n = 6, то "single"
    - если n = 12, то "two-digit"
    - если n >= 100, то "three-digit or more"
-}
howManyDigits :: Int -> String
-- howManyDigits n = error "not implemented"
howManyDigits n 
  | abs n < 10 = "single"
  | abs n < 100 = "two-digit"
  | otherwise = "three-digit or more" 
{-
  `describeNumber` возвращает полное описание целого числа, используя
  функции `tellSign` и `howManyDigits`:
    - если n = 0, то "zero single"
    - если n = 6, то "positive single"
    - если n = -12, то "negative two-digit"
    - если n >= 100, то "positive three-digit or more"
-}
describeNumber :: Int -> String
-- describeNumber n = error "not implemented"
describeNumber n = (tellSign n) ++ " " ++ (howManyDigits n)

-- </Задачи для самостоятельного решения>

-- Рекурсия:

{-
  В Haskell нет циклов как в императивных языках:

    for (int i = 0; i < 10; i++) или for i in [1..10]

  потому что все циклы это просто синтаксический сахар поверх счетчика и меток
  с "go to" или итератора в языках высокого уровня. Для этого необходимы переменные,
  а точнее изменение состояния.

  В качестве примера рассмотрим функцию, вычитающую единицу из числа
  и всегда возвращающую ноль:
-}
makeZero :: Int -> Int
makeZero x =
  if x == 0 then 0 -- если x равен 0, то мы возвращаем 0, это база рекурсии
  else makeZero (x - 1) -- иначе вызываем саму функцию, уменьшив x на единицу
-- ^ это шаг рекурсии

-- <Задачи для самостоятельного решения>

{-
  Реализуйте факториал:
  - если n = 0, то 1
  - если n > 0, то n * (n - 1)!

  Обратите внимание на тип `Integer`. Это тип целых чисел, но без ограничений
  по размеру. Он не переполняется и его можно безопасно использовать для
  больших чисел.
-}
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

{-
  На вход приходит целое число. Необходимо вернуть количество цифр:
    - если n = 0, то 1
    - если n = -14, то 2
    - если n = 144545, то 6
-}
digitsCount :: Int -> Int
digitsCount n 
  | abs n < 10 = 1
  | otherwise = 1 + digitsCount (div n 10)




-- </Задачи для самостоятельного решения>

-- Паттерн матчинг, `case _ of` и guards

{-
  Haskell обладает мощным механизмом pattern matching (или сопоставление по образцу).

  Например функцию `makeZero` мы можем переписать как:
-}
makeZero' :: Int -> Int
makeZero' 0 = 0
makeZero' x = makeZero' (x - 1)

{-
  Давайте рассмотрим вызов `makeZero' 2`.

  Функция начнет смотреть на аргумент и искать уравнение сверху вниз.
  Так как 2 /= 0, то первое уравнение будет пропущено и выполнено второе.
  В нем x равен 2 и тогда будет произведен вызов `makeZero' (2 - 1)`,
  который в свою очередь вычислится в выражение `makeZero' 1`.

  Функция снова начнет смотреть на аргумент и искать уравнение сверху вниз.
  Так как 1 /= 0, то первое уравнение будет пропущено и выполнено второе.
  Мы получим `makeZero' 0`, которое уже вернет нам 0 и вычисление закончится.
-}

{-
  Мы использовали синтаксис определения функций для составления уравнений.
  Помимо этого есть и другой механизм, который называется `case _ of`:
-}
makeZero'' :: Int -> Int
makeZero'' x = case x of
  0 -> 0
  x -> makeZero' (x - 1)

{-
  На самом деле компилятор преобразовывает все уравнения, как в функции `makeZero'`,
  в функции с использованием `case _ of`.

  Pattern matching в Haskell работает на всех термах (кроме функций). То есть,
  в отличии от switch-case в привычных языках, здесь можно матчиться на любых значениях:
-}
matchString :: String -> String
matchString "hello" = "world"
matchString "work" = "money"
matchString _ = "everything else"
--          ^ эта запись означает, что нам неважно какой здесь терм и мы его игнорируем

{-
  Для удобства есть синтаксический сахар над функциями под названием guards:
-}
isOldEnoughToBuyBeer :: Int -> String
isOldEnoughToBuyBeer n
  | n >= 21 = "Yes, sure, in America that's enough"
  | n >= 18 = "Yes, sure, in Europe, Russia and some other countries"
  -- ^ здесь можно комбинировать какие угодно условия, главное чтобы
  -- выражение имело тип `Bool`
  | otherwise = "No, too young"
  -- ^ `otherwise` — это просто константа из стандартной библиотеки и равна False

{-
  Также на уровне синтаксиса языка есть кортежи:

    x = (2, "Hello") :: (Int, String)
    fst x == 2
    snd x == "Hello"

  Конечно же, бывают кортежи с бОльшим числом элементов.
    y = ('a', "abc", 5, True)
    
  К сожалению, в стандартной библиотеке нет функций для выделения определённого элемента кортежа.
  Такие функции можно реализовать самостоятельно с помощью pattern matching.
  
  fst3 (x, _, _) = x
  snd3 (_, y, _) = y
  thrd3 (_, _, z) = z
-}
