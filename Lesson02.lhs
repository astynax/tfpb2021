> {-# OPTIONS -Wall #-}
> {-# LANGUAGE StandaloneKindSignatures #-}
> {-# LANGUAGE TypeOperators #-}

> module Lesson02 where

Урок №2. Алгебраические типы данных
===================================

Выше включаются парочка расширений и отображение предупреждений.

`StandaloneKindSignatures` позволяет указывать типы для конструкторов типов
(см ниже). А `TypeOperators` позволяет давать конструкторам типов (и значений)
имена в виде инфиксных операторов (ниже так же будет пример).

> import GHC.Types (Type)

`Type` означает некий тип, который не нужно дальше достраивать, добавляя к нему
типы-параметры. Обычно нам не нужно описывать типы типов, поэтому и
импортировать `Type` не приходится. Я же `Type` использую для наглядности.

Пользовательские типы в Haskell объявляются с помощью ключевого слова `data`,
после которого идёт *конструктор типа* с параметрами или без, затем следует
символ равенства и один или несколько *конструкторов данных*, также с
параметрами или без. Вот простейший пример:

> type Unit :: Type -- это уже "конечный" тип
> data Unit = Unit

Конструктор типа тут не имеет параметров, а значит это уже конечный тип.
Значение у типа ровно одно, так что конструктор значений означает сам себя.
Так как значение ровно одно, то и информации никакой это значение не несёт.

Заметьте, что пространство имён для типов и пространство имён для значений
не пересекаются, что позволяет иметь одинаковые имена конструктору типов и
конструктору значений.

У Haskell уже есть готовый тип с одним значением. Называют его так же "unit",
но пишется он покороче:

< data () = ()

Тогда как конструктор типа всегда должен присутствовать, конструкторов
значений может и не быть. Такие типы без значений называют *незаселёнными*
(uninhabitated).

Вот вам пример:

> type Void' :: Type  -- тип есть
> data Void'  -- значений нет!

Такой тип может быть полезен, например, когда, когда мы знаем, что вычисление
некоторого выражения никогда не завершится или гарантировано завершится
с ошибкой: вычислить Void' не получится — значений-то нет!

> looped :: Void'
> looped = looped -- эта функция зацикливается, поэтому можно использовать Void'

Типы-произведения
-----------------

Это такие составные типы данных, значения которых *одновременно* содержат
несколько вложенных значений и описывают некоторую осмысленную сложную
сущность. Количество значений ("мощность") составного типа равна
декартовому произведению мощностей вложенных типов.

Скажем, RGB-цвет является осмысленной совокупностью трёх чисел:

> data RGB = RGB Int Int Int -- тут лучше бы взять Word8, но путь будет Int

Количество возможных цветов здесь будет равно произведению трёх количеств
значений типа Int.

Конструкторы значений могут выступать в роли функций, принимающих
значения *полей* по одному, пока не получится итоговое составное значение:

> -- В REPL команда ":t RGB" покажет
> -- RGB :: Int -> Int -> Int -> RGB
> blueish :: Int -> RGB
> blueish = RGB 0 0 -- частично применили

> blues :: [RGB]
> blues = [blueish 100, blueish 150, blueish 200]

Но не нужно думать о том, что конструкторы значений это просто функции.
Конструкторы позволяют ещё и деконструировать значения, например, при
использовании pattern matching:

> isBlueish :: RGB -> Bool
> isBlueish (RGB 0 0 _) = True
> isBlueish _           = False

А вот пример произведения типа-произведения с параметром:

> type Pair :: Type -> Type
> data Pair a = Pair a a

Заметьте, что "тип типа" выглядит как тип функции. Так получается потому, что
конструктор типов `Pair` имеет параметр — типовую переменную `a`. И конечный
тип можно получить, только указав тип-параметр:

> pairOfBooleans :: Pair Bool
> pairOfBooleans = Pair True False

> pairOfNums :: Num a => Pair a
> pairOfNums = Pair 1 2

Обратите внимание, что во втором примере подстановка переменной `a` в качестве
параметра для `Pair`, это применение конструктора типов, пусть и оставляющее
итоговый тип полиморфным!

Встроенные в Haskell кортежи имеют свой собственный синтаксис, но в целом это
всего лишь произведения с количеством аргументов, равным количеству
вкладываемых типов:

> center :: (Int, Int)
> center = (0, 0)

> cartItem :: (String, Int, Float) -- наименование, количество, цена
> cartItem = ("apple", 2, 12.5)

Конструктор типа-произведения можно описывать в виде так называемой
записи/record, давай полям имена:

> data CMYK = CMYK
>   { cyan    :: Int
>   , magenta :: Int
>   , yellow  :: Int
>   , black   :: Int
>   }

Такой конструктор всё ещё можно использовать как функцию с позиционными
параметрами, но можно задавать значения полям по их именам:

> cmyk1, cmyk2 :: CMYK  -- да, так можно!
> cmyk1 = CMYK 100 200 100 0
> cmyk2 = CMYK
>   { black = 0
>   , cyan = 100
>   , magenta = 200
>   , yellow = 100
>   }

Для каждого поля генерируется функция-getter, извлекающая значение
соответствующего поля:

> -- :t magenta
> -- magenta :: CMYK -> Int
> magenta200 :: Int
> magenta200 = magenta cmyk2

Записи можно деконструировать позиционно, а можно использовать имена полей:

> magentish, yellowish :: CMYK -> Bool
> magentish (CMYK c _ y k) = c == 0 && y == 0 && k == 0
> yellowish CMYK{cyan=c, yellow=y, black=k} =
>   c == 0 && y == 0 && k == 0

Ещё стоит отметить возможность менять значения отдельных полей у уже готовых
записей:

> cmyk3 :: CMYK
> cmyk3 = cmyk2{black = 42}

В качестве типов полей можно указывать типы-аргументы конструктора типа:

> data SimplePerson extra = SimplePerson
>   { spName  :: String
>   , spAge   :: Int
>   , spExtra :: extra
>   }

Поскольку функции-getters в рамках одного модуля находятся в одном
пространстве имён, то использовать одно и то же имя поля в нескольких типов
просто так не получится. Один из способов обхода этого ограничения
заключается в задании всем полям записи одинакового префикса, часто
получаемого из имени типа (акроним или просто camelCase), как я сделал выше.

Если тип-произведение имеет в качестве поля `()`, то произведение перестаёт
зависеть от этого поля: так как у `()` ровно одно значение, то и умножение на
единицу ничего не меняет. А вот использование незаселённого типа в роли
множителя делает весь тип-произведение незаселённым — это уже умножение на
ноль!

Типы-суммы
----------

Тип-сумма описывается указанием нескольких конструкторов значений,
разделённых символом `|`:

> data ChessColor = White | Black

Мощность типа-суммы равна сумме мощностей слагаемых. Конструкторы значений
без полей означают сами себя, поэтому и мощность `ChessColor` равна двум.

Любой из конструкторов может иметь поля:

> data Color a = Red | Blue | Other a

Что интересно, в качестве поля одного из конструкторов-слагаемых может быть и
незаселённый тип! Это не позволит применять данный конструктор и сумма
уменьшится, поскольку одно из слагаемый будет помножено на ноль. Но другие
конструкторы при этом могут быть доступны. Пример:

> colorOfVoid :: Color Void'
> colorOfVoid = Other looped -- тут можно только зациклиться или упасть

> isVoidColored :: Color Void' -> Bool
> isVoidColored (Other _) = True  -- ленивость Haskell спасает!
> isVoidColored _         = False

Способов указать прямо в виде типа "либо число, либо строка" в Haskell нет,
нужно каждый отдельный тип снабдить конструктором и собрать из них сумму. Это
может показаться ограничением, но зато позволяет отличать разные по смыслу
значения одинаковых типов:

> data Name
>   = JustName String
>   | Id Int  -- тут число
>   | SSN Int -- и тут число
>   | Noname

> data Person extra = Person
>   { pName  :: Name  -- сумма в роли множителя
>   , pAge   :: Int
>   , pExtra :: extra
>   }

Если же нужен тип "либо-либо", но сумму заводить ну никак не хочется, то можно
собрать "анонимный" тип из нескольких экземпляров Either — это такой
встроенный типа вида:

< data Either a b
<   = Left a  -- либо "a"
<   | Right b -- либо "b"

Так, например, `Person extra` из примера выше мог бы быть закодирован в виде

> type Person' extra = Either String (Either Int (Either Int extra))

Здесь `type` означает не описание нового типа и не указание "типа типа", а
описание *псевдонима* (alias) для имеющегося сложного типа. Так вот, этот
`Person'` *изоморфен* описанному выше `Person`, то есть имеет такую же мощность
и всегда можно написать функцию, которая *однозначно* превратит любое
значение одного типа в значение другого и наоборот без потери информации.

Раз уж выше вспомнили про `Either`, то стоит упомянуть и `Maybe`, который
выглядит примерно так:

< data Maybe a = Just a | Nothing

Здесь `Nothing` символизирует отсутствие значения конкретного типа. То есть
информация о вложенном типе `a` сохраняется, даже если значение `Maybe a`
было построено с помощью конструктора `Nothing`:

> noInts :: Maybe Int
> noInts = Nothing

> noBools :: Maybe Bool
> noBools = Nothing

Рассмотрим комплексный пример, добавив Maybe и ещё одну сумму к Person:

> -- питомец
> data Pet
>   = Dog String
>   | Snail
>   | Ants Int

> bobWithAnts :: Person Pet
> bobWithAnts = Person
>   { pName = JustName "Bob"
>   , pAge = 100
>   , pExtra = Ants 13
>   }

> data User a
>   = Registered (Person a)
>   | Guest

Эта функция будет проверять, что пользователь зарегистрирован и имеет хотя бы
сотню муравьёв:

> hasManyAnts :: User (Maybe Pet) -> Bool
> hasManyAnts u = case u of
>   Registered (Person {pExtra = Just (Ants n)})
>     | n >= 100 -> True
>   _            -> False

А вот пример модели системы Kanban:

> data Desk = Desk
>   { boards :: [(BoardName, [Card])]
>     -- ^ список именованных досок со списком карточек на каждой
>   }

> data Card =
>   Card
>   { key :: CardId
>   , title :: String
>   , body :: String
>   , color :: Color Void'
>   , author :: User ()
>   , assignees :: [User ()]
>   }

> data Err -- "ожидаемые исключения" в виде типа
>   = BadCardId CardId
>   | BadBoardName BoardName

> newtype CardId = CardId Integer
> newtype BoardName = BoardName String

Функция переноса карточки с указанным CardId на доску с заданным именем,
принимающая текущее состояние досок и возвращающая новое с учётом возможных
ошибок:

> move :: CardId -> BoardName -> Desk -> Either Err Desk
> move = undefined

newtype
-------

Выше в примере были описаны два типа с помощью ключевого слова `newtype`. Это
особый вид типов с ровно одним конструктором и ровно одним полем. Такие типы
часто выступают в роли типобезопасной обёртки над имеющимся типом, но в
процессе компиляции обёртка стирается. Таким образом достигается "бесплатность
абстракции": на этапе проверки типов каждый newtype выглядит как новый тип
(отсюда и название), но тратить ресурсы на запаковку-распаковку не придётся.
Зато и CardId с обычным Integer перепутать не получится, чего нельзя достичь
использованием псевдонимов (те являются просто новыми именами для старых
типов и увеличивают только читаемость).

Незаселённые типы как метки для фантомных типов
-----------------------------------------------

Рассмотрим ещё один пример: моделирование системы учёта единиц измерения при
проведении вычислений.

Это ядро системы: обёртка над числом, сохраняющая информацию о системе единиц
и имени измеряемой величины.

> newtype MUnit system name = MUnit Float

Обратите внимание, что аргументы конструктора типов не используются на
правой стороне. Такие типы-аргументы, не использующиеся на уровне
конструкторов значений, называются *фантомными типами*.

Так как фантомные типы не используются в качестве полей значений, то могут
быть не заселены и будут выступать только как метки. Опишем несколько таких
меток:

> data Imperial -- имперская система
> data Metric   -- метрическая система (СИ)

> data Distance -- расстояние
> data Time     -- время

> data In a b   -- соотношение "В" двух единиц измерения

Скорость или же "мера расстояния В меру времени", зависит от конкретной системы

> type Speed system = MUnit system (Distance `In` Time)

В метрической системе расстояние измеряется в метрах

> type Meters = MUnit Metric Distance

Ну а секунды они везде секунды

> type Seconds system = MUnit system Time

> speed :: Speed Metric
> speed = MUnit 250 -- м/сек

Обобщённое умножение единиц измерения с сокращением числителя и знаменателя

> mult
>   :: MUnit s (a `In` b)
>   -> MUnit s b
>   -> MUnit s a
> mult (MUnit s) (MUnit t) = MUnit (s * t)

А это уже вычисление пройденного пути с контролем единиц измерения:

> path :: Meters
> path = speed `mult` time
>   where
>     time = MUnit 4 :: Seconds system

Тип `In` можно было бы описать в виде оператора и вообще добавить операторов
на уровне типов, что и позволяет расширение `TypeOperators`. Правда, тут есть
одно ограничение: операторы-типы должны иметь имена, начинающиеся на ":".

> data a :/ b
> infixl 7 :/
> type Acceleration system = Speed system :/ MUnit system Time

infixl тут указывает, что оператор `:/` является лево-ассоциативным и имеет
приоритет "7" (в Haskell приоритеты могут быть от 0 до 9, чем больше, тем выше).
