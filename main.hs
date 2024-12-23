import Data.Set (Set)
import qualified Data.Set as Set

-- Типы для отношений
type Entity = String
type Relation = (Entity, Entity)

-- Структура данных для системы
data CausalDualSystem = CausalDualSystem
  { causalRelations :: Set Relation
  , dualRelations   :: Set Relation
  } deriving (Show)

-- Функция для создания пустой системы
emptySystem :: CausalDualSystem
emptySystem = CausalDualSystem Set.empty Set.empty

-- Добавление причинно-следственной связи
addCausalRelation :: Entity -> Entity -> CausalDualSystem -> CausalDualSystem
addCausalRelation a b system
  | a == b = system -- Исключаение рефлексивных связей
  | (b, a) `Set.member` causalRelations system = error "Антисимметрия нарушена"
  | otherwise = system { causalRelations = Set.insert (a, b) (causalRelations system) }

-- Проверка транзитивности причинно-следственных связей
checkCausality :: CausalDualSystem -> Bool
checkCausality system =
  all (\(a, b) -> all (\(b', c) -> b == b' ==> Set.member (a, c) (causalRelations system))
       (Set.toList $ causalRelations system))
      (Set.toList $ causalRelations system)
  where
    (==>) x y = not x || y -- Импликация

-- Добавление дуалистической связи
addDualRelation :: Entity -> Entity -> CausalDualSystem -> CausalDualSystem
addDualRelation a b system
  | a == b = system
  | otherwise = system
      { dualRelations = Set.insert (a, b) $
                        Set.insert (b, a) (dualRelations system) }

checkDualism :: CausalDualSystem -> Bool
checkDualism system = all symmetric (Set.toList $ dualRelations system)
  where
    symmetric (a, b) = Set.member (b, a) (dualRelations system)

main :: IO ()
main = do
  let system = emptySystem
      system' = addCausalRelation "A" "B" $
                addCausalRelation "B" "C" $
                addDualRelation "X" "Y" $
                addDualRelation "Y" "Z" system
  putStrLn $ "Система: " ++ show system'
  putStrLn $ "Транзитивность причинно-следственных связей: " ++ show (checkCausality system')
  putStrLn $ "Симметрия дуализма: " ++ show (checkDualism system')
