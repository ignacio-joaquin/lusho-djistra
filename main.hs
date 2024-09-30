import qualified Data.Map as Map

type Nodo = Int
type Peso = Int
type Arista = (Nodo, Peso)   -- (Destino, Peso)
type Grafo = [(Nodo, [Arista])]

vecinos :: Grafo -> Nodo -> [Arista]
vecinos grafo nodo = case lookup nodo grafo of
    Just aristas -> aristas
    Nothing -> []

inicializarDist :: Nodo -> [Nodo] -> Map.Map Nodo Peso
inicializarDist nodoOrigen nodos = 
  Map.fromList [(nodo, if nodo == nodoOrigen then 0 else maxBound) | nodo <- nodos]

dijkstra :: Grafo -> Nodo -> (Map.Map Nodo Peso, Map.Map Nodo (Maybe Nodo))
dijkstra g a = go (inicializarDist a (map fst g)) Map.empty [a] Map.empty
  where
    go distancias visitados [] predecesores = (distancias, predecesores)  -- No hay más nodos que visitar
    go distancias visitados queue predecesores =
      let nodoActual = minimumBy (\x y -> compare (distancias Map.! x) (distancias Map.! y)) queue
      in if Map.member nodoActual visitados
         then go distancias visitados (delete nodoActual queue) predecesores
         else
           let nuevaDistancia = distancias Map.! nodoActual
               newVisitados = Map.insert nodoActual nuevaDistancia visitados
               vecinosActuales = vecinos g nodoActual
               (distanciasActualizadas, nuevosNodos, nuevosPredecesores) = 
                   foldl (actualizar distancias nodoActual) (distancias, delete nodoActual queue, predecesores) vecinosActuales
           in go distanciasActualizadas newVisitados nuevosNodos nuevosPredecesores

    actualizar distancias nodoActual (dist, queue, predes) (nodoVecino, peso) =
        let nuevaDistancia = (dist Map.! nodoActual) + peso
        in if nuevaDistancia < (dist Map.! nodoVecino)
            then (Map.insert nodoVecino nuevaDistancia dist, nodoVecino : queue, Map.insert nodoVecino (Just nodoActual) predes)
            else (dist, queue, predes)

-- Funciones auxiliares
minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy cmp xs = foldl1 minBy xs
  where minBy x y = if cmp x y == LT then x else y

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:ys) = if x == y then ys else y : delete x ys
