import qualified Data.Map as Map
import qualified Data.Heap as Heap

type Nodo = Int
type Peso = Int
type Arista = (Nodo, Peso)   -- (Destino, Peso)
type Grafo = [(Nodo, [Arista])]
type ColaPrioridad = H.MinHeap (Peso, Nodo)  -- (distancia, nodo)

vecinos :: Grafo -> Nodo -> [Arista]
vecinos grafo nodo = case lookup nodo grafo of
    Just aristas -> aristas
    Nothing -> []


inicializar :: Nodo -> [Nodo] -> (Map.Map Nodo Peso, ColaPrioridad)
inicializar nodoOrigen nodos = 
  let distancias = Map.fromList [(nodo, if nodo == nodoOrigen then 0 else maxBound) | nodo <- nodos]
      cola = H.singleton (0, nodoOrigen)  -- Comienza con el nodo origen
  in (distancias, cola)



dijkstra :: Grafo -> Nodo -> Nodo -> 
dijkstra g a b
    let distancias = iniciarDistancias g a
    let predecesore = empty
    let visitados = []
