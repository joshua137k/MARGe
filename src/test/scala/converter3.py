# -----------------------------------------------------------
# 1. Tamanhos de Array e Constantes
# -----------------------------------------------------------
# A variável 'x' do tipo 'clock' foi omitida, pois não é utilizada no código.
NUM_EDGES = 4
NUM_HYPEREDGES = 5
NUM_IDS = 4
c = 0

# -----------------------------------------------------------
# 2. Definições de Estrutura (emuladas com dicionários)
# -----------------------------------------------------------
# Em Python, podemos usar dicionários ou classes para emular structs do C.
# Dicionários são simples e eficazes para este caso.

# Array de Arestas (A) - Lista de dicionários
A = [
    {"id": 0, "stat": True},  # Índice 0: Chocolate -> Insert : GetChoc
    {"id": 1, "stat": True},  # Índice 1: Coffee -> Insert : GetCoffee
    {"id": 2, "stat": True},  # Índice 2: Insert -> Coffee : ct50
    {"id": 3, "stat": True}   # Índice 3: Insert -> Chocolate : eur1
]

# Array de Hiperarestas (L) - Lista de dicionários
L = [
    {"id": 2, "type": False, "stat": True, "is_edge_target": True, "trg_index": 3},  # Regra '-' afetando o alvo 'eur1'
    {"id": 2, "type": True, "stat": True, "is_edge_target": False, "trg_index": 4}, # Regra '-' afetando o alvo 'lastct50'
    {"id": 3, "type": False, "stat": True, "is_edge_target": True, "trg_index": 2},  # Regra '-' afetando o alvo 'ct50'
    {"id": 3, "type": False, "stat": True, "is_edge_target": True, "trg_index": 3},  # Regra '-' afetando o alvo 'eur1'
    {"id": 2, "type": False, "stat": False, "is_edge_target": False, "trg_index": 2} # Regra 'lastct50' afetando o alvo 'ct50'
]

# -----------------------------------------------------------
# 4. Definição da Função de Atualização
# -----------------------------------------------------------

def update_hyperedges_by_id(edge_id):
    """
    Atualiza as hiperarestas com base em um ID de aresta fornecido.
    """
    for i in range(NUM_HYPEREDGES):
        # Verifica se o ID da hiperaresta corresponde ao ID da aresta e se está ativa
        if L[i]["id"] == edge_id and L[i]["stat"]:
            if L[i]["is_edge_target"]:
                print(i, L[i])
                A[L[i]["trg_index"]]["stat"] = L[i]["type"]
            else:
                # O alvo é uma Hiperaresta (na própria lista L)
                L[L[i]["trg_index"]]["stat"] = L[i]["type"]

# -----------------------------------------------------------
# 5. Exemplo de como testar a função
# -----------------------------------------------------------
'''
print("Estado Inicial de A:")
for index, item in enumerate(A):
    print(f"A[{index}]: {item}")

print("\nEstado Inicial de L:")
for index, item in enumerate(L):
    print(f"L[{index}]: {item}")
'''
# Chamando a função para testar com um ID de aresta específico
test_edge_id = 2
print(f"\n--- Chamando update_hyperedges_by_id com edge_id = {test_edge_id} ---\n")
update_hyperedges_by_id(test_edge_id)
update_hyperedges_by_id(1)
update_hyperedges_by_id(test_edge_id)

print("Estado Final de A:")
for index, item in enumerate(A):
    print(f"A[{index}]: {item}")

print("\nEstado Final de L:")
for index, item in enumerate(L):
    print(f"L[{index}]: {item}")