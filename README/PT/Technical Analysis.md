### **Análise Técnica das Implementações no Projeto MARGe**

O trabalho realizado consistiu na modernização e extensão de uma ferramenta de animação de sistemas formais, aplicando princípios de engenharia de software para criar uma plataforma robusta, interativa e interoperável. As principais contribuições podem ser divididas em quatro áreas de implementação: **Frontend Interativo**, **Backend Semântico**, a **Ponte de Comunicação Scala.js/JavaScript**, e os **Módulos de Integração**.

#### **1. Frontend: Uma Interface de Utilizador Reativa e Dinâmica**

A mudança mais significativa foi a substituição completa da camada de visualização estática por uma interface dinâmica construída em **JavaScript** com a biblioteca **Cytoscape.js**.

* **Arquitetura de Renderização:** Em vez de gerar uma nova imagem (Mermaid) a cada passo, foi implementado um ciclo de vida de componentes visuais.

  * `setupInitialCytoscape`: No carregamento inicial, o código extrai as posições dos nós calculadas pelo Mermaid (`extractMermaidPositions`) para criar um layout `preset` visualmente agradável, evitando um rearranjo brusco.
  * `renderCytoscapeGraph`: Nas atualizações seguintes, esta função aplica de forma eficiente um novo conjunto de dados JSON (`currentCytoscapeInstance.json(...)`) à instância *existente* do Cytoscape, evitando recriar o DOM e permitindo animações suaves.

* **Interatividade e Gestão de Estado na Interface:**

  * **Gestão de Eventos:** Foram adicionados *event listeners* (`cy.on('tap', ...)` e `cy.on('mouseover', ...)`) que capturam interações do utilizador no grafo e as traduzem em chamadas para a lógica de backend.
  * **Feedback Visual:** A interface fornece resposta imediata, com transições animadas através de classes CSS (`transition-flash`), e o estado dos nós/arestas (ativo, desativado, atual) é refletido visualmente.
  * **Painel Lateral Dinâmico (`updateSidePanel`):** Construído dinamicamente em JavaScript a cada passo da simulação, mostra o estado atual (variáveis, relógios, transições ativas) e inclui controlos interativos (botões, *checkboxes*). Toda a lógica de controlo temporal (`toggleAutoDelay`, `handleDelayClick`) é gerida no frontend.

#### **2. Backend: Extensão do Motor Semântico em Scala**

O núcleo lógico da aplicação, implementado em **Scala**, foi ampliado para suportar formalismos mais expressivos.

* **Expansão do Modelo de Dados (`Program2.scala`):** A `case class RxGraph` foi estendida para incluir:

  * `val_env: Map[QName, Int]` — ambiente de variáveis.
  * `clocks: Set[QName]`, `clock_env: Map[QName, Double]` — gestão de relógios.
  * `invariants: Map[QName, Condition]` — invariantes temporais associadas a estados.
  * `edgeConditions` e `edgeUpdates` — mapeamentos entre transições e as suas guardas/atualizações.

* **Semântica Operacional (`RxSemantics.scala`):**

  * A função `nextEdge` foi reescrita para incluir a nova lógica, executando:

    1. Filtragem de transições ativas.
    2. Avaliação de guardas (`Condition.evaluate`).
    3. Processamento das hiper-arestas.
    4. Aplicação das atualizações (`applyUpdates`).
    5. Verificação de invariantes (`checkInvariant`).
  * Foi implementada a semântica temporal com `nextDelay` e o avanço do tempo em `CaosConfig2.advanceTime`.

* **Motor de Verificação PDL (`PdlEvaluator.scala`):** Implementa um interpretador recursivo para fórmulas de Lógica Proposicional Dinâmica.

  * `evaluateFormula` percorre a árvore sintática da fórmula, aplicando regras lógicas.
  * `evaluateProgram` calcula o conjunto de estados alcançáveis através de um programa `α`, tratando ações atómicas, sequências, escolhas e iterações, segundo o padrão clássico das lógicas dinâmicas.

#### **3. Ponte de Comunicação: Interoperabilidade Scala.js ↔ JavaScript**

A ligação entre o backend Scala (compilado em JavaScript) e o frontend é um elemento fundamental da arquitetura.

* **API Exposta com `@JSExport`:** Métodos no objeto Scala `CaosConfig2`, como `takeStep(edgeJson: String)`, `undoStep()` e `advanceTime(delay: Double)`, foram anotados com `@JSExport`, tornando-se acessíveis no JavaScript (`CaosConfig2.takeStep(...)`).

* **Serialização de Dados (JSON):**

  * A função `generateSimulationJson` em Scala atua como *serializer*, convertendo o estado interno (`RxGraph`) numa string JSON com toda a informação necessária: estrutura do grafo (`graphElements`), dados do painel (`panelData`) e a última transição (`lastTransition`).

* **Chamadas Backend → Frontend:** O Scala invoca funções JavaScript via o objeto `global` do Scala.js. Por exemplo, `global.renderCytoscapeGraph(...)` é usado após o cálculo do novo estado para atualizar a visualização.

#### **4. Módulos de Integração: Tradutores e Exportadores**

O projeto evoluiu além de uma ferramenta monolítica, passando a incluir compiladores que traduzem os formalismos do MARGe para outras ferramentas.

* **Tradutor RG → GLTS (`MaRGeTranslator.scala`):** Implementa um compilador que transforma um Grafo Reativo (com regras dinâmicas) num Grafo Reativo Guardado (GRG), simulando a reconfiguração com variáveis de controlo `_active` e guardas condicionais. Preserva a semântica original, reduzindo a reconfiguração ao nível dos dados.

* **Exportadores UPPAAL (`UppaalConverter.scala`, `UppaalConverter2.scala`):** Geradores de código que percorrem o `RxGraph` e constroem uma árvore XML correspondente ao formato `.xta` do UPPAAL.

  * Mapeiam estados RG para `locations`, transições para `transitions`, e guardas/invariantes para *labels* (`guard`, `invariant`).
  * Lógicas complexas de atualização são encapsuladas em funções Uppaal, e a reconfiguração é representada com arrays globais, demonstrando um domínio profundo do formalismo alvo.

---

Em síntese, do ponto de vista técnico, o projeto demonstra competências de desenvolvimento *full-stack* num ecossistema especializado (**Scala.js**), com uma arquitetura bem estruturada, gestão explícita de estado e implementação de algoritmos avançados de semântica formal e compilação.
