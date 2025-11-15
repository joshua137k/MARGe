### **Evolução do Projeto MARGe: Da Animação de Grafos Reativos a uma Plataforma Integrada de Modelação e Verificação Formal**

O trabalho desenvolvido representa uma expansão significativa dos objetivos inicialmente definidos no plano de trabalho do projeto MARGe. A proposta original de integrar um interpretador de lógica modal foi não apenas cumprida, como amplamente superada, transformando o MARGe de uma simples ferramenta de animação num ambiente robusto para a modelação, simulação e verificação formal de sistemas reconfiguráveis complexos.

As contribuições podem ser organizadas em três eixos fundamentais: a extensão do formalismo base, a introdução de capacidades avançadas de verificação formal e o estabelecimento de mapeamentos formais para outros sistemas de modelação.

#### **1. Extensão do Formalismo de Grafos Reativos**

O poder expressivo do modelo de **Grafos Reativos (RG)** foi ampliado com a introdução de conceitos de **guardas** e **tempo**, dando origem a dois novos formalismos mais ricos.

* **Grafos Reativos Guardados (GRG – Guarded Reactive Graphs):** O formalismo foi estendido para incluir **variáveis de estado** e **guardas** sobre as transições. Neste modelo, a ativação de uma transição depende não só da estrutura do grafo, mas também da satisfação de condições lógicas sobre o estado atual das variáveis. As transições podem ainda induzir **atualizações atómicas** nessas variáveis, permitindo modelar sistemas cujo comportamento é guiado por dados, e não apenas por eventos. Esta extensão é essencial para representar de forma fiel protocolos e sistemas de software complexos.

* **Grafos Reativos Temporizados (Timed Reactive Graphs):** Foi introduzida uma semântica de **tempo contínuo**, inspirada no formalismo dos **Autómatos Temporizados**. Os estados podem conter **invariantes temporais** (condições que devem permanecer verdadeiras enquanto o sistema se encontra nesse estado), e as guardas das transições podem incluir **restrições temporais** baseadas em relógios. Esta extensão permite ao MARGe modelar e analisar **sistemas de tempo real** e **protocolos de comunicação sensíveis ao tempo**.

#### **2. Análise Formal e Verificação de Propriedades**

O objetivo de análise formal foi alcançado com a implementação de um verificador baseado na **Lógica Proposicional Dinâmica (PDL)**, uma lógica modal mais expressiva do que as lógicas modais tradicionais.

Em vez de verificar apenas propriedades estáticas, o verificador PDL permite especificar e analisar propriedades sobre **sequências de execução** (programas). Assim, é possível formular e responder a questões como:

* “É possível, através da execução da sequência de ações `α`, alcançar um estado onde a propriedade `φ` é verdadeira?” (representado por `<α>φ`)
* “Após qualquer execução da sequência de ações `α`, o estado resultante satisfaz necessariamente a propriedade `φ`?” (representado por `[α]φ`)

Esta capacidade acrescenta uma dimensão dinâmica à análise, permitindo a verificação de **alcançabilidade**, **segurança** e **vivacidade** diretamente sobre os modelos. Atualmente, o verificador PDL atua sobre a estrutura lógica e de dados do grafo, ainda sem considerar as restrições temporais.

#### **3. Nova Plataforma de Simulação Interativa e Dinâmica**

A principal limitação do animador original residia na sua natureza estática, baseada na geração de diagramas sequenciais através da biblioteca **Mermaid**. Para suportar a complexidade e a interatividade exigidas pelos novos formalismos (GRG e temporizados), a camada de visualização foi totalmente redesenhada com a biblioteca **Cytoscape.js**. Esta transição representa uma mudança profunda — de uma ferramenta de visualização estática para um **ambiente de simulação interativo** — com as seguintes características:

* **Representação Dinâmica do Grafo:** O grafo é agora um objeto persistente e interativo, atualizado em tempo real. Isto permite um feedback visual instantâneo e uma experiência de utilização mais fluida.
* **Animação Semântica:** As transições são visualizadas diretamente na interface. As arestas e nós relevantes são destacados durante cada passo da execução, tornando a sequência de ações do sistema clara e fácil de seguir, o que facilita a depuração e a compreensão do comportamento do modelo.

#### **4. Mapeamentos Formais e Interoperabilidade**

Para validar os formalismos e potenciar a sua análise com ferramentas externas, foram desenvolvidos **tradutores formais** que estabelecem um mapeamento semântico entre os Grafos Reativos e outros modelos computacionais consolidados.

* **Tradução de RG/GRG para GLTS (Guarded Labeled Transition Systems):** Foi criado um procedimento de compilação que traduz a semântica de reconfiguração dos Grafos Reativos (baseada em hiper-arestas) para um **Sistema de Transição Rotulado e Guardado (GLTS)** equivalente. Nesta tradução, o comportamento dinâmico dos RG é representado através de variáveis de controlo e guardas complexas, demonstrando formalmente como a reconfiguração pode ser simulada num formalismo mais tradicional.

* **Tradução para o Formalismo de UPPAAL:** Um dos avanços mais relevantes foi a capacidade de exportar modelos, incluindo os **Grafos Reativos Temporizados**, para o formato **XML do UPPAAL**, uma das ferramentas mais reconhecidas na verificação de sistemas de tempo real. Esta tradução não é meramente sintática; constitui uma **ponte formal** que permite submeter os modelos criados no MARGe aos algoritmos de *model checking* do UPPAAL para análise exaustiva e automática.

Complementarmente, a interface do MARGe foi redesenhada para facilitar a exploração visual e intuitiva de modelos complexos. A nova interface permite **simulação passo-a-passo**, **animação de transições** e **inspeção em tempo real** das variáveis e relógios, tornando-se uma ferramenta essencial para a depuração e para a compreensão do comportamento dos sistemas modelados.
