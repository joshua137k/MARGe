

### **Guia Completo para Modelação e Verificação com a Plataforma MARGe**

Este documento serve como um guia abrangente para a utilização da plataforma MARGe. Começaremos por detalhar a sintaxe da linguagem, explicando cada elemento que a compõe. Em seguida, mostraremos como aplicar essa sintaxe para construir modelos práticos, desde sistemas simples a sistemas complexos com estado e tempo. Finalmente, demonstraremos como usar a Lógica Proposicional Dinâmica (PDL) para verificar formalmente as propriedades dos seus modelos.

### 1. A Sintaxe da Linguagem MARGe

O parser do MARGe lê um ficheiro de texto e traduz as suas declarações para a estrutura de dados interna `RxGraph`, que o motor de simulação executa. Para usar a ferramenta, é essencial compreender os blocos de construção que o parser reconhece.

#### 1.1 Declarações Globais

Utilize as seguintes declarações para configurar o estado geral e o ambiente do seu modelo:

| Sintaxe | Exemplo | O que faz |
| :--- | :--- | :--- |
| `init <estado>;` | `init s0;` | Define `s0` como um dos estados iniciais do sistema. |
| `int <var> = <val>;` | `int counter = 0;` | Declara uma variável inteira chamada `counter` com valor inicial `0`. |
| `clock <c1>, <c2>;` | `clock t1, t2;` | Declara um ou mais relógios (`t1`, `t2`), que sempre começam em `0`. |
| `inv <estado>: <cond>;`| `inv locked: t <= 10;` | Associa uma condição de invariante ao estado `locked`. |

*Nomes (`QName`):* Nomes de estados, variáveis e relógios podem ser simples (`s0`) ou qualificados para modularidade (`aut1.s0`), usando `.` ou `/` como separadores.

#### 1.2 Arestas: O Coração da Linguagem

A estrutura fundamental de uma aresta é: `origem <SETA> destino <ATRIBUTOS...>;`

**a) Tipos de Seta (`<SETA>`)**

A seta que você usa define o tipo de relação que está a criar:

| Seta | Significado | Exemplo |
| :--- | :--- | :--- |
| `-->` | **Aresta Simples:** Uma transição de um estado para outro. | `s0 --> s1` |
| `->>` | **Regra de Ativação (ON):** Um evento ativa outro. | `a ->> b` |
| `--!` | **Regra de Desativação (OFF):** Um evento desativa outro. | `a --! b` |
| `--#--`| **Desativação Mútua:** Atalho para `a --! b` e `b --! a`. | `goLeft --#-- goRight` |
| `---->`| **Dependência:** Atalho para `a ->> b` e `b --! b`. | `A.look ----> B.goLeft`|

**b) Atributos da Aresta (`<ATRIBUTOS...>` )**

Após `origem <SETA> destino`, pode adicionar os seguintes atributos opcionais em qualquer ordem:

1.  **Rótulo (Label):**
    *   **Sintaxe:** `: <nome>`
    *   **Exemplo:** `s0 --> s1: a`

2.  **Guarda e/ou Bloco de Atualização:**
    *   **Sintaxe (Inline):** `if (<condição>)`
    *   **Sintaxe (Bloco):** `if (<condição>) then { <atualizações> }`
    *   **Exemplo:** `s1 --> s2: timeout if (t >= 10) then { counter' := 0; }`

3.  **Estado Desativado (Disabled):**
    *   **Sintaxe:** `disabled`
    *   **Exemplo:** `s2 --> s0: reset disabled` (A aresta `reset` é criada, mas começa inativa).

#### 1.3 Expressões (Condições e Atualizações)

**a) Condições:** Usadas em guardas (`if`) e invariantes (`inv`).
*   **Estrutura:** `valor1 <op> valor2`, onde `<op>` pode ser `==`, `!=`, `>`, `<`, `>=`, `<=`.
*   **Lógica:** Condições podem ser combinadas com `AND` e `OR`, usando parênteses `()` para agrupar.
*   **Exemplo:** `if ((t < 10 AND counter > 0) OR emergency == 1)`

**b) Atualizações:** Usadas dentro dos blocos `then { ... }`.
*   **Sintaxe:** `<variável>' := <expressão>` (Note o apóstrofo `'`).
*   **Expressões:** Um número (`c' := 0`), uma variável (`x' := y`), ou uma soma/subtração (`c' := c + 1`).
*   **Regra para Relógios:** A única atualização permitida para um relógio é o reset para zero (`t' := 0`).

---

### 2. Modelação na Prática: Construindo Grafos

Agora, vamos aplicar a sintaxe para construir os diferentes tipos de modelos que o MARGe suporta.

#### 2.1 O Básico: Grafos Reativos Simples (RG)

Um RG modela sistemas onde o comportamento (as transições disponíveis) pode mudar dinamicamente. Para isso, utilizamos arestas simples (`-->`) e regras de reconfiguração (`->>`, `--!`).

**Exemplo: Um Interruptor com Bloqueio**
Ligar (`turn_on`) desativa a própria ação de ligar, que só é reativada ao desligar (`turn_off`).

```plaintext
init S_Off

// Transições básicas (arestas simples)
S_Off --> S_On: turn_on
S_On  --> S_Off: turn_off

// Regras de reconfiguração (hiper-arestas)
turn_on  --! turn_on   // Ao ligar, desativa a ação de ligar
turn_off ->> turn_on   // Ao desligar, reativa a ação de ligar
```

#### 2.2 Adicionando Estado: Grafos Reativos Guardados (GRG)

Um GRG adiciona estado de dados ao modelo. Para isso, utilizamos as declarações `int` e os atributos de aresta `if` e `then {...}`.

**Exemplo: Limitar o Número de Ativações**
O sistema permite a execução da ação `step` no máximo 3 vezes.

```plaintext
int counter = 0
init Ready

// A transição é guardada por uma condição sobre 'counter'
// e atualiza a variável após a sua execução.
Ready --> Ready: step if (counter < 3) then {
  counter' := counter + 1
}
```

#### 2.3 Introduzindo o Tempo: Grafos Reativos Temporizados

Para sistemas com restrições de tempo, utilizamos as declarações `clock` e `inv`, além de condições e atualizações temporais.

**Exemplo: Um Mecanismo de Timeout**
Após iniciar, o sistema deve ser finalizado em 10 unidades de tempo, caso contrário, um `timeout` é forçado.

```plaintext
clock t
init Idle

// A invariante força uma saída do estado 'Working' se t > 10
inv Working: t <= 10

// Inicia o processo e reseta o relógio
Idle --> Working: start then {
  t' := 0
}

// A ação de finalizar só pode ocorrer antes de t=10 (devido à invariante)
Working --> Idle: finish

// Se o tempo atingir 10, a única transição possível é o timeout
Working --> Idle: timeout if (t >= 10)
```

---

### 3. Verificação de Propriedades com Lógica Proposicional Dinâmica (PDL)

Após modelar o seu sistema, pode usar a análise PDL para verificar formalmente o seu comportamento. A PDL permite fazer perguntas complexas sobre as sequências de execução do seu modelo.

#### 3.1 A Estrutura dos Programas `α` em PDL

O poder da PDL reside na sua capacidade de descrever sequências de execução (`α`) dentro das suas modalidades. Estes programas são construídos da seguinte forma:

*   **Ação Atómica (`Act`):** O nome de uma ação. Ex: `turn_on`.
*   **Sequência (`Seq`):** Dois programas em sequência. Ex: `step ; step`.
*   **Escolha Não-Determinística (`Choice`):** Uma alternativa entre programas. Ex: `go_left + go_right`.
*   **Iteração (`Star`):** Um programa executado zero ou mais vezes. Ex: `step*`.

#### 3.2 Sintaxe e Exemplos Práticos de Fórmulas PDL

Na interface do MARGe, insira o estado inicial e a fórmula a ser verificada.

*   **Modalidade `<α>φ` (Possibilidade):** "É **possível** executar a sequência `α` e alcançar um estado onde `φ` é verdadeiro?"
*   **Modalidade `[α]φ` (Necessidade):** "**Toda** execução da sequência `α` leva **necessariamente** a um estado onde `φ` é verdadeiro?"

**Exemplos Práticos:**

1.  **Alcançabilidade (usando `Act`):**
    *   No exemplo do Interruptor (a partir de `S_Off`):
    *   **Fórmula:** ` <turn_on>S_On `
    *   **Resultado:** `true`. É possível executar `turn_on` e chegar a `S_On`.

2.  **Verificação de Reconfiguração (usando `Seq`):**
    *   No exemplo do Interruptor (a partir de `S_On`):
    *   **Fórmula:** ` [turn_off]<turn_on>S_On `
    *   **Resultado:** `true`. Após *qualquer* execução de `turn_off`, é *sempre* possível executar `turn_on`.

3.  **Verificação de Condições (usando `Seq` e Proposições de Guarda):**
    *   No exemplo do Contador (a partir de `Ready`):
    *   **Fórmula:** ` <step;step;step>[counter == 3] `
    *   **Resultado:** `true`. É possível executar `step` três vezes e chegar a um estado onde a condição `counter == 3` é verdadeira.

4.  **Verificação de Bloqueio (Deadlock):**
    *   No exemplo do Contador (a partir de `Ready`):
    *   **Fórmula:** ` <step;step;step;step>true `
    *   **Resultado:** `false`. Não é possível executar `step` quatro vezes, pois a guarda falha.

5.  **Verificação de Alternativas (usando `Choice`):**
    *   Imagine um modelo com um estado `Center` e duas transições: `Center --> Left: move_left` e `Center --> Right: move_right`. A partir de `Center`:
    *   **Fórmula:** ` <move_left + move_right>true `
    *   **Significado:** "É possível executar `move_left` OU `move_right`?" (Esta é uma forma útil de verificar que o sistema não está num estado de deadlock se houver alternativas).
    *   **Resultado:** `true` (assumindo que pelo menos uma das transições está ativa).