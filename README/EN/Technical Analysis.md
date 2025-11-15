### **Technical Analysis of Implementations in the MARGe Project**

The work carried out involved the modernization and extension of a formal systems animation tool, applying software engineering principles to create a robust, interactive, and interoperable platform. The main contributions can be divided into four implementation areas: **Interactive Frontend**, **Semantic Backend**, the **Scala.js/JavaScript Communication Bridge**, and the **Integration Modules**.

#### **1. Frontend: A Reactive and Dynamic User Interface**

The most significant change was the complete replacement of the static visualization layer with a dynamic interface built in **JavaScript** using the **Cytoscape.js** library.

* **Rendering Architecture:** Instead of generating a new image (Mermaid) at each step, a visual component lifecycle was implemented.

  * `setupInitialCytoscape`: On initial load, the code extracts node positions calculated by Mermaid (`extractMermaidPositions`) to create a visually pleasing `preset` layout, avoiding abrupt rearrangements.
  * `renderCytoscapeGraph`: On subsequent updates, this function efficiently applies a new JSON dataset (`currentCytoscapeInstance.json(...)`) to the *existing* Cytoscape instance, avoiding DOM recreation and enabling smooth animations.

* **Interactivity and UI State Management:**

  * **Event Handling:** *Event listeners* (`cy.on('tap', ...)` and `cy.on('mouseover', ...)`) capture user interactions on the graph and translate them into backend logic calls.
  * **Visual Feedback:** The interface provides immediate feedback, with animated transitions through CSS classes (`transition-flash`), while the state of nodes/edges (active, disabled, current) is reflected visually.
  * **Dynamic Side Panel (`updateSidePanel`):** Built dynamically in JavaScript at each simulation step, it displays the current state (variables, clocks, active transitions) and includes interactive controls (buttons, checkboxes). All time control logic (`toggleAutoDelay`, `handleDelayClick`) is managed on the frontend.

#### **2. Backend: Extending the Semantic Engine in Scala**

The logical core of the application, implemented in **Scala**, was expanded to support richer formalisms.

* **Data Model Expansion (`Program2.scala`):** The `case class RxGraph` was extended to include:

  * `val_env: Map[QName, Int]` — variable environment.
  * `clocks: Set[QName]`, `clock_env: Map[QName, Double]` — clock management.
  * `invariants: Map[QName, Condition]` — temporal invariants associated with states.
  * `edgeConditions` and `edgeUpdates` — mappings between transitions and their guards/updates.

* **Operational Semantics (`RxSemantics.scala`):**

  * The `nextEdge` function was rewritten to incorporate the new logic, performing:

    1. Filtering of active transitions.
    2. Evaluation of guards (`Condition.evaluate`).
    3. Processing of triggered hyper-edges.
    4. Application of updates (`applyUpdates`).
    5. Verification of invariants (`checkInvariant`).
  * Temporal semantics were implemented through `nextDelay` and time advancement in `CaosConfig2.advanceTime`.

* **PDL Verification Engine (`PdlEvaluator.scala`):** Implements a recursive interpreter for Propositional Dynamic Logic formulas.

  * `evaluateFormula` traverses the syntax tree of a PDL formula, applying logical rules.
  * `evaluateProgram` computes the set of reachable states through a program `α`, handling atomic actions, sequences, choices, and iterations according to the classical dynamic logic paradigm.

#### **3. Communication Bridge: Scala.js ↔ JavaScript Interoperability**

The connection between the Scala backend (compiled to JavaScript) and the frontend is a key architectural component.

* **API Exposure with `@JSExport`:** Methods in the Scala object `CaosConfig2`, such as `takeStep(edgeJson: String)`, `undoStep()`, and `advanceTime(delay: Double)`, are annotated with `@JSExport`, making them accessible in JavaScript (`CaosConfig2.takeStep(...)`).

* **Data Serialization (JSON):**

  * The Scala function `generateSimulationJson` acts as a *serializer*, converting the internal state (`RxGraph`) into a JSON string containing all necessary information: graph structure (`graphElements`), side panel data (`panelData`), and the last transition (`lastTransition`).

* **Backend → Frontend Calls:** Scala invokes JavaScript functions through the Scala.js `global` object. For example, `global.renderCytoscapeGraph(...)` is called after computing the new state to update the visualization.

#### **4. Integration Modules: Translators and Exporters**

The project evolved beyond a monolithic tool, incorporating compilers that map MARGe formalisms to other external tools.

* **RG → GLTS Translator (`MaRGeTranslator.scala`):** Implements a compiler that transforms a Reactive Graph (with dynamic rules) into a Guarded Reactive Graph (GRG), simulating reconfiguration with control variables (`_active`) and conditional guards. This translation preserves the original semantics, flattening reconfiguration to the data level.

* **UPPAAL Exporters (`UppaalConverter.scala`, `UppaalConverter2.scala`):** Code generators that traverse the `RxGraph` and programmatically construct an XML tree matching the UPPAAL `.xta` format.

  * They map RG states to `locations`, transitions to `transitions`, and guards/invariants to *labels* (`guard`, `invariant`).
  * Complex update logic is encapsulated in Uppaal functions, while reconfiguration is encoded via global state arrays, demonstrating deep understanding of the target formalism.

