### **Evolution of the MARGe Project: From Reactive Graph Animation to an Integrated Platform for Modeling and Formal Verification**

The work developed represents a significant expansion of the objectives originally defined in the MARGe project plan. The initial proposal to integrate a modal logic interpreter was not only achieved but also greatly surpassed, transforming MARGe from a simple animation tool into a robust environment for modeling, simulation, and formal verification of complex reconfigurable systems.

The contributions can be organized into three main areas: the extension of the core formalism, the introduction of advanced formal verification capabilities, and the establishment of formal mappings to other modeling systems.

#### **1. Extension of the Reactive Graph Formalism**

The expressive power of the **Reactive Graph (RG)** model was expanded with the introduction of **state** and **time** concepts, leading to two new, more expressive formalisms.

* **Guarded Reactive Graphs (GRG):** The formalism was extended to include **state variables** and **guards** on transitions. In this model, the activation of a transition depends not only on the graph’s structural configuration but also on logical conditions over the current state of the variables. Transitions can also induce **atomic updates** to these variables, enabling the modeling of systems whose behavior depends on data as well as events. This extension is essential for faithfully representing complex software systems and protocols.

* **Timed Reactive Graphs (TRG):** A continuous-time semantics was introduced, inspired by the formalism of **Timed Automata**. States can include **temporal invariants** (conditions that must remain true while the system stays in that state), and transition guards can include **time constraints** based on clocks. This extension enables MARGe to model and analyze **real-time systems** and **time-sensitive communication protocols**.

#### **2. Formal Analysis and Property Verification**

The goal of formal analysis was achieved through the implementation of a verifier based on **Propositional Dynamic Logic (PDL)**, a modal logic that is more expressive than standard modal logics.

Rather than checking only static properties of states, the PDL verifier allows for the specification and verification of properties over **execution sequences** (programs). It is thus possible to formulate and answer questions such as:

* “Is it possible, through the execution of an action sequence `α`, to reach a state where the property `φ` holds?” (represented as `<α>φ`)
* “After any execution of the action sequence `α`, does the resulting state necessarily satisfy the property `φ`?” (represented as `[α]φ`)

This capability adds a dynamic dimension to analysis, allowing the verification of **reachability**, **safety**, and **liveness** properties directly on the models. Currently, the PDL verifier operates on the logical and data structure of the graph, not yet incorporating temporal constraints.

#### **3. A New Interactive and Dynamic Simulation Platform**

The main limitation of the original animator was its static nature, based on generating sequential diagrams using the **Mermaid** library. To support the complexity and interactivity required by the new formalisms (GRG and TRG), the visualization layer was completely redesigned using **Cytoscape.js**. This transition represents a major paradigm shift—from a static visualization tool to an **interactive simulation environment**—with the following key features:

* **Dynamic Graph Representation:** The graph is now a persistent and interactive object, updated in real time. This allows instant visual feedback and a smoother user experience.
* **Semantic Animation:** Transitions are visually animated in the interface. Relevant edges and nodes are highlighted during each execution step, making the system’s behavior explicit and easy to follow, which facilitates debugging and model comprehension.

#### **4. Formal Mappings and Interoperability**

To validate the formalisms and enable their analysis using external tools, **formal translators** were developed to establish semantic mappings between Reactive Graphs and other well-established computational models.

* **Translation from RG/GRG to GLTS (Guarded Labeled Transition Systems):** A compilation procedure was created that translates the reconfiguration semantics of Reactive Graphs (based on hyperedges) into an equivalent **Guarded Labeled Transition System (GLTS)**. In this translation, the dynamic behavior of RGs is encoded through control variables and complex guards, formally demonstrating how reconfiguration can be simulated in a more traditional framework.

* **Translation to the UPPAAL Formalism:** One of the most significant developments is the ability to export models—including **Timed Reactive Graphs**—to **UPPAAL’s XML format**, one of the most recognized tools for real-time system verification. This translation is not merely syntactic; it forms a **formal bridge** that allows MARGe models to be analyzed using UPPAAL’s powerful model-checking algorithms for exhaustive and automatic analysis.

Additionally, the MARGe interface was redesigned to support intuitive exploration of complex models. The new interface enables **step-by-step simulation**, **transition animation**, and **real-time inspection** of variable and clock states, making it an indispensable tool for debugging and for building intuition about system behavior.

---

In summary, the MARGe project has evolved from a simple graph animator into a **comprehensive environment for modeling and formal verification**, capable of representing, simulating, and analyzing reconfigurable, guarded, and timed systems—establishing itself as an essential tool for research and education in reactive and timed systems.
