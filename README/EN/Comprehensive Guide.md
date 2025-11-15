### **Comprehensive Guide to Modeling and Verification with the MARGe Platform**

This document serves as a complete guide for using the MARGe platform. It begins by detailing the syntax of the modeling language, explaining each of its components. Then, it demonstrates how to apply this syntax to construct practical models, ranging from simple systems to complex ones with state and timing. Finally, it shows how to use **Propositional Dynamic Logic (PDL)** to formally verify the properties of your models.

### 1. The Syntax of the MARGe Language

The MARGe parser reads a text file and translates its declarations into the internal data structure `RxGraph`, which the simulation engine executes. To use the tool effectively, it is essential to understand the basic building blocks that the parser recognizes.

#### 1.1 Global Declarations

Use the following declarations to configure the general state and environment of your model:

| Syntax                 | Example                | Description                                                          |
| :--------------------- | :--------------------- | :------------------------------------------------------------------- |
| `init <state>;`        | `init s0;`             | Defines `s0` as one of the initial states of the system.             |
| `int <var> = <val>;`   | `int counter = 0;`     | Declares an integer variable named `counter` with initial value `0`. |
| `clock <c1>, <c2>;`    | `clock t1, t2;`        | Declares one or more clocks (`t1`, `t2`), which always start at `0`. |
| `inv <state>: <cond>;` | `inv locked: t <= 10;` | Associates a time invariant condition with state `locked`.           |

*Names (`QName`):* Names of states, variables, and clocks can be simple (`s0`) or qualified for modularity (`aut1.s0`), using `.` or `/` as separators.

#### 1.2 Edges: The Core of the Language

The fundamental structure of an edge is: `source <ARROW> target <ATTRIBUTES...>;`

**a) Arrow Types (`<ARROW>`)**

The arrow you use defines the type of relationship you are creating:

| Arrow   | Meaning                                                        | Example                 |
| :------ | :------------------------------------------------------------- | :---------------------- |
| `-->`   | **Simple Edge:** A transition from one state to another.       | `s0 --> s1`             |
| `->>`   | **Activation Rule (ON):** An event activates another.          | `a ->> b`               |
| `--!`   | **Deactivation Rule (OFF):** An event deactivates another.     | `a --! b`               |
| `--#--` | **Mutual Deactivation:** Shortcut for `a --! b` and `b --! a`. | `goLeft --#-- goRight`  |
| `---->` | **Dependency:** Shortcut for `a ->> b` and `b --! b`.          | `A.look ----> B.goLeft` |

**b) Edge Attributes (`<ATTRIBUTES...>` )**

After `source <ARROW> target`, you can add the following optional attributes in any order:

1. **Label:**

   * **Syntax:** `: <name>`
   * **Example:** `s0 --> s1: a`

2. **Guard and/or Update Block:**

   * **Inline Syntax:** `if (<condition>)`
   * **Block Syntax:** `if (<condition>) then { <updates> }`
   * **Example:** `s1 --> s2: timeout if (t >= 10) then { counter' := 0; }`

3. **Disabled Edge:**

   * **Syntax:** `disabled`
   * **Example:** `s2 --> s0: reset disabled` (The edge `reset` is created but starts inactive.)

#### 1.3 Expressions (Conditions and Updates)

**a) Conditions:** Used in guards (`if`) and invariants (`inv`).

* **Structure:** `value1 <op> value2`, where `<op>` can be `==`, `!=`, `>`, `<`, `>=`, `<=`.
* **Logic:** Conditions can be combined with `AND` and `OR`, using parentheses `()` for grouping.
* **Example:** `if ((t < 10 AND counter > 0) OR emergency == 1)`

**b) Updates:** Used inside `then { ... }` blocks.

* **Syntax:** `<variable>' := <expression>` (note the apostrophe `'`).
* **Expressions:** A number (`c' := 0`), a variable (`x' := y`), or an arithmetic operation (`c' := c + 1`).
* **Clock Rule:** The only valid update for a clock is resetting it to zero (`t' := 0`).

---

### 2. Practical Modeling: Building Graphs

Now let’s apply the syntax to construct different types of models supported by MARGe.

#### 2.1 The Basics: Simple Reactive Graphs (RG)

An RG models systems whose behavior (available transitions) can dynamically change. We use simple edges (`-->`) and reconfiguration rules (`->>`, `--!`).

**Example: A Switch with Locking Mechanism**
Turning on (`turn_on`) deactivates the action itself, which can only be reactivated by turning off (`turn_off`).

```plaintext
init S_Off

// Basic transitions (simple edges)
S_Off --> S_On: turn_on
S_On  --> S_Off: turn_off

// Reconfiguration rules (hyper-edges)
turn_on  --! turn_on   // When turned on, disables the 'turn_on' action
turn_off ->> turn_on   // When turned off, reactivates the 'turn_on' action
```

#### 2.2 Adding State: Guarded Reactive Graphs (GRG)

A GRG adds data state to the model, using `int` declarations and edge attributes `if` and `then {...}`.

**Example: Limiting the Number of Activations**
The system allows the `step` action to be executed at most 3 times.

```plaintext
int counter = 0
init Ready

Ready --> Ready: step if (counter < 3) then {
  counter' := counter + 1
}
```

#### 2.3 Introducing Time: Timed Reactive Graphs

For systems with time constraints, use `clock` and `inv` declarations, as well as temporal conditions and updates.

**Example: A Timeout Mechanism**
After starting, the system must finish within 10 time units; otherwise, a `timeout` is triggered.

```plaintext
clock t
init Idle

inv Working: t <= 10

Idle --> Working: start then {
  t' := 0
}

Working --> Idle: finish
Working --> Idle: timeout if (t >= 10)
```

---

### 3. Property Verification with Propositional Dynamic Logic (PDL)

After modeling your system, you can use PDL analysis to formally verify its behavior. PDL allows you to ask complex questions about the execution sequences of your model.

#### 3.1 The Structure of Programs `α` in PDL

The power of PDL lies in its ability to describe **execution sequences** (`α`) within its modalities. These programs are built as follows:

* **Atomic Action (`Act`):** The name of an action. Ex: `turn_on`.
* **Sequence (`Seq`):** Two programs executed in sequence. Ex: `step ; step`.
* **Non-Deterministic Choice (`Choice`):** An alternative between programs. Ex: `go_left + go_right`.
* **Iteration (`Star`):** A program executed zero or more times. Ex: `step*`.

#### 3.2 Syntax and Practical Examples of PDL Formulas

In the MARGe interface, enter the initial state and the formula to be verified.

* **Modality `<α>φ` (Possibility):** “Is it **possible** to execute the sequence `α` and reach a state where `φ` holds?”
* **Modality `[α]φ` (Necessity):** “Does **every** execution of the sequence `α` necessarily lead to a state where `φ` holds?”

**Practical Examples:**

1. **Reachability (using `Act`):**

   * Example: Switch model (starting from `S_Off`)
   * **Formula:** `<turn_on>S_On`
   * **Result:** `true` — It is possible to execute `turn_on` and reach `S_On`.

2. **Reconfiguration Verification (using `Seq`):**

   * Example: Switch model (starting from `S_On`)
   * **Formula:** `[turn_off]<turn_on>S_On`
   * **Result:** `true` — After any execution of `turn_off`, it is always possible to execute `turn_on`.

3. **State Condition Verification (using `Seq` and Guards):**

   * Example: Counter model (starting from `Ready`)
   * **Formula:** `<step;step;step>[counter == 3]`
   * **Result:** `true` — It is possible to execute `step` three times and reach a state where `counter == 3`.

4. **Deadlock Verification:**

   * Example: Counter model (starting from `Ready`)
   * **Formula:** `<step;step;step;step>true`
   * **Result:** `false` — It is not possible to execute `step` four times, as the guard fails.

5. **Alternative Paths (using `Choice`):**

   * Example: A model with a state `Center` and two transitions — `Center --> Left: move_left` and `Center --> Right: move_right`.
   * **Formula:** `<move_left + move_right>true`
   * **Meaning:** “Is it possible to execute `move_left` OR `move_right`?” — useful for checking that the system is not deadlocked if alternatives exist.
   * **Result:** `true` (assuming at least one transition is active).
