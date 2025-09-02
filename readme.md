# Animator of Multi Action Reactive Graphs (MARGe)

<!-- Experiments in https://dtinas10.github.io/MARGe/lib/caos/tool/index.html -->
Run a snapshot of this tool in https://fm-dcc.github.io/MARGe

Tutorial Video: https://www.dropbox.com/scl/fo/cm0tw42zlebqqzh7s054a/h?rlkey=urd0z5ern6akgkc3l8dqq8l7c&dl=0

# Caos

This project uses and the Caos's framework, placed at `lib/caos`. More information on it can be found online:

 - Caos' GitHub page: https://github.com/arcalab/CAOS
 - Caos' tutorial: https://arxiv.org/abs/2304.14901
 - Caos' demo video: https://youtu.be/Xcfn3zqpubw 

The project can also be included as a submodule, as explained in the documentation of Caos.

## Requirements

- JVM (>=1.8)
- sbt

## Compilation

You need to get the submodules dependencies (CAOS library), and later compile using ScalaJS, following the steps below.
The result will be a JavaScript file that is already being imported by an existing HTML file. 

1. `git submodule update --init`
2. `sbt fastLinkJS` obs jdk17
3. open the file `lib/tool/index.html`


my compile `sbt -java-home "C:\Program Files\Java\jdk-17" fastLinkJS`
