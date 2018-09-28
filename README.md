# stacked-dag

[![Hackage version](https://img.shields.io/hackage/v/stacked-dag.svg?style=flat)](https://hackage.haskell.org/package/stacked-dag)  [![Build Status](https://travis-ci.org/junjihashimoto/stacked-dag.png?branch=master)](https://travis-ci.org/junjihashimoto/stacked-dag)

Ascii DAG for visualization of dataflow

stacked-dag can show Ascii-DAG(Directed acyclic graph) from a Dot file of graphviz.
Dataflow's direction is from top to bottom.
'o' means a node. A label of the node is shown to the left side.
Other characters are edges of DAG.

A sample of DAG is below.

```
o o    l0,l4
|/
o    l1
|
o    l2
|
o    l3
```

# Usages

Write a Dot file of graphviz.

```
$ cat > sample.dot
digraph graphname {
  0 [label="l0"];
  1 [label="l1"];
  2 [label="l2"];
  3 [label="l3"];
  4 [label="l4"];
  0->1;
  1->2;
  2->3;
  4->1;
}
```

Show ascii DAG by following command.

```
$ stacked-dag dot sample.dot
o o    l0,l4
|/
o    l1
|
o    l2
|
o    l3
```

# Samples

```
$ stacked-dag samples
o o    l0,l1
|/
o o o    l2,l4,l6
|/_/
o    l3
|
o    l5
---
o o    l0,l1
 x
o o    l2,l3
---
o    l0
|\
o o    l1,l2
---
o
|\
o |
|/
o
---
o
|\
o |
| |
o |
|/
o
```

