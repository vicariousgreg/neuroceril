# NeuroCERIL
`neuroceril` includes an implementation of NeuroCERIL, a programmable neural network that implements a causal inference algorithm for imitation learning

## Requirements


* [numpy](http://www.numpy.org/) is required for the base implementation
* [pycuda](http://pypi.org/project/pycuda/) is required to run on GPUs
* [networkx](http://networkx.org/) is required to generate unification test cases
* [matplotlib](http://matplotlib.org/) is required to use built-in plotting of test data

## Installation

1. [Clone or download](https://help.github.com/articles/cloning-a-repository/) this repository into a directory of your choice.
2. Add the `src` sub-directory to your [PYTHONPATH](https://docs.python.org/2/using/cmdline.html#envvar-PYTHONPATH).

## Basic Usage

The ``build_neuroceril.py`` script contains functions for constructing a NeuroCERIL instance and loading it with imitation learning programs.  The ``test_neuroneuroceril.py`` includes an implementation of the causal inference algorithm, and a test function that loads a causal knowledge base and test demonstration.  Causal knowledge base and demonstration files are contained in the ``kb`` and ``demos`` directories, respectively.

Example command to run the ``replace_red_with_spare_1`` demonstration:

```
python3 test_neuroceril.py -t imitate -k kb_il_streamlined -n demo_replace_red_with_spare_1 -v
```

This will run the full neural implementation, which is quite computationally expensive.  To emulate the neural architecture, use the ``-m`` flag:

```
python3 test_neuroceril.py -t imitate -k kb_il_streamlined -n demo_replace_red_with_spare_1 -v -m
```
