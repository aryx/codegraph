# codegraph
Source code dependencies visualizer

## Usage for `codegraph`
```sh
$ ./codegraph_build -lang cmt ~/pfff
```

to generate a `graph_code.marshall` file in `~/pfff` containing
all dependency information about the pfff codebase using the
typed bytecode `.cmt` files generated during the compilation of pfff.
```sh
$ ./codegraph ~/pfff
```

This should launch a gtk-based GUI that allows you to visualize
source code dependencies.

