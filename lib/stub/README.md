The shared stub, referenced by the root flake and by every partition.

A `follows` cannot cross a flake boundary, so a layer that refers to a name it
does not own must still declare that name. It does NOT have to declare the real
input: it declares this directory instead. That costs no fetch when the layer is
locked and records no rev, so there is no second pin that can drift from the
owner's.

`lib/inputs.nix` replaces each stub node with the value from whichever source
owns that name, keyed by the input name -- nix keys a root input's lock node by
name, not by content, so every input pointing here still gets its own node.

Nothing ever reads the contents of this directory.
