# Synacor Challenge

My attempt at solving the [synacor challenge](https://challenge.synacor.com) using Clojure.

Run it with:

```bash
clj -M:run exec [filename]
```

To execute the filename in the VM (filename defaults to `challenge.bin`)

You can also use:

```bash
clj -M:run debug [filename]
```

To step through the execution one instruction at a time, or

```bash
clj -M:run disasm [filename]
```

To print out a disassembly of the file.
