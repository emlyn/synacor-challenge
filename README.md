# Synacor Challenge

My attempt at solving the [synacor challenge](https://challenge.synacor.com) using Clojure.

## Codes

The codes that you find are personalised for each downloaded binary, so they won't match
if you compare them to someone else who used a different binary.

If you want to use my binary to solve it yourself (as the website seems to be down),
the hashes of the codes (obtained by `echo code_here | md5`) are:

```text
c1b358b00151e470f4d1b5caa0e651d3
7c76ee3809234edeb4a15798896d33bc
cd5ea467516713c96526a08b001382cd
fc9f3b102a876f3fbfe242969e848b13
564f7a0f42983dd41d7c7628294ec682
ef33b1c906a63c73164a042075854243
1c1b2761984d8e5ef8634465d2aad6f7
008964d9009ed3a09aa163efbf301b44
```

## Running

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
