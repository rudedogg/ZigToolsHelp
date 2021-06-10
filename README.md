# ZigTools

A description of this package.

## Building the ZigSyntaxChecker
```shell
cd Sources/ZigSyntaxChecker/src/
zig build-lib src/main.zig -dynamic --name ZigSyntaxChecker
```

### Build .o
`zig build-obj src/main.zig --name ZigSyntaxChecker`

### Build dynamic library
`zig build-lib src/main.zig -dynamic --name ZigSyntaxChecker`

### Build static library
`zig build-lib src/main.zig --name ZigSyntaxChecker`
