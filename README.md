# S.A.M - Simple Assembler Maschin

This is a simple assmenbler interpreter.

## Specs
There are six register (a to f), 1024 memory cells and a 'unlimited' stack.
The calls are case-insensitive.

``` ASM
mov [register/memory] [register/memory]
add [register] [register/memory]
sub [register] [register/memory]
mul [register] [register/memory]
div [register] [register/memory]
```