# S.A.M - Simple Assembler Maschin

This is a simple assmenbler interpreter.

## Specs
There are six register (a to f), 1024 memory cells and a 'unlimited' stack.
The calls are case-insensitive.

``` ASM
mov 	[register]  [register/memory/const]
mov 	[memory]    [const/register]
add 	[register]  [register/memory/const]
sub 	[register]  [register/memory/const]
mul 	[register]  [register/memory/const]
div 	[register]  [register/memory/const]
and	    [register]  [register/memory/const]
or      [register]  [register/memory/const]
cmp 	[register]  [register/memory/const]
jmp 	[label/line]
push 	[register]
pop 	[register]
halt
```

never forget a `halt` at the end of your programm.
