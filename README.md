What?
_____

A reimplementation of a Lego EV3 LMS assembler.
Virtual Machine and instructions [are documented here](https://education.lego.com/en-gb/support/mindstorms-ev3/developer-kits)

Why?
____

Just for fun and mostly to look into parsing.
Better implementations exist: [https://github.com/ev3dev/lmsasm](https://github.com/ev3dev/lmsasm)

Notes
_____

Syntax is *not* compatible with original LMS assembler.
Instruction names are the same, albeit in lowercase and without 'op' prefix.
Self-explanatory code is attached in [test.lms](test.lms) This assembler allows for function calls and global variables and all that, but recursion does not seem to be supported by a VM.

Variable types are byte|half|word|string, with strings having length specifier.
Function arguments must be prepended with keywords "in" and "out". No floating point type, but word _should_ work just fine since virtual machine does not store type information nor does it have separate space for FPU.

No semantical checks are done -- you're on your own. Error messages are shabby as well.


