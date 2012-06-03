Just a bit of code to test some persistent / functional data structure
libraries in Common Lisp

So far, just some helper functions to test maps from these libraries:

* x.fdatatypes
* fset
* funds

Preliminary results seem to show that x.fdatatypes is the fastest
and most efficient (memory-wise) at building maps, while fset is a bit
slower and funds is quite a bit slower and takes more memory.
