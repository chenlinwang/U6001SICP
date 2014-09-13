# Mistakes in Original Files of Project 4

## Code Error
My `mit-scheme` version:
>  Release 9.2 || Microcode 15.3 || Runtime 15.7 || SF 4.41 || LIAR/C 4.118 || Edwin 3.116

### Container Class, HAVE-THING? Method
The code should be:
```scheme
(lambda (thing) (memq thing things))
```


## Text Error

### all-people
Page 22, Computer Exercise 3, the `all-people` procedure is in the file `objsys.scm`, line 261.
