<h1>Code Wars</h1>

This repository contains several different projects and also 
solutions to some Codewars exercises.

<h2>C-like Compiler</h2>

C-like language to Assembly language (register-based) compiler.</br> 

NOTE:
<ul>
  <li>No static type system</li>
  <li>Assembly language runs on a simulator</li> 
</ul>
<h3>Problem Statement</h3>

Build a compiler that takes a text file in C source code, returns the text 
file in Assembly language and then feed the result in the Assembly interpreter
to check if results of the compiled program match the expected ones (assembly
program takes some expected arguments and returns expected value)

<h3>Source Language Definition (C-like)</h3>

    function   ::= '[' arg-list ']' expression
    
    arg-list   ::= /* nothing */
                 | variable arg-list

    expression ::= term
                 | expression '+' term
                 | expression '-' term

    term       ::= factor
                 | term '*' factor
                 | term '/' factor

    factor     ::= number
                 | variable
                 | '(' expression ')'

<h3>Target Language Definition (Assembly)</h3>

    "IM n"     // load the constant value n into R0
    "AR n"     // load the n-th input argument into R0
    "SW"       // swap R0 and R1
    "PU"       // push R0 onto the stack
    "PO"       // pop the top value off of the stack into R0
    "AD"       // add R1 to R0 and put the result in R0
    "SU"       // subtract R1 from R0 and put the result in R0
    "MU"       // multiply R0 by R1 and put the result in R0
    "DI"       // divide R0 by R1 and put the result in R0

<h3>How It Works</h3>

Text is parsed into a so-called Abstract Syntax Tree which is then simplified
into a secondary AST using some C language rules (equalities, etc.).
This is phase 1. </br>
Secondary AST is then compiled into assembly language in phase 2 using translation
table for instructions. <br/>
Functions in the compiler must operate on the tree data structure with various
types of nodes. Tree is a recursive data structure and there are 3 ways to
write functions that operate on trees:
1. Direct recursion (normally the first implementation)
2. Build recursive higher-order functions like fold or fold-map and use them throughout the compiler
3. Not writing any recursive functions and rather use F-algebras (or recursion schemes)

<h3>Recursion Schemes</h3>
The last rewrite of the compiler was done using recursion schemes - more specifically Catamorphism.
Therefore the entire declaration of the program has no recursion at all. Only the evaluation itself 
is of course recursive. Very good explanation on this topic is by Bartosz Milewski's Programming Cafe:
https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/

<h3>Theory In Practise</h3>
The implementation of the compiler with Catamorphism is not efficient but it's a very nice way
to demonstrate how theory can be applied in practise. There are other places where this technique 
can be applied even for production use cases for instance building DSLs.


<h2>Other Projects</h2>

Will add the description..
