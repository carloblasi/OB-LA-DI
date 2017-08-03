# OB-LA-DI
Compiler for the OB-LA-DI programming language. It is written entirely in Scala and uses the NASM assembler to compile the generated assembly code.

## Dependencies
In order to run, this software requires the following dependencies:
* [Java](https://www.java.com/)
* [NASM](http://www.nasm.us/)

## Usage
Move `ob-la-di.jar` and `ob-la-di` found in the `release` folder of this project to `/usr/local/bin`.
Then simply call `ob-la-di` from the terminal as such:
```
ob-la-di path_to_the_file
```
This will compile and execute (if no error occurs) the file passed as argument. The final executable file can be found in the `home` directory.

## Compatibility
This compiler works only on Mac OS X computers with Intel processor (almost every Apple computer built from 2007 until now).
