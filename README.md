# asm65
asm65 - Macro Cross Assembler for the 6502 processor
WARNING - THIS IS UNRELEASED AND VERY MUCH WORK IN PROGRESS

#### Synopsis
asm65 is a command line tool that allows the cross assembly of source files aimed at the 6502 processor. It takes an input file (e.g. myfile.a65 or test.asm) and creates the following output files, some of which are optional:

* .d65 file containing debug information such as symbols, source lines, etc. (NOT IMPLEMENTED)
* .hex file containing output information in .hex format which is human readable (NEEDS WORK, NOT IN CORRECT INTEL FORMAT)
* .lst file containing a listing of the assembler output
* .log file containing errors encountered during the assembly
* .map file containing the symbol information
* .o65 file which creates the object code and symbol information suitable for use with a linker (NOT IMPLEMENTED)

#### Development Status
This is very much experimental and was developed by the author as a learning tool for how assemblers work in general.
Please don't use this for anything serious that you would object to losing. Whilst having been extensively tested, and coming with 
working examples, there is no guarantee that it will work correctly with all input files.

#### Development Environment
To compile this software, you will need Lazarus 2.10 or later. It has been tested on Windows. As it is
only a simple text and file based application, it should be relatively easy to recompile on other hosts which are
supported by the Lazarus ecosystem, for example macOS, Linux, etc.

#### Dependencies
To modify the grammar for the opcode compiler, or XA80 itself, will require the use of a tool called LaCoGen (Lazarus Compiler Generator).
LaCoGen is available from the duncanamps GitHub. This tool turns .lac grammar description files into .lacobj files which describe the state tables for a lexer and a parser.
If you don't want to change the grammar, just use the asm65.lacobj file as it stands.

#### Documentation
The docs/ folder contains a user guide explaining how some of the features work.

#### Folder Structure
Folders are organised as follows:

* <root> the Lazarus project files, licence and .gitignore
  * code_examples/ - A number of test files, some of which are deliberately designed not to work
  * docs/ - Documentation
  * lac/ - The LaCoGen grammar for asm65. The asm65.lac file is compiled into asm65.lacobj
  * units/ - The bulk of the source code resides in here

#### Known Issues 
* The software is anything but complete and doesn't always correspond with the user manual
* Only the 6502 is catered for at this time
* The hex output is not in the correct Intel format
* Object file output isn't implemented
* Debug file output isn't implemented

#### Author
Duncan Munro
duncan@duncanamps.com