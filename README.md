# tptp-utils
Library for TPTP-related utility services

`tptp-utils` may be referenced as [![DOI](https://zenodo.org/badge/435301165.svg)](https://zenodo.org/badge/latestdoi/435301165).

-------------------------------

`tptp-utils` is an application for pre- and post-processing automated theorem proving input files (so-called problem files)
given in the [TPTP](http://tptp.org/) (*Thousands of Problems for Theorem Proving*) syntax standard.
It is written in Scala and freely available as open-source software.

Please check the TPTP web page for an introduction to the different language
dialects (THF, TFF, FOF, CNF, ...) for automated theorem provers.

Current features include:
 - Syntax checking *(validate that the input file is syntactically well-formed)*
 - Reparsing *(Read the input file and print an abstract syntax tree in JSON format)*
 - Dialect transformation *(Translate problem from TPTP dialect A to TPTP dialect B)*
 - Normal form computations *(Convert problem to some normal form)*
 - Decidable FOL fragment detection *(Analyze whether formulas in the TPTP file come from a known decidable FOL fragment)*

Experimental features include:
 - Linting *(Read and check the problem file for suspicious content, malformed logic specification, etc.)* **Work in progress!**
 - Importing (fragments of) LegalRuleML files to [normative TPTP](https://github.com/leoprover/logic-embedding) **Work in progress!**
 
## Usage

`tptp-utils` is a command-line tool, reads its input from an input file (or stdin)
and prints its result to stdout (or some output file), as follows:

```
usage: tptputils [options] <command> [command parameters] <problem file>

 <command> is the command to be executed (see below). <problem file> can be
 either a file name or '-' (without quotes) for stdin. If <output file> is
 specified, the result is written to <output file>, otherwise to stdout.

 Commands:
  parse        Parse the problem and return SZS Success if successful;
               SZS SyntaxError otherwise.
  reparse      Parse the problem and, if successful, print the AST of
               the parsed problem in a JSON-based format.
  transform    Parse a problem, and transform and print it in a different
               TPTP language. This is possible if the goal language is at
               least as expressive as the source language, e.g. transforming
               a FOF problem into a THF problem. Auxiliary formulae might be
               added if necessary, e.g., new type declarations.

               The goal language is specified as a mandatory command parameter
               using one of the following values:
               --CNF, --TCF, --FOF, --TFF, --THF
  downgrade    Parse a problem, transform and print it in a less expressive TPTP
               language. This will fail if the problem contains formulae that are
               not directly expressible in the goal language, e.g., lambdas in THF
               when transforming to TFF and similar.
               If the goal language is more expressive
               instead, then `transform` will be executed instead.

               The goal language is specified as a mandatory command parameter
               using one of the following values:
               --TFF

  lint         Inspect the input problem for suspicious constructs, unused symbols,
               malformed logic specifications, etc.

  import       Translate, if possible, the input file into an adequate TPTP
               representation.

               The source language is specified as a mandatory command parameter:
               --LRML   (for import from LegalRuleML)

  normalize    Transform the input wrt. some normal form given as parameter.
               Valid parameters are (more to come):
               --prenex (for prenex normal form)

  fragment     Analyze the input whether it is member of some known fragment of FOL.
               Works only for FOF/TFF inputs. Fragments are noted inside the annotation
               of the annotated formulas. Can recognize:
               Propositional,
               BernaysSchoenfinkelRamsey,
               MonadicFirstOrder,
               Löwenheim,
               LöbGurevich,
               GödelKalmárSchütte,
               Ackermann,
               GurevichMaslovOrevkov

 Options:
   --tstp      Enable TSTP-compatible output: The output in <output file>
               (or stdout) will start with a SZS status value and the output
               will be wrapped within SZS BEGIN and SZS END block delimiters.
               Disabled by default.

  --recursive  Recursively parse all includes contained in the input file.
               This might make it necessary to set the TPTP environment variable
               if TPTP-specific includes need to be resolved.

  --output <output file>
               Write output to <output file> instead of stdout.

  --version    Print the version number of the executable and terminate.

  --help       Print this description and terminate.
```

### Example 1:
As an example, if you want to check syntactic well-formedness of some file,
say, `path/to/problem.p`, invoke as follows:
```bash
> ./tptp-utils parse /path/to/problem.p
```
If exiting without any error (exit status 0), parsing succeeded. If, on the other hand,
there is a syntax error, the output will look similar to the following with exit status 1:
```
Error: Input file could not be parsed, parse error at 33:1: Expected DOT but read LOWERWORD 'thf'
```


### Example 2 (TSTP-compatible output):
If providing the `--tstp` option, `tptp-utils` will give results compatible to the
TSTP data exchange format. Additional output data (for reparse and transform command) is then provided
between SZS output delimiters. Same example as before, but with TSTP-output:

```bash
> ./tptp-utils --tstp parse /path/to/problem.p
% SZS status Success for /home/lex/TPTP/Problems/SYN/SYN000^1.p
```
If there is a syntax error, the output will look similar to the following:
```
% SZS status Error for /home/lex/Downloads/test.p : Input file could not be parsed, parse error at 33:1: Expected DOT but read LOWERWORD 'thf
```

Using TSTP output is important when, e.g., exchanging this information between reasoning systems.

### As library

`tptp-utils` may also be included as library to your Scala software projects.

## Installation

You can use the pre-built executable .jar file (in the release section), without
any other installation steps. 

You may build `tptp-utils` from source: This requires a JDK and the scala build tool `sbt` in
a reasonably current version.

## License
`tptp-utils` is published under MIT license, see `LICENSE` file for details.
