## An implementation of Go mini-language

**License**: APACHE LICENSE, VERSION 2.0

**Author**: Azim Muradov, azim.muradov.dev@gmail.com


### Features:

Project parts:

|     To do      | In progress |     Done     |
| :------------: | :---------: | :----------: |
| pretty printer |    docs     |     CLI      |
|   unit tests   |             |     AST      |
|                |             |    lexer     |
|                |             |    parser    |
|                |             | analyzed AST |
|                |             |   analyzer   |
|                |             | interpreter  |


#### Done:

- `int`, `bool`, `string` support
- `void` support
- multidimensional arrays support
- function literals (anonymous functions)
- operators (arithmetic, logic, comparison)
- `if`, `else`
- `for`
- recursion, mutual recursion
- function definitions
- variable declarations
- variable assignments
- globals support
- closures (including mutable closures)
- short variable declarations
- increment, decrement
- parser, lexer
- analyzer (check for name collision or missing names, type checker, const expressions converters (w/o `const` keyword))
- interpreter
- several stdlib functions (`len`, `print`, `println`, `panic`)
