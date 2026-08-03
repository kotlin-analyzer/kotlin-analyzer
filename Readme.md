# Language server implementation for Kotlin in Rust (in Devlopement)
This LSP is inspired by Rust Analyzer, so much that it steals the parsing algorithm used in it, which was in turn, inspired by the Kotlin parser in Kotlin/Java
by JetBrains.
This also publishes(WIP) reusable crates that can be used to build tools for Kotlin, e.g. parser, and lexer which can be used to implement to
implement linters and formatters.
This is pretty much in development as I try to work on this in my free time, so there are no release artefacts yet. If you want to see latest progress,
check pull requests or non-main branches.

## Progress

- [ ] Kotlin
    - [x] Lexer
    - [ ] Parser
        - [x] All Language constructs
        - [ ] Tests
            - [ ] Unit Tests
            - [ ] Conformance Tests 
    - [ ] AST (In progress)
    - [ ] AST Traversal API
- [ ] Java (Not started, but a lot easier to parse than Kotlin)
- [ ] LSP client (Requires at least AST Traversal API for .kt files)

## Why
I want a Kotlin experience that is decoupled from IntelliJ. It should not matter if I am developing in Emacs, Helix, Vs Code or Zed.
It should just work, and work well (goal is to get as good as Rust Analyzer is to Rust). I should not have to know all grade/maven shenanigans (resolve dependencies on activation). And it should be fast. And don't break in a thousand different ways.

### Why not just contribute to the new LSP from JetBrains
Writing complex logic such as those used in parsing and traversing context free grammars can be rather painful, writing it in Rust keeps my sanity intact.
Also competition can be good for both.

## How to contribute
Nothing special, there are some few open issues and I will make more soon, start from asking to pick one and let me know if you need more context.
Please refer to Rust Project code of conduct for our code of conduct.

## How to Support
I am not able to dedicate much time to this becaus I have to work, consider donating (I hope to setup GitHub Sponsor at some point).
Contribute to the code or start using some of the crates in your own tools, say the lexer and parsers.
