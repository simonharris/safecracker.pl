# safecracker.pl

Automated solving of the Sunday Telegraph's "Safe Cracker" numeric logic puzzle from a cameraphone photo of the printed puzzle.

There are three subsystems:

 - [Parser](parser): a Natural Language Processing and logical reasoning engine written in Prolog, which parses the puzzle text using a Definite Clause Grammar, and converts the clues into CLP(FD) constraints
 - [API](web/api2): a back-end system built on the FastAPI framework in Python, which performs the Optical Character Recognition, interfaces with Prolog, and exposes a streaming HTTP API
 - [Site](web/site): a responsive, interactive front-end JavaScript web app built with Vue.js

Disclaimer: I only buy that awful paper for the puzzles.

## Dependencies

### [plunit_assert](https://github.com/simonharris/plunit_assert)

 `plunit_assert` is a unit testing library for Prolog. It can currently be installed using:

```
?- pack_install(plunit_assert).
```
