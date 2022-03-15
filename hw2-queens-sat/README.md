# hw2-queens-sat

This package solves `N queens puzzle` via reduction to SAT.

- `src/Normalizer.hs`: module with formulas logic (converting and pretty-printing)
- `src/Board.hs`: module which builds coordinates for lines (stright and diagonal)
- `src/MinisatHelper.hs`: wrapper around minisat
- `src/Evaluator.hs`: module for evaluating formulas
- `src/QueenSat.hs`: main module which builds formula and passes it to minisat

Execute `stack build && stack exec queen-sat` to run program which reads `N` and runs minisat to find suitable arrangement of `N` queens on `N x N` board.

Execute `stack build && stack test` to run tests.

**Note**: `minisat` should be installed.
