# pulmurice-client

Client for *Pulmurice* programming challenge game.

## Short guide

1. Install Haskell: [Haskell Platform 2014.2.0.0](https://www.haskell.org/platform/) should be fine.
2. Clone the repository: `git clone git@github.com:futurice/pulmurice-client.git`
3. Try to compile:

    ```sh
    # Fetch git modules (as dependencies aren't in the hackage)
    git submodule init
    git submodule update

    # Cabal sandbox
    cabal sandbox init
    cabal install --only-dependencies

    # Try to build
    cabal configure
    cabal build
    ```

4. Try out

    ```
    # Signup, you will receive an email with a token
    cabal run -- signup <teamname> <email-address>

    # Fetch new puzzle, it should be auto-solved
    cabal run -- new -t TEAM_TOKEN echo 10

    # Alternatively, you can set PULMURICE_TEAMTOKEN environment variable
    export PULMURICE_TEAMTOKEN=<YOURTOKEN>
    cabal run -- new echo 10
    # Puzzle echo solved -- 6669ade9d7ac6b1d9386f2afd8c14a3b93152fb27671911092604bf5dbe6ab2d

    # List available puzzles:
    cabal run -- puzzles
    # echo                 Echoes the string
    # reverse              Reverses the string
    # square               Square the number
    # ...

    cabal run -- new reverse 10
    # don't know how to solve the puzzle :(
    # Puzzle reverse: cd5fd6af61e71c2eb3539d6af6d89bd90b090235b37f3e2db2230ff71b805609
    # Reverses the string
    # Input:
    # uffuu

    # Edit solver
    $(EDITOR) src/Solver.hs
    # Solving cd5fd6af61e71c2eb3539d6af6d89bd90b090235b37f3e2db2230ff71b805609
    # Puzzle reverse solved -- cd5fd6af61e71c2eb3539d6af6d89bd90b090235b37f3e2db2230ff71b805609

    # For more commands:
    cabal run -- --help
    ```

5. To solve other puzzle types, edit `src/Solver.hs`
