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

    # For more commands:
    cabal run -- --help
    ```

5. To solve other puzzle types, edit `src/Solver.hs`
