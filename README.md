# Parallel Autocomplete

This program parses a corpus of English text for word counts and provides at most K autocomplete suggestions by descending frequency to each input string given by the user.

Commands:
```
stack build
stack exec autocomplete-exe <filename> <k> <seq|par> -- +RTS -N1 -s -ls
```

Example sequential execution:
```
stack exec autocomplete-exe 100-0.txt 500 seq -- +RTS -N1 -s -ls
```

Example parallel execution:
```
stack exec autocomplete-exe 100-0.txt 500 par -- +RTS -N4 -s -ls
```