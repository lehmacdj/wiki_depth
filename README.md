# wiki_depth
This finds the cycle that following the first link in the main text section of
a wikipedia article converges to.  Takes the final bit of the url of a
wikipedia article as an argument.

## Building
```
make
```

## Running
Simply run the executable as follows from the command line:
```
./wiki_depth <wikipedia-page-name>
```
\<wikipedia-page-name> is the part of the url for a wikipedia page that comes after `/wiki/`. 
For example for the url `https://en.wikipedia.org/wiki/Philosophy` call the command as
`./wiki_depth Philosophy`.

## Bugs
- This will follow links that are in parenthesis. This is not the intended
  behavior for this executable.
- This is pretty slow, it probably should be possible to make this faster

## Dependencies
- tagsoup
- http-conduit
