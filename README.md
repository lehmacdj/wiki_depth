# wiki_depth
This finds the cycle that following the first link in the main text section of
a wikipedia article converges to.  Takes the final bit of the url of a
wikipedia article as an argument.

## Building
You will need to install these Haskell packages as dependencies in order to be
able to build successfully.
- tagsoup
- http-conduit

Once you have installed the dependencies you can just use:
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
- There are some bugs for certain pages. See details at comment in getBody
- Only works on wikipedia, ideally I would pass a command line flag to make this work
  on any arbitrary wiki. Then it would be possible to find interesting conclusions about
  community wikis. This might be hard to implement though since ever wiki might not have the
  same HTML.
