# mhash 

Simple program for minhashing documents written in Haskell.

## Caveats

I make no claims that this implementation is faster, better, more
correct, or more  secure than any other implementation.  Nor do I
make  the  claim  that  this  is the  best  way  of  implementing
Minhashing, that the parameters chosen in it are the best.

It is simply  an implementation of of minhashing  that I've tried
on  my machine  and  found  that with  a  database  of a  hundred
thousand or so  reddit comments, 500 hashes per  document, and an
n-gram size of  5, the algorithm was able to  detect when I added
duplicate posts in what seemed to  be a reasonable amount of time
to me on my machine.

## What is minhashing?

Minhashing is a more sophisticated alternative to term-frequency,
inverse-document  frequency (TF-IDF)  search. Rather  than search
for documents based on how many  key words are it has, minhashing
searches for  documents based on how  many consequitive sequences
of words it  shares with some document you are  searching for. In
addition to being used  for finding potential duplicate documents
in a  database, it's  also used in  plagurism-detection software,
and a  variant called SimHash is  used by Google to  find similar
news stories.

Minhashing is a locality-sensitive  hashing technique. That means
that it  creates a hash  (or in this case,  a list of  hashes) in
such a way that similar documents are more likely to have similar
hashes.

## How does the algorithm work?

The algorithm works by breaking  a document down into overlapping
sequences of  words, called  either n-grams  or shingles.  Then a
hashing algorithm  with some arbitrary salt  hashes every n-gram.
The algorithm  then chooses whatever  happens to be  the smallest
hash  for the  document. That  hash represents  some sequence  of
words  in the  document,  and  by taking  the  smallest hash  the
algorithm is  essentially taking  selecting a random  sequence of
words from the document.

Taking  the  smallest hash  is  locality-sensitive;  if the  hash
happened  to be  0,  then we  would be  guaranteed  that if  that
sequence  of words  (represented by  the hash  we chose)  were to
occur in another  document, that hash would also  be chosen since
there are no hashes numbers smaller  than zero (at least with the
hash algorithm chosen for this program).  While a hash of value 1
wouldn't be guaranteed  to be selected by  another document since
it is possible that a second document contains both a sequence of
words that hashes to 1 and a  sequence of words that hashes to 0.
However, the  smaller the number is,  the less likely it  is that
the  second  document will  not  contain  the same  hash  despite
containing the same randomly-chosen sample of text in it.

Because a single n-gram from a document is not enough to properly
compare their similarity, we have to  vary the salt a few hundred
times in order  to more properly represent the document  as a set
of hashes. However, we've at  least reduced the dimensionality of
the document to something small enough that we can search through
a  large database  of  documents in  an amount  of  time that  is
hopefully reasonable for your use-case.

## Instructions

Create a new database file in ~/.mhash:

    $ mhash setup                 


To create  a new table to  store hashes, you will  need to choose
the number  of hashes to  use and the  length of n-grams  to use.
Think of minhashing as taking a sample of words in a document and
checking to see if that little sample of words appears in another
document. If  you use a 10-gram,  you are using samples  from the
document that  are 10 words  long. The  number of hashes  is then
like the number of samples to check. Choose a number large enough
that  it  is  enough  to statistically  distinguish  between  two
documents.

Thus, to create  a new table named \"docs\" that  uses 500 hashes
and 10-grams:


    $ mhash new dosc 500 10


To fix the typo above, we type:

    $ mhash rename dosc docs


Set your new table as the default table:

    $ mhash set-default docs

    OR

    $ mhash set docs


Hash a directory of text files to the database:

    $ mhash add-dir /home/you/Documents/TextFilesDir/


Alternatively, add a single file:

    $ mhash add /home/you/Documents/TextFilesDir/doc30134.txt


Search for the top 10 documents similar to doc30134.txt:

    $ mhash find-similar /home/you/Documents/TextFilesDir/doc30134.txt 10


You can get the first couple of lines from those documents, too:

    $ mhash print-similar /home/you/Documents/TextFilesDir/doc30134.txt 10


Additionally, you can list all tables on file:

    $ mhash list-tables


You can list every filepath currently in the default table:

    $ mhash list-docs


You can also list every filepath in a table not set as default:

    $ mhash list-docs notDefTbl


For a cheatsheet, you can type:

    $ mhash -h

    OR

    $ mhash help

    OR

    $ mhash ?


Additionally, if  you want  to augment this  program or  I didn't
think of  something you need, the  entire thing is just  a SQLite
database  located at  ~/.mhash.  Even for  a  non-coder, you  can
probably figure  things out by  using SqliteBrowser or  a similar
tool.

## Code Structure

### Main.hs

Dispatches  command line  arguments to  the appropriate  function
calls.

### Lib.hs

Exports services/functions to be used in Main.

### BasicDB.hs

Database operations  which are  used throughout the  code; simple
helper functions which provide just a little bit more abstraction
over HDBC.

### Groupings.hs

Some helper functions that group lists of objects:

    * ngram for breaking a list of tokens into groups of n words

    * toGroupsOfN which is similar, but does allow overlapping

    * tokenize which converts a string to a list of tokens, and

    * getNumBuckets  which calculates  the number  of buckets  (size of
      groups  of items  to find  matches for,  also called  "bands") to
      search for in the database.

### MoonLogic.hs

Explicit data-modelling of  the list of minhashes  to represent a
document. Wraps that list of minhashes into the type MoonInt (see
numerphile.com/videos/lunar-arithmetic). In addition  to making a
MoonInt more typesafe by distinguishing it from a list of Ints, a
list of hashes also forms a monoid under the pairwise application
of  min,  allowing it  to  use  several built-in  functions  like
mconcat.  Also defines  the  function "jaccard"  for finding  the
jaccard similarity of two MoonInts.

### ParallelHash.hs

A parallel version of the hashing functions used in this project.
The serial  version is  no longer  present in  src and  was moved
entirely to  the directory test  so that it could  be benchmarked
against the parallel version. The parallel version actually takes
the same  amount of time to  complete on my machine,  but I coded
the  whole thing  on  a  slow laptop  while  visiting my  parents
out-of-state,  so presumably  on more  sophisticated machines  it
will outperform the serial verison rather than just match it.

### Reads.hs

Basic SQL  operations which  read from the  database, but  do not
alter or  further process the data,  nor do they make  changes to
the database.

### Updates.hs

Basic  SQL  operations  which  create permanent  changes  to  the
database.

### Services.hs

Functions which chain  together operations defined in  all of the
files  above  in order  to  accomplish  certain use-cases.  Every
function, with  the exception of  hashDoc_ is called  directly by
main (main also  calls two functions from  Updates directly since
the use-case can be translated directly into SQL).

### StopWords.hs

A list of common and not very interesting words to be filterd out
from the algorithm. Obtained from:
    https://algs4.cs.princeton.edu/35applications/stopwords.txt
