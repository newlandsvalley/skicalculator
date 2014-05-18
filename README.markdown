## _SKI Calculator_ 

This is an exercise inspired by Raymond Smullyan's [to mock a Mockingbird](http://en.wikipedia.org/wiki/To_Mock_a_Mockingbird). It consists essentially of two processes:

*   Alpha Elimination.  This takes as input a combinator bird, consisting only of variables x,y,z.... bracketed appropriately and converts it to a SKI calculus representation (i.e. only including S,K and I) by a process of α-elimination of each variable in turn. 

*   SKI Interpretation.  This is the reverse process - i.e. it applies the SKI representation to the target variables and re-constitutes the original combinator.

The α-elimination algorithm is outlined in chapter 18 of the book.  Unit tests exist for all the major combinator birds listed in _Who's Who Among the Birds_ converting them to SKI and back again. 