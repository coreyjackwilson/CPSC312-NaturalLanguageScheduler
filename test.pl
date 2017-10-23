:- dynamic foo/3.
foo(a,b,c).
foo(c,d,e).
rep(X,X2) :-
  retract(foo(X,Y,Z)),
  assert(foo(X2,Y,Z)).
