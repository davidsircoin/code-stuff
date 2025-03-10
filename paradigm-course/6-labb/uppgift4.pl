transpose([[a11,a12,a13,a14]
          ,[a21,a22,a23,a24]
          ,[a31,a32,a33,a34]
          ,[a41,a42,a43,a44]],
          [[a11,a21,a31,a41]
          ,[a12,a22,a32,a42]
          ,[a13,a23,a33,a43]
          ,[a14,a24,a34,a44]]).

  diagonals([[a11,_,_,a14]
            ,[_,a22,a23,_]
            ,[_,a32,a33,_]
            ,[a41,_,_,a44]],[[a11,a22,a33,a44],[a14,a23,a32,a41]]).


sum_elems([],0).
sum_elems([X|XS],Sum) :- sum_elems(XS,SumXS),
                         Sum is X + SumXS.

isSumEqual(XS,N) :- sum_elems(XS,Sum), Sum is N.


all34([]).
all34([XS|XSS]) :- isSumEqual(XS,34), all34(XSS).
