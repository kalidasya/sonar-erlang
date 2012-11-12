-module(linelength).

hello(A) -> 
    B = A+1, %first error
    B = A +1, %first error
    C= B -2, %second error
    D =C- 2. %third error
