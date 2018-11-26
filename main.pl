use_module(library(clpfd)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The game board is a list of columns

Each column is a term col(Num,Free,TP,TN,P), with:
- Num: column number Num
- boolean Free: whether a piece can be placed "on top" (= at the end)
- TP: colour of topmost piece: TP
- TN: max. number of consecutive topmost pieces of same colour
- P: Pieces in this column

Pieces are either
empty cell:   - 
piece of red player:   'X'
piece of blue player:   'O'
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


% Empty Cell:
empty(-).

% Empty Board:
% - 
empty_board(N, M, Board) :-


welcomeScreen :- print('**************************************'),
    nl,   
    print('*                                             *'),
    nl,     
    print('*        WELCOME TO             *'),
    nl, 
    print('*   PROLOG CONNECT 4!     *'),
    nl,     
    print('*                                             *'),
    nl,      
    print('**************************************'),
    nl,
    print(' play at your own risk...').
