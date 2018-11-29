use_module(library(clpfd)).





% Empty Board:
% - 

/*
initBoard :-
    col(1,true,empty,0,0,[empty,empty,empty,empty,empty,empty]),
    col(2,true,empty,0,0,[empty,empty,empty,empty,empty,empty]),
    col(3,true,empty,0,0,[empty,empty,empty,empty,empty,empty]),
    col(4,true,empty,0,0,[empty,empty,empty,empty,empty,empty]),
    col(5,true,empty,0,0,[empty,empty,empty,empty,empty,empty]),
    col(6,true,empty,0,0,[empty,empty,empty,empty,empty,empty]),
    col(7,true,empty,0,0,[empty,empty,empty,empty,empty,empty]).
*/
/*
initState(col(1,true,empty,0,0,[]),
          col(2,true,empty,0,0,[]),
          col(3,true,empty,0,0,[]),
          col(4,true,empty,0,0,[]),
          col(5,true,empty,0,0,[]),
          col(6,true,empty,0,0,[]),
          col(7,true,empty,0,0,[])
          ).          
 
initState(row(1,[]),
          row(2,[]),
          row(3,[]),
          row(4,[]),
          row(5,[]),
          row(6,[]),
*/
/*
boardState :-
    col(1,Free,TP,TN,L,[Top|Bot]),
    col(2,Free,TP,TN,L,[Top|Bot]),
    col(3,Free,TP,TN,L,[Top|Bot]),
    col(4,Free,TP,TN,L,[Top|Bot]),
    col(5,Free,TP,TN,L,[Top|Bot]),
    col(6,Free,TP,TN,L,[Top|Bot]),
    col(7,Free,TP,TN,L,[Top|Bot]).
*/




%% a Game takes a move and the current state and returns a result game state
%%type Game = Move -> State -> Result


%%-- a Move is a piece drop to a column with room in it.
%move(playerType(PL), col(Num,free,_,TN,L,P)) :- 
%    col(Num,free,PL,TN,L+1,[P|PL]).
%move(playerType(PL), col(Num,free,_,TN,5,P)) :- 
%    col(Num,false,PL,TN,6,P).

%move(playerType(PL), ColNum, row(Num,List)) :-
%          insertPiece(ColNum, row(Num,List))).

%insertPiece(ColNum, row(Num,List)) :-
    

move(concede).

%%-- the State keeps track of the internal state of the game and the available moves
%%data State = State InternalState [Move]

%%-- a Result is what happens after each move
%%%data Result = EndOfGame PlayerType GameBoard    -- end of game, value, ending state
%            | MyTurn State              -- continue current player's turn with new state
%            | YourTurn State            -- continue next player's turn with new state
 %           | InvalidMove State



%-- Current Game State:
%-- - Gameboard
%-- - Player's turn
%data InternalState = GameState GameBoard PlayerType

/*
%-- a Player takes the current state of the game and returns a move
type Player = State -> Move
type IOPlayer = State -> IO Move
*/
player(playerType(_), gameState(_), listOfMoves).


%-- PlayerType is the two players in the game
%data PlayerType = North | South
%  deriving (Eq, Show)

playerType(red).
playerType(blue).
player(blue).
player(red).


/*
-- flipPlayer returns the opposite of the given player type
flipPlayer :: PlayerType -> PlayerType
flipPlayer North = South
flipPlayer South = North
*/

flipPlayer(red) :- blue.
flipPlayer(blue) :- red.


/*

%Pieces are either
%empty cell:   '-', empty 
%piece of red player:   'X', red
%piece of blue player:   'O', blue
 */

standardRows(6).  
standardCols(7). 


col(1).
col(2).
col(3).
col(4).
col(5).
col(6).
col(7).

row(1).
row(2).
row(3).
row(4).
row(5).
row(6).

%%Square is a single square tile on a Connect4 grid
%It can host either players' piece, 
%or be empty.
square(col(_),row(_)) :- empty.
square(col(_),row(_)) :- red.
square(col(_),row(_)) :- blue.
% test data
square(col(1),row(_),x).
square(col(2),row(_),x).
square(col(3),row(_),o).
square(col(4),row(_),o).
square(col(5),row(_),x).
square(col(6),row(_),x).
square(col(7),row(_),x).

fullcol(col(X), 
        square(col(X),row(1),_),
        square(col(X),row(2),_),
        square(col(X),row(3),_),
        square(col(X),row(4),_),
        square(col(X),row(5),_),
        square(col(X),row(6),_)
        ).

fullrow(row(Y),
        square(col(1),row(Y),_),
        square(col(2),row(Y),_),
        square(col(3),row(Y),_),
        square(col(4),row(Y),_),
        square(col(5),row(Y),_),
        square(col(6),row(Y),_),
        square(col(7),row(Y),_)
        ).


red.
blue.
empty.

%Gameplay:
%


/*
-- 
data Square = Square Int Int
    deriving (Show)

instance Eq Square where
    (Square a b) == (Square c d) = a == c && b == d

instance Ord Square where
    (Square a b) <= (Square c d) = a < c || (a == c && b <= d)
*/


/*
-- GameBoard is a map from a square to the piece in that square
type GameBoard = Map Square Piece
*/



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UI DISPLAY SECTION:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


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



mainOptionsScreen :- nl, 
    print('START MENU:'),
    nl,  
    nl,       
    print('Player VS Player [ Enter 1 ]'),
    nl, 
    print('Player VS AI [ Enter 2 ]'),
    nl, 
    print('Exit [ Enter any other key ]'),
    nl.    
     

play :- welcomeScreen, 
    nl,
    mainOptionsScreen,
    nl,
    read(X),
    play_mode(X).


play_mode(1) :- print('Playing against your friend'),
    nl,
    print('Press any key to continue'),
    read(_),
    displayBoardExample,
    nl,
    gameTurn(initBoardExample, red).
play_mode(2) :- print('Playing against AI'),
    nl,
    print('Press any key to continue'), %TODO replace for real game board
    read(_),
    displayBoardExample, %TODO replace for real game board
    nl.
 %   gameTurn(initBoardExample, red).    
play_mode(_) :- print('Good bye').


%gameTurn(Board, Player) :- 
%    win(Board, Player), 
%    write(Player), write(' Wins!').
%gameTurn(Board, _) :- 
%    full(Board), 
%    write('It\'s a Draw!').
gameTurn(Board, red) :-
    nl,
    write(red),
    write(' player\'s turn:'),
    nl,
    write('Moves Available:'),
    nl,
    %TODO: display list of available moves here
    %ONLY list of moves, or concede, 
    % are available to players.
    read(_), 
%    getMove(Player, Move), %TODO: this gets user kb input as move or concede
%    userMove(Board, Move, BoardAfter),
    %displayBoard(BoardAfter),
    displayBoardExample, %TODO replace for real game board
    gameTurn(BoardAfter1, blue).


gameTurn(Board, blue) :-
    nl,
    write(blue),
    write(' player\'s turn:'),
    nl,
    write('Moves Available:'),
    nl,
    %TODO: display list of available moves here
    %ONLY list of moves, or concede, 
    % are available to players.
    read(_), 
%    getMove(Player, Move), %TODO: this gets user kb input as move or concede
%    userMove(Board, Move, BoardAfter),
    %displayBoard(BoardAfter),
    displayBoardExample, %TODO replace for real game board
    gameTurn(BoardAfter1, red).


%USE THIS
getListOfAvailMoves(Board, ListTotal) :- 
    getListOfAvailMoves(1, Board, [], ListTotal).
%CASE: col has available move
getListOfAvailMoves(Count, Board, ListInit, ListTotal) :-
    nth1(Count,Board,Col),  %get nth col from board
    member('_', Col),  %incl if col has empties in it
    append(Count,ListInit,ListTotal), %incl N if its a col w/ empties in it
    Count1 is Count +1,
    getListOfAvailMoves(Count1, Board, ListTotal).
%CASE: col DOESNT has available move
getListOfAvailMoves(Count, Board, _, ListTotal) :-
    nth1(Count,Board,Col),  %get nth col from board
    \+ member('_', Col),  %incl if col has empties in it
    Count1 is Count+1,
    getListOfAvailMoves(Count1, Board, ListTotal).
%ENDCASE
getListOfAvailMoves(8, _, _, _).


%TODO
%getMove(Player, Move, ListOfMoves)
%%getMove(Player, Move, Concede) Concede: qq, QQ, Qq, qQ


%win(Board, Player) :- Board, Player. %STUB
%full(Board) :- Board. %STUB

%getMove(Player, Move) :- Player, Move. %STUB %TODO: this gets user kb input as move or concede
%userMove(Board, Move, BoardAfter):- 
%    Board, Move, BoardAfter. %STUB

%shows all possible columns a player can drop a piece in
%allValidUserMoves(Board, Player, ListOfCols) :- 
 
/*
    integer(N),
    N >= 1,
    N =< 7,
    nth1(N, Board, C),
    valid_move_column(C).
valid_move_column([H|_]) :- H = '-'.
*/

% Insert a piece to board
/* NOT THIS ONE
insert(Board, Player, ColNum, BoardAfter) :-
    insertToCol(
        col(Col, ColNum,true,_,_,_,_),
        Player, CAfter),
    updateBoard(Board, Col, CAfter, BoardAfter).

insertToCol(Col, ColNum, Player, CAfter) :- 
    col(ColNum,true,empty,0,0,[empty,empty,empty,empty,empty,empty]).
*/



%%update_board([H|T], 1, C, [C|T]).
%update_board([H|T], N, C, [H|R]) :- 
%   N2 is N-1, update_board(T, N2, C, R).

%updateBoard(Board, Col, CAfter, BoardAfter) :-
    

/*
% Insert a piece to board
insert(Board, Player, Col, BoardAfter) :-
    nth1(Col, Board, C), % gives us the desired Column of Board as C
    insertToCol(C, Player, CAfter),
    updateBoard(Board, Col, CAfter, BoardAfter).


% Insert a piece into a specificed column
insertToCol(['-',X|T], Colour, [Colour,X|T]) :- \+X = '-'.
insertToCol(['-'], Colour, [Colour]).
insertToCol([H|T], Colour, [H|R]) :- insertToCol(T, Colour, R).
*/



/*
%play_player(Board)
%play_player(Board)
%play_player(Board)
play_player(Board) :-
    displayBoardExample,
    print('Player X turn:'),
    nl,
    read(N),
%    insert_piece(Board, x, N, BoardX),
%    display_board(BoardN),
    displayBoardExample, 
    print('Player O turn:'),
    nl,
    read(N),
%    insert_piece(Board, o, N, BoardO),
%    display_board(BoardO),
    play_player(BoardO).
*/

%insert_piece(Board, Colour, Column, BoardX).


displayPiece(empty) :- print('-').
displayPiece(red) :- print('X').
displayPiece(blue) :- print('O').

exampleBoard :-
    col(1,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(2,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(3,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(4,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(5,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(6,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(7,true,empty,0,[empty,empty,empty,empty,empty,empty]).


/*
   Each column is a term col(Num,Free,TP,TN,PC,Ps), where:
      Num: column number
      Free: yes/no, whether a piece can be placed "on top" (= at the end)
      TP: Colour of topmost piece
      TN: max. number of consecutive topmost pieces of same colour
      PC: Piece Count
      Ps: List Pieces in this column
 */ 

/*
initBoardExample :- [
    col(1,true,empty,0,0,['_', '_', '_', '_', '_', '_']),
    col(2,true,empty,0,0,['_', '_', '_', '_', '_', '_']),
    col(3,true,empty,0,0,['_', '_', '_', '_', '_', '_']),
    col(4,true,empty,0,0,['_', '_', '_', '_', '_', '_']),
    col(5,true,empty,0,0,['_', '_', '_', '_', '_', '_']),
    col(6,true,empty,0,0,['_', '_', '_', '_', '_', '_']),
    col(7,true,empty,0,0,['_', '_', '_', '_', '_', '_'])
                    ].
*/

initBoard :- initBoardFull.
initBoardFull(                   
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_']
                    ).

/*    
initBoardExample :- [
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_']
                    ].
*/
    
    
displayBoardExample :- 
    print(' |_|_|_|_|_|_|_| '),
    nl,    
    print(' |_|_|_|_|_|_|_| '),
    nl,    
    print(' |_|_|_|_|_|_|_| '),
    nl,    
    print(' |_|_|_|_|_|_|_| '),
    nl,    
    print(' |_|_|_|_|_|_|_| '),
    nl,    
    print(' |_|_|_|_|_|_|_| ').

% display board row by row (?)
%displayBoard

% display board row by row (?)
displayBoard(row(6)) :-
    displayRow(row(6)).
displayBoard(row(Y)) :-
    displayRow(row(Y)),
    nl,
    Y1 is Y+1,
    displayBoard(row(Y1)).

displayRow(row(Y)) :- 
    write(' |'),         
    displaySquares(col(1),row(Y)).

displaySquares(col(7),row(Y)) :-
    square(col(7),row(Y),Piece),
    write(Piece),
    write('|').
displaySquares(col(X),row(Y)) :-
    square(col(X),row(Y),Piece),
    write(Piece),
    write('|'),
    X1 is X + 1,
    displaySquares(col(X1), row(Y)).

    /*
*/
