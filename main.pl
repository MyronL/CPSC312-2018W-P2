:- use_module(library(clpfd)).
:- use_module(library(lists)).

player(playerType(_), gameState(_), listOfMoves).

%-- PlayerType is the two players in the game
%data PlayerType = North | South
%  deriving (Eq, Show)

playerType(red).
playerType(blue).
player(blue).
player(red).

red.
blue.
empty.

%GLOBAL VARS FOR DELAYS
superQuickDelay(0.6).
littleDelay(0.9).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PLAY SECTION:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

welcomeScreen :- print('**************************************'),
    nl,
    print('*                                   *'),
    nl,
    print('*        WELCOME TO                 *'),
    nl,
    print('*   PROLOG CONNECT 4!               *'),
    nl,
    print('*                                   *'),
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
    print('Ready... 3. 2.. 1...'),
    sleep(0.6),
    nl,
    print('GO!'),
    nl,
    initBoardFull(B),
    displayBoard(B),
    gameTurn(B, red).

play_mode(2) :- print('Playing against AI'),
    nl,
    print('Ready... 3. 2.. 1...'),
    sleep(0.6),
    nl,
    print('GO!'),
    nl,
    initBoardFull(B),
    displayBoard(B),
    gameTurnMachine(B, red, human).

play_mode(_) :-
    print('Good bye'),
    abort.

gameTurn(Board, _) :- win(Board, x), 
    write('Congratulations Player 1 Wins!'),
    nl,
    littleDelay(S),
    sleep(S),
    nl,    
    play.
gameTurn(Board, _) :- win(Board, o), 
    write('Congratulations Player 2 Wins!'),
    nl,    
    littleDelay(S),
    sleep(S),
    nl,
    play.
gameTurn(Board, _) :- full(Board), write('It\'s a tie!').
gameTurn(Board, Player) :-
    nl,
    write(Player),
    write(' player\'s turn:'),
    nl,
    write('Moves Available:'),
    getListOfAvailMoves(Board, ListTotal),
    write(ListTotal),
    nl,
    write('Please select a column. (Or enter qq to quit)'),
    nl,
    read(Move),
    getMove(Player, Move, ListTotal, Board, BoardAfter),
    displayBoard(BoardAfter),
    flipPlayer(Player, OtherPlayer),
    gameTurn(BoardAfter, OtherPlayer).

%Use this for getting opposite player.
%Use as flipPlayer(Player, OtherPlayer).
flipPlayer(red, blue).
flipPlayer(blue, red).


gameTurnMachine(Board, _, _) :- win(Board, x), 
    write('Congratulations Player 1 Wins!'),
    nl,
    littleDelay(S),
    sleep(S),
    nl,    
    play.
gameTurnMachine(Board, _, _) :- win(Board, o), 
    write('Nice try... AI Wins!'),
    nl,
    littleDelay(S),
    sleep(S),
    nl,    
    play.
gameTurnMachine(Board, _, _) :- full(Board), write('It\'s a tie!').
gameTurnMachine(Board, Player, human) :-
    nl,
    write(Player),
    write(' player\'s turn:'),
    nl,
    write('Moves Available: '),
    getListOfAvailMoves(Board, ListTotal),
    write(ListTotal),
    nl,
    read(Move),
    getMove(Player, Move, ListTotal, Board, BoardAfter),
    displayBoard(BoardAfter),
    gameTurnMachine(BoardAfter, Player, ai).

gameTurnMachine(Board, Player, ai) :-
    nl,
    write('Machine\'s turn'),
    nl,
    littleDelay(S),
    sleep(S),  %slight delay so user comprehends what AI move is
    machineTurn(blue, Board, BoardMachine),
    write('Machine Played [#]'), %TODO 
    nl,
    displayBoard(BoardMachine),
    gameTurnMachine(BoardMachine, Player, human).



%Use this for getting opposite player type.
%Use as flipPlayerType(PlayerType, OtherPlayerType).
flipPlayerType(human, ai).
flipPlayerType(ai, human).






% Winning Conditions
win(Board, Player) :- rowWin(Board, Player).
win(Board, Player) :- columnWin(Board, Player).
win(Board, Player) :- diagonalWin(Board, Player).

columnWin(Board, Player) :-
    append(_, [Column|_], Board),
    append(_, [Player, Player, Player, Player|_], Column).

rowWin(Board, Player) :-
    transpose(Board, Board1),
    columnWin(Board1, Player).

diagonalWin(Board, Player) :- diagonalRight(Board, Player).
diagonalWin(Board, Player) :- diagonalLeft(Board, Player).

diagonalRight(Board, Player) :-
    append(_, [Column1, Column2, Column3, Column4|_], Board),
    append(Elem1, [Player|_], Column1),
    append(Elem2, [Player|_], Column2),
    append(Elem3, [Player|_], Column3),
    append(Elem4, [Player|_], Column4),
    length(Elem1, N1),
    length(Elem2, N2),
    length(Elem3, N3),
    length(Elem4, N4),
    N2 is N1 + 1,
    N3 is N2 + 1,
    N4 is N3 + 1.

diagonalLeft(Board, Player) :-
    append(_, [Column1, Column2, Column3, Column4|_], Board),
    append(Elem1, [Player|_], Column1),
    append(Elem2, [Player|_], Column2),
    append(Elem3, [Player|_], Column3),
    append(Elem4, [Player|_], Column4),
    length(Elem1, N1),
    length(Elem2, N2),
    length(Elem3, N3),
    length(Elem4, N4),
    N2 is N1 - 1,
    N3 is N2 - 1,
    N4 is N3 - 1.


% FOR TIE GAMES:
% full(Board) - Checks if board is full
full(Board) :- full(Board, 7).
full(_, 0). 
%CASE: board full, recursive
full(Board, N) :-
    nth1(N, Board, Col),
    \+ member('_', Col),
    N1 is N-1,
    full(Board, N1).





%USE THIS
showListTotal([]).
showListTotal([H|T]) :-
    write(H),
    showListTotal(T).

getListOfAvailMoves(Board, ListTotal) :-
    getListOfAvailMoves(7, Board, [], ListTotal).
%ENDCASE
getListOfAvailMoves(0, _, ListTotal, ListTotal).
%CASE: col has available move
getListOfAvailMoves(Count, Board, Acc, ListTotal) :-
    nth1(Count,Board,Col),  %get nth col from board
    columnFree(Col),  %incl if col has empties in it
%    append(Count,ListInit,ListTotal), %incl N if its a col w/ empties in it
    Count1 is Count-1,
    getListOfAvailMoves(Count1, Board, [Count|Acc], ListTotal).
%CASE: col DOESNT has available move
getListOfAvailMoves(Count, Board, Acc, ListTotal) :-
    Count1 is Count-1,
    getListOfAvailMoves(Count1, Board, Acc, ListTotal).

columnFree(Column) :- member('_',Column).

getMove(Player, Move, ListOfMoves, Board, BoardAfter) :-
    member(Move, ListOfMoves),
    insertToBoard(Board, Move, Player, BoardAfter).
getMove(blue, Move, _, _, _) :-
    isConcede(Move),
    write('Blue player has conceded'),
    nl,
    nl,
    play.
getMove(red, Move, _, _, _) :-
    isConcede(Move),
    write('Red player has conceded'),
    nl,
    nl,
    play.
getMove(Player, _, ListOfMoves, Board, BoardAfter) :-
    write('Not valid move. Please select a valid move '),
    write(ListOfMoves),
    nl,
    read(NewMove),
    nl,
    getMove(Player, NewMove, ListOfMoves, Board, BoardAfter).


isConcede(Move) :- Move == qq.

insertToBoard(Board, Move, Player, BoardAfter) :-
    nth1(Move, Board, Col),
    insertColumn(Col, Player, ColNew),
    updateBoard(Board, Move, ColNew, BoardAfter).

insertColumn(['_',X|T], Player, [PlayerPiece, X|T]) :-
    playerPiece(Player, PlayerPiece),
    \+ X == '_'.
insertColumn(['_'], Player, [PlayerPiece]) :-
    playerPiece(Player, PlayerPiece).
insertColumn([H|T], Player, [H|R]) :-
    insertColumn(T, Player, R).

playerPiece(red, x).
playerPiece(blue, o).
playerPiece(empty, _).

updateBoard([_|T], 1, ColNew, [ColNew|T]).
updateBoard([H|T], Move, ColNew, [H|X]) :- Move1 is Move-1, updateBoard(T, Move1, ColNew, X).


displayPiece(empty) :- print('-').
displayPiece(red) :- print('X').
displayPiece(blue) :- print('O').

initBoard :- initBoardFull.
initBoardFull([
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_']
    ]).

% TODO
%================ ================ 
%AI 
machineTurn(_, Board, Board) :- win(Board, x).
machineTurn(Player, Board, BoardAfter) :-
    selectMove(Board, 4, Player, Move),
    insertToBoard(Board, Move, Player, BoardAfter).

% Select Move gets best move
% depth is how far you want to search into a path
selectMove(Board, Depth, Player, Move) :-
    columnScore(Board, Depth, Player, 1, Score1),
    columnScore(Board, Depth, Player, 2, Score2),
    columnScore(Board, Depth, Player, 3, Score3),
    columnScore(Board, Depth, Player, 4, Score4),
    columnScore(Board, Depth, Player, 5, Score5),
    columnScore(Board, Depth, Player, 6, Score6),
    columnScore(Board, Depth, Player, 7, Score7),
    selectBestScore([Score1,Score2,Score3,Score4,Score5,Score6,Score7], Move).


%columnScore(Board, Depth, Player, Column, Score)
columnScore(_,0,_,_,0).
columnScore(Board,_,_,_,0) :- full(Board).
columnScore(Board,_,_,Column,-42) :- \+ validCpuMove(Board, Column).
columnScore(Board,_,Player,Column,Score) :-
    canWin(Board, x, Column),
    movesLeft(Board, MovesLeft),
    Score is ((MovesLeft - 1)//2).
columnScore(Board, Depth, Player, Column, Score) :-
    validCpuMove(Board, Column),
    insertToBoard(Board, Column, Player, BoardAfter),
    flipPlayer(Player, Player2),
    Depth1 is Depth-1,
    columnScore(BoardAfter, Depth1, Player2, 1, S1),
    columnScore(BoardAfter, Depth1, Player2, 2, S2),
    columnScore(BoardAfter, Depth1, Player2, 3, S3),
    columnScore(BoardAfter, Depth1, Player2, 4, S4),
    columnScore(BoardAfter, Depth1, Player2, 5, S5),
    columnScore(BoardAfter, Depth1, Player2, 6, S6),
    columnScore(BoardAfter, Depth1, Player2, 7, S7),
    selectBestScore([S1,S2,S3,S4,S5,S6,S7], Max),
    Score is -1 * Max.

validCpuMove(Board, Move) :-
    integer(Move),
    Move >= 1,
    Move =< 7,
    nth1(Move, Board, [H|_]),
    H = '_'.


selectBestScore(ScoreList, Move) :-
    selectMax(ScoreList, Max),
    findBestMove(Move, ScoreList, Max).


findBestMove(Index, ScoreList, Score) :-
    findall(N, nth1(N, ScoreList, Score), Indexes),
    length(Indexes, Length),
    Bounds is Length+1,
    random(1, Bounds, Random),
    nth1(Random, Indexes, I),
    Index is I.

selectMax([], -42).
selectMax([H|T], Max) :-
    selectMax(T, Max2),
    Max is max(H,Max2).

movesLeft([], 0).
movesLeft([H|T], Moves) :-
    movesLeftColumn(H, Move1),
    movesLeft(T, Move2),
    Moves is Move1 + Move2.

movesLeftColumn([], 0).
movesLeftColumn([H|T], Moves) :-
    dif(H, '_'),
    movesLeftColumn(T, Moves).
movesLeftColumn(['_'|T], Moves) :-
    movesLeftColumn(T, Move1),
    Moves is Move1 + 1.

canWin(Board, Player, Column) :-
    validCpuMove(Board,Column),
    insertToBoard(Board, Column, blue, BoardAfter),
    win(BoardAfter, Player).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UI DISPLAY SECTION:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */



% display board row by row (?)
displayBoard(Board) :-
    transpose(Board, BoardNew),
    displayRows(BoardNew).

displayRows([]).
displayRows([H|T]) :-
    write(' |'),
    displaySquares(H),
    nl,
    displayRows(T).

displaySquares([]).
displaySquares([H|T]) :-
    write(H),
    write('|'),
    displaySquares(T).



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TESTING SECTION:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */



testBoardColWinX([
    ['_', '_', '_', '_', '_', 'o'],
    ['_', '_', 'x', 'x', 'x', 'x'],
    ['_', '_', '_', '_', '_', 'o'],
    ['_', '_', '_', '_', 'o', 'o'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_']
    ]).

testBoardRowWinO([
    ['_', '_', '_', '_', '_', 'o'],
    ['_', '_', 'x', 'x', 'x', 'o'],
    ['_', '_', '_', '_', '_', 'o'],
    ['_', '_', '_', '_', 'x', 'o'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_']
    ]).    

%testBoardColWinX(B), win(B, x). %B= matrix from testBoardColWinX
%testBoardColWinX(B), win(B, o). %false
%testBoardRowWinO(B), win(B, o). %B= matrix from testBoardRowWinO
%testBoardRowWinO(B), win(B, x). %false

%CHECKING WIN CONDITIONS LIVE:
%(input these as ?- queries.)
%testBoardColWinX(B), gameTurn(B, _). %Player1 wins
%testBoardRowWinO(B), gameTurn(B, _). %Player2 wins
%
%testBoardColWinX(B), gameTurnMachine(B, red, ai). %Player1 wins
%testBoardColWinX(B), gameTurnMachine(B, red, human). %Player1 wins
%testBoardRowWinO(B), gameTurnMachine(B, red, ai). %AI wins
%testBoardRowWinO(B), gameTurnMachine(B, red, human). %AI wins



testBoardDiagWinX([
    ['_', '_', 'x', 'o', 'o', 'x'],
    ['_', '_', '_', 'x', 'o', 'o'],
    ['_', '_', '_', '_', 'x', 'o'],
    ['_', '_', '_', '_', '_', 'x'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_']
    ]).

testBoardDiagWinO([
    ['_', '_', '_', '_', '_', 'o'],
    ['_', '_', '_', '_', 'o', 'x'],
    ['_', '_', '_', 'o', 'x', 'x'],
    ['_', '_', 'o', 'x', 'x', 'o'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_']
    ]).

%testBoardDiagWinX(B), win(B, x). %B= matrix from testBoardDiagWinX
%testBoardDiagWinX(B), win(B, o). %false
%testBoardDiagWinO(B), win(B, o). %B= matrix from testBoardDiagWinO
%testBoardDiagWinO(B), win(B, x). %false

%CHECKING WIN CONDITIONS LIVE:
%(input these as ?- queries.)
%testBoardDiagWinX(B), gameTurn(B, _). %Player1 wins
%testBoardDiagWinO(B), gameTurn(B, _). %Player2 wins

%testBoardDiagWinX(B), gameTurnMachine(B, red, human). %Player1 wins
%testBoardDiagWinX(B), gameTurnMachine(B, red, ai). %Player1 wins
%testBoardDiagWinO(B), gameTurnMachine(B, red, human). %AI wins
%testBoardDiagWinO(B), gameTurnMachine(B, red, ai). %AI wins

testBoardIsFullDraw([
    ['x', 'x', 'x', 'o', 'o', 'o'],
    ['x', 'o', 'o', 'x', 'x', 'x'],
    ['o', 'x', 'x', 'x', 'o', 'o'],
    ['x', 'x', 'o', 'o', 'x', 'o'],
    ['o', 'o', 'x', 'o', 'x', 'o'],
    ['o', 'o', 'x', 'o', 'x', 'x'],
    ['x', 'x', 'o', 'x', 'o', 'o']
    ]).

%testBoardIsFullDraw(B), win(B, o). %false
%testBoardIsFullDraw(B), win(B, x). %false

%CHECKING WIN CONDITIONS LIVE:
%(input these as ?- queries.)
%testBoardIsFullDraw(B), gameTurn(B, _). %Tie Game

%testBoardIsFullDraw(B), gameTurnMachine(B, red, human). %Tie Game
%testBoardIsFullDraw(B), gameTurnMachine(B, red, ai). %Tie Game


testBoardHalfFull([
    ['x', 'x', 'x', 'o', 'o', 'o'],
    ['x', 'o', 'o', 'x', 'x', 'x'],
    ['o', 'x', 'x', 'x', 'o', 'o'],
    ['_', '_', '_', 'x', 'x', 'o'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_']
    ]).

%testBoardHalfFull(B), win(B, o). %false
%testBoardHalfFull(B), win(B, x). %false
%
%%CHECKING WIN CONDITIONS LIVE:
%(input these as ?- queries.)
%testBoardHalfFull(B), gameTurnMachine(B, red, human).
%testBoardHalfFull(B), gameTurnMachine(B, red, ai).

/*
Acknowledgements and references:
https://github.com/rvinas/connect-4-prolog/blob/master/connect4.pl
https://github.com/csatterfield/connect_four/blob/master/connect_four.pl
*/