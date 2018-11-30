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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UI DISPLAY SECTION:
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
    %sleep(2),
    nl,
    print('GO!'),
    nl,
    initBoardFull(B),
    displayBoard(B),
    gameTurn(B, red).

play_mode(2) :- print('Playing against AI'),
    nl,
    print('Ready... 3. 2.. 1...'),
    sleep(2),
    nl,
    print('GO!'),
    nl,
    initBoardFull(B),
    displayBoard(B),
    gameTurnMachine(B, red).

play_mode(_) :-
    print('Good bye'),
    abort.

gameTurn(Board, _) :- win(Board, x), write('Congratulations Player 1 Wins!').
gameTurn(Board, _) :- win(Board, o), write('Congratulations Player 2 Wins!').
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
    %TODO: display list of available moves here
    %ONLY list of moves, or concede,
    % are available to players.
    read(Move),
    getMove(Player, Move, ListTotal, Board, BoardAfter),
%    userMove(Board, Move, BoardAfter),
%    insertToBoard(Board, Move, Player, BoardAfter),
    %displayBoard(BoardAfter),
    displayBoard(BoardAfter), %TODO replace for real game board
    flipPlayer(Player, OtherPlayer),
    gameTurn(BoardAfter, OtherPlayer).

gameTurnMachine(Board, Player) :-
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
    nl,
    write('Machine\'s turn'),
    nl,
    machineTurn(blue, BoardAfter, BoardMachine),
    displayBoard(BoardMachine),
    gameTurnMachine(BoardMachine, Player).

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

% Checks if board is full
full(Board) :-
    \+ (append(_, [Column|_], Board),
        append(_, [-|_], Column)).

%Use this for getting opposite player.
%Use as flipPlayer(Player, OtherPlayer).
flipPlayer(red, blue).
flipPlayer(blue, red).

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
    write('Red player has condeded'),
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

updateBoard([_|T], 1, ColNew, [ColNew|T]).
updateBoard([H|T], Move, ColNew, [H|X]) :- Move1 is Move-1, updateBoard(T, Move1, ColNew, X).


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
machineTurn(_, Board, Board) :- win(Board, red).
machineTurn(Player, Board, BoardAfter) :-
    selectMove(Board, 3, Player, Move),
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
    canWin(Board, Player, Column),
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
    nth1(Move, Board, (H|_)),
    H == '_'.


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

selectMin([], 42).
selectMin([H|T], Min) :-
    selectMin(T,Min2),
    Min is min(H,Min2).

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
    insertToBoard(Board, Column, Player, BoardAfter),
    win(BoardAfter, Player).

displayBoardExample :-
    write(' |_|_|_|_|_|_|_| '),
    nl,
    write(' |_|_|_|_|_|_|_| '),
    nl,
    write(' |_|_|_|_|_|_|_| '),
    nl,
    write(' |_|_|_|_|_|_|_| '),
    nl,
    write(' |_|_|_|_|_|_|_| '),
    nl,
    write(' |_|_|_|_|_|_|_| ').

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
