lexer(Tokens) -->
	white_space,
	(
		(
			":=", !, { Token = tokAssign }
			;			
			",", !, { Token = tokComa }
			;
			";", !, { Token = tokSemicolon }
			;
			"+", !, { Token = tokPlus }
			;
			"-", !, { Token = tokMinus }
			;
			"*", !, { Token = tokTimes }
			;
			"(", !, { Token = tokLeftParenthese }
			;
			")", !, { Token = tokRightParenthese }
			;
			"=", !, { Token = tokEquals }			
			;
			"<>", !, { Token = tokNotEquals }
			;
			">=", !, { Token = tokEqGreater }
			;			
			"<=", !, { Token = tokEqLower }
			;
			"<", !, { Token = tokLowerThan } 
			;
			">", !, { Token = tokGreaterThan }
			;
			
			digit(D), !,
			number(D, N), { Token = tokNumber(N) }
			;
			letter(L), !,
			identifier(L, Id),
			{
				member(
					(Id, Token),
					[
						(write, tokWrite),
						(begin, tokBegin),
						(end, tokEnd),
						(program, tokProgram),
						(read, tokRead),
						(local,tokLocal),
						(div, tokDiv),
						(mod, tokMod),
						(and, tokAnd),
						(or, tokOr),
						(not, tokNot),
						(if, tokIf),
						(fi, tokFi),
						(then, tokThen),
						(else, tokElse),
						(while,tokWhile),
						(do, tokDo),
						(done, tokDone),
						(return, tokReturn)
					]
				), !
				;
				Token = tokId(Id)
			}
			;
			[_],
			{ Token = tokUnknown }
		), !,
		{ Tokens = [Token|TokList]}, lexer(TokList) 
		;
		[], { Tokens = [] }
	).

digit(D) -->
	[D],
	{ code_type(D, digit) }.

digits([D|T]) -->
	digit(D), !,
	digits(T).
digits([]) -->
	[].

number(D, N) -->
	digits(Ds),
	{ number_chars(N, [D|Ds]) }.				

letter(L) -->
	[L],
	{ code_type(L, alpha) }.

alphanum([A|T]) -->
	[A],
	{ code_type(A, alnum) % TUTAJ ZMIANA - może być w identyfikatorze apostrof oraz podkreślnik
	  ;
	  code_type(A,quote)
	  ;
	  code_type(A,csym) 
	}, !, 
	alphanum(T).
	
alphanum([]) -->
	[].

	
identifier(L, Id) -->
	alphanum(As),
	{ atom_codes(Id, [L|As]) }.

white_space -->
	[Char], { code_type(Char, space) }, !, white_space.

white_space -->
	"(*", !,
	commentary, white_space.
   
white_space --> [].

commentary -->
	"*)", !.

commentary -->
    [_],!,
    commentary.




/* ------------------------------------ PARSOWANIE ------------------------------ */


additive_op(+) -->
   [tokPlus], !.
additive_op(-) -->
   [tokMinus].

multiplicative_op(*) -->
   [tokTimes], !.
multiplicative_op(//) -->
   [tokDiv], !.
multiplicative_op(mod) -->
   [tokMod].

relative_op(<) -->
	[tokLowerThan], !.
relative_op(<=) -->
	[tokEqLower], !.
relative_op(>) -->
	[tokGreaterThan], !.   
relative_op(>=) -->
	[tokEqGreater], !.
relative_op(=) -->
	[tokEquals], !.
relative_op(<>) -->
	[tokNotEquals].
	

arith_expr(Expr) -->
   element(Element), arith_expr(Element, Expr).

arith_expr(Acc, Expr) -->
   additive_op(Op), !, element(Element),
   { Acc1 = additiveNode(Op, Acc, Element) },
   arith_expr(Acc1, Expr).

arith_expr(Acc, Acc) -->
   [].

element(Expr) -->
   factor(Factor),
   element(Factor, Expr).

element(Acc, Expr) -->
   multiplicative_op(Op), !, factor(Factor),
   { Acc1 = multiplicativeNode(Op, Acc, Factor) },
   element(Acc1, Expr).
element(Acc, Acc) -->
   [].

factor(Expr) --> 
	[tokMinus], simple_expr(Expr1), {Expr = minus(Expr1)}
	;
	simple_expr(Expr).


simple_expr(Expr) -->
   (  [tokLeftParenthese], !, arith_expr(Expr), [tokRightParenthese]
   ;  [tokNumber(N)], !, { Expr = const(N) }
   ;  variable(VarName), { Expr = variable(VarName) }
   ).


   

logical_expr(Expr) -->
	conjunction(Left), logical_expr(Left,Expr).

logical_expr(Acc,Expr) -->
	[tokOr], !, conjunction(Right),
	{ Acc1 = dysjunctionNode(or,Acc,Right) },
	logical_expr(Acc1,Expr).
	
logical_expr(Acc,Acc) --> [].

conjunction(Expr) -->
	condition(Left), conjunction(Left,Expr).

conjunction(Acc,Expr) -->
	[tokAnd], condition(Right),
	{ Acc1 = conjunctionNode(and,Acc,Right) },
	conjunction(Acc1,Expr).

conjunction(Acc,Acc) --> [].

condition(Expr) -->
	relative_expr(Expr)
	;
	[tokNot], relative_expr(Expr1), { Expr = notNode(Expr1) }.

relative_expr(Expr) -->
	[tokLeftParenthese], logical_expr(Expr), [tokRightParenthese]
	;
	arith_expr(Left), relative_op(Op), arith_expr(Right),
	{ Expr = relativeNode(Op, Left, Right) }.

 
   
	
program(Program) -->
	[tokProgram],
	[tokId(Id)],
	block(Block),
	{ Program = programNode(Id, Block) }.
	
block(Block) -->
	declarations(VarList), 
	[tokBegin],
	complex_instruction(InstructionsList), 
	[tokEnd],
	{ Block = blockNode(VarList, InstructionsList) }.  



complex_instruction([Instruction]) --> instruction(Instruction).
complex_instruction([Instruction | Rest]) -->
	instruction(Instruction),
	[tokSemicolon],
	complex_instruction(Rest).


instruction(Instruction) -->
	[tokWrite], !,  arith_expr(Expr),
	{ Instruction = writeNode(Expr) }
	;
	[tokRead], !,  variable(VarName),
	{ Instruction = readNode(VarName) }
	;
	variable(VarName), !,  [tokAssign], arith_expr(Expr),
	{ Instruction = assignNode(VarName, Expr) }
	;
	[tokIf], !, logical_expr(Expr), [tokThen], complex_instruction(List),
	(
		[tokElse], !, complex_instruction(ListElse), [tokFi],
		{ Instruction = ifElseNode(if, Expr, then, List, else, ListElse) }
		;
		[tokFi], { Instruction = ifNode(if, Expr, then, List) }
		
	)
	;
	[tokWhile], !, logical_expr(Expr), [tokDo], complex_instruction(List), [tokDone],
	{ Instruction = whileNode(while,Expr,do,List) }
	;
	[tokReturn], arith_expr(Expr),
	{ Instruction = return(Expr) }.
	

	

declarations([]) --> [].
declarations(List) -->
	declaration(VarList),
	{ append(VarList, Rest, List) },
	declarations(Rest).
	
declaration(VarNames) -->
	declarator(VarNames).


declarator(VarNames) -->
	[tokLocal],
	variables(VarNames).

variables([VarName]) --> variable(VarName).
variables(VarNames)	-->
	variable(VarName),
	[tokComa], 
	{ VarNames = [VarName | Rest] },
	variables(Rest).
	
variable(VarName) -->
	[tokId(VarName)].
	

parse(CharCodeList, AST) :-
	phrase(lexer(TokList), CharCodeList),
	phrase(program(AST), TokList).	
	

	
	
	
	
/*---------------------------TRANSLACJA DO MAKRO-ASEMBLERA------------------------------------------*/	

%ETYKIETA
label(_).


%alokacja pamięci dla zmiennych na samym początku programu, wskaźnik stosu ustawiony zaraz za adresami zmiennych
allocate_mem([], [], StackPointer, StackPointer).
allocate_mem([VarName | VarList], [(VarName,N) | ResVarList], N, StackPointer) :-
	N1 is N - 1,
	allocate_mem(VarList, ResVarList, N1, StackPointer).
	
	 	
%program	
translate(programNode(_, Block), SextiumTokens) :-
	translate(Block, SextiumTokens).

%blok	
translate(blockNode(VarList, InstructionsList), SextiumTokens) :-
	allocate_mem(VarList, VarAdresses, 65534, StackPointer), %alokacja pamięci dla zmiennych
	LR1 is StackPointer - 0, % Tworzymy rejestr dla wyrażeń logicznych
	LR2 is LR1 - 1, 		 % Tworzymy drugi rejestr dla wyrażeń logicznych
	StackPointer1 is LR2 - 1,
	translate(LR1, LR2, StackPointer1, VarAdresses, InstructionsList, SextiumTokens).

%instrukcja złożona
translate(LR1, LR2, _, _, [], []) :- !.
translate(LR1, LR2, StackPointer, VarList, [Instruction | Rest], SextiumTokens) :-
	translate(LR1, LR2, StackPointer, VarList, Instruction, SextiumHead),
	translate(LR1, LR2, StackPointer, VarList, Rest, SextiumTail),
	append(SextiumHead, SextiumTail, SextiumTokens).
	
%instrukcja write
translate(LR1, LR2, StackPointer, VarList, writeNode(Expr),SextiumTokens) :- 
	translate(LR1, LR2, StackPointer, VarList, Expr, ExprTokens),
	append(ExprTokens, [swapD, const(2), syscall], SextiumTokens).

%instrukcja assign
translate(LR1, LR2, StackPointer, VarList, assignNode(VarName, Expr), SextiumTokens) :-
		translate(LR1, LR2, StackPointer, VarList, Expr, ExprTokens),
		member((VarName, Adress), VarList),
		append(ExprTokens, [swapD, const(Adress), swapA, swapD, store], SextiumTokens).
	
%instrukcja read	
translate(LR1, LR2, StackPointer, VarList, readNode(VarName), [const(N), swapA, const(1), syscall, store]) :-
	 member((VarName,N), VarList).


%instrukcja if-bez-elsa
translate(LR1, LR2, StackPointer, VarList, ifNode(if, Expr, then, Instructions), SextiumTokens) :-
	translate(LR1, LR2, StackPointer, VarList, Expr, SextiumHead),
	translate(LR1, LR2, StackPointer, VarList, Instructions, SextiumTail),
	label(E),
	append(SextiumHead, [swapD, const(E), swapA, swapD, branchZ], List1),
	append(List1, SextiumTail, List2),
	append(List2, [label(E)], SextiumTokens).
	 
%instrukcja if-z-elsem
translate(LR1, LR2, StackPointer, VarList, ifElseNode(if, Expr, then, ListIf, else, ListElse), SextiumTokens) :-
	translate(LR1, LR2, StackPointer, VarList, Expr, SextiumExpr),
	translate(LR1, LR2, StackPointer, VarList, ListIf, SextiumIf),
	translate(LR1, LR2, StackPointer, VarList, ListElse, SextiumElse),
	label([E1, E2]),
	% branchZ(E1) = [swapD, const(E1), swapA, swapD, branchZ]
	append(SextiumExpr, [swapD, const(E1), swapA, swapD, branchZ], List1),
	append(List1, SextiumIf, List2),
	% jump(E2) = [const(E2), jump]
	append(List2, [const(E2), jump, label(E1)], List3),
	append(List3, SextiumElse, List4),
	append(List4, [label(E2)], SextiumTokens).

%instrukcja while
translate(LR1, LR2, StackPointer, VarList, whileNode(while,Expr,do,List), SextiumTokens) :-
	translate(LR1, LR2, StackPointer, VarList, Expr, SextiumExpr),
	translate(LR1, LR2, StackPointer, VarList, List, SextiumInstr),
	label([E1,E2]),
	append([label(E2)], SextiumExpr, SextiumExpr1),
	append(SextiumExpr1, [swapD, const(E1), swapA, swapD, branchZ], List1),
	append(List1, SextiumInstr, List2),
	append(List2, [const(E2), jump, label(E1)], SextiumTokens).




	
%węzeł or -  dodajemy zwartościowane dwa dysjunkty i powinniśmy jeszcze odjąć ich iloczyn - tego nie dokończyłem
translate(LR1, LR2, StackPointer, VarList, dysjunctionNode(or, Acc, Right), SextiumTokens) :-
	translate(LR1, LR2, StackPointer, VarList, Acc, SextiumHead),
	StackPointer1 is StackPointer - 1,
	translate(LR1, LR2, StackPointer1, VarList, Right, SextiumTail),
	append(SextiumHead, [swapD, const(StackPointer), swapA, swapD, store], NewSextiumHead),
	append(SextiumTail, [swapD, const(StackPointer), swapA, load, add], NewSextiumTail),
	append(NewSextiumHead, NewSextiumTail, SextiumTokens). 

%węzeł and - mnożymy przez siebie wynik wartościowania dwóch koniunktów - przy wyniku mnożenia równemu jeden wiemy, że dwa koniunkty są prawdziwe
translate(LR1, LR2, StackPointer, VarList, conjunctionNode(and, Acc, Right), SextiumTokens) :-
	translate(LR1, LR2, StackPointer, VarList, Acc, SextiumHead),
	StackPointer1 is StackPointer - 1,
	translate(LR1, LR2, StackPointer1, VarList, Right, SextiumTail),
	append(SextiumHead, [swapD, const(StackPointer), swapA, swapD, store], NewSextiumHead),
	append(SextiumTail, [swapD, const(StackPointer), swapA, load, mult], NewSextiumTail), 
	append(NewSextiumHead, NewSextiumTail, SextiumTokens).

%węzeł not - działanie: ACC = (ACC-1)^2 - wiemy, że na tym poziomie mamy już zwartościowane boolowsko wyrażenie relacyjne
translate(LR1, LR2, StackPointer, VarList, notNode(Expr), SextiumTokens) :-
		translate(LR1, LR2, StackPointer, VarList, Expr, SextiumHead),
		append(SextiumHead, [swapD, const(1), swapD, sub, swapD, const(0), add, mult], SextiumTokens).


%węzeł relative
translate(LR1, LR2, StackPointer, VarList, relativeNode(Op, LeftExpr, RightExpr), SextiumTokens) :-
			translate(LR1, LR2, StackPointer, VarList, LeftExpr, SextiumHead),
			StackPointer1 is StackPointer - 1,
			%wyszło trochę spaghetti, ale przynajmniej edge case'y obsługuje
			translate(LR1, LR2, StackPointer1, VarList, RightExpr, SextiumTail),
			(
				Op = '=',
				append(SextiumHead, [swapD, const(StackPointer), swapA, swapD, store, swapD, const(E1), swapA, swapD, branchN, %skocz do label(E1), jeśli wyrażenie po lewej stronie jest ujemne.
				const(1), swapD, const(LR1), swapA, swapD, store,const(E2), jump, %w przeciwnym razie wyrażenie po lewej >= 0, więc do rejestru LR1 zapisujemy wartość 1 i skaczemy do label(E2)
				label(E1), const(0), swapD, const(LR1), swapA, swapD, store, label(E2)], NewSextiumHead), %skoczyliśmy do label(E1), więc do rejestru LR1 zapisujemy wartość 0
									
									
				append(SextiumTail, [swapD, const(StackPointer1), swapA, swapD, store, swapD, const(E3), swapA, swapD, branchN, %skocz do label(E3), jeśli wyrażenie po prawej stronie jest ujemne
				const(1), swapD, const(LR2), swapA, swapD, store, const(E4), jump, %w przeciwnym razie wyrażenie po prawej >= 0, więc do rejestru LR2 zapisujemy wartość 1 i skaczemy do label(E4)
				label(E3), const(0), swapD, const(LR2), swapA, swapD, store, %skoczyliśmy do label(E3), więc do rejestru LR1 zapisujemy wartość 0
				label(E4), const(LR2), swapA, load, swapD, const(LR1), swapA, load, sub, swapD, const(E5), swapA, swapD, branchZ, %wykonaj działanie ACC = LR1 - LR2: jeśli ACC = 0, to obie liczby mają ten sam znak
				const(E7), jump ,label(E5), const(StackPointer1), swapA, load, swapD, const(StackPointer), swapA, load, sub, %jeśli jednak nie mają, to skocz do label(E7), a jeśli mają, to wykonaj ACC = LEWE WYR - PRAWE WYR
				swapD, const(E6), swapA, swapD, branchZ, const(E7), jump, % Jeśli wynik jest równy zero, to wyrażenie jest prawdą i skaczemy do label(E6)
				label(E6), const(1), swapD, const(E8), jump,  label(E7), const(0), swapD, label(E8), swapD], NewSextiumTail) %jeśli wyrażenie okazało się prawdą (label(E6)), to DR = 1 i skocz do label(E8), wpp ACC = 0
				
				;
				
				Op = '<',
				append(SextiumTail, [swapD, const(StackPointer1), swapA, swapD, store, swapD, const(E1), swapA, swapD, branchN, const(1), swapD, const(LR2), swapA, swapD, store,const(E2), jump,
				label(E1), const(0), swapD, const(LR2), swapA, swapD, store, label(E2)], NewSextiumHead), %początek identyczny, tylko obliczamy najpierw wyrażenie z prawej i sprawdzamy znak
			
				
				append(SextiumHead, [swapD, const(StackPointer), swapA, swapD, store, swapD, const(E3), swapA, swapD, branchN, %obliczamy wyrażenie z lewej i sprawdzamy znak
				const(1), swapD, const(LR1), swapA, swapD, store, const(E4), jump, %ciąg dalszy
				label(E3), const(0), swapD, const(LR1), swapA, swapD, store, label(E4), const(LR2), swapA, load, swapD, const(LR1), swapA, load, sub, swapD, const(E5), swapA, swapD, branchZ, %ciąg dalszy
				swapD, const(E6), swapA, swapD, branchN, const(E7), jump, %jeśli wyrażenie po lewej jest ujemne, a po prawej dodatnie (LR1 = 0, LR2 = 1), to skocz do label(E6), wpp skocz do label(E7)
				label(E5), const(StackPointer1), swapA, load, swapD, const(StackPointer), swapA, load, sub, swapD, const(E6), swapA, swapD, branchN, const(E7), jump, %wyrażenia mają ten sam znak - można je odjąć od siebie
				label(E6), const(1), swapD, const(E8), jump, label(E7), const(0), swapD, label(E8), swapD], NewSextiumTail)
				
				;
				
				Op = '>',
				append(SextiumTail, [swapD, const(StackPointer1), swapA, swapD, store, swapD, const(E1), swapA, swapD, branchN, const(1), swapD, const(LR2), swapA, swapD, store,const(E2), jump,
				label(E1), const(0), swapD, const(LR2), swapA, swapD, store, label(E2)], NewSextiumHead), %początek identyczny, tylko obliczamy najpierw wyrażenie z prawej i sprawdzamy znak
			
				
				append(SextiumHead, [swapD, const(StackPointer), swapA, swapD, store, swapD, const(E3), swapA, swapD, branchN, %obliczamy wyrażenie z lewej i sprawdzamy znak
				const(1), swapD, const(LR1), swapA, swapD, store, const(E4), jump, %to samo
				label(E3), const(0), swapD, const(LR1), swapA, swapD, store, label(E4), const(LR1), swapA, load, swapD, const(LR2), swapA, load, sub, swapD, const(E5), swapA, swapD, branchZ, %to samo
				swapD, const(E6), swapA, swapD, branchN, const(E7), jump, %jeśli wyrażenie po lewej jest dodatnie, a po prawej ujemne (LR1 = 1, LR2 = 0), to skocz do label(E6), wpp skocz do label(E7)
				label(E5), const(StackPointer), swapA, load, swapD, const(StackPointer1), swapA, load, sub, swapD, const(E6), swapA, swapD, branchN, const(E7), jump, %wyrażenia mają ten sam znak - można je odjąć od siebie
				label(E6), const(1), swapD, const(E8), jump, label(E7), const(0), swapD, label(E8), swapD], NewSextiumTail)
				
				;
				
				Op = '<=',
				append(SextiumTail, [swapD, const(StackPointer1), swapA, swapD, store, swapD, const(E1), swapA, swapD, branchN, const(1), swapD, const(LR2), swapA, swapD, store,const(E2), jump,
				label(E1), const(0), swapD, const(LR2), swapA, swapD, store, label(E2)], NewSextiumHead), %początek identyczny, tylko obliczamy najpierw wyrażenie z prawej i sprawdzamy znak
			
				
				append(SextiumHead, [swapD, const(StackPointer), swapA, swapD, store, swapD, const(E3), swapA, swapD, branchN, %obliczamy wyrażenie z lewej i sprawdzamy znak
				const(1), swapD, const(LR1), swapA, swapD, store, const(E4), jump, %to samo
				label(E3), const(0), swapD, const(LR1), swapA, swapD, store, label(E4), const(LR2), swapA, load, swapD, const(LR1), swapA, load, sub, swapD, const(E5), swapA, swapD, branchZ, %to samo
				swapD, const(E6), swapA, swapD, branchN, const(E7), jump, %jeśli wyrażenie po lewej jest ujemne, a po prawej dodatnie (LR1 = 0, LR2 = 1), to skocz do label(E6), wpp skocz do label(E7)
				label(E5), const(StackPointer1), swapA, load, swapD, const(StackPointer), swapA, load, sub, swapD, const(E6), swapA, swapD, branchN, branchZ, const(E7), jump, %wyrażenia mają ten sam znak - można je odjąć od siebie
				label(E6), const(1), swapD, const(E8), jump, label(E7), const(0), swapD, label(E8), swapD], NewSextiumTail)
				
				;
				
				Op = '>=',
				append(SextiumTail, [swapD, const(StackPointer1), swapA, swapD, store, swapD, const(E1), swapA, swapD, branchN, const(1), swapD, const(LR2), swapA, swapD, store,const(E2), jump,
				label(E1), const(0), swapD, const(LR2), swapA, swapD, store, label(E2)], NewSextiumHead), %początek identyczny, tylko obliczamy najpierw wyrażenie z prawej i sprawdzamy znak
			
				
				append(SextiumHead, [swapD, const(StackPointer), swapA, swapD, store, swapD, const(E3), swapA, swapD, branchN, %obliczamy wyrażenie z lewej i sprawdzamy znak
				const(1), swapD, const(LR1), swapA, swapD, store, const(E4), jump, %to samo
				label(E3), const(0), swapD, const(LR1), swapA, swapD, store, label(E4), const(LR1), swapA, load, swapD, const(LR2), swapA, load, sub, swapD, const(E5), swapA, swapD, branchZ, %to samo
				swapD, const(E6), swapA, swapD, branchN, const(E7), jump, %jeśli wyrażenie po lewej jest dodatnie, a po prawej ujemne (LR1 = 1, LR2 = 0), to skocz do label(E6), wpp skocz do label(E7)
				label(E5), const(StackPointer), swapA, load, swapD, const(StackPointer1), swapA, load, sub, swapD, const(E6), swapA, swapD, branchN, branchZ, const(E7), jump, %wyrażenia mają ten sam znak - można je odjąć od siebie
				label(E6), const(1), swapD, const(E8), jump, label(E7), const(0), swapD, label(E8), swapD], NewSextiumTail)
				
				;
				
				Op = '<>',
				append(SextiumHead, [swapD, const(StackPointer), swapA, swapD, store, swapD, const(E1), swapA, swapD, branchN, %skocz do label(E1), jeśli wyrażenie po lewej stronie jest ujemne.
				const(1), swapD, const(LR1), swapA, swapD, store,const(E2), jump, %w przeciwnym razie wyrażenie po lewej >= 0, więc do rejestru LR1 zapisujemy wartość 1 i skaczemy do label(E2)
				label(E1), const(0), swapD, const(LR1), swapA, swapD, store, label(E2)], NewSextiumHead), %skoczyliśmy do label(E1), więc do rejestru LR1 zapisujemy wartość 0
									
									
				append(SextiumTail, [swapD, const(StackPointer1), swapA, swapD, store, swapD, const(E3), swapA, swapD, branchN, %jak wyżej, tylko dotyczy wyrażenia po prawej stronie
				const(1), swapD, const(LR2), swapA, swapD, store, const(E4), jump, %jak wyżej, tylko dotyczy rejestru LR2
				label(E3), const(0), swapD, const(LR2), swapA, swapD, store, %j.w.
				label(E4), const(LR2), swapA, load, swapD, const(LR1), swapA, load, sub, swapD, const(E5), swapA, swapD, branchZ, %wykonaj działanie ACC = LR1 - LR2, jeśli ACC = 0, to obie liczby mają ten sam znak
				const(E7), jump ,label(E5), const(StackPointer1), swapA, load, swapD, const(StackPointer), swapA, load, sub, %skocz do label(E7), jeśli jednak nie mają, a jeśli mają, to wykonaj ACC = LEWE WYR - PRAWE WYR
				swapD, const(E6), swapA, swapD, branchZ, const(E7), jump, % Jeśli wynik jest równy zero, to wyrażenie jest fałszem i skaczemy do label(E6)
				label(E6), const(0), swapD, const(E8), jump,  label(E7), const(1), swapD, label(E8), swapD], NewSextiumTail) %jeśli wyrażenie okazało się fałszem (label(E6)), to DR = 0 i skocz do label(E8), wpp ACC = 1
				
				
				
			),
			append(NewSextiumHead, NewSextiumTail, SextiumTokens).
		




		
%węzeł addytywny	
translate(LR1, LR2, StackPointer, VarList, additiveNode(Op,Acc,Right), SextiumTokens) :-
	translate(LR1, LR2, StackPointer, VarList, Acc, SextiumHead),
	StackPointer1 is StackPointer - 1,
	translate(LR1, LR2, StackPointer1, VarList, Right, SextiumTail),
	append(SextiumHead, [swapD, const(StackPointer), swapA, swapD, store], SextiumLeft),
	( 
	Op = '-', 
	append(SextiumTail, [swapD, const(StackPointer), swapA, load, sub], SextiumRight)
	;
	Op = '+',
	append(SextiumTail, [swapD, const(StackPointer), swapA, load, add], SextiumRight)
	),
	append(SextiumLeft, SextiumRight, SextiumTokens).
	
%węzeł multiplikatywny
translate(LR1, LR2, StackPointer, VarList, multiplicativeNode(Op,Acc,Right), SextiumTokens) :-
	translate(LR1, LR2, StackPointer, VarList, Acc, SextiumHead),
	StackPointer1 is StackPointer - 1,
	translate(LR1, LR2, StackPointer1, VarList, Right, SextiumTail),
	append(SextiumHead, [swapD, const(StackPointer), swapA, swapD, store], SextiumLeft),
	( 
	Op = '*', 
	append(SextiumTail, [swapD, const(StackPointer), swapA, load, mult], SextiumRight)
	;
	Op = '//',
	append(SextiumTail, [swapD, const(StackPointer), swapA, load, div], SextiumRight)
	;
	Op = 'mod',
	append(SextiumTail, [swapD, const(StackPointer), swapA, load, div, swapD, const(65520), swapD, shift], SextiumRight)
	),
	append(SextiumLeft, SextiumRight, SextiumTokens).

%węzeł z unarnym minusem	
translate(LR1, LR2, StackPointer, VarList, minus(Expr),SextiumTokens) :-
		translate(LR1, LR2, StackPointer, VarList, Expr, SexTok),
		append(SexTok, [swapD, const(0), sub], SextiumTokens).
	
translate(LR1, LR2, StackPointer, VarList, const(N), [const(N)]).
translate(LR1, LR2, StackPointer, VarList, variable(VarName), [const(Adress), swapA, load]) :- member((VarName, Adress), VarList).

/*----------------------------------------------------------------------------------------------------------------------------------------*/









															/*------------------------ TRANSLACJA Z MAKROASEMBLERA DO KODU WYNIKOWEGO ----------------------------------*/

% PAKOWANIE SŁÓW BEZ LABELI ORAZ SAMYCH LABELI OSOBNO
pack([H1, H2, H3, H4 | SextiumTokens], [ [H1, H2, H3, H4] | QuadraSextiumTokens]) :-
	\+ H1 = label(_), \+ H2 = label(_), \+ H3 = label(_), \+ H4 = label(_),
	pack(SextiumTokens, QuadraSextiumTokens).

pack([H1, H2, H3 | SextiumTokens], [ [H1, H2, H3] | QuadraSextiumTokens ] ) :-
	\+ H1 = label(_), \+ H2 = label(_), \+ H3 = label(_),
	pack(SextiumTokens, QuadraSextiumTokens).

pack([H1, H2 | SextiumTokens], [ [H1, H2] | QuadraSextiumTokens] ) :-
	\+ H1 = label(_), \+ H2 = label(_),
	pack(SextiumTokens, QuadraSextiumTokens).

pack([H1 | SextiumTokens], [ [H1] | QuadraSextiumTokens]) :-
	\+ H1 = label(_),
	pack(SextiumTokens, QuadraSextiumTokens).
	
pack([ label(X) | SextiumTokens ], [ [label(X)] | QuadraSextiumTokens]) :-
	pack(SextiumTokens, QuadraSextiumTokens).

pack([], [[const(0), syscall]]).


% TŁUMACZENIE LISTY SŁÓW MAKROASSEMBLERA, A DOKŁADNIEJ PREDYKAT CONST(N) NA SAMO CONST ORAZ LICZBĘ N ZNAJDUJĄCEJ SIĘ W NASTĘPNYM SŁOWIE
translate_quadras([Word | Rest], [NewWord, Numbers | NewRest]) :-
	translate_word(Word, NewWord, Numbers),
	translate_quadras(Rest, NewRest).
translate_quadras([],[]).

% TŁUMACZENIE POJEDYNCZEGO SŁOWA - PRZYKŁAD : DLA SŁOWA [const(N1), swapA, swapD, const(N2)] przetłumaczy: [const, swapA, swapD, const] oraz zaraz za tą listą wstawi listę liczb [N1, N2]
translate_word([const(N) | Rest], [const | Result], [N | Numbers]) :-
	translate_word(Rest, Result, Numbers).
translate_word([Sth | Rest], [Sth | Result], Numbers) :-
	translate_word(Rest, Result, Numbers).
translate_word([],[], []).

% JEŚLI W JEDNYM SŁOWIE BYŁO WIĘCEJ NIŻ JEDNO CONST(N), TO ZDARZĄ SIĘ LISTY TYPU [LICZBA1, LICZBA2] - TEN PREDYKAT DZIELI JE NA OSOBNE LISTY JEDNOELEMENTOWE
cut([ [A,B] | Rest ], [ [A], [B] | T]) :-
	number(A), !, cut(Rest, T).
cut([ [A, B, C] | Rest ], [ [A], [B], [C] | T ]) :-
	number(A), !, cut(Rest, T).
cut( [NotNumber | T], [NotNumber | Rest]) :-
	cut(T, Rest).
cut([],[]).

% MOGŁO SIĘ TEŻ ZDARZYĆ, ŻE WRZUCONO LISTY PUSTE TAM, GDZIE CONST(N) W SŁOWIE NIE WYSTĄPIŁO - TEN PREDYKAT WYRZUCA JE Z LISTY
clear([[] | Rest], NewRest) :- !,
	clear(Rest, NewRest).
clear([Word | Rest], [Word | NewRest]) :- 
	clear(Rest, NewRest).
clear([], []).


% UNIFIKACJA ADRESÓW OPATRZONYCH LABELAMI I WYRZUCENIE LABELI Z LISTY SŁÓW
unif_labels(ListOfWords, Result) :- unif_labels(ListOfWords, Result, 0).

unif_labels([ [X] | Rest ], [ [X] | NewRest ], WordNumber) :-
	var(X),
	!,
	WordNumber1 is WordNumber + 1,
	unif_labels(Rest, NewRest, WordNumber1).
	
unif_labels([ [label(WordNumber)] | Rest ], NewRest, WordNumber) :-
	!,
	unif_labels(Rest, NewRest, WordNumber).
	
unif_labels([Word | Rest], [Word | NewRest], WordNumber) :-
	WordNumber1 is WordNumber + 1,
	!,
	unif_labels(Rest, NewRest, WordNumber1).

unif_labels([], [], _).
	



%-----------------ZAMIANA SŁÓW MAKRO-ASEMBLEROWYCH NA LISTY LICZB CAŁKOWITYCH
decList([Word | RestOfWords], [ResWord | RestOfResWords]) :-
	decWord(Word, AlmostResWord, 4096),
	add_all(AlmostResWord, ResWord),
	decList(RestOfWords, RestOfResWords).
decList([],[]).

decWord([Number], [Number], _) :- number(Number), !.

decWord([H | T], [Number | NewT], Factor) :-
	(
		H = syscall, Number is Factor * 1
		;
		H = load, Number is Factor * 2
		;
		H = store, Number is Factor * 3
		;
		H = swapA, Number is Factor * 4
		;
		H = swapD, Number is Factor * 5
		;
		H = branchZ, Number is Factor * 6
		;
		H = branchN, Number is Factor * 7
		;
		H = jump, Number is Factor * 8
		;
		H = const, Number is Factor * 9
		;
		H = add, Number is Factor * 10
		;
		H = sub, Number is Factor * 11
		;
		H = mult, Number is Factor * 12
		;
		H = div, Number is Factor * 13
		;
		H = shift, Number is Factor * 14	
	),
	Factor1 is Factor div 16,
	decWord(T, NewT, Factor1).
decWord([],[],_).



add_all(Numbers, Res) :- add_all(Numbers, Res, 0).
add_all([Number| Numbers], Res, Acc) :-
	Acc1 is Acc + Number,
	add_all(Numbers, Res, Acc1).
add_all([],Res,Res).
%---------------------------------------------------------------------------------------	





%główny predykat
algol16(Source, SextiumBin) :-
	parse(Source, Ast),
	translate(Ast, SextiumTokens),
	pack(SextiumTokens, PackedWords),
	translate_quadras(PackedWords, NewPackedWords),
	cut(NewPackedWords, NewNewPackedWords),
	clear(NewNewPackedWords, AlmostSextiumBin),
	%unif_labels(AlmostSextiumBin, SextiumBin).
	unif_labels(AlmostSextiumBin, AlmostAlmostSextiumBin),
	decList(AlmostAlmostSextiumBin, SextiumBin).

	
%pomocniczy predykat do debugowania	
decTOhex([],[]) :- !.
decTOhex( [H|T], [X|XS] ) :-
	format(atom(X), '~16r', [H]),
	decTOhex(T,XS).

	



	








