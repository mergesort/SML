(*Joseph Fabisevich*)
(*106098624*)
(*CSE 307*)
(*Homework #1*)

(* #1 DEC2BIN *)
(*Convert a decimal number to a binary one*)
fun dec2bin (x) =
	if x <= 1
		then [x]
	else 
		dec2bin (x div 2) @ [(x mod 2)];

(* Helper function LEN *)
(*Find the length of a list*)
fun len(L) = 
   if L = []
       then 0
   else
       1 + len(tl(L));
 
(* Helper function FACTORIAL *)
(*Find the factorial of a number*)
fun factorial(x) = 
	if x = 0
		then 0
	else
		x + factorial(x-1); 
          
(* Helper function POW *)
(*Find the power of a number in a certain base*)
fun pow(base, exp) = 
   if exp >= 1
       then base * pow(base, exp - 1)
   else
       1;    
       
(* Helper function BINDEC *)
fun bindec(L) = 
   if L = []
       then 0
   else
       if hd(L) = 1
           then (pow(2, len(L))) + bindec(tl(L))
       else
           bindec(tl(L));

(* #2 BINDEC *)
(*Convert a binary number to a decimal*)
fun bin2dec(L) = 
	bindec(L) div 2;

(* #3 COUNT *)
(*Get the count of the values in a list*)
fun count(x, L) =
	if L = []
		then 0
	else
		if hd(L) = x
			then 1 + count(x, tl(L))
		else
			count(x, tl(L));
	
(* Helper function FIND *)
(*Return true if the number exists in a list, false if not*)
fun find(x, L) = 
	if L = []
		then false
	else
		if hd(L) = x
			then true
		else
			find(x, tl(L));
	
	
(* #4 DUPLICATE *)
(*Find if a list contains a duplicate value*)
fun duplicate(L) = 
	if L = []
		then false
	else
		if find(hd(L), tl(L)) = true
			then true
		else
			duplicate(tl(L));
			
(* #5 REMOVE *)
(*Remove the first instance of a value in a list*)
fun remove(x, L) = 
	if L = []
		then []
	else
		if find(x, tl(L)) = true
			then tl(L)
		else
			hd(L) :: remove(x, tl(L));
			
(* #6 ISPERMUTATION*)
(*Check if list 1 is a permutation of list 2*)
fun is_permutation (L1, L2) =
	if L1 = [] orelse L2 = []
		then true
	else
		if find(hd(L1), L2) = true
			then is_permutation(tl(L1), remove(hd(L1), L2))
		else
			false;

(* #7 PREFIX *)
(*Take the value v and prepend it to every list in the list L*)
fun prefix (v, L) =
    if L = []    
		then []
    else
        [v :: hd(L)] @ prefix (v, tl(L));
