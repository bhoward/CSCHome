; Example Pep/8 program using all addressing modes
; Brian Howard, April 2009

;--------------------------------------------------
; // Original C++ program
; #include <iostream>
; using namespace std;
;
; int global;
; int globalArray[10];
; int *pointer;
;
; void fill(int *start, int *end, int value)
; {
;   for (pointer = start; pointer != end; ++pointer) {
;     *pointer = value;
;   }
; }
;
; void threeQuarters(int &n)
; {
;   // Do n -= n/4, using an explicit temp
;   int temp = n / 4;
;   n -= temp;
; }
;
; int fun(int param, int &refParam, int paramArray[])
; {
;   int local;
;   int localArray[10];
;
;   if (param == 0) {
;     return 42;
;   }
;
;   refParam -= param;
;   local = refParam + global;
;   threeQuarters(local);
;   fill(localArray, localArray + 10, param);
;   cout << global << ", " << globalArray[param] << endl;
;   cout << local << ", " << localArray[param] << endl;
;   cout << refParam << ", " << paramArray[param] << endl;
;   return fun(param - 1, refParam, localArray);
; }
;
; int main()
; {
;   global = 58;
;   fill(globalArray, globalArray + 10, 37);
;   cout << fun(9, global, globalArray) << endl;
;   cout << global << endl;
;   return 0;
; }
;--------------------------------------------------

            BR          main
; int global;
global:     .BLOCK      2

; int globalArray[10];
globalAr:   .BLOCK      20

; int *pointer;
pointer:    .BLOCK      2

; void fill(int *start, int *end, int value)
; {
start:      .EQUATE     6
end:        .EQUATE     4
value:      .EQUATE     2
; NOTE: no return value or local variables

;   for (pointer = start; pointer != end; ++pointer) {
fill:       LDA         start,s
            STA         pointer,d
lab1:       CPA         end,s       ; assumes pointer in A
            BREQ        lab2

;     *pointer = value;
            LDA         value,s
            STA         pointer,n

;   }
            LDA         pointer,d
            ADDA        2,i         ; increment of int *
            STA         pointer,d
            BR          lab1

; }
lab2:       RET0

; void threeQuarters(int &n)
; {
;   // Do n -= n/4, using an explicit temp
;   int temp = n / 4;
n:          .EQUATE     4
temp:       .EQUATE     0

threeQua:   SUBSP       2,i         ; allocate local
            LDA         n,sf
            ASRA
            ASRA
            STA         temp,s

;   n -= temp;
            LDA         n,sf
            SUBA        temp,s
            STA         n,sf

; }
            RET2                    ; deallocate local

; int fun(int param, int &refParam, int paramArray[])
; {
;   int local;
;   int localArray[10];
rv:         .EQUATE     30
param:      .EQUATE     28
refParam:   .EQUATE     26
paramArr:   .EQUATE     24
local:      .EQUATE     20
localArr:   .EQUATE     0

fun:        SUBSP       22,i        ; allocate locals

;   if (param == 0) {
            LDX         param,s
            BRNE        lab3

;     return 42;
            LDA         42,i
            STA         rv,s
            ADDSP       22,i        ; deallocate locals
            RET0
;   }

;   refParam -= param;
lab3:       LDA         refParam,sf
            SUBA        param,s
            STA         refParam,sf

;   local = refParam + global;
            ADDA        global,d    ; refParam already in A
            STA         local,s

;   threeQuarters(local);
            MOVSPA
            ADDA        local,i     ; compute addr of local
            STA         -2,s        ; push arg

            SUBSP       2,i
            CALL        threeQua
            ADDSP       2,i

;   fill(localArray, localArray + 10, param);
            MOVSPA
            ADDA        localArr,i
            STA         -2,s        ; push first arg
            ADDA        20,i        ; compute localArray + 10 (as int *)
            STA         -4,s        ; push second arg
            LDA         param,s
            STA         -6,s        ; push third arg

            SUBSP       6,i
            CALL        fill
            ADDSP       6,i

;   cout << global << ", " << globalArray[param] << endl;
            DECO        global,d
            STRO        comma,d
            ASLX                    ; scale int array index
            DECO        globalAr,x
            CHARO       endl,i

;   cout << local << ", " << localArray[param] << endl;
            DECO        local,s
            STRO        comma,d
            DECO        localArr,sx
            CHARO       endl,i

;   cout << refParam << ", " << paramArray[param] << endl;
            DECO        refParam,sf
            STRO        comma,d
            DECO        paramArr,sxf
            CHARO       endl,i
 
;   return fun(param - 1, refParam, localArray);
            LDA         param,s
            SUBA        1,i
            STA         -4,s        ; push first arg
            LDA         refParam,s
            STA         -6,s        ; push second arg
            MOVSPA
            ADDA        localArr,i
            STA         -8,s        ; push third arg
 
            SUBSP       8,i
            CALL        fun
            ADDSP       8,i

            LDA         -2,s        ; pop result
            STA         rv,s
            ADDSP       22,i        ; deallocate locals
            RET0

; }

; int main()
; {
;   global = 58;
main:       LDA         58,i
            STA         global,d

;   fill(globalArray, globalArray + 10, 37);
            LDA         globalAr,i
            STA         -2,s
            ADDA        20,i
            STA         -4,s
            LDA         37,i
            STA         -6,s

            SUBSP       6,i
            CALL        fill
            ADDSP       6,i

;   cout << fun(9, global, globalArray) << endl;
            LDA         9,i
            STA         -4,s
            LDA         global,i
            STA         -6,s
            LDA         globalAr,i
            STA         -8,s

            SUBSP       8,i
            CALL        fun
            ADDSP       8,i

            DECO        -2,s
            CHARO       endl,i

;   cout << global << endl;
            DECO        global,d
            CHARO       endl,i

;   return 0;
; }
            STOP

endl:       .EQUATE     10
comma:      .ASCII      ", \x00"

            .END
