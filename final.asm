data_seg		SEGMENT
    ; Promenljive potrebne za rad sa grafikom
    pozX dw ?      ; trenutna pozicija znaka
    pozY dw ?
    sirina dw ?    ; maksimalna sirina i visina ekrana
    visina dw ? 
    adresa dw ?    ; ofsetna adresa znaka
    boja db ?
    znak db ?      ; pomocna promenljiva za makro readString
    ; Promenljive potrebne za program
    porukaX db 'Unesite vrh kupe x: $'
    porukaY db 'Unesite vrh kupe y: $'
    porukaVisina db 'Unesite visinu kupe: $'
    porukaPrecnik db 'Unesite bazu kupe: $'
    strBroj db '            '
    kupaPrecnik dw ?
    vrhX dw ?
    vrhY dw ?
    kupaVisina dw ?
    poruka db 'Pritisnite neki taster...$'
    ; Promenljive za crtanje linija
    TempW dw ?
    pointX dw ? 
    pointY dw ?
    point1X dw ? 
    point1Y dw ?
    point2X dw ? 
    point2Y dw ?
data_seg		ENDS

code_seg	    SEGMENT
		ASSUME cs:code_seg, ds: data_seg
		
; Pomocni macro-i za DrawCircle

; Assign variable b to a
Assign macro a, b
    mov ax, [b]
    mov [a], ax    
endm

;a = -a 
Negate macro a
    mov ax, [a]
    neg ax
    mov [a], ax    
endm

;a = a+1 
IncVar macro a
    mov ax, [a]
    inc ax
    mov [a], ax    
endm

;a = a-1 
DecVar macro a
    mov ax, [a]
    dec ax
    mov [a], ax    
endm

Compare2Variables macro a, b
    mov cx, [a]
    cmp cx, [b]
endm

CompareVariableAndNumber macro a, b
    mov cx, [a]
    cmp cx, b
endm

;c = a+b
AddAndAssign macro c, a, b
    mov ax, [a]
    add ax, [b]
    mov [c], ax
endm 

;c = a-b
SubAndAssign macro c, a, b
    mov ax, [a]
    sub ax, [b]
    mov [c], ax
endm

;d = a+b+c
Add3NumbersAndAssign macro d, a, b, c
    mov ax, [a]
    add ax, [b]
    add ax, [c]
    mov [d], ax
endm 

;d = a-b-c
Sub3NumbersAndAssign macro d, a, b, c
    mov ax, [a]
    sub ax, [b]
    sub ax, [c]
    mov [d], ax
endm

DrawPixel macro x, y
    
    mov cx, [x]  ; column  
    mov dx, [y]  ; row  
     
    mov al, 10  ; green
    mov ah, 0ch ; put pixel
    int 10h     
endm

DrawCircle macro circleCenterX, circleCenterY, radius
    
    ; Koristimo vec koriscene promenljive zbog manjka memorije
    
    mov sirina, 0                                ;balance dw 0
    mov visina, 0                                ;xoff dw 0
    mov adresa, 0                                ;yoff dw 0 
                                    
    mov pointX, 0                                ;xplusx dw 0
    mov pointY, 0                                ;xminusx dw 0
    mov point1X, 0                               ;yplusy dw 0
    mov point1Y, 0                               ;yminusy dw 0
                                    
                                                 
    mov point2X, 0                               ;xminusy dw 0
    mov point2Y, 0                               ;yplusx dw 0
    mov TempW, 0                                 ;xplusy dw 0
    mov kupaVisina, 0                            ;yminusx dw 0
    
    Assign adresa, radius
    
    Assign sirina, radius
    Negate sirina
    
    draw_circle_loop:
     
    AddAndAssign pointX, circleCenterX, visina
    SubAndAssign pointY, circleCenterX, visina
    AddAndAssign point1X, circleCenterY, adresa
    SubAndAssign point1Y, circleCenterY, adresa
     
    AddAndAssign TempW, circleCenterX, adresa
    SubAndAssign point2X, circleCenterX, adresa
    AddAndAssign point2Y, circleCenterY, visina
    SubAndAssign kupaVisina, circleCenterY, visina
     
    ;180 to 225
    DrawPixel point2X, point2Y
    
    ;270 to 225
    DrawPixel pointY, point1X
    
    ;270 to 315 
    DrawPixel pointX, point1X
    
    ;360 to 315
    DrawPixel TempW, point2Y

    Add3NumbersAndAssign sirina, sirina, visina, visina

    CompareVariableAndNumber sirina, 0
    jl balance_negative
    DecVar adresa
    
    Sub3NumbersAndAssign sirina, sirina, adresa, adresa
    
    balance_negative:
    IncVar visina
    
    Compare2Variables visina, adresa
    jg end_drawing
    jmp draw_circle_loop
    
    
    end_drawing:    
endm
		
krajPrograma macro
    mov ax, 4c02h
    int 21h
endm

macro initGraph
    push ax
    mov ax, 0B800h
    mov es, ax
    mov sirina, 80
    mov visina, 25
    mov pozX, 0        ; vrednost tekuceg polja je (0,0)
    mov pozY, 0
    mov adresa, 0
    mov boja, 7        ; siva boja na crnoj podlozi
    pop ax
endm

; Pomera nas na polje definisano parametrima makroa              
macro setXY x y
    push ax
    push bx
    push dx
    ; vrednosti se smestaju u pozX i pozY, a nakon toga se racuna adresa tog znaka 
    ; u ekranskoj memoriji po formuli adresa = pozY*160+pozX*2
    mov pozX, x
    mov pozY, y 
    mov ax, pozY     
    mov bx, sirina
    shl bx, 1      ; pomeranje bitova u levo za 1: mnozenje sa 2
    mul bx         ; mnozi bx sa ax
    mov bx, pozX   ; opet pomeranje u levo
    shl bx, 1      ; i na kraju sabiranje
    add ax, bx
    mov adresa, ax
    
    pop dx
    pop bx
    pop ax
endm

; Ispis stringa na ekran
writeString macro str
    LOCAL petlja, kraj    ; dve lokalne petlje
    push ax
    push bx  
    push si               ; kao index u stringu
    mov si, 0             ; nulti znak stringa je pocetni
    mov ah, boja
    mov bx, adresa        ; smestamo ga na adresu
petlja:
    mov al, str[si]       ; iz stringa se uzimaju znakovi redom i smestaju se u al
    cmp al, '$'           ; ako se naidje na $ prelazi se na kraj ispisa
    je kraj
    mov es:[bx], al       ; iz registra al se zajedno sa bojom stavljaju na ekran
    mov es:[bx+1], ah
    add bx, 2
    add si, 1             ; pomeramo se za 2 mesta u memoriji
    jmp petlja
kraj:           

    mov ax, pozX          ; pozicija X se pomera za duzinu stringa
    add ax, si
    mov bx, pozY
    setXY ax bx
    pop si
    pop bx
    pop ax
endm

; Novi red
writeLn proc  
    push ax
    push bx
    mov bx, pozY
    add bx, 1       ; prvo povecavamo pozY za 1
    mov ax, 0       ; pozX ce biti 0
    setXY ax, bx    ; postavljanje
    pop bx
    pop ax
    ret
writeLn endp       

; Konvertovanje stringa u broj               
strtoint proc
    push ax
    push bx
    push cx
    push dx
    push si
    mov bp, sp
    mov bx, [bp+14]
    mov ax, 0
    mov cx, 0
    mov si, 10
petlja1:
    mov cl, [bx]
    cmp cl, '$'
    je kraj1
    mul si
    sub cx, 48
    add ax, cx
    inc bx  
    jmp petlja1
kraj1:
    mov bx, [bp+12] 
    mov [bx], ax 
    pop si  
    pop dx
    pop cx
    pop bx
    pop ax
    ret 4
strtoint endp     

; Redefinisan makro ucitavanja stringa sa tastature
; tako da i on koristi direktan pristup ekranskoj memoriji 
; za prikazivanje ucitanih znakova

readString macro str 
    LOCAL unos, nastavi, kraj
    push ax
    push bx
    push cx
    push dx
    push si 

    mov si, 0            ; koristi se kao brojac
    mov bx, adresa       ; adresa tekuceg znaka na ekranu
    mov cx, pozX         ; u cx i dx pamtimo poziciju tekuceg znaka
    mov dx, pozY

unos:
    readKey znak         ; ucitavamo jedan znak
    cmp znak, 13         ; 13 = ENTER --> kraj
    je kraj
    cmp znak, 8          ; 8 = BACKSPACE
    jne nastavi
    cmp si, 0            
    je unos              ; ako je broj unetih znakova 0 samo nastavi dalje
    sub cx, 1            ; ako broj unetih znakova != 0 pomeramo se za 1 znak <
    setXY cx dx          ; postavljamo novu poziciju
    write ' '            ; upisujemo 'prazan znak'
    dec si               ; smanjimo brojac
    jmp unos
nastavi:
    mov al, znak         
    mov str[si], al      ; smestanje znaka u string
    write al             ; ispis na ekran
    add cx, 1            ; pozX += 1
    setXY cx, dx  
    inc si               ; povecavamo brojac stringa
    jmp unos             ; ucitaj sledeci znak
     
kraj:
    mov str[si], '$'     ; stavljamo $, sredjujemo stek
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
endm

; Postavljanje tekuce boje
macro setColor b
    mov boja, b
endm  

; Ucitavanje znaka bez prikaza i memorisanja           
keyPress macro
    push ax
    mov ah, 08
    int 21h
    pop ax
endm  

; Ispis znaka na tekucu poziciju ekrana
macro Write c
    push bx        
    push dx
    mov bx, adresa
    mov es:[bx], c
    mov dl, boja      ; stavljamo boju prvo u pomocni registar dl
    mov es:[bx+1], dl ; boja se smesta na adresu odmah posle znaka
    pop dx
    pop bx
endm  

; Ucitavanje jednog znaka   
readkey macro c
    push ax
    mov ah, 08
    int 21h
    mov c, al
    pop ax
endm

Macro DrawLine2DDY p1X, p1Y, p2X, p2Y
	local l1, lp, nxt
	mov dx, 1
	mov ax, [p1X]
	cmp ax, [p2X]
	jbe l1
	neg dx ; turn delta to -1
l1:
	mov ax, [p2Y]
	shr ax, 1 ; div by 2
	mov [TempW], ax
	mov ax, [p1X]
	mov [pointX], ax
	mov ax, [p1Y]
	mov [pointY], ax
	mov bx, [p2Y]
	sub bx, [p1Y]
	absolute bx
	mov cx, [p2X]
	sub cx, [p1X]
	absolute cx
	mov ax, [p2Y]
lp:
	pusha
	call PIXEL
	popa
	inc [pointY]
	cmp [TempW], 0
	jge nxt
	add [TempW], bx ; bx = (p2Y - p1Y) = deltay
	add [pointX], dx ; dx = delta
nxt:
	sub [TempW], cx ; cx = abs(p2X - p1X) = daltax
	cmp [pointY], ax ; ax = p2Y
	jne lp
	call PIXEL
ENDM DrawLine2DDY

Macro DrawLine2DDX p1X, p1Y, p2X, p2Y
	local l1, lp, nxt
	mov dx, 1
	mov ax, [p1Y]
	cmp ax, [p2Y]
	jbe l1
	neg dx ; turn delta to -1
l1:
	mov ax, [p2X]
	shr ax, 1 ; div by 2
	mov [TempW], ax
	mov ax, [p1X]
	mov [pointX], ax
	mov ax, [p1Y]
	mov [pointY], ax
	mov bx, [p2X]
	sub bx, [p1X]
	absolute bx
	mov cx, [p2Y]
	sub cx, [p1Y]
	absolute cx
	mov ax, [p2X]
lp:
	pusha
	call PIXEL
	popa
	inc [pointX]
	cmp [TempW], 0
	jge nxt
	add [TempW], bx ; bx = abs(p2X - p1X) = deltax
	add [pointY], dx ; dx = delta
nxt:
	sub [TempW], cx ; cx = abs(p2Y - p1Y) = deltay
	cmp [pointX], ax ; ax = p2X
	jne lp
	call PIXEL
ENDM DrawLine2DDX
Macro absolute a
	local l1
	cmp a, 0
	jge l1
	neg a
l1:
Endm

start:  
    lea dx, data_seg
    mov ds, dx
        
    ; inicijalizacija promenljivih
    initGraph
         
          
         
    ; Ucitavanje vrha X
    writeString porukaX
    readString strBroj
    push offset strBroj
    push offset vrhX
    call strtoint
    call writeLn  
        
    ; Ucitavanje vrha Y            
    writeString porukaY
    readString strBroj
    push offset strBroj
    push offset vrhY
    call strtoint
    call writeLn 
            
    ; Ucitavanje visine
    writeString porukaVisina
    readString strBroj
    push offset strBroj
    push offset kupaVisina
    call strtoint
    call writeLn  
         
    ; Ucitavanje Precnik
    writeString porukaPrecnik
    readString strBroj
    push offset strBroj
    push offset kupaPrecnik
    call strtoint
    call writeLn 
        
    mov ah, 0   ; set display mode function
    mov al, 13h ; mode 13h = 320x200 pixels 256 colors
    int 10h
        
    mov [boja], 61
    mov ax, vrhX              
    mov bx, kupaPrecnik
    sub ax, bx                ; Dole levo x (x - r)
    mov [point1X], ax 
    mov ax, vrhY
    mov bx, kupaVisina
    add ax, bx               ; Dole levo y (x + y)
    mov [point1Y], ax 
    mov ax, vrhX             ; Vrh x
    mov [point2X], ax 
    mov ax, vrhY             ; Vrh y
    mov [point2Y], ax    
    call DrawLine2D
    	
    mov [boja], 2
    mov ax, vrhX              
    mov bx, kupaPrecnik
    add ax, bx               ; Dole desno x (x + r)
    mov [point1X], ax 
    mov ax, vrhY
    mov bx, kupaVisina
    add ax, bx               ; Dole levo y (x + y)
    mov [point1Y], ax 
    mov ax, vrhX             ; Vrh x
    mov [point2X], ax 
    mov ax, vrhY             ; Vrh y
    mov [point2Y], ax    
    call DrawLine2D
        
        
        
    mov ax, vrhX
    mov pozX, ax
    ; Centra kruga ( vrhY + h)
    mov ax, vrhY
    mov bx, kupaVisina
    add ax, bx               
    mov pozY, ax
        
        
    DrawCircle pozX, pozY , kupaPrecnik
        
    keyPress
    krajPrograma

PROC DrawLine2D
	mov cx, [point1X]
	sub cx, [point2X]
	absolute cx
	mov bx, [point1Y]
	sub bx, [point2Y]
	absolute bx
	cmp cx, bx
	jae DrawLine2Dp1 ; deltaX > deltaY
	mov ax, [point1X]
	mov bx, [point2X]
	mov cx, [point1Y]
	mov dx, [point2Y]
	cmp cx, dx
	jbe DrawLine2DpNxt1 ; point1Y <= point2Y
	xchg ax, bx
	xchg cx, dx
DrawLine2DpNxt1:
	mov [point1X], ax
	mov [point2X], bx
	mov [point1Y], cx
	mov [point2Y], dx
	DrawLine2DDY point1X, point1Y, point2X, point2Y
	ret
DrawLine2Dp1:
	mov ax, [point1X]
	mov bx, [point2X]
	mov cx, [point1Y]
	mov dx, [point2Y]
	cmp ax, bx
	jbe DrawLine2DpNxt2 ; point1X <= point2X
	xchg ax, bx
	xchg cx, dx
DrawLine2DpNxt2:
	mov [point1X], ax
	mov [point2X], bx
	mov [point1Y], cx
	mov [point2Y], dx
	DrawLine2DDX point1X, point1Y, point2X, point2Y
	ret
ENDP DrawLine2D
PROC PIXEL
	mov bh,0h
	mov cx,[pointX]
	mov dx,[pointY]
	mov al,[boja]
	mov ah,0Ch
	int 10h
	ret
ENDP PIXEL
code_seg        ENDS
END start