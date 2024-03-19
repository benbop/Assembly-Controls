jumps
IDEAL
MODEL small
STACK 100h
DATASEG
; --------------------------
;Single player
Player_Buffer_S db 21 dup(?),0
filename_screen_S db 'Start.bmp',0
filename_screen_Ss db 'Again.bmp',0

filehandle_S dw ?
Header_S db 54 dup (0)
Palette_S db 256*4 dup (0)
ScrLine_S db 320 dup (0) ; unrelated to actual image size
ErrorMsg_S db 'Error', 13, 10 ,'$'

filename_S db 'score.txt',0
filehandle_score_S dw ?

PlayerName_S db 16 dup(?),13 ,'$'

MaxScore_S db 10 dup(?),0
Y_S dw 100 ; of charchter
X_S dw 100 ; of charchter
TIME_AUX_S DB 0 ; for waiting in between frames 

PressFlag_S db 0 ; press flag
SnakeBody_S dw 5	; body of charcter
Xapple_S dw 80 ; y of apple
yapple_S dw 80 ; x of apple
Random_x_S dw 80 ; my rnds
Random_y_S dw 80 ; my rnds
Speed_S dw 5 ; speed of snake
Player_Score_S DB 48 ; ascii value of 0
level_S db 1

SEED_S dw 11

CenterSquare_Y_S dw 0
CenterSquare_X_S dw 0
CenterApple_Y_S dw 0
CenterApple_X_S dw 0

Xat_S db 0
Yat_S db 0

Max_S db 0
NewLine_S db ' '
flagforscore_S db 0

GetName_S db 'Enter 3 Charchter Name ',13, 10 ,'$'
savedscore_S db 0

crrMax_S dw 0

;Multiplier
Tuturial db 'Tut.bmp',0
filename_OutOfTimep1 db 'TimeP1.bmp',0
filename_OutOfTimep2 db 'TimeP2.bmp',0
filename_screen1 db 'Player1.bmp',0
filename_screen2 db 'Player2.bmp',0
filehandle dw ?
Header db 54 dup (0)
Palette db 256*4 dup (0)
ScrLine db 320 dup (0) ; unrelated to actual image size
ErrorMsg db 'Error', 13, 10 ,'$'

filename db 'score.txt',0
filehandle_score dw ?
Message db 'Hello World!'

Y dw 100 ; of charchter
X dw 100 ; of charchter

FstY dw 70 ; of charchter
FstX dw 200 ; of charchter
TIME_AUX DB 0 ; for waiting in between frames 

SnakeBody dw 5	; body of charcter
Xapple dw 80 ; y of apple
yapple dw 80 ; x of apple

PressFlag1 db 0 ; press flag
PressFlag2 db 0
FstBody dw 5	; body of charcter
Fstxapple dw 200 ; y of apple
Fstyapple dw 80 ; x of apple
Random_x dw 80 ; my rnds
Random_y dw 80 ; my rnds
Speed dw 5 ; speed of snake
Player_Score_fst DB 48 ; ascii value of 0
level_fst db 1

Player_Score_Scnd db 48
level_Scnd db 1
SEED dw 11

FstCenterSquare_FstY dw 0
FstCenterSquare_FstX dw 0
FstCenterApple_FstY dw 0
FstCenterApple_FstX dw 0

CenterSquare_Y dw 0
CenterSquare_X dw 0
CenterApple_Y dw 0
CenterApple_X dw 0

Max db 0
NewLine db ' '
flagforscore db 0

screenFlag db 0


Timer_ db 00 ; hold crr time
StartScreen db 'Start.bmp',0
instructions db 'Tut.bmp',0

;notes
note_C  dw 261
note_D  dw 294
note_E  dw 329
note_F  dw 349
note_G  dw 392
note_A  dw 440
note_B  dw 494

easteregg db 1
CODESEG

; --------------------------    
; Your code here
; --------------------------
delayms equ [bp+4]
proc delay 
		push bp
		mov bp, sp
		mov ax , delayms
		mov di, 1000    
		mul di          ; DX:AX=CX*AX (in AX duration of the note * multi)
		
		
		mov cx, dx      ; into register CX the high byte
		mov dx, ax      ; into 
		mov ax, 8600h
		int 15h         ; when ah=86, CX:DX - number of microseconds to wait (only accurate to 977 us)

		in al,61h       
        	and al,11111100b
        	out 61h,al

		mov cx, 0		;pause between notes
		mov dx, 10000	
		mov ax, 8600h
		int 15h
		    
		pop bp
		ret 2             
endp delay
proc OpenFile_start
; Open file
mov ah, 3Dh
xor al, al
mov dx, offset StartScreen
int 21h
jc openerrortut
mov [filehandle], ax
ret
openerrortut :
mov dx, offset ErrorMsg
mov ah, 9h
int 21h
ret
endp OpenFile_start

proc OpenFile_Tut
; Open file
mov ah, 3Dh
xor al, al
mov dx, offset instructions
int 21h
jc openerror1
mov [filehandle], ax
ret
openerror1 :
mov dx, offset ErrorMsg
mov ah, 9h
int 21h
ret
endp OpenFile_Tut

proc Timer ; wait 60 seconds before eneding the game
	mov ah , 2ch
	int 21h
	cmp [Timer_], dh
	je Time_passed
	ret
	Time_passed:
	mov bh , [Player_Score_fst]
	cmp [Player_Score_Scnd],bh
	jg Scnd_Wins
	Fst_Wins:
	call OpenFile_OutOfTimeP1
		call ReadHeader
		call ReadPalette
		call CopyPal
		call CopyBitmap
		loopFst:
		mov ah , 11h
		int 16h
		JZ loopFst
		
		MOV AH, 00h       ; Function 00h - Read Key Stroke
		INT 16h
		
		cmp al , 'p'
		jne loopFst
		call reset_game
		exit_temp1:
		ret
	Scnd_Wins : 
	call OpenFile_OutOfTimeP2
		call ReadHeader
		call ReadPalette
		call CopyPal
		call CopyBitmap
		loopScnd:
		mov ah , 11h
		int 16h
		JZ loopScnd
		
		MOV AH, 00h       ; Function 00h - Read Key Stroke
		INT 16h
		
		cmp al , 'p'
		jne loopScnd
		call reset_game
		exit_temp2:
		ret
	
ENDP Timer

proc CloseFile
; Close file
mov ah,3Eh
mov bx, [filehandle_score]
int 21h
ret
endp CloseFile


	proc set_cursor_Pfst
	xor dx , DX
MOV AH, 02h
    MOV BH, 0
    MOV DH, 2 ; Adjust row for the score display
    MOV DL, 10 ; Adjust column for the score display
    INT 10h
	RET
	ENDP set_cursor_Pfst
PROC Score_fst
	
	
    mov ah, 09h  ; Display character function
	mov bh, 0    ; Page number
	mov cx, 1    ; Number of times to display character
	ADD AL, '0'        ; Convert to ASCII
	mov al, [byte ptr Player_Score_fst]  ; display level_fst
	
    mov bl, [byte ptr level_fst]    ; Attribute (text color)
    int 10h
  
	ret
	
ENDP Score_fst

	
	proc set_cursor_PScnd
MOV AH, 02h
    MOV BH, 0
    MOV DH, 1 ; Adjust row for the score display
    MOV DL, 188 ; Adjust column for the score display
    INT 10h
	RET
	ENDP set_cursor_PScnd
PROC Score_Scnd

	
    mov ah, 09h  ; Display character function
	mov bh, 0    ; Page number
	mov cx, 1    ; Number of times to display character
	ADD AL, '0'        ; Convert to ASCII
	mov al, [byte ptr Player_Score_Scnd]  ; display level_fst
	
    mov bl, [byte ptr level_Scnd]    ; Attribute (text color)
    int 10h
    
	
	ret
ENDP Score_Scnd
  PROC moves

    mov ah , 11h
	int 16h
    JZ exit_moves     ; Jump if ZF is set (no key pressed)

    MOV AH, 00h       ; Function 00h - Read Key Stroke
    INT 16h
	; ah is for extended like arrow keys
	
    ; Check if AH contains 'W' (ASCII value 87)
    CMP AL, 'w'
	je w_helper

    ; Check if AH contains 'D' (ASCII value 68)
    CMP AL, 'd'
    je d_helper
    ; Check if AH contains 'A' (ASCII value 65)
    CMP AL, 'a'
    je a_helper
    ; Check if AH contains 'S' 'S'
    CMP AL, 's'
    Je s_helper
	
	CMP Ah, 48h ; arrow up key 
	je u_helper

    CMP Ah, 4Dh ; arrow left key
    je k_helper
    
    CMP Ah, 4Bh ; arrow right key      
    je h_helper
    
    CMP Ah, 50h    ; arrow down key  
    Je j_helper
	
exit_moves:


    RET

ENDP moves
j_helper:
mov [PressFlag2] , 4
ret

h_helper:
mov [PressFlag2] , 3
ret
k_helper:
mov [PressFlag2] , 2
ret
u_helper:
mov [PressFlag2] , 1
ret
s_helper:
mov [PressFlag1] , 4
ret
a_helper:
mov [PressFlag1] , 3
ret
d_helper:
mov [PressFlag1] , 2
ret
w_helper:
mov [PressFlag1] , 1
ret

Proc LastPressed
cmp [PressFlag1],0
jne next

next:
CALL moves
cmp [PressFlag2],1
JNE temp
mov cx , [speed]
SUB [FstY],cx  
temp:

cmp [PressFlag2] , 2
jne temp1
mov cx , [speed]
add [FstX],cx  
temp1:

cmp [PressFlag2] , 3
jne temp2
mov cx , [speed]
sub [FstX],cx  
temp2:

cmp [PressFlag2] , 4
jne temp3
mov cx , [speed]
add [FstY],cx  
temp3:


cmp [PressFlag1] , 1
JNE temp4
mov cx , [speed]
SUB [Y],cx 
temp4:

cmp [PressFlag1] , 2
jne temp5
mov cx , [speed]
add [X],cx  
temp5:

cmp [PressFlag1] , 3
jne temp6
mov cx , [speed]
SUB [X],cx  
temp6:

cmp [PressFlag1] , 4
jne temp7
mov cx , [speed]
add [Y],cx  
temp7:

ret
endp LastPressed


Proc Keyboard

	call LastPressed
	call draw
	ret
	endp Keyboard




PROC draw
;Border
MOV AX, 0C07h     ; Function 0Ch, Set Pixel Color
    MOV BH, 0         ; Page number (usually 0 in mode 13h)

    ; Draw the gray border
    MOV CX, 175         ; X-coordinate of the left border
    MOV DX, 25         ; Y-coordinate of the top border
    MOV SI, 0
    MOV DI, 0
    MOV AL, 8          ; Color for gray pixels

    draw_row_loop_border1:
        draw_pixel_loop_border1:
            INC CX
            INC DI

            INT 10h
            CMP DI, 100       ; Width of the border
            JNE draw_pixel_loop_border1

            SUB CX, 100        ; Reset X-coordinate to start_game
            MOV DI, 0
            INC DX
            INC SI

            CMP SI,100         ; Height of the border
            JNE draw_row_loop_border1

	
    
	
	
    MOV cx, [Fstxapple]  ; FstX-coordinate of the apple
    MOV dx, [Fstyapple]  ; FstY-coordinate of the apple
    MOV SI, 0
    MOV DI, 0
    MOV AL, 6          ; BROWN (choose a color different from the snake)

    draw_row_loop_apple1:
        draw_pixel_loop_apple1:
            INC cx
            INC DI
            
            INT 10h
            CMP DI, [FstBody]  ; size of the apple must be the same size of the snake for collison to work
            JNE draw_pixel_loop_apple1
            
            SUB cx, [FstBody]  ; without this, the apple is not drawing up; it's drawing to the side
            MOV DI, 0
            INC dx
            INC SI
            
            CMP SI, [FstBody];  size of the apple must be the same size of the snake for collison to work
            JNE draw_row_loop_apple1
	
    ; Draw the snake
    MOV cx, [FstX]       ; FstX-coordinate of the snake head
    MOV dx, [FstY]       ; FstY-coordinate of the snake head
    MOV SI, 0
    MOV DI, 0
    MOV AL, 5         ; MAGENTA green (you can choose a different color)

    draw_row_loop1:
        draw_pixel_loop1:
            INC cx
            INC DI
            
            INT 10h
            CMP DI, [FstBody] 
            JNE draw_pixel_loop1
            
            SUB cx, [FstBody]
            MOV DI, 0
            INC dx
            INC SI
            
            CMP SI, [FstBody] 
            JNE draw_row_loop1


	
mov  si, [FstBody]      ; 5 (best to keep this an odd number: 3, 5, 7, ...
shr  si, 1                ; 2

mov  bx, [FstX]
lea  ax, [bx + si + 1]    ; FstX + 3
mov  [FstCenterSquare_FstX], ax

mov  bx, [FstY]
lea  ax, [bx + si]        ; FstY + 2
mov  [FstCenterSquare_FstY], ax

mov  bx, [Fstxapple]
lea  ax, [bx + si + 1]    ; Fstxapple + 3
mov  [FstCenterApple_FstX], ax

mov  bx, [FstYapple]
lea  ax, [bx + si]        ; FstYapple + 2
mov  [FstCenterApple_FstY], ax

call check_Collision_player_Player1
	
    MOV AX, 0C07h     ; Function 0Ch, Set Pixel Color
    MOV BH, 0         ; Page number (usually 0 in mode 13h)

    ; Draw the gray border
    MOV CX, 25         ; X-coordinate of the left border
    MOV DX, 25         ; Y-coordinate of the top border
    MOV SI, 0
    MOV DI, 0
    MOV AL, 8          ; Color for gray pixels

    draw_row_loop_border:
        draw_pixel_loop_border:
            INC CX
            INC DI

            INT 10h
            CMP DI, 100       ; Width of the border
            JNE draw_pixel_loop_border

            SUB CX, 100        ; Reset X-coordinate to start_game
            MOV DI, 0
            INC DX
            INC SI

            CMP SI, 100        ; Height of the border
            JNE draw_row_loop_border

    
   
    ;
	
    MOV AX, 0C07h     ; Function 0Ch, Set Pixel Color
    MOV BH, 0         ; Page number (usually 0 in mode 13h)
    
    ; Draw the apple
	
	
	
	
    MOV CX, [Xapple]  ; X-coordinate of the apple
    MOV DX, [yapple]  ; Y-coordinate of the apple
    MOV SI, 0
    MOV DI, 0
    MOV AL, 4          ; Color (choose a color different from the snake)

    draw_row_loop_apple:
        draw_pixel_loop_apple:
            INC CX
            INC DI
            
            INT 10h
            CMP DI, [SnakeBody]  ; size of the apple must be the same size of the snake for collison to work
            JNE draw_pixel_loop_apple
            
            SUB CX, [SnakeBody]  ; without this, the apple is not drawing up; it's drawing to the side
            MOV DI, 0
            INC DX
            INC SI
            
            CMP SI, [SnakeBody];  size of the apple must be the same size of the snake for collison to work
            JNE draw_row_loop_apple
	
    ; Draw the snake
    MOV CX, [X]       ; X-coordinate of the snake head
    MOV DX, [Y]       ; Y-coordinate of the snake head
    MOV SI, 0
    MOV DI, 0
    MOV AL, 2         ; Color green (you can choose a different color)

    draw_row_loop:
        draw_pixel_loop:
            INC CX
            INC DI
            
            INT 10h
            CMP DI, [SnakeBody] 
            JNE draw_pixel_loop
            
            SUB CX, [SnakeBody]
            MOV DI, 0
            INC DX
            INC SI
            
            CMP SI, [SnakeBody] 
            JNE draw_row_loop



	
	
mov  si, [SnakeBody]      ; 5 (best to keep this an odd number: 3, 5, 7, ...
shr  si, 1                ; 2

mov  bx, [X]
lea  ax, [bx + si + 1]    ; X + 3
mov  [CenterSquare_X], ax

mov  bx, [Y]
lea  ax, [bx + si]        ; Y + 2
mov  [CenterSquare_Y], ax

mov  bx, [Xapple]
lea  ax, [bx + si + 1]    ; Xapple + 3
mov  [CenterApple_X], ax

mov  bx, [Yapple]
lea  ax, [bx + si]        ; Yapple + 2
mov  [CenterApple_Y], ax
call check_Collision



		
	
	
    RET
ENDP draw
proc OpenFile_OutOfTimeP1
; Open file
mov ah, 3Dh
xor al, al
mov dx, offset filename_OutOfTimep1
int 21h
jc openerror_notimep1
mov [filehandle], ax
ret
openerror_notimep1 :
mov dx, offset ErrorMsg
mov ah, 9h
int 21h
ret
endp OpenFile_OutOfTimeP1

proc OpenFile_OutOfTimeP2
; Open file
mov ah, 3Dh
xor al, al
mov dx, offset filename_OutOfTimep2
int 21h
jc openerror_notimep2
mov [filehandle], ax
ret
openerror_notimep2 :
mov dx, offset ErrorMsg
mov ah, 9h
int 21h
ret
endp OpenFile_OutOfTimeP2

proc OpenFile1
; Open file
mov ah, 3Dh
xor al, al
mov dx, offset filename_screen2
int 21h
jc openerror1_M
mov [filehandle], ax
ret
openerror1_M :
mov dx, offset ErrorMsg
mov ah, 9h
int 21h
ret
endp OpenFile1
proc ReadHeader1
; Read BMP file header, 54 bytes
mov ah,3fh
mov bx, [filehandle]
mov cx,54
mov dx,offset Header
int 21h
ret
endp ReadHeader1
proc ReadPalette1
; Read BMP file color palette, 256 colors * 4 bytes (400h)
mov ah,3fh
mov cx,400h
mov dx,offset Palette
int 21h
ret
endp ReadPalette1
proc CopyPal1
; Copy the colors palette to the video memory
; The number of the first color should be sent to port 3C8h
; The palette is sent to port 3C9h
mov si,offset Palette
mov cx,256
mov dx,3C8h
mov al,0
; Copy starting color to port 3C8h
out dx,al
; Copy palette itself to port 3C9h
inc dx
PalLoop1:
; Note: Colors in a BMP file are saved as BGR values rather than RGB .
mov al,[si+2] ; Get red value .
shr al,2 ; Max. is 255, but video palette maximal
; value is 63. Therefore dividing by 4.
out dx,al ; Send it .
mov al,[si+1] ; Get green value .
shr al,2
out dx,al ; Send it .
mov al,[si] ; Get blue value .
shr al,2
out dx,al ; Send it .
add si,4 ; Point to next color .
; (There is a null chr. after every color.)
loop PalLoop1
ret
endp CopyPal1
proc CopyBitmap1
; BMP graphics are saved upside-down .
; Read the graphic line by line (200 lines in VGA format),
; displaying the lines from bottom to top.
mov ax, 0A000h
mov es, ax
mov cx,200
PrintBMPLoop1 :
push cx
; di = cx*320, point to the correct screen line
mov di,cx
shl cx,6
shl di,8
add di,cx
; Read one line
mov ah,3fh
mov cx,320
mov dx,offset ScrLine
int 21h
; Copy one line into video memory
cld ; Clear direction flag, for movsb
mov cx,320
mov si,offset ScrLine
rep movsb ; Copy line to the screen
 ;rep movsb is same as the following code :
 ;mov es:di, ds:si
 ;inc si
 ;inc di
 ;dec cx
 ;loop until cx=0
pop cx
loop PrintBMPLoop1
ret
endp CopyBitmap1





proc OpenFile
; Open file
mov ah, 3Dh
xor al, al
mov dx, offset filename_screen_S
int 21h
jc openerror2
mov [filehandle], ax
ret
openerror2 :
mov dx, offset ErrorMsg
mov ah, 9h
int 21h
ret
endp OpenFile
proc OpenFile_YouLose
; Open file
mov ah, 3Dh
xor al, al
mov dx, offset filename_screen_Ss
int 21h
jc openerror21
mov [filehandle], ax
ret
openerror21 :
mov dx, offset ErrorMsg
mov ah, 9h
int 21h
ret
endp OpenFile_YouLose
proc ReadHeader
; Read BMP file header, 54 bytes
mov ah,3fh
mov bx, [filehandle]
mov cx,54
mov dx,offset Header
int 21h
ret
endp ReadHeader
proc ReadPalette
; Read BMP file color palette, 256 colors * 4 bytes (400h)
mov ah,3fh
mov cx,400h
mov dx,offset Palette
int 21h
ret
endp ReadPalette
proc CopyPal
; Copy the colors palette to the video memory
; The number of the first color should be sent to port 3C8h
; The palette is sent to port 3C9h
mov si,offset Palette
mov cx,256
mov dx,3C8h
mov al,0
; Copy starting color to port 3C8h
out dx,al
; Copy palette itself to port 3C9h
inc dx
PalLoop:
; Note: Colors in a BMP file are saved as BGR values rather than RGB .
mov al,[si+2] ; Get red value .
shr al,2 ; Max. is 255, but video palette maximal
; value is 63. Therefore dividing by 4.
out dx,al ; Send it .
mov al,[si+1] ; Get green value .
shr al,2
out dx,al ; Send it .
mov al,[si] ; Get blue value .
shr al,2
out dx,al ; Send it .
add si,4 ; Point to next color .
; (There is a null chr. after every color.)
loop PalLoop
ret
endp CopyPal
proc CopyBitmap
; BMP graphics are saved upside-down .
; Read the graphic line by line (200 lines in VGA format),
; displaying the lines from bottom to top.
mov ax, 0A000h
mov es, ax
mov cx,200
PrintBMPLoop :
push cx
; di = cx*320, point to the correct screen line
mov di,cx
shl cx,6
shl di,8
add di,cx
; Read one line
mov ah,3fh
mov cx,320
mov dx,offset ScrLine
int 21h
; Copy one line into video memory
cld ; Clear direction flag, for movsb
mov cx,320
mov si,offset ScrLine
rep movsb ; Copy line to the screen
 ;rep movsb is same as the following code :
 ;mov es:di, ds:si
 ;inc si
 ;inc di
 ;dec cx
 ;loop until cx=0
pop cx
loop PrintBMPLoop
ret
endp CopyBitmap






 PROC check_collision
  mov  ax, [CenterSquare_X]
  sub  ax, [CenterApple_X]
  jns  DeltaFstX
  neg  ax
DeltaFstX:
  cmp  ax, [SnakeBody]
  jnb  NoCollision

  mov  ax, [CenterSquare_Y]
  sub  ax, [CenterApple_Y]
  jns  DeltaFstY
  neg  ax
DeltaFstY:
  cmp  ax, [SnakeBody]
  jnb  NoCollision
Collision:
	
	Inc [Player_Score_fst]
	cmp [Player_Score_fst], 53 ; 58 is '9' in ascii
	jne score_Show1
	
	
	inc [level_fst]
	mov [Player_Score_fst], 49 ;  set to 1
	mov cx, [word ptr level_fst]
		
		INC [Speed]
		
		
	
	
	score_Show1:
	; change apple postion
	CALL rnd_y
    CALL rnd_x
    
	; show score
	xor dx , dx
	CALL set_cursor_Pfst
	CALL Score_fst
	
	
	
	
	
   

NoCollision:
call check_bounds ; checks for out of bounds
    RET
	
ENDP check_collision ; hitbox code

 PROC check_Collision_player_Player1
  mov  ax, [FstCenterSquare_FstX]
  sub  ax, [FstCenterApple_FstX]
  jns  DeltaFstX_player1
  neg  ax
DeltaFstX_player1:
  cmp  ax, [FstBody]
  jnb  NoCollision_player_player1

  mov  ax, [FstCenterSquare_FstY]
  sub  ax, [FstCenterApple_FstY]
  jns  DeltaFstY_player1
  neg  ax
DeltaFstY_player1:
  cmp  ax, [FstBody]
  jnb  NoCollision_player_player1
Collision_player:
	
	Inc [Player_Score_Scnd]
	cmp [Player_Score_Scnd], 58 ; 58 is '9' in ascii
	jne score_Show
	
	
	inc [level_Scnd]
	mov [Player_Score_Scnd], 49 ;  set to 1
	mov cx, [word ptr level_Scnd]
		
	INC [Speed]
		
		
	
	
	score_Show:
	; change apple postion
    
	CALL rnd_y_player1
    CALL rnd_x_player1
	
	call set_cursor_PScnd
	call Score_Scnd
	
	
	
   

NoCollision_player_player1:
call check_bounds ; checks for out of bounds
    RET
	
ENDP check_Collision_player_Player1 ; hitbox code
;Randoms 
PROC rand2num1toValue_FstY
    push dx
    push bx
	
    xor dx, dx          ; Compute randval(DFstX) mod 10 to get num
	
	
	
    mov bx, 14          ;     between 1 and border height / 5
    div bx
    inc dx             ; DFstX = modulo from division
	                 
    xor ax , ax
	mov al ,5 ; make sure it stays on the y axis of 5 or current value of body and apple
	mul dx
	mov [Random_y],ax 
	
    pop bx
    pop dx
    RET
	ENDP rand2num1toValue_FstY
	
	
	
	PROC rand2num1toValue_FstX
	push dx
    push bx
	 
    xor dx, dx          ; Compute randval(DFstX) mod 10 to get num
	
	xor dx, dx          ; Compute randval(DFstX) mod 10 to get num
    mov bx, 90          ; Set the range 
    div bx              ; Divide by the range
	inc dx
    mov [Random_x], dx
	
    pop bx
    pop dx
    ret
	ENDP rand2num1toValue_FstX
	
	
	; Set LCG PRNG seed to system timer ticks
; Inputs:   AFstX = seed
; Modifies: AFstX 
; Return:   nothing 
Proc srandsystime
    xor ax, ax          ; Int 1Ah/AH=0 to get system timer in CFstX:DFstX 
    int 1Ah
    mov [seed], dx      ; seed = 16-bit value from DFstX
    ret
ENDP srandsystime

PROC rand
    push dx
    mov ax, 25173       ; LCG Multiplier
    mul [word ptr seed] ; DFstX:AFstX = LCG multiplier * seed
    add ax, 13849       ; Add LCG increment value
    mov [seed], ax      ; Update seed
    ; AFstX = (multiplier * seed + increment) mod 65536
    pop dx
    ret
ENDP rand

Proc rnd_x
		
	call srandsystime   ; Seed PRNG with system time, call once only 
    call rand           ; Get a random number in AFstX
    call rand2num1toValue_FstX
    push bx
	
	MOV bx , [Random_x]
	add bx , 25
    mov [Xapple], bx
	pop bx
    ret
ENDP rnd_x
Proc rnd_x_player1
		
	call srandsystime   ; Seed PRNG with system time, call once only 
    call rand           ; Get a random number in AFstX
    call rand2num1toValue_FstX
    push bx
	
	MOV bx , [Random_x]
	add bx , 175
    mov [Fstxapple], bx
	pop bx
    ret
ENDP rnd_x_player1

Proc rnd_y_player1
	call srandsystime   ; Seed PRNG with system time, call once only 
    call rand           ; Get a random number in AFstX
    call rand2num1toValue_FstY
	push bx
	
	MOV bx , [Random_y]
	add bx , 49
    mov [FstYapple], bx
	pop bx
    RET
ENDP rnd_y_player1

Proc rnd_y
	call srandsystime   ; Seed PRNG with system time, call once only 
    call rand           ; Get a random number in AFstX
    call rand2num1toValue_FstY
	push bx
	
	MOV bx , [Random_y]
	add bx , 49
    mov [yapple], bx
	pop bx
    RET
ENDP rnd_y


PROC check_bounds
    ; Check if the snake's head is out of bounds
    mov ax, [FstX]
    cmp ax, 174
    jl out_of_bounds1   ; Jump if FstX coordinate is less than 40

    mov ax, [FstX]
    cmp ax, 274
    jg out_of_bounds1   ; Jump if FstX coordinate is greater than 240

    mov ax, [FstY]
    cmp ax, 24
    jl out_of_bounds1   ; Jump if FstY coordinate is less than 50

    mov ax, [FstY]
    cmp ax, 124
    jg out_of_bounds1   ; Jump if FstY coordinate is greater than 150
	
	
	mov ax, [X]
    cmp ax, 24
    jl out_of_bounds2   ; Jump if FstX coordinate is less than 40

    mov ax, [X]
    cmp ax, 124
    jg out_of_bounds2   ; Jump if FstX coordinate is greater than 240

    mov ax, [Y]
    cmp ax, 24
    jl out_of_bounds2   ; Jump if FstY coordinate is less than 50

    mov ax, [Y]
    cmp ax, 124
    jg out_of_bounds2   ; Jump if FstY coordinate is greater than 150
	ret
	out_of_bounds1:
		mov [speed],0
		mov [X] , 75
		mov [Y] , 50
		mov [screenFlag],1
		call OpenFile
		call ReadHeader
		call ReadPalette
		call CopyPal
		call CopyBitmap
	jmp check
	out_of_bounds2:
		mov [speed],0
		mov [FstX] , 250
		mov [FstY] , 50
		mov [screenFlag],2
		call OpenFile1
		call ReadHeader1
		call ReadPalette1
		call CopyPal1
		call CopyBitmap1
	
   


check:
	MOV AH, 01h       ; Function 01h - Check for Key Press
    INT 16h
    JZ check    ; Jump if ZF is set (no key pressed)

    MOV AH, 00h       ; Function 00h - Read Key Stroke
    INT 16h
	cmp al , 'p'
	jne check
	



Reset:
call reset_game
    RET
ENDP check_bounds	




PROC game_logic
	push dx ; for timer
    CHECK_TIME:                      ;time checking loop
        MOV AH, 2Ch                  ;get the system time
        INT 21h                      ;is the current time equal to the previous one(TIME_AUX)?
        CMP DL, [TIME_AUX]           ;is the current time equal to the previous one(TIME_AUX)?
		
        JE CHECK_TIME              ; if it is the same, skip updating the game state
		
        ; If it reaches this point, it's because the time has passed
        MOV [TIME_AUX], DL           ;update time
		pop dx; for timer
		call Timer ; check timer
		
		CALL Keyboard
		
		call draw
		
		
		
		
        RET

ENDP game_logic
;SinglePlayer starts here

proc OpenFile_Score
; Open file for reading and writing
	mov ah, 3Dh
	mov al, 2
	mov dx, offset filename_S
	int 21h
	jc openerror
	mov [filehandle_score_S], ax
	ret
	openerror :
	mov dx, offset ErrorMsg_S
	mov ah, 9h
	int 21h
	ret
endp OpenFile_Score
proc WriteToFile 
	; Write score to file
	mov ah,40h	
	mov bx, [filehandle_score_S]
	
	
	
	cmp bx, 0
    je file_not_open ; check if file not open
	
	
	mov ah, 3Fh
    mov bx, [filehandle_score_S]

    ; Check if file is not open
    cmp bx, 0
    je file_not_open
	
	
    ; Read the first 2 characters from the file into MaxScore_S
    mov cx, 2
	
    lea dx, [MaxScore_S]
	
	
	
    int 21h
	
    ; Convert ASCII characters to numeric value
	
    mov al, [MaxScore_S]
    sub al, '0' ; get level_S value numeric
    mov ah, 0
	shl ax , 3
	add al , [MaxScore_S+1] ; currently dosent check good but GL
	sub al ,'0'
	
	
	mov [crrMax_S], ax
	
	mov ax , 0
	
	mov al, [level_S]
    mov ah, 0
	shl ax , 3
	add al , [Player_Score_S]
	sub al ,'0'
	
    ; Compare the scores
    cmp ax, [crrMax_S]
    jng NotBigger
	
	
	
	mov ah, 40h
	mov cx, 2 ; Num of bytes 
	mov al , [level_S] ; put player score in msg
	add al , '0'
	mov [Player_Buffer_S] , al
	mov al , [Player_Score_S] ; put player score in msg
	mov [Player_Buffer_S+1] , al
	
	mov dx, offset Player_Buffer_S
	int 21h
	jc write_error
	mov ah, 40h
	mov cx, 3 ; Num of bytes 
	mov dx, offset PlayerName_S+2
	int 21h
	
	NotBigger:
	
	
ret

file_not_open:
jmp exit

write_error:
    jmp exit
	

endp WriteToFile


proc set_cursor
MOV AH, 02h
    MOV BH, 0
    MOV DH, [Yat_S] ; Adjust row for the score display
    MOV DL, [Xat_S] ; Adjust column for the score display
    INT 10h
	RET
	ENDP set_cursor
PROC Score

	
    mov ah, 09h  ; Display character function
	mov bh, 0    ; Page number
	mov cx, 1    ; Number of times to display character
	ADD AL, '0'        ; Convert to ASCII
	mov al, [Player_Score_S]  ; display level_S
	
    mov bl, [level_S]    ; Attribute (text color)
    int 10h
    MOV AH, 02h
    MOV BH, 0
    MOV DH, 1 ; Adjust row for the score display
    MOV DL, 1 ; Adjust column for the score display
    INT 10h
	
    
    MOV AH, 02h
    MOV BH, 0
    MOV DH, 1 ; Adjust row for the score displayx
    MOV DL, 20 ; Adjust column for the score display y
	int 10h
	
ENDP Score

  
PROC moves_S

    MOV AH, 01h       ; Function 01h - Check for Key Press
    INT 16h
    JZ exit_moves_S     ; Jump if ZF is set (no key pressed)

    MOV AH, 00h       ; Function 00h - Read Key Stroke
    INT 16h

    ; Check if AH contains 'W' (ASCII value 87)
    CMP AL, 'w'
	je w_pressed

    ; Check if AH contains 'D' (ASCII value 68)
    CMP AL, 'd'
    je d_pressed
    ; Check if AH contains 'A' (ASCII value 65)
    CMP AL, 'a'
    je a_pressed
    ; Check if AH contains 'S' 'S'
    CMP AL, 's'
    JMP s_pressed

exit_moves_S:

    RET

ENDP moves_S	

PROC w_pressed
    mov [PressFlag_S], 1
	mov cx , [Speed_S]
	SUB [Y_S], cx
	
	

    CHECK_TIME1:         ;time checking loop
    MOV AH,2Ch 					 ;get the system time
    INT 21h 			 ;is the current time equal to the previous one(TIME_AUX_S)?
    CMP DL,[TIME_AUX_S] 			 ;is the current time equal to the previous one(TIME_AUX_S)?

    JE CHECK_TIME1
    MOV [TIME_AUX_S],DL 
	
    CALL draw_S
	
	call moves_S
	CMP [PressFlag_S],1
    je w_pressed
    
	RET
ENDP w_pressed

PROC d_pressed
    mov [PressFlag_S], 2
	mov cx , [Speed_S]
	ADD [X_S], cx
	

    CHECK_TIME2:          ;time checking loop
    MOV AH,2Ch 					 ;get the system time
    INT 21h 			 ;is the current time equal to the previous one(TIME_AUX_S)?
    CMP DL,[TIME_AUX_S] 			 ;is the current time equal to the previous one(TIME_AUX_S)?

    JE CHECK_TIME2
    MOV [TIME_AUX_S],DL 
	
    CALL draw_S
	
	call moves_S
	cmp [PressFlag_S],2
    je d_pressed
	ret
ENDP d_pressed

PROC a_pressed
    mov [PressFlag_S], 3
	mov cx , [Speed_S]
	SUB [X_S], cx
	

    CHECK_TIME3:           ;time checking loop
    MOV AH,2Ch 					 ;get the system time
    INT 21h 			 ;is the current time equal to the previous one(TIME_AUX_S)?
    CMP DL,[TIME_AUX_S] 			 ;is the current time equal to the previous one(TIME_AUX_S)?
	
    JE CHECK_TIME3
    MOV [TIME_AUX_S],DL 
	
    CALL draw_S
	
	call moves_S
	CMP [PressFlag_S],3
    je a_pressed
	ret
ENDP a_pressed

PROC s_pressed
    mov [PressFlag_S], 4
	mov cx , [Speed_S]
	ADD [Y_S], cx
    
    CHECK_TIME4:           ;time checking loop
    MOV AH,2Ch 					 ;get the system time
    INT 21h 			 ;is the current time equal to the previous one(TIME_AUX_S)?
    CMP DL,[TIME_AUX_S] 			 ;is the current time equal to the previous one(TIME_AUX_S)?

    JE CHECK_TIME4
	
    MOV [TIME_AUX_S],DL 
    CALL draw_S
	
	call moves_S
	
	CMP [PressFlag_S],4
    je s_pressed
	RET
ENDP s_pressed
 



PROC draw_S


	
    MOV AX, 0C07h     ; Function 0Ch, Set Pixel Color
    MOV BH, 0         ; Page number (usually 0 in mode 13h)

    ; Draw the gray border
    MOV CX, 40         ; X_S-coordinate of the left border
    MOV DX, 50         ; Y_S-coordinate of the top border
    MOV SI, 0
    MOV DI, 0
    MOV AL, 8          ; Color for gray pixels

    draw_row_loop_border_S:
        draw_pixel_loop_border_S:
            INC CX
            INC DI

            INT 10h
            CMP DI, 240       ; Width of the border
            JNE draw_pixel_loop_border_S

            SUB CX, 240        ; Reset X_S-coordinate to start_game
            MOV DI, 0
            INC DX
            INC SI

            CMP SI, 150        ; Height of the border
            JNE draw_row_loop_border_S

    
   
    
    MOV AX, 0C07h     ; Function 0Ch, Set Pixel Color
    MOV BH, 0         ; Page number (usually 0 in mode 13h)
    
    ; Draw the apple
	
	
	
	
    MOV CX, [Xapple_S]  ; X_S-coordinate of the apple
    MOV DX, [yapple_S]  ; Y_S-coordinate of the apple
    MOV SI, 0
    MOV DI, 0
    MOV AL, 4          ; Color (choose a color different from the snake)

    draw_row_loop_apple_S:
        draw_pixel_loop_apple_S:
            INC CX
            INC DI
            
            INT 10h
            CMP DI, [SnakeBody_S]  ; size of the apple must be the same size of the snake for collison to work
            JNE draw_pixel_loop_apple_S
            
            SUB CX, [SnakeBody_S]  ; without this, the apple is not drawing up; it's drawing to the side
            MOV DI, 0
            INC DX
            INC SI
            
            CMP SI, [SnakeBody_S];  size of the apple must be the same size of the snake for collison to work
            JNE draw_row_loop_apple_S
	
    ; Draw the snake
    MOV CX, [X_S]       ; X_S-coordinate of the snake head
    MOV DX, [Y_S]       ; Y_S-coordinate of the snake head
    MOV SI, 0
    MOV DI, 0
    MOV AL, 2         ; Color green (you can choose a different color)

    draw_row_loop_S:
        draw_pixel_loop_S:
            INC CX
            INC DI
            
            INT 10h
            CMP DI, [SnakeBody_S] 
            JNE draw_pixel_loop_S
            
            SUB CX, [SnakeBody_S]
            MOV DI, 0
            INC DX
            INC SI
            
            CMP SI, [SnakeBody_S] 
            JNE draw_row_loop_S



	
	
mov  si, [SnakeBody_S]      ; 5 (best to keep this an odd number: 3, 5, 7, ...
shr  si, 1                ; 2

mov  bx, [X_S]
lea  ax, [bx + si + 1]    ; X_S + 3
mov  [CenterSquare_X_S], ax

mov  bx, [Y_S]
lea  ax, [bx + si]        ; Y_S + 2
mov  [CenterSquare_Y_S], ax

mov  bx, [Xapple_S]
lea  ax, [bx + si + 1]    ; Xapple_S + 3
mov  [CenterApple_X_S], ax

mov  bx, [yapple_S]
lea  ax, [bx + si]        ; Yapple + 2
mov  [CenterApple_Y_S], ax
call check_collision_S


		
	
	
    RET
ENDP draw_S


proc OpenFile_S
; Open file
mov ah, 3Dh
xor al, al
mov dx, offset filename_screen_S
int 21h
jc openerror1_S
mov [filehandle_S], ax
ret
openerror1_S :
mov dx, offset ErrorMsg_S
mov ah, 9h
int 21h
ret
endp OpenFile_S
proc ReadHeader_S
; Read BMP file header, 54 bytes
mov ah,3fh
mov bx, [filehandle_S]
mov cx,54
mov dx,offset Header_S
int 21h
ret
endp ReadHeader_S
proc ReadPalette_S
; Read BMP file color palette, 256 colors * 4 bytes (400h)
mov ah,3fh
mov cx,400h
mov dx,offset Palette_S
int 21h
ret
endp ReadPalette_S
proc CopyPal_S
; Copy the colors palette to the video memory
; The number of the first color should be sent to port 3C8h
; The palette is sent to port 3C9h
mov si,offset Palette_S
mov cx,256
mov dx,3C8h
mov al,0
; Copy starting color to port 3C8h
out dx,al
; Copy palette itself to port 3C9h
inc dx
PalLoop_S:
; Note: Colors in a BMP file are saved as BGR values rather than RGB .
mov al,[si+2] ; Get red value .
shr al,2 ; Max_S. is 255, but video palette maximal
; value is 63. Therefore dividing by 4.
out dx,al ; Send it .
mov al,[si+1] ; Get green value .
shr al,2
out dx,al ; Send it .
mov al,[si] ; Get blue value .
shr al,2
out dx,al ; Send it .
add si,4 ; Point to next color .
; (There is a null chr. after every color.)
loop PalLoop_S
ret
endp CopyPal_S
proc CopyBitmap_S
; BMP graphics are saved upside-down .
; Read the graphic line by line (200 lines in VGA format),
; displaying the lines from bottom to top.
mov ax, 0A000h
mov es, ax
mov cx,200
PrintBMPLoop_S :
push cx
; di = cx*320, point to the correct screen line
mov di,cx
shl cx,6
shl di,8
add di,cx
; Read one line
mov ah,3fh
mov cx,320
mov dx,offset ScrLine_S
int 21h
; Copy one line into video memory
cld ; Clear direction flag, for movsb
mov cx,320
mov si,offset ScrLine_S
rep movsb ; Copy line to the screen
pop cx
loop PrintBMPLoop_S
ret
endp CopyBitmap_S



 PROC check_collision_S
  mov  ax, [CenterSquare_X_S]
  sub  ax, [CenterApple_X_S]
  jns  DeltaX_S
  neg  ax
DeltaX_S:
  cmp  ax, [SnakeBody_S]
  jnb  NoCollision_S

  mov  ax, [CenterSquare_Y_S]
  sub  ax, [CenterApple_Y_S]
  jns  DeltaY_S
  neg  ax
DeltaY_S:
  cmp  ax, [SnakeBody_S]
  jnb  NoCollision_S
Collision_S:
	
	Inc [Player_Score_S]
	cmp [Player_Score_S], 58 ; 58 is '9' in ascii
	jne score_Show_S
	
	
	inc [level_S]
	mov [Player_Score_S], 49 ;  set to 1
	mov cx, [word ptr level_S]
		
		INC [Speed_S]
		
		
	
	; show score
	score_Show_S:
	 CALL rnd_y_S;change apple location y
    CALL rnd_x_S ; change apple location x
	
	
	call set_cursor ; display score
	call Score 
	
	
	
	
	
   ; change apple postion
   

NoCollision_S:
call check_bounds_S ; checks for out of bounds
    RET
	
ENDP check_collision_S ; hitbox code
;Randoms 
PROC rand2num1toValue_Y_S
    push dx
    push bx
	
    xor dx, dx          ; Compute randval(DX) mod 10 to get num
	
	
	
    mov bx, 25          ;     between 1 and border height / 5
    div bx
    inc dx             ; DX = modulo from division
	                 
    xor ax , AX
	mov al ,5 ; make sure it stays on the y axis of 5 or current value of body and apple
	mul DX
	mov [Random_y_S],ax 
	ADD [Random_y_S], 50 ; 50 is border
	
	
    pop bx
    pop dx
    RET
	ENDP rand2num1toValue_Y_S
	
	PROC rand2num1toValue_X_S
	push dx
    push bx
	 
    xor dx, dx          ; Compute randval(DX) mod 10 to get num
	
	xor dx, dx          ; Compute randval(DX) mod 10 to get num
    mov bx, 201          ; Set the range (240 - 40 + 1 = 201)
    div bx              ; Divide by the range
	inc dx
    mov [Random_x_S], dx
	
    pop bx
    pop dx
    ret
	ENDP rand2num1toValue_X_S
	; Set LCG PRNG seed to system timer ticks
; Inputs:   AX = seed
; Modifies: AX 
; Return:   nothing 

Proc srandsystime_S
    xor ax, ax          ; Int 1Ah/AH=0 to get system timer in CX:DX 
    int 1Ah
    mov [SEED_S], dx      ; seed = 16-bit value from DX
    ret
ENDP srandsystime_S

PROC rand_S
    push dx
    mov ax, 25173       ; LCG Multiplier
    mul [word ptr SEED_S] ; DX:AX = LCG multiplier * seed
    add ax, 13849       ; Add LCG increment value
    mov [SEED_S], ax      ; Update seed
    ; AX = (multiplier * seed + increment) mod 65536
    pop dx
    ret
ENDP rand_S

Proc rnd_x_S
		
	call srandsystime_S   ; Seed PRNG with system time, call once only 
    call rand_S           ; Get a random number in AX
    call rand2num1toValue_X_S
    push bx
	
	MOV bx , [Random_x_S]
	add bx , 40
    mov [Xapple_S], bx
	pop bx
    ret
ENDP rnd_x_S

Proc rnd_y_S
	call srandsystime_S   ; Seed PRNG with system time, call once only 
    call rand_S           ; Get a random number in AX
    call rand2num1toValue_Y_S

	
	MOV bx , [Random_y_S]
    mov [yapple_S], bx
    RET
ENDP rnd_y_S

proc CloseFile_test
  mov  ah, 3Eh
  mov  bx, [filehandle_S]
  int  21h
  ret
endp CloseFile_test
PROC check_bounds_S

    ; Check if the snake's head is out of bounds
    mov ax, [X_S]
    cmp ax, 40
    jl out_of_bounds_S   ; Jump if X_S coordinate is less than 40

    mov ax, [X_S]
    cmp ax, 280
    jg out_of_bounds_S   ; Jump if X_S coordinate is greater than 240

    mov ax, [Y_S]
    cmp ax, 50
    jl out_of_bounds_S   ; Jump if Y_S coordinate is less than 50

    mov ax, [Y_S]
    cmp ax, 200
    jg out_of_bounds_S   ; Jump if Y_S coordinate is greater than 150
	
    ret

out_of_bounds_S:

call OpenFile_Score
call WriteToFile
CALL CloseFile


;end game screen
call OpenFile_YouLose
call ReadHeader
call ReadPalette
call CopyPal
call CopyBitmap


check_S:
	MOV AH, 01h       ; Function 01h - Check for Key Press
    INT 16h
    JZ check_S    ; Jump if ZF is set (no key pressed)

    MOV AH, 00h       ; Function 00h - Read Key Stroke
    INT 16h
	cmp al , 'p' ; play again
    je reset_game_S
	
	
jmp out_of_bounds_S
    RET
ENDP check_bounds_S	
	
 proc reset_game_S

        ; Your reset code goes here
        ; reset game and then rerun it
		
		mov [X_S], 100
		mov [Y_S],100
		mov [Xapple_S],80
		mov [yapple_S],80
		mov [Speed_S] , 5
		mov [level_S] , 1
		mov [Player_Score_S] , 48
			
		jmp start_game
		; reset color pallet
		
        JMP game_loop
		endp reset_game_S

PROC game_logic_S

    CHECK_TIME_S:                      ;time checking loop
        MOV AH, 2Ch                  ;get the system time
        INT 21h                      ;is the current time equal to the previous one(TIME_AUX_S)?
        CMP DL, [TIME_AUX_S]           ;is the current time equal to the previous one(TIME_AUX_S)?
		
        JE CHECK_TIME_S              ; if it is the same, skip updating the game state

        ; If it reaches this point, it's because the time has passed
        MOV [TIME_AUX_S], DL           ;update time
		
		
		CALL moves_S
		
		call draw_S
		
        RET

ENDP game_logic_S

freq equ [bp+4]
proc NoteItself
    push bp
    mov bp, sp
	
    

    ; Send the frequency to the speaker port
    in al, 61h
    or al, 00000011b
    out 61h, al           ; Enable speaker

    mov al, 0B6h
    out 43h, al           ; Set the command byte for timer channel 2
	
	mov di, freq          ; Load the frequency value
    mov dx, 12h           ; Load the high word of the divisor
    mov ax, 2870h         ; Load the low word of the divisor
    div di                ; Divide DX:AX by frequency to get the divisor

    
    out 42h, al           ; Send lower byte
    mov al, ah
    out 42h, al           ; Send upper byte
	
	
	
    pop bp
    ret 2
endp NoteItself

proc jingle_start

;jingle_bell
cmp [easteregg] , 3
je twinkle

cmp [easteregg] , 2
je happybirthday

inc [easteregg]
push [note_E]   ; E
call NoteItself
push 400        ; Duration
call delay

push [note_E]   ; E
call NoteItself
push 400        ; Duration
call delay

push [note_E]   ; E
call NoteItself
push 800        ; Duration (longer for the pause)
call delay

push [note_E]   ; E
call NoteItself
push 400        ; Duration
call delay

push [note_E]   ; E
call NoteItself
push 400        ; Duration
call delay

push [note_E]   ; E
call NoteItself
push 800        ; Duration (longer for the pause)
call delay

push [note_E]   ; E
call NoteItself
push 400        ; Duration
call delay

push [note_G]   ; G
call NoteItself
push 400        ; Duration
call delay

push [note_C]   ; C
call NoteItself
push 400        ; Duration
call delay

push [note_D]   ; D
call NoteItself
push 400        ; Duration
call delay

push [note_E]   ; E
call NoteItself
push 400        ; Duration
call delay

push [note_F]   ; F
call NoteItself
push 400        ; Duration
call delay

push [note_F]   ; F
call NoteItself
push 1000        ; Duration (longer for the pause)
call delay

push [note_F]   ; F
call NoteItself
push 400        ; Duration
call delay

push [note_F]   ; F
call NoteItself
push 400        ; Duration
call delay

push [note_F]   ; F
call NoteItself
push 400        ; Duration
call delay

push [note_E]   ; E
call NoteItself
push 400        ; Duration
call delay

push [note_E]   ; E
call NoteItself
push 400        ; Duration
call delay

push [note_E]   ; E
call NoteItself
push 400        ; Duration
call delay

push [note_E]   ; E
call NoteItself
push 400        ; Duration
call delay

push [note_D]   ; D
call NoteItself
push 400        ; Duration
call delay

push [note_D]   ; D
call NoteItself
push 400        ; Duration
call delay

push [note_E]   ; E
call NoteItself
push 400        ; Duration
call delay

push [note_D]   ; D
call NoteItself
push 800        ; Duration (longer for the pause)
call delay
jmp end_songs
	happybirthday:
	; Happy Birthday
	inc [easteregg]
push [note_C]
call NoteItself
push 300
call delay

push [note_C]
call NoteItself
push 300
call delay

push [note_D]
call NoteItself
push 600
call delay

push [note_C]
call NoteItself
push 600
call delay

push [note_F]
call NoteItself
push 600
call delay

push [note_E]
call NoteItself
push 1200
call delay

push [note_C]
call NoteItself
push 300
call delay

push [note_C]
call NoteItself
push 300
call delay

push [note_D]
call NoteItself
push 600
call delay

push [note_C]
call NoteItself
push 600
call delay

push [note_G]
call NoteItself
push 600
call delay

push [note_F]
call NoteItself
push 1200
call delay
jmp end_songs
twinkle:
; Twinkle, Twinkle, Little Star
push [note_C]
call NoteItself
push 300
call delay

push [note_C]
call NoteItself
push 300
call delay

push [note_G]
call NoteItself
push 300
call delay

push [note_G]
call NoteItself
push 300
call delay

push [note_A]
call NoteItself
push 300
call delay

push [note_A]
call NoteItself
push 300
call delay

push [note_G]
call NoteItself
push 600
call delay

push [note_F]
call NoteItself
push 300
call delay

push [note_F]
call NoteItself
push 300
call delay

push [note_E]
call NoteItself
push 300
call delay

push [note_E]
call NoteItself
push 300
call delay

push [note_D]
call NoteItself
push 300
call delay

push [note_D]
call NoteItself
push 300
call delay

push [note_C]
call NoteItself
push 600
call delay
mov [easteregg], 1
	end_songs:
	in al,61h
        		and al,11111100b
        		out 61h,al 	;disable speaker
				jmp start_game

ret
endp jingle_start

	
start_game:
    MOV ax, @data
    MOV DS, ax
	
	MOV AX, 13h
    INT 10h
	
	call OpenFile_start
	call ReadHeader1
	call ReadPalette1
	call CopyPal1
	call CopyBitmap1
	
	
	
	loopstart:

		MOV AH, 00h       ; Function 00h - Read Key Stroke
		INT 16h
		
		cmp al , 'm'
		je Multiplier
		cmp al , 's'
		je Singplayer
		cmp al , 'q'
		je jingle_start
		jmp loopstart
		
	Singplayer:	
	cmp [savedscore_S] , 0
	jne alreadysaved
	
	mov ah,40h ; prints the msg
	mov bx,1
	mov cx, 23
	mov dx,OFFSET GetName_S
	int 21h 
	mov ah, 0Ah
	mov dx, offset PlayerName_S
	mov bx, dx
	mov [byte ptr bx], 4
	
	int 21h
    mov [savedscore_S],1
	
	
	
	
	
	
	alreadysaved:
	MOV AX, 13h
    INT 10h ; Set video mode 13h (320x200 pixels, 256 colors)
	
	
    
	
	
    game_loop_S:
	 CALL game_logic_S
        ; Check for key press to reset the game
        
        

        JMP game_loop_S

	Multiplier:
	
	mov ah , 2ch ; reposnsible for time setting of the game
	int 21h
	mov [Timer_],DH
	sub [Timer_],1
	;
	call OpenFile_Tut
	call ReadHeader1
	call ReadPalette1
	call CopyPal1
	call CopyBitmap1
	Tut:

		MOV AH, 00h       ; Function 00h - Read Key Stroke
		INT 16h
		
		cmp al , 'c'
		je After_Tut
		jmp Tut
		
		After_Tut:
    MOV ax, 13h
    INT 10h ; Set video mode 13h (320x200 pixels, 256 colors)
	
	
    game_loop:
	 CALL game_logic
	 
	 
        ; Check for key press to reset the game
        
        

        JMP game_loop
	
	  proc reset_game
        ; FstYour reset code goes here
        ; reset game and then rerun it
		mov [Timer_],0
		mov [FstX], 200
		mov [FstY],70
		mov [X] , 100
		mov [Y] , 70
		mov [Xapple],80
		mov [yapple], 80
		mov [Fstxapple],200
		mov [Fstyapple],80
		mov [Speed] , 5
		mov [level_fst] , 1
		mov [Player_Score_Scnd],48
		mov [Player_Score_fst] , 48
		mov [PressFlag1],0
		mov [PressFlag2],0
		
		jmp start_game
		; reset color pallet
		
        JMP game_loop
		endp reset_game

exit:
    mov ax, 4c00h
    int 21h
END start_game
