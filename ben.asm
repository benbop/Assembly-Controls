; IDFK WTF ARE THE FIRST 2 CHARCHTERS DONT FUCKING CHANGE THEM PLS JUST REMEMBER
; FIRST 2 YOU WRITE FOR ABSOLUTE NO FUCKIGN REASON
; SECOND TWO YOU WRITE ONLY IF THE SCORE IS BIGGER 
; LAST THREE ARE THE FUCKING Name
IDEAL
MODEL small
STACK 100h
DATASEG
; --------------------------
Player_Buffer db 2 dup(?),0
filename_screen db 'Again.bmp',0
filehandle dw ?
Header db 54 dup (0)
Palette db 256*4 dup (0)
ScrLine db 320 dup (0) ; unrelated to actual image size
ErrorMsg db 'Error', 13, 10 ,'$'

filename db 'score.txt',0
filehandle_score dw ?

PlayerName db 6 dup(?),13 ,'$'

MaxScore db 2 dup(?),0
Y dw 100 ; of charchter
X dw 100 ; of charchter
TIME_AUX DB 0 ; for waiting in between frames 

PressFlag db 0 ; press flag
SnakeBody dw 5	; body of charcter
Xapple dw 80 ; y of apple
yapple dw 80 ; x of apple
Random_x dw 80 ; my rnds
Random_y dw 80 ; my rnds
Speed dw 5 ; speed of snake
Player_Score DB 48 ; ascii value of 0
level db 1

SEED dw 11

CenterSquare_Y dw 0
CenterSquare_X dw 0
CenterApple_Y dw 0
CenterApple_X dw 0

Xat db 0
Yat db 0

Max db 0
NewLine db ' '
flagforscore db 0

GetName db 'Enter 3 Charchter Name ',13, 10 ,'$'
savedscore db 0

crrMax dw 0
; --------------------------
CODESEG

proc OpenFile_Score
; Open file for reading and writing
	mov ah, 3Dh
	mov al, 2
	mov dx, offset filename
	int 21h
	jc openerror
	mov [filehandle_score], ax
	ret
	openerror :
	mov dx, offset ErrorMsg
	mov ah, 9h
	int 21h
	ret
endp OpenFile_Score
proc WriteToFile 
	; Write score to file
	mov ah,40h	
	mov bx, [filehandle_score]
	
	
	
	cmp bx, 0
    je file_not_open ; check if file not open
	
	
	mov ah, 3Fh
    mov bx, [filehandle_score]

    ; Check if file is not open
    cmp bx, 0
    je file_not_open
	
	
    ; Read the first 2 characters from the file into MaxScore
    mov cx, 2
	
    lea dx, [MaxScore]
	
	
	
    int 21h
	
    ; Convert ASCII characters to numeric value
	
    mov al, [MaxScore]
    sub al, '0' ; get level value numeric
    mov ah, 0
	shl ax , 3
	add al , [MaxScore+1] ; currently dosent check good but GL
	sub al ,'0'
	
	
	mov [crrMax], ax
	
	mov ax , 0
	
	mov al, [level]
    mov ah, 0
	shl ax , 3
	add al , [Player_Score]
	sub al ,'0'
	
    ; Compare the scores
    cmp ax, [crrMax]
    jng NotBigger
	
	
	
	mov ah, 40h
	mov cx, 2 ; Num of bytes 
	mov al , [level] ; put player score in msg
	add al , '0'
	mov [Player_Buffer] , al
	mov al , [Player_Score] ; put player score in msg
	mov [Player_Buffer+1] , al
	
	mov dx, offset Player_Buffer
	int 21h
	jc write_error
	mov ah, 40h
	mov cx, 3 ; Num of bytes 
	mov dx, offset PlayerName+2
	int 21h
	
	NotBigger:
	
	
ret

file_not_open:
jmp exit

write_error:
    jmp exit
	

endp WriteToFile
proc CloseFile
; Close file
mov ah,3Eh
mov bx, [filehandle_score]
int 21h
ret
endp CloseFile

proc set_cursor
MOV AH, 02h
    MOV BH, 0
    MOV DH, [Yat] ; Adjust row for the score display
    MOV DL, [Xat] ; Adjust column for the score display
    INT 10h
	RET
	ENDP set_cursor
PROC Score

	
    mov ah, 09h  ; Display character function
	mov bh, 0    ; Page number
	mov cx, 1    ; Number of times to display character
	ADD AL, '0'        ; Convert to ASCII
	mov al, [Player_Score]  ; display level
	
    mov bl, [level]    ; Attribute (text color)
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

  
PROC moves

    MOV AH, 01h       ; Function 01h - Check for Key Press
    INT 16h
    JZ exit_moves     ; Jump if ZF is set (no key pressed)

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

exit_moves:

    RET

ENDP moves

PROC w_pressed
    mov [PressFlag], 1
	mov cx , [Speed]
	SUB [Y], cx
	
	

    CHECK_TIME1:         ;time checking loop
    MOV AH,2Ch 					 ;get the system time
    INT 21h 			 ;is the current time equal to the previous one(TIME_AUX)?
    CMP DL,[TIME_AUX] 			 ;is the current time equal to the previous one(TIME_AUX)?

    JE CHECK_TIME1
    MOV [TIME_AUX],DL 
	
    CALL draw
	
	call moves
	CMP [PressFlag],1
    je w_pressed
    
	RET
ENDP w_pressed

PROC d_pressed
    mov [PressFlag], 2
	mov cx , [Speed]
	ADD [X], cx
	

    CHECK_TIME2:          ;time checking loop
    MOV AH,2Ch 					 ;get the system time
    INT 21h 			 ;is the current time equal to the previous one(TIME_AUX)?
    CMP DL,[TIME_AUX] 			 ;is the current time equal to the previous one(TIME_AUX)?

    JE CHECK_TIME2
    MOV [TIME_AUX],DL 
	
    CALL draw
	
	call moves
	cmp [PressFlag],2
    je d_pressed
	ret
ENDP d_pressed

PROC a_pressed
    mov [PressFlag], 3
	mov cx , [Speed]
	SUB [X], cx
	

    CHECK_TIME3:           ;time checking loop
    MOV AH,2Ch 					 ;get the system time
    INT 21h 			 ;is the current time equal to the previous one(TIME_AUX)?
    CMP DL,[TIME_AUX] 			 ;is the current time equal to the previous one(TIME_AUX)?
	
    JE CHECK_TIME3
    MOV [TIME_AUX],DL 
	
    CALL draw
	
	call moves
	CMP [PressFlag],3
    je a_pressed
	ret
ENDP a_pressed

PROC s_pressed
    mov [PressFlag], 4
	mov cx , [Speed]
	ADD [Y], cx
    
    CHECK_TIME4:           ;time checking loop
    MOV AH,2Ch 					 ;get the system time
    INT 21h 			 ;is the current time equal to the previous one(TIME_AUX)?
    CMP DL,[TIME_AUX] 			 ;is the current time equal to the previous one(TIME_AUX)?

    JE CHECK_TIME4
	
    MOV [TIME_AUX],DL 
    CALL draw
	
	call moves
	
	CMP [PressFlag],4
    je s_pressed
	RET
ENDP s_pressed
 



PROC draw


	
    MOV AX, 0C07h     ; Function 0Ch, Set Pixel Color
    MOV BH, 0         ; Page number (usually 0 in mode 13h)

    ; Draw the gray border
    MOV CX, 40         ; X-coordinate of the left border
    MOV DX, 50         ; Y-coordinate of the top border
    MOV SI, 0
    MOV DI, 0
    MOV AL, 8          ; Color for gray pixels

    draw_row_loop_border:
        draw_pixel_loop_border:
            INC CX
            INC DI

            INT 10h
            CMP DI, 240       ; Width of the border
            JNE draw_pixel_loop_border

            SUB CX, 240        ; Reset X-coordinate to start
            MOV DI, 0
            INC DX
            INC SI

            CMP SI, 150        ; Height of the border
            JNE draw_row_loop_border

    
   
    
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
call check_collision


		
	
	
    RET
ENDP draw


proc OpenFile
; Open file
mov ah, 3Dh
xor al, al
mov dx, offset filename_screen
int 21h
jc openerror1
mov [filehandle], ax
ret
openerror1 :
mov dx, offset ErrorMsg
mov ah, 9h
int 21h
ret
endp OpenFile
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
pop cx
loop PrintBMPLoop
ret
endp CopyBitmap



 PROC check_collision
  mov  ax, [CenterSquare_X]
  sub  ax, [CenterApple_X]
  jns  DeltaX
  neg  ax
DeltaX:
  cmp  ax, [SnakeBody]
  jnb  NoCollision

  mov  ax, [CenterSquare_Y]
  sub  ax, [CenterApple_Y]
  jns  DeltaY
  neg  ax
DeltaY:
  cmp  ax, [SnakeBody]
  jnb  NoCollision
Collision:
	
	Inc [Player_Score]
	cmp [Player_Score], 58 ; 58 is '9' in ascii
	jne score_Show
	
	
	inc [level]
	mov [Player_Score], 49 ;  set to 1
	mov cx, [word ptr level]
		
		INC [Speed]
		
		
	
	; show score
	score_Show:
	 CALL rnd_y;change apple location y
    CALL rnd_x ; change apple location x
	
	
	call set_cursor ; display score
	call Score 
	
	
	
	
	
   ; change apple postion
   

NoCollision:
call check_bounds ; checks for out of bounds
    RET
	
ENDP check_collision ; hitbox code
;Randoms 
PROC rand2num1toValue_Y
    push dx
    push bx
	
    xor dx, dx          ; Compute randval(DX) mod 10 to get num
	
	
	
    mov bx, 25          ;     between 1 and border height / 5
    div bx
    inc dx             ; DX = modulo from division
	                 
    xor ax , AX
	mov al ,5 ; make sure it stays on the y axis of 5 or current value of body and apple
	mul DX
	mov [Random_y],ax 
	ADD [Random_y], 50 ; 50 is border
	
	
    pop bx
    pop dx
    RET
	ENDP rand2num1toValue_Y
	
	PROC rand2num1toValue_X
	push dx
    push bx
	 
    xor dx, dx          ; Compute randval(DX) mod 10 to get num
	
	xor dx, dx          ; Compute randval(DX) mod 10 to get num
    mov bx, 201          ; Set the range (240 - 40 + 1 = 201)
    div bx              ; Divide by the range
	inc dx
    mov [Random_x], dx
	
    pop bx
    pop dx
    ret
	ENDP rand2num1toValue_X
	; Set LCG PRNG seed to system timer ticks
; Inputs:   AX = seed
; Modifies: AX 
; Return:   nothing 

Proc srandsystime
    xor ax, ax          ; Int 1Ah/AH=0 to get system timer in CX:DX 
    int 1Ah
    mov [seed], dx      ; seed = 16-bit value from DX
    ret
ENDP srandsystime

PROC rand
    push dx
    mov ax, 25173       ; LCG Multiplier
    mul [word ptr seed] ; DX:AX = LCG multiplier * seed
    add ax, 13849       ; Add LCG increment value
    mov [seed], ax      ; Update seed
    ; AX = (multiplier * seed + increment) mod 65536
    pop dx
    ret
ENDP rand

Proc rnd_x
		
	call srandsystime   ; Seed PRNG with system time, call once only 
    call rand           ; Get a random number in AX
    call rand2num1toValue_X
    push bx
	
	MOV bx , [Random_x]
	add bx , 40
    mov [Xapple], bx
	pop bx
    ret
ENDP rnd_x

Proc rnd_y
	call srandsystime   ; Seed PRNG with system time, call once only 
    call rand           ; Get a random number in AX
    call rand2num1toValue_Y

	
	MOV bx , [Random_y]
    mov [yapple], bx
    RET
ENDP rnd_y

proc CloseFile_test
  mov  ah, 3Eh
  mov  bx, [filehandle]
  int  21h
  ret
endp CloseFile_test
PROC check_bounds

    ; Check if the snake's head is out of bounds
    mov ax, [X]
    cmp ax, 40
    jl out_of_bounds   ; Jump if X coordinate is less than 40

    mov ax, [X]
    cmp ax, 280
    jg out_of_bounds   ; Jump if X coordinate is greater than 240

    mov ax, [Y]
    cmp ax, 50
    jl out_of_bounds   ; Jump if Y coordinate is less than 50

    mov ax, [Y]
    cmp ax, 200
    jg out_of_bounds   ; Jump if Y coordinate is greater than 150
	
    ret

out_of_bounds:

call OpenFile_Score
call WriteToFile
CALL CloseFile


;end game screen
call OpenFile
call ReadHeader
call ReadPalette
call CopyPal
call CopyBitmap


check:
	MOV AH, 01h       ; Function 01h - Check for Key Press
    INT 16h
    JZ check    ; Jump if ZF is set (no key pressed)

    MOV AH, 00h       ; Function 00h - Read Key Stroke
    INT 16h
	cmp al , 'p' ; play again
    je reset_game
	
	
jmp out_of_bounds
    RET
ENDP check_bounds	
	
 proc reset_game
        ; Your reset code goes here
        ; reset game and then rerun it
		mov [X], 100
		mov [Y],100
		mov [Xapple],80
		mov [yapple],80
		mov [Speed] , 5
		mov [level] , 1
		mov [Player_Score] , 48
			
		jmp start
		; reset color pallet
		
        JMP game_loop
		endp reset_game

PROC game_logic

    CHECK_TIME:                      ;time checking loop
        MOV AH, 2Ch                  ;get the system time
        INT 21h                      ;is the current time equal to the previous one(TIME_AUX)?
        CMP DL, [TIME_AUX]           ;is the current time equal to the previous one(TIME_AUX)?
		
        JE CHECK_TIME              ; if it is the same, skip updating the game state

        ; If it reaches this point, it's because the time has passed
        MOV [TIME_AUX], DL           ;update time
		
		
		CALL moves
		
		call draw
		
        RET

ENDP game_logic



start:
	
	
    MOV AX, @data
    MOV DS, AX
	cmp [savedscore] , 0
	jne alreadysaved
	
	mov ah,40h ; prints the msg
	mov bx,1
	mov cx, 23
	mov dx,OFFSET GetName
	int 21h 
	mov ah, 0Ah
	mov dx, offset PlayerName
	mov bx, dx
	mov [byte ptr bx], 4
	
	int 21h
    mov [savedscore],1
	
	
	
	
	
	
	alreadysaved:
	MOV AX, 13h
    INT 10h ; Set video mode 13h (320x200 pixels, 256 colors)
	
	
    
	
	
    game_loop:
	 CALL game_logic
        ; Check for key press to reset the game
        
        

        JMP game_loop
	
	 



	exit:	
   mov ax, 4c00h
int 21h

END start
