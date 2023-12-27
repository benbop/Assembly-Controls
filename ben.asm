IDEAL
MODEL small
STACK 100h
DATASEG
; --------------------------

Y dw 100
X dw 100
Board dw 200
TIME_AUX DB 0
CheckKeyPress DB, 0
; --------------------------
CODESEG
PROC CLEAR_SCREEN              ;clear the screen by restarting the video mode
	
			MOV AH,00h                   ;set the configuration to video mode
			MOV AL,13h                   ;choose the video mode
			INT 10h    					 ;execute the configuration 
		
			MOV AH,0Bh 					 ;set the configuration
			MOV BH,00h 					 ;to the background color
			MOV BL,00h 					 ;choose black as background color
			INT 10h    					 ;execute the configuration
			
			RET
			
	 ENDP CLEAR_SCREEN
	

proc moves
    mov ah, 01h       ; Function 01h - Check for Key Press
    int 16h
    jz exit_moves     ; Jump if ZF is set (no key pressed)

    mov ah, 00h       ; Function 00h - Read Key Stroke
    int 16h

    ; Check if AH contains 'W' (ASCII value 87)
    cmp al, 'w'
    je w_pressed
    ; Check if AH contains 'S' (ASCII value 83)
    cmp al, 's'
    je s_pressed
    ; Check if AH contains 'D' (ASCII value 68)
    cmp al, 'd'
    je d_pressed
    ; Check if AH contains 'A' (ASCII value 65)
    cmp al, 'a'
	
    je a_pressed
    ; Check if AH contains '1B' (Escape key)
    cmp al, 27h
    je exit_moves
    
exit_moves:
    ret

w_pressed:
	
    sub [Y], 5
	jmp exit_moves
    

d_pressed:
    add [X], 5
	jmp exit_moves

a_pressed:
    sub [X], 5
    jmp exit_moves

s_pressed:
    add [Y], 5
    jmp exit_moves

endp moves

proc draw_player
	mov ax, 13h
    int 10h
	
    mov ax, 0C07h     ; Function 0Ch, Set Pixel Color
    mov bh, 0         ; Page number (usually 0 in mode 13h)
	
    mov cx, [X]       ; X-coordinate
    mov dx, [Y]       ; Y-coordinate
	mov si,0
	mov di,0
	mov al, 2; Color green
	draw_row_loop:
	
	draw_pixel_loop:
	inc cx	         
	inc di
	
	int 10h
	cmp di,16
	jne draw_pixel_loop
	sub cx, 16
	mov di, 0
	inc dx
	inc si
	
	int 10h
	cmp si,2
	jne draw_row_loop
	
	
    ret
endp draw_player
		
proc draw_ground
mov ax, 13h
    int 10h
	
    mov ax, 0C07h     ; Function 0Ch, Set Pixel Color
    mov bh, 0         ; Page number (usually 0 in mode 13h)
	
    mov cx, 0       ; X-coordinate
    mov dx, 10       ; Y-coordinate
	mov si,0
	mov di,0
	mov al, 15; Color white
	
	loopy:
	inc  cx
	int 10h
	cmp cx , 320
	jne loopy
	
	RET
endp draw_ground
mov ax, 13h
int 10h

start:
    mov ax, @data
    mov ds, ax

   mov ax, 13h
    int 10h ; Set video mode 13h (320x200 pixels, 256 colors)
    

    game_loop:
		CHECK_TIME:                      ;time checking loop
			MOV AH,2Ch 					 ;get the system time
			INT 21h 			 ;is the current time equal to the previous one(TIME_AUX)?
			CMP DL,[TIME_AUX] 			 ;is the current time equal to the previous one(TIME_AUX)?
			call moves
			
			JE CHECK_TIME   		     ;if it is the same, check again
			
;           If it reaches this point, it's because the time has passed
  
			MOV [TIME_AUX],DL              ;update time
			
			CALL CLEAR_SCREEN
			
		call draw_player
		
    jmp game_loop

exit:
    mov ax, 4c00h
    int 21h

END start
