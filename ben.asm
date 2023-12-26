IDEAL
MODEL small
STACK 100h
DATASEG
; --------------------------

Y dw 100
X dw 100
Board dw 200
DrawFlag db 0  ; Flag to control drawing
; --------------------------
CODESEG
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
	mov [DrawFlag], 1
    jmp exit_moves

d_pressed:
    add [X], 5
	mov [DrawFlag], 1
    jmp exit_moves

a_pressed:
    sub [X], 5
	mov [DrawFlag], 1
    jmp exit_moves

s_pressed:
    add [Y], 5
	mov [DrawFlag], 1
    jmp exit_moves

endp moves

proc draw
	mov ax, 13h
    int 10h
	
    mov ax, 0C07h     ; Function 0Ch, Set Pixel Color
    mov bx, 0         ; Page number (usually 0 in mode 13h)
	
    mov cx, [X]       ; X-coordinate
    mov dx, [Y]       ; Y-coordinate
	mov si,0
	mov di,0
	mov al, 4; Color (4 is often red in mode 13h)
	draw_row_loop:
	
	draw_pixel_loop:
	inc cx	         
	inc di
	
	int 10h
	cmp di,4
	jne draw_pixel_loop
	sub cx, 4
	mov di, 0
	inc dx
	inc si
	
	int 10h
	cmp si,4
	jne draw_row_loop
	
	
    ret
endp draw
		

start:
    mov ax, @data
    mov ds, ax

   mov ax, 13h
    int 10h ; Set video mode 13h (320x200 pixels, 256 colors)
    

    game_loop:
        call moves
			
		call draw
		
        ; Add a small delay to control the game speed
        mov cx, 20000
        delay_loop:
            loop delay_loop

    ; Check for an exit condition (e.g., ESC key press)
    ; Add your exit condition here

    ; --------------------------

    jmp game_loop

exit:
    mov ax, 4c00h
    int 21h

END start
