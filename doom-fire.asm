; STNICCC-Archie
; A port of STNICCC-2000 by Oxygene for the Acorn Archimedes series
; 

.equ _DEBUG, 1

.equ Screen_Banks, 1
.equ Screen_Mode, 9
.equ Screen_Width, 320
.equ Screen_Height, 256
.equ Window_Width, 256
.equ Window_Height, 200
.equ Screen_Stride, Screen_Width/2		; 4bpp
.equ Screen_Bytes, Screen_Stride*Screen_Height
.equ Window_Stride, Screen_Width/2		; 4bpp
.equ Window_Bytes, Window_Stride*Window_Height

.include "swis.h.asm"

.org 0x8000

Start:
    adr sp, stack_base
	B main

.skip 1024
stack_base:

scr_bank:
	.long 0

main:
	MOV r0,#22	;Set MODE
	SWI OS_WriteC
	MOV r0,#Screen_Mode
	SWI OS_WriteC

	; Set screen size for number of buffers
	MOV r0, #DynArea_Screen
	SWI OS_ReadDynamicArea
	MOV r0, #DynArea_Screen
	MOV r2, #Screen_Bytes * Screen_Banks
	SUBS r1, r2, r1
	SWI OS_ChangeDynamicArea
	MOV r0, #DynArea_Screen
	SWI OS_ReadDynamicArea
	CMP r1, #Screen_Bytes * Screen_Banks
	ADRCC r0, error_noscreenmem
	SWICC OS_GenerateError

	MOV r0,#23	;Disable cursor
	SWI OS_WriteC
	MOV r0,#1
	SWI OS_WriteC
	MOV r0,#0
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC

	; Set palette
	bl set_palette

	; Clear all screen buffers
	mov r1, #1
.1:
	str r1, scr_bank

	; CLS bank N
	mov r0, #OSByte_WriteVDUBank
	swi OS_Byte
	mov r0, #12
	SWI OS_WriteC

	ldr r1, scr_bank
	add r1, r1, #1
	cmp r1, #Screen_Banks
	ble .1

	; Start with bank 1
	mov r1, #1
	str r1, scr_bank
	
	; Claim the Error vector
	MOV r0, #ErrorV
	ADR r1, error_handler
	MOV r2, #0
	SWI OS_Claim

	; Claim the Event vector
	mov r0, #EventV
	adr r1, event_handler
	mov r2, #0
	swi OS_AddToVector

	; Late system init here
	bl get_screen_addr
	ldr r12, screen_addr

	mov r4, #15
	mov r1, #Screen_Height-1
	bl plot_horizontal_line

	; Enable Vsync event
	mov r0, #OSByte_EventEnable
	mov r1, #Event_VSync
	SWI OS_Byte

main_loop:   
	; debug
	bl debug_write_vsync_count

	; Block if we've not even had a vsync since last time - we're >50Hz!
.if Screen_Banks > 2
	ldr r1, last_vsync
.1:
	ldr r2, vsync_count
	cmp r1, r2
	beq .1
	str r2, last_vsync

	; Swap banks
	; Display whichever bank we've just written to
	ldr r1, scr_bank			; bank we want to display next
	str r1, buffer_pending		; we might overwrite a bank if too fast (drop a frame?)
	; If we have more than 3 banks then this needs to be a queue
	; This now happens in vsync event handler
.else
	mov r0, #OSByte_WriteDisplayBank
	swi OS_Byte
.endif

	; Increment to next bank for writing
	ldr r1, scr_bank
	add r1, r1, #1
	cmp r1, #Screen_Banks
	movgt r1, #1
	str r1, scr_bank

	; Now set the screen bank to write to
	mov r0, #OSByte_WriteVDUBank
	swi OS_Byte

	; Wait for vsync if double buffering
	.if Screen_Banks <= 2
	mov r0, #OSByte_Vsync
	swi OS_Byte
	.endif

	; Back buffer address for writing bank stored at screen_addr
	bl get_screen_addr
	ldr r12, screen_addr

	;Do stuff here!
	bl do_fire

	;Exit if SPACE is pressed
	MOV r0, #OSByte_ReadKey
	MOV r1, #IKey_Space
	MOV r2, #0xff
	SWI OS_Byte
	
	CMP r1, #0xff
	CMPEQ r2, #0xff
	BEQ exit
	
	B main_loop

wtaf_pad:
	.skip 0

error_noscreenmem:
	.long 0
	.byte "Cannot allocate screen memory!"
	.p2align 2
	.long 0

.if _DEBUG
debug_write_vsync_count:
	mov r0, #30
	swi OS_WriteC

	ldr r0, vsync_count
	adr r1, debug_string
	mov r2, #8
	swi OS_ConvertHex4

	adr r0, debug_string
	swi OS_WriteO

	mov pc, r14

debug_write_r0:
	adr r1, debug_string
	mov r2, #8
	swi OS_ConvertHex2
	adr r0, debug_string
	swi OS_WriteO
	mov r0, #32
	swi OS_WriteC
	mov pc, r14

debug_write_16:
	adr r1, debug_string
	mov r2, #8
	swi OS_ConvertHex4
	adr r0, debug_string
	swi OS_WriteO
	mov r0, #32
	swi OS_WriteC
	mov pc, r14

debug_write_32:
	adr r1, debug_string
	mov r2, #12
	swi OS_ConvertHex8
	adr r0, debug_string
	swi OS_WriteO
	mov r0, #32
	swi OS_WriteC
	mov pc, r14

debug_string:
	.skip 12
.endif

get_screen_addr:
	str lr, [sp, #-4]!
	adr r0, screen_addr_input
	adr r1, screen_addr
	swi OS_ReadVduVariables
	ldr pc, [sp], #4
	
screen_addr_input:
	.long VD_ScreenStart, -1
screen_addr:
	.long 0

exit:	
	; wait for vsync (any pending buffers)
	mov r0, #19
	swi OS_Byte

	; disable vsync event
	mov r0, #OSByte_EventDisable
	mov r1, #Event_VSync
	swi OS_Byte

	; release our event handler
	mov r0, #EventV
	adr r1, event_handler
	mov r2, #0
	swi OS_Release

	; release our error handler
	mov r0, #ErrorV
	adr r1, error_handler

	; Display whichever bank we've just written to
	mov r0, #OSByte_WriteDisplayBank
	ldr r1, scr_bank
	swi OS_Byte
	; and write to it
	mov r0, #OSByte_WriteVDUBank
	ldr r1, scr_bank
	swi OS_Byte

	; Show our final frame count
	bl debug_write_vsync_count

	SWI OS_Exit

; R0=event number
event_handler:
	cmp r0, #Event_VSync
	movnes pc, r14

	STMDB sp!, {r0-r1, lr}

	; update the vsync counter
	LDR r0, vsync_count
	ADD r0, r0, #1
	STR r0, vsync_count

.if Screen_Banks > 2
	; is there a new screen buffer ready to display?
	LDR r1, buffer_pending
	CMP r1, #0
	LDMEQIA sp!, {r0-r1, pc}

	; set the display buffer
	MOV r0, #0
	STR r0, buffer_pending
	MOV r0, #OSByte_WriteDisplayBank

	; some SVC stuff I don't understand :)
	STMDB sp!, {r2-r12}
	MOV r9, pc     ;Save old mode
	ORR r8, r9, #3 ;SVC mode
	TEQP r8, #0
	MOV r0,r0
	STR lr, [sp, #-4]!
	SWI XOS_Byte
	LDR lr, [sp], #4
	TEQP r9, #0    ;Restore old mode
	MOV r0, r0
	LDMIA sp!, {r2-r12}
.endif

	LDMIA sp!, {r0-r1, pc}

vsync_count:
	.long 0

last_vsync:
	.long -1

buffer_pending:
	.long 0

error_handler:
	STMDB sp!, {r0-r2, lr}
	MOV r0, #OSByte_EventDisable
	MOV r1, #Event_VSync
	SWI OS_Byte
	MOV r0, #EventV
	ADR r1, event_handler
	mov r2, #0
	SWI OS_Release
	MOV r0, #ErrorV
	ADR r1, error_handler
	MOV r2, #0
	SWI OS_Release
	MOV r0, #OSByte_WriteDisplayBank
	LDR r1, scr_bank
	SWI OS_Byte
	LDMIA sp!, {r0-r2, lr}
	MOVS pc, lr

set_palette:
	str lr, [sp, #-4]!
	mov r0, #12
	adr r1, palette_data
	add r2, r1, #16*5

	.1:
	swi OS_Word
	add r1, r1, #5
	cmp r1, r2
	blt .1

	ldr pc, [sp], #4

.macro COLOUR l, r, g, b
	.byte \l, 16, \r * 16, \g * 255, \b * 255 
.endm

.macro COLOUR_LERP l, r1, g1, b1, delta, r2, g2, b2
	.byte \l, 16
	.byte \r1 * 255 + (\r2-\r1) * \delta * 255
	.byte \g1 * 255 + (\g2-\g1) * \delta * 255
	.byte \b1 * 255 + (\b2-\b1) * \delta * 255
.endm

palette_data:
	; logical colour, physical colour, red, green, blue
	COLOUR 		0, 0, 0, 0					; black
	COLOUR_LERP 1, 0, 0, 0, 0.20, 1, 0, 0	; black->red
	COLOUR_LERP 2, 0, 0, 0, 0.40, 1, 0, 0	; black->red
	COLOUR_LERP 3, 0, 0, 0, 0.60, 1, 0, 0	; black->red
	COLOUR_LERP 4, 0, 0, 0, 0.80, 1, 0, 0	; black->red
	COLOUR_LERP 5, 0, 0, 0, 1.00, 1, 0, 0	; red

	COLOUR_LERP 6, 1, 0, 0, 0.20, 1, 1, 0	; red->yellow
	COLOUR_LERP 7, 1, 0, 0, 0.40, 1, 1, 0	; red->yellow
	COLOUR_LERP 8, 1, 0, 0, 0.60, 1, 1, 0	; red->yellow
	COLOUR_LERP 9, 1, 0, 0, 0.80, 1, 1, 0	; red->yellow
	COLOUR_LERP 10, 1, 0, 0, 1.00, 1, 1, 0	; yellow

	COLOUR_LERP 11, 1, 1, 0, 0.20, 1, 1, 1	; yellow->white
	COLOUR_LERP 12, 1, 1, 0, 0.40, 1, 1, 1	; yellow->white
	COLOUR_LERP 13, 1, 1, 0, 0.60, 1, 1, 1	; yellow->white
	COLOUR_LERP 14, 1, 1, 0, 0.80, 1, 1, 1	; yellow->white
	COLOUR_LERP 15, 1, 1, 0, 1.00, 1, 1, 1	; white
	.p2align 2

; R12=screen_addr, trashes r7, r8, r9
window_cls:
	ldr r8, screen_addr
	add r9, r8, #Window_Bytes

	mov r0, #0
	mov r1, #0
	mov r2, #0
	mov r3, #0
	mov r4, #0
	mov r5, #0
	mov r6, #0
	mov r7, #0
.1:
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	add r8, r8, #32
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	add r8, r8, #32
	cmp r8, r9
	blt .1

	mov pc, lr

; R0=x, R1=y, R4=colour, R12=screen_addr, trashes r10, r11
plot_pixel:
	; ptr = screen_addr + starty * screen_stride + startx DIV 2
	add r10, r12, r1, lsl #7	; r10 = screen_addr + starty * 128
	add r10, r10, r1, lsl #5	; r10 += starty * 32 = starty * 160
	add r10, r10, r0, lsr #1	; r10 += startx DIV 2

	ldrb r11, [r10]				; load screen byte

	tst r0, #1					; odd or even pixel?
	andeq r11, r11, #0xF0		; mask out left hand pixel
	orreq r11, r11, r4			; mask in colour as left hand pixel

	andne r11, r11, #0x0F		; mask out right hand pixel
	orrne r11, r11, r4, lsl #4	; mask in colour as right hand pixel

	strb r11, [r10]				; store screen byte
	mov pc, lr

; R0=x, R1=y, R12=screen_addr, trashes r10
; returns R4=colour
read_pixel:
	; ptr = screen_addr + starty * screen_stride + startx DIV 2
	add r10, r12, r1, lsl #7	; r10 = screen_addr + starty * 128
	add r10, r10, r1, lsl #5	; r10 += starty * 32 = starty * 160
	add r10, r10, r0, lsr #1	; r10 += startx DIV 2

	ldrb r4, [r10]				; load screen byte

	tst r0, #1					; odd or even pixel?
	andeq r4, r4, #0x0F			; mask out right hand pixel
	movne r4, r4, lsr #4		; mask out left hand pixel
	mov pc, lr

; R1=y, R4=colour
plot_horizontal_line:
	str lr, [sp, #-4]!
	mov r0, #0
	.1:
	bl plot_pixel
	add r0, r0, #1
	cmp r0, #Screen_Width
	blt .1
	ldr pc, [sp], #4


.macro RND
    TST    R9, R9, LSR #1                       ; top bit into Carry
    MOVS   R11, R8, RRX                         ; 33 bit rotate right
    ADC    R9, R9, R9                           ; carry into lsb of R1
    EOR    R11, R11, R8, LSL #12                ; (involved!)
    EOR    R8, R11, R11, LSR #20                ; (similarly involved!)
.endm

; DOOM FIRE!
; R0 = x
; R1 = y
; R2 = x
; R3 = y
; R4 = pixel colour
; R5 = screen byte
; R6 = ptr
; R7 = rnd temp
; R8 = seed
; R9 = bit
; R10 = source_ptr
; R11 = temp
; R12 = dest_ptr
do_fire:
	str lr, [sp, #-4]!

	ldr r8, rnd_seed			; seed
	mov r9, #1					; bit

	mov r3, #Screen_Height - 64

	; R10 = ptr to start of source line
	add r10, r12, r3, lsl #7	; r10 = screen_addr + y * 128
	add r10, r10, r3, lsl #5	; r10 += y * 32 = y * 160
	; R12 = ptr to start of dest line
	sub r12, r10, #Screen_Stride

	.1:
	mov r2, #0

	.2:
	; source is contiguous
	; could read a source word at a time
	; unroll 8x

	; spread fire

	; read source byte
	ldrb r5, [r10], #1			; load screen byte

	; Left pixel
	; inline rnd
	RND

	; read left pixel
	mov r4, r5, lsr #4			; mask out left hand pixel
	cmp r4, #0
	moveq r0, r2
	beq .3

	; update colour with some randomness
	and r7, r8, #1				; rnd & 1
	subs r4, r4, r7				; colour -= rnd & 1

	; randomise destination a bit
	and r7, r8, #3
	add r0, r2, r7
	subs r0, r0, #1				; dest_x += (rnd & 3)-1
	movlt r0, #0				; or MOD ScreenWidth

	.3:
	; plot dest pixel
	add r6, r12, r0, lsr #1		; r6 = dest_start + x DIV 2

	ldrb r11, [r6]				; load screen byte
	tst r0, #1					; odd or even pixel?
	andeq r11, r11, #0xF0		; mask out left hand pixel
	orreq r11, r11, r4			; mask in colour as left hand pixel
	andne r11, r11, #0x0F		; mask out right hand pixel
	orrne r11, r11, r4, lsl #4	; mask in colour as right hand pixel
	strb r11, [r6]				; store screen byte

	add r2, r2, #1

	; Right pixel
	; inline rnd
	RND

	; read right pixel
	and r4, r5, #0x0F
	cmp r4, #0
	moveq r0, r2
	beq .4

	; update colour with some randomness
	and r7, r8, #1				; rnd & 1
	subs r4, r4, r7				; colour -= rnd & 1

	; randomise destination a bit
	and r7, r8, #3
	add r0, r2, r7
	subs r0, r0, #1				; dest_x += (rnd & 3)-1
	movlt r0, #0				; or MOD ScreenWidth

	.4:
	; plot dest pixel
	add r6, r12, r0, lsr #1		; r6 = dest_start + x DIV 2

	ldrb r11, [r6]				; load screen byte
	tst r0, #1					; odd or even pixel?
	andeq r11, r11, #0xF0		; mask out left hand pixel
	orreq r11, r11, r4			; mask in colour as left hand pixel
	andne r11, r11, #0x0F		; mask out right hand pixel
	orrne r11, r11, r4, lsl #4	; mask in colour as right hand pixel
	strb r11, [r6]				; store screen byte

	add r2, r2, #1

	cmp r2, #Screen_Width
	blt .2

	; Next line
	add r12, r12, #Screen_Stride

	add r3, r3, #1
	cmp r3, #Screen_Height	
	blt .1

	str r8, rnd_seed

	ldr pc, [sp], #4

rnd_seed:
	.long 0x12345678

rnd:
; enter with seed in R0 (32 bits), R1 (1 bit in least significant bit)
; R2 is used as a temporary register.
; on exit the new seed is in R0 and R1 as before
; Note that a seed of 0 will always produce a new seed of 0.
; All other values produce a maximal length sequence.
;
; Moved to R11 as temp, R8 as seed and R9 and bit
	RND
	mov    pc, lr
