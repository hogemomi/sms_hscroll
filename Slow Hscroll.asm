; -------------------------------------------------------------
;   Slow horizontal scroll                                       
; -------------------------------------------------------------

.sdsctag 0.1, "Slow Hscroll", "Step 1 - Scroller", "hogem

.memorymap	; create 2 x 16 kb slots for rom.
    defaultslot 
    slotsize $400
    slot 0 $0000        	; rom bank 0 (0-16 kb).
    slot 1 $4000        	; rom bank 1 (16-32 kb).
    slotsize $200
    slot 2 $c000        	; ram.
    .endm

.rombankmap               	; map rom to 2 x 16 kb banks.
    bankstotal 
    banksize $400

    banks 
.endr

.equ fractional_inc $002

.enum $c000 export        	; export labels to symbol file
    fixed_point dw      	; Q8.8 fixed point buffer
    vspeed d
    scroll db          	 ; vdp scroll register buffer.

.end

.bank 0 slot 
.org 
di                  ; disable interrupts
im 1                ; interrupt mode 1
ld sp,$dff0         ; default stack pointer address
jp inigam           ; initialize game

; Read the vdp status flag at every frame interrupt

.orga $0038                ; frame interrupt address
ex af,af'           ; save accumulator in its shadow reg
in a,$bf            ; satisfy interrupt
ex af,af'           ; restore accumulator
ei                  ; enable interrupts
ret                 ; return from interrupt

; Disable the pause button - this is an unforgiving game

.orga $0066                ; pause button interrupt
retn                ; disable pause button

; Initialize game
; Initialize the VDP registers

inigam ld hl,regdat        ; point to register init data
ld b,11             ; 11 bytes of register data
ld c,$80            ; VDP register command byte

-:     ld a,(hl)           ; load one byte of data into A
out ($bf),a         ; output data to VDP command port
ld a,c              ; load the command byte
out ($bf),a         ; output it to the VDP command port
inc hl              ; inc. pointer to next byte of data
inc c               ; inc. command byte to next register
djnz -              ; jump back to '-' if b > 0

;=============================================================
; Clear VRA
;=============================================================
; 1. Set VRAM write address to $000
ld hl,$0000 | $400
call vramp
; 2. Output 16KB of zeroe
ld bc,$4000     ; Counter for 16KB of VRA
-:  xor 
out ($be),a ; Output to VRAM address, which is auto-incremented after each writ
dec b
ld a,
or 
jr nz,

; Setup the background assets for the main loop

ld hl,$c000         ; color bank 1, color 0
call vrampr         ; prepare vram
ld hl,bgpal         ; background palette
ld bc,16             ; 4 colors
call vramwr         ; set background palette

ld hl,$0000         ; first tile @ index 0
call vrampr         ; prepare vram
ld hl,bgtile        ; background tile data (the road)
ld bc,192*32          ; 2 tiles (!), each tile is 32 bytes
call vramwr         ; write background tiles to vram

ld hl,$3800         	; point to name table.
call vrampr         	; prepare vram.
ld hl,bgmap         	; point to background tilemap data.
ld bc,32*28*2       	; 32 x 28 tiles, each is 2 bytes.
call vramwr        	; write name table to vram.

xor a               	; set A = 0.
ld (scroll),a       	; reset scroll register buffer.

ld a,%11100000      	; turn screen on - normal sprites.
ld b,
call setreg         	; set register 1.

ld hl,
ld (fixed_point),h
ld (vspeed),h
e

; This is the main loop

mloop halt                	; start main loop with vblank.

; Update vdp right when vblank begins
ld a,(scroll)        	; 1-byte scroll reg. buffer in ram
ld b,8               	; target VDP register 9 (v-scroll).
call setreg         	; now vdp register = buffer, and the
; screen scrolls accordingl
; fixed point mathmati
ld hl,(fixed_point) 	; load Q8.8 fixed minority
ld de,fractional_in
add hl,d
ld (vspeed),h

; Update fixed point valu
ld (fixed_point),h

; scroll background update the scroll buffe
ld a,(scroll
ld b,
sub 
ld (scroll),
ld a,
cp $0
jr z,initialize_fixedpoin

jr mloo

; Initialize fixed_point value
initialize_fixedpoint
ld hl,
ld (fixed_point),h
jr mloo
; -------------------------------------------------------------
; SUBROUTINE
; -------------------------------------------------------------
; PREPARE VRAM
; Set up vdp to recieve data at vram address in HL

vrampr push a
ld a,
out ($bf),
ld a,
or $4
out ($bf),
pop a
re

; -------------------------------------------------------------
; WRITE TO VRA
; Write BC amount of bytes from data source pointed to by HL
; Tip: Use vrampr before calling

vramwr ld a,(hl
out ($be),
inc h
dec b
ld a,
or 
jp nz,vramw
re

; -------------------------------------------------------------
; SET VDP REGISTER
; Write to target register
; A = byte to be loaded into vdp register
; B = target register 0-10

setreg out ($bf),a         ; output command word 1/2
ld a,$8
or 
out ($bf),a         ; output command word 2/2
re

; -------------------------------------------------------------
; DAT
; -------------------------------------------------------------
; Initial values for the 11 vdp registers

regdat .db %00100110       ; reg. 0, display and interrupt mode
; bit 4 = line interrupt (disabled)
; 5 = blank left column (disabled)
; 6 = hori. scroll inhibit (disabled)
; 7 = vert. scroll inhibit (disabled)

.db %00101000       ; reg. 1, display and interrupt mode
; bit 0 = zoomed sprites (enabled)
; 1 = 8 x 16 sprites (disabled)
; 5 = frame interrupt (enabled)
; 6 = display (blanked)

.db $ff             ; reg. 2, name table address
; $ff = name table at $3800

.db $ff             ; reg. 3, n.a
; always set it to $ff

.db $ff             ; reg. 4, n.a
; always set it to $ff

.db $ff             ; reg. 5, sprite attribute table
; $ff = sprite attrib. table at $3F00

.db $ff             ; reg. 6, sprite tile address
; $ff = sprite tiles in bank 2

.db %11111111       ; reg. 7, border color
; set to color 3 in bank 2

.db $01             ; reg. 8, horizontal scroll value = 0

.db $00             ; reg. 9, vertical scroll value = 0

.db $ff             ; reg. 10, raster line interrupt
; turn off line int. requests

; Background assets

bgpal  .include "assets\background (palette).inc
bgtile .include "assets\background (tiles).inc
bgmap  .include "assets\background (tilemap).inc"	
