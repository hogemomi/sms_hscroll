; ------------------------------------;
;        Holizontal Scroll            ;
; ------------------------------------;

.sdsctag 0.1, "Hscroll", "Step 1 - Scroller", "hogel"

.memorymap           ; create 2 x 16 kb slots for rom.
    defaultslot 0
    slotsize $8000
    slot 0 $0000     ; rom bank 0 (0-16 kb).
    slot 1 $4000     ; rom bank 1 (16-32 kb)
    slotsize $2000
    slot 2 $c000     ; ram.
.endme

.rombankmap          ; map rom to 2 x 16 kb banks.
    bankstotal 2
    banksize $8000
    banks 2
.endro

.define  VDPcontrol $bf
.define  MapHeight $18
.define  MapWidth $200
.define  ScreenBottomVram $3e3e
.define  fractional_inc $0080

 ; Organize ram.

.enum $c000 export   ; export labels to symbol file.
    NextRawSrc dw
    NextRawVram dw
    NextColSrc dw
    NextColVram dw
    DrawLoopCount dw
    ScrollVal dw
    fixedPoint dw
    ScrollSpeed db   ; scroll speed
    Scroll db        ; vdp scroll register buffer
    Frame db         ; frame counter
    VDPstatus db
.ende

.bank 0 slot 0
.org 0
    di            ; disable interrupts.
    im 1          ; interrupt mode 1.
    ld sp,$dff0      ; default stack pointer address.
    jp inigam        ; initialize game.

; Read the vdp status flag at every frame interrupt.

.orga $0038          ; frame interrupt address
    ex af,af'        ; save accumulator in its shadow reg.
    in a,(VDPcontrol)   ; read status flags from VDP control
    ld (VDPstatus),a    ; save vdp status
    ex af,af'        ; restore accumulator
    reti                ; return from interrupt

; Disable the pause button - this is an unforgiving game!

.orga $0066          ; pause button interrupt.
    retn          ; disable pause button.

; Initialize game.
; Initialize the VDP registers.

inigam ld hl,regdat     ; point to register init data.
    ld b,11          ; 11 bytes of register data.
    ld c,$80         ; VDP register command byte.

-:
    ld a,(hl)        ; load one byte of data into A.
    out ($bf),a      ; output data to VDP command port.
    ld a,c        ; load the command byte.
    out ($bf),a      ; output it to the VDP command port.
    inc hl        ; inc. pointer to next byte of data.
    inc c         ; inc. command byte to next register.
    djnz -        ; jump back to '-' if b > 0.

;==============================================================
; Clear VRAM
;==============================================================
; 1. Set VRAM write address to $0000
    ld hl,$0000 | $4000
    call vrampr
; 2. Output 16KB of zeroes
    ld bc,$4000     ; Counter for 16KB of VRAM
-:  xor a
    out ($be),a ; Output to VRAM address, which is auto-incremented after each write
    dec bc
    ld a,b
    or c
    jr nz,-

; Setup the background assets for the main loop.
    ld hl,$c000   ; color bank 1, color 0
    call vrampr
    ld hl,bgpal
    ld bc,16   ; 16 colors
    call vramwr

    ld hl,$0000      ; first tile @ index 0.
    call vrampr
    ld hl,bgtile
    ld bc,192*32   ; each tile is 32 bytes.
    call vramwr

; Map placement at start
; Initial buffer
    ld hl,$3800
    ld (NextRawVram),hl
    ld hl,bgmap
    ld (NextRawSrc),hl

; loop count set
    ld bc,MapHeight
    ld (DrawLoopCount),bc

; ---------------
; start map configuration
draw_startmap:
    ld hl,(NextRawVram)
    call vrampr
; Wriite mapdata
    ld hl,(NextRawSrc)
    ld bc,$0040
    call vramwr

; Vram address update
    ld hl,(NextRawVram)
    ld de,$0040
    add hl,de
    ld (NextRawVram),hl

; Map source add update
    ld hl,(NextRawSrc)
    ld bc,MapWidth
    add hl,bc
    ld (NextRawSrc),hl

; loop count update
    ld bc,(DrawLoopCount)
    dec c
    ld (DrawLoopCount),bc
    jr nz,draw_startmap

; --------------------
; Initiarize buffer
    ld a,1
    ld (ScrollSpeed),a
    xor a         ; set A = 0
    ld (Frame),a
    ld (Scroll),a
    ld hl,$0000
    ld (fixedPoint),hl
    ld (ScrollVal),hl

    ; preset map columun address
    ld hl,bgmap
    ld bc,$0040 ;map width screenx2
    add hl,bc
    ld (NextColSrc),hl

    ; preset vram address
    ld hl,$3800
    ld (NextColVram),hl

    ld a,%11100000  ; turn screen on - normal sprites
    ld b,1
    call setreg  ; set register 1

; -----------------
; main loop
; -----------------
mainloop:
    ei
    halt   ; start main loop with vblank
    call wait_vblank

; Update vdp right when vblank begins
hscroll:
    ld a,(Scroll)
    ld b,$08
    call setreg

; -------------------
; fixed point mathmatic
    ld hl,(fixedPoint)
    ld de,fractional_inc
    add hl,de
; Update fixed point value
    ld (fixedPoint),hl

; Scroll value update
; or
; Scroll buffer update
    ld a,h
    cp $01
    jp nz,scrollbuf_update

; Scroll value update
    ld bc,(ScrollVal)
    add hl,bc
    ld (ScrollVal),hl

; Scroll Buffer update
scrollbuf_update:
    ld a,(Scroll)
    ld b,h
    sub b
    ld (Scroll),a
    ld a,h
    cp $01
    jp z,drawcoltiming
    jp mainloop

; -------------------
; Draw Column Timing check every 8px scroll
drawcoltiming:
    ld a,(Scroll)
    and %00000111
    call z,draw_column

; ----------------------
; Initialize fixed_point values
initialize_fixedpoint:
    ld hl,0
    ld (fixedPoint),hl

; -------------------
; Map end check
    ld hl,(ScrollVal)
    ld a,h
    cp $07
    jp nz,mainloop
    ld a,l
    cp $bf
    jp nz,mainloop
    jp stopscroll_loop

; ----------------------
; Stop Scroll
stopscroll_loop:
    xor a
    ld (ScrollSpeed),a
    ld a,$01
    ld (Scroll),a
    jp mainloop

; --------------------------------------------------------------
; SUBROUTINES
; --------------------------------------------------------------
; PREPARE VRAM.
; Set up vdp to recieve data at vram address in HL.

vrampr:
    push af
    ld a,l
    out ($bf),a
    ld a,h
    or $40
    out ($bf),a
    pop af
    ret

; --------------------------------------------------------------
; WRITE TO VRAM
; Write BC amount of bytes from data source pointed to by HL.
; Tip: Use vrampr before calling.

vramwr:
    ld a,(hl)
    out ($be),a
    inc hl
    dec bc
    ld a,c
    or b
    jp nz,vramwr
    ret

; --------------------------------------------------------------
; SET VDP REGISTER.
; Write to target register.
; A = byte to be loaded into vdp register.
; B = target register 0-10.

setreg:
    out ($bf),a      ; output command word 1/2.
    ld a,$80
    or b
    out ($bf),a      ; output command word 2/2.
    ret

; ----------------------
; Wait Vblank
wait_vblank:
    ld a,(VDPstatus)
    bit 7,a  ; check vblank bit
    jp z, wait_vblank
    res 7,a
    ld (VDPstatus),a
    ret

; ----------------------
draw_column:
; Loop counter
    ld a,MapHeight
    ld (DrawLoopCount),a

drawcolumn_loop:
; write to vram
    ld hl,(NextColVram)
    call vrampr
    ld hl,(NextColSrc)
    ld bc,2
    call vramwr

; Row Vram Source update
    ld hl,(NextColVram)
    ld bc,$0040
    add hl,bc
    ld (NextColVram),hl

    ld hl,(NextColSrc)
    ld bc,MapWidth
    add hl,bc
    ld (NextColSrc),hl

; loop counter
    ld a,(DrawLoopCount)
    dec a
    ld (DrawLoopCount),a
    jp nz,drawcolumn_loop

; Next column source add
    ld hl,(NextColSrc)
    ld bc,$2ffe
    or a
    sbc hl,bc
    ld (NextColSrc),hl

; Vram add reset
    ld hl,(NextColVram)
    ld a,h
    cp $3e
    jr nz,next_colvramadd
    ld a,l
    cp $3e
    jp nz,next_colvramadd

; Move top vram add
    ld hl,$3800
    ld (NextColVram),hl
    ret

; Next column vram add
    next_colvramadd:
    ld hl,(NextColVram)
    ld bc,$05fe
    or a
    sbc hl,bc
    ld (NextColVram),hl
    ret
; --------------------------------------------------------------
; DATA
; --------------------------------------------------------------
; Initial values for the 11 vdp registers.

regdat .db %00100110    ; reg. 0, display and interrupt mode.
                  ; bit 4 = line interrupt (disabled).
                  ; 5 = blank left column (disabled).
                  ; 6 = hori. scroll inhibit (disabled).
                  ; 7 = vert. scroll inhibit (disabled).

    .db %00101000    ; reg. 1, display and interrupt mode.
                  ; bit 0 = zoomed sprites (enabled).
                  ; 1 = 8 x 16 sprites (disabled).
                  ; 5 = frame interrupt (enabled).
                  ; 6 = display (blanked).

    .db $ff          ; reg. 2, name table address.
                  ; $ff = name table at $3800.

    .db $ff          ; reg. 3, n.a.
                  ; always set it to $ff.

    .db $ff          ; reg. 4, n.a.
                  ; always set it to $ff.

    .db $ff          ; reg. 5, sprite attribute table.
                  ; $ff = sprite attrib. table at $3F00.

    .db $ff          ; reg. 6, sprite tile address.
                  ; $ff = sprite tiles in bank 2.

    .db %11111111    ; reg. 7, border color.
                  ; set to color 3 in bank 2.

    .db $01          ; reg. 8, horizontal scroll value = 0.

    .db $00          ; reg. 9, vertical scroll value = 0.

    .db $ff          ; reg. 10, raster line interrupt.
                  ; turn off line int. requests.

; Background assets.

bgpal   .include "Assets_test\palette.inc"
bgtile  .include "Assets_test\tiles.inc"
bgmap   .include "Assets_test\tilemap3.inc"
