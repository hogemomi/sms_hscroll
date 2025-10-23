; ------------------------------------;
;        holizontal scroll            ;
; ------------------------------------;

.sdsctag 0.1, "hscroll", "step 1 - scroller", "hogel"

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

.define  vdpcontrol $bf
.define  mapheight $18
.define  mapwidth $200
.define  screenbottomvram $3e3e
.define  fractional_inc $0040

 ; organize ram.

.enum $c000 export      ; export labels to symbol file.
    nextrawsrc dw
    nextrawvram dw
    nextcolsrc dw
    nextcolvram dw
    drawloopcount dw
    scrollcount dw
    fixedpoint dw
    scrollval dw
    screencount db
    scrollspeed db
    scroll db        ; vdp scroll register buffer
    frame db         ; frame counter
    vdpstatus db
.ende

.bank 0 slot 0
.org 0
    di            ; disable interrupts.
    im 1          ; interrupt mode 1.
    ld sp,$dff0      ; default stack pointer address.
    jp inigam        ; initialize game.

; read the vdp status flag at every frame interrupt.

.orga $0038          ; frame interrupt address
    ex af,af'        ; save accumulator in its shadow reg.
    in a,(vdpcontrol)   ; read status flags from vdp control
    ld (vdpstatus),a    ; save vdp status
    ex af,af'        ; restore accumulator
    reti                ; return from interrupt

; disable the pause button - this is an unforgiving game!

.orga $0066          ; pause button interrupt.
    retn          ; disable pause button.

; initialize game.
; initialize the vdp registers.

inigam ld hl,regdat     ; point to register init data.
    ld b,11          ; 11 bytes of register data.
    ld c,$80         ; vdp register command byte.

-:
    ld a,(hl)        ; load one byte of data into a.
    out ($bf),a      ; output data to vdp command port.
    ld a,c        ; load the command byte.
    out ($bf),a      ; output it to the vdp command port.
    inc hl        ; inc. pointer to next byte of data.
    inc c         ; inc. command byte to next register.
    djnz -        ; jump back to '-' if b > 0.

;==============================================================
; clear vram
;==============================================================
; 1. set vram write address to $0000
    ld hl,$0000 | $4000
    call vrampr
; 2. output 16kb of zeroes
    ld bc,$4000     ; counter for 16kb of vram
-:  xor a
    out ($be),a ; output to vram address, which is auto-incremented after each write
    dec bc
    ld a,b
    or c
    jr nz,-

; setup the background assets for the main loop.
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

; map placement at start
; initial buffer
    ld hl,$3800
    ld (nextrawvram),hl
    ld hl,bgmap
    ld (nextrawsrc),hl

; loop count set
    ld bc,mapheight
    ld (drawloopcount),bc

; ---------------
; start map configuration
draw_startmap:
    ld hl,(nextrawvram)
    call vrampr
; wriite mapdata
    ld hl,(nextrawsrc)
    ld bc,$0040
    call vramwr

; vram address update
    ld hl,(nextrawvram)
    ld de,$0040
    add hl,de
    ld (nextrawvram),hl

; map source add update
    ld hl,(nextrawsrc)
    ld bc,mapwidth
    add hl,bc
    ld (nextrawsrc),hl

; loop count update
    ld bc,(drawloopcount)
    dec c
    ld (drawloopcount),bc
    jr nz,draw_startmap

; --------------------
; initiarize buffer
    ld a,1
    ld (scrollspeed),a
    ld (scrollval),a
    xor a         ; set a = 0
    ld (frame),a
    ld (scroll),a
    ld (scrollcount),a
    ld (screencount),a
    ld hl,$0001
    ld (fixedpoint),hl

    ; preset map columun address
    ld hl,bgmap
    ld bc,$0040 ;map width screenx2
    add hl,bc
    ld (nextcolsrc),hl

    ; preset vram address
    ld hl,$3800
    ld (nextcolvram),hl

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

; ----------------------
; update vdp right when vblank begins!
    ld a,(scroll)
    ld b,$08
    call setreg

; -------------------
; draw column timing check every 8px scroll
drawcoltiming:
    ld a,(scrollval)
    and %00000111
    call z,draw_column

; -------------------
; fixed point mathmatic
fixedpointath:
    ld hl,(fixedpoint) 
    ld de,fractional_inc
    add hl,de
; update fixed point value
    ld (fixedpoint),hl
    ld a,h
    cp $01
    jr nz,scrollupdate

; scroll value update
    ld bc,(scrollval)
    inc bc
    ld (scrollval),bc

; scroll background update the scroll buffer
scrollupdate:
    ld a,(scroll)
    ld b,h
    sub b
    ld (scroll),a
    ld a,h
    cp $01
    jp z,drawcoltiming
    jp mainloop

; ----------------------
; initialize fixed_point values
initialize_fixedpoint:
    ld hl,0
    ld (fixedpoint),hl
    jp mainloop

; --------------------------------------------------------------
; subroutines
; --------------------------------------------------------------
; prepare vram.
; set up vdp to recieve data at vram address in hl.

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
; write to vram
; write bc amount of bytes from data source pointed to by hl.
; tip: use vrampr before calling.

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
; set vdp register.
; write to target register.
; a = byte to be loaded into vdp register.
; b = target register 0-10.

setreg:
    out ($bf),a      ; output command word 1/2.
    ld a,$80
    or b
    out ($bf),a      ; output command word 2/2.
    ret

; ----------------------
; wait vblank
wait_vblank:
    ld a,(vdpstatus)
    bit 7,a  ; check vblank bit
    jp z, wait_vblank
    res 7,a
    ld (vdpstatus),a
    ret

; ----------------------
draw_column:
; loop counter
    ld a,mapheight
    ld (drawloopcount),a

drawcolumn_loop:
; write to vram
    ld hl,(nextcolvram)
    call vrampr
    ld hl,(nextcolsrc)
    ld bc,2
    call vramwr

; row vram source update
    ld hl,(nextcolvram)
    ld bc,$0040
    add hl,bc
    ld (nextcolvram),hl

    ld hl,(nextcolsrc)
    ld bc,mapwidth
    add hl,bc
    ld (nextcolsrc),hl

; loop counter
    ld a,(drawloopcount)
    dec a
    ld (drawloopcount),a
    jp nz,drawcolumn_loop

; next column source add
    ld hl,(nextcolsrc)
    ld bc,$2ffe
    or a
    sbc hl,bc
    ld (nextcolsrc),hl

; vram add reset
    ld hl,(nextcolvram)
    ld a,h
    cp $3e
    jr nz,next_colvramadd
    ld a,l
    cp $3e
    jp nz,next_colvramadd

; move top vram add
    ld hl,$3800
    ld (nextcolvram),hl
    ret

; next column vram add
    next_colvramadd:
    ld hl,(nextcolvram)
    ld bc,$05fe
    or a
    sbc hl,bc
    ld (nextcolvram),hl
    ret
; --------------------------------------------------------------
; data
; --------------------------------------------------------------
; initial values for the 11 vdp registers.

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
                  ; $ff = sprite attrib. table at $3f00.

    .db $ff          ; reg. 6, sprite tile address.
                  ; $ff = sprite tiles in bank 2.

    .db %11111111    ; reg. 7, border color.
                  ; set to color 3 in bank 2.

    .db $01          ; reg. 8, horizontal scroll value = 0.

    .db $00          ; reg. 9, vertical scroll value = 0.

    .db $ff          ; reg. 10, raster line interrupt.
                  ; turn off line int. requests.

; background assets.

bgpal   .include "assets_test\palette.inc"
bgtile  .include "assets_test\tiles.inc"
bgmap   .include "assets_test\tilemap3.inc"
