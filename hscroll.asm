; -------------------------------------------------------------;
;             Holizontal Scroll                    ;
; -------------------------------------------------------------;

.sdsctag 0.1, "Hscroll", "Step 2 - Rewright mapdata", "hogel"

.memorymap           ; create 2 x 16 kb slots for rom.
    defaultslot 0
    slotsize $4000
    slot 0 $0000     ; rom bank 0 (0-16 kb).
    slot 1 $4000     ; rom bank 1 (16-32 kb).
    slotsize $2000
    slot 2 $c000     ; ram.
.endme

.rombankmap          ; map rom to 2 x 16 kb banks.
    bankstotal 2
    banksize $4000
    banks 2
.endro

.equ   scrollspeed 1         ; players' vertical speed.
       VdpControl $bf
       VdpData $be


 ; Organize ram.
.enum $c000 export      ; export labels to symbol file.
    NextRawSrc dw
    NextRawVram dw
    NextColSrc dw
    NextColVram dw
    LoopCount dw
    scroll db        ; vdp scroll register buffer.
    frame db         ; frame counter.
    WriteHalfflag db  ; vblank flag
.ende

.bank 0 slot 0
.org 0
    di            ; disable interrupts.
    im 1          ; interrupt mode 1.
    ld sp,$dff0      ; default stack pointer address.
    jp inigam        ; initialize game.

; Read the vdp status flag at every frame interrupt.

.orga $0038          ; frame interrupt address.
    ex af,af'        ; save accumulator in its shadow reg.
    in a,$bf         ; satisfy interrupt.
    ex af,af'        ; restore accumulator.
    ei            ; enable interrupts.
    ret           ; return from interrupt.

; Disable the pause button - this is an unforgiving game!

.orga $0066          ; pause button interrupt.
    retn          ; disable pause button.

; Initialize game.
; Initialize the VDP registers.

inigam ld hl,regdat     ; point to register init data.
    ld b,11          ; 11 bytes of register data.
    ld c,$80         ; VDP register command byte.

-:     ld a,(hl)        ; load one byte of data into A.
    out (VdpControl),a      ; output data to VDP command port.
    ld a,c        ; load the command byte.
    out (VdpControl),a      ; output it to the VDP command port.
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
    out (VdpData),a ; Output to VRAM address, which is auto-incremented after each write
    dec bc
    ld a,b
    or c
    jr nz,-

    ; Setup the background assets for the main loop.

    ld hl,$c000      ; color bank 1, color 0.
    call vrampr      ; prepare vram.
    ld hl,bgpal      ; background palette.
    ld bc,16          ; 4 colors.
    call vramwr      ; set background palette.
    
    ld hl,$0000      ; first tile @ index 0.
    call vrampr      ; prepare vram.
    ld hl,bgtile     ; background tile data (the road).
    ld bc,192*32       ; each tile is 32 bytes.
    call vramwr      ; write background tiles to vram.

; Map placement at start
; Initial buffer
    ld hl,$3800
    ld (NextRawVram),hl
    ld hl,bgmap
    ld (NextRawSrc),hl

; loop count set
    ld bc,24
    ld (LoopCount),bc

; start map configuration
draw_startmap:
    ld hl,(NextRawVram) ; Write Vram Addressing
    call vrampr

    ld hl,(NextRawSrc)  ; Wriite mapdata
    ld bc,64
    call vramwr

    ld de,$0040        ; Map data address update
    add hl,de
    ld (NextRawSrc),hl; Vram address update

    ld hl,(NextRawVram)
    ld de,$0040
    add hl,de
    ld (NextRawVram),hl

; loop count update
    ld bc,(LoopCount)
    dec c
    ld (LoopCount),bc
    jr nz,draw_startmap

; main loop preset
; Initiarize buffer
    xor a         ; set A = 0.
    ld (frame),a
    ld (scroll),a    ; reset scroll register buffer.

    ; preset map columun address
    ld hl,bgmap
    ld bc,65 ;column of 33
    add hl,bc
    ld (NextColSrc),hl

    ; preset vram address
    ld hl,$3800
    ld (NextColVram),hl

; draw map flag check intialize
    xor a
    ld (WriteHalfflag),a

    ld a,%11100000      ; turn screen on - normal sprites.
    ld b,1
    call setreg      ; set register 1.

; This is the main loop.

mloop
    ei
    halt          ; start main loop with vblank.

; Vblank flag check
    ld a,(VDPStatus)    
    bit 7,a             ; VBlankフラグを確認
    jr z, mloop          ; VBlankでなければループに戻る

    res 7,a             ; VBlankフラグをリセット
    ld (VDPStatus),a    

; Update vdp right when vblank begins!
    ld a,(scroll)    ; 1-byte scroll reg. buffer in ram.
    ld b,$08        ; target VDP register 9 (v-scroll).
    call setreg      ; now vdp register = buffer, and the
                  ; screen scrolls accordingly.

; Scroll background - update the scroll buffer.
    ld a,(scroll)    ; get scroll buffer value.
    sub vspeed       ; subtract vertical speed.
    ld (scroll),a    ; update scroll buffer.

; Conditional branching
    and %00000111
    jr nz, mloop
    
; Loop counter initialize
    ld a,12
    ld (LoopCount),a

; draw map flag check
    ld a, (WriteHalfflag)
    and a
    jr nz, draw_second_half

draw_first_half:
    ld a, 12
    ld (LoopCount), a
    jr drawcolumn

draw_second_half:
    ld a, 12
    ld (LoopCount), a

drawcolumn
    ld hl,(NextColVram)
    call vrampr
    ld hl,(NextColSrc)
    ld bc,2
    call vramwr

; Vram & Src update
    ld hl,(NextColVram)
    ld bc,$0040
    add hl,bc
    ld (NextColVram),hl

    ld hl,(NextColSrc)
    ld bc,$0080 ;Next column add
    sbc hl,bc
    ld (NextColSrc),hl

; map source update test
    ld hl,bgmap
    ld bc,64
    add hl,bc
    ld (NextColSrc),hl

    ld a, (write_half_flag)
    xor 1
    ld (write_half_flag), a

; loop count update
    ld a,(LoopCount)
    dec a
    ld (LoopCount),a
    jr nz,drawcolumn

    ld hl,(NextRawSrc)
    ld bc,$0b7e ;Next column add
    sbc hl,bc
    ld (NextColSrc),hl ;save column src buffer

    jp mloop

; --------------------------------------------------------------
; SUBROUTINES
; --------------------------------------------------------------
; PREPARE VRAM.
; Set up vdp to recieve data at vram address in HL.

vrampr push af
    ld a,l
    out (VdpCntrol),a
    ld a,h
    or $40
    out (VdpControl),a
    pop af
    ret

; --------------------------------------------------------------
; WRITE TO VRAM
; Write BC amount of bytes from data source pointed to by HL.
; Tip: Use vrampr before calling.

vramwr ld a,(hl)
    out (VdpData),a
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

setreg out (VdpControl),a      ; output command word 1/2.
    ld a,$80
    or b
    out (VdpControl),a      ; output command word 2/2.
    ret

WaitVBlank:
; Wait for vertical blanking phase.
    in a,$bf           ; get VDP status flags
    bit 7,a              ; get frame interrupt
    jp z,WaitVBlank      ; keep looping if FI is set
    res 7,a              ; reset bit 7 of VDP status flags
    ld (VDPStatus),a     ; update VDP status flags
    
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

bgpal   .include "Assets_colorbar\palette.inc"
bgtile  .include "Assets_colorbar\tiles.inc"
bgmap   .include "Assets_colorbar\tilemap3.inc"
