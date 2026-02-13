ram_E9          = $e9 ; alias for starfieldScrollY for compatibility
; Disassembly of ~\Projects\Programming\reversing\6502\shuttle\orig\Space Shuttle.bin
; Disassembled 02/12/26 16:14:53
; Using Stella 7.0
;
; ROM properties name : Space Shuttle (1983) (Activision)
; ROM properties MD5  : 5894c9c0c1e7e29f3ab86c6d3f673361
; Bankswitch type     : F8* (8K) 
;
; Legend: *  = CODE not yet run (tentative code)
;         D  = DATA directive (referenced in some way)
;         G  = GFX directive, shown as '#' (stored in player, missile, ball)
;         P  = PGFX directive, shown as '*' (stored in playfield)
;         C  = COL directive, shown as color constants (stored in player color)
;         CP = PCOL directive, shown as color constants (stored in playfield color)
;         CB = BCOL directive, shown as color constants (stored in background color)
;         A  = AUD directive (stored in audio registers)
;         i  = indexed accessed only
;         c  = used by code executed in RAM
;         s  = used by stack
;         !  = page crossed, 1 cycle penalty

    processor 6502


;-----------------------------------------------------------
;      Color constants
;-----------------------------------------------------------

BLACK            = $00
YELLOW           = $10
BROWN            = $20
ORANGE           = $30
RED              = $40
MAUVE            = $50
VIOLET           = $60
PURPLE           = $70
BLUE             = $80
BLUE_CYAN        = $90
CYAN             = $a0
CYAN_GREEN       = $b0
GREEN            = $c0
GREEN_YELLOW     = $d0
GREEN_BEIGE      = $e0
BEIGE            = $f0


;-----------------------------------------------------------
;      Bankswitching Constants
;-----------------------------------------------------------

bank0Strobe     = $FFF8
bank1Strobe     = $FFF9

NULL			 = $0000

;-----------------------------------------------------------
;      TIA Registers
;-----------------------------------------------------------
;CXM1P          = $01  ; (Ri)
;CXP0FB         = $02  ; (Ri)
;CXP1FB         = $03  ; (Ri)
CXM0FB          = $04  ; (R)
CXM1FB          = $05  ; (R)
;CXBLPF         = $06  ; (Ri)
;CXPPMM         = $07  ; (Ri)
;INPT0          = $08  ; (Ri)
;INPT1          = $09  ; (Ri)
;INPT2          = $0a  ; (Ri)
;INPT3          = $0b  ; (Ri)
INPT4           = $0c  ; (R)
;INPT5          = $0d  ; (Ri)
;$1e            = $0e  ; (Ri)
;$1f            = $0f  ; (Ri)

VSYNC           = $00  ; (W)
VBLANK          = $01  ; (W)
WSYNC           = $02  ; (W)
NUSIZ0          = $04  ; (W)
NUSIZ1          = $05  ; (W)
COLUP0          = $06  ; (W)
COLUP1          = $07  ; (W)
COLUPF          = $08  ; (W)
COLUBK          = $09  ; (W)
CTRLPF          = $0a  ; (W)
REFP0           = $0b  ; (W)
REFP1           = $0c  ; (W)
PF0             = $0d  ; (W)
PF1             = $0e  ; (W)
PF2             = $0f  ; (W)
RESP0           = $10  ; (W)
RESP1           = $11  ; (W)
RESM0           = $12  ; (W)
RESM1           = $13  ; (W)
RESBL           = $14  ; (W)
AUDC0           = $15  ; (W)
AUDC1           = $16  ; (W)
AUDF0           = $17  ; (W)
AUDF1           = $18  ; (W)
AUDV0           = $19  ; (W)
AUDV1           = $1a  ; (W)
GRP0            = $1b  ; (W)
GRP1            = $1c  ; (W)
ENAM0           = $1d  ; (W)
ENAM1           = $1e  ; (W)
ENABL           = $1f  ; (W)
HMP0            = $20  ; (W)
HMP1            = $21  ; (W)
HMM0            = $22  ; (W)
HMM1            = $23  ; (W)
HMBL            = $24  ; (W)
VDELP0          = $25  ; (W)
VDELP1          = $26  ; (W)
HMOVE           = $2a  ; (W)
HMCLR           = $2b  ; (W)
CXCLR           = $2c  ; (W)

SWCHA           = $0280
SWCHB           = $0282
INTIM           = $0284
TIM8T           = $0295
TIM64T          = $0296


;-----------------------------------------------------------
;      ZERO Page Labels
;-----------------------------------------------------------

rngSeed         = $80
crosshairX          = $81
targetX          = $82
targetAuxX          = $83
currentScreenId          = $84
ram_85          = $85
ram_86          = $86
ram_87          = $87
ram_88          = $88
ram_89          = $89
ram_8A          = $8a
;                 $8b  (i)
;                 $8c  (i)
;                 $8d  (i)
;                 $8e  (i)
;                 $8f  (i)
;                 $90  (i)
;                 $91  (i)
;                 $92  (i)
;                 $93  (i)
;                 $94  (i)
ram_95          = $95
ram_96          = $96
ram_97          = $97
ram_98          = $98
ram_99          = $99
ram_9A          = $9a
ram_9B          = $9b
ram_9C          = $9c
ram_9D          = $9d
ram_9E          = $9e
ram_9F          = $9f
ram_A0          = $a0
ram_A1          = $a1
ram_A2          = $a2
ram_A3          = $a3
ram_A4          = $a4
ram_A5          = $a5
ram_A6          = $a6
ram_A7          = $a7
ram_A8          = $a8
ram_A9          = $a9
ram_AA          = $aa
ram_AB          = $ab
ram_AC          = $ac
ram_AD          = $ad
ram_AE          = $ae
ram_AF          = $af
ram_B0          = $b0
ram_B1          = $b1
ram_B2          = $b2
ram_B3          = $b3
ram_B4          = $b4
ram_B5          = $b5
ram_B6          = $b6
ram_B7          = $b7
ram_B8          = $b8
ram_B9          = $b9
ram_BA          = $ba
ram_BB          = $bb
ram_BC          = $bc

ram_BE          = $be

ram_C0          = $c0
ram_C1          = $c1
ram_C2          = $c2
ram_C3          = $c3
ram_C4          = $c4
ram_C5          = $c5
ram_C6          = $c6
ram_C7          = $c7
ram_C8          = $c8
ram_C9          = $c9
ram_CA          = $ca
ram_CB          = $cb
screenPtr1L          = $cc
screenPtr1H          = $cd
screenPtr2L          = $ce
screenPtr2H          = $cf
screenPtr3L          = $d0
screenPtr3H          = $d1
screenPtr4L          = $d2
screenPtr4H          = $d3
screenPtr5L          = $d4
screenPtr5H          = $d5
screenPtr6L          = $d6
screenPtr6H          = $d7
ram_D8          = $d8
;                 $d9  (i)
;                 $da  (i)
;                 $db  (i)
ram_DC          = $dc
ram_DD          = $dd
ram_DE          = $de
ram_DF          = $df
ram_E0          = $e0
ram_E1          = $e1
ram_E2          = $e2
ram_E3          = $e3
starfieldHorizontalMotion          = $e4

ram_E6          = $e6; (s)
ram_E7          = $e7
ram_E8          = $e8
starfieldScrollY          = $e9
ram_EA          = $ea
ram_EB          = $eb
ram_EC          = $ec
ram_ED          = $ed
ram_EE          = $ee
ram_EF          = $ef
ram_F0          = $f0
ram_F1          = $f1
ram_F2          = $f2
ram_F3          = $f3
ram_F4          = $f4
ram_F5          = $f5
ram_F6          = $f6
ram_F7          = $f7
ram_F8          = $f8
ram_F9          = $f9
ram_FA          = $fa
ram_FB          = $fb
ram_FC          = $fc
starfieldVerticalCounter          = $fd
;                 $fe  (s)
;                 $ff  (s)


;-----------------------------------------------------------
;      Non Locatable Labels
;-----------------------------------------------------------

Lf129           = $f129


;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

Start           = $f003


;***********************************************************
;      Bank 0 / 0..1
;***********************************************************

    SEG     CODE
    ORG     $0000
    RORG    $d000

resetBank0
    bit     bank1Strobe                     ; $d000 *)
    
;-----------------------------------------------------------
;      Code
;-----------------------------------------------------------
startBank0Kernel
    stx     ENAM0
    stx     ENAM1
    stx     COLUPF
    dex
    stx     PF0
    sta     RESBL
    stx     PF1
    stx     PF2
    lda     #BLACK|$6
    sta     COLUBK
    lda     #$c0
    sta     HMBL
    sta     ENABL
    txs
    inx
    stx     GRP0
    lda     #BLUE|$2
    sta     COLUP1
    lda     #BLUE|$a
    sta     COLUP0
    stx     NUSIZ0
    stx     REFP0
    lda     #$05
    sta     NUSIZ1
    stx     PF2
    sta     HMOVE
    stx     PF0
    stx     PF1
    lda     targetX
    sta     HMCLR
    jsr     $d958
    lda     targetAuxX
    inx
    jsr     $d958
    lda     #YELLOW|$8
    sta     COLUPF
    ldx     #$07
;-----------------------------------------------------------
;      Wait for VBlank Timer / Start Kernel (Bank 0)
;-----------------------------------------------------------
waitForVBlankTimer
    ldy     #$00
    lda     $d9e8,x
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sty     PF0
    sta     CTRLPF
    lda     #$7f
    sta     ENABL
    sta     PF1
    dey
    sty     GRP1
    lda     $dbfa,x
    sta     GRP0
    sty     PF2
    sta     HMCLR
    lda     #$c0
    sta     PF0
    sty     PF1
    dex
    bpl     waitForVBlankTimer
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    iny
    sty     PF0
    sty     PF1
    sty     PF2
    sty     ENABL
    sty     GRP0
    sty     GRP1
    nop
    stx     RESBL
    ldx     ram_E3
    cpx     #$04
    bcc     Ld0a3
    lda     ram_B5
    cmp     #$02
    bne     Ld0a3
    lda     ram_C1
    and     $debd,x
    bne     Ld0a3
    bit     rngSeed
    bmi     Ld0a1
    dec     ram_B2
    .byte   $2c ;bit                ;4-5 =  60 *
Ld0a1
    inc     ram_B2
Ld0a3
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sty     COLUP0
    sty     COLUP1
    sty     COLUPF
    lda     #$01
    sta     NUSIZ0
    sta     NUSIZ1
    sta     CTRLPF
    ldx     #$05
    lda     #$02
    sta     HMCLR
    sta     ENABL
    sta     RESP1
Ld0bf
    sta     WSYNC
;---------------------------------------
    lda     #$30
    sta     PF0
    lda     $dafa,x
    sta     GRP0
    sta     GRP1
    nop
    nop
    nop
    sta     RESP0
    sta.w   RESP1
    sta.w   RESP0
    sta.w   RESP1
    sta.w   RESP0
    sta.w   RESP1
    sta.w   RESP0
    sta.w   RESP1
    sta.w   RESP0
    sta.w   RESP1
    sta.w   RESP0
    sty     PF0
    dex
    bne     Ld0bf
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    stx     GRP0
    stx     GRP1
    stx     ENABL
    stx     NUSIZ0
    stx     NUSIZ1
    inx
    lda     #BLUE|$9
    sta     RESP0
    stx     CTRLPF
    sta     COLUPF
    lda     #YELLOW|$8
    sta     COLUP0
    ldx     #BLACK|$c
    lda     ram_A1
    beq     Ld11d
    lda     ram_C1
    and     #$20
    beq     Ld11d
    ldx     #$82
Ld11d
    stx     COLUP1
    ldx     #$08
    lda     crosshairX
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sec
Ld128
    sbc     #$0f
    bcs     Ld128
    eor     #$0f
    asl
    asl
    asl
    asl
    adc     #$80
    sta     COLUBK,x
    sta     HMP1
kernelDrawCockpitWindows
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     $d9ef,x
    sta     GRP0
    lda     $dfed,x
    sta     GRP1
    sty     PF0
    lda     #$7f
    sta     PF1
    dey
    sty     PF2
    sta     HMCLR
    lda     #$c0
    sta     PF0
    sty     PF1
    iny
    dex
    bne     kernelDrawCockpitWindows
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sty     GRP1
    sty     GRP0
    sty     GRP1
    sty     PF0
    sty     PF1
    sty     PF2
    lda     ram_B1
    sec
    sbc     ram_B0
    cmp     #$80
    bne     Ld180
    ldx     ram_E2
    cpx     #$ff
    bne     Ld180
    inc     ram_E2
    lda     rngSeed
    sta     ram_B2
Ld180
    ldx     #BLACK|$4
Ld182
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    dex
    bne     Ld182
    stx     COLUBK
    stx     REFP0
    stx     REFP1
    inx
    stx     VDELP0
    stx     VDELP1
    ldx     #$03
    stx     NUSIZ0
    stx.w   NUSIZ1
    ldy     #$a0
    sta     RESP0
    ldx     #$10
    sta     RESP1
    sty     HMP0
    stx     HMP1
    ldy     #BROWN|$6
    sta     RESM0
    sty     HMM0
    ldx     #BLUE_CYAN|$2
    lda     ram_9B
    cmp     #$10
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sty     COLUBK
    lda     #$fe
    sta     PF2
    bcs     Ld1cd
    lda     ram_C1
    and     #$10
    beq     Ld1cd
    ldx     #$86
    stx     COLUPF
    ldy     #$80
    bne     Ld1e1
Ld1cd
    stx     COLUPF
    ldy     #BLACK|$a
    lda     ram_85
    cmp     #$19
    bcc     Ld1df
    beq     Ld1e1
    lda     ram_C1
    and     #$10
    beq     Ld1e1
Ld1df
    ldy     #$58
Ld1e1
    sty     COLUP0
    sty     COLUP1
    sta     HMCLR
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     #$06
    sta     ram_FA
    jsr     $d979
    ldy     #$00
    sty     GRP0
    sty     GRP1
    sta     HMOVE
    sty     GRP0
    sty     ram_F4
    sty     ram_F5
    sty     NUSIZ0
    lda     ram_F1
    bne     Ld217
    lda     ram_85
    cmp     #$17
    beq     Ld214
    cmp     #$05
    bcs     Ld217
Ld214
    asl
    sta     ENAM0
Ld217
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    ldx     #$03
    stx     NUSIZ0
    inx
    stx     ENAM0
    lda     ram_AD
    clc
    adc     #$38
    jsr     $d958
    sty     COLUPF
    ldx     #CYAN_GREEN|$0
    stx     COLUP0
    stx     COLUP1
    ldy     #$18
    lda     #$d0
    sta     HMP0
    sta     HMP1
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     #BLACK|$b
    sta     COLUPF
    lda     #$17
    sta     CTRLPF
    lda     #$d9
    sta     screenPtr1H
    lda     currentScreenId
    and     #$01
    beq     Ld252
    lda     #$7d
Ld252
    ldx     #$08
    clc
    sta     HMCLR
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
Ld25b
    sta     screenPtr2L,x
    adc     #$19
    dex
    dex
    bpl     Ld25b
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     currentScreenId
    lsr
    clc
    adc     #$da
    sta     screenPtr6H
    sta     screenPtr5H
    sta     screenPtr4H
    sta     screenPtr3H
    sta     screenPtr2H
    lda     ram_C1
    ldx     currentScreenId
    bit     ram_E1
    bpl     Ld285
    and     #$fc
    beq     Ld28a
    bne     Ld290
Ld285
    and     $d9f8,x
    beq     Ld290
Ld28a
    lda     #$e5
    sec
    sbc     ram_AC
    .byte   $2c ;bit                ;4-2 =   9 *
Ld290
    lda     #$cc
    sta     screenPtr1L
    lda     #$06
    sta     PF2
kernelDrawMainView
    lda     (screenPtr1L),y
    sta     ENABL
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sty     ram_FB
    lda     (screenPtr6L),y
    sta     GRP0
    lda     (screenPtr5L),y
    sta     GRP1
    lda     (screenPtr4L),y
    sta     GRP0
    lda     (screenPtr3L),y
    tax
    lda     (screenPtr2L),y
    ldy     #$00
    stx     GRP1
    sta     GRP0
    sty     GRP1
    sty     GRP0
    ldy     ram_FB
    dey
    bpl     kernelDrawMainView
    iny
    sty     GRP1
    sty     ENABL
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    nop
    lda     #$fe
    sta     PF2
    lda     #BLACK|$c
    sta     COLUP0
    sta     COLUP1
    ldy     #$07
    nop
    ldx     #$40
    stx     HMP0
    stx     HMP1
    lda     ram_C8
    and     #$1f
    cmp     #$14
    stx     HMBL
    sta     RESBL
    bcs     Ld2f4
    ldy     #$00
    cmp     #$0c
    bcc     Ld2f4
    sbc     #$0c
    tay
Ld2f4
    sty     ram_FA
    tya
    eor     #$07
    sta     ram_FB
    lda     #$e8
    ldx     #$08
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sec
Ld304
    sta     screenPtr2L,x
    sbc     #$08
    sta     screenPtr1L,x
    sbc     #$08
    dex
    dex
    dex
    dex
    bpl     Ld304
    sta     HMCLR
    sta     HMOVE
    ldx     #$00
    stx     PF2
    stx     COLUPF
    lda     #BLACK|$8
    sta     COLUBK
    lda     #$d8
    sta     screenPtr1H
    sta     screenPtr2H
    sta     screenPtr3H
    sta     screenPtr4H
    sta     screenPtr5H
    sta     screenPtr6H
    sta     WSYNC
;---------------------------------------
    stx     COLUBK
    jsr     $d979
    lda     #$c0
    sta     HMP0
    sta     HMP1
    sta     HMOVE
    ldx     #$00
    stx     GRP0
    stx     GRP1
    stx     GRP0
    dex
    stx     PF0
    stx     PF1
    ldx     #$07
    stx     PF2
    lda     #$31
    sta     CTRLPF
    lda     #$01
    sta     NUSIZ1
    sta     HMCLR
    lda     #$10
    sta     HMBL
    stx     ENABL
kernelDrawStatusBar
    lda     $d8bb,x
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    tay
    nop
    lda     $d89b,x
    sta     GRP0
    lda     $d8f0,x
    sta     COLUPF
    lda     $d8a3,x
    sta     GRP1
    lda     $d8ab,x
    sta     GRP0
    lda     $d8b3,x
    txs
    ldx     #BLACK|$0
    sta     GRP1
    sty     GRP0
    sta     GRP1
    stx     COLUPF
    tsx
    dex
    dec     ram_FB
    bpl     kernelDrawStatusBar
    ldx     #$ff
    txs
    lda     #$02
    sta     VBLANK
    inx
    stx     GRP0
    stx     GRP1
    stx     GRP0
    stx     NUSIZ1
    ldx     ram_B4
    beq     Ld3aa
    lda     ram_C1
    and     #$02
    bne     Ld3ac
    inx
Ld3aa
    stx     WSYNC
;---------------------------------------
Ld3ac
    stx     WSYNC
;---------------------------------------
    cpx     #$05
    beq     Ld3b4
    stx     WSYNC
;---------------------------------------
Ld3b4
    ldx     #$dc
    stx     TIM8T
    bit     ram_F9
    bmi     Ld403
    lda     ram_B6
    beq     Ld403
    lda     ram_A4
    beq     Ld403
    ldy     currentScreenId
    beq     Ld3cf
    cpy     #$03
    bne     Ld403
    adc     #$14
Ld3cf
    tax
    lda     $d868,x
    ldx     ram_F8
    inx
    clc
    adc     $dafa,x
    cmp     ram_F6
    bcc     Ld3e7
    sbc     $d9e6,x
    cmp     ram_F6
    bcc     Ld3f9
    beq     Ld3f9
Ld3e7
    inc     ram_F5
    ldx     ram_E3
    cpx     #$06
    bne     Ld3f1
    inc     ram_F5
Ld3f1
    lda     #$41
    ldx     ram_CA
    bne     Ld403
    beq     Ld401
Ld3f9
    lda     #$00
    ldx     ram_CA
    cpx     #$41
    bne     Ld403
Ld401
    sta     ram_CA
Ld403
    ldx     #$00
    lda     crosshairX
    cmp     targetAuxX
    bcc     Ld411
    sbc     #$0b
    cmp     targetAuxX
    bcc     Ld412
Ld411
    dex
Ld412
    stx     ram_A1
    lda     SWCHA
    cmp     #$ff
    beq     Ld422
    ldx     #$ff
    stx     ram_F7
    inx
    stx     ram_B8
Ld422
    inc     ram_B9
    bne     Ld444
    lda     ram_BA
    cmp     #$0f
    bcc     Ld430
    ldx     ram_B6
    stx     ram_B7
Ld430
    inc     ram_BA
    bne     Ld444
    inc     ram_B8
    lda     ram_B8
    and     #$04
    beq     Ld444
    sta     ram_B8
    lda     #$00
    sta     ram_B6
    sta     ram_B7
Ld444
    lda     currentScreenId
    cmp     #$04
    bne     Ld45f
    lda     ram_E9
    bmi     Ld45f
    dec     ram_EE
    bne     Ld45f
    asl
    ora     #$02
    sta     ram_EE
    lda     ram_CA
    bne     Ld45f
    lda     #$34
    sta     ram_CA
Ld45f
    lda     ram_C1
    and     #$07
    bne     Ld48b
    bit     ram_F9
    bpl     Ld481
    lda     ram_B5
    cmp     #$02
    bne     Ld477
    dec     ram_88
    bpl     Ld481
Ld473
    inc     ram_88
    bpl     Ld481
Ld477
    cmp     #$04
    beq     Ld481
    lda     ram_88
    cmp     #$0d
    bne     Ld473
Ld481
    lda     ram_B7
    beq     Ld489
    ldx     #$01
    stx     ram_C8
Ld489
    dec     ram_C8
Ld48b
    ldx     ram_C9
    cpx     #$fe
    bne     Ld495
    stx     ram_CB
    inc     ram_C9
Ld495
    lda     ram_CA
    beq     Ld4c8
    sta     AUDC0
    jsr     $ddfb
    tax
    lda     $d94c,x
    inc     ram_CB
    cmp     ram_CB
    bcs     Ld4d0
    lda     #$00
    sta     ram_CB
    inc     ram_C9
    lda     $d8f7,x
    adc     ram_C9
    tax
    lda     $d903,x
    beq     Ld4c8
    sta     AUDF0
    jsr     $ddfb
    ldx     ram_B6
    bne     Ld4c3
    txa
Ld4c3
    sta     AUDV0
    jmp     $d4d0
    
Ld4c8
    sta     AUDV0
    sta     ram_CA
    ldx     #$fe
    stx     ram_C9
Ld4d0
    lda     ram_B6
    beq     Ld502
    
    .byte   $a9,$03,$85,$1a,$a9,$06,$85,$16 ; $d4d4 *)
    .byte   $a9,$1f,$85,$18,$a5,$81,$c9,$0f ; $d4dc *)
    .byte   $d0,$04,$a6,$b4,$f0,$1c,$a2,$08 ; $d4e4 *)
    .byte   $86,$16,$20,$fb,$dd,$18,$69,$02 ; $d4ec *)
    .byte   $65,$b4,$a6,$ca,$d0,$0a,$a2,$02 ; $d4f4 *)
    .byte   $86,$15,$a2,$0e,$86,$17         ; $d4fc *)
    
Ld502
    sta     AUDV0
    sta     AUDV1
    lda     ram_B7
    beq     Ld52a
    lda     ram_B5
    bne     Ld514
    jmp     $d728
    
Ld511
    jmp     $d647
    
Ld514
    .byte   $a5,$b4,$f0,$06,$a9,$02,$85,$ab ; $d514 *)
    .byte   $d0,$1e,$a5,$c4,$d0,$1a,$a5,$a6 ; $d51c *)
    .byte   $c9,$df,$b0,$02,$a9,$e0         ; $d524 *)
    
Ld52a
    sbc     #$08
    eor     #$ff
    bit     ram_F9
    bpl     Ld534
    
    .byte   $a9,$18                         ; $d532 *)
    
Ld534
    sta     ram_FA
    ldx     ram_AF
    beq     Ld53c
    
    .byte   $e6,$be                         ; $d53a *)
    
Ld53c
    inc     ram_BE
    cmp     ram_BE
    bcs     Ld511
    lda     #$00
    sta     ram_BE
    inc     ram_B1
    bpl     Ld554
    
    .byte   $a5,$ea,$c9,$08,$f0,$08         ; $d54a *)
    
Ld550
    inc     ram_EA
    bpl     Ld558
Ld554
    dec     ram_EA
    bmi     Ld550
Ld558
    inc     ram_E0
    lda     ram_B5
    cmp     #$02
    bne     Ld564
    
    .byte   $24,$f9,$30,$05                 ; $d560 *)
    
Ld564
    lda     ram_E0
    lsr
    bcc     Ld56b
    inc     ram_B0
Ld56b
    inc     ram_EF
    lda     ram_B7
    beq     Ld577
    
    .byte   $a5,$ab,$29,$01,$f0,$1d         ; $d571 *)
    
Ld577
    dec     ram_89
    lda     ram_89
    cmp     #$0e
    bne     Ld594
    lda     #$14
    sta     ram_89
    ldx     #$0b
    lda     ram_8A,x
    tay
    dex
    bmi     Ld592
    lda     ram_8A,x
    sty     ram_8A,x
    jmp     $d587
    
Ld592
    sty     ram_95
Ld594
    lda     ram_AB
    and     #$08
    beq     Ld5ab
    
    .byte   $a2,$0b,$d6,$8a,$b5,$8a,$c9,$ff ; $d59a *)
    .byte   $d0,$04,$a9,$84,$95,$8a,$ca,$10 ; $d5a2 *)
    .byte   $f1                             ; $d5aa *)
    
Ld5ab
    lda     ram_AB
    and     #$02
    beq     Ld5d4
    
    .byte   $e6,$89,$a5,$89,$c9,$15,$d0,$17 ; $d5b1 *)
    .byte   $a9,$0f,$85,$89,$a2,$00,$b5,$8a ; $d5b9 *)
    .byte   $a8,$e8,$e0,$0c,$f0,$07,$b5,$8a ; $d5c1 *)
    .byte   $94,$8a,$4c,$c1,$d5,$85,$8a,$c6 ; $d5c9 *)
    .byte   $ef,$c6,$ef                     ; $d5d1 *)
    
Ld5d4
    lda     ram_AB
    and     #$04
    beq     Ld5eb
    
    .byte   $a2,$0b,$f6,$8a,$b5,$8a,$c9,$85 ; $d5da *)
    .byte   $90,$04,$a9,$00,$95,$8a,$ca,$10 ; $d5e2 *)
    .byte   $f1                             ; $d5ea *)
    
Ld5eb
    lda     ram_B7
    beq     Ld5f5
    
    .byte   $a5,$b5,$c9,$02,$f0,$03         ; $d5ef *)
    
Ld5f5
    jmp     $d728
    
    .byte   $a0,$01,$a5,$fa,$a6,$e2,$e0,$ff ; $d5f8 *)
    .byte   $d0,$02,$a9,$28,$38,$e9,$18,$f0 ; $d600 *)
    .byte   $3e,$90,$04,$c8,$49,$ff,$18,$69 ; $d608 *)
    .byte   $12,$e6,$bf,$c5,$bf,$f0,$02,$b0 ; $d610 *)
    .byte   $2e,$a9,$00,$85,$bf,$c0,$01,$f0 ; $d618 *)
    .byte   $03,$e6,$b0,$2c,$c6,$b0,$a6,$e3 ; $d620 *)
    .byte   $a5,$bd,$e6,$bd,$dd,$b1,$df,$d0 ; $d628 *)
    .byte   $16,$a9,$00,$85,$bd,$a5,$80,$dd ; $d630 *)
    .byte   $fa,$dc,$b0,$09,$0a,$0a,$90,$03 ; $d638 *)
    .byte   $c6,$b2,$2c,$e6,$b2,$84,$f4     ; $d640 *)
    
Ld647
    lda     ram_B0
    sec
    sbc     ram_B1
    cmp     #$10
    bcc     Ld653
    jmp     $d6d2
    
Ld653
    lsr
    tax
    lda     $d893,x
    sta     NUSIZ0
    lda     $dfde,x
    sta     ram_FB
    lda     $d9c2,x
    clc
    adc     ram_EA
    cmp     #$9f
    bcc     Ld66b
    
    .byte   $a9,$9f                         ; $d669 *)
    
Ld66b
    sta     COLUP0
    ldy     #$00
    lda     ram_B2
    clc
    adc     #$50
    cmp     #$a0
    bcs     Ld684
    adc     #$b0
    bcs     Ld681
    
    .byte   $c8,$49,$ff,$69,$01             ; $d67c *)
    
Ld681
    cmp     $dfe6,x
Ld684
    bcs     Ld6d0
    cpx     #$00
    beq     Ld696
    
    .byte   $86,$fc,$e0,$06,$90,$01,$98,$4a ; $d68a *)
    .byte   $c6,$fc,$d0,$fb                 ; $d692 *)
    
Ld696
    sta     ram_FA
    lda     ram_B2
    cpy     #$01
    beq     Ld6a2
    clc
    adc     ram_FA
    .byte   $2c ;bit                ;4-3 =  22
Ld6a2
    sbc     ram_FA
    clc
    adc     #$50
    cmp     ram_DF
    beq     Ld6b8
    lda     ram_C1
    and     #$03
    bne     Ld6b8
    bcs     Ld6b6
    
    .byte   $c6,$df,$2c                     ; $d6b3 *)
    
Ld6b6
    inc     ram_DF
Ld6b8
    ldy     ram_DF
    lda     ram_C1
    lsr
    sta     REFP0
    and     #$08
    bne     Ld6c7
    txa
    ora     #$08
    tax
Ld6c7
    sec
    tya
    sbc     $d9ca,x
    bcc     Ld728
    
    .byte   $b0,$3b                         ; $d6ce *)
Ld6d0
    .byte   $b0,$56                         ; $d6d0 *)
    
Ld6d2
    cmp     #$80
    bcc     Ld6de
    ldx     #$ce
    ldy     #$05
    lda     #BLUE_CYAN|$d
    bcs     Ld6e8
    
Ld6de
    .byte   $c9,$40,$90,$46,$a2,$0e,$a0,$00 ; $d6de *)
    .byte   $a9,$90                         ; $d6e6 *)
    
Ld6e8
    stx     ram_FB
    sty     NUSIZ0
    sta     COLUP0
    lda     ram_DF
    beq     Ld728
    cmp     #$94
    beq     Ld728
    lda     #$94
    ldx     ram_B2
    cpx     #$80
    bcc     Ld700
    
    .byte   $a9,$00                         ; $d6fe *)
    
Ld700
    cmp     ram_DF
    bcs     Ld707
    
    .byte   $c6,$df,$2c                     ; $d704 *)
    
Ld707
    inc     ram_DF
    lda     ram_DF
    sta     ram_DE
    lda     ram_B5
    cmp     #$02
    bne     Ld728
    
    .byte   $a5,$88,$d0,$11,$a5,$b3,$69,$0f ; $d713 *)
    .byte   $c9,$21,$b0,$09,$a5,$b7,$f0,$05 ; $d71b *)
    .byte   $a5,$a5,$e9,$c2,$2c             ; $d723 *)
    
Ld728
    lda     #$6a
    sta     ram_FA
    ldx     #$04
    lda     #$14
    sec
    sbc     ram_89
    adc     ram_FA
    sec
    sbc     #$15
    sta     ram_FA
    bpl     Ld756
    
    .byte   $69,$16,$18,$65,$fb,$4c,$58,$d7 ; $d73c *)
    
Ld744
    lda     ram_FA
    sec
    sbc     #$12
    sta     ram_FA
    bpl     Ld756
    
    .byte   $c9,$e2,$90,$05,$69,$15,$4c,$3e ; $d74d *)
    .byte   $d7                             ; $d755 *)
    
Ld756
    lda     #$04
    sta     ram_D8,x
    dex
    bpl     Ld744
    lda     #$ff
    sta     ram_DD
    lda     currentScreenId
    bne     endOfKernelSwitch
    
Ld765
    .byte   $20,$da,$d9,$b0,$0f,$a5,$aa,$69 ; $d765 *)
    .byte   $02,$85,$ad,$85,$f6,$a5,$a4     ; $d76d *)
    
bankSwitch0to1
    sta     ram_AC
    bit     bank1Strobe
    
    .byte   $a5,$b2,$69,$03,$10,$02,$a9,$00 ; $d779 *)
    .byte   $c9,$08,$90,$02,$a9,$06,$69,$02 ; $d781 *)
    .byte   $a6,$84,$d0,$02,$69,$16,$85,$ad ; $d789 *)
    .byte   $a9,$07,$d0,$df                 ; $d791 *)
    
;-----------------------------------------------------------
;      End of Kernel / Switch to Logic (Bank 0 -> Bank 1)
;-----------------------------------------------------------
endOfKernelSwitch
    cmp     #$03
    beq     Ld765
    cmp     #$02
    bne     Ld7ea
    
    .byte   $20,$da,$d9,$90,$28,$a5,$b2,$e9 ; $d79d *)
    .byte   $80,$20,$fb,$dd,$18,$e9,$01,$b0 ; $d7a5 *)
    .byte   $02,$a9,$00,$85,$ad,$a9,$d2,$38 ; $d7ad *)
    .byte   $e5,$a5,$18,$69,$0b,$c9,$15,$90 ; $d7b5 *)
    .byte   $09,$a2,$01,$c9,$80,$b0,$02,$a2 ; $d7bd *)
    .byte   $15,$8a,$4c,$74,$d7,$20,$a3,$d9 ; $d7c5 *)
    .byte   $4a,$18,$69,$14,$85,$ad,$a5,$b2 ; $d7cd *)
    .byte   $69,$0e,$c9,$1b,$90,$08,$c9,$8e ; $d7d5 *)
    .byte   $a9,$1b,$90,$02,$a9,$ff,$69,$01 ; $d7dd *)
    .byte   $49,$1f,$4a,$10,$57             ; $d7e5 *)
    
Ld7ea
    cmp     #$01
    bne     Ld808
    
    .byte   $20,$a3,$d9,$20,$da,$d9,$a5,$b0 ; $d7ee *)
    .byte   $b0,$02,$a5,$b1,$4a,$4a,$4a,$aa ; $d7f6 *)
    .byte   $18,$69,$05,$85,$ad,$bd,$ec,$de ; $d7fe *)
    .byte   $d0,$bf                         ; $d806 *)
    
Ld808
    cmp     #$04
    bne     Ld847
    
    .byte   $20,$da,$d9,$b0,$14,$a5,$e9,$10 ; $d80c *)
    .byte   $02,$a9,$00,$4a,$4a,$85,$ad,$a5 ; $d814 *)
    .byte   $a5,$4a,$d0,$02,$a9,$ff,$38,$b0 ; $d81c *)
    .byte   $1d,$a5,$b2,$69,$4f,$c9,$a0,$90 ; $d824 *)
    .byte   $02,$e9,$9f,$4a,$4a,$4a,$18,$69 ; $d82c *)
    .byte   $13,$85,$ad,$a5,$a5,$4a,$f0,$03 ; $d834 *)
    .byte   $49,$0f,$2c,$a9,$10,$18,$69,$03 ; $d83c *)
    
Ld844
    jmp     $d774
    
Ld847
    lda     #$00
    sta     ram_AD
    ldx     ram_9B
    ldy     ram_E3
    lda     #$03
    cpx     #$35
    bcc     Ld85b
    cpy     #$02
    bcc     Ld85b
    
    .byte   $a9,$09                         ; $d859 *)
    
Ld85b
    cpx     #$45
    bcc     Ld865
    cpy     #$04
    bcc     Ld865
    
    .byte   $a9,$0f                         ; $d863 *)
    
Ld865
    bne     Ld844
    
    .byte   $00,$04,$04,$04,$04,$04,$05,$05 ; $d867 *)
    .byte   $06,$07,$08,$0a,$0c,$0e,$11,$14 ; $d86f *)
    .byte   $17,$19,$1b,$1d,$1e,$1f,$1f,$1f ; $d877 *)
    .byte   $1d,$1f,$20,$20,$1f,$1d,$1b,$19 ; $d87f *)
    .byte   $18,$14,$14,$16,$19,$19,$16,$13 ; $d887 *)
    .byte   $0f,$0c,$09,$09                 ; $d88f *)
    .byte   $05                             ; $d893 D)
    .byte   $05,$00,$00,$00,$00,$00,$00     ; $d894 *)

activisionLogo  
    .byte   %01100001 ; | ##    #|            $d89b A)
    .byte   %00110001 ; |  ##   #|            $d89c A)
    .byte   %00011111 ; |   #####|            $d89d A)
    .byte   %00001101 ; |    ## #|            $d89e A)
    .byte   %00000111 ; |     ###|            $d89f A)
    .byte   %00000011 ; |      ##|            $d8a0 A)
    .byte   %00000001 ; |       #|            $d8a1 A)
    .byte   %00000000 ; |        |            $d8a2 A)
    .byte   %01110101 ; | ### # #|            $d8a3 C)
    .byte   %01000101 ; | #   # #|            $d8a4 C)
    .byte   %01000101 ; | #   # #|            $d8a5 C)
    .byte   %01000101 ; | #   # #|            $d8a6 C)
    .byte   %01110101 ; | ### # #|            $d8a7 C)
    .byte   %00000100 ; |     #  |            $d8a8 C)
    .byte   %01111111 ; | #######|            $d8a9 C)
    .byte   %00000000 ; |        |            $d8aa C)
    .byte   %01100000 ; | ##     |            $d8ab T)
    .byte   %01110000 ; | ###    |            $d8ac T)
    .byte   %01011000 ; | # ##   |            $d8ad G)
    .byte   %01001100 ; | #  ##  |            $d8ae G)
    .byte   %01000110 ; | #   ## |            $d8af G)
    .byte   %01000011 ; | #    ##|            $d8b0 G)
    .byte   %11000001 ; |##     #|            $d8b1 G)
    .byte   %00000000 ; |        |            $d8b2 G)
    .byte   %10111010 ; |# ### # |            $d8b3 G)
    .byte   %10001010 ; |#   # # |            $d8b4 G)
    .byte   %10111010 ; |# ### # |            $d8b5 G)
    .byte   %10100010 ; |# #   # |            $d8b6 G)
    .byte   %00111010 ; |  ### # |            $d8b7 G)
    .byte   %00000000 ; |        |            $d8b8 G)
    .byte   %11111110 ; |####### |            $d8b9 G)
    .byte   %00000000 ; |        |            $d8ba G)
    .byte   %11101001 ; |### #  #|            $d8bb G)
    .byte   %10101011 ; |# # # ##|            $d8bc G)
    .byte   %10101111 ; |# # ####|            $d8bd G)
    .byte   %10101101 ; |# # ## #|            $d8be G)
    .byte   %11101001 ; |### #  #|            $d8bf G)

copyrightGfx
    .byte   %00000000 ; |        |            $d8c0 G)
    .byte   %00000000 ; |        |            $d8c1 G)
    .byte   %00000000 ; |        |            $d8c2 G)
    .byte   %01110111 ; | ### ###|            $d8c3 G)
    .byte   %01010001 ; | # #   #|            $d8c4 G)
    .byte   %01110011 ; | ###  ##|            $d8c5 G)
    .byte   %01010001 ; | # #   #|            $d8c6 G)
    .byte   %01110111 ; | ### ###|            $d8c7 G)
    .byte   %00000000 ; |        |            $d8c8 G)
    .byte   %00000000 ; |        |            $d8c9 G)
    .byte   %00000000 ; |        |            $d8ca G)
    .byte   %00010001 ; |   #   #|            $d8cb G)
    .byte   %00010001 ; |   #   #|            $d8cc G)
    .byte   %00010111 ; |   # ###|            $d8cd G)
    .byte   %00010101 ; |   # # #|            $d8ce G)
    .byte   %00010111 ; |   # ###|            $d8cf G)
    .byte   %00000000 ; |        |            $d8d0 G)
    .byte   %10000000 ; |#       |            $d8d1 G)
    .byte   %10000000 ; |#       |            $d8d2 G)
    .byte   %10101010 ; |# # # # |            $d8d3 G)
    .byte   %10101010 ; |# # # # |            $d8d4 G)
    .byte   %10111010 ; |# ### # |            $d8d5 G)
    .byte   %00100111 ; |  #  ###|            $d8d6 G)
    .byte   %00100010 ; |  #   # |            $d8d7 G)
    .byte   %00000000 ; |        |            $d8d8 G)
    .byte   %00000011 ; |      ##|            $d8d9 G)
    .byte   %00000000 ; |        |            $d8da G)
    .byte   %01001011 ; | #  # ##|            $d8db G)
    .byte   %01001010 ; | #  # # |            $d8dc G)
    .byte   %01101011 ; | ## # ##|            $d8dd G)
    .byte   %00000000 ; |        |            $d8de G)
    .byte   %00001000 ; |    #   |            $d8df G)
    .byte   %00000000 ; |        |            $d8e0 G)
    .byte   %01000111 ; | #   ###|            $d8e1 G)
    .byte   %01000001 ; | #     #|            $d8e2 G)
    .byte   %01110111 ; | ### ###|            $d8e3 G)
    .byte   %01010101 ; | # # # #|            $d8e4 G)
    .byte   %01110101 ; | ### # #|            $d8e5 G)
    .byte   %00000000 ; |        |            $d8e6 G)
    .byte   %00000000 ; |        |            $d8e7 G)
    .byte   %00000000 ; |        |            $d8e8 G)
    .byte   %00000000 ; |        |            $d8e9 G)
    .byte   %00000000 ; |        |            $d8ea G)
    .byte   %11110111 ; |#### ###|            $d8eb G)
    .byte   %10010101 ; |#  # # #|            $d8ec G)
    .byte   %10000111 ; |#    ###|            $d8ed G)
    .byte   %10010000 ; |#  #    |            $d8ee G)
    .byte   %11110000 ; |####    |            $d8ef G)
    
    .byte   BLUE|$4                         ; $d8f0 CP)
    .byte   GREEN_YELLOW|$6                 ; $d8f1 CP)
    .byte   GREEN_YELLOW|$6                 ; $d8f2 CP)
    .byte   YELLOW|$a                       ; $d8f3 CP)
    .byte   BROWN|$6                        ; $d8f4 CP)
    .byte   BROWN|$6                        ; $d8f5 CP)
    .byte   RED|$4                          ; $d8f6 CP)
    .byte   BLACK|$0                        ; $d8f7 CP)
    
    .byte   $02,$04,$11,$13,$1e,$20,$2f,$40 ; $d8f8 *)
    .byte   $20,$40,$0d,$57,$00,$5f,$00
    .byte   %01010101 ; | # # # #|            $d907 G)
    .byte   %01010000 ; | # #    |            $d908 G)
    .byte   %01010101 ; | # # # #|            $d909 G)
    .byte   %01010000 ; | # #    |            $d90a G)
    .byte   %01010101 ; | # # # #|            $d90b G)
    .byte   %01010000 ; | # #    |            $d90c G)
    .byte   %01010101 ; | # # # #|            $d90d G)
    .byte   %01010000 ; | # #    |            $d90e G)
    .byte   %00000000 ; |        |            $d90f G)
    .byte   %01011010 ; | # ## # |            $d910 G)
    .byte   %01011000 ; | # ##   |            $d911 G)
    .byte   %01011010 ; | # ## # |            $d912 G)
    .byte   %01011000 ; | # ##   |            $d913 G)
    .byte   %01011010 ; | # ## # |            $d914 G)
    .byte   %00000000 ; |        |            $d915 G)
    .byte   %01001000 ; | #  #   |            $d916 G)
    .byte   %00001000 ; |    #   |            $d917 G)
    .byte   %01001000 ; | #  #   |            $d918 G)
    .byte   %00001000 ; |    #   |            $d919 G)
    .byte   %01001000 ; | #  #   |            $d91a G)
    .byte   %00001000 ; |    #   |            $d91b G)
    .byte   %01001000 ; | #  #   |            $d91c G)
    .byte   %00001000 ; |    #   |            $d91d G)
    .byte   %01001000 ; | #  #   |            $d91e G)
    .byte   %00001000 ; |    #   |            $d91f G)
    .byte   %00000000 ; |        |            $d920 G)
    .byte   %10111111 ; |# ######|            $d921 G)
    .byte   %00000000 ; |        |            $d922 G)
	
	.byte   $cb,$c3,$c5,$87,$88 ; $d920 *)
    .byte   $89,$8c,$4c,$4e,$4f,$4f,$2e,$2d ; $d928 *)
    .byte   $2e,$00,$64,$64,$44,$44,$44,$44 ; $d930 *)
    .byte   $44,$44,$44,$44,$04,$ec,$ec,$cc ; $d938 *)
    .byte   $8c,$2c,$00,$e2,$cb,$ae,$90,$73 ; $d940 *)
    .byte   $56,$36,$16,$00,$0a,$2a,$12,$08 ; $d948 *)
    .byte   $12,$0a,$04,$08,$03,$05,$02,$12 ; $d950 *)
    
Ld958
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sec
Ld95d
    sbc     #$0f
    bcs     Ld95d
    eor     #$0f
    asl
    asl
    asl
    nop
    nop
    sta     RESP0,x
    sta     HMCLR
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    nop
    nop
    nop
    asl
    adc     #$80
    sta     HMP0,x
    rts
    
Ld979
    ldy     ram_FA
    lda     (screenPtr1L),y
    sta     ram_FC
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     (screenPtr6L),y
    sta     GRP0
    lda     (screenPtr5L),y
    sta     GRP1
    lda     (screenPtr4L),y
    sta     GRP0
    lda     (screenPtr3L),y
    tax
    lda     (screenPtr2L),y
    ldy     ram_FC
    stx     GRP1
    sta     GRP0
    sty     GRP1
    sty     GRP0
    dec     ram_FA
    bpl     Ld979
    rts
    
    .byte   $a2,$02,$a5,$88,$d0,$13,$a5,$b3 ; $d9a3 *)
    .byte   $69,$10,$c9,$21,$b0,$0b,$a5,$b0 ; $d9ab *)
    .byte   $e5,$b1,$18,$69,$11,$c9,$20,$90 ; $d9b3 *)
    .byte   $03,$a9,$28,$ca,$86,$84,$60     ; $d9bb *)
    
    .byte   BLUE_CYAN|$a                    ; $d9c2 C)
    
    .byte   $9a,$98,$98,$96,$96,$94,$93,$06 ; $d9c3 *)
    .byte   $06                             ; $d9cb *)
    .byte   $01,$01,$01,$01,$01,$01,$04,$04 ; $d9cc D)
    .byte   $00,$00,$00,$00,$00,$00,$18,$a5 ; $d9d4 D)
    .byte   $c1,$29,$01,$d0,$01,$38,$60,$00 ; $d9dc D)
    .byte   $00                             ; $d9e4 D)
 
 
;-----------------------------------------------------------
;      Graphic Data: Dashboard & Screens
;-----------------------------------------------------------
    
    .byte   %11111111 ; |########|            $d9e5 G)
    .byte   %00000111 ; |     ###|            $d9e6 G)
    .byte   %00000101 ; |     # #|            $d9e7 G)
    .byte   %00100001 ; |  #    #|            $d9e8 G)
    .byte   %00000001 ; |       #|            $d9e9 G)
    .byte   %00000001 ; |       #|            $d9ea G)
    .byte   %00000001 ; |       #|            $d9eb G)
    .byte   %00000001 ; |       #|            $d9ec G)
    .byte   %00000001 ; |       #|            $d9ed G)
    .byte   %00000001 ; |       #|            $d9ee G)
    .byte   %00100001 ; |  #    #|            $d9ef G)
    
thrustTSprite   
    .byte   %00100000 ; |  #     |            $d9f0 G)
    .byte   %00100000 ; |  #     |            $d9f1 G)
    .byte   %00100000 ; |  #     |            $d9f2 G)
    .byte   %00100000 ; |  #     |            $d9f3 G)
    .byte   %00100000 ; |  #     |            $d9f4 G)
    .byte   %00100000 ; |  #     |            $d9f5 G)
    .byte   %10101000 ; |# # #   |            $d9f6 G)
    .byte   %11111000 ; |#####   |            $d9f7 G)
    .byte   %00010000 ; |   #    |            $d9f8 G)
    .byte   %00010001 ; |   #   #|            $d9f9 G)
    .byte   %00010000 ; |   #    |            $d9fa G)
    .byte   %00010000 ; |   #    |            $d9fb G)
    .byte   %00010000 ; |   #    |            $d9fc G)
    .byte   %00000000 ; |        |            $d9fd G)
    .byte   %11111111 ; |########|            $d9fe G)
    .byte   %00000000 ; |        |            $d9ff G)

launchScreen0
    .byte   %11110110 ; |#### ## |            $da00 G)
    .byte   %11110110 ; |#### ## |            $da01 G)
    .byte   %11111011 ; |##### ##|            $da02 G)
    .byte   %11111011 ; |##### ##|            $da03 G)
    .byte   %11101101 ; |### ## #|            $da04 G)
    .byte   %11101101 ; |### ## #|            $da05 G)
    .byte   %11101110 ; |### ### |            $da06 G)
    .byte   %11101110 ; |### ### |            $da07 G)
    .byte   %11111111 ; |########|            $da08 G)
    .byte   %11111111 ; |########|            $da09 G)
    .byte   %11111111 ; |########|            $da0a G)
    .byte   %11111111 ; |########|            $da0b G)
    .byte   %11111111 ; |########|            $da0c G)
    .byte   %11111111 ; |########|            $da0d G)
    .byte   %11111111 ; |########|            $da0e G)
    .byte   %11111111 ; |########|            $da0f G)
    .byte   %11111111 ; |########|            $da10 G)
    .byte   %11111111 ; |########|            $da11 G)
    .byte   %11111111 ; |########|            $da12 G)
    .byte   %10001111 ; |#   ####|            $da13 G)
    .byte   %11011111 ; |## #####|            $da14 G)
    .byte   %11011111 ; |## #####|            $da15 G)
    .byte   %11011111 ; |## #####|            $da16 G)
    .byte   %10011111 ; |#  #####|            $da17 G)
    .byte   %11111111 ; |########|            $da18 G)

launchScreen1
    .byte   %11110111 ; |#### ###|            $da19 G)
    .byte   %11110111 ; |#### ###|            $da1a G)
    .byte   %11111111 ; |########|            $da1b G)
    .byte   %11111111 ; |########|            $da1c G)
    .byte   %11111111 ; |########|            $da1d G)
    .byte   %11111111 ; |########|            $da1e G)
    .byte   %11111111 ; |########|            $da1f G)
    .byte   %11111111 ; |########|            $da20 G)
    .byte   %01111111 ; | #######|            $da21 G)
    .byte   %10111111 ; |# ######|            $da22 G)
    .byte   %11011111 ; |## #####|            $da23 G)
    .byte   %11100111 ; |###  ###|            $da24 G)
    .byte   %00111001 ; |  ###  #|            $da25 G)
    .byte   %01111110 ; | ###### |            $da26 G)
    .byte   %00111111 ; |  ######|            $da27 G)
    .byte   %10111111 ; |# ######|            $da28 G)
    .byte   %00111111 ; |  ######|            $da29 G)
    .byte   %11111111 ; |########|            $da2a G)
    .byte   %11111111 ; |########|            $da2b G)
    .byte   %11111111 ; |########|            $da2c G)
    .byte   %11111111 ; |########|            $da2d G)
    .byte   %11111111 ; |########|            $da2e G)
    .byte   %11111111 ; |########|            $da2f G)
    .byte   %11111111 ; |########|            $da30 G)
    .byte   %11111111 ; |########|            $da31 G)

launchScreen2
    .byte   %10111101 ; |# #### #|            $da32 G)
    .byte   %10111101 ; |# #### #|            $da33 G)
    .byte   %11111111 ; |########|            $da34 G)
    .byte   %11111111 ; |########|            $da35 G)
    .byte   %11111111 ; |########|            $da36 G)
    .byte   %11111111 ; |########|            $da37 G)
    .byte   %11111111 ; |########|            $da38 G)
    .byte   %11111111 ; |########|            $da39 G)
    .byte   %11111111 ; |########|            $da3a G)
    .byte   %11111111 ; |########|            $da3b G)
    .byte   %11111111 ; |########|            $da3c G)
    .byte   %11111111 ; |########|            $da3d G)
    .byte   %11111111 ; |########|            $da3e G)
    .byte   %01111111 ; | #######|            $da3f G)
    .byte   %10001111 ; |#   ####|            $da40 G)
    .byte   %11110001 ; |####   #|            $da41 G)
    .byte   %11111110 ; |####### |            $da42 G)
    .byte   %11111111 ; |########|            $da43 G)
    .byte   %11111100 ; |######  |            $da44 G)
    .byte   %11111110 ; |####### |            $da45 G)
    .byte   %11111100 ; |######  |            $da46 G)
    .byte   %11111110 ; |####### |            $da47 G)
    .byte   %11111100 ; |######  |            $da48 G)
    .byte   %11111111 ; |########|            $da49 G)
    .byte   %11111111 ; |########|            $da4a G)

launchScreen3
    .byte   %11101111 ; |### ####|            $da4b G)
    .byte   %11101111 ; |### ####|            $da4c G)
    .byte   %11111111 ; |########|            $da4d G)
    .byte   %00000000 ; |        |            $da4e G)
    .byte   %01111011 ; | #### ##|            $da4f G)
    .byte   %01111011 ; | #### ##|            $da50 G)
    .byte   %01010101 ; | # # # #|            $da51 G)
    .byte   %01111111 ; | #######|            $da52 G)
    .byte   %01111111 ; | #######|            $da53 G)
    .byte   %01010101 ; | # # # #|            $da54 G)
    .byte   %01111011 ; | #### ##|            $da55 G)
    .byte   %01111011 ; | #### ##|            $da56 G)
    .byte   %00000000 ; |        |            $da57 G)
    .byte   %11111111 ; |########|            $da58 G)
    .byte   %11111111 ; |########|            $da59 G)
    .byte   %11111111 ; |########|            $da5a G)
    .byte   %00111111 ; |  ######|            $da5b G)
    .byte   %11001111 ; |##  ####|            $da5c G)
    .byte   %11110011 ; |####  ##|            $da5d G)
    .byte   %11111100 ; |######  |            $da5e G)
    .byte   %11111111 ; |########|            $da5f G)
    .byte   %11111111 ; |########|            $da60 G)
    .byte   %11111111 ; |########|            $da61 G)
    .byte   %11111111 ; |########|            $da62 G)
    .byte   %11111111 ; |########|            $da63 G)

launchScreen4
    .byte   %01111011 ; | #### ##|            $da64 G)
    .byte   %01111011 ; | #### ##|            $da65 G)
    .byte   %11111111 ; |########|            $da66 G)
    .byte   %00010011 ; |   #  ##|            $da67 G)
    .byte   %11011011 ; |## ## ##|            $da68 G)
    .byte   %11011011 ; |## ## ##|            $da69 G)
    .byte   %01010011 ; | # #  ##|            $da6a G)
    .byte   %11011011 ; |## ## ##|            $da6b G)
    .byte   %11011011 ; |## ## ##|            $da6c G)
    .byte   %01010011 ; | # #  ##|            $da6d G)
    .byte   %11011011 ; |## ## ##|            $da6e G)
    .byte   %11011011 ; |## ## ##|            $da6f G)
    .byte   %00010011 ; |   #  ##|            $da70 G)
    .byte   %11111011 ; |##### ##|            $da71 G)
    .byte   %11111011 ; |##### ##|            $da72 G)
    .byte   %01010011 ; | # #  ##|            $da73 G)
    .byte   %10111011 ; |# ### ##|            $da74 G)
    .byte   %01011011 ; | # ## ##|            $da75 G)
    .byte   %11110011 ; |####  ##|            $da76 G)
    .byte   %11111011 ; |##### ##|            $da77 G)
    .byte   %01111011 ; | #### ##|            $da78 G)
    .byte   %10110011 ; |# ##  ##|            $da79 G)
    .byte   %11111111 ; |########|            $da7a G)
    .byte   %11111111 ; |########|            $da7b G)
    .byte   %11111111 ; |########|            $da7c G)

;======================
; Orbit Screen
;======================
orbitScreen0
    .byte   %11111111 ; |########|            $da7d G)
    .byte   %11111110 ; |####### |            $da7e G)
    .byte   %11111110 ; |####### |            $da7f G)
    .byte   %11111110 ; |####### |            $da80 G)
    .byte   %11111111 ; |########|            $da81 G)
    .byte   %11111111 ; |########|            $da82 G)
    .byte   %11000011 ; |##    ##|            $da83 G)
    .byte   %11011111 ; |## #####|            $da84 G)
    .byte   %11001111 ; |##  ####|            $da85 G)
    .byte   %11011111 ; |## #####|            $da86 G)
    .byte   %11000111 ; |##   ###|            $da87 G)
    .byte   %11011111 ; |## #####|            $da88 G)
    .byte   %11001111 ; |##  ####|            $da89 G)
    .byte   %11011111 ; |## #####|            $da8a G)
    .byte   %11000010 ; |##    # |            $da8b G)
    .byte   %11011110 ; |## #### |            $da8c G)
    .byte   %11001111 ; |##  ####|            $da8d G)
    .byte   %11011111 ; |## #####|            $da8e G)
    .byte   %11000111 ; |##   ###|            $da8f G)
    .byte   %11011111 ; |## #####|            $da90 G)
    .byte   %11001111 ; |##  ####|            $da91 G)
    .byte   %11011111 ; |## #####|            $da92 G)
    .byte   %11000011 ; |##    ##|            $da93 G)
    .byte   %11111111 ; |########|            $da94 G)
    .byte   %11111111 ; |########|            $da95 G)

orbitalScreen1
    .byte   %11111111 ; |########|            $da96 G)
    .byte   %00000000 ; |        |            $da97 G)
    .byte   %10101010 ; |# # # # |            $da98 G)
    .byte   %10101010 ; |# # # # |            $da99 G)
    .byte   %11111111 ; |########|            $da9a G)
    .byte   %11111111 ; |########|            $da9b G)
    .byte   %11111111 ; |########|            $da9c G)
    .byte   %11111111 ; |########|            $da9d G)
    .byte   %11111111 ; |########|            $da9e G)
    .byte   %11111111 ; |########|            $da9f G)
    .byte   %11111111 ; |########|            $daa0 G)
    .byte   %11111111 ; |########|            $daa1 G)
    .byte   %11111111 ; |########|            $daa2 G)
    .byte   %11111111 ; |########|            $daa3 G)
    .byte   %10101010 ; |# # # # |            $daa4 G)
    .byte   %11111111 ; |########|            $daa5 G)
    .byte   %01111111 ; | #######|            $daa6 G)
    .byte   %10111111 ; |# ######|            $daa7 G)
    .byte   %11011111 ; |## #####|            $daa8 G)
    .byte   %11101111 ; |### ####|            $daa9 G)
    .byte   %11110000 ; |####    |            $daaa G)
    .byte   %11111111 ; |########|            $daab G)
    .byte   %11111111 ; |########|            $daac G)
    .byte   %11111111 ; |########|            $daad G)
    .byte   %11111111 ; |########|            $daae G)

orbitalScreen2	
    .byte   %11111111 ; |########|            $daaf G)
    .byte   %00000000 ; |        |            $dab0 G)
    .byte   %10101010 ; |# # # # |            $dab1 G)
    .byte   %10101010 ; |# # # # |            $dab2 G)
    .byte   %11111111 ; |########|            $dab3 G)
    .byte   %11111111 ; |########|            $dab4 G)
    .byte   %11111111 ; |########|            $dab5 G)
    .byte   %11111111 ; |########|            $dab6 G)
    .byte   %11111111 ; |########|            $dab7 G)
    .byte   %11111111 ; |########|            $dab8 G)
    .byte   %11111111 ; |########|            $dab9 G)
    .byte   %11111111 ; |########|            $daba G)
    .byte   %11111111 ; |########|            $dabb G)
    .byte   %11111110 ; |####### |            $dabc G)
    .byte   %10101000 ; |# # #   |            $dabd G)
    .byte   %11111011 ; |##### ##|            $dabe G)
    .byte   %11110111 ; |#### ###|            $dabf G)
    .byte   %11101111 ; |### ####|            $dac0 G)
    .byte   %11011111 ; |## #####|            $dac1 G)
    .byte   %10111111 ; |# ######|            $dac2 G)
    .byte   %01111111 ; | #######|            $dac3 G)
    .byte   %11111111 ; |########|            $dac4 G)
    .byte   %11111111 ; |########|            $dac5 G)
    .byte   %11111111 ; |########|            $dac6 G)
    .byte   %11111111 ; |########|            $dac7 G)

orbitalScreen3
    .byte   %11111111 ; |########|            $dac8 G)
    .byte   %00000000 ; |        |            $dac9 G)
    .byte   %10101010 ; |# # # # |            $daca G)
    .byte   %10101010 ; |# # # # |            $dacb G)
    .byte   %11111111 ; |########|            $dacc G)
    .byte   %11111111 ; |########|            $dacd G)
    .byte   %11111111 ; |########|            $dace G)
    .byte   %11111111 ; |########|            $dacf G)
    .byte   %11110000 ; |####    |            $dad0 G)
    .byte   %11101111 ; |### ####|            $dad1 G)
    .byte   %11011111 ; |## #####|            $dad2 G)
    .byte   %10111111 ; |# ######|            $dad3 G)
    .byte   %01111111 ; | #######|            $dad4 G)
    .byte   %11111111 ; |########|            $dad5 G)
    .byte   %10101010 ; |# # # # |            $dad6 G)
    .byte   %11111111 ; |########|            $dad7 G)
    .byte   %11111111 ; |########|            $dad8 G)
    .byte   %11111111 ; |########|            $dad9 G)
    .byte   %11111111 ; |########|            $dada G)
    .byte   %11111111 ; |########|            $dadb G)
    .byte   %11111111 ; |########|            $dadc G)
    .byte   %11111111 ; |########|            $dadd G)
    .byte   %11111111 ; |########|            $dade G)
    .byte   %11111111 ; |########|            $dadf G)
    .byte   %11111111 ; |########|            $dae0 G)

orbitalScreen4	
    .byte   %11111111 ; |########|            $dae1 G)
    .byte   %00000011 ; |      ##|            $dae2 G)
    .byte   %10101011 ; |# # # ##|            $dae3 G)
    .byte   %10101011 ; |# # # ##|            $dae4 G)
    .byte   %11111111 ; |########|            $dae5 G)
    .byte   %11111111 ; |########|            $dae6 G)
    .byte   %11111111 ; |########|            $dae7 G)
    .byte   %11111111 ; |########|            $dae8 G)
    .byte   %01111111 ; | #######|            $dae9 G)
    .byte   %10111111 ; |# ######|            $daea G)
    .byte   %11011111 ; |## #####|            $daeb G)
    .byte   %11101111 ; |### ####|            $daec G)
    .byte   %11110111 ; |#### ###|            $daed G)
    .byte   %11111011 ; |##### ##|            $daee G)
    .byte   %10101011 ; |# # # ##|            $daef G)
    .byte   %11111111 ; |########|            $daf0 G)
    .byte   %11111111 ; |########|            $daf1 G)
    .byte   %11111111 ; |########|            $daf2 G)
    .byte   %11111111 ; |########|            $daf3 G)
    .byte   %11110011 ; |####  ##|            $daf4 G)
    .byte   %11110111 ; |#### ###|            $daf5 G)
    .byte   %11110011 ; |####  ##|            $daf6 G)
    .byte   %11111011 ; |##### ##|            $daf7 G)
    .byte   %11110011 ; |####  ##|            $daf8 G)
    .byte   %11111111 ; |########|            $daf9 G)

    .byte   %00000101 ; |     # #|            $dafa G)
    .byte   %00000010 ; |      # |            $dafb G)
    .byte   %10000010 ; |#     # |            $dafc G)
    .byte   %10000010 ; |#     # |            $dafd G)
    .byte   %10000010 ; |#     # |            $dafe G)
    .byte   %00000010 ; |      # |            $daff G)

;======================
;Satellite dock screen
;======================
satScreen0
    .byte   %11111111 ; |########|            $db00 G)
    .byte   %11111110 ; |####### |            $db01 G)
    .byte   %11111111 ; |########|            $db02 G)
    .byte   %11111111 ; |########|            $db03 G)
    .byte   %11111111 ; |########|            $db04 G)
    .byte   %11111111 ; |########|            $db05 G)
    .byte   %11111111 ; |########|            $db06 G)
    .byte   %11111111 ; |########|            $db07 G)
    .byte   %11111111 ; |########|            $db08 G)
    .byte   %11111111 ; |########|            $db09 G)
    .byte   %11111111 ; |########|            $db0a G)
    .byte   %11111110 ; |####### |            $db0b G)
    .byte   %10000001 ; |#      #|            $db0c G)
    .byte   %11111110 ; |####### |            $db0d G)
    .byte   %11111111 ; |########|            $db0e G)
    .byte   %11111111 ; |########|            $db0f G)
    .byte   %11111111 ; |########|            $db10 G)
    .byte   %11111111 ; |########|            $db11 G)
    .byte   %11111111 ; |########|            $db12 G)
    .byte   %10011111 ; |#  #####|            $db13 G)
    .byte   %11011111 ; |## #####|            $db14 G)
    .byte   %10011111 ; |#  #####|            $db15 G)
    .byte   %11011111 ; |## #####|            $db16 G)
    .byte   %10011111 ; |#  #####|            $db17 G)
    .byte   %11111111 ; |########|            $db18 G)

SatScreen1
    .byte   %11111111 ; |########|            $db19 G)
    .byte   %00111111 ; |  ######|            $db1a G)
    .byte   %01111111 ; | #######|            $db1b G)
    .byte   %11111111 ; |########|            $db1c G)
    .byte   %11111111 ; |########|            $db1d G)
    .byte   %11111111 ; |########|            $db1e G)
    .byte   %11111111 ; |########|            $db1f G)
    .byte   %11111111 ; |########|            $db20 G)
    .byte   %11111111 ; |########|            $db21 G)
    .byte   %11111111 ; |########|            $db22 G)
    .byte   %11111111 ; |########|            $db23 G)
    .byte   %10111111 ; |# ######|            $db24 G)
    .byte   %11000000 ; |##      |            $db25 G)
    .byte   %10111111 ; |# ######|            $db26 G)
    .byte   %11111111 ; |########|            $db27 G)
    .byte   %11111111 ; |########|            $db28 G)
    .byte   %11111111 ; |########|            $db29 G)
    .byte   %11111111 ; |########|            $db2a G)
    .byte   %11111111 ; |########|            $db2b G)
    .byte   %11111111 ; |########|            $db2c G)
    .byte   %11111111 ; |########|            $db2d G)
    .byte   %11111111 ; |########|            $db2e G)
    .byte   %11111111 ; |########|            $db2f G)
    .byte   %11111111 ; |########|            $db30 G)
    .byte   %11111111 ; |########|            $db31 G)

satScreen2
    .byte   %11111111 ; |########|            $db32 G)
    .byte   %11111010 ; |##### # |            $db33 G)
    .byte   %10011010 ; |#  ## # |            $db34 G)
    .byte   %11111111 ; |########|            $db35 G)
    .byte   %10011111 ; |#  #####|            $db36 G)
    .byte   %11111111 ; |########|            $db37 G)
    .byte   %10011111 ; |#  #####|            $db38 G)
    .byte   %11111111 ; |########|            $db39 G)
    .byte   %10011111 ; |#  #####|            $db3a G)
    .byte   %11111111 ; |########|            $db3b G)
    .byte   %10011111 ; |#  #####|            $db3c G)
    .byte   %11111111 ; |########|            $db3d G)
    .byte   %10011111 ; |#  #####|            $db3e G)
    .byte   %11111111 ; |########|            $db3f G)
    .byte   %10011111 ; |#  #####|            $db40 G)
    .byte   %11111111 ; |########|            $db41 G)
    .byte   %10011111 ; |#  #####|            $db42 G)
    .byte   %11111111 ; |########|            $db43 G)
    .byte   %10011111 ; |#  #####|            $db44 G)
    .byte   %11111111 ; |########|            $db45 G)
    .byte   %10011111 ; |#  #####|            $db46 G)
    .byte   %11111111 ; |########|            $db47 G)
    .byte   %10010001 ; |#  #   #|            $db48 G)
    .byte   %11111111 ; |########|            $db49 G)
    .byte   %11111111 ; |########|            $db4a G)

satScreen3
    .byte   %11111111 ; |########|            $db4b G)
    .byte   %10101010 ; |# # # # |            $db4c G)
    .byte   %10101010 ; |# # # # |            $db4d G)
    .byte   %11111111 ; |########|            $db4e G)
    .byte   %11111111 ; |########|            $db4f G)
    .byte   %11111011 ; |##### ##|            $db50 G)
    .byte   %11111011 ; |##### ##|            $db51 G)
    .byte   %11111011 ; |##### ##|            $db52 G)
    .byte   %11111011 ; |##### ##|            $db53 G)
    .byte   %11111011 ; |##### ##|            $db54 G)
    .byte   %11111011 ; |##### ##|            $db55 G)
    .byte   %11110101 ; |#### # #|            $db56 G)
    .byte   %11111111 ; |########|            $db57 G)
    .byte   %11110101 ; |#### # #|            $db58 G)
    .byte   %11111011 ; |##### ##|            $db59 G)
    .byte   %11111011 ; |##### ##|            $db5a G)
    .byte   %11111011 ; |##### ##|            $db5b G)
    .byte   %11111011 ; |##### ##|            $db5c G)
    .byte   %11111011 ; |##### ##|            $db5d G)
    .byte   %11111011 ; |##### ##|            $db5e G)
    .byte   %11111111 ; |########|            $db5f G)
    .byte   %11111111 ; |########|            $db60 G)
    .byte   %11110001 ; |####   #|            $db61 G)
    .byte   %11110111 ; |#### ###|            $db62 G)
    .byte   %11111111 ; |########|            $db63 G)

satScreen4
    .byte   %11111111 ; |########|            $db64 G)
    .byte   %10101011 ; |# # # ##|            $db65 G)
    .byte   %10101011 ; |# # # ##|            $db66 G)
    .byte   %11111111 ; |########|            $db67 G)
    .byte   %11111111 ; |########|            $db68 G)
    .byte   %11111111 ; |########|            $db69 G)
    .byte   %11111111 ; |########|            $db6a G)
    .byte   %11111111 ; |########|            $db6b G)
    .byte   %11111111 ; |########|            $db6c G)
    .byte   %11111111 ; |########|            $db6d G)
    .byte   %11111111 ; |########|            $db6e G)
    .byte   %11111111 ; |########|            $db6f G)
    .byte   %11111111 ; |########|            $db70 G)
    .byte   %11111111 ; |########|            $db71 G)
    .byte   %11111111 ; |########|            $db72 G)
    .byte   %11111111 ; |########|            $db73 G)
    .byte   %11111111 ; |########|            $db74 G)
    .byte   %11111111 ; |########|            $db75 G)
    .byte   %11111111 ; |########|            $db76 G)
    .byte   %11111111 ; |########|            $db77 G)
    .byte   %11111111 ; |########|            $db78 G)
    .byte   %11111011 ; |##### ##|            $db79 G)
    .byte   %11110001 ; |####   #|            $db7a G)
    .byte   %11111011 ; |##### ##|            $db7b G)
    .byte   %11111111 ; |########|            $db7c G)

;======================
; Reentry screen
;======================
reentryScreen0
    .byte   %11011110 ; |## #### |            $db7d G)
    .byte   %11011110 ; |## #### |            $db7e G)
    .byte   %11111111 ; |########|            $db7f G)
    .byte   %11000000 ; |##      |            $db80 G)
    .byte   %11011110 ; |## #### |            $db81 G)
    .byte   %11011110 ; |## #### |            $db82 G)
    .byte   %11010101 ; |## # # #|            $db83 G)
    .byte   %11011111 ; |## #####|            $db84 G)
    .byte   %11011111 ; |## #####|            $db85 G)
    .byte   %11010101 ; |## # # #|            $db86 G)
    .byte   %11011110 ; |## #### |            $db87 G)
    .byte   %11011110 ; |## #### |            $db88 G)
    .byte   %11000000 ; |##      |            $db89 G)
    .byte   %11111111 ; |########|            $db8a G)
    .byte   %11111111 ; |########|            $db8b G)
    .byte   %11111111 ; |########|            $db8c G)
    .byte   %11111111 ; |########|            $db8d G)
    .byte   %11110001 ; |####   #|            $db8e G)
    .byte   %11111101 ; |###### #|            $db8f G)
    .byte   %11111111 ; |########|            $db90 G)
    .byte   %11111111 ; |########|            $db91 G)
    .byte   %11000000 ; |##      |            $db92 G)
    .byte   %11111111 ; |########|            $db93 G)
    .byte   %11111111 ; |########|            $db94 G)
    .byte   %11111111 ; |########|            $db95 G)

reentryScreen1
    .byte   %11110111 ; |#### ###|            $db96 G)
    .byte   %11110111 ; |#### ###|            $db97 G)
    .byte   %11111111 ; |########|            $db98 G)
    .byte   %00000111 ; |     ###|            $db99 G)
    .byte   %11110111 ; |#### ###|            $db9a G)
    .byte   %11110111 ; |#### ###|            $db9b G)
    .byte   %01010111 ; | # # ###|            $db9c G)
    .byte   %11110111 ; |#### ###|            $db9d G)
    .byte   %11110111 ; |#### ###|            $db9e G)
    .byte   %01010111 ; | # # ###|            $db9f G)
    .byte   %11110111 ; |#### ###|            $dba0 G)
    .byte   %11110111 ; |#### ###|            $dba1 G)
    .byte   %00000111 ; |     ###|            $dba2 G)
    .byte   %11111111 ; |########|            $dba3 G)
    .byte   %11111111 ; |########|            $dba4 G)
    .byte   %11111111 ; |########|            $dba5 G)
    .byte   %11010111 ; |## # ###|            $dba6 G)
    .byte   %11101111 ; |### ####|            $dba7 G)
    .byte   %11010111 ; |## # ###|            $dba8 G)
    .byte   %11111110 ; |####### |            $dba9 G)
    .byte   %11100000 ; |###     |            $dbaa G)
    .byte   %00001111 ; |    ####|            $dbab G)
    .byte   %11111111 ; |########|            $dbac G)
    .byte   %11111111 ; |########|            $dbad G)
    .byte   %11111111 ; |########|            $dbae G)

reentryScreen2
    .byte   %10111101 ; |# #### #|            $dbaf G)
    .byte   %10111101 ; |# #### #|            $dbb0 G)
    .byte   %11111111 ; |########|            $dbb1 G)
    .byte   %11111111 ; |########|            $dbb2 G)
    .byte   %11111111 ; |########|            $dbb3 G)
    .byte   %11111111 ; |########|            $dbb4 G)
    .byte   %11111111 ; |########|            $dbb5 G)
    .byte   %11111111 ; |########|            $dbb6 G)
    .byte   %11111111 ; |########|            $dbb7 G)
    .byte   %11111111 ; |########|            $dbb8 G)
    .byte   %11111111 ; |########|            $dbb9 G)
    .byte   %11111110 ; |####### |            $dbba G)
    .byte   %11111101 ; |###### #|            $dbbb G)
    .byte   %11111101 ; |###### #|            $dbbc G)
    .byte   %11111110 ; |####### |            $dbbd G)
    .byte   %11111111 ; |########|            $dbbe G)
    .byte   %11111111 ; |########|            $dbbf G)
    .byte   %11111110 ; |####### |            $dbc0 G)
    .byte   %11000001 ; |##     #|            $dbc1 G)
    .byte   %00011111 ; |   #####|            $dbc2 G)
    .byte   %11111111 ; |########|            $dbc3 G)
    .byte   %10001111 ; |#   ####|            $dbc4 G)
    .byte   %10111111 ; |# ######|            $dbc5 G)
    .byte   %11111111 ; |########|            $dbc6 G)
    .byte   %11111111 ; |########|            $dbc7 G)

reentryScreen3	
    .byte   %11101111 ; |### ####|            $dbc8 G)
    .byte   %11101111 ; |### ####|            $dbc9 G)
    .byte   %11111011 ; |##### ##|            $dbca G)
    .byte   %10011100 ; |#  ###  |            $dbcb G)
    .byte   %10111111 ; |# ######|            $dbcc G)
    .byte   %10111111 ; |# ######|            $dbcd G)
    .byte   %10111111 ; |# ######|            $dbce G)
    .byte   %11111111 ; |########|            $dbcf G)
    .byte   %11111100 ; |######  |            $dbd0 G)
    .byte   %11110011 ; |####  ##|            $dbd1 G)
    .byte   %11001111 ; |##  ####|            $dbd2 G)
    .byte   %00111111 ; |  ######|            $dbd3 G)
    .byte   %11111111 ; |########|            $dbd4 G)
    .byte   %11111111 ; |########|            $dbd5 G)
    .byte   %00111101 ; |  #### #|            $dbd6 G)
    .byte   %11011101 ; |## ### #|            $dbd7 G)
    .byte   %11011101 ; |## ### #|            $dbd8 G)
    .byte   %00111000 ; |  ###   |            $dbd9 G)
    .byte   %11111111 ; |########|            $dbda G)
    .byte   %11111111 ; |########|            $dbdb G)
    .byte   %11111111 ; |########|            $dbdc G)
    .byte   %11111111 ; |########|            $dbdd G)
    .byte   %11111111 ; |########|            $dbde G)
    .byte   %11111111 ; |########|            $dbdf G)
    .byte   %11111111 ; |########|            $dbe0 G)

reentryScreen4	
    .byte   %01111011 ; | #### ##|            $dbe1 G)
    .byte   %01111011 ; | #### ##|            $dbe2 G)
    .byte   %11111111 ; |########|            $dbe3 G)
    .byte   %11110011 ; |####  ##|            $dbe4 G)
    .byte   %00111011 ; |  ### ##|            $dbe5 G)
    .byte   %11011011 ; |## ## ##|            $dbe6 G)
    .byte   %11010011 ; |## #  ##|            $dbe7 G)
    .byte   %00111011 ; |  ### ##|            $dbe8 G)
    .byte   %11111011 ; |##### ##|            $dbe9 G)
    .byte   %11110011 ; |####  ##|            $dbea G)
    .byte   %11111011 ; |##### ##|            $dbeb G)
    .byte   %11111011 ; |##### ##|            $dbec G)
    .byte   %11110011 ; |####  ##|            $dbed G)
    .byte   %11111011 ; |##### ##|            $dbee G)
    .byte   %11111011 ; |##### ##|            $dbef G)
    .byte   %11110011 ; |####  ##|            $dbf0 G)
    .byte   %11111011 ; |##### ##|            $dbf1 G)
    .byte   %11111011 ; |##### ##|            $dbf2 G)
    .byte   %11110011 ; |####  ##|            $dbf3 G)
    .byte   %11111011 ; |##### ##|            $dbf4 G)
    .byte   %11111011 ; |##### ##|            $dbf5 G)
    .byte   %11110011 ; |####  ##|            $dbf6 G)
    .byte   %11111111 ; |########|            $dbf7 G)
    .byte   %11111111 ; |########|            $dbf8 G)
    .byte   %11111111 ; |########|            $dbf9 G)


    .byte   %10011001 ; |#  ##  #|            $dbfa G)
    .byte   %10011001 ; |#  ##  #|            $dbfb G)
    .byte   %00111100 ; |  ####  |            $dbfc G)
    .byte   %00111100 ; |  ####  |            $dbfd G)
    .byte   %01111110 ; | ###### |            $dbfe G)
    .byte   %01111110 ; | ###### |            $dbff G)

;======================
; Landing Screen
;======================
landingScreen0	
    .byte   %11111111 ; |########|            $dc00 G)
    .byte   %11111111 ; |########|            $dc01 G) 
    .byte   %10000000 ; |#       |            $dc02 G)
    .byte   %11111111 ; |########|            $dc03 G)
    .byte   %11111111 ; |########|            $dc04 G)
    .byte   %11100111 ; |###  ###|            $dc05 G)
    .byte   %11111001 ; |#####  #|            $dc06 G)
    .byte   %10111110 ; |# ##### |            $dc07 G)
    .byte   %11001111 ; |##  ####|            $dc08 G)
    .byte   %11110011 ; |####  ##|            $dc09 G)
    .byte   %11111101 ; |###### #|            $dc0a G)
    .byte   %11111110 ; |####### |            $dc0b G)
    .byte   %11111111 ; |########|            $dc0c G)
    .byte   %11111111 ; |########|            $dc0d G)
    .byte   %11111111 ; |########|            $dc0e G)
    .byte   %11111111 ; |########|            $dc0f G)
    .byte   %11111111 ; |########|            $dc10 G)
    .byte   %11111111 ; |########|            $dc11 G)
    .byte   %11111111 ; |########|            $dc12 G)
    .byte   %11001110 ; |##  ### |            $dc13 G)
    .byte   %11101110 ; |### ### |            $dc14 G)
    .byte   %11001010 ; |##  # # |            $dc15 G)
    .byte   %11011110 ; |## #### |            $dc16 G)
    .byte   %11001110 ; |##  ### |            $dc17 G)
    .byte   %11111111 ; |########|            $dc18 G)

LandingScreen1
    .byte   %11111111 ; |########|            $dc19 G)
    .byte   %11111111 ; |########|            $dc1a G)
    .byte   %00000000 ; |        |            $dc1b G)
    .byte   %11111111 ; |########|            $dc1c G)
    .byte   %11111111 ; |########|            $dc1d G)
    .byte   %11111111 ; |########|            $dc1e G)
    .byte   %11111111 ; |########|            $dc1f G)
    .byte   %11111111 ; |########|            $dc20 G)
    .byte   %01111111 ; | #######|            $dc21 G)
    .byte   %10111111 ; |# ######|            $dc22 G)
    .byte   %11011111 ; |## #####|            $dc23 G)
    .byte   %11101111 ; |### ####|            $dc24 G)
    .byte   %01101111 ; | ## ####|            $dc25 G)
    .byte   %10110111 ; |# ## ###|            $dc26 G)
    .byte   %10110111 ; |# ## ###|            $dc27 G)
    .byte   %11011011 ; |## ## ##|            $dc28 G)
    .byte   %11011011 ; |## ## ##|            $dc29 G)
    .byte   %11101101 ; |### ## #|            $dc2a G)
    .byte   %11101110 ; |### ### |            $dc2b G)
    .byte   %11110111 ; |#### ###|            $dc2c G)
    .byte   %11110111 ; |#### ###|            $dc2d G)
    .byte   %01111011 ; | #### ##|            $dc2e G)
    .byte   %11111100 ; |######  |            $dc2f G)
    .byte   %01111111 ; | #######|            $dc30 G)
    .byte   %11111111 ; |########|            $dc31 G)

LandingScreen2
    .byte   %11111111 ; |########|            $dc32 G)
    .byte   %11100101 ; |###  # #|            $dc33 G)
    .byte   %00111101 ; |  #### #|            $dc34 G)
    .byte   %11111111 ; |########|            $dc35 G)
    .byte   %11100101 ; |###  # #|            $dc36 G)
    .byte   %11111110 ; |####### |            $dc37 G)
    .byte   %11111111 ; |########|            $dc38 G)
    .byte   %11100111 ; |###  ###|            $dc39 G)
    .byte   %11111111 ; |########|            $dc3a G)
    .byte   %11111111 ; |########|            $dc3b G)
    .byte   %11100111 ; |###  ###|            $dc3c G)
    .byte   %11111111 ; |########|            $dc3d G)
    .byte   %11111111 ; |########|            $dc3e G)
    .byte   %11100111 ; |###  ###|            $dc3f G)
    .byte   %11111111 ; |########|            $dc40 G)
    .byte   %11111111 ; |########|            $dc41 G)
    .byte   %11100111 ; |###  ###|            $dc42 G)
    .byte   %11111111 ; |########|            $dc43 G)
    .byte   %11111111 ; |########|            $dc44 G)
    .byte   %00100111 ; |  #  ###|            $dc45 G)
    .byte   %11111111 ; |########|            $dc46 G)
    .byte   %11111111 ; |########|            $dc47 G)
    .byte   %00100100 ; |  #  #  |            $dc48 G)
    .byte   %11111111 ; |########|            $dc49 G)
    .byte   %11111111 ; |########|            $dc4a G)

LandingScreen3
    .byte   %11111111 ; |########|            $dc4b G)
    .byte   %01010101 ; | # # # #|            $dc4c G)
    .byte   %01010101 ; | # # # #|            $dc4d G)
    .byte   %11111111 ; |########|            $dc4e G)
    .byte   %11111101 ; |###### #|            $dc4f G)
    .byte   %11111111 ; |########|            $dc50 G)
    .byte   %01111101 ; | ##### #|            $dc51 G)
    .byte   %01111111 ; | #######|            $dc52 G)
    .byte   %10111101 ; |# #### #|            $dc53 G)
    .byte   %10111111 ; |# ######|            $dc54 G)
    .byte   %10111101 ; |# #### #|            $dc55 G)
    .byte   %11011111 ; |## #####|            $dc56 G)
    .byte   %11011101 ; |## ### #|            $dc57 G)
    .byte   %11011111 ; |## #####|            $dc58 G)
    .byte   %11101101 ; |### ## #|            $dc59 G)
    .byte   %11101111 ; |### ####|            $dc5a G)
    .byte   %11101101 ; |### ## #|            $dc5b G)
    .byte   %11110111 ; |#### ###|            $dc5c G)
    .byte   %11110101 ; |#### # #|            $dc5d G)
    .byte   %11110111 ; |#### ###|            $dc5e G)
    .byte   %11111010 ; |##### # |            $dc5f G)
    .byte   %11111010 ; |##### # |            $dc60 G)
    .byte   %01111010 ; | #### # |            $dc61 G)
    .byte   %11111111 ; |########|            $dc62 G)
    .byte   %11111111 ; |########|            $dc63 G)
LandingScreen4	
    .byte   %11111111 ; |########|            $dc64 G)
    .byte   %01010101 ; | # # # #|            $dc65 G)
    .byte   %01010101 ; | # # # #|            $dc66 G)
    .byte   %11111111 ; |########|            $dc67 G)
    .byte   %11111101 ; |###### #|            $dc68 G)
    .byte   %11111011 ; |##### ##|            $dc69 G)
    .byte   %11110111 ; |#### ###|            $dc6a G)
    .byte   %11110111 ; |#### ###|            $dc6b G)
    .byte   %11101111 ; |### ####|            $dc6c G)
    .byte   %11101111 ; |### ####|            $dc6d G)
    .byte   %11101111 ; |### ####|            $dc6e G)
    .byte   %11011111 ; |## #####|            $dc6f G)
    .byte   %11011111 ; |## #####|            $dc70 G)
    .byte   %11011111 ; |## #####|            $dc71 G)
    .byte   %10111111 ; |# ######|            $dc72 G)
    .byte   %10111111 ; |# ######|            $dc73 G)
    .byte   %10111111 ; |# ######|            $dc74 G)
    .byte   %01111111 ; | #######|            $dc75 G)
    .byte   %01111111 ; | #######|            $dc76 G)
    .byte   %01111111 ; | #######|            $dc77 G)
    .byte   %11111111 ; |########|            $dc78 G)
    .byte   %11111011 ; |##### ##|            $dc79 G)
    .byte   %11110001 ; |####   #|            $dc7a G)
    .byte   %11111011 ; |##### ##|            $dc7b G)
    .byte   %11111111 ; |########|            $dc7c G)

;======================
; STS 101 Screen
;======================
stsScreen0
    .byte   %11111111 ; |########|            $dc7d G)
    .byte   %11111111 ; |########|            $dc7e G)
    .byte   %11111111 ; |########|            $dc7f G)
    .byte   %11111111 ; |########|            $dc80 G)
    .byte   %11111111 ; |########|            $dc81 G)
    .byte   %11111111 ; |########|            $dc82 G)
    .byte   %11111111 ; |########|            $dc83 G)
    .byte   %11111111 ; |########|            $dc84 G)
    .byte   %11111111 ; |########|            $dc85 G)
    .byte   %11111111 ; |########|            $dc86 G)
    .byte   %11111111 ; |########|            $dc87 G)
    .byte   %11111111 ; |########|            $dc88 G)
    .byte   %11111111 ; |########|            $dc89 G)
    .byte   %11111111 ; |########|            $dc8a G)
    .byte   %11111111 ; |########|            $dc8b G)
    .byte   %11111111 ; |########|            $dc8c G)
    .byte   %11111111 ; |########|            $dc8d G)
    .byte   %11111111 ; |########|            $dc8e G)
    .byte   %10001101 ; |#   ## #|            $dc8f G)
    .byte   %11101101 ; |### ## #|            $dc90 G)
    .byte   %10001101 ; |#   ## #|            $dc91 G)
    .byte   %10111101 ; |# #### #|            $dc92 G)
    .byte   %10001000 ; |#   #   |            $dc93 G)
    .byte   %11111111 ; |########|            $dc94 G)
    .byte   %11111111 ; |########|            $dc95 G)

stsScreen1 
    .byte   %11111111 ; |########|            $dc96 G)
    .byte   %00000000 ; |        |            $dc97 G)
    .byte   %01111111 ; | #######|            $dc98 G)
    .byte   %10011111 ; |#  #####|            $dc99 G)
    .byte   %10101111 ; |# # ####|            $dc9a G)
    .byte   %11010111 ; |## # ###|            $dc9b G)
    .byte   %11011011 ; |## ## ##|            $dc9c G)
    .byte   %11101101 ; |### ## #|            $dc9d G)
    .byte   %11101110 ; |### ### |            $dc9e G)
    .byte   %11110111 ; |#### ###|            $dc9f G)
    .byte   %11110111 ; |#### ###|            $dca0 G)
    .byte   %11111011 ; |##### ##|            $dca1 G)
    .byte   %11111011 ; |##### ##|            $dca2 G)
    .byte   %11111101 ; |###### #|            $dca3 G)
    .byte   %11111101 ; |###### #|            $dca4 G)
    .byte   %11111110 ; |####### |            $dca5 G)
    .byte   %11111110 ; |####### |            $dca6 G)
    .byte   %11111111 ; |########|            $dca7 G)
    .byte   %10001111 ; |#   ####|            $dca8 G)
    .byte   %11101111 ; |### ####|            $dca9 G)
    .byte   %10001111 ; |#   ####|            $dcaa G)
    .byte   %10111111 ; |# ######|            $dcab G)
    .byte   %10001111 ; |#   ####|            $dcac G)
    .byte   %11111111 ; |########|            $dcad G)
    .byte   %11111111 ; |########|            $dcae G)

stsScreen2
    .byte   %11111111 ; |########|            $dcaf G)
    .byte   %00000000 ; |        |            $dcb0 G)
    .byte   %11111111 ; |########|            $dcb1 G)
    .byte   %11111111 ; |########|            $dcb2 G)
    .byte   %01010101 ; | # # # #|            $dcb3 G)
    .byte   %01101101 ; | ## ## #|            $dcb4 G)
    .byte   %10101011 ; |# # # ##|            $dcb5 G)
    .byte   %10111011 ; |# ### ##|            $dcb6 G)
    .byte   %10111010 ; |# ### # |            $dcb7 G)
    .byte   %00010001 ; |   #   #|            $dcb8 G)
    .byte   %10010011 ; |#  #  ##|            $dcb9 G)
    .byte   %10010011 ; |#  #  ##|            $dcba G)
    .byte   %10010011 ; |#  #  ##|            $dcbb G)
    .byte   %10010011 ; |#  #  ##|            $dcbc G)
    .byte   %10010011 ; |#  #  ##|            $dcbd G)
    .byte   %11010110 ; |## # ## |            $dcbe G)
    .byte   %11010110 ; |## # ## |            $dcbf G)
    .byte   %01010101 ; | # # # #|            $dcc0 G)
    .byte   %01101101 ; | ## ## #|            $dcc1 G)
    .byte   %10111011 ; |# ### ##|            $dcc2 G)
    .byte   %10111011 ; |# ### ##|            $dcc3 G)
    .byte   %11010111 ; |## # ###|            $dcc4 G)
    .byte   %11010111 ; |## # ###|            $dcc5 G)
    .byte   %11101111 ; |### ####|            $dcc6 G)
    .byte   %11101111 ; |### ####|            $dcc7 G)

stsScreen3	
    .byte   %11111111 ; |########|            $dcc8 G)
    .byte   %00000001 ; |       #|            $dcc9 G)
    .byte   %11111101 ; |###### #|            $dcca G)
    .byte   %11110011 ; |####  ##|            $dccb G)
    .byte   %11101011 ; |### # ##|            $dccc G)
    .byte   %11010111 ; |## # ###|            $dccd G)
    .byte   %10110111 ; |# ## ###|            $dcce G)
    .byte   %01101111 ; | ## ####|            $dccf G)
    .byte   %11101111 ; |### ####|            $dcd0 G)
    .byte   %11011111 ; |## #####|            $dcd1 G)
    .byte   %11011111 ; |## #####|            $dcd2 G)
    .byte   %10111111 ; |# ######|            $dcd3 G)
    .byte   %10111111 ; |# ######|            $dcd4 G)
    .byte   %01111111 ; | #######|            $dcd5 G)
    .byte   %01111111 ; | #######|            $dcd6 G)
    .byte   %11111111 ; |########|            $dcd7 G)
    .byte   %11111111 ; |########|            $dcd8 G)
    .byte   %11111111 ; |########|            $dcd9 G)
    .byte   %11110001 ; |####   #|            $dcda G)
    .byte   %11111011 ; |##### ##|            $dcdb G)
    .byte   %11111011 ; |##### ##|            $dcdc G)
    .byte   %11111011 ; |##### ##|            $dcdd G)
    .byte   %11110011 ; |####  ##|            $dcde G)
    .byte   %11111111 ; |########|            $dcdf G)
    .byte   %11111111 ; |########|            $dce0 G)

stsScreen4	
    .byte   %11111111 ; |########|            $dce1 G)
    .byte   %11111111 ; |########|            $dce2 G)
    .byte   %11111111 ; |########|            $dce3 G)
    .byte   %11111111 ; |########|            $dce4 G)
    .byte   %11111111 ; |########|            $dce5 G)
    .byte   %11111111 ; |########|            $dce6 G)
    .byte   %11111111 ; |########|            $dce7 G)
    .byte   %11111111 ; |########|            $dce8 G)
    .byte   %11111111 ; |########|            $dce9 G)
    .byte   %11111111 ; |########|            $dcea G)
    .byte   %11111111 ; |########|            $dceb G)
    .byte   %11111111 ; |########|            $dcec G)
    .byte   %11111111 ; |########|            $dced G)
    .byte   %11111111 ; |########|            $dcee G)
    .byte   %11111111 ; |########|            $dcef G)
    .byte   %11111111 ; |########|            $dcf0 G)
    .byte   %11111111 ; |########|            $dcf1 G)
    .byte   %11111111 ; |########|            $dcf2 G)
    .byte   %10110001 ; |# ##   #|            $dcf3 G)
    .byte   %01011011 ; | # ## ##|            $dcf4 G)
    .byte   %01011011 ; | # ## ##|            $dcf5 G)
    .byte   %01011011 ; | # ## ##|            $dcf6 G)
    .byte   %10110011 ; |# ##  ##|            $dcf7 G)
    .byte   %11111111 ; |########|            $dcf8 G)
    .byte   %11111111 ; |########|            $dcf9 G)




    .byte   %10010000 ; |#  #    |            $dcfa G)
    .byte   %11010000 ; |## #    |            $dcfb G)
    .byte   %11110000 ; |####    |            $dcfc G)
    .byte   %11111111 ; |########|            $dcfd G)
    .byte   %11111111 ; |########|            $dcfe G)
    .byte   %11111111 ; |########|            $dcff G)
    .byte   %11111111 ; |########|            $dd00 G)
    .byte   %11110111 ; |#### ###|            $dd01 G)
    .byte   %11110111 ; |#### ###|            $dd02 G)
    .byte   %11110011 ; |####  ##|            $dd03 G)
    .byte   %11110101 ; |#### # #|            $dd04 G)
    .byte   %11110011 ; |####  ##|            $dd05 G)
    .byte   %11111111 ; |########|            $dd06 G)
    .byte   %11110111 ; |#### ###|            $dd07 G)
    .byte   %11110111 ; |#### ###|            $dd08 G)
    .byte   %11110101 ; |#### # #|            $dd09 G)
    .byte   %11110010 ; |####  # |            $dd0a G)
    .byte   %11110111 ; |#### ###|            $dd0b G)
    .byte   %11111111 ; |########|            $dd0c G)
    .byte   %11110111 ; |#### ###|            $dd0d G)
    .byte   %11110111 ; |#### ###|            $dd0e G)
    .byte   %11110011 ; |####  ##|            $dd0f G)
    .byte   %11110101 ; |#### # #|            $dd10 G)
    .byte   %11110011 ; |####  ##|            $dd11 G)
    .byte   %11111111 ; |########|            $dd12 G)
    .byte   %10101010 ; |# # # # |            $dd13 G)
    .byte   %10101010 ; |# # # # |            $dd14 G)
    .byte   %10011000 ; |#  ##   |            $dd15 G)
    .byte   %10101010 ; |# # # # |            $dd16 G)
    .byte   %10011101 ; |#  ### #|            $dd17 G)
    .byte   %11111111 ; |########|            $dd18 G)
    .byte   %11111111 ; |########|            $dd19 G)
    .byte   %01011011 ; | # ## ##|            $dd1a G)
    .byte   %01011011 ; | # ## ##|            $dd1b G)
    .byte   %00011011 ; |   ## ##|            $dd1c G)
    .byte   %01010101 ; | # # # #|            $dd1d G)
    .byte   %10110101 ; |# ## # #|            $dd1e G)
    .byte   %11111111 ; |########|            $dd1f G)
    .byte   %01010001 ; | # #   #|            $dd20 G)
    .byte   %01011101 ; | # ### #|            $dd21 G)
    .byte   %01010001 ; | # #   #|            $dd22 G)
    .byte   %01010111 ; | # # ###|            $dd23 G)
    .byte   %01010001 ; | # #   #|            $dd24 G)
    .byte   %11111111 ; |########|            $dd25 G)
    .byte   %01000110 ; | #   ## |            $dd26 G)
    .byte   %01011101 ; | # ### #|            $dd27 G)
    .byte   %01011101 ; | # ### #|            $dd28 G)
    .byte   %01011101 ; | # ### #|            $dd29 G)
    .byte   %01011110 ; | # #### |            $dd2a G)
    .byte   %11111111 ; |########|            $dd2b G)
    .byte   %11011010 ; |## ## # |            $dd2c G)
    .byte   %11011010 ; |## ## # |            $dd2d G)
    .byte   %11011010 ; |## ## # |            $dd2e G)
    .byte   %11011010 ; |## ## # |            $dd2f G)
    .byte   %10001010 ; |#   # # |            $dd30 G)
    .byte   %11111111 ; |########|            $dd31 G)
    .byte   %11111111 ; |########|            $dd32 G)
    .byte   %00110110 ; |  ## ## |            $dd33 G)
    .byte   %01101010 ; | ## # # |            $dd34 G)
    .byte   %01101010 ; | ## # # |            $dd35 G)
    .byte   %01101010 ; | ## # # |            $dd36 G)
    .byte   %01110111 ; | ### ###|            $dd37 G)
    .byte   %11111111 ; |########|            $dd38 G)
    .byte   %00010110 ; |   # ## |            $dd39 G)
    .byte   %11010101 ; |## # # #|            $dd3a G)
    .byte   %00010101 ; |   # # #|            $dd3b G)
    .byte   %01110101 ; | ### # #|            $dd3c G)
    .byte   %00010110 ; |   # ## |            $dd3d G)
    .byte   %11111111 ; |########|            $dd3e G)
    .byte   %11101111 ; |### ####|            $dd3f G)
    .byte   %01101111 ; | ## ####|            $dd40 G)
    .byte   %01101111 ; | ## ####|            $dd41 G)
    .byte   %01101111 ; | ## ####|            $dd42 G)
    .byte   %11000111 ; |##   ###|            $dd43 G)
    .byte   %11111111 ; |########|            $dd44 G)
    .byte   %11010001 ; |## #   #|            $dd45 G)
    .byte   %11010101 ; |## # # #|            $dd46 G)
    .byte   %10010101 ; |#  # # #|            $dd47 G)
    .byte   %01010111 ; | # # ###|            $dd48 G)
    .byte   %11010001 ; |## #   #|            $dd49 G)
    .byte   %11111111 ; |########|            $dd4a G)
    .byte   %11111111 ; |########|            $dd4b G)
    .byte   %10100111 ; |# #  ###|            $dd4c G)
    .byte   %10101011 ; |# # # ##|            $dd4d G)
    .byte   %00101011 ; |  # # ##|            $dd4e G)
    .byte   %10101011 ; |# # # ##|            $dd4f G)
    .byte   %01100111 ; | ##  ###|            $dd50 G)
    .byte   %11111111 ; |########|            $dd51 G)
    .byte   %11011011 ; |## ## ##|            $dd52 G)
    .byte   %01011011 ; | # ## ##|            $dd53 G)
    .byte   %01010011 ; | # #  ##|            $dd54 G)
    .byte   %01001011 ; | #  # ##|            $dd55 G)
    .byte   %11011011 ; |## ## ##|            $dd56 G)
    .byte   %11111111 ; |########|            $dd57 G)
    .byte   %11111111 ; |########|            $dd58 G)
    .byte   %11111111 ; |########|            $dd59 G)
    .byte   %11111111 ; |########|            $dd5a G)
    .byte   %11111111 ; |########|            $dd5b G)
    .byte   %11111111 ; |########|            $dd5c G)
    .byte   %11111111 ; |########|            $dd5d G)
    .byte   %11111111 ; |########|            $dd5e G)
    .byte   %01111111 ; | #######|            $dd5f G)
    .byte   %11111111 ; |########|            $dd60 G)
    .byte   %01111111 ; | #######|            $dd61 G)
    .byte   %11111111 ; |########|            $dd62 G)
    .byte   %11111111 ; |########|            $dd63 G)
    .byte   %11111111 ; |########|            $dd64 G)
    .byte   %00010111 ; |   # ###|            $dd65 G)
    .byte   %11010111 ; |## # ###|            $dd66 G)
    .byte   %00010011 ; |   #  ##|            $dd67 G)
    .byte   %01110101 ; | ### # #|            $dd68 G)
    .byte   %00010011 ; |   #  ##|            $dd69 G)
    .byte   %11111111 ; |########|            $dd6a G)
    .byte   %00010111 ; |   # ###|            $dd6b G)
    .byte   %11010111 ; |## # ###|            $dd6c G)
    .byte   %00010011 ; |   #  ##|            $dd6d G)
    .byte   %01110101 ; | ### # #|            $dd6e G)
    .byte   %00010011 ; |   #  ##|            $dd6f G)
    .byte   %11111111 ; |########|            $dd70 G)
    .byte   %11111111 ; |########|            $dd71 G)
    .byte   %11111111 ; |########|            $dd72 G)
    .byte   %11111111 ; |########|            $dd73 G)
    .byte   %11111111 ; |########|            $dd74 G)
    .byte   %11111111 ; |########|            $dd75 G)
    .byte   %11111111 ; |########|            $dd76 G)
    .byte   %11111111 ; |########|            $dd77 G)
    .byte   %11111111 ; |########|            $dd78 G)
    .byte   %11111111 ; |########|            $dd79 G)
    .byte   %11111111 ; |########|            $dd7a G)
    .byte   %11111111 ; |########|            $dd7b G)
    .byte   %11111111 ; |########|            $dd7c G)

unknownScreen0	
    .byte   %11111111 ; |########|            $dd7d G)
    .byte   %11111111 ; |########|            $dd7e G)
    .byte   %11111111 ; |########|            $dd7f G)
    .byte   %11111111 ; |########|            $dd80 G)
    .byte   %11111111 ; |########|            $dd81 G)
    .byte   %11111111 ; |########|            $dd82 G)
    .byte   %11111111 ; |########|            $dd83 G)
    .byte   %11111111 ; |########|            $dd84 G)
    .byte   %11111111 ; |########|            $dd85 G)
    .byte   %11111111 ; |########|            $dd86 G)
    .byte   %11111111 ; |########|            $dd87 G)
    .byte   %11111110 ; |####### |            $dd88 G)
    .byte   %11111101 ; |###### #|            $dd89 G)
    .byte   %11111011 ; |##### ##|            $dd8a G)
    .byte   %11110111 ; |#### ###|            $dd8b G)
    .byte   %11101111 ; |### ####|            $dd8c G)
    .byte   %11101111 ; |### ####|            $dd8d G)
    .byte   %11011111 ; |## #####|            $dd8e G)
    .byte   %11011110 ; |## #### |            $dd8f G)
    .byte   %10111101 ; |# #### #|            $dd90 G)
    .byte   %10110000 ; |# ##    |            $dd91 G)
    .byte   %10001111 ; |#   ####|            $dd92 G)
    .byte   %11111111 ; |########|            $dd93 G)
    .byte   %11111111 ; |########|            $dd94 G)
    .byte   %11111111 ; |########|            $dd95 G)

unknownScreen1
    .byte   %11111111 ; |########|            $dd96 G)
    .byte   %11111111 ; |########|            $dd97 G)
    .byte   %11111111 ; |########|            $dd98 G)
    .byte   %11111111 ; |########|            $dd99 G)
    .byte   %11111111 ; |########|            $dd9a G)
    .byte   %11111100 ; |######  |            $dd9b G)
    .byte   %11111010 ; |##### # |            $dd9c G)
    .byte   %11110110 ; |#### ## |            $dd9d G)
    .byte   %11101110 ; |### ### |            $dd9e G)
    .byte   %10011110 ; |#  #### |            $dd9f G)
    .byte   %01111110 ; | ###### |            $dda0 G)
    .byte   %11111000 ; |#####   |            $dda1 G)
    .byte   %11110110 ; |#### ## |            $dda2 G)
    .byte   %11101110 ; |### ### |            $dda3 G)
    .byte   %11011100 ; |## ###  |            $dda4 G)
    .byte   %11011010 ; |## ## # |            $dda5 G)
    .byte   %11101010 ; |### # # |            $dda6 G)
    .byte   %00101011 ; |  # # ##|            $dda7 G)
    .byte   %11011101 ; |## ### #|            $dda8 G)
    .byte   %11111101 ; |###### #|            $dda9 G)
    .byte   %00001101 ; |    ## #|            $ddaa G)
    .byte   %11110011 ; |####  ##|            $ddab G)
    .byte   %11111111 ; |########|            $ddac G)
    .byte   %11111111 ; |########|            $ddad G)
    .byte   %11111111 ; |########|            $ddae G)

unknownScreen2
    .byte   %11111111 ; |########|            $ddaf G)
    .byte   %11111111 ; |########|            $ddb0 G)
    .byte   %11101111 ; |### ####|            $ddb1 G)
    .byte   %11010111 ; |## # ###|            $ddb2 G)
    .byte   %00111001 ; |  ###  #|            $ddb3 G)
    .byte   %11101110 ; |### ### |            $ddb4 G)
    .byte   %10101010 ; |# # # # |            $ddb5 G)
    .byte   %10101010 ; |# # # # |            $ddb6 G)
    .byte   %10101010 ; |# # # # |            $ddb7 G)
    .byte   %10101010 ; |# # # # |            $ddb8 G)
    .byte   %10101010 ; |# # # # |            $ddb9 G)
    .byte   %10101010 ; |# # # # |            $ddba G)
    .byte   %10101010 ; |# # # # |            $ddbb G)
    .byte   %11111110 ; |####### |            $ddbc G)
    .byte   %10101010 ; |# # # # |            $ddbd G)
    .byte   %11111110 ; |####### |            $ddbe G)
    .byte   %10010010 ; |#  #  # |            $ddbf G)
    .byte   %01101101 ; | ## ## #|            $ddc0 G)
    .byte   %11111111 ; |########|            $ddc1 G)
    .byte   %11111111 ; |########|            $ddc2 G)
    .byte   %11111111 ; |########|            $ddc3 G)
    .byte   %11111111 ; |########|            $ddc4 G)
    .byte   %11111111 ; |########|            $ddc5 G)
    .byte   %11111111 ; |########|            $ddc6 G)
    .byte   %11111111 ; |########|            $ddc7 G)

unknownScreen3
    .byte   %11111111 ; |########|            $ddc8 G)
    .byte   %11111111 ; |########|            $ddc9 G)
    .byte   %11111111 ; |########|            $ddca G)
    .byte   %11111111 ; |########|            $ddcb G)
    .byte   %11111111 ; |########|            $ddcc G)
    .byte   %01111111 ; | #######|            $ddcd G)
    .byte   %10111111 ; |# ######|            $ddce G)
    .byte   %11011111 ; |## #####|            $ddcf G)
    .byte   %11101111 ; |### ####|            $ddd0 G)
    .byte   %11110011 ; |####  ##|            $ddd1 G)
    .byte   %11111100 ; |######  |            $ddd2 G)
    .byte   %00111111 ; |  ######|            $ddd3 G)
    .byte   %11011111 ; |## #####|            $ddd4 G)
    .byte   %11101111 ; |### ####|            $ddd5 G)
    .byte   %01110111 ; | ### ###|            $ddd6 G)
    .byte   %10110111 ; |# ## ###|            $ddd7 G)
    .byte   %10101111 ; |# # ####|            $ddd8 G)
    .byte   %10101000 ; |# # #   |            $ddd9 G)
    .byte   %01110111 ; | ### ###|            $ddda G)
    .byte   %01111111 ; | #######|            $dddb G)
    .byte   %01100000 ; | ##     |            $dddc G)
    .byte   %10011111 ; |#  #####|            $dddd G)
    .byte   %11111111 ; |########|            $ddde G)
    .byte   %11111111 ; |########|            $dddf G)
    .byte   %11111111 ; |########|            $dde0 G)

unknownScreen4
    .byte   %11111111 ; |########|            $dde1 G)
    .byte   %11111111 ; |########|            $dde2 G)
    .byte   %11111111 ; |########|            $dde3 G)
    .byte   %11111111 ; |########|            $dde4 G)
    .byte   %11111111 ; |########|            $dde5 G)
    .byte   %11111111 ; |########|            $dde6 G)
    .byte   %11111111 ; |########|            $dde7 G)
    .byte   %11111111 ; |########|            $dde8 G)
    .byte   %11111111 ; |########|            $dde9 G)
    .byte   %11111111 ; |########|            $ddea G)
    .byte   %11111111 ; |########|            $ddeb G)
    .byte   %01111111 ; | #######|            $ddec G)
    .byte   %10111111 ; |# ######|            $dded G)
    .byte   %11011111 ; |## #####|            $ddee G)
    .byte   %11101111 ; |### ####|            $ddef G)
    .byte   %11110111 ; |#### ###|            $ddf0 G)
    .byte   %11110111 ; |#### ###|            $ddf1 G)
    .byte   %11111011 ; |##### ##|            $ddf2 G)
    .byte   %01111011 ; | #### ##|            $ddf3 G)
    .byte   %10111101 ; |# #### #|            $ddf4 G)
    .byte   %00001101 ; |    ## #|            $ddf5 G)
    .byte   %11110001 ; |####   #|            $ddf6 G)
    .byte   %11111111 ; |########|            $ddf7 G)
    .byte   %11111111 ; |########|            $ddf8 G)
    .byte   %11111111 ; |########|            $ddf9 G)



    .byte   %01001010 ; | #  # # |            $ddfa G)
    .byte   %01001010 ; | #  # # |            $ddfb G)
    .byte   %01001010 ; | #  # # |            $ddfc G)
    .byte   %01001010 ; | #  # # |            $ddfd G)
    .byte   %01001010 ; | #  # # |            $ddfe G)
    .byte   %01100000 ; | ##     |            $ddff G)

numberSprites
zeroSprite
    .byte   %00011000 ; |   ##   |            $de00 G)
    .byte   %00100100 ; |  #  #  |            $de01 G)
    .byte   %00100100 ; |  #  #  |            $de02 G)
    .byte   %00000000 ; |        |            $de03 G)
    .byte   %00100100 ; |  #  #  |            $de04 G)
    .byte   %00100100 ; |  #  #  |            $de05 G)
    .byte   %00011000 ; |   ##   |            $de06 G)
    .byte   %00000000 ; |        |            $de07 G)
oneSprite
    .byte   %00000100 ; |     #  |            $de08 G)
    .byte   %00000100 ; |     #  |            $de09 G)
    .byte   %00000100 ; |     #  |            $de0a G)
    .byte   %00000000 ; |        |            $de0b G)
    .byte   %00000100 ; |     #  |            $de0c G)
    .byte   %00000100 ; |     #  |            $de0d G)
    .byte   %00000100 ; |     #  |            $de0e G)
    .byte   %00000000 ; |        |            $de0f G)
twoSprite
    .byte   %00011000 ; |   ##   |            $de10 G)
    .byte   %00100000 ; |  #     |            $de11 G)
    .byte   %00100000 ; |  #     |            $de12 G)
    .byte   %00011000 ; |   ##   |            $de13 G)
    .byte   %00000100 ; |     #  |            $de14 G)
    .byte   %00000100 ; |     #  |            $de15 G)
    .byte   %00011000 ; |   ##   |            $de16 G)
    .byte   %00000000 ; |        |            $de17 G)
threeSprite
    .byte   %00011000 ; |   ##   |            $de18 G)
    .byte   %00000100 ; |     #  |            $de19 G)
    .byte   %00000100 ; |     #  |            $de1a G)
    .byte   %00011000 ; |   ##   |            $de1b G)
    .byte   %00000100 ; |     #  |            $de1c G)
    .byte   %00000100 ; |     #  |            $de1d G)
    .byte   %00011000 ; |   ##   |            $de1e G)
fourSprite
    .byte   %00000000 ; |        |            $de1f G)
    .byte   %00000100 ; |     #  |            $de20 G)
    .byte   %00000100 ; |     #  |            $de21 G)
    .byte   %00000100 ; |     #  |            $de22 G)
    .byte   %00011000 ; |   ##   |            $de23 G)
    .byte   %00100100 ; |  #  #  |            $de24 G)
    .byte   %00100100 ; |  #  #  |            $de25 G)
    .byte   %00100100 ; |  #  #  |            $de26 G)
fiveSprite
    .byte   %00000000 ; |        |            $de27 G)
    .byte   %00011000 ; |   ##   |            $de28 G)
    .byte   %00000100 ; |     #  |            $de29 G)
    .byte   %00000100 ; |     #  |            $de2a G)
    .byte   %00011000 ; |   ##   |            $de2b G)
    .byte   %00100000 ; |  #     |            $de2c G)
    .byte   %00100000 ; |  #     |            $de2d G)
    .byte   %00011000 ; |   ##   |            $de2e G)
sixSprite	
    .byte   %00000000 ; |        |            $de2f G)
    .byte   %00011000 ; |   ##   |            $de30 G)
    .byte   %00100100 ; |  #  #  |            $de31 G)
    .byte   %00100100 ; |  #  #  |            $de32 G)
    .byte   %00011000 ; |   ##   |            $de33 G)
    .byte   %00100000 ; |  #     |            $de34 G)
    .byte   %00100000 ; |  #     |            $de35 G)
    .byte   %00011000 ; |   ##   |            $de36 G)
sevenSprite
    .byte   %00000000 ; |        |            $de37 G)
    .byte   %00000100 ; |     #  |            $de38 G)
    .byte   %00000100 ; |     #  |            $de39 G)
    .byte   %00000100 ; |     #  |            $de3a G)
    .byte   %00000000 ; |        |            $de3b G)
    .byte   %00100100 ; |  #  #  |            $de3c G)
    .byte   %00100100 ; |  #  #  |            $de3d G)
    .byte   %00011000 ; |   ##   |            $de3e G)
eightSprite	
    .byte   %00000000 ; |        |            $de3f G)
    .byte   %00011000 ; |   ##   |            $de40 G)
    .byte   %00100100 ; |  #  #  |            $de41 G)
    .byte   %00100100 ; |  #  #  |            $de42 G)
    .byte   %00011000 ; |   ##   |            $de43 G)
    .byte   %00100100 ; |  #  #  |            $de44 G)
    .byte   %00100100 ; |  #  #  |            $de45 G)
    .byte   %00011000 ; |   ##   |            $de46 G)
nineSprite	
    .byte   %00000000 ; |        |            $de47 G)
    .byte   %00011000 ; |   ##   |            $de48 G)
    .byte   %00000100 ; |     #  |            $de49 G)
    .byte   %00000100 ; |     #  |            $de4a G)
    .byte   %00011000 ; |   ##   |            $de4b G)
    .byte   %00100100 ; |  #  #  |            $de4c G)
    .byte   %00100100 ; |  #  #  |            $de4d G)
    .byte   %00011000 ; |   ##   |            $de4e G)
    .byte   %00000000 ; |        |            $de4f G)


    .byte   %00000000 ; |        |            $de50 G)
    .byte   %00000000 ; |        |            $de51 G)
    .byte   %00000000 ; |        |            $de52 G)
    .byte   %00011000 ; |   ##   |            $de53 G)
    .byte   %00000000 ; |        |            $de54 G)
    .byte   %00000000 ; |        |            $de55 G)
    .byte   %00000000 ; |        |            $de56 G)
    .byte   %00000000 ; |        |            $de57 G)
    .byte   %00000000 ; |        |            $de58 G)
    .byte   %00000000 ; |        |            $de59 G)
    .byte   %00000000 ; |        |            $de5a G)

displayTextSprites
fuelSprite0
    .byte   %11111111 ; |########|            $de5b G)
    .byte   %10110001 ; |# ##   #|            $de5c G)
    .byte   %10110101 ; |# ## # #|            $de5d G)
    .byte   %10010101 ; |#  # # #|            $de5e G)
    .byte   %10110101 ; |# ## # #|            $de5f G)
    .byte   %10010101 ; |#  # # #|            $de60 G)
fuelSprite1	
    .byte   %11111111 ; |########|            $de61 G)
    .byte   %00100111 ; |  #  ###|            $de62 G)
    .byte   %01101101 ; | ## ## #|            $de63 G)
    .byte   %00101111 ; |  # ####|            $de64 G)
    .byte   %01101101 ; | ## ## #|            $de65 G)
    .byte   %00101111 ; |  # ####|            $de66 G)

metSprite0	
    .byte   %11111111 ; |########|            $de67 G)
    .byte   %10111010 ; |# ### # |            $de68 G)
    .byte   %10111010 ; |# ### # |            $de69 G)
    .byte   %10101010 ; |# # # # |            $de6a G)
    .byte   %10010010 ; |#  #  # |            $de6b G)
    .byte   %10111010 ; |# ### # |            $de6c G)
metSprite1
    .byte   %11111111 ; |########|            $de6d G)
    .byte   %00110111 ; |  ## ###|            $de6e G)
    .byte   %11110111 ; |#### ###|            $de6f G)
    .byte   %01110111 ; | ### ###|            $de70 G)
    .byte   %11110111 ; |#### ###|            $de71 G)
    .byte   %00100011 ; |  #   ##|            $de72 G)

altSprite0	
    .byte   %11111111 ; |########|            $de73 G)
    .byte   %11010110 ; |## # ## |            $de74 G)
    .byte   %11010110 ; |## # ## |            $de75 G)
    .byte   %11000110 ; |##   ## |            $de76 G)
    .byte   %11010110 ; |## # ## |            $de77 G)
    .byte   %11101110 ; |### ### |            $de78 G)
altSprite1
    .byte   %11111111 ; |########|            $de79 G)
    .byte   %00110111 ; |  ## ###|            $de7a G)
    .byte   %11110111 ; |#### ###|            $de7b G)
    .byte   %11110111 ; |#### ###|            $de7c G)
    .byte   %11110111 ; |#### ###|            $de7d G)
    .byte   %11100011 ; |###   ##|            $de7e G)

sppermeterSprite0	
    .byte   %11111111 ; |########|            $de7f G)
    .byte   %10010110 ; |#  # ## |            $de80 G)
    .byte   %11010111 ; |## # ###|            $de81 G)
    .byte   %10010001 ; |#  #   #|            $de82 G)
    .byte   %10110101 ; |# ## # #|            $de83 G)
    .byte   %10010001 ; |#  #   #|            $de84 G)
sppermeterSprite1	
    .byte   %11111111 ; |########|            $de85 G)
    .byte   %11010101 ; |## # # #|            $de86 G)
    .byte   %01010101 ; | # # # #|            $de87 G)
    .byte   %01101011 ; | ## # ##|            $de88 G)
    .byte   %01111111 ; | #######|            $de89 G)
    .byte   %10111111 ; |# ######|            $de8a G)

statSprite0	
    .byte   %11111111 ; |########|            $de8b G)
    .byte   %10011011 ; |#  ## ##|            $de8c G)
    .byte   %11011011 ; |## ## ##|            $de8d G)
    .byte   %10011011 ; |#  ## ##|            $de8e G)
    .byte   %10111011 ; |# ### ##|            $de8f G)
    .byte   %10010001 ; |#  #   #|            $de90 G)
statSprite1	
    .byte   %11111111 ; |########|            $de91 G)
    .byte   %01011011 ; | # ## ##|            $de92 G)
    .byte   %01011011 ; | # ## ##|            $de93 G)
    .byte   %00011011 ; |   ## ##|            $de94 G)
    .byte   %01011011 ; | # ## ##|            $de95 G)
    .byte   %00010001 ; |   #   #|            $de96 G)

xminusAxSprite0	
    .byte   %11111111 ; |########|            $de97 G)
    .byte   %10101111 ; |# # ####|            $de98 G)
    .byte   %10101111 ; |# # ####|            $de99 G)
    .byte   %11011001 ; |## ##  #|            $de9a G)
    .byte   %10101111 ; |# # ####|            $de9b G)
    .byte   %10101111 ; |# # ####|            $de9c G)
xminusAxSprite1	
    .byte   %11111111 ; |########|            $de9d G)
    .byte   %01010101 ; | # # # #|            $de9e G)
    .byte   %01011011 ; | # ## ##|            $de9f G)
    .byte   %00010101 ; |   # # #|            $dea0 G)
    .byte   %01011111 ; | # #####|            $dea1 G)
    .byte   %10111111 ; |# ######|            $dea2 G)

yminusAxSprite0	
    .byte   %11111111 ; |########|            $dea3 G)
    .byte   %11011111 ; |## #####|            $dea4 G)
    .byte   %11011111 ; |## #####|            $dea5 G)
    .byte   %11011001 ; |## ##  #|            $dea6 G)
    .byte   %10101111 ; |# # ####|            $dea7 G)
    .byte   %10101111 ; |# # ####|            $dea8 G)
yminusAxSprite1	
    .byte   %11111111 ; |########|            $dea9 G)
    .byte   %01010101 ; | # # # #|            $deaa G)
    .byte   %01011011 ; | # ## ##|            $deab G)
    .byte   %00010101 ; |   # # #|            $deac G)
    .byte   %01011111 ; | # #####|            $dead G)
    .byte   %10111111 ; |# ######|            $deae G)

zminusAxSprite0	
    .byte   %11111111 ; |########|            $deaf G)
    .byte   %10001111 ; |#   ####|            $deb0 G)
    .byte   %10111111 ; |# ######|            $deb1 G)
    .byte   %11011001 ; |## ##  #|            $deb2 G)
    .byte   %11101111 ; |### ####|            $deb3 G)
    .byte   %10001111 ; |#   ####|            $deb4 G)
zminusAxSprite1	
    .byte   %11111111 ; |########|            $deb5 G)
    .byte   %01010101 ; | # # # #|            $deb6 G)
    .byte   %01011011 ; | # ## ##|            $deb7 G)
    .byte   %00010101 ; |   # # #|            $deb8 G)
    .byte   %01011111 ; | # #####|            $deb9 G)
    .byte   %10111111 ; |# ######|            $deba G)

pitDegSprite0
    .byte   %11111111 ; |########|            $debb G)
    .byte   %10111011 ; |# ### ##|            $debc G)
    .byte   %10111011 ; |# ### ##|            $debd G)
    .byte   %10001011 ; |#   # ##|            $debe G)
    .byte   %10101011 ; |# # # ##|            $debf G)
    .byte   %10001010 ; |#   # # |            $dec0 G)
pitDegSprite1	
    .byte   %11111111 ; |########|            $dec1 G)
    .byte   %01111111 ; | #######|            $dec2 G)
    .byte   %01111111 ; | #######|            $dec3 G)
    .byte   %01110001 ; | ###   #|            $dec4 G)
    .byte   %01110101 ; | ### # #|            $dec5 G)
    .byte   %00110001 ; |  ##   #|            $dec6 G)

yawSprite0
    .byte   %11111111 ; |########|            $dec7 G)
    .byte   %11011010 ; |## ## # |            $dec8 G)
    .byte   %11011010 ; |## ## # |            $dec9 G)
    .byte   %11011000 ; |## ##   |            $deca G)
    .byte   %10101010 ; |# # # # |            $decb G)
    .byte   %10101000 ; |# # #   |            $decc G)
yawSprite1
    .byte   %11111111 ; |########|            $decd G)
;-----------------------------------------------------------
;      Graphic Data: Number & Sprite Fonts
;-----------------------------------------------------------

largeNumberSprite0	
    .byte   %11010111 ; |## # ###|            $dece G)
    .byte   %10101011 ; |# # # ##|            $decf G)
    .byte   %10101011 ; |# # # ##|            $ded0 G)
    .byte   %10111011 ; |# ### ##|            $ded1 G)
    .byte   %10111011 ; |# ### ##|            $ded2 G)

rngSprite0
    .byte   %11111111 ; |########|            $ded3 G)
    .byte   %10110101 ; |# ## # #|            $ded4 G)
    .byte   %10101101 ; |# # ## #|            $ded5 G)
    .byte   %10001101 ; |#   ## #|            $ded6 G)
    .byte   %10110100 ; |# ## #  |            $ded7 G)
    .byte   %10001101 ; |#   ## #|            $ded8 G)
rngSprite1	
    .byte   %11111111 ; |########|            $ded9 G)
    .byte   %10100001 ; |# #    #|            $deda G)
    .byte   %10101101 ; |# # ## #|            $dedb G)
    .byte   %00101001 ; |  # #  #|            $dedc G)
    .byte   %10101111 ; |# # ####|            $dedd G)
    .byte   %10100001 ; |# #    #|            $dede G)

fltNumberSprite0	
    .byte   %11111111 ; |########|            $dedf G)
    .byte   %10110010 ; |# ##  # |            $dee0 G)
    .byte   %10110110 ; |# ## ## |            $dee1 G)
    .byte   %10010110 ; |#  # ## |            $dee2 G)
    .byte   %10110110 ; |# ## ## |            $dee3 G)
    .byte   %10010100 ; |#  # #  |            $dee4 G)
fltNumberSprite1	
    .byte   %11111111 ; |########|            $dee5 G)
    .byte   %11101011 ; |### # ##|            $dee6 G)
    .byte   %11000001 ; |##     #|            $dee7 G)
    .byte   %11101011 ; |### # ##|            $dee8 G)
    .byte   %11000001 ; |##     #|            $dee9 G)
    .byte   %01101011 ; | ## # ##|            $deea G)
    .byte   %11111111 ; |########|            $deeb G)
	
	
	.byte	$0e,$0f,$10,$11,$12,$13 ; $deea *)
    .byte   $13,$13,$13,$13,$13,$12,$11,$10 ; $def2 *)
    .byte   $0f,$0e,$0d,$0c,$0b,$0a,$09,$08 ; $defa *)
    .byte   $07,$07,$07,$07,$08,$09,$0a,$0b ; $df02 *)
    .byte   $0c,$19

messageTextSpritea
launchSprite0	
    .byte   %00000000 ; |        |            $df0c G)
    .byte   %00110101 ; |  ## # #|            $df0d G)
    .byte   %00100101 ; |  #  # #|            $df0e G)
    .byte   %00100111 ; |  #  ###|            $df0f G)
    .byte   %00100101 ; |  #  # #|            $df10 G)
    .byte   %00100111 ; |  #  ###|            $df11 G)
    .byte   %00000000 ; |        |            $df12 G)

launchSprite1
    .byte   %00000000 ; |        |            $df13 G)
    .byte   %01110100 ; | ### #  |            $df14 G)
    .byte   %01010100 ; | # # #  |            $df15 G)
    .byte   %01010101 ; | # # # #|            $df16 G)
    .byte   %01010110 ; | # # ## |            $df17 G)
    .byte   %01010100 ; | # # #  |            $df18 G)
    .byte   %00000000 ; |        |            $df19 G)

launchSprite2	
    .byte   %00000000 ; |        |            $df1a G)
    .byte   %10110101 ; |# ## # #|            $df1b G)
    .byte   %10100101 ; |# #  # #|            $df1c G)
    .byte   %10100111 ; |# #  ###|            $df1d G)
    .byte   %10100101 ; |# #  # #|            $df1e G)
    .byte   %10110101 ; |# ## # #|            $df1f G)
    .byte   %00000000 ; |        |            $df20 G)

scrubSprite0	

    .byte   %00000000 ; |        |            $df21 G)
    .byte   %00011011 ; |   ## ##|            $df22 G)
    .byte   %00001010 ; |    # # |            $df23 G)
    .byte   %00011010 ; |   ## # |            $df24 G)
    .byte   %00010010 ; |   #  # |            $df25 G)
    .byte   %00011011 ; |   ## ##|            $df26 G)
    .byte   %00000000 ; |        |            $df27 G)
scrubSprite1	
    .byte   %00000000 ; |        |            $df28 G)
    .byte   %01010111 ; | # # ###|            $df29 G)
    .byte   %01010101 ; | # # # #|            $df2a G)
    .byte   %01100101 ; | ##  # #|            $df2b G)
    .byte   %01010101 ; | # # # #|            $df2c G)
    .byte   %01100101 ; | ##  # #|            $df2d G)
    .byte   %00000000 ; |        |            $df2e G)
scrubSprite2
    .byte   %00000000 ; |        |            $df2f G)
    .byte   %01100000 ; | ##     |            $df30 G)
    .byte   %01010000 ; | # #    |            $df31 G)
    .byte   %01100000 ; | ##     |            $df32 G)
    .byte   %01010000 ; | # #    |            $df33 G)
    .byte   %01100000 ; | ##     |            $df34 G)

welcomeHomeSprite0
    .byte   %00000000 ; |        |            $df35 G)
    .byte   %10001011 ; |#   # ##|            $df36 G)
    .byte   %11011010 ; |## ## # |            $df37 G)
    .byte   %10101011 ; |# # # ##|            $df38 G)
    .byte   %10001010 ; |#   # # |            $df39 G)
    .byte   %10001011 ; |#   # ##|            $df3a G)
    .byte   %00000000 ; |        |            $df3b G)

welcomeHomeSprite1	
    .byte   %00000000 ; |        |            $df3c G)
    .byte   %01101101 ; | ## ## #|            $df3d G)
    .byte   %01001001 ; | #  #  #|            $df3e G)
    .byte   %01001001 ; | #  #  #|            $df3f G)
    .byte   %01001001 ; | #  #  #|            $df40 G)
    .byte   %01001101 ; | #  ## #|            $df41 G)
    .byte   %00000000 ; |        |            $df42 G)

welcomeHomeSprite2	
    .byte   %00000000 ; |        |            $df43 G)
    .byte   %11010001 ; |## #   #|            $df44 G)
    .byte   %01010001 ; | # #   #|            $df45 G)
    .byte   %01010101 ; | # # # #|            $df46 G)
    .byte   %01011011 ; | # ## ##|            $df47 G)
    .byte   %11010001 ; |## #   #|            $df48 G)
    .byte   %00000000 ; |        |            $df49 G)

welcomeHomeSprite3	
    .byte   %00000000 ; |        |            $df4a G)
    .byte   %01100010 ; | ##   # |            $df4b G)
    .byte   %01000010 ; | #    # |            $df4c G)
    .byte   %01100011 ; | ##   ##|            $df4d G)
    .byte   %01000010 ; | #    # |            $df4e G)
    .byte   %01100010 ; | ##   # |            $df4f G)
    .byte   %00000000 ; |        |            $df50 G)

welcomeHomeSprite4
    .byte   %00000000 ; |        |            $df51 G)
    .byte   %10111010 ; |# ### # |            $df52 G)
    .byte   %10101010 ; |# # # # |            $df53 G)
    .byte   %10101010 ; |# # # # |            $df54 G)
    .byte   %10101011 ; |# # # ##|            $df55 G)
    .byte   %10111010 ; |# ### # |            $df56 G)
    .byte   %00000000 ; |        |            $df57 G)

welcomeHomeSprite5
    .byte   %00000000 ; |        |            $df58 G)
    .byte   %00101100 ; |  # ##  |            $df59 G)
    .byte   %00101000 ; |  # #   |            $df5a G)
    .byte   %10101100 ; |# # ##  |            $df5b G)
    .byte   %01101000 ; | ## #   |            $df5c G)
    .byte   %00101100 ; |  # ##  |            $df5d G)

rendezvousSprite0
    .byte   %00000000 ; |        |            $df5e G)
    .byte   %00001010 ; |    # # |            $df5f G)
    .byte   %00001010 ; |    # # |            $df60 G)
    .byte   %00001100 ; |    ##  |            $df61 G)
    .byte   %00001010 ; |    # # |            $df62 G)
    .byte   %00001100 ; |    ##  |            $df63 G)
    .byte   %00000000 ; |        |            $df64 G)

rendezvousSprite1
    .byte   %00000000 ; |        |            $df65 G)
    .byte   %11010010 ; |## #  # |            $df66 G)
    .byte   %10010010 ; |#  #  # |            $df67 G)
    .byte   %11010110 ; |## # ## |            $df68 G)
    .byte   %10011010 ; |#  ## # |            $df69 G)
    .byte   %11010010 ; |## #  # |            $df6a G)
    .byte   %00000000 ; |        |            $df6b G)

rendezvousSprite2
    .byte   %00000000 ; |        |            $df6c G)
    .byte   %11001101 ; |##  ## #|            $df6d G)
    .byte   %10101001 ; |# # #  #|            $df6e G)
    .byte   %10101100 ; |# # ##  |            $df6f G)
    .byte   %10101000 ; |# # #   |            $df70 G)
    .byte   %11001101 ; |##  ## #|            $df71 G)
    .byte   %00000000 ; |        |            $df72 G)

rendezvousSprite3
    .byte   %00000000 ; |        |            $df73 G)
    .byte   %11001001 ; |##  #  #|            $df74 G)
    .byte   %00010101 ; |   # # #|            $df75 G)
    .byte   %10010101 ; |#  # # #|            $df76 G)
    .byte   %01010101 ; | # # # #|            $df77 G)
    .byte   %11010101 ; |## # # #|            $df78 G)
    .byte   %00000000 ; |        |            $df79 G)

rendezvousSprite4
    .byte   %00000000 ; |        |            $df7a G)
    .byte   %11011101 ; |## ### #|            $df7b G)
    .byte   %01010100 ; | # # #  |            $df7c G)
    .byte   %01010101 ; | # # # #|            $df7d G)
    .byte   %01010101 ; | # # # #|            $df7e G)
    .byte   %11010101 ; |## # # #|            $df7f G)
    .byte   %00000000 ; |        |            $df80 G)

rendezvousSprite5
    .byte   %00000000 ; |        |            $df81 G)
    .byte   %11000000 ; |##      |            $df82 G)
    .byte   %01000000 ; | #      |            $df83 G)
    .byte   %11000000 ; |##      |            $df84 G)
    .byte   %00000000 ; |        |            $df85 G)
    .byte   %11000000 ; |##      |            $df86 G)

missionAbortSprite0	
    .byte   %00000000 ; |        |            $df87 G)
    .byte   %10001010 ; |#   # # |            $df88 G)
    .byte   %10001010 ; |#   # # |            $df89 G)
    .byte   %10101010 ; |# # # # |            $df8a G)
    .byte   %11011010 ; |## ## # |            $df8b G)
    .byte   %10001010 ; |#   # # |            $df8c G)
    .byte   %00000000 ; |        |            $df8d G)

missionAbortSprite1	
    .byte   %00000000 ; |        |            $df8e G)
    .byte   %11101110 ; |### ### |            $df8f G)
    .byte   %00100010 ; |  #   # |            $df90 G)
    .byte   %11101110 ; |### ### |            $df91 G)
    .byte   %10001000 ; |#   #   |            $df92 G)
    .byte   %11101110 ; |### ### |            $df93 G)
    .byte   %00000000 ; |        |            $df94 G)

missionAbortSprite2
    .byte   %00000000 ; |        |            $df95 G)
    .byte   %10111010 ; |# ### # |            $df96 G)
    .byte   %10101010 ; |# # # # |            $df97 G)
    .byte   %10101010 ; |# # # # |            $df98 G)
    .byte   %10101011 ; |# # # ##|            $df99 G)
    .byte   %10111010 ; |# ### # |            $df9a G)
    .byte   %00000000 ; |        |            $df9b G)

missionAbortSprite3	
    .byte   %00000000 ; |        |            $df9c G)
    .byte   %01000101 ; | #   # #|            $df9d G)
    .byte   %01000101 ; | #   # #|            $df9e G)
    .byte   %11000111 ; |##   ###|            $df9f G)
    .byte   %01000101 ; | #   # #|            $dfa0 G)
    .byte   %01000111 ; | #   ###|            $dfa1 G)
    .byte   %00000000 ; |        |            $dfa2 G)

missionAbortSprite4	
    .byte   %00000000 ; |        |            $dfa3 G)
    .byte   %01100111 ; | ##  ###|            $dfa4 G)
    .byte   %01010101 ; | # # # #|            $dfa5 G)
    .byte   %01100101 ; | ##  # #|            $dfa6 G)
    .byte   %01010101 ; | # # # #|            $dfa7 G)
    .byte   %01100111 ; | ##  ###|            $dfa8 G)
    .byte   %00000000 ; |        |            $dfa9 G)

missionAbortSprite5
    .byte   %00000000 ; |        |            $dfaa G)
    .byte   %01010010 ; | # #  # |            $dfab G)
    .byte   %01010010 ; | # #  # |            $dfac G)
    .byte   %01100010 ; | ##   # |            $dfad G)
    .byte   %01010010 ; | # #  # |            $dfae G)
    .byte   %01100111 ; | ##  ###|            $dfaf G)


    .byte   %00000000 ; |        |            $dfb0 G)
    .byte   %00000011 ; |      ##|            $dfb1 G)
    .byte   %00000010 ; |      # |            $dfb2 G)
    .byte   %00000001 ; |       #|            $dfb3 G)

nasaLogo  
    .byte   %00000000 ; |        |            $dfb4 G)
    .byte   %00000000 ; |        |            $dfb5 G)
    .byte   %00000000 ; |        |            $dfb6 G)
    .byte   %00000000 ; |        |            $dfb7 G)
    .byte   %00000000 ; |        |            $dfb8 G)
    .byte   %00000000 ; |        |            $dfb9 G)
    .byte   %00000000 ; |        |            $dfba G)
    .byte   %00001001 ; |    #  #|            $dfbb G)
    .byte   %00001010 ; |    # # |            $dfbc G)
    .byte   %00001010 ; |    # # |            $dfbd G)
    .byte   %00001010 ; |    # # |            $dfbe G)
    .byte   %00001010 ; |    # # |            $dfbf G)
    .byte   %00001010 ; |    # # |            $dfc0 G)
    .byte   %00000100 ; |     #  |            $dfc1 G)
    .byte   %00100011 ; |  #   ##|            $dfc2 G)
    .byte   %10100010 ; |# #   # |            $dfc3 G)
    .byte   %10100010 ; |# #   # |            $dfc4 G)
    .byte   %10010100 ; |#  # #  |            $dfc5 G)
    .byte   %10010100 ; |#  # #  |            $dfc6 G)
    .byte   %10010100 ; |#  # #  |            $dfc7 G)
    .byte   %10001000 ; |#   #   |            $dfc8 G)
    .byte   %11100100 ; |###  #  |            $dfc9 G)
    .byte   %00010100 ; |   # #  |            $dfca G)
    .byte   %00010100 ; |   # #  |            $dfcb G)
    .byte   %01100010 ; | ##   # |            $dfcc G)
    .byte   %10000010 ; |#     # |            $dfcd G)
    .byte   %10000010 ; |#     # |            $dfce G)
    .byte   %01110001 ; | ###   #|            $dfcf G)
    .byte   %01000000 ; | #      |            $dfd0 G)
    .byte   %01000000 ; | #      |            $dfd1 G)
    .byte   %01000000 ; | #      |            $dfd2 G)
    .byte   %10000000 ; |#       |            $dfd3 G)
    .byte   %10000000 ; |#       |            $dfd4 G)
    .byte   %10000000 ; |#       |            $dfd5 G)
    .byte   %00000000 ; |        |            $dfd6 G)
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$ce ; $dfd7 D)
    .byte   $ae,$8e,$6e,$4e,$2e,$0e,$0e     ; $dfdf *)
    .byte   $26                             ; $dfe6 D)
    .byte   $32,$3d,$43,$46,$48,$4a,$4c     ; $dfe7 *)

thrustPointer    
    .byte   %11111110 ; |####### |            $dfee G)
    .byte   %10010010 ; |#  #  # |            $dfef G)
    .byte   %10010010 ; |#  #  # |            $dff0 G)
    .byte   %00010000 ; |   #    |            $dff1 G)
    .byte   %10010010 ; |#  #  # |            $dff2 G)
    .byte   %01111100 ; | #####  |            $dff3 G)
    .byte   %00111000 ; |  ###   |            $dff4 G)
    .byte   %10010010 ; |#  #  # |            $dff5 G)
    
    .byte   $00,$00,$00                     ; $dff6 *)
BANK1STROBE
    .byte   $00                             ; $dff9 D)
    .byte   $00,$00
	.byte	$00,$d0                 
    .byte   $00,$00                         ;



; Bank split here
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################
;####################################################################


;***********************************************************
;      Bank 1 / 0..1
;***********************************************************

    SEG     CODE
    ORG     $1000
    RORG    $f000

resetBank1
    bit     bank0Strobe
startBank1Init
    cld
    ldx     #$ff
    txs
    inx
    txa
;-----------------------------------------------------------
;      Vertical Blank / Logic (Bank 1)
;-----------------------------------------------------------
logicStart
    sta     VSYNC,x
    inx
    bne     logicStart
    jsr     Lfe6e
Lf011
    ldy     #$29
    sty     TIM64T
    lda     rngSeed
    asl
    asl
    asl
    eor     rngSeed
    asl
    rol     rngSeed
    inc     ram_C1
    lda     ram_C1
    tay
    and     #$3f
    bne     Lf054
    lda     ram_B5
    bne     Lf054
    lda     ram_AE
    beq     Lf054
    inc     ram_AE
    cmp     #$09
    bne     Lf054
    sta     ram_AE
    lda     ram_B6
    bne     Lf054
    bit     ram_F9
    bmi     Lf04a
    ldx     #$09
    lda     ram_C6
    lsr
    and     #$64
    bne     Lf050
Lf04a
    dec     ram_B6
    sta     currentScreenId
    ldx     #$07
Lf050
    sta     ram_9E
    stx     ram_85
Lf054
    lda     ram_F5
    jsr     Lfcae
    tya
    and     #$0f
    bne     Lf076
    inc     ram_E6
    lda     ram_B6
    beq     Lf076
    clc
    lda     #$01
    ldx     ram_B4
    beq     Lf06d
    adc     #$06
Lf06d
    ldx     ram_A1
    beq     Lf073
    adc     #$02
Lf073
    jsr     Lfcae
Lf076
    bit     ram_F9
    bpl     Lf084
    lda     ram_A5
    cmp     #$d2
    bne     Lf0db
    lda     #$00
    beq     Lf08a
Lf084
    lda     ram_C6
    and     #$48
    bne     Lf0db
Lf08a
    sta     ram_B4
    ldx     ram_B6
    beq     Lf0db
    ldx     ram_B5
    cpx     #$01
    bne     Lf0db
    stx     ram_AB
    sta     ram_B1
    inc     ram_B5
    sta     ram_CA
    sta     ram_E6
    ldx     #$95
    lda     ram_A6
    cmp     #$d2
    bcc     Lf0b8
    lda     #$00
    bit     ram_F9
    bmi     Lf0c3
    ldx     #$90
    lda     #$21
    sbc     ram_AA
    cmp     #$08
    bcc     Lf0bd
Lf0b8
    jsr     Lfcc7
    bcs     Lf0db
Lf0bd
    asl
    asl
    asl
    asl
    adc     #$40
Lf0c3
    sta     ram_B0
    bit     ram_F7
    bpl     Lf0cb
    lda     #$10
Lf0cb
    bit     ram_F9
    bmi     Lf0d5
    lda     ram_B2
    asl
    asl
    asl
    asl
Lf0d5
    sta     ram_B2
    inc     ram_ED
    inc     currentScreenId
Lf0db
    lda     ram_B4
    beq     Lf0e8
    tya
    and     #$02
    bne     Lf150
    lda     ram_BC
Lf0e6
    beq     Lf0fd
Lf0e8
    ldx     targetAuxX
    lsr
    bcs     Lf0f5
Lf0ed
    dec     targetX
    dec     targetAuxX
    cpx     #$0d
    bne     Lf0fd
Lf0f5
    inc     targetX
    inc     targetAuxX
    cpx     #$81
    beq     Lf0ed
Lf0fd
    lda     ram_B6
    beq     Lf126
    lda     ram_B4
    beq     Lf126
    bit     ram_F9
    bpl     Lf10f
    lda     ram_BC
    beq     Lf14c
    bpl     Lf123
Lf10f
    bit     INPT4
    bmi     Lf14c
    ldx     ram_A9
    bne     Lf121
    inx
    lda     targetX
    cmp     crosshairX
    bcs     Lf11f
    inx
Lf11f
    stx     ram_A9
Lf121
    lda     ram_A9
Lf123
    lsr
    bcs     Lf129
Lf126
    dec     crosshairX
    .byte   $2c ;bit                ;4-5 =   4
Lf129
    inc     crosshairX
    lda     crosshairX
    cmp     #$0f
    bcc     Lf129
    cmp     #$8c
    beq     Lf126
    bit     INPT4
    bmi     Lf150
    lda     ram_B4
    bne     Lf150
    lda     ram_B6
    beq     Lf150
    lda     ram_B5
    bne     Lf150
    bit     ram_F9
    bmi     Lf150
    jmp     Lf23c
    
Lf14c
    lda     #$00
    sta     ram_A9
Lf150
    tya
    and     #$1f
    bne     Lf16f
    ldx     ram_B4
    cpx     #$05
    beq     Lf16f
    ldx     #$9f
    dec     ram_86
    lda     ram_86
    bne     Lf165
    stx     ram_86
Lf165
    and     #$03
    bne     Lf16f
    dec     ram_87
    bne     Lf16f
    stx     ram_87
Lf16f
    lda     ram_ED
    beq     Lf180
    inc     ram_ED
    bne     Lf180
    ldx     #$38
    jsr     Lfce6
    lda     #$05
    sta     ram_EC
Lf180
    lda     SWCHB
    sta     ram_C6
    lsr
    bcs     Lf1c1
    lda     ram_C7
    ldx     ram_B6
    beq     Lf196
    inc     ram_C7
    cmp     #$7f
    bne     Lf20c
    beq     Lf19a
Lf196
    cmp     #$01
    bne     Lf20c
Lf19a
    ldx     #$ff
    ldy     ram_A0
    cpy     #$02
    bcs     Lf1a4
    stx     ram_F9
Lf1a4
    cpy     #$03
    bcs     Lf1aa
    stx     ram_F8
Lf1aa
    jsr     Lfe65
    sty     ram_A0
    ldy     #$08
    sty     ram_C7
    sty     ram_B7
    ldy     #$00
    sty     ram_BA
    sty     ram_B8
    iny
    sty     ram_AE
Lf1be
    jmp     Lf7f6
    
Lf1c1
    lsr
    bcs     Lf208
    dec     ram_C7
    bne     Lf20c
    lda     #$30
    sta     ram_C7
    lda     ram_F1
    beq     Lf1d8
    lda     #$00
    sta     ram_F1
    sta     ram_9F
    beq     Lf1fe
Lf1d8
    lda     ram_85
    ldx     ram_B7
    bne     Lf1f3
    cmp     #$0b
    beq     Lf1e6
    ldx     #$0b
    bne     Lf1fc
Lf1e6
    inc     ram_A0
    lda     ram_A0
    cmp     #$04
    bne     Lf1fe
    inx
    stx     ram_A0
    bne     Lf1fe
Lf1f3
    adc     #$02
    tax
    cmp     #$0b
    bcc     Lf1fc
    ldx     #$01
Lf1fc
    stx     ram_85
Lf1fe
    lda     ram_CA
    bne     Lf20c
    lda     #$34
    sta     ram_CA
    bne     Lf20c
Lf208
    lda     #$01
    sta     ram_C7
Lf20c
    lda     ram_B6
    beq     Lf1be
    dec     ram_C5
    bne     Lf286
    sed
    lda     #$3b
    sta     ram_C5
    lda     ram_9D
    cmp     #$a0
    bne     Lf250
    ldx     #$04
    jsr     Lfce6
    ldx     #$07
    lda     ram_9C
    sbc     #$01
    sta     ram_9C
    bne     Lf238
    ldx     crosshairX
    cpx     #$75
    bcc     Lf23c
    sta     ram_9D
    ldx     #$03
Lf238
    stx     ram_85
    bne     Lf278
Lf23c
    sta     ram_BB
    sta     ram_B4
    lda     #$b8
    sta     ram_C5
    ldx     #$16
    stx     ram_9C
    ldx     #$41
    stx     ram_CA
    ldx     #$1d
    bne     Lf238
Lf250
    lda     ram_9C
    adc     #$01
    sta     ram_9C
    lda     ram_9D
    adc     #$00
    sta     ram_9D
    bne     Lf278
    ldx     #$01
    lda     ram_9C
    cmp     #$25
    bne     Lf270
    lda     #$07
    sta     ram_EC
    lda     #$68
    sta     ram_CA
    bne     Lf276
Lf270
    cmp     #$03
    bne     Lf278
    ldx     #$05
Lf276
    stx     ram_B4
Lf278
    cld
    lda     ram_B5
    cmp     #$02
    bcs     Lf2a0
    ldx     ram_BB
    lda     Lfeab,x
    cmp     ram_9C
Lf286
    bne     Lf29a
    inc     ram_BB
    lda     Lfe8d,x
    and     #$0f
    sta     ram_BC
    cpx     #$00
    bne     Lf29a
    inx
    stx     ram_B4
    inc     crosshairX
Lf29a
    lda     ram_BB
    cmp     #$03
    bcs     Lf2a3
Lf2a0
    jmp     Lf32c
    
Lf2a3
    lda     crosshairX
    cmp     #$10
    bcs     Lf2ad
    ldx     ram_B4
    beq     Lf2a0
Lf2ad
    lsr
    lsr
    lsr
    lsr
    lsr
    clc
    adc     #$0b
    eor     #$0f
    adc     #$14
    dec     ram_A8
    bpl     Lf2c2
    sta     ram_A8
    jsr     Lfed5
Lf2c2
    ldx     ram_A6
    lda     #$05
    cpx     #$20
    bcc     Lf2cc
    lda     #$03
Lf2cc
    cpx     #$80
    bcc     Lf2d2
    lda     #$01
Lf2d2
    cpx     #$d8
    bcc     Lf2d8
    lda     #$00
Lf2d8
    dec     ram_A7
    bpl     Lf2f3
    sta     ram_A7
    lda     ram_86
    sbc     #$04
    bcc     Lf2e6
    sta     ram_86
Lf2e6
    lda     ram_87
    adc     #$03
    cmp     #$a0
    bcs     Lf2f0
    sta     ram_87
Lf2f0
    jsr     Lfd20
Lf2f3
    lda     ram_C1
    and     #$1f
    bne     Lf328
    lda     rngSeed
    cmp     #$b0
    bcc     Lf307
    lsr
    bcs     Lf305
    inc     ram_B2
    .byte   $2c ;bit                ;4-5 =  22 *
Lf305
    dec     ram_B2
Lf307
    lda     SWCHA
    asl
    bcs     Lf30f
    inc     ram_B2
Lf30f
    asl
    bcs     Lf314
    dec     ram_B2
Lf314
    ldy     ram_AA
    asl
    bcs     Lf31f
    cpy     #$21
    beq     Lf31f
    inc     ram_AA
Lf31f
    asl
    bcs     Lf328
    cpy     #$00
    beq     Lf328
    dec     ram_AA
Lf328
    cld
    jmp     Lf3a7
    
Lf32c
    lda     ram_B5
    cmp     #$03
    beq     Lf2f3
    cmp     #$04
    bne     Lf34d
    ldx     #$17
    bit     INPT4
    bmi     Lf33e
    ldx     #$03
Lf33e
    stx     ram_85
    ldy     #$01
    lda     SWCHA
    and     #$f0
    cmp     #$f0
    beq     Lf3a5
    dec     ram_C0
Lf34d
    bne     Lf3a7
    ldx     #$04
    stx     ram_C0
    tax
    txa
    asl
    tax
    bcs     Lf36b
    dec     ram_B2
    lda     ram_B2
    cmp     #$ff
    bne     Lf365
    lda     #$9f
    sta     ram_B2
Lf365
    cmp     #$4f
    bne     Lf36b
    inc     ram_B2
Lf36b
    txa
    asl
    tax
    bcs     Lf382
    inc     ram_B2
    lda     ram_B2
    cmp     #$a0
    bne     Lf37c
    lda     #$00
    sta     ram_B2
Lf37c
    cmp     #$50
    bne     Lf382
    dec     ram_B2
Lf382
    lda     ram_F3
    bne     Lf3a7
    lda     ram_CA
    cmp     #$98
    beq     Lf3a7
    txa
    asl
    tax
    bcs     Lf399
    lda     ram_88
    cmp     #$10
    beq     Lf3a7
    inc     ram_88
Lf399
    txa
    asl
    bcs     Lf3a7
    lda     ram_88
    beq     Lf3a7
    dec     ram_88
    bpl     Lf3a7
Lf3a5
    sty     ram_C0
Lf3a7
    lda     ram_B2
    bne     Lf407
    lda     ram_88
    bne     Lf407
    lda     ram_DE
    cmp     #$50
    bcs     Lf407
    cmp     #$47
    bcc     Lf407
    lda     ram_B1
    sbc     ram_B0
    bne     Lf407
    lda     ram_A5
    cmp     #$d2
    bne     Lf407
Lf3c5
    inc     ram_E2
    bne     Lf3cb
    dec     ram_E2
Lf3cb
    lda     ram_E2
    cmp     #$80
    bne     Lf3f5
    ldx     #$24
    stx     ram_CA
    dec     ram_B0
    ldx     ram_E3
    cpx     #$06
    beq     Lf3df
    inc     ram_E3
Lf3df
    sed
    clc
    lda     ram_9E
    adc     #$01
    sta     ram_9E
    lda     ram_9B
    adc     Lfdee,x
    sta     ram_9B
    bcc     Lf3f4
    lda     #$99
    sta     ram_9B
Lf3f4
    cld
Lf3f5
    bcc     Lf416
    ldx     #$05
    cmp     #$fe
    beq     Lf403
    cmp     #$ff
    beq     Lf416
    ldx     #$1f
Lf403
    stx     ram_85
    bne     Lf45c
Lf407
    lda     ram_E2
    cmp     #$80
    bcc     Lf412
    cmp     #$ff
    bne     Lf3c5
    .byte   $2c ;bit                ;4-2 =  13 *
Lf412
    lda     #$00
    sta     ram_E2
Lf416
    lda     ram_B5
    cmp     #$04
    bne     Lf44b
    lda     ram_A5
    cmp     #$0b
    bcc     Lf44b
    ldx     ram_F9
    bmi     Lf44b
    lda     SWCHA
    cmp     #$cf
    bcc     Lf44b
    ldx     ram_E3
    cpx     #$06
    beq     Lf435
    ldx     ram_A0
Lf435
    lda     ram_C1
    and     Lfe39,x
    bne     Lf44b
    lda     ram_B2
    cmp     #$51
    bcs     Lf449
    cmp     #$4f
    bcs     Lf44b
    inc     ram_B2
    .byte   $2c ;bit                ;4-5 =  24 *
Lf449
    dec     ram_B2
Lf44b
    ldx     #$75
    lda     ram_A5
    cmp     #$ff
    beq     Lf46d
    ldx     ram_B5
    cpx     #$02
    beq     Lf45f
    jmp     Lf5c7
    
Lf45c
    jmp     Lf538
    
Lf45f
    ldx     #$70
    cmp     #$c3
    bcc     Lf46d
    ldx     #$80
    lda     ram_A6
    cmp     #$a9
    bne     Lf470
Lf46d
    jsr     Lfcc7
Lf470
    lda     SWCHA
    and     #$f0
    cmp     #$f0
    beq     Lf45c
    dec     ram_C0
    bne     Lf4f2
    ldx     #$18
    stx     ram_C0
    stx     ram_AF
    jsr     Lfce6
    lsr
    lsr
    lsr
    lsr
    eor     #$0f
    tax
    lda     #$09
    ldy     ram_85
    cpy     #$15
    bne     Lf497
    lda     #$01
Lf497
    jsr     Lfcae
    lda     ram_B6
    beq     Lf45c
    bit     ram_F9
    bmi     Lf4a8
    lda     ram_C6
    and     #$48
    bne     Lf4f5
Lf4a8
    txa
    lsr
    tay
    bcc     Lf4bc
    bit     INPT4
    bmi     Lf4b7
    jsr     Lfd42
    jmp     Lf4cc
    
Lf4b7
    ldx     #$0d
    jsr     Lfed5
Lf4bc
    tya
    lsr
    tay
    bcc     Lf4d5
    lda     #$00
    sta     ram_AF
    bit     INPT4
    bmi     Lf4d0
    jsr     Lfd20
Lf4cc
    ldx     #$11
    bne     Lf4f0
Lf4d0
    ldx     #$0d
    jsr     Lfeea
Lf4d5
    bit     INPT4
    bpl     Lf4f2
    tya
    lsr
    tay
    bcc     Lf4e5
    lda     #$04
    inc     ram_B2
    jsr     Lfd0f
Lf4e5
    tya
    lsr
    bcc     Lf4f0
    dec     ram_B2
    lda     #$08
    jsr     Lfd0f
Lf4f0
    stx     ram_85
Lf4f2
    jmp     Lf5c7
    
Lf4f5
    txa
    lsr
    tay
    bcc     Lf508
    lda     ram_88
    dec     ram_88
    bpl     Lf502
    sta     ram_88
Lf502
    ldx     #$01
Lf504
    lda     #$13
    bne     Lf532
Lf508
    tya
    lsr
    tay
    bcc     Lf519
    lda     ram_88
    cmp     #$10
    beq     Lf515
    inc     ram_88
Lf515
    ldx     #$02
    bne     Lf504
Lf519
    tya
    lsr
    tay
    bcc     Lf524
    dec     ram_B3
    ldx     #$04
    bne     Lf52c
Lf524
    tya
    lsr
    bcc     Lf538
    inc     ram_B3
    ldx     #$08
Lf52c
    lda     #$05
    sta     ram_C0
    lda     #$15
Lf532
    sta     ram_85
    stx     ram_AB
    bne     Lf577
Lf538
    lda     ram_B3
    clc
    adc     #$10
    lsr
    lsr
    lsr
    lsr
    lsr
    tax
    lda     ram_CA
    cmp     #$58
    beq     Lf55f
    lda     Lfd84,x
    sta     ram_AB
    ldy     #$00
    sty     ram_E0
    sty     ram_AF
    lda     ram_CA
    cmp     #$18
    bne     Lf55c
    sty     ram_CA
Lf55c
    iny
    sty     ram_C0
Lf55f
    lda     ram_C6
    and     #$48
    beq     Lf5c7
    bit     INPT4
    bmi     Lf5c7
    lda     #$fe
    sta     ram_C9
    lda     #$58
    sta     ram_CA
    sta     ram_AF
    lda     ram_C1
    and     #$0f
Lf577
    bne     Lf5c7
    lda     #$01
    ldy     ram_88
    cpy     #$07
    beq     Lf583
    lda     #$02
Lf583
    jsr     Lfcae
    cpx     #$02
    bcs     Lf593
Lf58a
    jsr     Lfed5
    cpy     #$08
    bcs     Lf5c7
    bcc     Lf5c4
Lf593
    cpx     #$02
    bne     Lf59d
    dec     ram_B2
    lda     #$05
    bne     Lf5b2
Lf59d
    cpx     #$06
    bcs     Lf5aa
    jsr     Lfeea
    cpy     #$07
    bcs     Lf5be
    bcc     Lf5c7
Lf5aa
    cpx     #$07
    beq     Lf58a
    inc     ram_B2
    lda     #$09
Lf5b2
    sta     ram_AB
    ldx     #$0f
    stx     ram_85
    cpy     #$07
    beq     Lf5c7
    bcc     Lf5c4
Lf5be
    jsr     Lfd20
    jmp     Lf5c7
    
Lf5c4
    jsr     Lfd42
Lf5c7
    lda     ram_B5
    cmp     #$02
    bcs     Lf5d0
Lf5cd
    jmp     Lf76f
    
Lf5d0
    bit     ram_F9
    bpl     Lf5d8
    lda     ram_E3
    bne     Lf5de
Lf5d8
    lda     ram_A6
    cmp     #$bf
    bcs     Lf5cd
Lf5de
    lda     ram_A5
    cmp     #$d7
    bcs     Lf5cd
    sta     ram_AF
    cmp     #$c8
    bne     Lf615
    dec     ram_A5
    ldy     #$00
    sty     ram_B2
    sty     ram_AB
    ldx     #$03
    stx     ram_B5
    stx     ram_85
    stx     currentScreenId
    ldx     #$09
    stx     ram_AA
    stx     ram_F6
    ldx     #$65
    lda     ram_B3
    bne     Lf612
    ldx     #$55
    lda     ram_88
    cmp     #$0d
    beq     Lf615
    bcs     Lf612
    ldx     #$60
Lf612
    jsr     Lfcc7
Lf615
    ldx     ram_E8
    beq     Lf61c
Lf619
    jmp     Lf6ab
    
Lf61c
    lda     ram_AB
    and     #$0c
    ora     #$01
    sta     ram_AB
    lda     currentScreenId
    cmp     #$04
    bne     Lf635
    lda     ram_88
    adc     #$03
    ldx     ram_C6
    bpl     Lf63c
    lsr
    bpl     Lf63c
Lf635
    ldx     ram_A4
    lda     Lfda8,x
    sta     ram_C4
Lf63c
    inc     ram_C2
    cmp     ram_C2
    bcs     Lf619
    lda     #$00
    sta     ram_C2
    jsr     Lfd42
    lda     ram_A5
    cmp     #$a7
    bcs     Lf690
    cmp     #$78
    bcc     Lf65b
    inc     ram_E1
    bne     Lf690
    dec     ram_E1
    bne     Lf690
Lf65b
    cmp     #$1e
    bne     Lf68a
    inc     ram_B5
    inc     currentScreenId
    ldx     #$02
    stx     ram_F2
    lda     #$40
    sta     ram_E9
    dec     ram_A5
    lda     #$00
    sta     ram_E7
    bit     ram_F9
    bpl     Lf67e
    stx     ram_88
    bit     ram_F7
    bpl     Lf680
    lda     #$27
    .byte   $2c ;bit                ;4-2 =  51 *
Lf67e
    lda     #$4f
Lf680
    sta     ram_B2
    ldx     #$35
    lda     ram_CA
    cmp     #$41
    beq     Lf612
Lf68a
    lda     ram_E1
    beq     Lf690
    dec     ram_E1
Lf690
    lda     ram_A5
    cmp     #$30
    bcs     Lf6a0
    cmp     #$1e
    bcc     Lf6a0
    inc     ram_E7
    bne     Lf6a0
    dec     ram_E7
Lf6a0
    dec     ram_C3
    bpl     Lf6ab
    lda     #$0a
    sta     ram_C3
    jsr     Lfeea
Lf6ab
    lda     currentScreenId
    cmp     #$04
    bne     Lf6c9
    dec     ram_EB
    bpl     Lf6c9
    lda     #$2a
    sta     ram_EB
    dec     ram_E9
    bpl     Lf6c9
    bit     ram_F0
    bmi     Lf6c9
    lda     ram_CA
    bne     Lf6c9
    lda     #$44
    sta     ram_CA
Lf6c9
    lda     ram_A5
    cmp     #$78
    bcs     Lf721
    ldx     #$1e
    stx     AUDF1
    ldx     #$08
    stx     AUDC1
    ldx     #$01
    bit     ram_F0
    bpl     Lf6df
    ldx     #$03
Lf6df
    stx     AUDV1
    lda     ram_98
    bne     Lf736
    ldy     ram_99
    bne     Lf736
    lda     ram_E8
    bne     Lf712
    dec     ram_E8
    ldx     #$98
    jsr     Lfce6
    ldx     #$15
    lda     ram_E9
    bpl     Lf71e
    ldx     #$20
    cmp     #$eb
    bcc     Lf71e
    ldx     #$40
    lda     ram_F0
    beq     Lf71e
    lda     ram_88
    adc     #$05
    sta     ram_88
    and     #$10
    beq     Lf712
    sta     ram_88
Lf712
    lda     ram_B2
    cmp     #$18
    bcc     Lf724
    cmp     #$97
    bcs     Lf724
    ldx     #$10
Lf71e
    jsr     Lfcc7
Lf721
    jmp     Lf76f
    
Lf724
    lda     ram_F3
    bne     Lf732
    lda     ram_88
    bne     Lf732
    inc     ram_F3
    lda     #$a2
    sta     ram_CA
Lf732
    lda     ram_E9
    cmp     #$90
Lf736
    bne     Lf76f
    ldx     #$30
    lda     ram_88
    beq     Lf743
    jsr     Lfcc7
    bcs     Lf76f
Lf743
    sty     ram_96
    sty     ram_B6
    sty     ram_BA
    sty     ram_F9
    ldx     #$21
    bit     ram_F8
    bpl     Lf753
    ldx     #$19
Lf753
    stx     ram_85
    ldx     #$05
    bit     ram_F8
    bmi     Lf76b
    ldy     ram_E3
    beq     Lf76b
    inx
    cpy     #$06
    bne     Lf76b
    lda     ram_9B
    cmp     #$75
    bcc     Lf76b
    inx
Lf76b
    stx     currentScreenId
    sty     ram_F8
Lf76f
    ldy     ram_F4
    beq     Lf789
    cpy     #$01
    bne     Lf77c
    beq     Lf786
bank1EntryFromBank0
    jmp     bank1Handler
    
Lf77c
    cpy     #$02
    bne     Lf789
    jsr     Lfd20
    jmp     Lf789
    
Lf786
    jsr     Lfd42
Lf789
    ldx     ram_85
    cpx     #$0d
    bne     Lf799
    clc
    lda     ram_B1
    sbc     ram_B0
    eor     #$ff
    jmp     Lf7c0
    
Lf799
    lda     ram_B2
    cpx     #$0f
    beq     Lf7be
    lda     ram_E9
    cpx     #$17
    beq     Lf7be
    lda     #$d2
    sec
    sbc     ram_A5
    cpx     #$11
    beq     Lf7be
    lda     ram_88
    asl
    asl
    sbc     #$1b
    cpx     #$13
    beq     Lf7be
    cpx     #$15
    bne     Lf7f5
    lda     ram_B3
Lf7be
    cmp     #$80
Lf7c0
    ldx     #$00
    bcc     Lf7ca
    ldx     #$a0
    eor     #$ff
    adc     #$00
Lf7ca
    stx     ram_A3
    tay
    lsr
    lsr
    lsr
    lsr
    tax
    lda     Lfd9a,x
    sta     ram_A2
    cpx     #$0d
    bcc     Lf7dd
    inc     ram_A3
Lf7dd
    cpx     #$07
    bcc     Lf7e3
    inc     ram_A3
Lf7e3
    tya
    and     #$0f
    cmp     #$0a
    bcc     Lf7ec
    adc     #$05
Lf7ec
    sed
    adc     ram_A2
    sta     ram_A2
    bcc     Lf7f5
    inc     ram_A3
Lf7f5
    cld
Lf7f6
    ldx     #$06
    ldy     ram_85
    lda     ram_F1
    beq     Lf800
    ldy     #$09
Lf800
    sty     ram_FA
    cpy     #$0d
    bcc     Lf808
    ldy     #$0d
Lf808
    lda.wy  ram_96,y
    and     #$f0
    lsr
    jsr     Lfddc
    dex
    lda.wy  ram_96,y
    and     #$0f
    asl
    asl
    asl
    jsr     Lfddc
    dey
    dex
    bpl     Lf808
    ldx     #$06
    ldy     #$54
Lf825
    lda     screenPtr1L,x
    bne     Lf82f
    sty     screenPtr1L,x
    dex
    dex
    bne     Lf825
Lf82f
    lda     ram_FA
    lsr
    tax
    lda     graphicOffsetTable,x
    cpx     #$0c
    bcs     Lf848
    sta     screenPtr6L
    adc     #$06
    sta     screenPtr5L
    lda     #$de
    sta     screenPtr6H
    sta     screenPtr5H
    bne     Lf857
Lf848
    ldx     #$0a
    ldy     #$df
    clc
Lf84d
    sta     screenPtr1L,x
    sty     screenPtr1H,x
    adc     #$07
    dex
    dex
    bpl     Lf84d
Lf857
    ldx     INTIM
    bne     Lf857
    stx     WSYNC
;---------------------------------------
    ldy     ram_B4
    beq     Lf870
    lda     ram_C1
    and     #$02
    beq     Lf870
    cpy     #$05
    bne     Lf86e
    stx     WSYNC
;---------------------------------------
Lf86e
    stx     WSYNC
;---------------------------------------
Lf870
    stx     WSYNC
;---------------------------------------
    sta     ram_FA
    lda     #$0f
    and     ram_B8
    lsr
    sta     VBLANK
    dex
    stx     PF2
    inx
    stx     VDELP0
    stx     VDELP1
    lda     #$80
    sta     HMBL,x
    lda     #$15
    sta     CTRLPF
    lda     ram_98
    cmp     #$09
    sta     RESBL
    bcs     Lf8c2
    ldy     ram_B7
    beq     Lf8c8
    ldy     ram_B5
    bne     Lf8c2
    eor     #$0f
    sbc     #$05
    rol
    sta     ram_FA
    sta     ram_FC
    lda     #$14
    sbc     ram_FA
    asl
    adc     #$0b
    sta     ram_FB
    lda     ram_B4
    beq     Lf8bb
    lda     ram_C1
    and     #$02
    beq     Lf8bb
    inc     ram_FC
    dec     ram_FA
Lf8bb
    lda     ram_AE
    adc     #$06
    tay
    bcc     Lf8fb
Lf8c2
    ldy     ram_B5
    bne     Lf8c8
    inc     ram_B5
Lf8c8
    lda     #$01
    sta     ram_FA
    lda     ram_A4
    lsr
    eor     #$0f
    tay
    lda     rngSeed
    bit     ram_EC
    beq     Lf8de
    dec     ram_EC
    lda     #$1e
    bne     Lf8e8
Lf8de
    cmp     ram_E1
    bcs     Lf8ed
    and     #$07
    tay
    lda     Lfebf,y
Lf8e8
    sta     COLUP1
    jmp     Lf903
    
Lf8ed
    cmp     ram_E7
    bcs     Lf8f5
    lda     #$08
    bne     Lf8e8
Lf8f5
    lda     ram_B7
    bne     Lf8fb
    ldy     #$05
Lf8fb
    lda     Lfd8a,y
    sta     COLUP1
    lda     Lfec5,y
Lf903
    sta     COLUBK
    sta     starfieldVerticalCounter
Lf907
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     Lfe27,x
    sta     PF0
    lda     Lff01,x
    sta     PF1
    lda     Lfe31,x
    sta     PF2
    sta     HMCLR
    inx
    cpx     #$0a
    bcc     Lf907
    ldx     ram_FA
Lf923
    sta     WSYNC
;---------------------------------------
    lda     #$30
    sta     PF0
    lda     #$00
    sta     PF1
    sta     PF2
    dex
    bne     Lf923
    lda     ram_B7
    beq     Lf991
    lda     ram_B5
    bne     Lf991
    sta     REFP0
    lda     #$07
    sta     NUSIZ0
    lda     ram_86
    jsr     Lfe00
    ldx     #$0a
Lf947
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     Lfe78,x
    sta     GRP0
    lda     Lfea0,x
    adc     ram_AE
    sta     COLUP0
    lda     Lfe8c,x
    sta     HMP0
    dex
    bne     Lf947
    stx     WSYNC
;---------------------------------------
    stx     GRP0
    ldx     ram_FB
Lf965
    stx     WSYNC
;---------------------------------------
    dex
    bne     Lf965
    lda     ram_87
    jsr     Lfe00
    ldx     #$0a
Lf971
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     Lfe82,x
    sta     GRP0
    lda     Lfea0,x
    adc     ram_AE
    sta     COLUP0
    lda     Lfe96,x
    sta     HMP0
    dex
    bne     Lf971
    stx     WSYNC
;---------------------------------------
    stx     GRP0
    ldx     ram_FC
    bne     Lf9fe
Lf991
    cmp     #$04
    beq     Lfa01
    txs
    lda     #$04
    sta     ram_FC
    ldy     ram_89
    lda     #$3c
    sta     ram_FB
Lf9a0
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     (ram_DC),y
    sta     GRP0
    lda     Lfeb9,y
    sta     ENAM1
    dec     ram_FB
    bmi     Lf9f1
    dey
    sta     HMCLR
    bmi     Lf9e5
    cpy     #$02
    beq     Lf9c2
    cpy     #$08
    beq     Lf9c2
    cpy     #$0e
    bne     Lf9a0
Lf9c2
    tsx
    inx
    txs
    lda     ram_89,x
    sta     ram_FA
    lda     (ram_DC),y
    dey
    sta     WSYNC
;---------------------------------------
    sta     GRP0
    lda     ram_FA
Lf9d2
    sbc     #$0f
    bcs     Lf9d2
    eor     #$0f
    asl
    asl
    asl
    asl
    adc     #$80
    sta     HMM1
    sta     RESM1
    jmp     Lf9a0
    
Lf9e5
    dec     ram_FC
    ldx     ram_FC
    lda     ram_D8,x
    sta     ram_DC
    ldy     #$11
    bne     Lf9a0
Lf9f1
    ldx     #$ff
    txs
    inx
    ldy     ram_89
    cpy     #$14
    bne     Lf9fd
    sta     WSYNC
;---------------------------------------
Lf9fd
    inx
Lf9fe
    jmp     Lfba7
    
Lfa01
    ldx     ram_A5
Lfa03
    sta     WSYNC
;---------------------------------------
    dex
    bpl     Lfa03
    lda     ram_88
    tax
    eor     #$1f
    sec
    sbc     #$0f
    sta     ram_FC
Lfa12
    sta     WSYNC
;---------------------------------------
    dex
    bpl     Lfa12
    inx
    stx     REFP0
    lda     #$05
    sta     NUSIZ0
    lda     ram_86
    jsr     Lfdf5
    ldx     #$07
Lfa25
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     Lfdc5,x
    jsr     Lfb9c
    bne     Lfa25
    lda     ram_87
    jsr     Lfdf5
    ldx     #$07
Lfa38
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     Lfdcc,x
    jsr     Lfb9c
    bne     Lfa38
    lda     ram_B2
    clc
    adc     #$6d
    bcs     Lfa4f
    cmp     #$a0
    bcc     Lfa51
Lfa4f
    sbc     #$a0
Lfa51
    jsr     Lfe00
    inx
    lda     ram_B2
    clc
    adc     #$24
    cmp     #$a0
    bcc     Lfa60
    sbc     #$a0
Lfa60
    jsr     Lfe00
    lda     #$17
    sta     NUSIZ0
    sta     NUSIZ1
    ldy     #$24
    sty     COLUP0
    sty     COLUP1
    ldx     #$04
Lfa71
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     Lfdd7,x
    sta     COLUBK
    lda     Lfdd3,x
    sta     GRP0
    sta     GRP1
    dex
    sty     HMP0
    sty     HMP1
    bne     Lfa71
    sta     WSYNC
;---------------------------------------
    stx     GRP0
    stx     GRP1
    sty     COLUBK
    lda     ram_B2
    cmp     #$50
    bcc     Lfa99
    adc     #$5f
    clc
Lfa99
    adc     #$52
    tay
    ldx     #$02
    jsr     Lfe00
    tya
    clc
    adc     #$01
    inx
    jsr     Lfe00
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    jsr     Lff00
    ldy     #$00
    lda     ram_B2
    cmp     #$50
    bcc     Lfac5
    ldx     #$10
    stx     HMM0
    ldx     #$01
    stx     HMM1
    cmp     #$78
    jmp     Lfacf
    
Lfac5
    ldx     #$f0
    stx     HMM1
    ldx     #$00
    stx     HMM0
    cmp     #$28
Lfacf
    beq     Lfad7
    ldy     #$10
    bcc     Lfad7
    ldy     #$f0
Lfad7
    sty     starfieldHorizontalMotion,x
    sta     WSYNC
;---------------------------------------
    lda     #$20
    sta     COLUBK
    lda     ram_B2
    cmp     #$50
    bcc     Lfae7
    sbc     #$50
Lfae7
    cmp     #$29
    bcc     Lfaef
    eor     #$ff
    sbc     #$b0
Lfaef
    lsr
    sta     ram_FA
    lda     ram_E8
    beq     Lfb02
    lda     ram_C1
    and     #$03
    bne     Lfb02
    lda     ram_B6
    beq     Lfb02
    dec     ram_E9
Lfb02
    lda     ram_E9
    eor     #$ff
    clc
    adc     #$36
    sta     starfieldVerticalCounter
    sta     WSYNC
;---------------------------------------
    lda     #$28
    sta     COLUBK
    lda     #$1d
    sec
    sbc     ram_A5
    clc
    adc     ram_FC
    sta     ram_FC
    ldy     #$0f
    lda     ram_C1
    and     #$10
    bne     Lfb25
    ldy     #$0a
Lfb25
    sty     COLUP0
    sty     COLUP1
    sty     CXCLR
    ldy     #$00
    ;-----------------------------------------------------------
    ; Main Space View Loop (Window Effect)
    ; Draws the starfield/dashboard lines visible through the cockpit.
    ;-----------------------------------------------------------
kernelDrawCockpitWindow
    sta     WSYNC
;---------------------------------------
    sta     HMOVE                       ; Apply fine motion (moves stars left/right).
    iny
    cpy     ram_FA                      ; Compare loop counter to Motion Threshold?
    bcc     Lfb3e                       ; Branch if "fast motion" update not needed yet.
    ldy     #$00
    lda     starfieldHorizontalMotion,x ; Load motion value from table.
    sta     HMM0,x                      ; Apply to HMM0 (or HMM1 if X=1).
    bcs     Lfb42
Lfb3e
    lda     #$00
    sta     HMM0,x                      ; Clear motion if below threshold.
Lfb42
    lda     #$00
    dec     starfieldVerticalCounter    ; Decrement the star/line pattern counter.
    bpl     Lfb4c
    sta     ENAM0                       ; If counter wrapped, clear ENAM0.
    bmi     Lfb5c                       ; And clear ENAM1 (via fallthrough/branch logic).
Lfb4c
    ;-----------------------------------------------------------
    ; Masking Trick:
    ; The code attempts to enable the Missiles (Stars/Lines)
    ; every scanline inside the window area.
    ; However, it checks collision with the Playfield (Window Frame).
    ; If a missile hits the frame, it is disabled (masked).
    ; This makes the stars appear "behind" the cockpit.
    ;-----------------------------------------------------------
    
    ; Check Missile 0 (Orange vertical lines).
    bit     CXM0FB                      ; Check M0 collision with Playfield/Player.
    bmi     Lfb52                       ; If hit (inside wall/frame), skip enable (keep 0/Off).
    lda     #$02                        ; Else, Enable M0 (1 pixel).
Lfb52
    sta     ENAM0

    lda     #$00                        ; Prepare 0 (Off).
    ; Check Missile 1 (Green Stars).
    bit     CXM1FB                      ; Check M1 collision with Playfield/Player.
    bmi     Lfb5c                       ; If hit (inside wall/frame), skip enable (keep 0/Off).
    lda     #$02                        ; Else, Enable M1 (1 pixel).
Lfb5c
    sta     ENAM1
    dec     ram_FC                      ; Decrement scanline counter.
    bpl     kernelDrawCockpitWindow     ; Loop for next scanline.
    txs
    ldx     #$0a
Lfb65
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     Lfe26,x
    sta     PF0
    lda     Lff00,x
    sta     PF1
    lda     Lfe30,x
    sta     PF2
    stx     ram_FB
    tsx
    iny
    cpy     ram_FA
    bcc     Lfb88
    ldy     #$00
    lda     starfieldHorizontalMotion,x
    sta     HMM0,x
    bcs     Lfb8c
Lfb88
    lda     #$00
    sta     HMM0,x
Lfb8c
    dec     starfieldVerticalCounter
    bpl     Lfb94
    sta     ENAM0
    sta     ENAM1
Lfb94
    ldx     ram_FB
    dex
    bne     Lfb65
    jmp     exitLogicToKernel
    
Lfb9c
    sta     GRP0
    lda     Lfdbe,x
    sta     COLUP0
    sta     HMCLR
    dex
    rts
    
Lfba7
    stx     WSYNC
;---------------------------------------
    dex
    bne     Lfba7
    stx     GRP0
    stx     ENAM1
    lda     ram_B7
    beq     Lfbc2
    lda     #$10
    ldy     ram_B5
    beq     Lfbc2
    ldy     starfieldVerticalCounter
    cpy     #$90
    bne     Lfbc2
    lda     ram_88
Lfbc2
    sta     ram_FC
    lda     ram_EF
    sta     WSYNC
;---------------------------------------
    and     #$0f
    tay
    nop
    stx     COLUP0
    ldx     #$11
    stx     CTRLPF
    lda     #$15
    ldx     #$05
    sta     NUSIZ0
Lfbd8
    dex
    bne     Lfbd8
    sta     RESM0
    sta     HMM0,x
    lda     #$c0
    sta     HMP0,x
    dex
    stx     ENAM0
    ldx     #$11
    lda     #PURPLE|$9
    sta     RESP0
Lfbec
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sta     COLUPF
    lda     Lfe33,x
    sta     PF0
    lda     Lfe3b,x
    sta     PF1
    lda     Lfe43,x
    sta     PF2
Lfc01
    lda     #$ff
    sta     GRP0
    dey
    bpl     Lfc0a
    ldy     #$0f
Lfc0a
    sta     HMCLR
    lda     starfieldVerticalCounter
    dec     ram_FC
    bpl     Lfc18
    lda     Lfe55,y
    sec
    adc     ram_EA
Lfc18
    dex
    beq     exitLogicToKernel
    cpx     #$0a
    bcs     Lfbec
    sta     WSYNC
;---------------------------------------
    sta     COLUBK
    lda     #BLACK|$0
    sta     COLUPF
    lda     Lfe26,x
    sta     PF0
    lda     Lff00,x
    sta     PF1
    lda     Lfe30,x
    sta     PF2
    bcc     Lfc01
;-----------------------------------------------------------
;      Exit Logic -> Switch to Kernel (Bank 1 -> Bank 0)
;-----------------------------------------------------------
exitLogicToKernel
    sta     WSYNC
;---------------------------------------
    jmp     resetBank1
    
bank1Handler
    bit     ram_F9
    bmi     waitForTimerVSYNC
    lda     ram_B5
    cmp     #$02
    bne     Lfc60
    bit     ram_C6
    bmi     Lfc5c
    lda     ram_E6
    bpl     Lfc60
    ldx     #$41
    stx     ram_CA
    cmp     #$ff
    bne     Lfc60
    ldx     #$85
    jsr     Lfcc7
Lfc5c
    ldx     #$01
    stx     ram_E6
Lfc60
    lda     ram_ED
    bne     waitForTimerVSYNC
    bit     ram_C6
    bpl     Lfc85
    ldx     #$50
    lda     ram_B5
    beq     waitForTimerVSYNC
    and     #$01
    bne     Lfc82
    lda     ram_F0
    bne     waitForTimerVSYNC
    dec     ram_F0
    .byte   $2c ;bit                ;4-5 =  30 *
Lfc79
    inc     ram_F0
    ldx     #$72
    jsr     Lfce6
    bne     waitForTimerVSYNC
Lfc82
    jsr     Lfcc7
Lfc85
    lda     ram_F0
    bne     Lfc79
;-----------------------------------------------------------
;      Wait For Timer / Start VSYNC (Bank 1)
;-----------------------------------------------------------
waitForTimerVSYNC
    ldx     INTIM
    bne     waitForTimerVSYNC
    ldy     #$02
    sty     WSYNC
;---------------------------------------
    sty     VSYNC
    lda     ram_F2
    beq     Lfca2
    lda     ram_CA
    bne     Lfca2
    dec     ram_F2
    lda     #$82
    sta     ram_CA
Lfca2
    lda     ram_DE
    jsr     Lfe00
    stx     WSYNC
;---------------------------------------
    stx     VSYNC
    jmp     Lf011
    
Lfcae
    bit     ram_F8
    bmi     Lfd09
    sed
    sec
    sta     ram_FA
    lda     ram_9A
    sbc     ram_FA
    sta     ram_9A
    lda     ram_9B
    sbc     #$00
    sta     ram_9B
    cld
    bcs     Lfd09
    ldx     #$99
Lfcc7
    sec
    lda     ram_B6
    beq     Lfd09
    stx     ram_9F
    txa
    ldx     ram_F8
    beq     Lfced
    inx
    ldy     #$05
Lfcd6
    cmp     Lfd09,y
    beq     Lfcf1
    dey
    bne     Lfcd6
    bit     ram_F9
    bmi     Lfd09
    sta     ram_F1
    ldx     #$b4
Lfce6
    stx     ram_CA
    ldx     #$fe
    stx     ram_C9
    rts
    
Lfced
    stx     ram_9A
    stx     ram_9B
Lfcf1
    stx     ram_BA
    stx     ram_F1
    stx     ram_B6
    stx     ram_B4
    stx     ram_AB
    stx     ram_E1
    stx     ram_B3
    stx     ram_E7
    stx     ram_F9
    stx     ram_F8
    ldx     #$1b
    stx     ram_85
Lfd09
    rts
    
    .byte   $95,$70,$75,$80,$10             ; $fd0a (*)
    
Lfd0f
    ora     ram_AB
    sta     ram_AB
    lda     ram_E3
    beq     Lfd1d
    bit     rngSeed
    bmi     Lfd1d
    inc     ram_B0
Lfd1d
    ldx     #$0f
    rts
    
Lfd20
    lda     ram_A5
    cmp     #$ff
    beq     Lfd40
    sed
    lda     ram_98
    clc
    adc     #$01
    sta     ram_98
    bcc     Lfd38
    lda     ram_99
    adc     #$00
    sta     ram_99
    inc     ram_A4
Lfd38
    lda     ram_98
    and     #$0f
    bne     Lfd40
Lfd3e
    inc     ram_A5
Lfd40
    cld
    rts
    
Lfd42
    sed
    lda     ram_98
    sec
    sbc     #$01
    sta     ram_98
    bcs     Lfd54
    lda     ram_99
    sbc     #$00
    sta     ram_99
    dec     ram_A4
Lfd54
    lda     ram_98
    and     #$0f
    bne     Lfd40
    dec     ram_A5
    lda     ram_A5
    cmp     #$ff
    bne     Lfd40
    beq     Lfd3e
    
Lfd64
    .byte   $01,$0f,$12,$0d,$05,$19,$66,$6e ; $fd64 (D)
    .byte   $0d,$14,$07,$47,$2b,$67,$17,$57 ; $fd6c (D)
    .byte   $0e,$76,$2f,$4a,$7f,$5c,$00,$00 ; $fd74 (D)
    .byte   $00,$00,$99,$99,$15,$a0,$00,$00 ; $fd7c (D)
Lfd84
    .byte   $01,$05                         ; $fd84 (D)
    .byte   $04,$06,$02,$0a                 ; $fd86 (*)
Lfd8a
    .byte   $08,$09,$0f,$0f,$0f             ; $fd8a (*)
    
    .byte   BLACK|$f                        ; $fd8f (C)
    
    .byte   $0b                             ; $fd90 (*)
    
    .byte   BLACK|$8                        ; $fd91 (C)
    
    .byte   $88,$88,$86,$86,$68,$68,$86,$86 ; $fd92 (*)
Lfd9a
    .byte   $00,$16,$32,$48,$64,$80,$96,$12 ; $fd9a (*)
    .byte   $28,$44,$60,$76,$92,$08         ; $fda2 (*)
Lfda8
    .byte   $24,$40,$03,$04,$04,$03,$03,$02 ; $fda8 (*)
    .byte   $02,$02,$02,$02,$02,$02,$03,$03 ; $fdb0 (*)
    .byte   $06,$06,$06,$07,$14,$14         ; $fdb8 (*)
Lfdbe
    .byte   $0e,$0e,$08,$1a,$0c,$0e,$0e     ; $fdbe (*)
Lfdc5
    .byte   $0e,$00,$1f,$3c,$ff,$7f,$3e     ; $fdc5 (*)
Lfdcc
    .byte   $18,$00,$1f,$7c,$fe,$3f,$7c     ; $fdcc (*)
Lfdd3
    .byte   $38,$ff,$ee,$cc                 ; $fdd3 (*)
Lfdd7
    .byte   $88,$ae,$7c,$7a,$78             ; $fdd7 (*)
    
Lfddc
    cpy     #$04
    bcs     Lfde6
    bit     ram_E1
    bpl     Lfde6
    lda     #$50
Lfde6
    sta     screenPtr1L,x
    lda     #$de
    sta     screenPtr1H,x
    dex
    rts
    
Lfdee
    .byte   $15,$20,$25,$30,$35,$40,$40     ; $fdee (*)
    
Lfdf5
    clc
    adc     ram_B2
    bcs     Lfdfe
    cmp     #$a0
    bcc     Lfe00
Lfdfe
    sbc     #$9f
Lfe00
    sta     WSYNC
;---------------------------------------
    sec
Lfe03
    sbc     #$0f
    bcs     Lfe03
    eor     #$0f
    asl
    asl
    asl
    asl
    adc     #$80
    sta     RESP0,x
    sta     WSYNC
;---------------------------------------
    sta     HMP0,x
    rts
    
;-----------------------------------------------------------
;      Graphic Pointers (Low Bytes?)
;      Point to $FE7F, $FE73, etc.
;-----------------------------------------------------------
graphicOffsetTable
    .byte   $7f,$73,$5b,$67,$8b,$df,$97,$a3 ; $fe16 (*)
    .byte   $af,$bb,$c7,$d3                 ; $fe1e (*)
    .byte   $b4                             ; $fe22 (D)
    .byte   $87,$0c,$5e                     ; $fe23 (*)
Lfe26
    .byte   $35                             ; $fe26 (*)
    
Lfe27
    .byte   %11110000 ; |****    |            $fe27 (P)
    .byte   %11110000 ; |****    |            $fe28 (P)
    .byte   %11110000 ; |****    |            $fe29 (P)
    .byte   %11110000 ; |****    |            $fe2a (P)
    .byte   %11110000 ; |****    |            $fe2b (P)
    .byte   %11110000 ; |****    |            $fe2c (P)
    .byte   %01110000 ; | ***    |            $fe2d (P)
    .byte   %01110000 ; | ***    |            $fe2e (P)
    .byte   %01110000 ; | ***    |            $fe2f (P)
Lfe30
    .byte   %01110000 ; | ***    |            $fe30 (P)
Lfe31
    .byte   %11100000 ; |***     |            $fe31 (P)
    .byte   %11000000 ; |**      |            $fe32 (P)
Lfe33
    .byte   %11000000 ; |**      |            $fe33 (P)
    .byte   %10000000 ; |*       |            $fe34 (P)
    .byte   %10000000 ; |*       |            $fe35 (P)
    .byte   %10000000 ; |*       |            $fe36 (P)
    .byte   %00000000 ; |        |            $fe37 (P)
    .byte   %00000000 ; |        |            $fe38 (P)
Lfe39
    .byte   %00000000 ; |        |            $fe39 (P)
    .byte   %00000000 ; |        |            $fe3a (P)
    
Lfe3b
    .byte   $02,$01                         ; $fe3b (*)
    
    .byte   %11110000 ; |****    |            $fe3d (P)
    .byte   %10000000 ; |*       |            $fe3e (P)
    .byte   %00000000 ; |        |            $fe3f (P)
    .byte   %00000000 ; |        |            $fe40 (P)
    .byte   %00000000 ; |        |            $fe41 (P)
    .byte   %00000000 ; |        |            $fe42 (P)
Lfe43
    .byte   %00000000 ; |        |            $fe43 (P)
    .byte   %00000000 ; |        |            $fe44 (P)
    .byte   %11111111 ; |********|            $fe45 (P)
    .byte   %11111111 ; |********|            $fe46 (P)
    .byte   %01111111 ; | *******|            $fe47 (P)
    .byte   %00011111 ; |   *****|            $fe48 (P)
    .byte   %00000111 ; |     ***|            $fe49 (P)
    .byte   %00000001 ; |       *|            $fe4a (P)
    .byte   %00000000 ; |        |            $fe4b (P)
    .byte   %00000000 ; |        |            $fe4c (P)
    .byte   %11111111 ; |********|            $fe4d (P)
    .byte   %11111111 ; |********|            $fe4e (P)
    .byte   %11111111 ; |********|            $fe4f (P)
    .byte   %11111111 ; |********|            $fe50 (P)
    .byte   %11111111 ; |********|            $fe51 (P)
    .byte   %11111111 ; |********|            $fe52 (P)
    .byte   %11111100 ; |******  |            $fe53 (P)
    .byte   %00000000 ; |        |            $fe54 (P)
    
Lfe55
    .byte   PURPLE|$6                       ; $fe55 (CP)
    .byte   PURPLE|$5                       ; $fe56 (CP)
    .byte   PURPLE|$4                       ; $fe57 (CP)
    .byte   PURPLE|$6                       ; $fe58 (CP)
    .byte   PURPLE|$5                       ; $fe59 (CP)
    .byte   PURPLE|$4                       ; $fe5a (CP)
    .byte   PURPLE|$3                       ; $fe5b (CP)
    .byte   PURPLE|$3                       ; $fe5c (CP)
    .byte   PURPLE|$2                       ; $fe5d (CP)
    .byte   PURPLE|$1                       ; $fe5e (CP)
    .byte   PURPLE|$0                       ; $fe5f (CP)
    .byte   PURPLE|$2                       ; $fe60 (CP)
    .byte   PURPLE|$0                       ; $fe61 (CP)
    .byte   PURPLE|$1                       ; $fe62 (CP)
    .byte   PURPLE|$2                       ; $fe63 (CP)
    .byte   PURPLE|$3                       ; $fe64 (CP)
    
Lfe65
    lda     #$00
    ldx     #$77
Lfe69
    sta     rngSeed,x
    dex
    bne     Lfe69
Lfe6e
    ldx     #$21
Lfe70
    lda     Lfd64,x
    sta     rngSeed,x
    dex
    bpl     Lfe70
Lfe78
    rts
    
    .byte   %10001000 ; |#   #   |            $fe79 (G)
    .byte   %11001100 ; |##  ##  |            $fe7a (G)
    .byte   %11101110 ; |### ### |            $fe7b (G)
    .byte   %11111110 ; |####### |            $fe7c (G)
    .byte   %11111111 ; |########|            $fe7d (G)
    .byte   %11111011 ; |##### ##|            $fe7e (G)
    .byte   %10111011 ; |# ### ##|            $fe7f (G)
    .byte   %10111011 ; |# ### ##|            $fe80 (G)
    .byte   %01011001 ; | # ##  #|            $fe81 (G)
Lfe82
    .byte   %00010001 ; |   #   #|            $fe82 (G)
    .byte   %11011101 ; |## ### #|            $fe83 (G)
    .byte   %11011101 ; |## ### #|            $fe84 (G)
    .byte   %10111011 ; |# ### ##|            $fe85 (G)
    .byte   %11011011 ; |## ## ##|            $fe86 (G)
    .byte   %11111011 ; |##### ##|            $fe87 (G)
    .byte   %11111111 ; |########|            $fe88 (G)
    .byte   %11111110 ; |####### |            $fe89 (G)
    .byte   %11101111 ; |### ####|            $fe8a (G)
    .byte   %11001101 ; |##  ## #|            $fe8b (G)
Lfe8c
    .byte   %10001001 ; |#   #  #|            $fe8c (G)
    
Lfe8d
    .byte   $01,$e0,$e0,$f2,$00,$11,$20,$21 ; $fe8d (D)
    .byte   $e2                             ; $fe95 (D)
Lfe96
    .byte   $20,$02,$e1,$22,$e0,$e1,$f0,$01 ; $fe96 (D)
    .byte   $12,$21                         ; $fe9e (D)
Lfea0
    .byte   $22                             ; $fea0 (D)
    
    .byte   ORANGE|$2                       ; $fea1 (C)
    .byte   BROWN|$2                        ; $fea2 (C)
    .byte   YELLOW|$2                       ; $fea3 (C)
    .byte   YELLOW|$2                       ; $fea4 (C)
    .byte   BLACK|$4                        ; $fea5 (C)
    .byte   BLACK|$4                        ; $fea6 (C)
    .byte   BLACK|$4                        ; $fea7 (C)
    .byte   BLACK|$4                        ; $fea8 (C)
    .byte   BLACK|$4                        ; $fea9 (C)
    .byte   BLACK|$4                        ; $feaa (C)
    
Lfeab
    .byte   $04,$00,$03,$07,$10,$13,$15,$22 ; $feab (*)
    .byte   $40,$41,$65,$66,$70,$72         ; $feb3 (*)
Lfeb9
    .byte   $76,$77                         ; $feb9 (D)
    .byte   $80                             ; $febb (*)
    .byte   $85,$88,$89                     ; $febc (D)
Lfebf
    .byte   $e6,$32                         ; $febf (D)
    .byte   $f4                             ; $fec1 (*)
    .byte   $00,$24,$20                     ; $fec2 (D)
Lfec5
    .byte   $36,$46                         ; $fec5 (D)
    .byte   $90                             ; $fec7 (*)
    .byte   $90,$90                         ; $fec8 (D)
    
    .byte   BLUE_CYAN|$0                    ; $feca (CB)
    
    .byte   $90                             ; $fecb (D)
    
    .byte   BLUE_CYAN|$0                    ; $fecc (CB)
    
    .byte   $80                             ; $fecd (D)
    .byte   $60,$70,$82,$62,$64,$84,$86     ; $fece (*)
    
Lfed5
    inc     ram_A6
    bne     Lfedc
    dec     ram_A6
    rts
    
Lfedc
    sed
    lda     ram_96
    clc
    adc     #$01
    sta     ram_96
    lda     ram_97
    adc     #$00
    bcc     Lfefd
Lfeea
    dec     ram_A6
    bne     Lfef1
    inc     ram_A6
    rts
    
Lfef1
    sed
    sec
    lda     ram_96
    sbc     #$01
    sta     ram_96
    lda     ram_97
    sbc     #$00
Lfefd
    sta     ram_97
    cld
Lff00
    rts
    
Lff01
    .byte   %11000000 ; |##      |            $ff01 (G)
    .byte   %10000000 ; |#       |            $ff02 (G)
    .byte   %10000000 ; |#       |            $ff03 (G)
satelliteGfx
    .byte   %00000000 ; |        |            $ff04 (G)
    .byte   %00000000 ; |        |            $ff05 (G)
    .byte   %00000000 ; |        |            $ff06 (G)
    .byte   %00000000 ; |        |            $ff07 (G)
    .byte   %00000000 ; |        |            $ff08 (G)
    .byte   %00000000 ; |        |            $ff09 (G)
    .byte   %00000000 ; |        |            $ff0a (G)
    .byte   %00000000 ; |        |            $ff0b (G)
    .byte   %00000000 ; |        |            $ff0c (G)
    .byte   %00000000 ; |        |            $ff0d (G)
    .byte   %00000000 ; |        |            $ff0e (G)
    .byte   %00000000 ; |        |            $ff0f (G)
    .byte   %00000000 ; |        |            $ff10 (G)
    .byte   %00000000 ; |        |            $ff11 (G)
    .byte   %00000000 ; |        |            $ff12 (G)
    .byte   %00000000 ; |        |            $ff13 (G)
    .byte   %00000000 ; |        |            $ff14 (G)
    .byte   %00000000 ; |        |            $ff15 (G)
    .byte   %00000000 ; |        |            $ff16 (G)
    .byte   %00000000 ; |        |            $ff17 (G)
    .byte   %00000000 ; |        |            $ff18 (G)
    .byte   %00000000 ; |        |            $ff19 (G)
    .byte   %00000000 ; |        |            $ff1a (G)
    .byte   %00000000 ; |        |            $ff1b (G)
    .byte   %00000000 ; |        |            $ff1c (G)
    .byte   %00010000 ; |   #    |            $ff1d (G)
    .byte   %00010000 ; |   #    |            $ff1e (G)
    .byte   %00000000 ; |        |            $ff1f (G)
    .byte   %00000000 ; |        |            $ff20 (G)
    .byte   %00000000 ; |        |            $ff21 (G)
    .byte   %00000000 ; |        |            $ff22 (G)
    .byte   %00000000 ; |        |            $ff23 (G)
    .byte   %00000000 ; |        |            $ff24 (G)
    .byte   %00000000 ; |        |            $ff25 (G)
    .byte   %00000000 ; |        |            $ff26 (G)
    .byte   %00000000 ; |        |            $ff27 (G)
    .byte   %00000000 ; |        |            $ff28 (G)
    .byte   %00000000 ; |        |            $ff29 (G)
    .byte   %00000000 ; |        |            $ff2a (G)
    .byte   %00000000 ; |        |            $ff2b (G)
    .byte   %00000000 ; |        |            $ff2c (G)
    .byte   %00000000 ; |        |            $ff2d (G)
    .byte   %00000000 ; |        |            $ff2e (G)
    .byte   %00000000 ; |        |            $ff2f (G)
    .byte   %00000000 ; |        |            $ff30 (G)
    .byte   %00000000 ; |        |            $ff31 (G)
    .byte   %00000000 ; |        |            $ff32 (G)
    .byte   %00000000 ; |        |            $ff33 (G)
    .byte   %00000000 ; |        |            $ff34 (G)
    .byte   %00000000 ; |        |            $ff35 (G)
    .byte   %00000000 ; |        |            $ff36 (G)
    .byte   %00000000 ; |        |            $ff37 (G)
    .byte   %00000000 ; |        |            $ff38 (G)
    .byte   %00000000 ; |        |            $ff39 (G)
    .byte   %00000000 ; |        |            $ff3a (G)
    .byte   %00010000 ; |   #    |            $ff3b (G)
    .byte   %00011000 ; |   ##   |            $ff3c (G)
    .byte   %00010000 ; |   #    |            $ff3d (G)
    .byte   %00101000 ; |  # #   |            $ff3e (G)
    .byte   %00000000 ; |        |            $ff3f (G)
    .byte   %00000000 ; |        |            $ff40 (G)
    .byte   %00000000 ; |        |            $ff41 (G)
    .byte   %00000000 ; |        |            $ff42 (G)
    .byte   %00000000 ; |        |            $ff43 (G)
    .byte   %00000000 ; |        |            $ff44 (G)
    .byte   %00000000 ; |        |            $ff45 (G)
    .byte   %00000000 ; |        |            $ff46 (G)
    .byte   %00000000 ; |        |            $ff47 (G)
    .byte   %00000000 ; |        |            $ff48 (G)
    .byte   %00000000 ; |        |            $ff49 (G)
    .byte   %00000000 ; |        |            $ff4a (G)
    .byte   %00000000 ; |        |            $ff4b (G)
    .byte   %00000000 ; |        |            $ff4c (G)
    .byte   %00000000 ; |        |            $ff4d (G)
    .byte   %00000000 ; |        |            $ff4e (G)
    .byte   %00000000 ; |        |            $ff4f (G)
    .byte   %00000000 ; |        |            $ff50 (G)
    .byte   %00000000 ; |        |            $ff51 (G)
    .byte   %00000000 ; |        |            $ff52 (G)
    .byte   %00000000 ; |        |            $ff53 (G)
    .byte   %00000000 ; |        |            $ff54 (G)
    .byte   %00000000 ; |        |            $ff55 (G)
    .byte   %00000000 ; |        |            $ff56 (G)
    .byte   %00000000 ; |        |            $ff57 (G)
    .byte   %00000000 ; |        |            $ff58 (G)
    .byte   %00000000 ; |        |            $ff59 (G)
    .byte   %00000000 ; |        |            $ff5a (G)
    .byte   %00011000 ; |   ##   |            $ff5b (G)
    .byte   %00010000 ; |   #    |            $ff5c (G)
    .byte   %00111000 ; |  ###   |            $ff5d (G)
    .byte   %00010000 ; |   #    |            $ff5e (G)
    .byte   %00101000 ; |  # #   |            $ff5f (G)
    .byte   %00000000 ; |        |            $ff60 (G)
    .byte   %00000000 ; |        |            $ff61 (G)
    .byte   %00000000 ; |        |            $ff62 (G)
    .byte   %00000000 ; |        |            $ff63 (G)
    .byte   %00000000 ; |        |            $ff64 (G)
    .byte   %00000000 ; |        |            $ff65 (G)
    .byte   %00000000 ; |        |            $ff66 (G)
    .byte   %00000000 ; |        |            $ff67 (G)
    .byte   %00000000 ; |        |            $ff68 (G)
    .byte   %00000000 ; |        |            $ff69 (G)
    .byte   %00000000 ; |        |            $ff6a (G)
    .byte   %00000000 ; |        |            $ff6b (G)
    .byte   %00000000 ; |        |            $ff6c (G)
    .byte   %00000000 ; |        |            $ff6d (G)
    .byte   %00000000 ; |        |            $ff6e (G)
    .byte   %00000000 ; |        |            $ff6f (G)
    .byte   %00000000 ; |        |            $ff70 (G)
    .byte   %00000000 ; |        |            $ff71 (G)
    .byte   %00000000 ; |        |            $ff72 (G)
    .byte   %00000000 ; |        |            $ff73 (G)
    .byte   %00000000 ; |        |            $ff74 (G)
    .byte   %00000000 ; |        |            $ff75 (G)
    .byte   %00000000 ; |        |            $ff76 (G)
    .byte   %00000000 ; |        |            $ff77 (G)
    .byte   %00000000 ; |        |            $ff78 (G)
    .byte   %00000000 ; |        |            $ff79 (G)
    .byte   %00011000 ; |   ##   |            $ff7a (G)
    .byte   %00010000 ; |   #    |            $ff7b (G)
    .byte   %01010100 ; | # # #  |            $ff7c (G)
    .byte   %00111000 ; |  ###   |            $ff7d (G)
    .byte   %00010000 ; |   #    |            $ff7e (G)
    .byte   %00101000 ; |  # #   |            $ff7f (G)
    .byte   %01000100 ; | #   #  |            $ff80 (G)
    .byte   %00000000 ; |        |            $ff81 (G)
    .byte   %00000000 ; |        |            $ff82 (G)
    .byte   %00000000 ; |        |            $ff83 (G)
    .byte   %00000000 ; |        |            $ff84 (G)
    .byte   %00000000 ; |        |            $ff85 (G)
    .byte   %00000000 ; |        |            $ff86 (G)
    .byte   %00000000 ; |        |            $ff87 (G)
    .byte   %00000000 ; |        |            $ff88 (G)
    .byte   %00000000 ; |        |            $ff89 (G)
    .byte   %00000000 ; |        |            $ff8a (G)
    .byte   %00000000 ; |        |            $ff8b (G)
    .byte   %00000000 ; |        |            $ff8c (G)
    .byte   %00000000 ; |        |            $ff8d (G)
    .byte   %00000000 ; |        |            $ff8e (G)
    .byte   %00000000 ; |        |            $ff8f (G)
    .byte   %00000000 ; |        |            $ff90 (G)
    .byte   %00000000 ; |        |            $ff91 (G)
    .byte   %00000000 ; |        |            $ff92 (G)
    .byte   %00000000 ; |        |            $ff93 (G)
    .byte   %00000000 ; |        |            $ff94 (G)
    .byte   %00000000 ; |        |            $ff95 (G)
    .byte   %00000000 ; |        |            $ff96 (G)
    .byte   %00000000 ; |        |            $ff97 (G)
    .byte   %00000000 ; |        |            $ff98 (G)
    .byte   %00010000 ; |   #    |            $ff99 (G)
    .byte   %00011000 ; |   ##   |            $ff9a (G)
    .byte   %00010000 ; |   #    |            $ff9b (G)
    .byte   %00010000 ; |   #    |            $ff9c (G)
    .byte   %11010110 ; |## # ## |            $ff9d (G)
    .byte   %00111000 ; |  ###   |            $ff9e (G)
    .byte   %00010000 ; |   #    |            $ff9f (G)
    .byte   %00101000 ; |  # #   |            $ffa0 (G)
    .byte   %11000110 ; |##   ## |            $ffa1 (G)
    .byte   %00000000 ; |        |            $ffa2 (G)
    .byte   %00000000 ; |        |            $ffa3 (G)
    .byte   %00000000 ; |        |            $ffa4 (G)
    .byte   %00000000 ; |        |            $ffa5 (G)
    .byte   %00000000 ; |        |            $ffa6 (G)
    .byte   %00000000 ; |        |            $ffa7 (G)
    .byte   %00000000 ; |        |            $ffa8 (G)
    .byte   %00000000 ; |        |            $ffa9 (G)
    .byte   %00000000 ; |        |            $ffaa (G)
    .byte   %00000000 ; |        |            $ffab (G)
    .byte   %00000000 ; |        |            $ffac (G)
    .byte   %00000000 ; |        |            $ffad (G)
    .byte   %00000000 ; |        |            $ffae (G)
    .byte   %00000000 ; |        |            $ffaf (G)
    .byte   %00000000 ; |        |            $ffb0 (G)
    .byte   %00000000 ; |        |            $ffb1 (G)
    .byte   %00000000 ; |        |            $ffb2 (G)
    .byte   %00000000 ; |        |            $ffb3 (G)
    .byte   %00000000 ; |        |            $ffb4 (G)
    .byte   %00000000 ; |        |            $ffb5 (G)
    .byte   %00000000 ; |        |            $ffb6 (G)
    .byte   %00000000 ; |        |            $ffb7 (G)
    .byte   %00000000 ; |        |            $ffb8 (G)
    .byte   %00010000 ; |   #    |            $ffb9 (G)
    .byte   %00011000 ; |   ##   |            $ffba (G)
    .byte   %00111000 ; |  ###   |            $ffbb (G)
    .byte   %00010000 ; |   #    |            $ffbc (G)
    .byte   %00010000 ; |   #    |            $ffbd (G)
    .byte   %01010100 ; | # # #  |            $ffbe (G)
    .byte   %00111000 ; |  ###   |            $ffbf (G)
    .byte   %00010000 ; |   #    |            $ffc0 (G)
    .byte   %00101000 ; |  # #   |            $ffc1 (G)
    .byte   %01000100 ; | #   #  |            $ffc2 (G)
    .byte   %00000000 ; |        |            $ffc3 (G)
    .byte   %00000000 ; |        |            $ffc4 (G)
    .byte   %00000000 ; |        |            $ffc5 (G)
    .byte   %00000000 ; |        |            $ffc6 (G)
    .byte   %00000000 ; |        |            $ffc7 (G)
    .byte   %00000000 ; |        |            $ffc8 (G)
    .byte   %00000000 ; |        |            $ffc9 (G)
    .byte   %00000000 ; |        |            $ffca (G)
    .byte   %00000000 ; |        |            $ffcb (G)
    .byte   %00000000 ; |        |            $ffcc (G)
    .byte   %00000000 ; |        |            $ffcd (G)
    .byte   %00000000 ; |        |            $ffce (G)
    .byte   %00000000 ; |        |            $ffcf (G)
    .byte   %00000000 ; |        |            $ffd0 (G)
    .byte   %00000000 ; |        |            $ffd1 (G)
    .byte   %00000000 ; |        |            $ffd2 (G)
    .byte   %00000000 ; |        |            $ffd3 (G)
    .byte   %00000000 ; |        |            $ffd4 (G)
    .byte   %00000000 ; |        |            $ffd5 (G)
    .byte   %00000000 ; |        |            $ffd6 (G)
    .byte   %00000000 ; |        |            $ffd7 (G)
    .byte   %00010000 ; |   #    |            $ffd8 (G)
    .byte   %00111000 ; |  ###   |            $ffd9 (G)
    .byte   %00011000 ; |   ##   |            $ffda (G)
    .byte   %00111000 ; |  ###   |            $ffdb (G)
    .byte   %00010000 ; |   #    |            $ffdc (G)
    .byte   %10010010 ; |#  #  # |            $ffdd (G)
    .byte   %01010100 ; | # # #  |            $ffde (G)
    .byte   %00111000 ; |  ###   |            $ffdf (G)
    .byte   %00010000 ; |   #    |            $ffe0 (G)
    .byte   %00101000 ; |  # #   |            $ffe1 (G)
    .byte   %01000100 ; | #   #  |            $ffe2 (G)
    .byte   %10000010 ; |#     # |            $ffe3 (G)
    .byte   %00000000 ; |        |            $ffe4 (G)
    .byte   %00000000 ; |        |            $ffe5 (G)
    .byte   %00000000 ; |        |            $ffe6 (G)
    .byte   %00000000 ; |        |            $ffe7 (G)
    .byte   %00000000 ; |        |            $ffe8 (G)
    .byte   %00000000 ; |        |            $ffe9 (G)
    .byte   %00000000 ; |        |            $ffea (G)
    .byte   %00000000 ; |        |            $ffeb (G)
    .byte   %00000000 ; |        |            $ffec (G)
    .byte   %00000000 ; |        |            $ffed (G)
    .byte   %00000000 ; |        |            $ffee (G)
    .byte   %00000000 ; |        |            $ffef (G)
    .byte   %00000000 ; |        |            $fff0 (G)
    .byte   %00000000 ; |        |            $fff1 (G)
    .byte   %00000000 ; |        |            $fff2 (G)
    .byte   %00000000 ; |        |            $fff3 (G)
    .byte   %00000000 ; |        |            $fff4 (G)
    .byte   %00000000 ; |        |            $fff5 (G)
    .byte   %00000000 ; |        |            $fff6 (G)
    .byte   %00000000 ; |        |            $fff7 (G)
Lfff8
    .byte   $00                             ; $fff8 (D)
    .byte   $00,$00,$00                     ; $fff9 (*)
    .byte	$03,$f0
	.byte	$00,$00                 ; $fffc (D)
