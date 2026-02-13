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
ram_81          = $81
ram_82          = $82
ram_83          = $83
ram_84          = $84
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
ram_CC          = $cc
ram_CD          = $cd
ram_CE          = $ce
ram_CF          = $cf
ram_D0          = $d0
ram_D1          = $d1
ram_D2          = $d2
ram_D3          = $d3
ram_D4          = $d4
ram_D5          = $d5
ram_D6          = $d6
ram_D7          = $d7
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
ram_E4          = $e4

ram_E6          = $e6; (s)
ram_E7          = $e7
ram_E8          = $e8
ram_E9          = $e9
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
ram_FD          = $fd
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
    RORG    $f000

resetBank0
    bit     bank1Strobe                     ; $f000 (*)
    
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
    lda     ram_82
    sta     HMCLR
    jsr     $d958
    lda     ram_83
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
    bcc     Lf0a3
    lda     ram_B5
    cmp     #$02
    bne     Lf0a3
    lda     ram_C1
    and     $debd,x
    bne     Lf0a3
    bit     rngSeed
    bmi     Lf0a1
    dec     ram_B2
    .byte   $2c ;bit                ;4-5 =  60 *
Lf0a1
    inc     ram_B2
Lf0a3
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
Lf0bf
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
    bne     Lf0bf
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
    beq     Lf11d
    lda     ram_C1
    and     #$20
    beq     Lf11d
    ldx     #$82
Lf11d
    stx     COLUP1
    ldx     #$08
    lda     ram_81
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sec
Lf128
    sbc     #$0f
    bcs     Lf128
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
    bne     Lf180
    ldx     ram_E2
    cpx     #$ff
    bne     Lf180
    inc     ram_E2
    lda     rngSeed
    sta     ram_B2
Lf180
    ldx     #BLACK|$4
Lf182
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    dex
    bne     Lf182
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
    bcs     Lf1cd
    lda     ram_C1
    and     #$10
    beq     Lf1cd
    ldx     #$86
    stx     COLUPF
    ldy     #$80
    bne     Lf1e1
Lf1cd
    stx     COLUPF
    ldy     #BLACK|$a
    lda     ram_85
    cmp     #$19
    bcc     Lf1df
    beq     Lf1e1
    lda     ram_C1
    and     #$10
    beq     Lf1e1
Lf1df
    ldy     #$58
Lf1e1
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
    bne     Lf217
    lda     ram_85
    cmp     #$17
    beq     Lf214
    cmp     #$05
    bcs     Lf217
Lf214
    asl
    sta     ENAM0
Lf217
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
    sta     ram_CD
    lda     ram_84
    and     #$01
    beq     Lf252
    lda     #$7d
Lf252
    ldx     #$08
    clc
    sta     HMCLR
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
Lf25b
    sta     ram_CE,x
    adc     #$19
    dex
    dex
    bpl     Lf25b
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     ram_84
    lsr
    clc
    adc     #$da
    sta     ram_D7
    sta     ram_D5
    sta     ram_D3
    sta     ram_D1
    sta     ram_CF
    lda     ram_C1
    ldx     ram_84
    bit     ram_E1
    bpl     Lf285
    and     #$fc
    beq     Lf28a
    bne     Lf290
Lf285
    and     $d9f8,x
    beq     Lf290
Lf28a
    lda     #$e5
    sec
    sbc     ram_AC
    .byte   $2c ;bit                ;4-2 =   9 *
Lf290
    lda     #$cc
    sta     ram_CC
    lda     #$06
    sta     PF2
kernelDrawDashboard
    lda     (ram_CC),y
    sta     ENABL
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sty     ram_FB
    lda     (ram_D6),y
    sta     GRP0
    lda     (ram_D4),y
    sta     GRP1
    lda     (ram_D2),y
    sta     GRP0
    lda     (ram_D0),y
    tax
    lda     (ram_CE),y
    ldy     #$00
    stx     GRP1
    sta     GRP0
    sty     GRP1
    sty     GRP0
    ldy     ram_FB
    dey
    bpl     kernelDrawDashboard
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
    bcs     Lf2f4
    ldy     #$00
    cmp     #$0c
    bcc     Lf2f4
    sbc     #$0c
    tay
Lf2f4
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
Lf304
    sta     ram_CE,x
    sbc     #$08
    sta     ram_CC,x
    sbc     #$08
    dex
    dex
    dex
    dex
    bpl     Lf304
    sta     HMCLR
    sta     HMOVE
    ldx     #$00
    stx     PF2
    stx     COLUPF
    lda     #BLACK|$8
    sta     COLUBK
    lda     #$d8
    sta     ram_CD
    sta     ram_CF
    sta     ram_D1
    sta     ram_D3
    sta     ram_D5
    sta     ram_D7
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
    beq     Lf3aa
    lda     ram_C1
    and     #$02
    bne     Lf3ac
    inx
Lf3aa
    stx     WSYNC
;---------------------------------------
Lf3ac
    stx     WSYNC
;---------------------------------------
    cpx     #$05
    beq     Lf3b4
    stx     WSYNC
;---------------------------------------
Lf3b4
    ldx     #$dc
    stx     TIM8T
    bit     ram_F9
    bmi     Lf403
    lda     ram_B6
    beq     Lf403
    lda     ram_A4
    beq     Lf403
    ldy     ram_84
    beq     Lf3cf
    cpy     #$03
    bne     Lf403
    adc     #$14
Lf3cf
    tax
    lda     $d868,x
    ldx     ram_F8
    inx
    clc
    adc     $dafa,x
    cmp     ram_F6
    bcc     Lf3e7
    sbc     $d9e6,x
    cmp     ram_F6
    bcc     Lf3f9
    beq     Lf3f9
Lf3e7
    inc     ram_F5
    ldx     ram_E3
    cpx     #$06
    bne     Lf3f1
    inc     ram_F5
Lf3f1
    lda     #$41
    ldx     ram_CA
    bne     Lf403
    beq     Lf401
Lf3f9
    lda     #$00
    ldx     ram_CA
    cpx     #$41
    bne     Lf403
Lf401
    sta     ram_CA
Lf403
    ldx     #$00
    lda     ram_81
    cmp     ram_83
    bcc     Lf411
    sbc     #$0b
    cmp     ram_83
    bcc     Lf412
Lf411
    dex
Lf412
    stx     ram_A1
    lda     SWCHA
    cmp     #$ff
    beq     Lf422
    ldx     #$ff
    stx     ram_F7
    inx
    stx     ram_B8
Lf422
    inc     ram_B9
    bne     Lf444
    lda     ram_BA
    cmp     #$0f
    bcc     Lf430
    ldx     ram_B6
    stx     ram_B7
Lf430
    inc     ram_BA
    bne     Lf444
    inc     ram_B8
    lda     ram_B8
    and     #$04
    beq     Lf444
    sta     ram_B8
    lda     #$00
    sta     ram_B6
    sta     ram_B7
Lf444
    lda     ram_84
    cmp     #$04
    bne     Lf45f
    lda     ram_E9
    bmi     Lf45f
    dec     ram_EE
    bne     Lf45f
    asl
    ora     #$02
    sta     ram_EE
    lda     ram_CA
    bne     Lf45f
    lda     #$34
    sta     ram_CA
Lf45f
    lda     ram_C1
    and     #$07
    bne     Lf48b
    bit     ram_F9
    bpl     Lf481
    lda     ram_B5
    cmp     #$02
    bne     Lf477
    dec     ram_88
    bpl     Lf481
Lf473
    inc     ram_88
    bpl     Lf481
Lf477
    cmp     #$04
    beq     Lf481
    lda     ram_88
    cmp     #$0d
    bne     Lf473
Lf481
    lda     ram_B7
    beq     Lf489
    ldx     #$01
    stx     ram_C8
Lf489
    dec     ram_C8
Lf48b
    ldx     ram_C9
    cpx     #$fe
    bne     Lf495
    stx     ram_CB
    inc     ram_C9
Lf495
    lda     ram_CA
    beq     Lf4c8
    sta     AUDC0
    jsr     $ddfb
    tax
    lda     $d94c,x
    inc     ram_CB
    cmp     ram_CB
    bcs     Lf4d0
    lda     #$00
    sta     ram_CB
    inc     ram_C9
    lda     $d8f7,x
    adc     ram_C9
    tax
    lda     $d903,x
    beq     Lf4c8
    sta     AUDF0
    jsr     $ddfb
    ldx     ram_B6
    bne     Lf4c3
    txa
Lf4c3
    sta     AUDV0
    jmp     $d4d0
    
Lf4c8
    sta     AUDV0
    sta     ram_CA
    ldx     #$fe
    stx     ram_C9
Lf4d0
    lda     ram_B6
    beq     Lf502
    
    .byte   $a9,$03,$85,$1a,$a9,$06,$85,$16 ; $f4d4 (*)
    .byte   $a9,$1f,$85,$18,$a5,$81,$c9,$0f ; $f4dc (*)
    .byte   $d0,$04,$a6,$b4,$f0,$1c,$a2,$08 ; $f4e4 (*)
    .byte   $86,$16,$20,$fb,$dd,$18,$69,$02 ; $f4ec (*)
    .byte   $65,$b4,$a6,$ca,$d0,$0a,$a2,$02 ; $f4f4 (*)
    .byte   $86,$15,$a2,$0e,$86,$17         ; $f4fc (*)
    
Lf502
    sta     AUDV0
    sta     AUDV1
    lda     ram_B7
    beq     Lf52a
    lda     ram_B5
    bne     Lf514
    jmp     $d728
    
Lf511
    jmp     $d647
    
Lf514
    .byte   $a5,$b4,$f0,$06,$a9,$02,$85,$ab ; $f514 (*)
    .byte   $d0,$1e,$a5,$c4,$d0,$1a,$a5,$a6 ; $f51c (*)
    .byte   $c9,$df,$b0,$02,$a9,$e0         ; $f524 (*)
    
Lf52a
    sbc     #$08
    eor     #$ff
    bit     ram_F9
    bpl     Lf534
    
    .byte   $a9,$18                         ; $f532 (*)
    
Lf534
    sta     ram_FA
    ldx     ram_AF
    beq     Lf53c
    
    .byte   $e6,$be                         ; $f53a (*)
    
Lf53c
    inc     ram_BE
    cmp     ram_BE
    bcs     Lf511
    lda     #$00
    sta     ram_BE
    inc     ram_B1
    bpl     Lf554
    
    .byte   $a5,$ea,$c9,$08,$f0,$08         ; $f54a (*)
    
Lf550
    inc     ram_EA
    bpl     Lf558
Lf554
    dec     ram_EA
    bmi     Lf550
Lf558
    inc     ram_E0
    lda     ram_B5
    cmp     #$02
    bne     Lf564
    
    .byte   $24,$f9,$30,$05                 ; $f560 (*)
    
Lf564
    lda     ram_E0
    lsr
    bcc     Lf56b
    inc     ram_B0
Lf56b
    inc     ram_EF
    lda     ram_B7
    beq     Lf577
    
    .byte   $a5,$ab,$29,$01,$f0,$1d         ; $f571 (*)
    
Lf577
    dec     ram_89
    lda     ram_89
    cmp     #$0e
    bne     Lf594
    lda     #$14
    sta     ram_89
    ldx     #$0b
    lda     ram_8A,x
    tay
    dex
    bmi     Lf592
    lda     ram_8A,x
    sty     ram_8A,x
    jmp     $d587
    
Lf592
    sty     ram_95
Lf594
    lda     ram_AB
    and     #$08
    beq     Lf5ab
    
    .byte   $a2,$0b,$d6,$8a,$b5,$8a,$c9,$ff ; $f59a (*)
    .byte   $d0,$04,$a9,$84,$95,$8a,$ca,$10 ; $f5a2 (*)
    .byte   $f1                             ; $f5aa (*)
    
Lf5ab
    lda     ram_AB
    and     #$02
    beq     Lf5d4
    
    .byte   $e6,$89,$a5,$89,$c9,$15,$d0,$17 ; $f5b1 (*)
    .byte   $a9,$0f,$85,$89,$a2,$00,$b5,$8a ; $f5b9 (*)
    .byte   $a8,$e8,$e0,$0c,$f0,$07,$b5,$8a ; $f5c1 (*)
    .byte   $94,$8a,$4c,$c1,$d5,$85,$8a,$c6 ; $f5c9 (*)
    .byte   $ef,$c6,$ef                     ; $f5d1 (*)
    
Lf5d4
    lda     ram_AB
    and     #$04
    beq     Lf5eb
    
    .byte   $a2,$0b,$f6,$8a,$b5,$8a,$c9,$85 ; $f5da (*)
    .byte   $90,$04,$a9,$00,$95,$8a,$ca,$10 ; $f5e2 (*)
    .byte   $f1                             ; $f5ea (*)
    
Lf5eb
    lda     ram_B7
    beq     Lf5f5
    
    .byte   $a5,$b5,$c9,$02,$f0,$03         ; $f5ef (*)
    
Lf5f5
    jmp     $d728
    
    .byte   $a0,$01,$a5,$fa,$a6,$e2,$e0,$ff ; $f5f8 (*)
    .byte   $d0,$02,$a9,$28,$38,$e9,$18,$f0 ; $f600 (*)
    .byte   $3e,$90,$04,$c8,$49,$ff,$18,$69 ; $f608 (*)
    .byte   $12,$e6,$bf,$c5,$bf,$f0,$02,$b0 ; $f610 (*)
    .byte   $2e,$a9,$00,$85,$bf,$c0,$01,$f0 ; $f618 (*)
    .byte   $03,$e6,$b0,$2c,$c6,$b0,$a6,$e3 ; $f620 (*)
    .byte   $a5,$bd,$e6,$bd,$dd,$b1,$df,$d0 ; $f628 (*)
    .byte   $16,$a9,$00,$85,$bd,$a5,$80,$dd ; $f630 (*)
    .byte   $fa,$dc,$b0,$09,$0a,$0a,$90,$03 ; $f638 (*)
    .byte   $c6,$b2,$2c,$e6,$b2,$84,$f4     ; $f640 (*)
    
Lf647
    lda     ram_B0
    sec
    sbc     ram_B1
    cmp     #$10
    bcc     Lf653
    jmp     $d6d2
    
Lf653
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
    bcc     Lf66b
    
    .byte   $a9,$9f                         ; $f669 (*)
    
Lf66b
    sta     COLUP0
    ldy     #$00
    lda     ram_B2
    clc
    adc     #$50
    cmp     #$a0
    bcs     Lf684
    adc     #$b0
    bcs     Lf681
    
    .byte   $c8,$49,$ff,$69,$01             ; $f67c (*)
    
Lf681
    cmp     $dfe6,x
Lf684
    bcs     Lf6d0
    cpx     #$00
    beq     Lf696
    
    .byte   $86,$fc,$e0,$06,$90,$01,$98,$4a ; $f68a (*)
    .byte   $c6,$fc,$d0,$fb                 ; $f692 (*)
    
Lf696
    sta     ram_FA
    lda     ram_B2
    cpy     #$01
    beq     Lf6a2
    clc
    adc     ram_FA
    .byte   $2c ;bit                ;4-3 =  22
Lf6a2
    sbc     ram_FA
    clc
    adc     #$50
    cmp     ram_DF
    beq     Lf6b8
    lda     ram_C1
    and     #$03
    bne     Lf6b8
    bcs     Lf6b6
    
    .byte   $c6,$df,$2c                     ; $f6b3 (*)
    
Lf6b6
    inc     ram_DF
Lf6b8
    ldy     ram_DF
    lda     ram_C1
    lsr
    sta     REFP0
    and     #$08
    bne     Lf6c7
    txa
    ora     #$08
    tax
Lf6c7
    sec
    tya
    sbc     $d9ca,x
    bcc     Lf728
    
    .byte   $b0,$3b                         ; $f6ce (*)
Lf6d0
    .byte   $b0,$56                         ; $f6d0 (*)
    
Lf6d2
    cmp     #$80
    bcc     Lf6de
    ldx     #$ce
    ldy     #$05
    lda     #BLUE_CYAN|$d
    bcs     Lf6e8
    
Lf6de
    .byte   $c9,$40,$90,$46,$a2,$0e,$a0,$00 ; $f6de (*)
    .byte   $a9,$90                         ; $f6e6 (*)
    
Lf6e8
    stx     ram_FB
    sty     NUSIZ0
    sta     COLUP0
    lda     ram_DF
    beq     Lf728
    cmp     #$94
    beq     Lf728
    lda     #$94
    ldx     ram_B2
    cpx     #$80
    bcc     Lf700
    
    .byte   $a9,$00                         ; $f6fe (*)
    
Lf700
    cmp     ram_DF
    bcs     Lf707
    
    .byte   $c6,$df,$2c                     ; $f704 (*)
    
Lf707
    inc     ram_DF
    lda     ram_DF
    sta     ram_DE
    lda     ram_B5
    cmp     #$02
    bne     Lf728
    
    .byte   $a5,$88,$d0,$11,$a5,$b3,$69,$0f ; $f713 (*)
    .byte   $c9,$21,$b0,$09,$a5,$b7,$f0,$05 ; $f71b (*)
    .byte   $a5,$a5,$e9,$c2,$2c             ; $f723 (*)
    
Lf728
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
    bpl     Lf756
    
    .byte   $69,$16,$18,$65,$fb,$4c,$58,$d7 ; $f73c (*)
    
Lf744
    lda     ram_FA
    sec
    sbc     #$12
    sta     ram_FA
    bpl     Lf756
    
    .byte   $c9,$e2,$90,$05,$69,$15,$4c,$3e ; $f74d (*)
    .byte   $d7                             ; $f755 (*)
    
Lf756
    lda     #$04
    sta     ram_D8,x
    dex
    bpl     Lf744
    lda     #$ff
    sta     ram_DD
    lda     ram_84
    bne     endOfKernelSwitch
    
Lf765
    .byte   $20,$da,$d9,$b0,$0f,$a5,$aa,$69 ; $f765 (*)
    .byte   $02,$85,$ad,$85,$f6,$a5,$a4     ; $f76d (*)
    
bankSwitch0to1
    sta     ram_AC
    bit     bank1Strobe
    
    .byte   $a5,$b2,$69,$03,$10,$02,$a9,$00 ; $f779 (*)
    .byte   $c9,$08,$90,$02,$a9,$06,$69,$02 ; $f781 (*)
    .byte   $a6,$84,$d0,$02,$69,$16,$85,$ad ; $f789 (*)
    .byte   $a9,$07,$d0,$df                 ; $f791 (*)
    
;-----------------------------------------------------------
;      End of Kernel / Switch to Logic (Bank 0 -> Bank 1)
;-----------------------------------------------------------
endOfKernelSwitch
    cmp     #$03
    beq     Lf765
    cmp     #$02
    bne     Lf7ea
    
    .byte   $20,$da,$d9,$90,$28,$a5,$b2,$e9 ; $f79d (*)
    .byte   $80,$20,$fb,$dd,$18,$e9,$01,$b0 ; $f7a5 (*)
    .byte   $02,$a9,$00,$85,$ad,$a9,$d2,$38 ; $f7ad (*)
    .byte   $e5,$a5,$18,$69,$0b,$c9,$15,$90 ; $f7b5 (*)
    .byte   $09,$a2,$01,$c9,$80,$b0,$02,$a2 ; $f7bd (*)
    .byte   $15,$8a,$4c,$74,$d7,$20,$a3,$d9 ; $f7c5 (*)
    .byte   $4a,$18,$69,$14,$85,$ad,$a5,$b2 ; $f7cd (*)
    .byte   $69,$0e,$c9,$1b,$90,$08,$c9,$8e ; $f7d5 (*)
    .byte   $a9,$1b,$90,$02,$a9,$ff,$69,$01 ; $f7dd (*)
    .byte   $49,$1f,$4a,$10,$57             ; $f7e5 (*)
    
Lf7ea
    cmp     #$01
    bne     Lf808
    
    .byte   $20,$a3,$d9,$20,$da,$d9,$a5,$b0 ; $f7ee (*)
    .byte   $b0,$02,$a5,$b1,$4a,$4a,$4a,$aa ; $f7f6 (*)
    .byte   $18,$69,$05,$85,$ad,$bd,$ec,$de ; $f7fe (*)
    .byte   $d0,$bf                         ; $f806 (*)
    
Lf808
    cmp     #$04
    bne     Lf847
    
    .byte   $20,$da,$d9,$b0,$14,$a5,$e9,$10 ; $f80c (*)
    .byte   $02,$a9,$00,$4a,$4a,$85,$ad,$a5 ; $f814 (*)
    .byte   $a5,$4a,$d0,$02,$a9,$ff,$38,$b0 ; $f81c (*)
    .byte   $1d,$a5,$b2,$69,$4f,$c9,$a0,$90 ; $f824 (*)
    .byte   $02,$e9,$9f,$4a,$4a,$4a,$18,$69 ; $f82c (*)
    .byte   $13,$85,$ad,$a5,$a5,$4a,$f0,$03 ; $f834 (*)
    .byte   $49,$0f,$2c,$a9,$10,$18,$69,$03 ; $f83c (*)
    
Lf844
    jmp     $d774
    
Lf847
    lda     #$00
    sta     ram_AD
    ldx     ram_9B
    ldy     ram_E3
    lda     #$03
    cpx     #$35
    bcc     Lf85b
    cpy     #$02
    bcc     Lf85b
    
    .byte   $a9,$09                         ; $f859 (*)
    
Lf85b
    cpx     #$45
    bcc     Lf865
    cpy     #$04
    bcc     Lf865
    
    .byte   $a9,$0f                         ; $f863 (*)
    
Lf865
    bne     Lf844
    
    .byte   $00,$04,$04,$04,$04,$04,$05,$05 ; $f867 (*)
    .byte   $06,$07,$08,$0a,$0c,$0e,$11,$14 ; $f86f (*)
    .byte   $17,$19,$1b,$1d,$1e,$1f,$1f,$1f ; $f877 (*)
    .byte   $1d,$1f,$20,$20,$1f,$1d,$1b,$19 ; $f87f (*)
    .byte   $18,$14,$14,$16,$19,$19,$16,$13 ; $f887 (*)
    .byte   $0f,$0c,$09,$09                 ; $f88f (*)
    .byte   $05                             ; $f893 (D)
    .byte   $05,$00,$00,$00,$00,$00,$00     ; $f894 (*)

activisionLogo  
    .byte   %01100001 ; | ##    #|            $f89b (A)
    .byte   %00110001 ; |  ##   #|            $f89c (A)
    .byte   %00011111 ; |   #####|            $f89d (A)
    .byte   %00001101 ; |    ## #|            $f89e (A)
    .byte   %00000111 ; |     ###|            $f89f (A)
    .byte   %00000011 ; |      ##|            $f8a0 (A)
    .byte   %00000001 ; |       #|            $f8a1 (A)
    .byte   %00000000 ; |        |            $f8a2 (A)
    .byte   %01110101 ; | ### # #|            $f8a3 (C)
    .byte   %01000101 ; | #   # #|            $f8a4 (C)
    .byte   %01000101 ; | #   # #|            $f8a5 (C)
    .byte   %01000101 ; | #   # #|            $f8a6 (C)
    .byte   %01110101 ; | ### # #|            $f8a7 (C)
    .byte   %00000100 ; |     #  |            $f8a8 (C)
    .byte   %01111111 ; | #######|            $f8a9 (C)
    .byte   %00000000 ; |        |            $f8aa (C)
    .byte   %01100000 ; | ##     |            $f8ab (T)
    .byte   %01110000 ; | ###    |            $f8ac (T)
    .byte   %01011000 ; | # ##   |            $f8ad (G)
    .byte   %01001100 ; | #  ##  |            $f8ae (G)
    .byte   %01000110 ; | #   ## |            $f8af (G)
    .byte   %01000011 ; | #    ##|            $f8b0 (G)
    .byte   %11000001 ; |##     #|            $f8b1 (G)
    .byte   %00000000 ; |        |            $f8b2 (G)
    .byte   %10111010 ; |# ### # |            $f8b3 (G)
    .byte   %10001010 ; |#   # # |            $f8b4 (G)
    .byte   %10111010 ; |# ### # |            $f8b5 (G)
    .byte   %10100010 ; |# #   # |            $f8b6 (G)
    .byte   %00111010 ; |  ### # |            $f8b7 (G)
    .byte   %00000000 ; |        |            $f8b8 (G)
    .byte   %11111110 ; |####### |            $f8b9 (G)
    .byte   %00000000 ; |        |            $f8ba (G)
    .byte   %11101001 ; |### #  #|            $f8bb (G)
    .byte   %10101011 ; |# # # ##|            $f8bc (G)
    .byte   %10101111 ; |# # ####|            $f8bd (G)
    .byte   %10101101 ; |# # ## #|            $f8be (G)
    .byte   %11101001 ; |### #  #|            $f8bf (G)

copyrightGfx
    .byte   %00000000 ; |        |            $f8c0 (G)
    .byte   %00000000 ; |        |            $f8c1 (G)
    .byte   %00000000 ; |        |            $f8c2 (G)
    .byte   %01110111 ; | ### ###|            $f8c3 (G)
    .byte   %01010001 ; | # #   #|            $f8c4 (G)
    .byte   %01110011 ; | ###  ##|            $f8c5 (G)
    .byte   %01010001 ; | # #   #|            $f8c6 (G)
    .byte   %01110111 ; | ### ###|            $f8c7 (G)
    .byte   %00000000 ; |        |            $f8c8 (G)
    .byte   %00000000 ; |        |            $f8c9 (G)
    .byte   %00000000 ; |        |            $f8ca (G)
    .byte   %00010001 ; |   #   #|            $f8cb (G)
    .byte   %00010001 ; |   #   #|            $f8cc (G)
    .byte   %00010111 ; |   # ###|            $f8cd (G)
    .byte   %00010101 ; |   # # #|            $f8ce (G)
    .byte   %00010111 ; |   # ###|            $f8cf (G)
    .byte   %00000000 ; |        |            $f8d0 (G)
    .byte   %10000000 ; |#       |            $f8d1 (G)
    .byte   %10000000 ; |#       |            $f8d2 (G)
    .byte   %10101010 ; |# # # # |            $f8d3 (G)
    .byte   %10101010 ; |# # # # |            $f8d4 (G)
    .byte   %10111010 ; |# ### # |            $f8d5 (G)
    .byte   %00100111 ; |  #  ###|            $f8d6 (G)
    .byte   %00100010 ; |  #   # |            $f8d7 (G)
    .byte   %00000000 ; |        |            $f8d8 (G)
    .byte   %00000011 ; |      ##|            $f8d9 (G)
    .byte   %00000000 ; |        |            $f8da (G)
    .byte   %01001011 ; | #  # ##|            $f8db (G)
    .byte   %01001010 ; | #  # # |            $f8dc (G)
    .byte   %01101011 ; | ## # ##|            $f8dd (G)
    .byte   %00000000 ; |        |            $f8de (G)
    .byte   %00001000 ; |    #   |            $f8df (G)
    .byte   %00000000 ; |        |            $f8e0 (G)
    .byte   %01000111 ; | #   ###|            $f8e1 (G)
    .byte   %01000001 ; | #     #|            $f8e2 (G)
    .byte   %01110111 ; | ### ###|            $f8e3 (G)
    .byte   %01010101 ; | # # # #|            $f8e4 (G)
    .byte   %01110101 ; | ### # #|            $f8e5 (G)
    .byte   %00000000 ; |        |            $f8e6 (G)
    .byte   %00000000 ; |        |            $f8e7 (G)
    .byte   %00000000 ; |        |            $f8e8 (G)
    .byte   %00000000 ; |        |            $f8e9 (G)
    .byte   %00000000 ; |        |            $f8ea (G)
    .byte   %11110111 ; |#### ###|            $f8eb (G)
    .byte   %10010101 ; |#  # # #|            $f8ec (G)
    .byte   %10000111 ; |#    ###|            $f8ed (G)
    .byte   %10010000 ; |#  #    |            $f8ee (G)
    .byte   %11110000 ; |####    |            $f8ef (G)
    
    .byte   BLUE|$4                         ; $f8f0 (CP)
    .byte   GREEN_YELLOW|$6                 ; $f8f1 (CP)
    .byte   GREEN_YELLOW|$6                 ; $f8f2 (CP)
    .byte   YELLOW|$a                       ; $f8f3 (CP)
    .byte   BROWN|$6                        ; $f8f4 (CP)
    .byte   BROWN|$6                        ; $f8f5 (CP)
    .byte   RED|$4                          ; $f8f6 (CP)
    .byte   BLACK|$0                        ; $f8f7 (CP)
    
    .byte   $02,$04,$11,$13,$1e,$20,$2f,$40 ; $f8f8 (*)
    .byte   $20,$40,$0d,$57,$00,$5f,$00
    .byte   %01010101 ; | # # # #|            $f907 (G)
    .byte   %01010000 ; | # #    |            $f908 (G)
    .byte   %01010101 ; | # # # #|            $f909 (G)
    .byte   %01010000 ; | # #    |            $f90a (G)
    .byte   %01010101 ; | # # # #|            $f90b (G)
    .byte   %01010000 ; | # #    |            $f90c (G)
    .byte   %01010101 ; | # # # #|            $f90d (G)
    .byte   %01010000 ; | # #    |            $f90e (G)
    .byte   %00000000 ; |        |            $f90f (G)
    .byte   %01011010 ; | # ## # |            $f910 (G)
    .byte   %01011000 ; | # ##   |            $f911 (G)
    .byte   %01011010 ; | # ## # |            $f912 (G)
    .byte   %01011000 ; | # ##   |            $f913 (G)
    .byte   %01011010 ; | # ## # |            $f914 (G)
    .byte   %00000000 ; |        |            $f915 (G)
    .byte   %01001000 ; | #  #   |            $f916 (G)
    .byte   %00001000 ; |    #   |            $f917 (G)
    .byte   %01001000 ; | #  #   |            $f918 (G)
    .byte   %00001000 ; |    #   |            $f919 (G)
    .byte   %01001000 ; | #  #   |            $f91a (G)
    .byte   %00001000 ; |    #   |            $f91b (G)
    .byte   %01001000 ; | #  #   |            $f91c (G)
    .byte   %00001000 ; |    #   |            $f91d (G)
    .byte   %01001000 ; | #  #   |            $f91e (G)
    .byte   %00001000 ; |    #   |            $f91f (G)
    .byte   %00000000 ; |        |            $f920 (G)
    .byte   %10111111 ; |# ######|            $f921 (G)
    .byte   %00000000 ; |        |            $f922 (G)
	
	.byte   $cb,$c3,$c5,$87,$88 ; $f920 (*)
    .byte   $89,$8c,$4c,$4e,$4f,$4f,$2e,$2d ; $f928 (*)
    .byte   $2e,$00,$64,$64,$44,$44,$44,$44 ; $f930 (*)
    .byte   $44,$44,$44,$44,$04,$ec,$ec,$cc ; $f938 (*)
    .byte   $8c,$2c,$00,$e2,$cb,$ae,$90,$73 ; $f940 (*)
    .byte   $56,$36,$16,$00,$0a,$2a,$12,$08 ; $f948 (*)
    .byte   $12,$0a,$04,$08,$03,$05,$02,$12 ; $f950 (*)
    
Lf958
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sec
Lf95d
    sbc     #$0f
    bcs     Lf95d
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
    
Lf979
    ldy     ram_FA
    lda     (ram_CC),y
    sta     ram_FC
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     (ram_D6),y
    sta     GRP0
    lda     (ram_D4),y
    sta     GRP1
    lda     (ram_D2),y
    sta     GRP0
    lda     (ram_D0),y
    tax
    lda     (ram_CE),y
    ldy     ram_FC
    stx     GRP1
    sta     GRP0
    sty     GRP1
    sty     GRP0
    dec     ram_FA
    bpl     Lf979
    rts
    
    .byte   $a2,$02,$a5,$88,$d0,$13,$a5,$b3 ; $f9a3 (*)
    .byte   $69,$10,$c9,$21,$b0,$0b,$a5,$b0 ; $f9ab (*)
    .byte   $e5,$b1,$18,$69,$11,$c9,$20,$90 ; $f9b3 (*)
    .byte   $03,$a9,$28,$ca,$86,$84,$60     ; $f9bb (*)
    
    .byte   BLUE_CYAN|$a                    ; $f9c2 (C)
    
    .byte   $9a,$98,$98,$96,$96,$94,$93,$06 ; $f9c3 (*)
    .byte   $06                             ; $f9cb (*)
    .byte   $01,$01,$01,$01,$01,$01,$04,$04 ; $f9cc (D)
    .byte   $00,$00,$00,$00,$00,$00,$18,$a5 ; $f9d4 (D)
    .byte   $c1,$29,$01,$d0,$01,$38,$60,$00 ; $f9dc (D)
    .byte   $00                             ; $f9e4 (D)
 
 
    .byte   %11111111 ; |########|            $f9e5 (G)
    .byte   %00000111 ; |     ###|            $f9e6 (G)
    .byte   %00000101 ; |     # #|            $f9e7 (G)
    .byte   %00100001 ; |  #    #|            $f9e8 (G)
    .byte   %00000001 ; |       #|            $f9e9 (G)
    .byte   %00000001 ; |       #|            $f9ea (G)
    .byte   %00000001 ; |       #|            $f9eb (G)
    .byte   %00000001 ; |       #|            $f9ec (G)
    .byte   %00000001 ; |       #|            $f9ed (G)
    .byte   %00000001 ; |       #|            $f9ee (G)
    .byte   %00100001 ; |  #    #|            $f9ef (G)
    .byte   %00100000 ; |  #     |            $f9f0 (G)
Lf9f1
    .byte   %00100000 ; |  #     |            $f9f1 (G)
    .byte   %00100000 ; |  #     |            $f9f2 (G)
    .byte   %00100000 ; |  #     |            $f9f3 (G)
    .byte   %00100000 ; |  #     |            $f9f4 (G)
    .byte   %00100000 ; |  #     |            $f9f5 (G)
    .byte   %10101000 ; |# # #   |            $f9f6 (G)
    .byte   %11111000 ; |#####   |            $f9f7 (G)
    .byte   %00010000 ; |   #    |            $f9f8 (G)
    .byte   %00010001 ; |   #   #|            $f9f9 (G)
    .byte   %00010000 ; |   #    |            $f9fa (G)
    .byte   %00010000 ; |   #    |            $f9fb (G)
    .byte   %00010000 ; |   #    |            $f9fc (G)
    .byte   %00000000 ; |        |            $f9fd (G)
    .byte   %11111111 ; |########|            $f9fe (G)
    .byte   %00000000 ; |        |            $f9ff (G)

;------------------------------------------------------------
; Display screens for each phase of teh shuttle flight.
; Each screen is 40x25 pixels, split into five 8 byte columns
;------------------------------------------------------------
;======================
;Launch Screen
launchScreen0
;======================
    .byte   %11110110 ; |#### ## |            $fa00 (G)
    .byte   %11110110 ; |#### ## |            $fa01 (G)
    .byte   %11111011 ; |##### ##|            $fa02 (G)
    .byte   %11111011 ; |##### ##|            $fa03 (G)
    .byte   %11101101 ; |### ## #|            $fa04 (G)
    .byte   %11101101 ; |### ## #|            $fa05 (G)
    .byte   %11101110 ; |### ### |            $fa06 (G)
    .byte   %11101110 ; |### ### |            $fa07 (G)
    .byte   %11111111 ; |########|            $fa08 (G)
    .byte   %11111111 ; |########|            $fa09 (G)
    .byte   %11111111 ; |########|            $fa0a (G)
    .byte   %11111111 ; |########|            $fa0b (G)
    .byte   %11111111 ; |########|            $fa0c (G)
    .byte   %11111111 ; |########|            $fa0d (G)
    .byte   %11111111 ; |########|            $fa0e (G)
    .byte   %11111111 ; |########|            $fa0f (G)
    .byte   %11111111 ; |########|            $fa10 (G)
    .byte   %11111111 ; |########|            $fa11 (G)
    .byte   %11111111 ; |########|            $fa12 (G)
    .byte   %10001111 ; |#   ####|            $fa13 (G)
    .byte   %11011111 ; |## #####|            $fa14 (G)
    .byte   %11011111 ; |## #####|            $fa15 (G)
    .byte   %11011111 ; |## #####|            $fa16 (G)
    .byte   %10011111 ; |#  #####|            $fa17 (G)
    .byte   %11111111 ; |########|            $fa18 (G)

launchScreen1
    .byte   %11110111 ; |#### ###|            $fa19 (G)
    .byte   %11110111 ; |#### ###|            $fa1a (G)
    .byte   %11111111 ; |########|            $fa1b (G)
    .byte   %11111111 ; |########|            $fa1c (G)
    .byte   %11111111 ; |########|            $fa1d (G)
    .byte   %11111111 ; |########|            $fa1e (G)
    .byte   %11111111 ; |########|            $fa1f (G)
    .byte   %11111111 ; |########|            $fa20 (G)
    .byte   %01111111 ; | #######|            $fa21 (G)
    .byte   %10111111 ; |# ######|            $fa22 (G)
    .byte   %11011111 ; |## #####|            $fa23 (G)
    .byte   %11100111 ; |###  ###|            $fa24 (G)
    .byte   %00111001 ; |  ###  #|            $fa25 (G)
    .byte   %01111110 ; | ###### |            $fa26 (G)
    .byte   %00111111 ; |  ######|            $fa27 (G)
    .byte   %10111111 ; |# ######|            $fa28 (G)
    .byte   %00111111 ; |  ######|            $fa29 (G)
    .byte   %11111111 ; |########|            $fa2a (G)
    .byte   %11111111 ; |########|            $fa2b (G)
    .byte   %11111111 ; |########|            $fa2c (G)
    .byte   %11111111 ; |########|            $fa2d (G)
    .byte   %11111111 ; |########|            $fa2e (G)
    .byte   %11111111 ; |########|            $fa2f (G)
    .byte   %11111111 ; |########|            $fa30 (G)
    .byte   %11111111 ; |########|            $fa31 (G)

launchScreen2
    .byte   %10111101 ; |# #### #|            $fa32 (G)
    .byte   %10111101 ; |# #### #|            $fa33 (G)
    .byte   %11111111 ; |########|            $fa34 (G)
    .byte   %11111111 ; |########|            $fa35 (G)
    .byte   %11111111 ; |########|            $fa36 (G)
    .byte   %11111111 ; |########|            $fa37 (G)
    .byte   %11111111 ; |########|            $fa38 (G)
    .byte   %11111111 ; |########|            $fa39 (G)
    .byte   %11111111 ; |########|            $fa3a (G)
    .byte   %11111111 ; |########|            $fa3b (G)
    .byte   %11111111 ; |########|            $fa3c (G)
    .byte   %11111111 ; |########|            $fa3d (G)
    .byte   %11111111 ; |########|            $fa3e (G)
    .byte   %01111111 ; | #######|            $fa3f (G)
    .byte   %10001111 ; |#   ####|            $fa40 (G)
    .byte   %11110001 ; |####   #|            $fa41 (G)
    .byte   %11111110 ; |####### |            $fa42 (G)
    .byte   %11111111 ; |########|            $fa43 (G)
    .byte   %11111100 ; |######  |            $fa44 (G)
    .byte   %11111110 ; |####### |            $fa45 (G)
    .byte   %11111100 ; |######  |            $fa46 (G)
    .byte   %11111110 ; |####### |            $fa47 (G)
    .byte   %11111100 ; |######  |            $fa48 (G)
    .byte   %11111111 ; |########|            $fa49 (G)
    .byte   %11111111 ; |########|            $fa4a (G)

launchScreen3
    .byte   %11101111 ; |### ####|            $fa4b (G)
    .byte   %11101111 ; |### ####|            $fa4c (G)
    .byte   %11111111 ; |########|            $fa4d (G)
    .byte   %00000000 ; |        |            $fa4e (G)
    .byte   %01111011 ; | #### ##|            $fa4f (G)
    .byte   %01111011 ; | #### ##|            $fa50 (G)
    .byte   %01010101 ; | # # # #|            $fa51 (G)
    .byte   %01111111 ; | #######|            $fa52 (G)
    .byte   %01111111 ; | #######|            $fa53 (G)
    .byte   %01010101 ; | # # # #|            $fa54 (G)
    .byte   %01111011 ; | #### ##|            $fa55 (G)
    .byte   %01111011 ; | #### ##|            $fa56 (G)
    .byte   %00000000 ; |        |            $fa57 (G)
    .byte   %11111111 ; |########|            $fa58 (G)
    .byte   %11111111 ; |########|            $fa59 (G)
    .byte   %11111111 ; |########|            $fa5a (G)
    .byte   %00111111 ; |  ######|            $fa5b (G)
    .byte   %11001111 ; |##  ####|            $fa5c (G)
    .byte   %11110011 ; |####  ##|            $fa5d (G)
    .byte   %11111100 ; |######  |            $fa5e (G)
    .byte   %11111111 ; |########|            $fa5f (G)
    .byte   %11111111 ; |########|            $fa60 (G)
    .byte   %11111111 ; |########|            $fa61 (G)
    .byte   %11111111 ; |########|            $fa62 (G)
    .byte   %11111111 ; |########|            $fa63 (G)

launchScreen4
    .byte   %01111011 ; | #### ##|            $fa64 (G)
    .byte   %01111011 ; | #### ##|            $fa65 (G)
    .byte   %11111111 ; |########|            $fa66 (G)
    .byte   %00010011 ; |   #  ##|            $fa67 (G)
    .byte   %11011011 ; |## ## ##|            $fa68 (G)
    .byte   %11011011 ; |## ## ##|            $fa69 (G)
    .byte   %01010011 ; | # #  ##|            $fa6a (G)
    .byte   %11011011 ; |## ## ##|            $fa6b (G)
    .byte   %11011011 ; |## ## ##|            $fa6c (G)
    .byte   %01010011 ; | # #  ##|            $fa6d (G)
    .byte   %11011011 ; |## ## ##|            $fa6e (G)
    .byte   %11011011 ; |## ## ##|            $fa6f (G)
    .byte   %00010011 ; |   #  ##|            $fa70 (G)
    .byte   %11111011 ; |##### ##|            $fa71 (G)
    .byte   %11111011 ; |##### ##|            $fa72 (G)
    .byte   %01010011 ; | # #  ##|            $fa73 (G)
    .byte   %10111011 ; |# ### ##|            $fa74 (G)
    .byte   %01011011 ; | # ## ##|            $fa75 (G)
    .byte   %11110011 ; |####  ##|            $fa76 (G)
    .byte   %11111011 ; |##### ##|            $fa77 (G)
    .byte   %01111011 ; | #### ##|            $fa78 (G)
    .byte   %10110011 ; |# ##  ##|            $fa79 (G)
    .byte   %11111111 ; |########|            $fa7a (G)
    .byte   %11111111 ; |########|            $fa7b (G)
    .byte   %11111111 ; |########|            $fa7c (G)

;======================
; Orbit Screen
;======================
orbitScreen0
    .byte   %11111111 ; |########|            $fa7d (G)
    .byte   %11111110 ; |####### |            $fa7e (G)
    .byte   %11111110 ; |####### |            $fa7f (G)
    .byte   %11111110 ; |####### |            $fa80 (G)
    .byte   %11111111 ; |########|            $fa81 (G)
    .byte   %11111111 ; |########|            $fa82 (G)
    .byte   %11000011 ; |##    ##|            $fa83 (G)
    .byte   %11011111 ; |## #####|            $fa84 (G)
    .byte   %11001111 ; |##  ####|            $fa85 (G)
    .byte   %11011111 ; |## #####|            $fa86 (G)
    .byte   %11000111 ; |##   ###|            $fa87 (G)
    .byte   %11011111 ; |## #####|            $fa88 (G)
    .byte   %11001111 ; |##  ####|            $fa89 (G)
    .byte   %11011111 ; |## #####|            $fa8a (G)
    .byte   %11000010 ; |##    # |            $fa8b (G)
    .byte   %11011110 ; |## #### |            $fa8c (G)
    .byte   %11001111 ; |##  ####|            $fa8d (G)
    .byte   %11011111 ; |## #####|            $fa8e (G)
    .byte   %11000111 ; |##   ###|            $fa8f (G)
    .byte   %11011111 ; |## #####|            $fa90 (G)
    .byte   %11001111 ; |##  ####|            $fa91 (G)
    .byte   %11011111 ; |## #####|            $fa92 (G)
    .byte   %11000011 ; |##    ##|            $fa93 (G)
    .byte   %11111111 ; |########|            $fa94 (G)
    .byte   %11111111 ; |########|            $fa95 (G)

orbitalScreen1
    .byte   %11111111 ; |########|            $fa96 (G)
    .byte   %00000000 ; |        |            $fa97 (G)
    .byte   %10101010 ; |# # # # |            $fa98 (G)
    .byte   %10101010 ; |# # # # |            $fa99 (G)
    .byte   %11111111 ; |########|            $fa9a (G)
    .byte   %11111111 ; |########|            $fa9b (G)
    .byte   %11111111 ; |########|            $fa9c (G)
    .byte   %11111111 ; |########|            $fa9d (G)
    .byte   %11111111 ; |########|            $fa9e (G)
    .byte   %11111111 ; |########|            $fa9f (G)
    .byte   %11111111 ; |########|            $faa0 (G)
    .byte   %11111111 ; |########|            $faa1 (G)
    .byte   %11111111 ; |########|            $faa2 (G)
    .byte   %11111111 ; |########|            $faa3 (G)
    .byte   %10101010 ; |# # # # |            $faa4 (G)
    .byte   %11111111 ; |########|            $faa5 (G)
    .byte   %01111111 ; | #######|            $faa6 (G)
    .byte   %10111111 ; |# ######|            $faa7 (G)
    .byte   %11011111 ; |## #####|            $faa8 (G)
    .byte   %11101111 ; |### ####|            $faa9 (G)
    .byte   %11110000 ; |####    |            $faaa (G)
    .byte   %11111111 ; |########|            $faab (G)
    .byte   %11111111 ; |########|            $faac (G)
    .byte   %11111111 ; |########|            $faad (G)
    .byte   %11111111 ; |########|            $faae (G)

orbitalScreen2	
    .byte   %11111111 ; |########|            $faaf (G)
    .byte   %00000000 ; |        |            $fab0 (G)
    .byte   %10101010 ; |# # # # |            $fab1 (G)
    .byte   %10101010 ; |# # # # |            $fab2 (G)
    .byte   %11111111 ; |########|            $fab3 (G)
    .byte   %11111111 ; |########|            $fab4 (G)
    .byte   %11111111 ; |########|            $fab5 (G)
    .byte   %11111111 ; |########|            $fab6 (G)
    .byte   %11111111 ; |########|            $fab7 (G)
    .byte   %11111111 ; |########|            $fab8 (G)
    .byte   %11111111 ; |########|            $fab9 (G)
    .byte   %11111111 ; |########|            $faba (G)
    .byte   %11111111 ; |########|            $fabb (G)
    .byte   %11111110 ; |####### |            $fabc (G)
    .byte   %10101000 ; |# # #   |            $fabd (G)
    .byte   %11111011 ; |##### ##|            $fabe (G)
    .byte   %11110111 ; |#### ###|            $fabf (G)
    .byte   %11101111 ; |### ####|            $fac0 (G)
    .byte   %11011111 ; |## #####|            $fac1 (G)
    .byte   %10111111 ; |# ######|            $fac2 (G)
    .byte   %01111111 ; | #######|            $fac3 (G)
    .byte   %11111111 ; |########|            $fac4 (G)
    .byte   %11111111 ; |########|            $fac5 (G)
    .byte   %11111111 ; |########|            $fac6 (G)
    .byte   %11111111 ; |########|            $fac7 (G)

orbitalScreen3
    .byte   %11111111 ; |########|            $fac8 (G)
    .byte   %00000000 ; |        |            $fac9 (G)
    .byte   %10101010 ; |# # # # |            $faca (G)
    .byte   %10101010 ; |# # # # |            $facb (G)
    .byte   %11111111 ; |########|            $facc (G)
    .byte   %11111111 ; |########|            $facd (G)
    .byte   %11111111 ; |########|            $face (G)
    .byte   %11111111 ; |########|            $facf (G)
    .byte   %11110000 ; |####    |            $fad0 (G)
    .byte   %11101111 ; |### ####|            $fad1 (G)
    .byte   %11011111 ; |## #####|            $fad2 (G)
    .byte   %10111111 ; |# ######|            $fad3 (G)
    .byte   %01111111 ; | #######|            $fad4 (G)
    .byte   %11111111 ; |########|            $fad5 (G)
    .byte   %10101010 ; |# # # # |            $fad6 (G)
    .byte   %11111111 ; |########|            $fad7 (G)
    .byte   %11111111 ; |########|            $fad8 (G)
    .byte   %11111111 ; |########|            $fad9 (G)
    .byte   %11111111 ; |########|            $fada (G)
    .byte   %11111111 ; |########|            $fadb (G)
    .byte   %11111111 ; |########|            $fadc (G)
    .byte   %11111111 ; |########|            $fadd (G)
    .byte   %11111111 ; |########|            $fade (G)
    .byte   %11111111 ; |########|            $fadf (G)
    .byte   %11111111 ; |########|            $fae0 (G)

orbitalScreen4	
    .byte   %11111111 ; |########|            $fae1 (G)
    .byte   %00000011 ; |      ##|            $fae2 (G)
    .byte   %10101011 ; |# # # ##|            $fae3 (G)
    .byte   %10101011 ; |# # # ##|            $fae4 (G)
    .byte   %11111111 ; |########|            $fae5 (G)
    .byte   %11111111 ; |########|            $fae6 (G)
    .byte   %11111111 ; |########|            $fae7 (G)
    .byte   %11111111 ; |########|            $fae8 (G)
    .byte   %01111111 ; | #######|            $fae9 (G)
    .byte   %10111111 ; |# ######|            $faea (G)
    .byte   %11011111 ; |## #####|            $faeb (G)
    .byte   %11101111 ; |### ####|            $faec (G)
    .byte   %11110111 ; |#### ###|            $faed (G)
    .byte   %11111011 ; |##### ##|            $faee (G)
    .byte   %10101011 ; |# # # ##|            $faef (G)
    .byte   %11111111 ; |########|            $faf0 (G)
    .byte   %11111111 ; |########|            $faf1 (G)
    .byte   %11111111 ; |########|            $faf2 (G)
    .byte   %11111111 ; |########|            $faf3 (G)
    .byte   %11110011 ; |####  ##|            $faf4 (G)
    .byte   %11110111 ; |#### ###|            $faf5 (G)
    .byte   %11110011 ; |####  ##|            $faf6 (G)
    .byte   %11111011 ; |##### ##|            $faf7 (G)
    .byte   %11110011 ; |####  ##|            $faf8 (G)
    .byte   %11111111 ; |########|            $faf9 (G)

    .byte   %00000101 ; |     # #|            $fafa (G)
    .byte   %00000010 ; |      # |            $fafb (G)
    .byte   %10000010 ; |#     # |            $fafc (G)
    .byte   %10000010 ; |#     # |            $fafd (G)
    .byte   %10000010 ; |#     # |            $fafe (G)
    .byte   %00000010 ; |      # |            $faff (G)

;======================
;Satellite dock screen
;======================
satScreen0
    .byte   %11111111 ; |########|            $fb00 (G)
    .byte   %11111110 ; |####### |            $fb01 (G)
    .byte   %11111111 ; |########|            $fb02 (G)
    .byte   %11111111 ; |########|            $fb03 (G)
    .byte   %11111111 ; |########|            $fb04 (G)
    .byte   %11111111 ; |########|            $fb05 (G)
    .byte   %11111111 ; |########|            $fb06 (G)
    .byte   %11111111 ; |########|            $fb07 (G)
    .byte   %11111111 ; |########|            $fb08 (G)
    .byte   %11111111 ; |########|            $fb09 (G)
    .byte   %11111111 ; |########|            $fb0a (G)
    .byte   %11111110 ; |####### |            $fb0b (G)
    .byte   %10000001 ; |#      #|            $fb0c (G)
    .byte   %11111110 ; |####### |            $fb0d (G)
    .byte   %11111111 ; |########|            $fb0e (G)
    .byte   %11111111 ; |########|            $fb0f (G)
    .byte   %11111111 ; |########|            $fb10 (G)
    .byte   %11111111 ; |########|            $fb11 (G)
    .byte   %11111111 ; |########|            $fb12 (G)
    .byte   %10011111 ; |#  #####|            $fb13 (G)
    .byte   %11011111 ; |## #####|            $fb14 (G)
    .byte   %10011111 ; |#  #####|            $fb15 (G)
    .byte   %11011111 ; |## #####|            $fb16 (G)
    .byte   %10011111 ; |#  #####|            $fb17 (G)
    .byte   %11111111 ; |########|            $fb18 (G)

SatScreen1
    .byte   %11111111 ; |########|            $fb19 (G)
    .byte   %00111111 ; |  ######|            $fb1a (G)
    .byte   %01111111 ; | #######|            $fb1b (G)
    .byte   %11111111 ; |########|            $fb1c (G)
    .byte   %11111111 ; |########|            $fb1d (G)
    .byte   %11111111 ; |########|            $fb1e (G)
    .byte   %11111111 ; |########|            $fb1f (G)
    .byte   %11111111 ; |########|            $fb20 (G)
    .byte   %11111111 ; |########|            $fb21 (G)
    .byte   %11111111 ; |########|            $fb22 (G)
    .byte   %11111111 ; |########|            $fb23 (G)
    .byte   %10111111 ; |# ######|            $fb24 (G)
    .byte   %11000000 ; |##      |            $fb25 (G)
    .byte   %10111111 ; |# ######|            $fb26 (G)
    .byte   %11111111 ; |########|            $fb27 (G)
    .byte   %11111111 ; |########|            $fb28 (G)
    .byte   %11111111 ; |########|            $fb29 (G)
    .byte   %11111111 ; |########|            $fb2a (G)
    .byte   %11111111 ; |########|            $fb2b (G)
    .byte   %11111111 ; |########|            $fb2c (G)
    .byte   %11111111 ; |########|            $fb2d (G)
    .byte   %11111111 ; |########|            $fb2e (G)
    .byte   %11111111 ; |########|            $fb2f (G)
    .byte   %11111111 ; |########|            $fb30 (G)
    .byte   %11111111 ; |########|            $fb31 (G)

satScreen2
    .byte   %11111111 ; |########|            $fb32 (G)
    .byte   %11111010 ; |##### # |            $fb33 (G)
    .byte   %10011010 ; |#  ## # |            $fb34 (G)
    .byte   %11111111 ; |########|            $fb35 (G)
    .byte   %10011111 ; |#  #####|            $fb36 (G)
    .byte   %11111111 ; |########|            $fb37 (G)
    .byte   %10011111 ; |#  #####|            $fb38 (G)
    .byte   %11111111 ; |########|            $fb39 (G)
    .byte   %10011111 ; |#  #####|            $fb3a (G)
    .byte   %11111111 ; |########|            $fb3b (G)
    .byte   %10011111 ; |#  #####|            $fb3c (G)
    .byte   %11111111 ; |########|            $fb3d (G)
    .byte   %10011111 ; |#  #####|            $fb3e (G)
    .byte   %11111111 ; |########|            $fb3f (G)
    .byte   %10011111 ; |#  #####|            $fb40 (G)
    .byte   %11111111 ; |########|            $fb41 (G)
    .byte   %10011111 ; |#  #####|            $fb42 (G)
    .byte   %11111111 ; |########|            $fb43 (G)
    .byte   %10011111 ; |#  #####|            $fb44 (G)
    .byte   %11111111 ; |########|            $fb45 (G)
    .byte   %10011111 ; |#  #####|            $fb46 (G)
    .byte   %11111111 ; |########|            $fb47 (G)
    .byte   %10010001 ; |#  #   #|            $fb48 (G)
    .byte   %11111111 ; |########|            $fb49 (G)
    .byte   %11111111 ; |########|            $fb4a (G)

satScreen3
    .byte   %11111111 ; |########|            $fb4b (G)
    .byte   %10101010 ; |# # # # |            $fb4c (G)
    .byte   %10101010 ; |# # # # |            $fb4d (G)
    .byte   %11111111 ; |########|            $fb4e (G)
    .byte   %11111111 ; |########|            $fb4f (G)
    .byte   %11111011 ; |##### ##|            $fb50 (G)
    .byte   %11111011 ; |##### ##|            $fb51 (G)
    .byte   %11111011 ; |##### ##|            $fb52 (G)
    .byte   %11111011 ; |##### ##|            $fb53 (G)
    .byte   %11111011 ; |##### ##|            $fb54 (G)
    .byte   %11111011 ; |##### ##|            $fb55 (G)
    .byte   %11110101 ; |#### # #|            $fb56 (G)
    .byte   %11111111 ; |########|            $fb57 (G)
    .byte   %11110101 ; |#### # #|            $fb58 (G)
    .byte   %11111011 ; |##### ##|            $fb59 (G)
    .byte   %11111011 ; |##### ##|            $fb5a (G)
    .byte   %11111011 ; |##### ##|            $fb5b (G)
    .byte   %11111011 ; |##### ##|            $fb5c (G)
    .byte   %11111011 ; |##### ##|            $fb5d (G)
    .byte   %11111011 ; |##### ##|            $fb5e (G)
    .byte   %11111111 ; |########|            $fb5f (G)
    .byte   %11111111 ; |########|            $fb60 (G)
    .byte   %11110001 ; |####   #|            $fb61 (G)
    .byte   %11110111 ; |#### ###|            $fb62 (G)
    .byte   %11111111 ; |########|            $fb63 (G)

satScreen4
    .byte   %11111111 ; |########|            $fb64 (G)
    .byte   %10101011 ; |# # # ##|            $fb65 (G)
    .byte   %10101011 ; |# # # ##|            $fb66 (G)
    .byte   %11111111 ; |########|            $fb67 (G)
    .byte   %11111111 ; |########|            $fb68 (G)
    .byte   %11111111 ; |########|            $fb69 (G)
    .byte   %11111111 ; |########|            $fb6a (G)
    .byte   %11111111 ; |########|            $fb6b (G)
    .byte   %11111111 ; |########|            $fb6c (G)
    .byte   %11111111 ; |########|            $fb6d (G)
    .byte   %11111111 ; |########|            $fb6e (G)
    .byte   %11111111 ; |########|            $fb6f (G)
    .byte   %11111111 ; |########|            $fb70 (G)
    .byte   %11111111 ; |########|            $fb71 (G)
    .byte   %11111111 ; |########|            $fb72 (G)
    .byte   %11111111 ; |########|            $fb73 (G)
    .byte   %11111111 ; |########|            $fb74 (G)
    .byte   %11111111 ; |########|            $fb75 (G)
    .byte   %11111111 ; |########|            $fb76 (G)
    .byte   %11111111 ; |########|            $fb77 (G)
    .byte   %11111111 ; |########|            $fb78 (G)
    .byte   %11111011 ; |##### ##|            $fb79 (G)
    .byte   %11110001 ; |####   #|            $fb7a (G)
    .byte   %11111011 ; |##### ##|            $fb7b (G)
    .byte   %11111111 ; |########|            $fb7c (G)

;======================
; Reentry screen
;======================
reentryScreen0
    .byte   %11011110 ; |## #### |            $fb7d (G)
    .byte   %11011110 ; |## #### |            $fb7e (G)
    .byte   %11111111 ; |########|            $fb7f (G)
    .byte   %11000000 ; |##      |            $fb80 (G)
    .byte   %11011110 ; |## #### |            $fb81 (G)
    .byte   %11011110 ; |## #### |            $fb82 (G)
    .byte   %11010101 ; |## # # #|            $fb83 (G)
    .byte   %11011111 ; |## #####|            $fb84 (G)
    .byte   %11011111 ; |## #####|            $fb85 (G)
    .byte   %11010101 ; |## # # #|            $fb86 (G)
    .byte   %11011110 ; |## #### |            $fb87 (G)
    .byte   %11011110 ; |## #### |            $fb88 (G)
    .byte   %11000000 ; |##      |            $fb89 (G)
    .byte   %11111111 ; |########|            $fb8a (G)
    .byte   %11111111 ; |########|            $fb8b (G)
    .byte   %11111111 ; |########|            $fb8c (G)
    .byte   %11111111 ; |########|            $fb8d (G)
    .byte   %11110001 ; |####   #|            $fb8e (G)
    .byte   %11111101 ; |###### #|            $fb8f (G)
    .byte   %11111111 ; |########|            $fb90 (G)
    .byte   %11111111 ; |########|            $fb91 (G)
    .byte   %11000000 ; |##      |            $fb92 (G)
    .byte   %11111111 ; |########|            $fb93 (G)
    .byte   %11111111 ; |########|            $fb94 (G)
    .byte   %11111111 ; |########|            $fb95 (G)

reentryScreen1
    .byte   %11110111 ; |#### ###|            $fb96 (G)
    .byte   %11110111 ; |#### ###|            $fb97 (G)
    .byte   %11111111 ; |########|            $fb98 (G)
    .byte   %00000111 ; |     ###|            $fb99 (G)
    .byte   %11110111 ; |#### ###|            $fb9a (G)
    .byte   %11110111 ; |#### ###|            $fb9b (G)
    .byte   %01010111 ; | # # ###|            $fb9c (G)
    .byte   %11110111 ; |#### ###|            $fb9d (G)
    .byte   %11110111 ; |#### ###|            $fb9e (G)
    .byte   %01010111 ; | # # ###|            $fb9f (G)
    .byte   %11110111 ; |#### ###|            $fba0 (G)
    .byte   %11110111 ; |#### ###|            $fba1 (G)
    .byte   %00000111 ; |     ###|            $fba2 (G)
    .byte   %11111111 ; |########|            $fba3 (G)
    .byte   %11111111 ; |########|            $fba4 (G)
    .byte   %11111111 ; |########|            $fba5 (G)
    .byte   %11010111 ; |## # ###|            $fba6 (G)
    .byte   %11101111 ; |### ####|            $fba7 (G)
    .byte   %11010111 ; |## # ###|            $fba8 (G)
    .byte   %11111110 ; |####### |            $fba9 (G)
    .byte   %11100000 ; |###     |            $fbaa (G)
    .byte   %00001111 ; |    ####|            $fbab (G)
    .byte   %11111111 ; |########|            $fbac (G)
    .byte   %11111111 ; |########|            $fbad (G)
    .byte   %11111111 ; |########|            $fbae (G)

reentryScreen2
    .byte   %10111101 ; |# #### #|            $fbaf (G)
    .byte   %10111101 ; |# #### #|            $fbb0 (G)
    .byte   %11111111 ; |########|            $fbb1 (G)
    .byte   %11111111 ; |########|            $fbb2 (G)
    .byte   %11111111 ; |########|            $fbb3 (G)
    .byte   %11111111 ; |########|            $fbb4 (G)
    .byte   %11111111 ; |########|            $fbb5 (G)
    .byte   %11111111 ; |########|            $fbb6 (G)
    .byte   %11111111 ; |########|            $fbb7 (G)
    .byte   %11111111 ; |########|            $fbb8 (G)
    .byte   %11111111 ; |########|            $fbb9 (G)
    .byte   %11111110 ; |####### |            $fbba (G)
    .byte   %11111101 ; |###### #|            $fbbb (G)
    .byte   %11111101 ; |###### #|            $fbbc (G)
    .byte   %11111110 ; |####### |            $fbbd (G)
    .byte   %11111111 ; |########|            $fbbe (G)
    .byte   %11111111 ; |########|            $fbbf (G)
    .byte   %11111110 ; |####### |            $fbc0 (G)
    .byte   %11000001 ; |##     #|            $fbc1 (G)
    .byte   %00011111 ; |   #####|            $fbc2 (G)
    .byte   %11111111 ; |########|            $fbc3 (G)
    .byte   %10001111 ; |#   ####|            $fbc4 (G)
    .byte   %10111111 ; |# ######|            $fbc5 (G)
    .byte   %11111111 ; |########|            $fbc6 (G)
    .byte   %11111111 ; |########|            $fbc7 (G)

reentryScreen3	
    .byte   %11101111 ; |### ####|            $fbc8 (G)
    .byte   %11101111 ; |### ####|            $fbc9 (G)
    .byte   %11111011 ; |##### ##|            $fbca (G)
    .byte   %10011100 ; |#  ###  |            $fbcb (G)
    .byte   %10111111 ; |# ######|            $fbcc (G)
    .byte   %10111111 ; |# ######|            $fbcd (G)
    .byte   %10111111 ; |# ######|            $fbce (G)
    .byte   %11111111 ; |########|            $fbcf (G)
    .byte   %11111100 ; |######  |            $fbd0 (G)
    .byte   %11110011 ; |####  ##|            $fbd1 (G)
    .byte   %11001111 ; |##  ####|            $fbd2 (G)
    .byte   %00111111 ; |  ######|            $fbd3 (G)
    .byte   %11111111 ; |########|            $fbd4 (G)
    .byte   %11111111 ; |########|            $fbd5 (G)
    .byte   %00111101 ; |  #### #|            $fbd6 (G)
    .byte   %11011101 ; |## ### #|            $fbd7 (G)
    .byte   %11011101 ; |## ### #|            $fbd8 (G)
    .byte   %00111000 ; |  ###   |            $fbd9 (G)
    .byte   %11111111 ; |########|            $fbda (G)
    .byte   %11111111 ; |########|            $fbdb (G)
    .byte   %11111111 ; |########|            $fbdc (G)
    .byte   %11111111 ; |########|            $fbdd (G)
    .byte   %11111111 ; |########|            $fbde (G)
    .byte   %11111111 ; |########|            $fbdf (G)
    .byte   %11111111 ; |########|            $fbe0 (G)

reentryScreen4	
    .byte   %01111011 ; | #### ##|            $fbe1 (G)
    .byte   %01111011 ; | #### ##|            $fbe2 (G)
    .byte   %11111111 ; |########|            $fbe3 (G)
    .byte   %11110011 ; |####  ##|            $fbe4 (G)
    .byte   %00111011 ; |  ### ##|            $fbe5 (G)
    .byte   %11011011 ; |## ## ##|            $fbe6 (G)
    .byte   %11010011 ; |## #  ##|            $fbe7 (G)
    .byte   %00111011 ; |  ### ##|            $fbe8 (G)
    .byte   %11111011 ; |##### ##|            $fbe9 (G)
    .byte   %11110011 ; |####  ##|            $fbea (G)
    .byte   %11111011 ; |##### ##|            $fbeb (G)
    .byte   %11111011 ; |##### ##|            $fbec (G)
    .byte   %11110011 ; |####  ##|            $fbed (G)
    .byte   %11111011 ; |##### ##|            $fbee (G)
    .byte   %11111011 ; |##### ##|            $fbef (G)
    .byte   %11110011 ; |####  ##|            $fbf0 (G)
    .byte   %11111011 ; |##### ##|            $fbf1 (G)
    .byte   %11111011 ; |##### ##|            $fbf2 (G)
    .byte   %11110011 ; |####  ##|            $fbf3 (G)
    .byte   %11111011 ; |##### ##|            $fbf4 (G)
    .byte   %11111011 ; |##### ##|            $fbf5 (G)
    .byte   %11110011 ; |####  ##|            $fbf6 (G)
    .byte   %11111111 ; |########|            $fbf7 (G)
    .byte   %11111111 ; |########|            $fbf8 (G)
    .byte   %11111111 ; |########|            $fbf9 (G)


    .byte   %10011001 ; |#  ##  #|            $fbfa (G)
    .byte   %10011001 ; |#  ##  #|            $fbfb (G)
    .byte   %00111100 ; |  ####  |            $fbfc (G)
    .byte   %00111100 ; |  ####  |            $fbfd (G)
    .byte   %01111110 ; | ###### |            $fbfe (G)
    .byte   %01111110 ; | ###### |            $fbff (G)

;======================
; Landing Screen
;======================
landingScreen0	
    .byte   %11111111 ; |########|            $fc00 (G)
    .byte   %11111111 ; |########|            $fc01 (G) 
    .byte   %10000000 ; |#       |            $fc02 (G)
    .byte   %11111111 ; |########|            $fc03 (G)
    .byte   %11111111 ; |########|            $fc04 (G)
    .byte   %11100111 ; |###  ###|            $fc05 (G)
    .byte   %11111001 ; |#####  #|            $fc06 (G)
    .byte   %10111110 ; |# ##### |            $fc07 (G)
    .byte   %11001111 ; |##  ####|            $fc08 (G)
    .byte   %11110011 ; |####  ##|            $fc09 (G)
    .byte   %11111101 ; |###### #|            $fc0a (G)
    .byte   %11111110 ; |####### |            $fc0b (G)
    .byte   %11111111 ; |########|            $fc0c (G)
    .byte   %11111111 ; |########|            $fc0d (G)
    .byte   %11111111 ; |########|            $fc0e (G)
    .byte   %11111111 ; |########|            $fc0f (G)
    .byte   %11111111 ; |########|            $fc10 (G)
    .byte   %11111111 ; |########|            $fc11 (G)
    .byte   %11111111 ; |########|            $fc12 (G)
    .byte   %11001110 ; |##  ### |            $fc13 (G)
    .byte   %11101110 ; |### ### |            $fc14 (G)
    .byte   %11001010 ; |##  # # |            $fc15 (G)
    .byte   %11011110 ; |## #### |            $fc16 (G)
    .byte   %11001110 ; |##  ### |            $fc17 (G)
    .byte   %11111111 ; |########|            $fc18 (G)

LandingScreen1
    .byte   %11111111 ; |########|            $fc19 (G)
    .byte   %11111111 ; |########|            $fc1a (G)
    .byte   %00000000 ; |        |            $fc1b (G)
    .byte   %11111111 ; |########|            $fc1c (G)
    .byte   %11111111 ; |########|            $fc1d (G)
    .byte   %11111111 ; |########|            $fc1e (G)
    .byte   %11111111 ; |########|            $fc1f (G)
    .byte   %11111111 ; |########|            $fc20 (G)
    .byte   %01111111 ; | #######|            $fc21 (G)
    .byte   %10111111 ; |# ######|            $fc22 (G)
    .byte   %11011111 ; |## #####|            $fc23 (G)
    .byte   %11101111 ; |### ####|            $fc24 (G)
    .byte   %01101111 ; | ## ####|            $fc25 (G)
    .byte   %10110111 ; |# ## ###|            $fc26 (G)
    .byte   %10110111 ; |# ## ###|            $fc27 (G)
    .byte   %11011011 ; |## ## ##|            $fc28 (G)
    .byte   %11011011 ; |## ## ##|            $fc29 (G)
    .byte   %11101101 ; |### ## #|            $fc2a (G)
    .byte   %11101110 ; |### ### |            $fc2b (G)
    .byte   %11110111 ; |#### ###|            $fc2c (G)
    .byte   %11110111 ; |#### ###|            $fc2d (G)
    .byte   %01111011 ; | #### ##|            $fc2e (G)
    .byte   %11111100 ; |######  |            $fc2f (G)
    .byte   %01111111 ; | #######|            $fc30 (G)
    .byte   %11111111 ; |########|            $fc31 (G)

LandingScreen2
    .byte   %11111111 ; |########|            $fc32 (G)
    .byte   %11100101 ; |###  # #|            $fc33 (G)
    .byte   %00111101 ; |  #### #|            $fc34 (G)
    .byte   %11111111 ; |########|            $fc35 (G)
    .byte   %11100101 ; |###  # #|            $fc36 (G)
    .byte   %11111110 ; |####### |            $fc37 (G)
    .byte   %11111111 ; |########|            $fc38 (G)
    .byte   %11100111 ; |###  ###|            $fc39 (G)
    .byte   %11111111 ; |########|            $fc3a (G)
    .byte   %11111111 ; |########|            $fc3b (G)
    .byte   %11100111 ; |###  ###|            $fc3c (G)
    .byte   %11111111 ; |########|            $fc3d (G)
    .byte   %11111111 ; |########|            $fc3e (G)
    .byte   %11100111 ; |###  ###|            $fc3f (G)
    .byte   %11111111 ; |########|            $fc40 (G)
    .byte   %11111111 ; |########|            $fc41 (G)
    .byte   %11100111 ; |###  ###|            $fc42 (G)
    .byte   %11111111 ; |########|            $fc43 (G)
    .byte   %11111111 ; |########|            $fc44 (G)
    .byte   %00100111 ; |  #  ###|            $fc45 (G)
    .byte   %11111111 ; |########|            $fc46 (G)
    .byte   %11111111 ; |########|            $fc47 (G)
    .byte   %00100100 ; |  #  #  |            $fc48 (G)
    .byte   %11111111 ; |########|            $fc49 (G)
    .byte   %11111111 ; |########|            $fc4a (G)

LandingScreen3
    .byte   %11111111 ; |########|            $fc4b (G)
    .byte   %01010101 ; | # # # #|            $fc4c (G)
    .byte   %01010101 ; | # # # #|            $fc4d (G)
    .byte   %11111111 ; |########|            $fc4e (G)
    .byte   %11111101 ; |###### #|            $fc4f (G)
    .byte   %11111111 ; |########|            $fc50 (G)
    .byte   %01111101 ; | ##### #|            $fc51 (G)
    .byte   %01111111 ; | #######|            $fc52 (G)
    .byte   %10111101 ; |# #### #|            $fc53 (G)
    .byte   %10111111 ; |# ######|            $fc54 (G)
    .byte   %10111101 ; |# #### #|            $fc55 (G)
    .byte   %11011111 ; |## #####|            $fc56 (G)
    .byte   %11011101 ; |## ### #|            $fc57 (G)
    .byte   %11011111 ; |## #####|            $fc58 (G)
    .byte   %11101101 ; |### ## #|            $fc59 (G)
    .byte   %11101111 ; |### ####|            $fc5a (G)
    .byte   %11101101 ; |### ## #|            $fc5b (G)
    .byte   %11110111 ; |#### ###|            $fc5c (G)
    .byte   %11110101 ; |#### # #|            $fc5d (G)
    .byte   %11110111 ; |#### ###|            $fc5e (G)
    .byte   %11111010 ; |##### # |            $fc5f (G)
    .byte   %11111010 ; |##### # |            $fc60 (G)
    .byte   %01111010 ; | #### # |            $fc61 (G)
    .byte   %11111111 ; |########|            $fc62 (G)
    .byte   %11111111 ; |########|            $fc63 (G)
LandingScreen4	
    .byte   %11111111 ; |########|            $fc64 (G)
    .byte   %01010101 ; | # # # #|            $fc65 (G)
    .byte   %01010101 ; | # # # #|            $fc66 (G)
    .byte   %11111111 ; |########|            $fc67 (G)
    .byte   %11111101 ; |###### #|            $fc68 (G)
    .byte   %11111011 ; |##### ##|            $fc69 (G)
    .byte   %11110111 ; |#### ###|            $fc6a (G)
    .byte   %11110111 ; |#### ###|            $fc6b (G)
    .byte   %11101111 ; |### ####|            $fc6c (G)
    .byte   %11101111 ; |### ####|            $fc6d (G)
    .byte   %11101111 ; |### ####|            $fc6e (G)
    .byte   %11011111 ; |## #####|            $fc6f (G)
    .byte   %11011111 ; |## #####|            $fc70 (G)
    .byte   %11011111 ; |## #####|            $fc71 (G)
    .byte   %10111111 ; |# ######|            $fc72 (G)
    .byte   %10111111 ; |# ######|            $fc73 (G)
    .byte   %10111111 ; |# ######|            $fc74 (G)
    .byte   %01111111 ; | #######|            $fc75 (G)
    .byte   %01111111 ; | #######|            $fc76 (G)
    .byte   %01111111 ; | #######|            $fc77 (G)
    .byte   %11111111 ; |########|            $fc78 (G)
    .byte   %11111011 ; |##### ##|            $fc79 (G)
    .byte   %11110001 ; |####   #|            $fc7a (G)
    .byte   %11111011 ; |##### ##|            $fc7b (G)
    .byte   %11111111 ; |########|            $fc7c (G)

;======================
; STS 101 Screen
;======================
stsScreen0
    .byte   %11111111 ; |########|            $fc7d (G)
    .byte   %11111111 ; |########|            $fc7e (G)
    .byte   %11111111 ; |########|            $fc7f (G)
    .byte   %11111111 ; |########|            $fc80 (G)
    .byte   %11111111 ; |########|            $fc81 (G)
    .byte   %11111111 ; |########|            $fc82 (G)
    .byte   %11111111 ; |########|            $fc83 (G)
    .byte   %11111111 ; |########|            $fc84 (G)
    .byte   %11111111 ; |########|            $fc85 (G)
    .byte   %11111111 ; |########|            $fc86 (G)
    .byte   %11111111 ; |########|            $fc87 (G)
    .byte   %11111111 ; |########|            $fc88 (G)
    .byte   %11111111 ; |########|            $fc89 (G)
    .byte   %11111111 ; |########|            $fc8a (G)
    .byte   %11111111 ; |########|            $fc8b (G)
    .byte   %11111111 ; |########|            $fc8c (G)
    .byte   %11111111 ; |########|            $fc8d (G)
    .byte   %11111111 ; |########|            $fc8e (G)
    .byte   %10001101 ; |#   ## #|            $fc8f (G)
    .byte   %11101101 ; |### ## #|            $fc90 (G)
    .byte   %10001101 ; |#   ## #|            $fc91 (G)
    .byte   %10111101 ; |# #### #|            $fc92 (G)
    .byte   %10001000 ; |#   #   |            $fc93 (G)
    .byte   %11111111 ; |########|            $fc94 (G)
    .byte   %11111111 ; |########|            $fc95 (G)

stsScreen1
    .byte   %11111111 ; |########|            $fc96 (G)
    .byte   %00000000 ; |        |            $fc97 (G)
    .byte   %01111111 ; | #######|            $fc98 (G)
    .byte   %10011111 ; |#  #####|            $fc99 (G)
    .byte   %10101111 ; |# # ####|            $fc9a (G)
    .byte   %11010111 ; |## # ###|            $fc9b (G)
    .byte   %11011011 ; |## ## ##|            $fc9c (G)
    .byte   %11101101 ; |### ## #|            $fc9d (G)
    .byte   %11101110 ; |### ### |            $fc9e (G)
    .byte   %11110111 ; |#### ###|            $fc9f (G)
    .byte   %11110111 ; |#### ###|            $fca0 (G)
    .byte   %11111011 ; |##### ##|            $fca1 (G)
    .byte   %11111011 ; |##### ##|            $fca2 (G)
    .byte   %11111101 ; |###### #|            $fca3 (G)
    .byte   %11111101 ; |###### #|            $fca4 (G)
    .byte   %11111110 ; |####### |            $fca5 (G)
    .byte   %11111110 ; |####### |            $fca6 (G)
    .byte   %11111111 ; |########|            $fca7 (G)
    .byte   %10001111 ; |#   ####|            $fca8 (G)
    .byte   %11101111 ; |### ####|            $fca9 (G)
    .byte   %10001111 ; |#   ####|            $fcaa (G)
    .byte   %10111111 ; |# ######|            $fcab (G)
    .byte   %10001111 ; |#   ####|            $fcac (G)
    .byte   %11111111 ; |########|            $fcad (G)
    .byte   %11111111 ; |########|            $fcae (G)

stsScreen2
    .byte   %11111111 ; |########|            $fcaf (G)
    .byte   %00000000 ; |        |            $fcb0 (G)
    .byte   %11111111 ; |########|            $fcb1 (G)
    .byte   %11111111 ; |########|            $fcb2 (G)
    .byte   %01010101 ; | # # # #|            $fcb3 (G)
    .byte   %01101101 ; | ## ## #|            $fcb4 (G)
    .byte   %10101011 ; |# # # ##|            $fcb5 (G)
    .byte   %10111011 ; |# ### ##|            $fcb6 (G)
    .byte   %10111010 ; |# ### # |            $fcb7 (G)
    .byte   %00010001 ; |   #   #|            $fcb8 (G)
    .byte   %10010011 ; |#  #  ##|            $fcb9 (G)
    .byte   %10010011 ; |#  #  ##|            $fcba (G)
    .byte   %10010011 ; |#  #  ##|            $fcbb (G)
    .byte   %10010011 ; |#  #  ##|            $fcbc (G)
    .byte   %10010011 ; |#  #  ##|            $fcbd (G)
    .byte   %11010110 ; |## # ## |            $fcbe (G)
    .byte   %11010110 ; |## # ## |            $fcbf (G)
    .byte   %01010101 ; | # # # #|            $fcc0 (G)
    .byte   %01101101 ; | ## ## #|            $fcc1 (G)
    .byte   %10111011 ; |# ### ##|            $fcc2 (G)
    .byte   %10111011 ; |# ### ##|            $fcc3 (G)
    .byte   %11010111 ; |## # ###|            $fcc4 (G)
    .byte   %11010111 ; |## # ###|            $fcc5 (G)
    .byte   %11101111 ; |### ####|            $fcc6 (G)
    .byte   %11101111 ; |### ####|            $fcc7 (G)

stsScreen3	
    .byte   %11111111 ; |########|            $fcc8 (G)
    .byte   %00000001 ; |       #|            $fcc9 (G)
    .byte   %11111101 ; |###### #|            $fcca (G)
    .byte   %11110011 ; |####  ##|            $fccb (G)
    .byte   %11101011 ; |### # ##|            $fccc (G)
    .byte   %11010111 ; |## # ###|            $fccd (G)
    .byte   %10110111 ; |# ## ###|            $fcce (G)
    .byte   %01101111 ; | ## ####|            $fccf (G)
    .byte   %11101111 ; |### ####|            $fcd0 (G)
    .byte   %11011111 ; |## #####|            $fcd1 (G)
    .byte   %11011111 ; |## #####|            $fcd2 (G)
    .byte   %10111111 ; |# ######|            $fcd3 (G)
    .byte   %10111111 ; |# ######|            $fcd4 (G)
    .byte   %01111111 ; | #######|            $fcd5 (G)
    .byte   %01111111 ; | #######|            $fcd6 (G)
    .byte   %11111111 ; |########|            $fcd7 (G)
    .byte   %11111111 ; |########|            $fcd8 (G)
    .byte   %11111111 ; |########|            $fcd9 (G)
    .byte   %11110001 ; |####   #|            $fcda (G)
    .byte   %11111011 ; |##### ##|            $fcdb (G)
    .byte   %11111011 ; |##### ##|            $fcdc (G)
    .byte   %11111011 ; |##### ##|            $fcdd (G)
    .byte   %11110011 ; |####  ##|            $fcde (G)
    .byte   %11111111 ; |########|            $fcdf (G)
    .byte   %11111111 ; |########|            $fce0 (G)

stsScreen4	
    .byte   %11111111 ; |########|            $fce1 (G)
    .byte   %11111111 ; |########|            $fce2 (G)
    .byte   %11111111 ; |########|            $fce3 (G)
    .byte   %11111111 ; |########|            $fce4 (G)
    .byte   %11111111 ; |########|            $fce5 (G)
    .byte   %11111111 ; |########|            $fce6 (G)
    .byte   %11111111 ; |########|            $fce7 (G)
    .byte   %11111111 ; |########|            $fce8 (G)
    .byte   %11111111 ; |########|            $fce9 (G)
    .byte   %11111111 ; |########|            $fcea (G)
    .byte   %11111111 ; |########|            $fceb (G)
    .byte   %11111111 ; |########|            $fcec (G)
    .byte   %11111111 ; |########|            $fced (G)
    .byte   %11111111 ; |########|            $fcee (G)
    .byte   %11111111 ; |########|            $fcef (G)
    .byte   %11111111 ; |########|            $fcf0 (G)
    .byte   %11111111 ; |########|            $fcf1 (G)
    .byte   %11111111 ; |########|            $fcf2 (G)
    .byte   %10110001 ; |# ##   #|            $fcf3 (G)
    .byte   %01011011 ; | # ## ##|            $fcf4 (G)
    .byte   %01011011 ; | # ## ##|            $fcf5 (G)
    .byte   %01011011 ; | # ## ##|            $fcf6 (G)
    .byte   %10110011 ; |# ##  ##|            $fcf7 (G)
    .byte   %11111111 ; |########|            $fcf8 (G)
    .byte   %11111111 ; |########|            $fcf9 (G)




    .byte   %10010000 ; |#  #    |            $fcfa (G)
    .byte   %11010000 ; |## #    |            $fcfb (G)
    .byte   %11110000 ; |####    |            $fcfc (G)
    .byte   %11111111 ; |########|            $fcfd (G)
    .byte   %11111111 ; |########|            $fcfe (G)
    .byte   %11111111 ; |########|            $fcff (G)
    .byte   %11111111 ; |########|            $fd00 (G)
    .byte   %11110111 ; |#### ###|            $fd01 (G)
    .byte   %11110111 ; |#### ###|            $fd02 (G)
    .byte   %11110011 ; |####  ##|            $fd03 (G)
    .byte   %11110101 ; |#### # #|            $fd04 (G)
    .byte   %11110011 ; |####  ##|            $fd05 (G)
    .byte   %11111111 ; |########|            $fd06 (G)
    .byte   %11110111 ; |#### ###|            $fd07 (G)
    .byte   %11110111 ; |#### ###|            $fd08 (G)
    .byte   %11110101 ; |#### # #|            $fd09 (G)
    .byte   %11110010 ; |####  # |            $fd0a (G)
    .byte   %11110111 ; |#### ###|            $fd0b (G)
    .byte   %11111111 ; |########|            $fd0c (G)
    .byte   %11110111 ; |#### ###|            $fd0d (G)
    .byte   %11110111 ; |#### ###|            $fd0e (G)
    .byte   %11110011 ; |####  ##|            $fd0f (G)
    .byte   %11110101 ; |#### # #|            $fd10 (G)
    .byte   %11110011 ; |####  ##|            $fd11 (G)
    .byte   %11111111 ; |########|            $fd12 (G)
    .byte   %10101010 ; |# # # # |            $fd13 (G)
    .byte   %10101010 ; |# # # # |            $fd14 (G)
    .byte   %10011000 ; |#  ##   |            $fd15 (G)
    .byte   %10101010 ; |# # # # |            $fd16 (G)
    .byte   %10011101 ; |#  ### #|            $fd17 (G)
    .byte   %11111111 ; |########|            $fd18 (G)
    .byte   %11111111 ; |########|            $fd19 (G)
    .byte   %01011011 ; | # ## ##|            $fd1a (G)
    .byte   %01011011 ; | # ## ##|            $fd1b (G)
    .byte   %00011011 ; |   ## ##|            $fd1c (G)
    .byte   %01010101 ; | # # # #|            $fd1d (G)
    .byte   %10110101 ; |# ## # #|            $fd1e (G)
    .byte   %11111111 ; |########|            $fd1f (G)
    .byte   %01010001 ; | # #   #|            $fd20 (G)
    .byte   %01011101 ; | # ### #|            $fd21 (G)
    .byte   %01010001 ; | # #   #|            $fd22 (G)
    .byte   %01010111 ; | # # ###|            $fd23 (G)
    .byte   %01010001 ; | # #   #|            $fd24 (G)
    .byte   %11111111 ; |########|            $fd25 (G)
    .byte   %01000110 ; | #   ## |            $fd26 (G)
    .byte   %01011101 ; | # ### #|            $fd27 (G)
    .byte   %01011101 ; | # ### #|            $fd28 (G)
    .byte   %01011101 ; | # ### #|            $fd29 (G)
    .byte   %01011110 ; | # #### |            $fd2a (G)
    .byte   %11111111 ; |########|            $fd2b (G)
    .byte   %11011010 ; |## ## # |            $fd2c (G)
    .byte   %11011010 ; |## ## # |            $fd2d (G)
    .byte   %11011010 ; |## ## # |            $fd2e (G)
    .byte   %11011010 ; |## ## # |            $fd2f (G)
    .byte   %10001010 ; |#   # # |            $fd30 (G)
    .byte   %11111111 ; |########|            $fd31 (G)
    .byte   %11111111 ; |########|            $fd32 (G)
    .byte   %00110110 ; |  ## ## |            $fd33 (G)
    .byte   %01101010 ; | ## # # |            $fd34 (G)
    .byte   %01101010 ; | ## # # |            $fd35 (G)
    .byte   %01101010 ; | ## # # |            $fd36 (G)
    .byte   %01110111 ; | ### ###|            $fd37 (G)
    .byte   %11111111 ; |########|            $fd38 (G)
    .byte   %00010110 ; |   # ## |            $fd39 (G)
    .byte   %11010101 ; |## # # #|            $fd3a (G)
    .byte   %00010101 ; |   # # #|            $fd3b (G)
    .byte   %01110101 ; | ### # #|            $fd3c (G)
    .byte   %00010110 ; |   # ## |            $fd3d (G)
    .byte   %11111111 ; |########|            $fd3e (G)
    .byte   %11101111 ; |### ####|            $fd3f (G)
    .byte   %01101111 ; | ## ####|            $fd40 (G)
    .byte   %01101111 ; | ## ####|            $fd41 (G)
    .byte   %01101111 ; | ## ####|            $fd42 (G)
    .byte   %11000111 ; |##   ###|            $fd43 (G)
    .byte   %11111111 ; |########|            $fd44 (G)
    .byte   %11010001 ; |## #   #|            $fd45 (G)
    .byte   %11010101 ; |## # # #|            $fd46 (G)
    .byte   %10010101 ; |#  # # #|            $fd47 (G)
    .byte   %01010111 ; | # # ###|            $fd48 (G)
    .byte   %11010001 ; |## #   #|            $fd49 (G)
    .byte   %11111111 ; |########|            $fd4a (G)
    .byte   %11111111 ; |########|            $fd4b (G)
    .byte   %10100111 ; |# #  ###|            $fd4c (G)
    .byte   %10101011 ; |# # # ##|            $fd4d (G)
    .byte   %00101011 ; |  # # ##|            $fd4e (G)
    .byte   %10101011 ; |# # # ##|            $fd4f (G)
    .byte   %01100111 ; | ##  ###|            $fd50 (G)
    .byte   %11111111 ; |########|            $fd51 (G)
    .byte   %11011011 ; |## ## ##|            $fd52 (G)
    .byte   %01011011 ; | # ## ##|            $fd53 (G)
    .byte   %01010011 ; | # #  ##|            $fd54 (G)
    .byte   %01001011 ; | #  # ##|            $fd55 (G)
    .byte   %11011011 ; |## ## ##|            $fd56 (G)
    .byte   %11111111 ; |########|            $fd57 (G)
    .byte   %11111111 ; |########|            $fd58 (G)
    .byte   %11111111 ; |########|            $fd59 (G)
    .byte   %11111111 ; |########|            $fd5a (G)
    .byte   %11111111 ; |########|            $fd5b (G)
    .byte   %11111111 ; |########|            $fd5c (G)
    .byte   %11111111 ; |########|            $fd5d (G)
    .byte   %11111111 ; |########|            $fd5e (G)
    .byte   %01111111 ; | #######|            $fd5f (G)
    .byte   %11111111 ; |########|            $fd60 (G)
    .byte   %01111111 ; | #######|            $fd61 (G)
    .byte   %11111111 ; |########|            $fd62 (G)
    .byte   %11111111 ; |########|            $fd63 (G)
    .byte   %11111111 ; |########|            $fd64 (G)
    .byte   %00010111 ; |   # ###|            $fd65 (G)
    .byte   %11010111 ; |## # ###|            $fd66 (G)
    .byte   %00010011 ; |   #  ##|            $fd67 (G)
    .byte   %01110101 ; | ### # #|            $fd68 (G)
    .byte   %00010011 ; |   #  ##|            $fd69 (G)
    .byte   %11111111 ; |########|            $fd6a (G)
    .byte   %00010111 ; |   # ###|            $fd6b (G)
    .byte   %11010111 ; |## # ###|            $fd6c (G)
    .byte   %00010011 ; |   #  ##|            $fd6d (G)
    .byte   %01110101 ; | ### # #|            $fd6e (G)
    .byte   %00010011 ; |   #  ##|            $fd6f (G)
    .byte   %11111111 ; |########|            $fd70 (G)
    .byte   %11111111 ; |########|            $fd71 (G)
    .byte   %11111111 ; |########|            $fd72 (G)
    .byte   %11111111 ; |########|            $fd73 (G)
    .byte   %11111111 ; |########|            $fd74 (G)
    .byte   %11111111 ; |########|            $fd75 (G)
    .byte   %11111111 ; |########|            $fd76 (G)
    .byte   %11111111 ; |########|            $fd77 (G)
    .byte   %11111111 ; |########|            $fd78 (G)
    .byte   %11111111 ; |########|            $fd79 (G)
    .byte   %11111111 ; |########|            $fd7a (G)
    .byte   %11111111 ; |########|            $fd7b (G)
    .byte   %11111111 ; |########|            $fd7c (G)

unknownScreen0	
    .byte   %11111111 ; |########|            $fd7d (G)
    .byte   %11111111 ; |########|            $fd7e (G)
    .byte   %11111111 ; |########|            $fd7f (G)
    .byte   %11111111 ; |########|            $fd80 (G)
    .byte   %11111111 ; |########|            $fd81 (G)
    .byte   %11111111 ; |########|            $fd82 (G)
    .byte   %11111111 ; |########|            $fd83 (G)
    .byte   %11111111 ; |########|            $fd84 (G)
    .byte   %11111111 ; |########|            $fd85 (G)
    .byte   %11111111 ; |########|            $fd86 (G)
    .byte   %11111111 ; |########|            $fd87 (G)
    .byte   %11111110 ; |####### |            $fd88 (G)
    .byte   %11111101 ; |###### #|            $fd89 (G)
    .byte   %11111011 ; |##### ##|            $fd8a (G)
    .byte   %11110111 ; |#### ###|            $fd8b (G)
    .byte   %11101111 ; |### ####|            $fd8c (G)
    .byte   %11101111 ; |### ####|            $fd8d (G)
    .byte   %11011111 ; |## #####|            $fd8e (G)
    .byte   %11011110 ; |## #### |            $fd8f (G)
    .byte   %10111101 ; |# #### #|            $fd90 (G)
    .byte   %10110000 ; |# ##    |            $fd91 (G)
    .byte   %10001111 ; |#   ####|            $fd92 (G)
    .byte   %11111111 ; |########|            $fd93 (G)
    .byte   %11111111 ; |########|            $fd94 (G)
    .byte   %11111111 ; |########|            $fd95 (G)

unknownScreen1
    .byte   %11111111 ; |########|            $fd96 (G)
    .byte   %11111111 ; |########|            $fd97 (G)
    .byte   %11111111 ; |########|            $fd98 (G)
    .byte   %11111111 ; |########|            $fd99 (G)
    .byte   %11111111 ; |########|            $fd9a (G)
    .byte   %11111100 ; |######  |            $fd9b (G)
    .byte   %11111010 ; |##### # |            $fd9c (G)
    .byte   %11110110 ; |#### ## |            $fd9d (G)
    .byte   %11101110 ; |### ### |            $fd9e (G)
    .byte   %10011110 ; |#  #### |            $fd9f (G)
    .byte   %01111110 ; | ###### |            $fda0 (G)
    .byte   %11111000 ; |#####   |            $fda1 (G)
    .byte   %11110110 ; |#### ## |            $fda2 (G)
    .byte   %11101110 ; |### ### |            $fda3 (G)
    .byte   %11011100 ; |## ###  |            $fda4 (G)
    .byte   %11011010 ; |## ## # |            $fda5 (G)
    .byte   %11101010 ; |### # # |            $fda6 (G)
    .byte   %00101011 ; |  # # ##|            $fda7 (G)
    .byte   %11011101 ; |## ### #|            $fda8 (G)
    .byte   %11111101 ; |###### #|            $fda9 (G)
    .byte   %00001101 ; |    ## #|            $fdaa (G)
    .byte   %11110011 ; |####  ##|            $fdab (G)
    .byte   %11111111 ; |########|            $fdac (G)
    .byte   %11111111 ; |########|            $fdad (G)
    .byte   %11111111 ; |########|            $fdae (G)

unknownScreen2
    .byte   %11111111 ; |########|            $fdaf (G)
    .byte   %11111111 ; |########|            $fdb0 (G)
    .byte   %11101111 ; |### ####|            $fdb1 (G)
    .byte   %11010111 ; |## # ###|            $fdb2 (G)
    .byte   %00111001 ; |  ###  #|            $fdb3 (G)
    .byte   %11101110 ; |### ### |            $fdb4 (G)
    .byte   %10101010 ; |# # # # |            $fdb5 (G)
    .byte   %10101010 ; |# # # # |            $fdb6 (G)
    .byte   %10101010 ; |# # # # |            $fdb7 (G)
    .byte   %10101010 ; |# # # # |            $fdb8 (G)
    .byte   %10101010 ; |# # # # |            $fdb9 (G)
    .byte   %10101010 ; |# # # # |            $fdba (G)
    .byte   %10101010 ; |# # # # |            $fdbb (G)
    .byte   %11111110 ; |####### |            $fdbc (G)
    .byte   %10101010 ; |# # # # |            $fdbd (G)
    .byte   %11111110 ; |####### |            $fdbe (G)
    .byte   %10010010 ; |#  #  # |            $fdbf (G)
    .byte   %01101101 ; | ## ## #|            $fdc0 (G)
    .byte   %11111111 ; |########|            $fdc1 (G)
    .byte   %11111111 ; |########|            $fdc2 (G)
    .byte   %11111111 ; |########|            $fdc3 (G)
    .byte   %11111111 ; |########|            $fdc4 (G)
    .byte   %11111111 ; |########|            $fdc5 (G)
    .byte   %11111111 ; |########|            $fdc6 (G)
    .byte   %11111111 ; |########|            $fdc7 (G)

unknownScreen3
    .byte   %11111111 ; |########|            $fdc8 (G)
    .byte   %11111111 ; |########|            $fdc9 (G)
    .byte   %11111111 ; |########|            $fdca (G)
    .byte   %11111111 ; |########|            $fdcb (G)
    .byte   %11111111 ; |########|            $fdcc (G)
    .byte   %01111111 ; | #######|            $fdcd (G)
    .byte   %10111111 ; |# ######|            $fdce (G)
    .byte   %11011111 ; |## #####|            $fdcf (G)
    .byte   %11101111 ; |### ####|            $fdd0 (G)
    .byte   %11110011 ; |####  ##|            $fdd1 (G)
    .byte   %11111100 ; |######  |            $fdd2 (G)
    .byte   %00111111 ; |  ######|            $fdd3 (G)
    .byte   %11011111 ; |## #####|            $fdd4 (G)
    .byte   %11101111 ; |### ####|            $fdd5 (G)
    .byte   %01110111 ; | ### ###|            $fdd6 (G)
    .byte   %10110111 ; |# ## ###|            $fdd7 (G)
    .byte   %10101111 ; |# # ####|            $fdd8 (G)
    .byte   %10101000 ; |# # #   |            $fdd9 (G)
    .byte   %01110111 ; | ### ###|            $fdda (G)
    .byte   %01111111 ; | #######|            $fddb (G)
    .byte   %01100000 ; | ##     |            $fddc (G)
    .byte   %10011111 ; |#  #####|            $fddd (G)
    .byte   %11111111 ; |########|            $fdde (G)
    .byte   %11111111 ; |########|            $fddf (G)
    .byte   %11111111 ; |########|            $fde0 (G)

unknownScreen4
    .byte   %11111111 ; |########|            $fde1 (G)
    .byte   %11111111 ; |########|            $fde2 (G)
    .byte   %11111111 ; |########|            $fde3 (G)
    .byte   %11111111 ; |########|            $fde4 (G)
    .byte   %11111111 ; |########|            $fde5 (G)
    .byte   %11111111 ; |########|            $fde6 (G)
    .byte   %11111111 ; |########|            $fde7 (G)
    .byte   %11111111 ; |########|            $fde8 (G)
    .byte   %11111111 ; |########|            $fde9 (G)
    .byte   %11111111 ; |########|            $fdea (G)
    .byte   %11111111 ; |########|            $fdeb (G)
    .byte   %01111111 ; | #######|            $fdec (G)
    .byte   %10111111 ; |# ######|            $fded (G)
    .byte   %11011111 ; |## #####|            $fdee (G)
    .byte   %11101111 ; |### ####|            $fdef (G)
    .byte   %11110111 ; |#### ###|            $fdf0 (G)
    .byte   %11110111 ; |#### ###|            $fdf1 (G)
    .byte   %11111011 ; |##### ##|            $fdf2 (G)
    .byte   %01111011 ; | #### ##|            $fdf3 (G)
    .byte   %10111101 ; |# #### #|            $fdf4 (G)
    .byte   %00001101 ; |    ## #|            $fdf5 (G)
    .byte   %11110001 ; |####   #|            $fdf6 (G)
    .byte   %11111111 ; |########|            $fdf7 (G)
    .byte   %11111111 ; |########|            $fdf8 (G)
    .byte   %11111111 ; |########|            $fdf9 (G)



    .byte   %01001010 ; | #  # # |            $fdfa (G)
    .byte   %01001010 ; | #  # # |            $fdfb (G)
    .byte   %01001010 ; | #  # # |            $fdfc (G)
    .byte   %01001010 ; | #  # # |            $fdfd (G)
    .byte   %01001010 ; | #  # # |            $fdfe (G)
    .byte   %01100000 ; | ##     |            $fdff (G)

numberSprites
zeroSprite
    .byte   %00011000 ; |   ##   |            $fe00 (G)
    .byte   %00100100 ; |  #  #  |            $fe01 (G)
    .byte   %00100100 ; |  #  #  |            $fe02 (G)
    .byte   %00000000 ; |        |            $fe03 (G)
    .byte   %00100100 ; |  #  #  |            $fe04 (G)
    .byte   %00100100 ; |  #  #  |            $fe05 (G)
    .byte   %00011000 ; |   ##   |            $fe06 (G)
    .byte   %00000000 ; |        |            $fe07 (G)
oneSprite
    .byte   %00000100 ; |     #  |            $fe08 (G)
    .byte   %00000100 ; |     #  |            $fe09 (G)
    .byte   %00000100 ; |     #  |            $fe0a (G)
    .byte   %00000000 ; |        |            $fe0b (G)
    .byte   %00000100 ; |     #  |            $fe0c (G)
    .byte   %00000100 ; |     #  |            $fe0d (G)
    .byte   %00000100 ; |     #  |            $fe0e (G)
    .byte   %00000000 ; |        |            $fe0f (G)
twoSprite
    .byte   %00011000 ; |   ##   |            $fe10 (G)
    .byte   %00100000 ; |  #     |            $fe11 (G)
    .byte   %00100000 ; |  #     |            $fe12 (G)
    .byte   %00011000 ; |   ##   |            $fe13 (G)
    .byte   %00000100 ; |     #  |            $fe14 (G)
    .byte   %00000100 ; |     #  |            $fe15 (G)
    .byte   %00011000 ; |   ##   |            $fe16 (G)
    .byte   %00000000 ; |        |            $fe17 (G)
threeSprite
    .byte   %00011000 ; |   ##   |            $fe18 (G)
    .byte   %00000100 ; |     #  |            $fe19 (G)
    .byte   %00000100 ; |     #  |            $fe1a (G)
    .byte   %00011000 ; |   ##   |            $fe1b (G)
    .byte   %00000100 ; |     #  |            $fe1c (G)
    .byte   %00000100 ; |     #  |            $fe1d (G)
    .byte   %00011000 ; |   ##   |            $fe1e (G)
fourSprite
    .byte   %00000000 ; |        |            $fe1f (G)
    .byte   %00000100 ; |     #  |            $fe20 (G)
    .byte   %00000100 ; |     #  |            $fe21 (G)
    .byte   %00000100 ; |     #  |            $fe22 (G)
    .byte   %00011000 ; |   ##   |            $fe23 (G)
    .byte   %00100100 ; |  #  #  |            $fe24 (G)
    .byte   %00100100 ; |  #  #  |            $fe25 (G)
    .byte   %00100100 ; |  #  #  |            $fe26 (G)
fiveSprite
    .byte   %00000000 ; |        |            $fe27 (G)
    .byte   %00011000 ; |   ##   |            $fe28 (G)
    .byte   %00000100 ; |     #  |            $fe29 (G)
    .byte   %00000100 ; |     #  |            $fe2a (G)
    .byte   %00011000 ; |   ##   |            $fe2b (G)
    .byte   %00100000 ; |  #     |            $fe2c (G)
    .byte   %00100000 ; |  #     |            $fe2d (G)
    .byte   %00011000 ; |   ##   |            $fe2e (G)
sixSprite	
    .byte   %00000000 ; |        |            $fe2f (G)
    .byte   %00011000 ; |   ##   |            $fe30 (G)
    .byte   %00100100 ; |  #  #  |            $fe31 (G)
    .byte   %00100100 ; |  #  #  |            $fe32 (G)
    .byte   %00011000 ; |   ##   |            $fe33 (G)
    .byte   %00100000 ; |  #     |            $fe34 (G)
    .byte   %00100000 ; |  #     |            $fe35 (G)
    .byte   %00011000 ; |   ##   |            $fe36 (G)
sevenSprite
    .byte   %00000000 ; |        |            $fe37 (G)
    .byte   %00000100 ; |     #  |            $fe38 (G)
    .byte   %00000100 ; |     #  |            $fe39 (G)
    .byte   %00000100 ; |     #  |            $fe3a (G)
    .byte   %00000000 ; |        |            $fe3b (G)
    .byte   %00100100 ; |  #  #  |            $fe3c (G)
    .byte   %00100100 ; |  #  #  |            $fe3d (G)
    .byte   %00011000 ; |   ##   |            $fe3e (G)
eightSprite	
    .byte   %00000000 ; |        |            $fe3f (G)
    .byte   %00011000 ; |   ##   |            $fe40 (G)
    .byte   %00100100 ; |  #  #  |            $fe41 (G)
    .byte   %00100100 ; |  #  #  |            $fe42 (G)
    .byte   %00011000 ; |   ##   |            $fe43 (G)
    .byte   %00100100 ; |  #  #  |            $fe44 (G)
    .byte   %00100100 ; |  #  #  |            $fe45 (G)
    .byte   %00011000 ; |   ##   |            $fe46 (G)
nineSprite	
    .byte   %00000000 ; |        |            $fe47 (G)
    .byte   %00011000 ; |   ##   |            $fe48 (G)
    .byte   %00000100 ; |     #  |            $fe49 (G)
    .byte   %00000100 ; |     #  |            $fe4a (G)
    .byte   %00011000 ; |   ##   |            $fe4b (G)
    .byte   %00100100 ; |  #  #  |            $fe4c (G)
    .byte   %00100100 ; |  #  #  |            $fe4d (G)
    .byte   %00011000 ; |   ##   |            $fe4e (G)
    .byte   %00000000 ; |        |            $fe4f (G)


    .byte   %00000000 ; |        |            $fe50 (G)
    .byte   %00000000 ; |        |            $fe51 (G)
    .byte   %00000000 ; |        |            $fe52 (G)
    .byte   %00011000 ; |   ##   |            $fe53 (G)
    .byte   %00000000 ; |        |            $fe54 (G)
    .byte   %00000000 ; |        |            $fe55 (G)
    .byte   %00000000 ; |        |            $fe56 (G)
    .byte   %00000000 ; |        |            $fe57 (G)
    .byte   %00000000 ; |        |            $fe58 (G)
    .byte   %00000000 ; |        |            $fe59 (G)
    .byte   %00000000 ; |        |            $fe5a (G)

displayTextSprites
fuelSprite0
    .byte   %11111111 ; |########|            $fe5b (G)
    .byte   %10110001 ; |# ##   #|            $fe5c (G)
    .byte   %10110101 ; |# ## # #|            $fe5d (G)
    .byte   %10010101 ; |#  # # #|            $fe5e (G)
    .byte   %10110101 ; |# ## # #|            $fe5f (G)
    .byte   %10010101 ; |#  # # #|            $fe60 (G)
fuelSprite1	
    .byte   %11111111 ; |########|            $fe61 (G)
    .byte   %00100111 ; |  #  ###|            $fe62 (G)
    .byte   %01101101 ; | ## ## #|            $fe63 (G)
    .byte   %00101111 ; |  # ####|            $fe64 (G)
    .byte   %01101101 ; | ## ## #|            $fe65 (G)
    .byte   %00101111 ; |  # ####|            $fe66 (G)

metSprite0	
    .byte   %11111111 ; |########|            $fe67 (G)
    .byte   %10111010 ; |# ### # |            $fe68 (G)
    .byte   %10111010 ; |# ### # |            $fe69 (G)
    .byte   %10101010 ; |# # # # |            $fe6a (G)
    .byte   %10010010 ; |#  #  # |            $fe6b (G)
    .byte   %10111010 ; |# ### # |            $fe6c (G)
metSprite1
    .byte   %11111111 ; |########|            $fe6d (G)
    .byte   %00110111 ; |  ## ###|            $fe6e (G)
    .byte   %11110111 ; |#### ###|            $fe6f (G)
    .byte   %01110111 ; | ### ###|            $fe70 (G)
    .byte   %11110111 ; |#### ###|            $fe71 (G)
    .byte   %00100011 ; |  #   ##|            $fe72 (G)

altSprite0	
    .byte   %11111111 ; |########|            $fe73 (G)
    .byte   %11010110 ; |## # ## |            $fe74 (G)
    .byte   %11010110 ; |## # ## |            $fe75 (G)
    .byte   %11000110 ; |##   ## |            $fe76 (G)
    .byte   %11010110 ; |## # ## |            $fe77 (G)
    .byte   %11101110 ; |### ### |            $fe78 (G)
altSprite1
    .byte   %11111111 ; |########|            $fe79 (G)
    .byte   %00110111 ; |  ## ###|            $fe7a (G)
    .byte   %11110111 ; |#### ###|            $fe7b (G)
    .byte   %11110111 ; |#### ###|            $fe7c (G)
    .byte   %11110111 ; |#### ###|            $fe7d (G)
    .byte   %11100011 ; |###   ##|            $fe7e (G)

sppermeterSprite0	
    .byte   %11111111 ; |########|            $fe7f (G)
    .byte   %10010110 ; |#  # ## |            $fe80 (G)
    .byte   %11010111 ; |## # ###|            $fe81 (G)
    .byte   %10010001 ; |#  #   #|            $fe82 (G)
    .byte   %10110101 ; |# ## # #|            $fe83 (G)
    .byte   %10010001 ; |#  #   #|            $fe84 (G)
sppermeterSprite1	
    .byte   %11111111 ; |########|            $fe85 (G)
    .byte   %11010101 ; |## # # #|            $fe86 (G)
    .byte   %01010101 ; | # # # #|            $fe87 (G)
    .byte   %01101011 ; | ## # ##|            $fe88 (G)
    .byte   %01111111 ; | #######|            $fe89 (G)
    .byte   %10111111 ; |# ######|            $fe8a (G)

statSprite0	
    .byte   %11111111 ; |########|            $fe8b (G)
    .byte   %10011011 ; |#  ## ##|            $fe8c (G)
    .byte   %11011011 ; |## ## ##|            $fe8d (G)
    .byte   %10011011 ; |#  ## ##|            $fe8e (G)
    .byte   %10111011 ; |# ### ##|            $fe8f (G)
    .byte   %10010001 ; |#  #   #|            $fe90 (G)
statSprite1	
    .byte   %11111111 ; |########|            $fe91 (G)
    .byte   %01011011 ; | # ## ##|            $fe92 (G)
    .byte   %01011011 ; | # ## ##|            $fe93 (G)
    .byte   %00011011 ; |   ## ##|            $fe94 (G)
    .byte   %01011011 ; | # ## ##|            $fe95 (G)
    .byte   %00010001 ; |   #   #|            $fe96 (G)

xminusAxSprite0	
    .byte   %11111111 ; |########|            $fe97 (G)
    .byte   %10101111 ; |# # ####|            $fe98 (G)
    .byte   %10101111 ; |# # ####|            $fe99 (G)
    .byte   %11011001 ; |## ##  #|            $fe9a (G)
    .byte   %10101111 ; |# # ####|            $fe9b (G)
    .byte   %10101111 ; |# # ####|            $fe9c (G)
xminusAxSprite1	
    .byte   %11111111 ; |########|            $fe9d (G)
    .byte   %01010101 ; | # # # #|            $fe9e (G)
    .byte   %01011011 ; | # ## ##|            $fe9f (G)
    .byte   %00010101 ; |   # # #|            $fea0 (G)
    .byte   %01011111 ; | # #####|            $fea1 (G)
    .byte   %10111111 ; |# ######|            $fea2 (G)

yminusAxSprite0	
    .byte   %11111111 ; |########|            $fea3 (G)
    .byte   %11011111 ; |## #####|            $fea4 (G)
    .byte   %11011111 ; |## #####|            $fea5 (G)
    .byte   %11011001 ; |## ##  #|            $fea6 (G)
    .byte   %10101111 ; |# # ####|            $fea7 (G)
    .byte   %10101111 ; |# # ####|            $fea8 (G)
yminusAxSprite1	
    .byte   %11111111 ; |########|            $fea9 (G)
    .byte   %01010101 ; | # # # #|            $feaa (G)
    .byte   %01011011 ; | # ## ##|            $feab (G)
    .byte   %00010101 ; |   # # #|            $feac (G)
    .byte   %01011111 ; | # #####|            $fead (G)
    .byte   %10111111 ; |# ######|            $feae (G)

zminusAxSprite0	
    .byte   %11111111 ; |########|            $feaf (G)
    .byte   %10001111 ; |#   ####|            $feb0 (G)
    .byte   %10111111 ; |# ######|            $feb1 (G)
    .byte   %11011001 ; |## ##  #|            $feb2 (G)
    .byte   %11101111 ; |### ####|            $feb3 (G)
    .byte   %10001111 ; |#   ####|            $feb4 (G)
zminusAxSprite1	
    .byte   %11111111 ; |########|            $feb5 (G)
    .byte   %01010101 ; | # # # #|            $feb6 (G)
    .byte   %01011011 ; | # ## ##|            $feb7 (G)
    .byte   %00010101 ; |   # # #|            $feb8 (G)
    .byte   %01011111 ; | # #####|            $feb9 (G)
    .byte   %10111111 ; |# ######|            $feba (G)

pitDegSprite0
    .byte   %11111111 ; |########|            $febb (G)
    .byte   %10111011 ; |# ### ##|            $febc (G)
    .byte   %10111011 ; |# ### ##|            $febd (G)
    .byte   %10001011 ; |#   # ##|            $febe (G)
    .byte   %10101011 ; |# # # ##|            $febf (G)
    .byte   %10001010 ; |#   # # |            $fec0 (G)
pitDegSprite1	
    .byte   %11111111 ; |########|            $fec1 (G)
    .byte   %01111111 ; | #######|            $fec2 (G)
    .byte   %01111111 ; | #######|            $fec3 (G)
    .byte   %01110001 ; | ###   #|            $fec4 (G)
    .byte   %01110101 ; | ### # #|            $fec5 (G)
    .byte   %00110001 ; |  ##   #|            $fec6 (G)

yawSprite0
    .byte   %11111111 ; |########|            $fec7 (G)
    .byte   %11011010 ; |## ## # |            $fec8 (G)
    .byte   %11011010 ; |## ## # |            $fec9 (G)
    .byte   %11011000 ; |## ##   |            $feca (G)
    .byte   %10101010 ; |# # # # |            $fecb (G)
    .byte   %10101000 ; |# # #   |            $fecc (G)
yawSprite1
    .byte   %11111111 ; |########|            $fecd (G)
    .byte   %11010111 ; |## # ###|            $fece (G)
    .byte   %10101011 ; |# # # ##|            $fecf (G)
    .byte   %10101011 ; |# # # ##|            $fed0 (G)
    .byte   %10111011 ; |# ### ##|            $fed1 (G)
    .byte   %10111011 ; |# ### ##|            $fed2 (G)

rngSprite0
    .byte   %11111111 ; |########|            $fed3 (G)
    .byte   %10110101 ; |# ## # #|            $fed4 (G)
    .byte   %10101101 ; |# # ## #|            $fed5 (G)
    .byte   %10001101 ; |#   ## #|            $fed6 (G)
    .byte   %10110100 ; |# ## #  |            $fed7 (G)
    .byte   %10001101 ; |#   ## #|            $fed8 (G)
rngSprite1	
    .byte   %11111111 ; |########|            $fed9 (G)
    .byte   %10100001 ; |# #    #|            $feda (G)
    .byte   %10101101 ; |# # ## #|            $fedb (G)
    .byte   %00101001 ; |  # #  #|            $fedc (G)
    .byte   %10101111 ; |# # ####|            $fedd (G)
    .byte   %10100001 ; |# #    #|            $fede (G)

fltNumberSprite0	
    .byte   %11111111 ; |########|            $fedf (G)
    .byte   %10110010 ; |# ##  # |            $fee0 (G)
    .byte   %10110110 ; |# ## ## |            $fee1 (G)
    .byte   %10010110 ; |#  # ## |            $fee2 (G)
    .byte   %10110110 ; |# ## ## |            $fee3 (G)
    .byte   %10010100 ; |#  # #  |            $fee4 (G)
fltNumberSprite1	
    .byte   %11111111 ; |########|            $fee5 (G)
    .byte   %11101011 ; |### # ##|            $fee6 (G)
    .byte   %11000001 ; |##     #|            $fee7 (G)
    .byte   %11101011 ; |### # ##|            $fee8 (G)
    .byte   %11000001 ; |##     #|            $fee9 (G)
    .byte   %01101011 ; | ## # ##|            $feea (G)
    .byte   %11111111 ; |########|            $feeb (G)
	
	
	.byte	$0e,$0f,$10,$11,$12,$13 ; $feea (*)
    .byte   $13,$13,$13,$13,$13,$12,$11,$10 ; $fef2 (*)
    .byte   $0f,$0e,$0d,$0c,$0b,$0a,$09,$08 ; $fefa (*)
    .byte   $07,$07,$07,$07,$08,$09,$0a,$0b ; $ff02 (*)
    .byte   $0c,$19

messageTextSpritea
launchSprite0	
    .byte   %00000000 ; |        |            $ff0c (G)
    .byte   %00110101 ; |  ## # #|            $ff0d (G)
    .byte   %00100101 ; |  #  # #|            $ff0e (G)
    .byte   %00100111 ; |  #  ###|            $ff0f (G)
    .byte   %00100101 ; |  #  # #|            $ff10 (G)
    .byte   %00100111 ; |  #  ###|            $ff11 (G)
    .byte   %00000000 ; |        |            $ff12 (G)

launchSprite1
    .byte   %00000000 ; |        |            $ff13 (G)
    .byte   %01110100 ; | ### #  |            $ff14 (G)
    .byte   %01010100 ; | # # #  |            $ff15 (G)
    .byte   %01010101 ; | # # # #|            $ff16 (G)
    .byte   %01010110 ; | # # ## |            $ff17 (G)
    .byte   %01010100 ; | # # #  |            $ff18 (G)
    .byte   %00000000 ; |        |            $ff19 (G)

launchSprite2	
    .byte   %00000000 ; |        |            $ff1a (G)
    .byte   %10110101 ; |# ## # #|            $ff1b (G)
    .byte   %10100101 ; |# #  # #|            $ff1c (G)
    .byte   %10100111 ; |# #  ###|            $ff1d (G)
    .byte   %10100101 ; |# #  # #|            $ff1e (G)
    .byte   %10110101 ; |# ## # #|            $ff1f (G)
    .byte   %00000000 ; |        |            $ff20 (G)

scrubSprite0	

    .byte   %00000000 ; |        |            $ff21 (G)
    .byte   %00011011 ; |   ## ##|            $ff22 (G)
    .byte   %00001010 ; |    # # |            $ff23 (G)
    .byte   %00011010 ; |   ## # |            $ff24 (G)
    .byte   %00010010 ; |   #  # |            $ff25 (G)
    .byte   %00011011 ; |   ## ##|            $ff26 (G)
    .byte   %00000000 ; |        |            $ff27 (G)
scrubSprite1	
    .byte   %00000000 ; |        |            $ff28 (G)
    .byte   %01010111 ; | # # ###|            $ff29 (G)
    .byte   %01010101 ; | # # # #|            $ff2a (G)
    .byte   %01100101 ; | ##  # #|            $ff2b (G)
    .byte   %01010101 ; | # # # #|            $ff2c (G)
    .byte   %01100101 ; | ##  # #|            $ff2d (G)
    .byte   %00000000 ; |        |            $ff2e (G)
scrubSprite2
    .byte   %00000000 ; |        |            $ff2f (G)
    .byte   %01100000 ; | ##     |            $ff30 (G)
    .byte   %01010000 ; | # #    |            $ff31 (G)
    .byte   %01100000 ; | ##     |            $ff32 (G)
    .byte   %01010000 ; | # #    |            $ff33 (G)
    .byte   %01100000 ; | ##     |            $ff34 (G)

welcomeHomeSprite0
    .byte   %00000000 ; |        |            $ff35 (G)
    .byte   %10001011 ; |#   # ##|            $ff36 (G)
    .byte   %11011010 ; |## ## # |            $ff37 (G)
    .byte   %10101011 ; |# # # ##|            $ff38 (G)
    .byte   %10001010 ; |#   # # |            $ff39 (G)
    .byte   %10001011 ; |#   # ##|            $ff3a (G)
    .byte   %00000000 ; |        |            $ff3b (G)

welcomeHomeSprite1	
    .byte   %00000000 ; |        |            $ff3c (G)
    .byte   %01101101 ; | ## ## #|            $ff3d (G)
    .byte   %01001001 ; | #  #  #|            $ff3e (G)
    .byte   %01001001 ; | #  #  #|            $ff3f (G)
    .byte   %01001001 ; | #  #  #|            $ff40 (G)
    .byte   %01001101 ; | #  ## #|            $ff41 (G)
    .byte   %00000000 ; |        |            $ff42 (G)

welcomeHomeSprite2	
    .byte   %00000000 ; |        |            $ff43 (G)
    .byte   %11010001 ; |## #   #|            $ff44 (G)
    .byte   %01010001 ; | # #   #|            $ff45 (G)
    .byte   %01010101 ; | # # # #|            $ff46 (G)
    .byte   %01011011 ; | # ## ##|            $ff47 (G)
    .byte   %11010001 ; |## #   #|            $ff48 (G)
    .byte   %00000000 ; |        |            $ff49 (G)

welcomeHomeSprite3	
    .byte   %00000000 ; |        |            $ff4a (G)
    .byte   %01100010 ; | ##   # |            $ff4b (G)
    .byte   %01000010 ; | #    # |            $ff4c (G)
    .byte   %01100011 ; | ##   ##|            $ff4d (G)
    .byte   %01000010 ; | #    # |            $ff4e (G)
    .byte   %01100010 ; | ##   # |            $ff4f (G)
    .byte   %00000000 ; |        |            $ff50 (G)

welcomeHomeSprite4
    .byte   %00000000 ; |        |            $ff51 (G)
    .byte   %10111010 ; |# ### # |            $ff52 (G)
    .byte   %10101010 ; |# # # # |            $ff53 (G)
    .byte   %10101010 ; |# # # # |            $ff54 (G)
    .byte   %10101011 ; |# # # ##|            $ff55 (G)
    .byte   %10111010 ; |# ### # |            $ff56 (G)
    .byte   %00000000 ; |        |            $ff57 (G)

welcomeHomeSprite5
    .byte   %00000000 ; |        |            $ff58 (G)
    .byte   %00101100 ; |  # ##  |            $ff59 (G)
    .byte   %00101000 ; |  # #   |            $ff5a (G)
    .byte   %10101100 ; |# # ##  |            $ff5b (G)
    .byte   %01101000 ; | ## #   |            $ff5c (G)
    .byte   %00101100 ; |  # ##  |            $ff5d (G)

rendezvousSprite0
    .byte   %00000000 ; |        |            $ff5e (G)
    .byte   %00001010 ; |    # # |            $ff5f (G)
    .byte   %00001010 ; |    # # |            $ff60 (G)
    .byte   %00001100 ; |    ##  |            $ff61 (G)
    .byte   %00001010 ; |    # # |            $ff62 (G)
    .byte   %00001100 ; |    ##  |            $ff63 (G)
    .byte   %00000000 ; |        |            $ff64 (G)

rendezvousSprite1
    .byte   %00000000 ; |        |            $ff65 (G)
    .byte   %11010010 ; |## #  # |            $ff66 (G)
    .byte   %10010010 ; |#  #  # |            $ff67 (G)
    .byte   %11010110 ; |## # ## |            $ff68 (G)
    .byte   %10011010 ; |#  ## # |            $ff69 (G)
    .byte   %11010010 ; |## #  # |            $ff6a (G)
    .byte   %00000000 ; |        |            $ff6b (G)

rendezvousSprite2
    .byte   %00000000 ; |        |            $ff6c (G)
    .byte   %11001101 ; |##  ## #|            $ff6d (G)
    .byte   %10101001 ; |# # #  #|            $ff6e (G)
    .byte   %10101100 ; |# # ##  |            $ff6f (G)
    .byte   %10101000 ; |# # #   |            $ff70 (G)
    .byte   %11001101 ; |##  ## #|            $ff71 (G)
    .byte   %00000000 ; |        |            $ff72 (G)

rendezvousSprite3
    .byte   %00000000 ; |        |            $ff73 (G)
    .byte   %11001001 ; |##  #  #|            $ff74 (G)
    .byte   %00010101 ; |   # # #|            $ff75 (G)
    .byte   %10010101 ; |#  # # #|            $ff76 (G)
    .byte   %01010101 ; | # # # #|            $ff77 (G)
    .byte   %11010101 ; |## # # #|            $ff78 (G)
    .byte   %00000000 ; |        |            $ff79 (G)

rendezvousSprite4
    .byte   %00000000 ; |        |            $ff7a (G)
    .byte   %11011101 ; |## ### #|            $ff7b (G)
    .byte   %01010100 ; | # # #  |            $ff7c (G)
    .byte   %01010101 ; | # # # #|            $ff7d (G)
    .byte   %01010101 ; | # # # #|            $ff7e (G)
    .byte   %11010101 ; |## # # #|            $ff7f (G)
    .byte   %00000000 ; |        |            $ff80 (G)

rendezvousSprite5
    .byte   %00000000 ; |        |            $ff81 (G)
    .byte   %11000000 ; |##      |            $ff82 (G)
    .byte   %01000000 ; | #      |            $ff83 (G)
    .byte   %11000000 ; |##      |            $ff84 (G)
    .byte   %00000000 ; |        |            $ff85 (G)
    .byte   %11000000 ; |##      |            $ff86 (G)

missionAbortSprite0	
    .byte   %00000000 ; |        |            $ff87 (G)
    .byte   %10001010 ; |#   # # |            $ff88 (G)
    .byte   %10001010 ; |#   # # |            $ff89 (G)
    .byte   %10101010 ; |# # # # |            $ff8a (G)
    .byte   %11011010 ; |## ## # |            $ff8b (G)
    .byte   %10001010 ; |#   # # |            $ff8c (G)
    .byte   %00000000 ; |        |            $ff8d (G)

missionAbortSprite1	
    .byte   %00000000 ; |        |            $ff8e (G)
    .byte   %11101110 ; |### ### |            $ff8f (G)
    .byte   %00100010 ; |  #   # |            $ff90 (G)
    .byte   %11101110 ; |### ### |            $ff91 (G)
    .byte   %10001000 ; |#   #   |            $ff92 (G)
    .byte   %11101110 ; |### ### |            $ff93 (G)
    .byte   %00000000 ; |        |            $ff94 (G)

missionAbortSprite2
    .byte   %00000000 ; |        |            $ff95 (G)
    .byte   %10111010 ; |# ### # |            $ff96 (G)
    .byte   %10101010 ; |# # # # |            $ff97 (G)
    .byte   %10101010 ; |# # # # |            $ff98 (G)
    .byte   %10101011 ; |# # # ##|            $ff99 (G)
    .byte   %10111010 ; |# ### # |            $ff9a (G)
    .byte   %00000000 ; |        |            $ff9b (G)

missionAbortSprite3	
    .byte   %00000000 ; |        |            $ff9c (G)
    .byte   %01000101 ; | #   # #|            $ff9d (G)
    .byte   %01000101 ; | #   # #|            $ff9e (G)
    .byte   %11000111 ; |##   ###|            $ff9f (G)
    .byte   %01000101 ; | #   # #|            $ffa0 (G)
    .byte   %01000111 ; | #   ###|            $ffa1 (G)
    .byte   %00000000 ; |        |            $ffa2 (G)

missionAbortSprite4	
    .byte   %00000000 ; |        |            $ffa3 (G)
    .byte   %01100111 ; | ##  ###|            $ffa4 (G)
    .byte   %01010101 ; | # # # #|            $ffa5 (G)
    .byte   %01100101 ; | ##  # #|            $ffa6 (G)
    .byte   %01010101 ; | # # # #|            $ffa7 (G)
    .byte   %01100111 ; | ##  ###|            $ffa8 (G)
    .byte   %00000000 ; |        |            $ffa9 (G)

missionAbortSprite5
    .byte   %00000000 ; |        |            $ffaa (G)
    .byte   %01010010 ; | # #  # |            $ffab (G)
    .byte   %01010010 ; | # #  # |            $ffac (G)
    .byte   %01100010 ; | ##   # |            $ffad (G)
    .byte   %01010010 ; | # #  # |            $ffae (G)
    .byte   %01100111 ; | ##  ###|            $ffaf (G)


    .byte   %00000000 ; |        |            $ffb0 (G)
    .byte   %00000011 ; |      ##|            $ffb1 (G)
    .byte   %00000010 ; |      # |            $ffb2 (G)
    .byte   %00000001 ; |       #|            $ffb3 (G)

nasaLogo  
    .byte   %00000000 ; |        |            $ffb4 (G)
    .byte   %00000000 ; |        |            $ffb5 (G)
    .byte   %00000000 ; |        |            $ffb6 (G)
    .byte   %00000000 ; |        |            $ffb7 (G)
    .byte   %00000000 ; |        |            $ffb8 (G)
    .byte   %00000000 ; |        |            $ffb9 (G)
    .byte   %00000000 ; |        |            $ffba (G)
    .byte   %00001001 ; |    #  #|            $ffbb (G)
    .byte   %00001010 ; |    # # |            $ffbc (G)
    .byte   %00001010 ; |    # # |            $ffbd (G)
    .byte   %00001010 ; |    # # |            $ffbe (G)
    .byte   %00001010 ; |    # # |            $ffbf (G)
    .byte   %00001010 ; |    # # |            $ffc0 (G)
    .byte   %00000100 ; |     #  |            $ffc1 (G)
    .byte   %00100011 ; |  #   ##|            $ffc2 (G)
    .byte   %10100010 ; |# #   # |            $ffc3 (G)
    .byte   %10100010 ; |# #   # |            $ffc4 (G)
    .byte   %10010100 ; |#  # #  |            $ffc5 (G)
    .byte   %10010100 ; |#  # #  |            $ffc6 (G)
    .byte   %10010100 ; |#  # #  |            $ffc7 (G)
    .byte   %10001000 ; |#   #   |            $ffc8 (G)
    .byte   %11100100 ; |###  #  |            $ffc9 (G)
    .byte   %00010100 ; |   # #  |            $ffca (G)
    .byte   %00010100 ; |   # #  |            $ffcb (G)
    .byte   %01100010 ; | ##   # |            $ffcc (G)
    .byte   %10000010 ; |#     # |            $ffcd (G)
    .byte   %10000010 ; |#     # |            $ffce (G)
    .byte   %01110001 ; | ###   #|            $ffcf (G)
    .byte   %01000000 ; | #      |            $ffd0 (G)
    .byte   %01000000 ; | #      |            $ffd1 (G)
    .byte   %01000000 ; | #      |            $ffd2 (G)
    .byte   %10000000 ; |#       |            $ffd3 (G)
    .byte   %10000000 ; |#       |            $ffd4 (G)
    .byte   %10000000 ; |#       |            $ffd5 (G)
    .byte   %00000000 ; |        |            $ffd6 (G)
    
    .byte   $00,$00,$00,$00,$00,$00,$00,$ce ; $ffd7 (D)
    .byte   $ae,$8e,$6e,$4e,$2e,$0e,$0e     ; $ffdf (*)
    .byte   $26                             ; $ffe6 (D)
    .byte   $32,$3d,$43,$46,$48,$4a,$4c     ; $ffe7 (*)

thrustPointer    
    .byte   %11111110 ; |####### |            $ffee (G)
    .byte   %10010010 ; |#  #  # |            $ffef (G)
    .byte   %10010010 ; |#  #  # |            $fff0 (G)
    .byte   %00010000 ; |   #    |            $fff1 (G)
    .byte   %10010010 ; |#  #  # |            $fff2 (G)
    .byte   %01111100 ; | #####  |            $fff3 (G)
    .byte   %00111000 ; |  ###   |            $fff4 (G)
    .byte   %10010010 ; |#  #  # |            $fff5 (G)
    
    .byte   $00,$00,$00                     ; $fff6 (*)
BANK1STROBE
    .byte   $00                             ; $fff9 (D)
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
    sta     ram_84
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
    inc     ram_84
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
    ldx     ram_83
    lsr
    bcs     Lf0f5
Lf0ed
    dec     ram_82
    dec     ram_83
    cpx     #$0d
    bne     Lf0fd
Lf0f5
    inc     ram_82
    inc     ram_83
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
    lda     ram_82
    cmp     ram_81
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
    dec     ram_81
    .byte   $2c ;bit                ;4-5 =   4
Lf129
    inc     ram_81
    lda     ram_81
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
    ldx     ram_81
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
    inc     ram_81
Lf29a
    lda     ram_BB
    cmp     #$03
    bcs     Lf2a3
Lf2a0
    jmp     Lf32c
    
Lf2a3
    lda     ram_81
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
    stx     ram_84
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
    lda     ram_84
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
    inc     ram_84
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
    lda     ram_84
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
    stx     ram_84
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
    lda     ram_CC,x
    bne     Lf82f
    sty     ram_CC,x
    dex
    dex
    bne     Lf825
Lf82f
    lda     ram_FA
    lsr
    tax
    lda     Lfe16,x
    cpx     #$0c
    bcs     Lf848
    sta     ram_D6
    adc     #$06
    sta     ram_D4
    lda     #$de
    sta     ram_D7
    sta     ram_D5
    bne     Lf857
Lf848
    ldx     #$0a
    ldy     #$df
    clc
Lf84d
    sta     ram_CC,x
    sty     ram_CD,x
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
    sta     ram_FD
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
    sty     ram_E4,x
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
    sta     ram_FD
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
Lfb2d
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    iny
    cpy     ram_FA
    bcc     Lfb3e
    ldy     #$00
    lda     ram_E4,x
    sta     HMM0,x
    bcs     Lfb42
Lfb3e
    lda     #$00
    sta     HMM0,x
Lfb42
    lda     #$00
    dec     ram_FD
    bpl     Lfb4c
    sta     ENAM0
    bmi     Lfb5c
Lfb4c
    bit     CXM0FB
    bmi     Lfb52
    lda     #$02
Lfb52
    sta     ENAM0
    lda     #$00
    bit     CXM1FB
    bmi     Lfb5c
    lda     #$02
Lfb5c
    sta     ENAM1
    dec     ram_FC
    bpl     Lfb2d
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
    lda     ram_E4,x
    sta     HMM0,x
    bcs     Lfb8c
Lfb88
    lda     #$00
    sta     HMM0,x
Lfb8c
    dec     ram_FD
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
    ldy     ram_FD
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
    lda     ram_FD
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
    sta     ram_CC,x
    lda     #$de
    sta     ram_CD,x
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
    
Lfe16
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
