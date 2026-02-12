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

ram_80          = $80
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
    
Start
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
Lf04b
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
    bpl     Lf04b
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
    bit     ram_80
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
Lf138
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
    bne     Lf138
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
    lda     ram_80
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
Lf298
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
    bpl     Lf298
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
Lf35e
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
    bpl     Lf35e
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
    bne     Lf795
    
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
    
Lf795
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
    
    .byte   %01100001 ; | ##    #|            $f89b (G)
    .byte   %00110001 ; |  ##   #|            $f89c (G)
    .byte   %00011111 ; |   #####|            $f89d (G)
    .byte   %00001101 ; |    ## #|            $f89e (G)
    .byte   %00000111 ; |     ###|            $f89f (G)
    .byte   %00000011 ; |      ##|            $f8a0 (G)
    .byte   %00000001 ; |       #|            $f8a1 (G)
    .byte   %00000000 ; |        |            $f8a2 (G)
    .byte   %01110101 ; | ### # #|            $f8a3 (G)
    .byte   %01000101 ; | #   # #|            $f8a4 (G)
    .byte   %01000101 ; | #   # #|            $f8a5 (G)
    .byte   %01000101 ; | #   # #|            $f8a6 (G)
    .byte   %01110101 ; | ### # #|            $f8a7 (G)
    .byte   %00000100 ; |     #  |            $f8a8 (G)
    .byte   %01111111 ; | #######|            $f8a9 (G)
    .byte   %00000000 ; |        |            $f8aa (G)
    .byte   %01100000 ; | ##     |            $f8ab (G)
    .byte   %01110000 ; | ###    |            $f8ac (G)
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
    .byte   %00000000 ; |        |            $f8c0 (G)
    .byte   %00000000 ; |        |            $f8c1 (G)
    .byte   %00000000 ; |        |            $f8c2 (G)
    
    .byte   $77,$51,$73,$51,$77             ; $f8c3 (D)
    
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
    .byte   $20,$40,$0d,$57,$00,$5f,$00,$55 ; $f900 (*)
    .byte   $50,$55,$50,$55,$50,$55,$50,$00 ; $f908 (*)
    .byte   $5a,$58,$5a,$58,$5a,$00,$48,$08 ; $f910 (*)
    .byte   $48,$08,$48,$08,$48,$08,$48,$08 ; $f918 (*)
    .byte   $00,$bf,$00,$cb,$c3,$c5,$87,$88 ; $f920 (*)
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
    .byte   $ff,$07,$05                     ; $f9e5 (*)
    .byte   $21,$01,$01,$01,$01,$01,$01,$21 ; $f9e8 (D)
    
    .byte   %00100000 ; |  #     |            $f9f0 (G)
    .byte   %00100000 ; |  #     |            $f9f1 (G)
    .byte   %00100000 ; |  #     |            $f9f2 (G)
    .byte   %00100000 ; |  #     |            $f9f3 (G)
    .byte   %00100000 ; |  #     |            $f9f4 (G)
    .byte   %00100000 ; |  #     |            $f9f5 (G)
    .byte   %10101000 ; |# # #   |            $f9f6 (G)
    .byte   %11111000 ; |#####   |            $f9f7 (G)
    
    .byte   $10,$11,$10,$10,$10             ; $f9f8 (*)
    .byte   $00                             ; $f9fd (D)
    .byte   $ff,$00,$f6,$f6,$fb,$fb,$ed,$ed ; $f9fe (*)
    .byte   $ee,$ee,$ff,$ff,$ff,$ff,$ff,$ff ; $fa06 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$8f,$df,$df ; $fa0e (*)
    .byte   $df,$9f,$ff,$f7,$f7,$ff,$ff,$ff ; $fa16 (*)
    .byte   $ff,$ff,$ff,$7f,$bf,$df,$e7,$39 ; $fa1e (*)
    .byte   $7e,$3f,$bf,$3f,$ff,$ff,$ff,$ff ; $fa26 (*)
    .byte   $ff,$ff,$ff,$ff,$bd,$bd,$ff,$ff ; $fa2e (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fa36 (*)
    .byte   $ff,$7f,$8f,$f1,$fe,$ff,$fc,$fe ; $fa3e (*)
    .byte   $fc,$fe,$fc,$ff,$ff,$ef,$ef,$ff ; $fa46 (*)
    .byte   $00,$7b,$7b,$55,$7f,$7f,$55,$7b ; $fa4e (*)
    .byte   $7b,$00,$ff,$ff,$ff,$3f,$cf,$f3 ; $fa56 (*)
    .byte   $fc,$ff,$ff,$ff,$ff,$ff,$7b,$7b ; $fa5e (*)
    .byte   $ff,$13,$db,$db,$53,$db,$db,$53 ; $fa66 (*)
    .byte   $db,$db,$13,$fb,$fb,$53,$bb,$5b ; $fa6e (*)
    .byte   $f3,$fb,$7b,$b3,$ff,$ff,$ff,$ff ; $fa76 (*)
    .byte   $fe,$fe,$fe,$ff,$ff,$c3,$df,$cf ; $fa7e (*)
    .byte   $df,$c7,$df,$cf,$df,$c2,$de,$cf ; $fa86 (*)
    .byte   $df,$c7,$df,$cf,$df,$c3,$ff,$ff ; $fa8e (*)
    .byte   $ff,$00,$aa,$aa,$ff,$ff,$ff,$ff ; $fa96 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$aa,$ff ; $fa9e (*)
    .byte   $7f,$bf,$df,$ef,$f0,$ff,$ff,$ff ; $faa6 (*)
    .byte   $ff,$ff,$00,$aa,$aa,$ff,$ff,$ff ; $faae (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$fe,$a8 ; $fab6 (*)
    .byte   $fb,$f7,$ef,$df,$bf,$7f,$ff,$ff ; $fabe (*)
    .byte   $ff,$ff,$ff,$00,$aa,$aa,$ff,$ff ; $fac6 (*)
    .byte   $ff,$ff,$f0,$ef,$df,$bf,$7f,$ff ; $face (*)
    .byte   $aa,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fad6 (*)
    .byte   $ff,$ff,$ff,$ff,$03,$ab,$ab     ; $fade (*)
    .byte   $ff                             ; $fae5 (D)
    .byte   $ff,$ff,$ff,$7f,$bf,$df,$ef,$f7 ; $fae6 (*)
    .byte   $fb,$ab,$ff,$ff,$ff,$ff,$f3,$f7 ; $faee (*)
    .byte   $f3,$fb,$f3,$ff,$05             ; $faf6 (*)
    
    .byte   %00000010 ; |      # |            $fafb (G)
    .byte   %10000010 ; |#     # |            $fafc (G)
    .byte   %10000010 ; |#     # |            $fafd (G)
    .byte   %10000010 ; |#     # |            $fafe (G)
    .byte   %00000010 ; |      # |            $faff (G)
    
    .byte   $ff,$fe,$ff,$ff,$ff,$ff,$ff,$ff ; $fb00 (*)
    .byte   $ff,$ff,$ff,$fe,$81,$fe,$ff,$ff ; $fb08 (*)
    .byte   $ff,$ff,$ff,$9f,$df,$9f,$df,$9f ; $fb10 (*)
    .byte   $ff,$ff,$3f,$7f,$ff,$ff,$ff,$ff ; $fb18 (*)
    .byte   $ff,$ff,$ff,$ff,$bf,$c0,$bf,$ff ; $fb20 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fb28 (*)
    .byte   $ff,$ff,$ff,$fa,$9a,$ff,$9f,$ff ; $fb30 (*)
    .byte   $9f,$ff,$9f,$ff,$9f,$ff,$9f,$ff ; $fb38 (*)
    .byte   $9f,$ff,$9f,$ff,$9f,$ff,$9f,$ff ; $fb40 (*)
    .byte   $91,$ff,$ff,$ff,$aa,$aa,$ff,$ff ; $fb48 (*)
    .byte   $fb,$fb,$fb,$fb,$fb,$fb,$f5,$ff ; $fb50 (*)
    .byte   $f5,$fb,$fb,$fb,$fb,$fb,$fb,$ff ; $fb58 (*)
    .byte   $ff,$f1,$f7,$ff,$ff,$ab,$ab,$ff ; $fb60 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fb68 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fb70 (*)
    .byte   $ff,$fb,$f1,$fb,$ff,$de,$de,$ff ; $fb78 (*)
    .byte   $c0,$de,$de,$d5,$df,$df,$d5,$de ; $fb80 (*)
    .byte   $de,$c0,$ff,$ff,$ff,$ff,$f1,$fd ; $fb88 (*)
    .byte   $ff,$ff,$c0,$ff,$ff,$ff,$f7,$f7 ; $fb90 (*)
    .byte   $ff,$07,$f7,$f7,$57,$f7,$f7,$57 ; $fb98 (*)
    .byte   $f7,$f7,$07,$ff,$ff,$ff,$d7,$ef ; $fba0 (*)
    .byte   $d7,$fe,$e0,$0f,$ff,$ff,$ff,$bd ; $fba8 (*)
    .byte   $bd,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fbb0 (*)
    .byte   $ff,$ff,$fe,$fd,$fd,$fe,$ff,$ff ; $fbb8 (*)
    .byte   $fe,$c1,$1f,$ff,$8f,$bf,$ff,$ff ; $fbc0 (*)
    .byte   $ef,$ef,$fb,$9c,$bf,$bf,$bf,$ff ; $fbc8 (*)
    .byte   $fc,$f3,$cf,$3f,$ff,$ff,$3d,$dd ; $fbd0 (*)
    .byte   $dd,$38,$ff,$ff,$ff,$ff,$ff,$ff ; $fbd8 (*)
    .byte   $ff,$7b,$7b,$ff,$f3,$3b,$db     ; $fbe0 (*)
    
    .byte   %11010011 ; |** *  **|            $fbe7 (P)
    
    .byte   $3b,$fb,$f3,$fb,$fb,$f3,$fb,$fb ; $fbe8 (*)
    .byte   $f3,$fb,$fb,$f3,$fb,$fb,$f3,$ff ; $fbf0 (*)
    .byte   $ff,$ff                         ; $fbf8 (*)
    
    .byte   %10011001 ; |#  ##  #|            $fbfa (G)
    .byte   %10011001 ; |#  ##  #|            $fbfb (G)
    .byte   %00111100 ; |  ####  |            $fbfc (G)
    .byte   %00111100 ; |  ####  |            $fbfd (G)
    .byte   %01111110 ; | ###### |            $fbfe (G)
    .byte   %01111110 ; | ###### |            $fbff (G)
    .byte   %11111111 ; |########|            $fc00 (G)
    .byte   %11111111 ; |########|            $fc01 (G)
    
    .byte   $80,$ff,$ff,$e7,$f9,$be,$cf,$f3 ; $fc02 (*)
    .byte   $fd,$fe,$ff,$ff,$ff,$ff,$ff,$ff ; $fc0a (*)
    .byte   $ff,$ce,$ee,$ca,$de,$ce,$ff,$ff ; $fc12 (*)
    .byte   $ff,$00,$ff,$ff,$ff,$ff,$ff,$7f ; $fc1a (*)
    .byte   $bf,$df,$ef,$6f,$b7,$b7,$db,$db ; $fc22 (*)
    .byte   $ed,$ee,$f7,$f7,$7b,$fc,$7f,$ff ; $fc2a (*)
    .byte   $ff,$e5,$3d,$ff,$e5,$fe,$ff,$e7 ; $fc32 (*)
    .byte   $ff,$ff,$e7,$ff,$ff,$e7,$ff,$ff ; $fc3a (*)
    .byte   $e7,$ff,$ff,$27,$ff,$ff,$24,$ff ; $fc42 (*)
    .byte   $ff,$ff,$55,$55,$ff,$fd,$ff,$7d ; $fc4a (*)
    .byte   $7f,$bd,$bf,$bd,$df,$dd,$df,$ed ; $fc52 (*)
    .byte   $ef,$ed,$f7,$f5,$f7,$fa,$fa,$7a ; $fc5a (*)
    .byte   $ff,$ff,$ff,$55,$55,$ff,$fd,$fb ; $fc62 (*)
    .byte   $f7,$f7,$ef,$ef,$ef,$df,$df,$df ; $fc6a (*)
    .byte   $bf,$bf,$bf,$7f,$7f,$7f,$ff,$fb ; $fc72 (*)
    .byte   $f1,$fb,$ff                     ; $fc7a (*)
    
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
    
    .byte   $90,$d0,$f0,$ff,$ff,$ff,$ff,$f7 ; $fcfa (*)
    .byte   $f7,$f3,$f5,$f3,$ff,$f7,$f7,$f5 ; $fd02 (*)
    .byte   $f2,$f7,$ff,$f7,$f7,$f3,$f5,$f3 ; $fd0a (*)
    .byte   $ff,$aa,$aa,$98,$aa,$9d,$ff,$ff ; $fd12 (*)
    .byte   $5b,$5b,$1b,$55,$b5,$ff,$51,$5d ; $fd1a (*)
    .byte   $51,$57,$51,$ff,$46,$5d,$5d,$5d ; $fd22 (*)
    .byte   $5e,$ff,$da,$da,$da,$da,$8a,$ff ; $fd2a (*)
    .byte   $ff,$36,$6a,$6a,$6a,$77,$ff,$16 ; $fd32 (*)
    .byte   $d5,$15,$75,$16,$ff,$ef,$6f,$6f ; $fd3a (*)
    .byte   $6f,$c7,$ff,$d1,$d5,$95,$57,$d1 ; $fd42 (*)
    .byte   $ff,$ff,$a7,$ab,$2b,$ab,$67,$ff ; $fd4a (*)
    .byte   $db,$5b,$53,$4b,$db,$ff,$ff,$ff ; $fd52 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$7f,$ff,$7f ; $fd5a (*)
    .byte   $ff,$ff,$ff,$17,$d7,$13,$75,$13 ; $fd62 (*)
    .byte   $ff,$17,$d7,$13,$75,$13,$ff,$ff ; $fd6a (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fd72 (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fd7a (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$fe,$fd ; $fd82 (*)
    .byte   $fb,$f7,$ef,$ef,$df,$de,$bd,$b0 ; $fd8a (*)
    .byte   $8f,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fd92 (*)
    .byte   $ff,$fc,$fa,$f6,$ee,$9e,$7e,$f8 ; $fd9a (*)
    .byte   $f6,$ee,$dc,$da,$ea,$2b,$dd,$fd ; $fda2 (*)
    .byte   $0d,$f3,$ff,$ff,$ff,$ff,$ff,$ef ; $fdaa (*)
    .byte   $d7,$39,$ee,$aa,$aa,$aa,$aa,$aa ; $fdb2 (*)
    .byte   $aa,$aa,$fe,$aa,$fe,$92,$6d,$ff ; $fdba (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fdc2 (*)
    .byte   $ff,$ff,$ff,$7f,$bf,$df,$ef,$f3 ; $fdca (*)
    .byte   $fc,$3f,$df,$ef,$77,$b7,$af,$a8 ; $fdd2 (*)
    .byte   $77,$7f,$60,$9f,$ff,$ff,$ff,$ff ; $fdda (*)
    .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $fde2 (*)
    .byte   $ff,$ff,$7f,$bf,$df,$ef,$f7,$f7 ; $fdea (*)
    .byte   $fb,$7b,$bd,$0d,$f1,$ff,$ff,$ff ; $fdf2 (*)
    .byte   $4a,$4a,$4a,$4a,$4a,$60,$18,$24 ; $fdfa (*)
    .byte   $24,$00,$24,$24,$18,$00,$04,$04 ; $fe02 (*)
    .byte   $04,$00,$04,$04,$04,$00,$18,$20 ; $fe0a (*)
    .byte   $20,$18,$04,$04,$18,$00,$18,$04 ; $fe12 (*)
    .byte   $04,$18,$04,$04,$18,$00,$04,$04 ; $fe1a (*)
    .byte   $04,$18,$24,$24,$24,$00,$18,$04 ; $fe22 (*)
    .byte   $04,$18,$20,$20,$18,$00,$18,$24 ; $fe2a (*)
    .byte   $24,$18,$20,$20,$18,$00,$04,$04 ; $fe32 (*)
    .byte   $04,$00,$24,$24,$18,$00,$18,$24 ; $fe3a (*)
    .byte   $24,$18,$24,$24,$18,$00,$18,$04 ; $fe42 (*)
    .byte   $04,$18,$24,$24,$18,$00,$00,$00 ; $fe4a (*)
    .byte   $00,$18,$00,$00,$00,$00,$00,$00 ; $fe52 (*)
    .byte   $00,$ff,$b1,$b5,$95,$b5,$95,$ff ; $fe5a (*)
    .byte   $27,$6d,$2f,$6d,$2f,$ff,$ba,$ba ; $fe62 (*)
    .byte   $aa,$92,$ba,$ff,$37,$f7,$77,$f7 ; $fe6a (*)
    .byte   $23,$ff,$d6,$d6,$c6,$d6,$ee,$ff ; $fe72 (*)
    .byte   $37,$f7,$f7,$f7,$e3,$ff,$96,$d7 ; $fe7a (*)
    .byte   $91,$b5,$91,$ff,$d5,$55,$6b,$7f ; $fe82 (*)
    .byte   $bf,$ff,$9b,$db,$9b,$bb,$91,$ff ; $fe8a (*)
    .byte   $5b,$5b,$1b,$5b,$11,$ff,$af,$af ; $fe92 (*)
    .byte   $d9,$af,$af,$ff,$55,$5b,$15,$5f ; $fe9a (*)
    .byte   $bf,$ff,$df,$df,$d9,$af,$af,$ff ; $fea2 (*)
    .byte   $55,$5b,$15,$5f,$bf,$ff,$8f,$bf ; $feaa (*)
    .byte   $d9,$ef,$8f,$ff,$55,$5b,$15,$5f ; $feb2 (*)
    .byte   $bf,$ff,$bb,$bb,$8b,$ab,$8a,$ff ; $feba (*)
    .byte   $7f,$7f,$71,$75,$31,$ff,$da,$da ; $fec2 (*)
    .byte   $d8,$aa,$a8,$ff,$d7,$ab,$ab,$bb ; $feca (*)
    .byte   $bb,$ff,$b5,$ad,$8d,$b4,$8d,$ff ; $fed2 (*)
    .byte   $a1,$ad,$29,$af,$a1,$ff,$b2,$b6 ; $feda (*)
    .byte   $96,$b6,$94,$ff,$eb,$c1,$eb,$c1 ; $fee2 (*)
    .byte   $6b,$ff,$0e,$0f,$10,$11,$12,$13 ; $feea (*)
    .byte   $13,$13,$13,$13,$13,$12,$11,$10 ; $fef2 (*)
    .byte   $0f,$0e,$0d,$0c,$0b,$0a,$09,$08 ; $fefa (*)
    .byte   $07,$07,$07,$07,$08,$09,$0a,$0b ; $ff02 (*)
    .byte   $0c,$19,$00,$35,$25,$27,$25,$27 ; $ff0a (*)
    .byte   $00,$00,$74,$54,$55,$56,$54,$00 ; $ff12 (*)
    .byte   $00,$b5,$a5,$a7,$a5,$b5,$00,$00 ; $ff1a (*)
    .byte   $1b,$0a,$1a,$12,$1b,$00,$00,$57 ; $ff22 (*)
    .byte   $55,$65,$55,$65,$00,$00,$60,$50 ; $ff2a (*)
    .byte   $60,$50,$60,$00,$8b,$da,$ab,$8a ; $ff32 (*)
    .byte   $8b,$00,$00,$6d,$49,$49,$49,$4d ; $ff3a (*)
    .byte   $00,$00,$d1,$51,$55,$5b,$d1,$00 ; $ff42 (*)
    .byte   $00,$62,$42,$63,$42,$62,$00,$00 ; $ff4a (*)
    .byte   $ba,$aa,$aa,$ab,$ba,$00,$00,$2c ; $ff52 (*)
    .byte   $28,$ac,$68,$2c,$00,$0a,$0a,$0c ; $ff5a (*)
    .byte   $0a,$0c,$00,$00,$d2,$92,$d6,$9a ; $ff62 (*)
    .byte   $d2,$00,$00,$cd,$a9,$ac,$a8,$cd ; $ff6a (*)
    .byte   $00,$00,$c9,$15,$95,$55,$d5,$00 ; $ff72 (*)
    .byte   $00,$dd,$54,$55,$55,$d5,$00,$00 ; $ff7a (*)
    .byte   $c0,$40,$c0,$00,$c0,$00,$8a,$8a ; $ff82 (*)
    .byte   $aa,$da,$8a,$00,$00,$ee,$22,$ee ; $ff8a (*)
    .byte   $88,$ee,$00,$00,$ba,$aa,$aa,$ab ; $ff92 (*)
    .byte   $ba,$00,$00,$45,$45,$c7,$45,$47 ; $ff9a (*)
    .byte   $00,$00,$67,$55,$65,$55,$67,$00 ; $ffa2 (*)
    .byte   $00,$52,$52,$62,$52,$67,$00,$03 ; $ffaa (*)
    .byte   $02,$01                         ; $ffb2 (*)
    
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
    .byte   $00,$00,$00,$d0                 ; $fffa (*)
    .byte   $00,$00                         ; $fffe (D)


;***********************************************************
;      Bank 1 / 0..1
;***********************************************************

    SEG     CODE
    ORG     $1000
    RORG    $f000

resetBank1
    bit     bank0Strobe
Start
    cld
    ldx     #$ff
    txs
    inx
    txa
Lf009
    sta     VSYNC,x
    inx
    bne     Lf009
    jsr     Lfe6e
Lf011
    ldy     #$29
    sty     TIM64T
    lda     ram_80
    asl
    asl
    asl
    eor     ram_80
    asl
    rol     ram_80
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
    lda     ram_80
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
    lda     ram_80
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
    jmp     Lfc38
    
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
    beq     Lfc38
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
Lfc38
    sta     WSYNC
;---------------------------------------
    jmp     resetBank1
    
bank1Handler
    bit     ram_F9
    bmi     Lfc89
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
    bne     Lfc89
    bit     ram_C6
    bpl     Lfc85
    ldx     #$50
    lda     ram_B5
    beq     Lfc89
    and     #$01
    bne     Lfc82
    lda     ram_F0
    bne     Lfc89
    dec     ram_F0
    .byte   $2c ;bit                ;4-5 =  30 *
Lfc79
    inc     ram_F0
    ldx     #$72
    jsr     Lfce6
    bne     Lfc89
Lfc82
    jsr     Lfcc7
Lfc85
    lda     ram_F0
    bne     Lfc79
Lfc89
    ldx     INTIM
    bne     Lfc89
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
    bit     ram_80
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
    sta     ram_80,x
    dex
    bne     Lfe69
Lfe6e
    ldx     #$21
Lfe70
    lda     Lfd64,x
    sta     ram_80,x
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
    .byte   %11000000 ; |**      |            $ff01 (P)
    .byte   %10000000 ; |*       |            $ff02 (P)
    .byte   %10000000 ; |*       |            $ff03 (P)
    .byte   %00000000 ; |        |            $ff04 (P)
    .byte   %00000000 ; |        |            $ff05 (P)
    .byte   %00000000 ; |        |            $ff06 (P)
    .byte   %00000000 ; |        |            $ff07 (P)
    .byte   %00000000 ; |        |            $ff08 (P)
    .byte   %00000000 ; |        |            $ff09 (P)
    .byte   %00000000 ; |        |            $ff0a (P)
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
    
    .byte   $00,$00,$00,$00,$10,$10,$00,$00 ; $ff19 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ff21 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ff29 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ff31 (*)
    .byte   $00,$00,$10,$18,$10,$28,$00,$00 ; $ff39 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ff41 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ff49 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ff51 (*)
    .byte   $00,$00,$18,$10,$38,$10,$28,$00 ; $ff59 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ff61 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ff69 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ff71 (*)
    .byte   $00,$18,$10,$54,$38,$10,$28,$44 ; $ff79 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ff81 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ff89 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ff91 (*)
    .byte   $10,$18,$10,$10,$d6,$38,$10,$28 ; $ff99 (*)
    .byte   $c6,$00,$00,$00,$00,$00,$00,$00 ; $ffa1 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ffa9 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ffb1 (*)
    .byte   $10,$18,$38,$10,$10,$54,$38,$10 ; $ffb9 (*)
    .byte   $28,$44,$00,$00,$00,$00,$00,$00 ; $ffc1 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ffc9 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$10 ; $ffd1 (*)
    .byte   $38,$18,$38,$10,$92,$54,$38,$10 ; $ffd9 (*)
    .byte   $28,$44,$82,$00,$00,$00,$00,$00 ; $ffe1 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $ffe9 (*)
    .byte   $00,$00,$00,$00,$00,$00,$00     ; $fff1 (*)
Lfff8
    .byte   $00                             ; $fff8 (D)
    .byte   $00,$00,$00                     ; $fff9 (*)
    .byte   $03,$f0,$00,$00                 ; $fffc (D)
