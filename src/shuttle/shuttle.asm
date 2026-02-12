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

BANK0STROBE     = $FFF8
BANK1STROBE     = $FFF9

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

Reset_Bank0
    bit     BANK1STROBE                     ; $f000 (*)
    
Start
    stx     ENAM0                   ;3        
    stx     ENAM1                   ;3        
    stx     COLUPF                  ;3        
    dex                             ;2        
    stx     PF0                     ;3        
    sta     RESBL                   ;3        
    stx     PF1                     ;3        
    stx     PF2                     ;3        
    lda     #BLACK|$6               ;2        
    sta     COLUBK                  ;3        
    lda     #$c0                    ;2        
    sta     HMBL                    ;3        
    sta     ENABL                   ;3        
    txs                             ;2        
    inx                             ;2        
    stx     GRP0                    ;3        
    lda     #BLUE|$2                ;2        
    sta     COLUP1                  ;3        
    lda     #BLUE|$a                ;2        
    sta     COLUP0                  ;3        
    stx     NUSIZ0                  ;3        
    stx     REFP0                   ;3        
    lda     #$05                    ;2        
    sta     NUSIZ1                  ;3        
    stx     PF2                     ;3        
    sta     HMOVE                   ;3        
    stx     PF0                     ;3        
    stx     PF1                     ;3        
    lda     ram_82                  ;3        
    sta     HMCLR                   ;3        
    jsr     $d958                   ;6        
    lda     ram_83                  ;3        
    inx                             ;2        
    jsr     $d958                   ;6        
    lda     #YELLOW|$8              ;2        
    sta     COLUPF                  ;3        
    ldx     #$07                    ;2   = 106
Lf04b
    ldy     #$00                    ;2        
    lda     $d9e8,x                 ;4        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     HMOVE                   ;3        
    sty     PF0                     ;3        
    sta     CTRLPF                  ;3        
    lda     #$7f                    ;2        
    sta     ENABL                   ;3        
    sta     PF1                     ;3        
    dey                             ;2        
    sty     GRP1                    ;3        
    lda     $dbfa,x                 ;4        
    sta     GRP0                    ;3        
    sty     PF2                     ;3        
    sta     HMCLR                   ;3        
    lda     #$c0                    ;2        
    sta     PF0                     ;3        
    sty     PF1                     ;3        
    dex                             ;2        
    bpl     Lf04b                   ;2/3      
    sta     WSYNC                   ;3   =  50
;---------------------------------------
    sta     HMOVE                   ;3        
    iny                             ;2        
    sty     PF0                     ;3        
    sty     PF1                     ;3        
    sty     PF2                     ;3        
    sty     ENABL                   ;3        
    sty     GRP0                    ;3        
    sty     GRP1                    ;3        
    nop                             ;2        
    stx     RESBL                   ;3        
    ldx     ram_E3                  ;3        
    cpx     #$04                    ;2        
    bcc     Lf0a3                   ;2/3      
    lda     ram_B5                  ;3         *
    cmp     #$02                    ;2         *
    bne     Lf0a3                   ;2/3       *
    lda     ram_C1                  ;3         *
    and     $debd,x                 ;4         *
    bne     Lf0a3                   ;2/3       *
    bit     ram_80                  ;3         *
    bmi     Lf0a1                   ;2/3       *
    dec     ram_B2                  ;5         *
    .byte   $2c ;bit                ;4-5 =  60 *
Lf0a1
    inc     ram_B2                  ;5   =   5 *
Lf0a3
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    sty     COLUP0                  ;3        
    sty     COLUP1                  ;3        
    sty     COLUPF                  ;3        
    lda     #$01                    ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    sta     CTRLPF                  ;3        
    ldx     #$05                    ;2        
    lda     #$02                    ;2        
    sta     HMCLR                   ;3        
    sta     ENABL                   ;3        
    sta     RESP1                   ;3   =  36
Lf0bf
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$30                    ;2        
    sta     PF0                     ;3        
    lda     $dafa,x                 ;4        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    sta     RESP0                   ;3        
    sta.w   RESP1                   ;4        
    sta.w   RESP0                   ;4        
    sta.w   RESP1                   ;4        
    sta.w   RESP0                   ;4        
    sta.w   RESP1                   ;4        
    sta.w   RESP0                   ;4        
    sta.w   RESP1                   ;4        
    sta.w   RESP0                   ;4        
    sta.w   RESP1                   ;4        
    sta.w   RESP0                   ;4        
    sty     PF0                     ;3        
    dex                             ;2        
    bne     Lf0bf                   ;2/3      
    sta     WSYNC                   ;3   =  74
;---------------------------------------
    sta     HMOVE                   ;3        
    stx     GRP0                    ;3        
    stx     GRP1                    ;3        
    stx     ENABL                   ;3        
    stx     NUSIZ0                  ;3        
    stx     NUSIZ1                  ;3        
    inx                             ;2        
    lda     #BLUE|$9                ;2        
    sta     RESP0                   ;3        
    stx     CTRLPF                  ;3        
    sta     COLUPF                  ;3        
    lda     #YELLOW|$8              ;2        
    sta     COLUP0                  ;3        
    ldx     #BLACK|$c               ;2        
    lda     ram_A1                  ;3        
    beq     Lf11d                   ;2/3      
    lda     ram_C1                  ;3        
    and     #$20                    ;2        
    beq     Lf11d                   ;2/3      
    ldx     #$82                    ;2   =  52 *
Lf11d
    stx     COLUP1                  ;3        
    ldx     #$08                    ;2        
    lda     ram_81                  ;3        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     HMOVE                   ;3        
    sec                             ;2   =   5
Lf128
    sbc     #$0f                    ;2        
    bcs     Lf128                   ;2/3      
    eor     #$0f                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    adc     #$80                    ;2        
    sta     COLUBK,x                ;4        
    sta     HMP1                    ;3   =  23
Lf138
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     $d9ef,x                 ;4        
    sta     GRP0                    ;3        
    lda     $dfed,x                 ;4        
    sta     GRP1                    ;3        
    sty     PF0                     ;3        
    lda     #$7f                    ;2        
    sta     PF1                     ;3        
    dey                             ;2        
    sty     PF2                     ;3        
    sta     HMCLR                   ;3        
    lda     #$c0                    ;2        
    sta     PF0                     ;3        
    sty     PF1                     ;3        
    iny                             ;2        
    dex                             ;2        
    bne     Lf138                   ;2/3      
    sta     WSYNC                   ;3   =  50
;---------------------------------------
    sta     HMOVE                   ;3        
    sty     GRP1                    ;3        
    sty     GRP0                    ;3        
    sty     GRP1                    ;3        
    sty     PF0                     ;3        
    sty     PF1                     ;3        
    sty     PF2                     ;3        
    lda     ram_B1                  ;3        
    sec                             ;2        
    sbc     ram_B0                  ;3        
    cmp     #$80                    ;2        
    bne     Lf180                   ;2/3      
    ldx     ram_E2                  ;3         *
    cpx     #$ff                    ;2         *
    bne     Lf180                   ;2/3       *
    inc     ram_E2                  ;5         *
    lda     ram_80                  ;3         *
    sta     ram_B2                  ;3   =  51 *
Lf180
    ldx     #BLACK|$4               ;2   =   2
Lf182
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    dex                             ;2        
    bne     Lf182                   ;2/3      
    stx     COLUBK                  ;3        
    stx     REFP0                   ;3        
    stx     REFP1                   ;3        
    inx                             ;2        
    stx     VDELP0                  ;3        
    stx     VDELP1                  ;3        
    ldx     #$03                    ;2        
    stx     NUSIZ0                  ;3        
    stx.w   NUSIZ1                  ;4        
    ldy     #$a0                    ;2        
    sta     RESP0                   ;3        
    ldx     #$10                    ;2        
    sta     RESP1                   ;3        
    sty     HMP0                    ;3        
    stx     HMP1                    ;3        
    ldy     #BROWN|$6               ;2        
    sta     RESM0                   ;3        
    sty     HMM0                    ;3        
    ldx     #BLUE_CYAN|$2           ;2        
    lda     ram_9B                  ;3        
    cmp     #$10                    ;2        
    sta     WSYNC                   ;3   =  67
;---------------------------------------
    sta     HMOVE                   ;3        
    sty     COLUBK                  ;3        
    lda     #$fe                    ;2        
    sta     PF2                     ;3        
    bcs     Lf1cd                   ;2/3      
    lda     ram_C1                  ;3         *
    and     #$10                    ;2         *
    beq     Lf1cd                   ;2/3       *
    ldx     #$86                    ;2         *
    stx     COLUPF                  ;3         *
    ldy     #$80                    ;2         *
    bne     Lf1e1                   ;2/3 =  29 *
Lf1cd
    stx     COLUPF                  ;3        
    ldy     #BLACK|$a               ;2        
    lda     ram_85                  ;3        
    cmp     #$19                    ;2        
    bcc     Lf1df                   ;2/3      
    beq     Lf1e1                   ;2/3      
    lda     ram_C1                  ;3         *
    and     #$10                    ;2         *
    beq     Lf1e1                   ;2/3 =  21 *
Lf1df
    ldy     #$58                    ;2   =   2 *
Lf1e1
    sty     COLUP0                  ;3        
    sty     COLUP1                  ;3        
    sta     HMCLR                   ;3        
    sta     WSYNC                   ;3   =  12
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     WSYNC                   ;3   =   6
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     #$06                    ;2        
    sta     ram_FA                  ;3        
    jsr     $d979                   ;6        
    ldy     #$00                    ;2        
    sty     GRP0                    ;3        
    sty     GRP1                    ;3        
    sta     HMOVE                   ;3        
    sty     GRP0                    ;3        
    sty     ram_F4                  ;3        
    sty     ram_F5                  ;3        
    sty     NUSIZ0                  ;3        
    lda     ram_F1                  ;3        
    bne     Lf217                   ;2/3      
    lda     ram_85                  ;3        
    cmp     #$17                    ;2        
    beq     Lf214                   ;2/3      
    cmp     #$05                    ;2        
    bcs     Lf217                   ;2/3 =  53
Lf214
    asl                             ;2         *
    sta     ENAM0                   ;3   =   5 *
Lf217
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    ldx     #$03                    ;2        
    stx     NUSIZ0                  ;3        
    inx                             ;2        
    stx     ENAM0                   ;3        
    lda     ram_AD                  ;3        
    clc                             ;2        
    adc     #$38                    ;2        
    jsr     $d958                   ;6        
    sty     COLUPF                  ;3        
    ldx     #CYAN_GREEN|$0          ;2        
    stx     COLUP0                  ;3        
    stx     COLUP1                  ;3        
    ldy     #$18                    ;2        
    lda     #$d0                    ;2        
    sta     HMP0                    ;3        
    sta     HMP1                    ;3        
    sta     WSYNC                   ;3   =  50
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     #BLACK|$b               ;2        
    sta     COLUPF                  ;3        
    lda     #$17                    ;2        
    sta     CTRLPF                  ;3        
    lda     #$d9                    ;2        
    sta     ram_CD                  ;3        
    lda     ram_84                  ;3        
    and     #$01                    ;2        
    beq     Lf252                   ;2/3      
    lda     #$7d                    ;2   =  27
Lf252
    ldx     #$08                    ;2        
    clc                             ;2        
    sta     HMCLR                   ;3        
    sta     WSYNC                   ;3   =  10
;---------------------------------------
    sta     HMOVE                   ;3   =   3
Lf25b
    sta     ram_CE,x                ;4        
    adc     #$19                    ;2        
    dex                             ;2        
    dex                             ;2        
    bpl     Lf25b                   ;2/3      
    sta     WSYNC                   ;3   =  15
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     ram_84                  ;3        
    lsr                             ;2        
    clc                             ;2        
    adc     #$da                    ;2        
    sta     ram_D7                  ;3        
    sta     ram_D5                  ;3        
    sta     ram_D3                  ;3        
    sta     ram_D1                  ;3        
    sta     ram_CF                  ;3        
    lda     ram_C1                  ;3        
    ldx     ram_84                  ;3        
    bit     ram_E1                  ;3        
    bpl     Lf285                   ;2/3      
    and     #$fc                    ;2         *
    beq     Lf28a                   ;2/3       *
    bne     Lf290                   ;2/3 =  44 *
Lf285
    and     $d9f8,x                 ;4        
    beq     Lf290                   ;2/3 =   6
Lf28a
    lda     #$e5                    ;2         *
    sec                             ;2         *
    sbc     ram_AC                  ;3         *
    .byte   $2c ;bit                ;4-2 =   9 *
Lf290
    lda     #$cc                    ;2        
    sta     ram_CC                  ;3        
    lda     #$06                    ;2        
    sta     PF2                     ;3   =  10
Lf298
    lda     (ram_CC),y              ;5        
    sta     ENABL                   ;3        
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     HMOVE                   ;3        
    sty     ram_FB                  ;3        
    lda     (ram_D6),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_D4),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_D2),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_D0),y              ;5        
    tax                             ;2        
    lda     (ram_CE),y              ;5        
    ldy     #$00                    ;2        
    stx     GRP1                    ;3        
    sta     GRP0                    ;3        
    sty     GRP1                    ;3        
    sty     GRP0                    ;3        
    ldy     ram_FB                  ;3        
    dey                             ;2        
    bpl     Lf298                   ;2/3      
    iny                             ;2        
    sty     GRP1                    ;3        
    sty     ENABL                   ;3        
    sta     WSYNC                   ;3   =  74
;---------------------------------------
    sta     HMOVE                   ;3        
    nop                             ;2        
    lda     #$fe                    ;2        
    sta     PF2                     ;3        
    lda     #BLACK|$c               ;2        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    ldy     #$07                    ;2        
    nop                             ;2        
    ldx     #$40                    ;2        
    stx     HMP0                    ;3        
    stx     HMP1                    ;3        
    lda     ram_C8                  ;3        
    and     #$1f                    ;2        
    cmp     #$14                    ;2        
    stx     HMBL                    ;3        
    sta     RESBL                   ;3        
    bcs     Lf2f4                   ;2/3      
    ldy     #$00                    ;2        
    cmp     #$0c                    ;2        
    bcc     Lf2f4                   ;2/3      
    sbc     #$0c                    ;2        
    tay                             ;2   =  55
Lf2f4
    sty     ram_FA                  ;3        
    tya                             ;2        
    eor     #$07                    ;2        
    sta     ram_FB                  ;3        
    lda     #$e8                    ;2        
    ldx     #$08                    ;2        
    sta     WSYNC                   ;3   =  17
;---------------------------------------
    sta     HMOVE                   ;3        
    sec                             ;2   =   5
Lf304
    sta     ram_CE,x                ;4        
    sbc     #$08                    ;2        
    sta     ram_CC,x                ;4        
    sbc     #$08                    ;2        
    dex                             ;2        
    dex                             ;2        
    dex                             ;2        
    dex                             ;2        
    bpl     Lf304                   ;2/3      
    sta     HMCLR                   ;3        
    sta     HMOVE                   ;3        
    ldx     #$00                    ;2        
    stx     PF2                     ;3        
    stx     COLUPF                  ;3        
    lda     #BLACK|$8               ;2        
    sta     COLUBK                  ;3        
    lda     #$d8                    ;2        
    sta     ram_CD                  ;3        
    sta     ram_CF                  ;3        
    sta     ram_D1                  ;3        
    sta     ram_D3                  ;3        
    sta     ram_D5                  ;3        
    sta     ram_D7                  ;3        
    sta     WSYNC                   ;3   =  64
;---------------------------------------
    stx     COLUBK                  ;3        
    jsr     $d979                   ;6        
    lda     #$c0                    ;2        
    sta     HMP0                    ;3        
    sta     HMP1                    ;3        
    sta     HMOVE                   ;3        
    ldx     #$00                    ;2        
    stx     GRP0                    ;3        
    stx     GRP1                    ;3        
    stx     GRP0                    ;3        
    dex                             ;2        
    stx     PF0                     ;3        
    stx     PF1                     ;3        
    ldx     #$07                    ;2        
    stx     PF2                     ;3        
    lda     #$31                    ;2        
    sta     CTRLPF                  ;3        
    lda     #$01                    ;2        
    sta     NUSIZ1                  ;3        
    sta     HMCLR                   ;3        
    lda     #$10                    ;2        
    sta     HMBL                    ;3        
    stx     ENABL                   ;3   =  65
Lf35e
    lda     $d8bb,x                 ;4        
    sta     WSYNC                   ;3   =   7
;---------------------------------------
    sta     HMOVE                   ;3        
    tay                             ;2        
    nop                             ;2        
    lda     $d89b,x                 ;4        
    sta     GRP0                    ;3        
    lda     $d8f0,x                 ;4        
    sta     COLUPF                  ;3        
    lda     $d8a3,x                 ;4        
    sta     GRP1                    ;3        
    lda     $d8ab,x                 ;4        
    sta     GRP0                    ;3        
    lda     $d8b3,x                 ;4        
    txs                             ;2        
    ldx     #BLACK|$0               ;2        
    sta     GRP1                    ;3        
    sty     GRP0                    ;3        
    sta     GRP1                    ;3        
    stx     COLUPF                  ;3        
    tsx                             ;2        
    dex                             ;2        
    dec     ram_FB                  ;5        
    bpl     Lf35e                   ;2/3      
    ldx     #$ff                    ;2        
    txs                             ;2        
    lda     #$02                    ;2        
    sta     VBLANK                  ;3        
    inx                             ;2        
    stx     GRP0                    ;3        
    stx     GRP1                    ;3        
    stx     GRP0                    ;3        
    stx     NUSIZ1                  ;3        
    ldx     ram_B4                  ;3        
    beq     Lf3aa                   ;2/3      
    lda     ram_C1                  ;3         *
    and     #$02                    ;2         *
    bne     Lf3ac                   ;2/3       *
    inx                             ;2   = 103 *
Lf3aa
    stx     WSYNC                   ;3   =   3
;---------------------------------------
Lf3ac
    stx     WSYNC                   ;3   =   3
;---------------------------------------
    cpx     #$05                    ;2        
    beq     Lf3b4                   ;2/3      
    stx     WSYNC                   ;3   =   7
;---------------------------------------
Lf3b4
    ldx     #$dc                    ;2        
    stx     TIM8T                   ;4        
    bit     ram_F9                  ;3        
    bmi     Lf403                   ;2/3!     
    lda     ram_B6                  ;3        
    beq     Lf403                   ;2/3!     
    lda     ram_A4                  ;3         *
    beq     Lf403                   ;2/3!      *
    ldy     ram_84                  ;3         *
    beq     Lf3cf                   ;2/3       *
    cpy     #$03                    ;2         *
    bne     Lf403                   ;2/3!      *
    adc     #$14                    ;2   =  32 *
Lf3cf
    tax                             ;2         *
    lda     $d868,x                 ;4         *
    ldx     ram_F8                  ;3         *
    inx                             ;2         *
    clc                             ;2         *
    adc     $dafa,x                 ;4         *
    cmp     ram_F6                  ;3         *
    bcc     Lf3e7                   ;2/3       *
    sbc     $d9e6,x                 ;4         *
    cmp     ram_F6                  ;3         *
    bcc     Lf3f9                   ;2/3       *
    beq     Lf3f9                   ;2/3 =  33 *
Lf3e7
    inc     ram_F5                  ;5         *
    ldx     ram_E3                  ;3         *
    cpx     #$06                    ;2         *
    bne     Lf3f1                   ;2/3       *
    inc     ram_F5                  ;5   =  17 *
Lf3f1
    lda     #$41                    ;2         *
    ldx     ram_CA                  ;3         *
    bne     Lf403                   ;2/3!      *
    beq     Lf401                   ;2/3!=   9 *
Lf3f9
    lda     #$00                    ;2         *
    ldx     ram_CA                  ;3         *
    cpx     #$41                    ;2         *
    bne     Lf403                   ;2/3 =   9 *
Lf401
    sta     ram_CA                  ;3   =   3 *
Lf403
    ldx     #$00                    ;2        
    lda     ram_81                  ;3        
    cmp     ram_83                  ;3        
    bcc     Lf411                   ;2/3      
    sbc     #$0b                    ;2        
    cmp     ram_83                  ;3        
    bcc     Lf412                   ;2/3 =  17
Lf411
    dex                             ;2   =   2 *
Lf412
    stx     ram_A1                  ;3        
    lda     SWCHA                   ;4        
    cmp     #$ff                    ;2        
    beq     Lf422                   ;2/3      
    ldx     #$ff                    ;2         *
    stx     ram_F7                  ;3         *
    inx                             ;2         *
    stx     ram_B8                  ;3   =  21 *
Lf422
    inc     ram_B9                  ;5        
    bne     Lf444                   ;2/3      
    lda     ram_BA                  ;3        
    cmp     #$0f                    ;2        
    bcc     Lf430                   ;2/3      
    ldx     ram_B6                  ;3         *
    stx     ram_B7                  ;3   =  20 *
Lf430
    inc     ram_BA                  ;5        
    bne     Lf444                   ;2/3      
    inc     ram_B8                  ;5         *
    lda     ram_B8                  ;3         *
    and     #$04                    ;2         *
    beq     Lf444                   ;2/3       *
    sta     ram_B8                  ;3         *
    lda     #$00                    ;2         *
    sta     ram_B6                  ;3         *
    sta     ram_B7                  ;3   =  30 *
Lf444
    lda     ram_84                  ;3        
    cmp     #$04                    ;2        
    bne     Lf45f                   ;2/3      
    lda     ram_E9                  ;3         *
    bmi     Lf45f                   ;2/3       *
    dec     ram_EE                  ;5         *
    bne     Lf45f                   ;2/3       *
    asl                             ;2         *
    ora     #$02                    ;2         *
    sta     ram_EE                  ;3         *
    lda     ram_CA                  ;3         *
    bne     Lf45f                   ;2/3       *
    lda     #$34                    ;2         *
    sta     ram_CA                  ;3   =  36 *
Lf45f
    lda     ram_C1                  ;3        
    and     #$07                    ;2        
    bne     Lf48b                   ;2/3      
    bit     ram_F9                  ;3        
    bpl     Lf481                   ;2/3      
    lda     ram_B5                  ;3        
    cmp     #$02                    ;2        
    bne     Lf477                   ;2/3      
    dec     ram_88                  ;5         *
    bpl     Lf481                   ;2/3 =  26 *
Lf473
    inc     ram_88                  ;5         *
    bpl     Lf481                   ;2/3 =   7 *
Lf477
    cmp     #$04                    ;2        
    beq     Lf481                   ;2/3      
    lda     ram_88                  ;3        
    cmp     #$0d                    ;2        
    bne     Lf473                   ;2/3 =  11
Lf481
    lda     ram_B7                  ;3        
    beq     Lf489                   ;2/3      
    ldx     #$01                    ;2        
    stx     ram_C8                  ;3   =  10
Lf489
    dec     ram_C8                  ;5   =   5
Lf48b
    ldx     ram_C9                  ;3        
    cpx     #$fe                    ;2        
    bne     Lf495                   ;2/3      
    stx     ram_CB                  ;3        
    inc     ram_C9                  ;5   =  15
Lf495
    lda     ram_CA                  ;3        
    beq     Lf4c8                   ;2/3      
    sta     AUDC0                   ;3         *
    jsr     $ddfb                   ;6         *
    tax                             ;2         *
    lda     $d94c,x                 ;4         *
    inc     ram_CB                  ;5         *
    cmp     ram_CB                  ;3         *
    bcs     Lf4d0                   ;2/3       *
    lda     #$00                    ;2         *
    sta     ram_CB                  ;3         *
    inc     ram_C9                  ;5         *
    lda     $d8f7,x                 ;4         *
    adc     ram_C9                  ;3         *
    tax                             ;2         *
    lda     $d903,x                 ;4         *
    beq     Lf4c8                   ;2/3       *
    sta     AUDF0                   ;3         *
    jsr     $ddfb                   ;6         *
    ldx     ram_B6                  ;3         *
    bne     Lf4c3                   ;2/3       *
    txa                             ;2   =  71 *
Lf4c3
    sta     AUDV0                   ;3         *
    jmp     $d4d0                   ;3   =   6 *
    
Lf4c8
    sta     AUDV0                   ;3        
    sta     ram_CA                  ;3        
    ldx     #$fe                    ;2        
    stx     ram_C9                  ;3   =  11
Lf4d0
    lda     ram_B6                  ;3        
    beq     Lf502                   ;2/3!     
    
    .byte   $a9,$03,$85,$1a,$a9,$06,$85,$16 ; $f4d4 (*)
    .byte   $a9,$1f,$85,$18,$a5,$81,$c9,$0f ; $f4dc (*)
    .byte   $d0,$04,$a6,$b4,$f0,$1c,$a2,$08 ; $f4e4 (*)
    .byte   $86,$16,$20,$fb,$dd,$18,$69,$02 ; $f4ec (*)
    .byte   $65,$b4,$a6,$ca,$d0,$0a,$a2,$02 ; $f4f4 (*)
    .byte   $86,$15,$a2,$0e,$86,$17         ; $f4fc (*)
    
Lf502
    sta     AUDV0                   ;3        
    sta     AUDV1                   ;3        
    lda     ram_B7                  ;3        
    beq     Lf52a                   ;2/3      
    lda     ram_B5                  ;3        
    bne     Lf514                   ;2/3      
    jmp     $d728                   ;3   =  24
    
Lf511
    jmp     $d647                   ;3   =   3
    
Lf514
    .byte   $a5,$b4,$f0,$06,$a9,$02,$85,$ab ; $f514 (*)
    .byte   $d0,$1e,$a5,$c4,$d0,$1a,$a5,$a6 ; $f51c (*)
    .byte   $c9,$df,$b0,$02,$a9,$e0         ; $f524 (*)
    
Lf52a
    sbc     #$08                    ;2        
    eor     #$ff                    ;2        
    bit     ram_F9                  ;3        
    bpl     Lf534                   ;2/3      
    
    .byte   $a9,$18                         ; $f532 (*)
    
Lf534
    sta     ram_FA                  ;3        
    ldx     ram_AF                  ;3        
    beq     Lf53c                   ;2/3      
    
    .byte   $e6,$be                         ; $f53a (*)
    
Lf53c
    inc     ram_BE                  ;5        
    cmp     ram_BE                  ;3        
    bcs     Lf511                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_BE                  ;3        
    inc     ram_B1                  ;5        
    bpl     Lf554                   ;2/3      
    
    .byte   $a5,$ea,$c9,$08,$f0,$08         ; $f54a (*)
    
Lf550
    inc     ram_EA                  ;5        
    bpl     Lf558                   ;2/3 =  46
Lf554
    dec     ram_EA                  ;5        
    bmi     Lf550                   ;2/3 =   7
Lf558
    inc     ram_E0                  ;5        
    lda     ram_B5                  ;3        
    cmp     #$02                    ;2        
    bne     Lf564                   ;2/3      
    
    .byte   $24,$f9,$30,$05                 ; $f560 (*)
    
Lf564
    lda     ram_E0                  ;3        
    lsr                             ;2        
    bcc     Lf56b                   ;2/3      
    inc     ram_B0                  ;5   =  24
Lf56b
    inc     ram_EF                  ;5        
    lda     ram_B7                  ;3        
    beq     Lf577                   ;2/3      
    
    .byte   $a5,$ab,$29,$01,$f0,$1d         ; $f571 (*)
    
Lf577
    dec     ram_89                  ;5        
    lda     ram_89                  ;3        
    cmp     #$0e                    ;2        
    bne     Lf594                   ;2/3      
    lda     #$14                    ;2        
    sta     ram_89                  ;3        
    ldx     #$0b                    ;2        
    lda     ram_8A,x                ;4        
    tay                             ;2        
    dex                             ;2        
    bmi     Lf592                   ;2/3      
    lda     ram_8A,x                ;4        
    sty     ram_8A,x                ;4        
    jmp     $d587                   ;3   =  50
    
Lf592
    sty     ram_95                  ;3   =   3
Lf594
    lda     ram_AB                  ;3        
    and     #$08                    ;2        
    beq     Lf5ab                   ;2/3      
    
    .byte   $a2,$0b,$d6,$8a,$b5,$8a,$c9,$ff ; $f59a (*)
    .byte   $d0,$04,$a9,$84,$95,$8a,$ca,$10 ; $f5a2 (*)
    .byte   $f1                             ; $f5aa (*)
    
Lf5ab
    lda     ram_AB                  ;3        
    and     #$02                    ;2        
    beq     Lf5d4                   ;2/3      
    
    .byte   $e6,$89,$a5,$89,$c9,$15,$d0,$17 ; $f5b1 (*)
    .byte   $a9,$0f,$85,$89,$a2,$00,$b5,$8a ; $f5b9 (*)
    .byte   $a8,$e8,$e0,$0c,$f0,$07,$b5,$8a ; $f5c1 (*)
    .byte   $94,$8a,$4c,$c1,$d5,$85,$8a,$c6 ; $f5c9 (*)
    .byte   $ef,$c6,$ef                     ; $f5d1 (*)
    
Lf5d4
    lda     ram_AB                  ;3        
    and     #$04                    ;2        
    beq     Lf5eb                   ;2/3      
    
    .byte   $a2,$0b,$f6,$8a,$b5,$8a,$c9,$85 ; $f5da (*)
    .byte   $90,$04,$a9,$00,$95,$8a,$ca,$10 ; $f5e2 (*)
    .byte   $f1                             ; $f5ea (*)
    
Lf5eb
    lda     ram_B7                  ;3        
    beq     Lf5f5                   ;2/3      
    
    .byte   $a5,$b5,$c9,$02,$f0,$03         ; $f5ef (*)
    
Lf5f5
    jmp     $d728                   ;3   =  29
    
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
    lda     ram_B0                  ;3        
    sec                             ;2        
    sbc     ram_B1                  ;3        
    cmp     #$10                    ;2        
    bcc     Lf653                   ;2/3      
    jmp     $d6d2                   ;3   =  15
    
Lf653
    lsr                             ;2        
    tax                             ;2        
    lda     $d893,x                 ;4        
    sta     NUSIZ0                  ;3        
    lda     $dfde,x                 ;4        
    sta     ram_FB                  ;3        
    lda     $d9c2,x                 ;4        
    clc                             ;2        
    adc     ram_EA                  ;3        
    cmp     #$9f                    ;2        
    bcc     Lf66b                   ;2/3      
    
    .byte   $a9,$9f                         ; $f669 (*)
    
Lf66b
    sta     COLUP0                  ;3        
    ldy     #$00                    ;2        
    lda     ram_B2                  ;3        
    clc                             ;2        
    adc     #$50                    ;2        
    cmp     #$a0                    ;2        
    bcs     Lf684                   ;2/3      
    adc     #$b0                    ;2        
    bcs     Lf681                   ;2/3      
    
    .byte   $c8,$49,$ff,$69,$01             ; $f67c (*)
    
Lf681
    cmp     $dfe6,x                 ;4   =  55
Lf684
    bcs     Lf6d0                   ;2/3      
    cpx     #$00                    ;2        
    beq     Lf696                   ;2/3      
    
    .byte   $86,$fc,$e0,$06,$90,$01,$98,$4a ; $f68a (*)
    .byte   $c6,$fc,$d0,$fb                 ; $f692 (*)
    
Lf696
    sta     ram_FA                  ;3        
    lda     ram_B2                  ;3        
    cpy     #$01                    ;2        
    beq     Lf6a2                   ;2/3      
    clc                             ;2        
    adc     ram_FA                  ;3        
    .byte   $2c ;bit                ;4-3 =  22
Lf6a2
    sbc     ram_FA                  ;3        
    clc                             ;2        
    adc     #$50                    ;2        
    cmp     ram_DF                  ;3        
    beq     Lf6b8                   ;2/3      
    lda     ram_C1                  ;3        
    and     #$03                    ;2        
    bne     Lf6b8                   ;2/3      
    bcs     Lf6b6                   ;2/3      
    
    .byte   $c6,$df,$2c                     ; $f6b3 (*)
    
Lf6b6
    inc     ram_DF                  ;5   =  26
Lf6b8
    ldy     ram_DF                  ;3        
    lda     ram_C1                  ;3        
    lsr                             ;2        
    sta     REFP0                   ;3        
    and     #$08                    ;2        
    bne     Lf6c7                   ;2/3      
    txa                             ;2        
    ora     #$08                    ;2        
    tax                             ;2   =  21
Lf6c7
    sec                             ;2        
    tya                             ;2        
    sbc     $d9ca,x                 ;4        
    bcc     Lf728                   ;2/3!     
    
    .byte   $b0,$3b                         ; $f6ce (*)
Lf6d0
    .byte   $b0,$56                         ; $f6d0 (*)
    
Lf6d2
    cmp     #$80                    ;2        
    bcc     Lf6de                   ;2/3      
    ldx     #$ce                    ;2        
    ldy     #$05                    ;2        
    lda     #BLUE_CYAN|$d           ;2        
    bcs     Lf6e8                   ;2/3 =  22
    
Lf6de
    .byte   $c9,$40,$90,$46,$a2,$0e,$a0,$00 ; $f6de (*)
    .byte   $a9,$90                         ; $f6e6 (*)
    
Lf6e8
    stx     ram_FB                  ;3        
    sty     NUSIZ0                  ;3        
    sta     COLUP0                  ;3        
    lda     ram_DF                  ;3        
    beq     Lf728                   ;2/3!     
    cmp     #$94                    ;2        
    beq     Lf728                   ;2/3!     
    lda     #$94                    ;2        
    ldx     ram_B2                  ;3        
    cpx     #$80                    ;2        
    bcc     Lf700                   ;2/3!     
    
    .byte   $a9,$00                         ; $f6fe (*)
    
Lf700
    cmp     ram_DF                  ;3        
    bcs     Lf707                   ;2/3      
    
    .byte   $c6,$df,$2c                     ; $f704 (*)
    
Lf707
    inc     ram_DF                  ;5        
    lda     ram_DF                  ;3        
    sta     ram_DE                  ;3        
    lda     ram_B5                  ;3        
    cmp     #$02                    ;2        
    bne     Lf728                   ;2/3      
    
    .byte   $a5,$88,$d0,$11,$a5,$b3,$69,$0f ; $f713 (*)
    .byte   $c9,$21,$b0,$09,$a5,$b7,$f0,$05 ; $f71b (*)
    .byte   $a5,$a5,$e9,$c2,$2c             ; $f723 (*)
    
Lf728
    lda     #$6a                    ;2        
    sta     ram_FA                  ;3        
    ldx     #$04                    ;2        
    lda     #$14                    ;2        
    sec                             ;2        
    sbc     ram_89                  ;3        
    adc     ram_FA                  ;3        
    sec                             ;2        
    sbc     #$15                    ;2        
    sta     ram_FA                  ;3        
    bpl     Lf756                   ;2/3      
    
    .byte   $69,$16,$18,$65,$fb,$4c,$58,$d7 ; $f73c (*)
    
Lf744
    lda     ram_FA                  ;3        
    sec                             ;2        
    sbc     #$12                    ;2        
    sta     ram_FA                  ;3        
    bpl     Lf756                   ;2/3      
    
    .byte   $c9,$e2,$90,$05,$69,$15,$4c,$3e ; $f74d (*)
    .byte   $d7                             ; $f755 (*)
    
Lf756
    lda     #$04                    ;2        
    sta     ram_D8,x                ;4        
    dex                             ;2        
    bpl     Lf744                   ;2/3      
    lda     #$ff                    ;2        
    sta     ram_DD                  ;3        
    lda     ram_84                  ;3        
    bne     Lf795                   ;2/3 = 108
    
Lf765
    .byte   $20,$da,$d9,$b0,$0f,$a5,$aa,$69 ; $f765 (*)
    .byte   $02,$85,$ad,$85,$f6,$a5,$a4     ; $f76d (*)
    
BankSwitch_0to1
    sta     ram_AC                  ;3        
    bit     BANK1STROBE                   ;4        
    
    .byte   $a5,$b2,$69,$03,$10,$02,$a9,$00 ; $f779 (*)
    .byte   $c9,$08,$90,$02,$a9,$06,$69,$02 ; $f781 (*)
    .byte   $a6,$84,$d0,$02,$69,$16,$85,$ad ; $f789 (*)
    .byte   $a9,$07,$d0,$df                 ; $f791 (*)
    
Lf795
    cmp     #$03                    ;2        
    beq     Lf765                   ;2/3      
    cmp     #$02                    ;2        
    bne     Lf7ea                   ;2/3      
    
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
    cmp     #$01                    ;2        
    bne     Lf808                   ;2/3!     
    
    .byte   $20,$a3,$d9,$20,$da,$d9,$a5,$b0 ; $f7ee (*)
    .byte   $b0,$02,$a5,$b1,$4a,$4a,$4a,$aa ; $f7f6 (*)
    .byte   $18,$69,$05,$85,$ad,$bd,$ec,$de ; $f7fe (*)
    .byte   $d0,$bf                         ; $f806 (*)
    
Lf808
    cmp     #$04                    ;2        
    bne     Lf847                   ;2/3      
    
    .byte   $20,$da,$d9,$b0,$14,$a5,$e9,$10 ; $f80c (*)
    .byte   $02,$a9,$00,$4a,$4a,$85,$ad,$a5 ; $f814 (*)
    .byte   $a5,$4a,$d0,$02,$a9,$ff,$38,$b0 ; $f81c (*)
    .byte   $1d,$a5,$b2,$69,$4f,$c9,$a0,$90 ; $f824 (*)
    .byte   $02,$e9,$9f,$4a,$4a,$4a,$18,$69 ; $f82c (*)
    .byte   $13,$85,$ad,$a5,$a5,$4a,$f0,$03 ; $f834 (*)
    .byte   $49,$0f,$2c,$a9,$10,$18,$69,$03 ; $f83c (*)
    
Lf844
    jmp     $d774                   ;3   =  26
    
Lf847
    lda     #$00                    ;2        
    sta     ram_AD                  ;3        
    ldx     ram_9B                  ;3        
    ldy     ram_E3                  ;3        
    lda     #$03                    ;2        
    cpx     #$35                    ;2        
    bcc     Lf85b                   ;2/3      
    cpy     #$02                    ;2        
    bcc     Lf85b                   ;2/3      
    
    .byte   $a9,$09                         ; $f859 (*)
    
Lf85b
    cpx     #$45                    ;2        
    bcc     Lf865                   ;2/3      
    cpy     #$04                    ;2        
    bcc     Lf865                   ;2/3      
    
    .byte   $a9,$0f                         ; $f863 (*)
    
Lf865
    bne     Lf844                   ;2/3      
    
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
    sta     WSYNC                   ;3   =  34
;---------------------------------------
    sta     HMOVE                   ;3        
    sec                             ;2   =   5
Lf95d
    sbc     #$0f                    ;2        
    bcs     Lf95d                   ;2/3      
    eor     #$0f                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    nop                             ;2        
    nop                             ;2        
    sta     RESP0,x                 ;4        
    sta     HMCLR                   ;3        
    sta     WSYNC                   ;3   =  26
;---------------------------------------
    sta     HMOVE                   ;3        
    nop                             ;2        
    nop                             ;2        
    nop                             ;2        
    asl                             ;2        
    adc     #$80                    ;2        
    sta     HMP0,x                  ;4        
    rts                             ;6   =  23
    
Lf979
    ldy     ram_FA                  ;3        
    lda     (ram_CC),y              ;5        
    sta     ram_FC                  ;3        
    sta     WSYNC                   ;3   =  14
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     (ram_D6),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_D4),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_D2),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_D0),y              ;5        
    tax                             ;2        
    lda     (ram_CE),y              ;5        
    ldy     ram_FC                  ;3        
    stx     GRP1                    ;3        
    sta     GRP0                    ;3        
    sty     GRP1                    ;3        
    sty     GRP0                    ;3        
    dec     ram_FA                  ;5        
    bpl     Lf979                   ;2/3      
    rts                             ;6   =  67
    
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

Reset_Bank1
    bit     BANK0STROBE                   ;4        
Start
    cld                             ;2        
    ldx     #$ff                    ;2        
    txs                             ;2        
    inx                             ;2        
    txa                             ;2   =  14
Lf009
    sta     VSYNC,x                 ;4        
    inx                             ;2        
    bne     Lf009                   ;2/3      
    jsr     Lfe6e                   ;6   =  14
Lf011
    ldy     #$29                    ;2        
    sty     TIM64T                  ;4        
    lda     ram_80                  ;3        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    eor     ram_80                  ;3        
    asl                             ;2        
    rol     ram_80                  ;5        
    inc     ram_C1                  ;5        
    lda     ram_C1                  ;3        
    tay                             ;2        
    and     #$3f                    ;2        
    bne     Lf054                   ;2/3      
    lda     ram_B5                  ;3        
    bne     Lf054                   ;2/3      
    lda     ram_AE                  ;3        
    beq     Lf054                   ;2/3      
    inc     ram_AE                  ;5         *
    cmp     #$09                    ;2         *
    bne     Lf054                   ;2/3       *
    sta     ram_AE                  ;3         *
    lda     ram_B6                  ;3         *
    bne     Lf054                   ;2/3       *
    bit     ram_F9                  ;3         *
    bmi     Lf04a                   ;2/3       *
    ldx     #$09                    ;2         *
    lda     ram_C6                  ;3         *
    lsr                             ;2         *
    and     #$64                    ;2         *
    bne     Lf050                   ;2/3 =  82 *
Lf04a
    dec     ram_B6                  ;5         *
    sta     ram_84                  ;3         *
    ldx     #$07                    ;2   =  10 *
Lf050
    sta     ram_9E                  ;3         *
    stx     ram_85                  ;3   =   6 *
Lf054
    lda     ram_F5                  ;3        
    jsr     Lfcae                   ;6        
    tya                             ;2        
    and     #$0f                    ;2        
    bne     Lf076                   ;2/3      
    inc     ram_E6                  ;5        
    lda     ram_B6                  ;3        
    beq     Lf076                   ;2/3      
    clc                             ;2         *
    lda     #$01                    ;2         *
    ldx     ram_B4                  ;3         *
    beq     Lf06d                   ;2/3       *
    adc     #$06                    ;2   =  36 *
Lf06d
    ldx     ram_A1                  ;3         *
    beq     Lf073                   ;2/3       *
    adc     #$02                    ;2   =   7 *
Lf073
    jsr     Lfcae                   ;6   =   6 *
Lf076
    bit     ram_F9                  ;3        
    bpl     Lf084                   ;2/3      
    lda     ram_A5                  ;3        
    cmp     #$d2                    ;2        
    bne     Lf0db                   ;2/3      
    lda     #$00                    ;2         *
    beq     Lf08a                   ;2/3 =  16 *
Lf084
    lda     ram_C6                  ;3        
    and     #$48                    ;2        
    bne     Lf0db                   ;2/3 =   7
Lf08a
    sta     ram_B4                  ;3        
    ldx     ram_B6                  ;3        
    beq     Lf0db                   ;2/3      
    ldx     ram_B5                  ;3         *
    cpx     #$01                    ;2         *
    bne     Lf0db                   ;2/3       *
    stx     ram_AB                  ;3         *
    sta     ram_B1                  ;3         *
    inc     ram_B5                  ;5         *
    sta     ram_CA                  ;3         *
    sta     ram_E6                  ;3         *
    ldx     #$95                    ;2         *
    lda     ram_A6                  ;3         *
    cmp     #$d2                    ;2         *
    bcc     Lf0b8                   ;2/3       *
    lda     #$00                    ;2         *
    bit     ram_F9                  ;3         *
    bmi     Lf0c3                   ;2/3       *
    ldx     #$90                    ;2         *
    lda     #$21                    ;2         *
    sbc     ram_AA                  ;3         *
    cmp     #$08                    ;2         *
    bcc     Lf0bd                   ;2/3 =  59 *
Lf0b8
    jsr     Lfcc7                   ;6         *
    bcs     Lf0db                   ;2/3 =   8 *
Lf0bd
    asl                             ;2         *
    asl                             ;2         *
    asl                             ;2         *
    asl                             ;2         *
    adc     #$40                    ;2   =  10 *
Lf0c3
    sta     ram_B0                  ;3         *
    bit     ram_F7                  ;3         *
    bpl     Lf0cb                   ;2/3       *
    lda     #$10                    ;2   =  10 *
Lf0cb
    bit     ram_F9                  ;3         *
    bmi     Lf0d5                   ;2/3       *
    lda     ram_B2                  ;3         *
    asl                             ;2         *
    asl                             ;2         *
    asl                             ;2         *
    asl                             ;2   =  16 *
Lf0d5
    sta     ram_B2                  ;3         *
    inc     ram_ED                  ;5         *
    inc     ram_84                  ;5   =  13 *
Lf0db
    lda     ram_B4                  ;3        
    beq     Lf0e8                   ;2/3      
    tya                             ;2         *
    and     #$02                    ;2         *
    bne     Lf150                   ;2/3!      *
    lda     ram_BC                  ;3   =  14 *
Lf0e6
    beq     Lf0fd                   ;2/3 =   2 *
Lf0e8
    ldx     ram_83                  ;3        
    lsr                             ;2        
    bcs     Lf0f5                   ;2/3 =   7
Lf0ed
    dec     ram_82                  ;5        
    dec     ram_83                  ;5        
    cpx     #$0d                    ;2        
    bne     Lf0fd                   ;2/3 =  14
Lf0f5
    inc     ram_82                  ;5        
    inc     ram_83                  ;5        
    cpx     #$81                    ;2        
    beq     Lf0ed                   ;2/3 =  14
Lf0fd
    lda     ram_B6                  ;3        
    beq     Lf126                   ;2/3      
    lda     ram_B4                  ;3         *
    beq     Lf126                   ;2/3       *
    bit     ram_F9                  ;3         *
    bpl     Lf10f                   ;2/3       *
    lda     ram_BC                  ;3         *
    beq     Lf14c                   ;2/3       *
    bpl     Lf123                   ;2/3 =  22 *
Lf10f
    bit     INPT4                   ;3         *
    bmi     Lf14c                   ;2/3       *
    ldx     ram_A9                  ;3         *
    bne     Lf121                   ;2/3       *
    inx                             ;2         *
    lda     ram_82                  ;3         *
    cmp     ram_81                  ;3         *
    bcs     Lf11f                   ;2/3       *
    inx                             ;2   =  22 *
Lf11f
    stx     ram_A9                  ;3   =   3 *
Lf121
    lda     ram_A9                  ;3   =   3 *
Lf123
    lsr                             ;2         *
    bcs     Lf129                   ;2/3 =   4 *
Lf126
    dec     ram_81                  ;5        
    .byte   $2c ;bit                ;4-5 =   4
Lf129
    inc     ram_81                  ;5        
    lda     ram_81                  ;3        
    cmp     #$0f                    ;2        
    bcc     Lf129                   ;2/3      
    cmp     #$8c                    ;2        
    beq     Lf126                   ;2/3      
    bit     INPT4                   ;3        
    bmi     Lf150                   ;2/3      
    lda     ram_B4                  ;3        
    bne     Lf150                   ;2/3      
    lda     ram_B6                  ;3        
    beq     Lf150                   ;2/3      
    lda     ram_B5                  ;3         *
    bne     Lf150                   ;2/3       *
    bit     ram_F9                  ;3         *
    bmi     Lf150                   ;2/3       *
    jmp     Lf23c                   ;3   =  44 *
    
Lf14c
    lda     #$00                    ;2         *
    sta     ram_A9                  ;3   =   5 *
Lf150
    tya                             ;2        
    and     #$1f                    ;2        
    bne     Lf16f                   ;2/3      
    ldx     ram_B4                  ;3        
    cpx     #$05                    ;2        
    beq     Lf16f                   ;2/3      
    ldx     #$9f                    ;2        
    dec     ram_86                  ;5        
    lda     ram_86                  ;3        
    bne     Lf165                   ;2/3      
    stx     ram_86                  ;3   =  28 *
Lf165
    and     #$03                    ;2        
    bne     Lf16f                   ;2/3      
    dec     ram_87                  ;5        
    bne     Lf16f                   ;2/3      
    stx     ram_87                  ;3   =  14 *
Lf16f
    lda     ram_ED                  ;3        
    beq     Lf180                   ;2/3      
    inc     ram_ED                  ;5         *
    bne     Lf180                   ;2/3       *
    ldx     #$38                    ;2         *
    jsr     Lfce6                   ;6         *
    lda     #$05                    ;2         *
    sta     ram_EC                  ;3   =  25 *
Lf180
    lda     SWCHB                   ;4        
    sta     ram_C6                  ;3        
    lsr                             ;2        
    bcs     Lf1c1                   ;2/3      
    lda     ram_C7                  ;3        
    ldx     ram_B6                  ;3        
    beq     Lf196                   ;2/3      
    inc     ram_C7                  ;5         *
    cmp     #$7f                    ;2         *
    bne     Lf20c                   ;2/3!      *
    beq     Lf19a                   ;2/3 =  30 *
Lf196
    cmp     #$01                    ;2        
    bne     Lf20c                   ;2/3!=   4
Lf19a
    ldx     #$ff                    ;2        
    ldy     ram_A0                  ;3        
    cpy     #$02                    ;2        
    bcs     Lf1a4                   ;2/3      
    stx     ram_F9                  ;3   =  12
Lf1a4
    cpy     #$03                    ;2        
    bcs     Lf1aa                   ;2/3      
    stx     ram_F8                  ;3   =   7
Lf1aa
    jsr     Lfe65                   ;6        
    sty     ram_A0                  ;3        
    ldy     #$08                    ;2        
    sty     ram_C7                  ;3        
    sty     ram_B7                  ;3        
    ldy     #$00                    ;2        
    sty     ram_BA                  ;3        
    sty     ram_B8                  ;3        
    iny                             ;2        
    sty     ram_AE                  ;3   =  30
Lf1be
    jmp     Lf7f6                   ;3   =   3
    
Lf1c1
    lsr                             ;2        
    bcs     Lf208                   ;2/3!     
    dec     ram_C7                  ;5         *
    bne     Lf20c                   ;2/3!      *
    lda     #$30                    ;2         *
    sta     ram_C7                  ;3         *
    lda     ram_F1                  ;3         *
    beq     Lf1d8                   ;2/3       *
    lda     #$00                    ;2         *
    sta     ram_F1                  ;3         *
    sta     ram_9F                  ;3         *
    beq     Lf1fe                   ;2/3 =  31 *
Lf1d8
    lda     ram_85                  ;3         *
    ldx     ram_B7                  ;3         *
    bne     Lf1f3                   ;2/3       *
    cmp     #$0b                    ;2         *
    beq     Lf1e6                   ;2/3       *
    ldx     #$0b                    ;2         *
    bne     Lf1fc                   ;2/3 =  16 *
Lf1e6
    inc     ram_A0                  ;5         *
    lda     ram_A0                  ;3         *
    cmp     #$04                    ;2         *
    bne     Lf1fe                   ;2/3       *
    inx                             ;2         *
    stx     ram_A0                  ;3         *
    bne     Lf1fe                   ;2/3 =  19 *
Lf1f3
    adc     #$02                    ;2         *
    tax                             ;2         *
    cmp     #$0b                    ;2         *
    bcc     Lf1fc                   ;2/3       *
    ldx     #$01                    ;2   =  10 *
Lf1fc
    stx     ram_85                  ;3   =   3 *
Lf1fe
    lda     ram_CA                  ;3         *
    bne     Lf20c                   ;2/3       *
    lda     #$34                    ;2         *
    sta     ram_CA                  ;3         *
    bne     Lf20c                   ;2/3 =  12 *
Lf208
    lda     #$01                    ;2        
    sta     ram_C7                  ;3   =   5
Lf20c
    lda     ram_B6                  ;3        
    beq     Lf1be                   ;2/3!     
    dec     ram_C5                  ;5         *
    bne     Lf286                   ;2/3       *
    sed                             ;2         *
    lda     #$3b                    ;2         *
    sta     ram_C5                  ;3         *
    lda     ram_9D                  ;3         *
    cmp     #$a0                    ;2         *
    bne     Lf250                   ;2/3       *
    ldx     #$04                    ;2         *
    jsr     Lfce6                   ;6         *
    ldx     #$07                    ;2         *
    lda     ram_9C                  ;3         *
    sbc     #$01                    ;2         *
    sta     ram_9C                  ;3         *
    bne     Lf238                   ;2/3       *
    ldx     ram_81                  ;3         *
    cpx     #$75                    ;2         *
    bcc     Lf23c                   ;2/3       *
    sta     ram_9D                  ;3         *
    ldx     #$03                    ;2   =  58 *
Lf238
    stx     ram_85                  ;3         *
    bne     Lf278                   ;2/3 =   5 *
Lf23c
    sta     ram_BB                  ;3         *
    sta     ram_B4                  ;3         *
    lda     #$b8                    ;2         *
    sta     ram_C5                  ;3         *
    ldx     #$16                    ;2         *
    stx     ram_9C                  ;3         *
    ldx     #$41                    ;2         *
    stx     ram_CA                  ;3         *
    ldx     #$1d                    ;2         *
    bne     Lf238                   ;2/3 =  25 *
Lf250
    lda     ram_9C                  ;3         *
    adc     #$01                    ;2         *
    sta     ram_9C                  ;3         *
    lda     ram_9D                  ;3         *
    adc     #$00                    ;2         *
    sta     ram_9D                  ;3         *
    bne     Lf278                   ;2/3       *
    ldx     #$01                    ;2         *
    lda     ram_9C                  ;3         *
    cmp     #$25                    ;2         *
    bne     Lf270                   ;2/3       *
    lda     #$07                    ;2         *
    sta     ram_EC                  ;3         *
    lda     #$68                    ;2         *
    sta     ram_CA                  ;3         *
    bne     Lf276                   ;2/3 =  39 *
Lf270
    cmp     #$03                    ;2         *
    bne     Lf278                   ;2/3       *
    ldx     #$05                    ;2   =   6 *
Lf276
    stx     ram_B4                  ;3   =   3 *
Lf278
    cld                             ;2         *
    lda     ram_B5                  ;3         *
    cmp     #$02                    ;2         *
    bcs     Lf2a0                   ;2/3       *
    ldx     ram_BB                  ;3         *
    lda     Lfeab,x                 ;4         *
    cmp     ram_9C                  ;3   =  19 *
Lf286
    bne     Lf29a                   ;2/3       *
    inc     ram_BB                  ;5         *
    lda     Lfe8d,x                 ;4         *
    and     #$0f                    ;2         *
    sta     ram_BC                  ;3         *
    cpx     #$00                    ;2         *
    bne     Lf29a                   ;2/3       *
    inx                             ;2         *
    stx     ram_B4                  ;3         *
    inc     ram_81                  ;5   =  30 *
Lf29a
    lda     ram_BB                  ;3         *
    cmp     #$03                    ;2         *
    bcs     Lf2a3                   ;2/3 =   7 *
Lf2a0
    jmp     Lf32c                   ;3   =   3 *
    
Lf2a3
    lda     ram_81                  ;3         *
    cmp     #$10                    ;2         *
    bcs     Lf2ad                   ;2/3       *
    ldx     ram_B4                  ;3         *
    beq     Lf2a0                   ;2/3 =  12 *
Lf2ad
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    clc                             ;2         *
    adc     #$0b                    ;2         *
    eor     #$0f                    ;2         *
    adc     #$14                    ;2         *
    dec     ram_A8                  ;5         *
    bpl     Lf2c2                   ;2/3       *
    sta     ram_A8                  ;3         *
    jsr     Lfed5                   ;6   =  34 *
Lf2c2
    ldx     ram_A6                  ;3         *
    lda     #$05                    ;2         *
    cpx     #$20                    ;2         *
    bcc     Lf2cc                   ;2/3       *
    lda     #$03                    ;2   =  11 *
Lf2cc
    cpx     #$80                    ;2         *
    bcc     Lf2d2                   ;2/3       *
    lda     #$01                    ;2   =   6 *
Lf2d2
    cpx     #$d8                    ;2         *
    bcc     Lf2d8                   ;2/3       *
    lda     #$00                    ;2   =   6 *
Lf2d8
    dec     ram_A7                  ;5         *
    bpl     Lf2f3                   ;2/3       *
    sta     ram_A7                  ;3         *
    lda     ram_86                  ;3         *
    sbc     #$04                    ;2         *
    bcc     Lf2e6                   ;2/3       *
    sta     ram_86                  ;3   =  20 *
Lf2e6
    lda     ram_87                  ;3         *
    adc     #$03                    ;2         *
    cmp     #$a0                    ;2         *
    bcs     Lf2f0                   ;2/3       *
    sta     ram_87                  ;3   =  12 *
Lf2f0
    jsr     Lfd20                   ;6   =   6 *
Lf2f3
    lda     ram_C1                  ;3         *
    and     #$1f                    ;2         *
    bne     Lf328                   ;2/3!      *
    lda     ram_80                  ;3         *
    cmp     #$b0                    ;2         *
    bcc     Lf307                   ;2/3!      *
    lsr                             ;2         *
    bcs     Lf305                   ;2/3       *
    inc     ram_B2                  ;5         *
    .byte   $2c ;bit                ;4-5 =  22 *
Lf305
    dec     ram_B2                  ;5   =   5 *
Lf307
    lda     SWCHA                   ;4         *
    asl                             ;2         *
    bcs     Lf30f                   ;2/3       *
    inc     ram_B2                  ;5   =  13 *
Lf30f
    asl                             ;2         *
    bcs     Lf314                   ;2/3       *
    dec     ram_B2                  ;5   =   9 *
Lf314
    ldy     ram_AA                  ;3         *
    asl                             ;2         *
    bcs     Lf31f                   ;2/3       *
    cpy     #$21                    ;2         *
    beq     Lf31f                   ;2/3       *
    inc     ram_AA                  ;5   =  16 *
Lf31f
    asl                             ;2         *
    bcs     Lf328                   ;2/3       *
    cpy     #$00                    ;2         *
    beq     Lf328                   ;2/3       *
    dec     ram_AA                  ;5   =  13 *
Lf328
    cld                             ;2         *
    jmp     Lf3a7                   ;3   =   5 *
    
Lf32c
    lda     ram_B5                  ;3         *
    cmp     #$03                    ;2         *
    beq     Lf2f3                   ;2/3!      *
    cmp     #$04                    ;2         *
    bne     Lf34d                   ;2/3       *
    ldx     #$17                    ;2         *
    bit     INPT4                   ;3         *
    bmi     Lf33e                   ;2/3       *
    ldx     #$03                    ;2   =  20 *
Lf33e
    stx     ram_85                  ;3         *
    ldy     #$01                    ;2         *
    lda     SWCHA                   ;4         *
    and     #$f0                    ;2         *
    cmp     #$f0                    ;2         *
    beq     Lf3a5                   ;2/3       *
    dec     ram_C0                  ;5   =  20 *
Lf34d
    bne     Lf3a7                   ;2/3       *
    ldx     #$04                    ;2         *
    stx     ram_C0                  ;3         *
    tax                             ;2         *
    txa                             ;2         *
    asl                             ;2         *
    tax                             ;2         *
    bcs     Lf36b                   ;2/3       *
    dec     ram_B2                  ;5         *
    lda     ram_B2                  ;3         *
    cmp     #$ff                    ;2         *
    bne     Lf365                   ;2/3       *
    lda     #$9f                    ;2         *
    sta     ram_B2                  ;3   =  34 *
Lf365
    cmp     #$4f                    ;2         *
    bne     Lf36b                   ;2/3       *
    inc     ram_B2                  ;5   =   9 *
Lf36b
    txa                             ;2         *
    asl                             ;2         *
    tax                             ;2         *
    bcs     Lf382                   ;2/3       *
    inc     ram_B2                  ;5         *
    lda     ram_B2                  ;3         *
    cmp     #$a0                    ;2         *
    bne     Lf37c                   ;2/3       *
    lda     #$00                    ;2         *
    sta     ram_B2                  ;3   =  25 *
Lf37c
    cmp     #$50                    ;2         *
    bne     Lf382                   ;2/3       *
    dec     ram_B2                  ;5   =   9 *
Lf382
    lda     ram_F3                  ;3         *
    bne     Lf3a7                   ;2/3       *
    lda     ram_CA                  ;3         *
    cmp     #$98                    ;2         *
    beq     Lf3a7                   ;2/3       *
    txa                             ;2         *
    asl                             ;2         *
    tax                             ;2         *
    bcs     Lf399                   ;2/3       *
    lda     ram_88                  ;3         *
    cmp     #$10                    ;2         *
    beq     Lf3a7                   ;2/3       *
    inc     ram_88                  ;5   =  32 *
Lf399
    txa                             ;2         *
    asl                             ;2         *
    bcs     Lf3a7                   ;2/3       *
    lda     ram_88                  ;3         *
    beq     Lf3a7                   ;2/3       *
    dec     ram_88                  ;5         *
    bpl     Lf3a7                   ;2/3 =  18 *
Lf3a5
    sty     ram_C0                  ;3   =   3 *
Lf3a7
    lda     ram_B2                  ;3         *
    bne     Lf407                   ;2/3!      *
    lda     ram_88                  ;3         *
    bne     Lf407                   ;2/3!      *
    lda     ram_DE                  ;3         *
    cmp     #$50                    ;2         *
    bcs     Lf407                   ;2/3!      *
    cmp     #$47                    ;2         *
    bcc     Lf407                   ;2/3!      *
    lda     ram_B1                  ;3         *
    sbc     ram_B0                  ;3         *
    bne     Lf407                   ;2/3!      *
    lda     ram_A5                  ;3         *
    cmp     #$d2                    ;2         *
    bne     Lf407                   ;2/3!=  36 *
Lf3c5
    inc     ram_E2                  ;5         *
    bne     Lf3cb                   ;2/3       *
    dec     ram_E2                  ;5   =  12 *
Lf3cb
    lda     ram_E2                  ;3         *
    cmp     #$80                    ;2         *
    bne     Lf3f5                   ;2/3       *
    ldx     #$24                    ;2         *
    stx     ram_CA                  ;3         *
    dec     ram_B0                  ;5         *
    ldx     ram_E3                  ;3         *
    cpx     #$06                    ;2         *
    beq     Lf3df                   ;2/3       *
    inc     ram_E3                  ;5   =  29 *
Lf3df
    sed                             ;2         *
    clc                             ;2         *
    lda     ram_9E                  ;3         *
    adc     #$01                    ;2         *
    sta     ram_9E                  ;3         *
    lda     ram_9B                  ;3         *
    adc     Lfdee,x                 ;4         *
    sta     ram_9B                  ;3         *
    bcc     Lf3f4                   ;2/3       *
    lda     #$99                    ;2         *
    sta     ram_9B                  ;3   =  29 *
Lf3f4
    cld                             ;2   =   2 *
Lf3f5
    bcc     Lf416                   ;2/3!      *
    ldx     #$05                    ;2         *
    cmp     #$fe                    ;2         *
    beq     Lf403                   ;2/3!      *
    cmp     #$ff                    ;2         *
    beq     Lf416                   ;2/3       *
    ldx     #$1f                    ;2   =  14 *
Lf403
    stx     ram_85                  ;3         *
    bne     Lf45c                   ;2/3 =   5 *
Lf407
    lda     ram_E2                  ;3         *
    cmp     #$80                    ;2         *
    bcc     Lf412                   ;2/3       *
    cmp     #$ff                    ;2         *
    bne     Lf3c5                   ;2/3!      *
    .byte   $2c ;bit                ;4-2 =  13 *
Lf412
    lda     #$00                    ;2         *
    sta     ram_E2                  ;3   =   5 *
Lf416
    lda     ram_B5                  ;3         *
    cmp     #$04                    ;2         *
    bne     Lf44b                   ;2/3       *
    lda     ram_A5                  ;3         *
    cmp     #$0b                    ;2         *
    bcc     Lf44b                   ;2/3       *
    ldx     ram_F9                  ;3         *
    bmi     Lf44b                   ;2/3       *
    lda     SWCHA                   ;4         *
    cmp     #$cf                    ;2         *
    bcc     Lf44b                   ;2/3       *
    ldx     ram_E3                  ;3         *
    cpx     #$06                    ;2         *
    beq     Lf435                   ;2/3       *
    ldx     ram_A0                  ;3   =  37 *
Lf435
    lda     ram_C1                  ;3         *
    and     Lfe39,x                 ;4         *
    bne     Lf44b                   ;2/3       *
    lda     ram_B2                  ;3         *
    cmp     #$51                    ;2         *
    bcs     Lf449                   ;2/3       *
    cmp     #$4f                    ;2         *
    bcs     Lf44b                   ;2/3       *
    inc     ram_B2                  ;5         *
    .byte   $2c ;bit                ;4-5 =  24 *
Lf449
    dec     ram_B2                  ;5   =   5 *
Lf44b
    ldx     #$75                    ;2         *
    lda     ram_A5                  ;3         *
    cmp     #$ff                    ;2         *
    beq     Lf46d                   ;2/3       *
    ldx     ram_B5                  ;3         *
    cpx     #$02                    ;2         *
    beq     Lf45f                   ;2/3       *
    jmp     Lf5c7                   ;3   =  19 *
    
Lf45c
    jmp     Lf538                   ;3   =   3 *
    
Lf45f
    ldx     #$70                    ;2         *
    cmp     #$c3                    ;2         *
    bcc     Lf46d                   ;2/3       *
    ldx     #$80                    ;2         *
    lda     ram_A6                  ;3         *
    cmp     #$a9                    ;2         *
    bne     Lf470                   ;2/3 =  15 *
Lf46d
    jsr     Lfcc7                   ;6   =   6 *
Lf470
    lda     SWCHA                   ;4         *
    and     #$f0                    ;2         *
    cmp     #$f0                    ;2         *
    beq     Lf45c                   ;2/3       *
    dec     ram_C0                  ;5         *
    bne     Lf4f2                   ;2/3       *
    ldx     #$18                    ;2         *
    stx     ram_C0                  ;3         *
    stx     ram_AF                  ;3         *
    jsr     Lfce6                   ;6         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    eor     #$0f                    ;2         *
    tax                             ;2         *
    lda     #$09                    ;2         *
    ldy     ram_85                  ;3         *
    cpy     #$15                    ;2         *
    bne     Lf497                   ;2/3       *
    lda     #$01                    ;2   =  54 *
Lf497
    jsr     Lfcae                   ;6         *
    lda     ram_B6                  ;3         *
    beq     Lf45c                   ;2/3       *
    bit     ram_F9                  ;3         *
    bmi     Lf4a8                   ;2/3       *
    lda     ram_C6                  ;3         *
    and     #$48                    ;2         *
    bne     Lf4f5                   ;2/3 =  23 *
Lf4a8
    txa                             ;2         *
    lsr                             ;2         *
    tay                             ;2         *
    bcc     Lf4bc                   ;2/3       *
    bit     INPT4                   ;3         *
    bmi     Lf4b7                   ;2/3       *
    jsr     Lfd42                   ;6         *
    jmp     Lf4cc                   ;3   =  22 *
    
Lf4b7
    ldx     #$0d                    ;2         *
    jsr     Lfed5                   ;6   =   8 *
Lf4bc
    tya                             ;2         *
    lsr                             ;2         *
    tay                             ;2         *
    bcc     Lf4d5                   ;2/3       *
    lda     #$00                    ;2         *
    sta     ram_AF                  ;3         *
    bit     INPT4                   ;3         *
    bmi     Lf4d0                   ;2/3       *
    jsr     Lfd20                   ;6   =  24 *
Lf4cc
    ldx     #$11                    ;2         *
    bne     Lf4f0                   ;2/3 =   4 *
Lf4d0
    ldx     #$0d                    ;2         *
    jsr     Lfeea                   ;6   =   8 *
Lf4d5
    bit     INPT4                   ;3         *
    bpl     Lf4f2                   ;2/3       *
    tya                             ;2         *
    lsr                             ;2         *
    tay                             ;2         *
    bcc     Lf4e5                   ;2/3       *
    lda     #$04                    ;2         *
    inc     ram_B2                  ;5         *
    jsr     Lfd0f                   ;6   =  26 *
Lf4e5
    tya                             ;2         *
    lsr                             ;2         *
    bcc     Lf4f0                   ;2/3       *
    dec     ram_B2                  ;5         *
    lda     #$08                    ;2         *
    jsr     Lfd0f                   ;6   =  19 *
Lf4f0
    stx     ram_85                  ;3   =   3 *
Lf4f2
    jmp     Lf5c7                   ;3   =   3 *
    
Lf4f5
    txa                             ;2         *
    lsr                             ;2         *
    tay                             ;2         *
    bcc     Lf508                   ;2/3!      *
    lda     ram_88                  ;3         *
    dec     ram_88                  ;5         *
    bpl     Lf502                   ;2/3       *
    sta     ram_88                  ;3   =  21 *
Lf502
    ldx     #$01                    ;2   =   2 *
Lf504
    lda     #$13                    ;2         *
    bne     Lf532                   ;2/3 =   4 *
Lf508
    tya                             ;2         *
    lsr                             ;2         *
    tay                             ;2         *
    bcc     Lf519                   ;2/3       *
    lda     ram_88                  ;3         *
    cmp     #$10                    ;2         *
    beq     Lf515                   ;2/3       *
    inc     ram_88                  ;5   =  20 *
Lf515
    ldx     #$02                    ;2         *
    bne     Lf504                   ;2/3 =   4 *
Lf519
    tya                             ;2         *
    lsr                             ;2         *
    tay                             ;2         *
    bcc     Lf524                   ;2/3       *
    dec     ram_B3                  ;5         *
    ldx     #$04                    ;2         *
    bne     Lf52c                   ;2/3 =  17 *
Lf524
    tya                             ;2         *
    lsr                             ;2         *
    bcc     Lf538                   ;2/3       *
    inc     ram_B3                  ;5         *
    ldx     #$08                    ;2   =  13 *
Lf52c
    lda     #$05                    ;2         *
    sta     ram_C0                  ;3         *
    lda     #$15                    ;2   =   7 *
Lf532
    sta     ram_85                  ;3         *
    stx     ram_AB                  ;3         *
    bne     Lf577                   ;2/3 =   8 *
Lf538
    lda     ram_B3                  ;3         *
    clc                             ;2         *
    adc     #$10                    ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    tax                             ;2         *
    lda     ram_CA                  ;3         *
    cmp     #$58                    ;2         *
    beq     Lf55f                   ;2/3       *
    lda     Lfd84,x                 ;4         *
    sta     ram_AB                  ;3         *
    ldy     #$00                    ;2         *
    sty     ram_E0                  ;3         *
    sty     ram_AF                  ;3         *
    lda     ram_CA                  ;3         *
    cmp     #$18                    ;2         *
    bne     Lf55c                   ;2/3       *
    sty     ram_CA                  ;3   =  51 *
Lf55c
    iny                             ;2         *
    sty     ram_C0                  ;3   =   5 *
Lf55f
    lda     ram_C6                  ;3         *
    and     #$48                    ;2         *
    beq     Lf5c7                   ;2/3       *
    bit     INPT4                   ;3         *
    bmi     Lf5c7                   ;2/3       *
    lda     #$fe                    ;2         *
    sta     ram_C9                  ;3         *
    lda     #$58                    ;2         *
    sta     ram_CA                  ;3         *
    sta     ram_AF                  ;3         *
    lda     ram_C1                  ;3         *
    and     #$0f                    ;2   =  30 *
Lf577
    bne     Lf5c7                   ;2/3       *
    lda     #$01                    ;2         *
    ldy     ram_88                  ;3         *
    cpy     #$07                    ;2         *
    beq     Lf583                   ;2/3       *
    lda     #$02                    ;2   =  13 *
Lf583
    jsr     Lfcae                   ;6         *
    cpx     #$02                    ;2         *
    bcs     Lf593                   ;2/3 =  10 *
Lf58a
    jsr     Lfed5                   ;6         *
    cpy     #$08                    ;2         *
    bcs     Lf5c7                   ;2/3       *
    bcc     Lf5c4                   ;2/3 =  12 *
Lf593
    cpx     #$02                    ;2         *
    bne     Lf59d                   ;2/3       *
    dec     ram_B2                  ;5         *
    lda     #$05                    ;2         *
    bne     Lf5b2                   ;2/3 =  13 *
Lf59d
    cpx     #$06                    ;2         *
    bcs     Lf5aa                   ;2/3       *
    jsr     Lfeea                   ;6         *
    cpy     #$07                    ;2         *
    bcs     Lf5be                   ;2/3       *
    bcc     Lf5c7                   ;2/3 =  16 *
Lf5aa
    cpx     #$07                    ;2         *
    beq     Lf58a                   ;2/3       *
    inc     ram_B2                  ;5         *
    lda     #$09                    ;2   =  11 *
Lf5b2
    sta     ram_AB                  ;3         *
    ldx     #$0f                    ;2         *
    stx     ram_85                  ;3         *
    cpy     #$07                    ;2         *
    beq     Lf5c7                   ;2/3       *
    bcc     Lf5c4                   ;2/3 =  14 *
Lf5be
    jsr     Lfd20                   ;6         *
    jmp     Lf5c7                   ;3   =   9 *
    
Lf5c4
    jsr     Lfd42                   ;6   =   6 *
Lf5c7
    lda     ram_B5                  ;3         *
    cmp     #$02                    ;2         *
    bcs     Lf5d0                   ;2/3 =   7 *
Lf5cd
    jmp     Lf76f                   ;3   =   3 *
    
Lf5d0
    bit     ram_F9                  ;3         *
    bpl     Lf5d8                   ;2/3       *
    lda     ram_E3                  ;3         *
    bne     Lf5de                   ;2/3 =  10 *
Lf5d8
    lda     ram_A6                  ;3         *
    cmp     #$bf                    ;2         *
    bcs     Lf5cd                   ;2/3 =   7 *
Lf5de
    lda     ram_A5                  ;3         *
    cmp     #$d7                    ;2         *
    bcs     Lf5cd                   ;2/3       *
    sta     ram_AF                  ;3         *
    cmp     #$c8                    ;2         *
    bne     Lf615                   ;2/3!      *
    dec     ram_A5                  ;5         *
    ldy     #$00                    ;2         *
    sty     ram_B2                  ;3         *
    sty     ram_AB                  ;3         *
    ldx     #$03                    ;2         *
    stx     ram_B5                  ;3         *
    stx     ram_85                  ;3         *
    stx     ram_84                  ;3         *
    ldx     #$09                    ;2         *
    stx     ram_AA                  ;3         *
    stx     ram_F6                  ;3         *
    ldx     #$65                    ;2         *
    lda     ram_B3                  ;3         *
    bne     Lf612                   ;2/3       *
    ldx     #$55                    ;2         *
    lda     ram_88                  ;3         *
    cmp     #$0d                    ;2         *
    beq     Lf615                   ;2/3       *
    bcs     Lf612                   ;2/3       *
    ldx     #$60                    ;2   =  66 *
Lf612
    jsr     Lfcc7                   ;6   =   6 *
Lf615
    ldx     ram_E8                  ;3         *
    beq     Lf61c                   ;2/3 =   5 *
Lf619
    jmp     Lf6ab                   ;3   =   3 *
    
Lf61c
    lda     ram_AB                  ;3         *
    and     #$0c                    ;2         *
    ora     #$01                    ;2         *
    sta     ram_AB                  ;3         *
    lda     ram_84                  ;3         *
    cmp     #$04                    ;2         *
    bne     Lf635                   ;2/3       *
    lda     ram_88                  ;3         *
    adc     #$03                    ;2         *
    ldx     ram_C6                  ;3         *
    bpl     Lf63c                   ;2/3       *
    lsr                             ;2         *
    bpl     Lf63c                   ;2/3 =  31 *
Lf635
    ldx     ram_A4                  ;3         *
    lda     Lfda8,x                 ;4         *
    sta     ram_C4                  ;3   =  10 *
Lf63c
    inc     ram_C2                  ;5         *
    cmp     ram_C2                  ;3         *
    bcs     Lf619                   ;2/3       *
    lda     #$00                    ;2         *
    sta     ram_C2                  ;3         *
    jsr     Lfd42                   ;6         *
    lda     ram_A5                  ;3         *
    cmp     #$a7                    ;2         *
    bcs     Lf690                   ;2/3       *
    cmp     #$78                    ;2         *
    bcc     Lf65b                   ;2/3       *
    inc     ram_E1                  ;5         *
    bne     Lf690                   ;2/3       *
    dec     ram_E1                  ;5         *
    bne     Lf690                   ;2/3 =  46 *
Lf65b
    cmp     #$1e                    ;2         *
    bne     Lf68a                   ;2/3       *
    inc     ram_B5                  ;5         *
    inc     ram_84                  ;5         *
    ldx     #$02                    ;2         *
    stx     ram_F2                  ;3         *
    lda     #$40                    ;2         *
    sta     ram_E9                  ;3         *
    dec     ram_A5                  ;5         *
    lda     #$00                    ;2         *
    sta     ram_E7                  ;3         *
    bit     ram_F9                  ;3         *
    bpl     Lf67e                   ;2/3       *
    stx     ram_88                  ;3         *
    bit     ram_F7                  ;3         *
    bpl     Lf680                   ;2/3       *
    lda     #$27                    ;2         *
    .byte   $2c ;bit                ;4-2 =  51 *
Lf67e
    lda     #$4f                    ;2   =   2 *
Lf680
    sta     ram_B2                  ;3         *
    ldx     #$35                    ;2         *
    lda     ram_CA                  ;3         *
    cmp     #$41                    ;2         *
    beq     Lf612                   ;2/3 =  12 *
Lf68a
    lda     ram_E1                  ;3         *
    beq     Lf690                   ;2/3       *
    dec     ram_E1                  ;5   =  10 *
Lf690
    lda     ram_A5                  ;3         *
    cmp     #$30                    ;2         *
    bcs     Lf6a0                   ;2/3       *
    cmp     #$1e                    ;2         *
    bcc     Lf6a0                   ;2/3       *
    inc     ram_E7                  ;5         *
    bne     Lf6a0                   ;2/3       *
    dec     ram_E7                  ;5   =  23 *
Lf6a0
    dec     ram_C3                  ;5         *
    bpl     Lf6ab                   ;2/3       *
    lda     #$0a                    ;2         *
    sta     ram_C3                  ;3         *
    jsr     Lfeea                   ;6   =  18 *
Lf6ab
    lda     ram_84                  ;3         *
    cmp     #$04                    ;2         *
    bne     Lf6c9                   ;2/3       *
    dec     ram_EB                  ;5         *
    bpl     Lf6c9                   ;2/3       *
    lda     #$2a                    ;2         *
    sta     ram_EB                  ;3         *
    dec     ram_E9                  ;5         *
    bpl     Lf6c9                   ;2/3       *
    bit     ram_F0                  ;3         *
    bmi     Lf6c9                   ;2/3       *
    lda     ram_CA                  ;3         *
    bne     Lf6c9                   ;2/3       *
    lda     #$44                    ;2         *
    sta     ram_CA                  ;3   =  41 *
Lf6c9
    lda     ram_A5                  ;3         *
    cmp     #$78                    ;2         *
    bcs     Lf721                   ;2/3!      *
    ldx     #$1e                    ;2         *
    stx     AUDF1                   ;3         *
    ldx     #$08                    ;2         *
    stx     AUDC1                   ;3         *
    ldx     #$01                    ;2         *
    bit     ram_F0                  ;3         *
    bpl     Lf6df                   ;2/3       *
    ldx     #$03                    ;2   =  26 *
Lf6df
    stx     AUDV1                   ;3         *
    lda     ram_98                  ;3         *
    bne     Lf736                   ;2/3!      *
    ldy     ram_99                  ;3         *
    bne     Lf736                   ;2/3!      *
    lda     ram_E8                  ;3         *
    bne     Lf712                   ;2/3!      *
    dec     ram_E8                  ;5         *
    ldx     #$98                    ;2         *
    jsr     Lfce6                   ;6         *
    ldx     #$15                    ;2         *
    lda     ram_E9                  ;3         *
    bpl     Lf71e                   ;2/3!      *
    ldx     #$20                    ;2         *
    cmp     #$eb                    ;2         *
    bcc     Lf71e                   ;2/3       *
    ldx     #$40                    ;2         *
    lda     ram_F0                  ;3         *
    beq     Lf71e                   ;2/3       *
    lda     ram_88                  ;3         *
    adc     #$05                    ;2         *
    sta     ram_88                  ;3         *
    and     #$10                    ;2         *
    beq     Lf712                   ;2/3       *
    sta     ram_88                  ;3   =  66 *
Lf712
    lda     ram_B2                  ;3         *
    cmp     #$18                    ;2         *
    bcc     Lf724                   ;2/3       *
    cmp     #$97                    ;2         *
    bcs     Lf724                   ;2/3       *
    ldx     #$10                    ;2   =  13 *
Lf71e
    jsr     Lfcc7                   ;6   =   6 *
Lf721
    jmp     Lf76f                   ;3   =   3 *
    
Lf724
    lda     ram_F3                  ;3         *
    bne     Lf732                   ;2/3       *
    lda     ram_88                  ;3         *
    bne     Lf732                   ;2/3       *
    inc     ram_F3                  ;5         *
    lda     #$a2                    ;2         *
    sta     ram_CA                  ;3   =  20 *
Lf732
    lda     ram_E9                  ;3         *
    cmp     #$90                    ;2   =   5 *
Lf736
    bne     Lf76f                   ;2/3       *
    ldx     #$30                    ;2         *
    lda     ram_88                  ;3         *
    beq     Lf743                   ;2/3       *
    jsr     Lfcc7                   ;6         *
    bcs     Lf76f                   ;2/3 =  17 *
Lf743
    sty     ram_96                  ;3         *
    sty     ram_B6                  ;3         *
    sty     ram_BA                  ;3         *
    sty     ram_F9                  ;3         *
    ldx     #$21                    ;2         *
    bit     ram_F8                  ;3         *
    bpl     Lf753                   ;2/3       *
    ldx     #$19                    ;2   =  21 *
Lf753
    stx     ram_85                  ;3         *
    ldx     #$05                    ;2         *
    bit     ram_F8                  ;3         *
    bmi     Lf76b                   ;2/3       *
    ldy     ram_E3                  ;3         *
    beq     Lf76b                   ;2/3       *
    inx                             ;2         *
    cpy     #$06                    ;2         *
    bne     Lf76b                   ;2/3       *
    lda     ram_9B                  ;3         *
    cmp     #$75                    ;2         *
    bcc     Lf76b                   ;2/3       *
    inx                             ;2   =  30 *
Lf76b
    stx     ram_84                  ;3         *
    sty     ram_F8                  ;3   =   6 *
Lf76f
    ldy     ram_F4                  ;3         *
    beq     Lf789                   ;2/3       *
    cpy     #$01                    ;2         *
    bne     Lf77c                   ;2/3       *
    beq     Lf786                   ;2/3       *
Bank1_Entry_From_Bank0
    jmp     Bank1_Handler                   ;3   =  14
    
Lf77c
    cpy     #$02                    ;2         *
    bne     Lf789                   ;2/3       *
    jsr     Lfd20                   ;6         *
    jmp     Lf789                   ;3   =  13 *
    
Lf786
    jsr     Lfd42                   ;6   =   6 *
Lf789
    ldx     ram_85                  ;3         *
    cpx     #$0d                    ;2         *
    bne     Lf799                   ;2/3       *
    clc                             ;2         *
    lda     ram_B1                  ;3         *
    sbc     ram_B0                  ;3         *
    eor     #$ff                    ;2         *
    jmp     Lf7c0                   ;3   =  20 *
    
Lf799
    lda     ram_B2                  ;3         *
    cpx     #$0f                    ;2         *
    beq     Lf7be                   ;2/3       *
    lda     ram_E9                  ;3         *
    cpx     #$17                    ;2         *
    beq     Lf7be                   ;2/3       *
    lda     #$d2                    ;2         *
    sec                             ;2         *
    sbc     ram_A5                  ;3         *
    cpx     #$11                    ;2         *
    beq     Lf7be                   ;2/3       *
    lda     ram_88                  ;3         *
    asl                             ;2         *
    asl                             ;2         *
    sbc     #$1b                    ;2         *
    cpx     #$13                    ;2         *
    beq     Lf7be                   ;2/3       *
    cpx     #$15                    ;2         *
    bne     Lf7f5                   ;2/3       *
    lda     ram_B3                  ;3   =  45 *
Lf7be
    cmp     #$80                    ;2   =   2 *
Lf7c0
    ldx     #$00                    ;2         *
    bcc     Lf7ca                   ;2/3       *
    ldx     #$a0                    ;2         *
    eor     #$ff                    ;2         *
    adc     #$00                    ;2   =  10 *
Lf7ca
    stx     ram_A3                  ;3         *
    tay                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    lsr                             ;2         *
    tax                             ;2         *
    lda     Lfd9a,x                 ;4         *
    sta     ram_A2                  ;3         *
    cpx     #$0d                    ;2         *
    bcc     Lf7dd                   ;2/3       *
    inc     ram_A3                  ;5   =  31 *
Lf7dd
    cpx     #$07                    ;2         *
    bcc     Lf7e3                   ;2/3       *
    inc     ram_A3                  ;5   =   9 *
Lf7e3
    tya                             ;2         *
    and     #$0f                    ;2         *
    cmp     #$0a                    ;2         *
    bcc     Lf7ec                   ;2/3       *
    adc     #$05                    ;2   =  10 *
Lf7ec
    sed                             ;2         *
    adc     ram_A2                  ;3         *
    sta     ram_A2                  ;3         *
    bcc     Lf7f5                   ;2/3       *
    inc     ram_A3                  ;5   =  15 *
Lf7f5
    cld                             ;2   =   2 *
Lf7f6
    ldx     #$06                    ;2        
    ldy     ram_85                  ;3        
    lda     ram_F1                  ;3        
    beq     Lf800                   ;2/3!     
    ldy     #$09                    ;2   =  12 *
Lf800
    sty     ram_FA                  ;3        
    cpy     #$0d                    ;2        
    bcc     Lf808                   ;2/3      
    ldy     #$0d                    ;2   =   9
Lf808
    lda.wy  ram_96,y                ;4        
    and     #$f0                    ;2        
    lsr                             ;2        
    jsr     Lfddc                   ;6        
    dex                             ;2        
    lda.wy  ram_96,y                ;4        
    and     #$0f                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    jsr     Lfddc                   ;6        
    dey                             ;2        
    dex                             ;2        
    bpl     Lf808                   ;2/3      
    ldx     #$06                    ;2        
    ldy     #$54                    ;2   =  44
Lf825
    lda     ram_CC,x                ;4        
    bne     Lf82f                   ;2/3      
    sty     ram_CC,x                ;4        
    dex                             ;2        
    dex                             ;2        
    bne     Lf825                   ;2/3 =  16
Lf82f
    lda     ram_FA                  ;3        
    lsr                             ;2        
    tax                             ;2        
    lda     Lfe16,x                 ;4        
    cpx     #$0c                    ;2        
    bcs     Lf848                   ;2/3      
    sta     ram_D6                  ;3         *
    adc     #$06                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$de                    ;2         *
    sta     ram_D7                  ;3         *
    sta     ram_D5                  ;3         *
    bne     Lf857                   ;2/3 =  33 *
Lf848
    ldx     #$0a                    ;2        
    ldy     #$df                    ;2        
    clc                             ;2   =   6
Lf84d
    sta     ram_CC,x                ;4        
    sty     ram_CD,x                ;4        
    adc     #$07                    ;2        
    dex                             ;2        
    dex                             ;2        
    bpl     Lf84d                   ;2/3 =  16
Lf857
    ldx     INTIM                   ;4        
    bne     Lf857                   ;2/3      
    stx     WSYNC                   ;3   =   9
;---------------------------------------
    ldy     ram_B4                  ;3        
    beq     Lf870                   ;2/3      
    lda     ram_C1                  ;3         *
    and     #$02                    ;2         *
    beq     Lf870                   ;2/3       *
    cpy     #$05                    ;2         *
    bne     Lf86e                   ;2/3       *
    stx     WSYNC                   ;3   =  19 *
;---------------------------------------
Lf86e
    stx     WSYNC                   ;3   =   3 *
;---------------------------------------
Lf870
    stx     WSYNC                   ;3   =   3
;---------------------------------------
    sta     ram_FA                  ;3        
    lda     #$0f                    ;2        
    and     ram_B8                  ;3        
    lsr                             ;2        
    sta     VBLANK                  ;3        
    dex                             ;2        
    stx     PF2                     ;3        
    inx                             ;2        
    stx     VDELP0                  ;3        
    stx     VDELP1                  ;3        
    lda     #$80                    ;2        
    sta     HMBL,x                  ;4        
    lda     #$15                    ;2        
    sta     CTRLPF                  ;3        
    lda     ram_98                  ;3        
    cmp     #$09                    ;2        
    sta     RESBL                   ;3        
    bcs     Lf8c2                   ;2/3      
    ldy     ram_B7                  ;3        
    beq     Lf8c8                   ;2/3      
    ldy     ram_B5                  ;3        
    bne     Lf8c2                   ;2/3      
    eor     #$0f                    ;2        
    sbc     #$05                    ;2        
    rol                             ;2        
    sta     ram_FA                  ;3        
    sta     ram_FC                  ;3        
    lda     #$14                    ;2        
    sbc     ram_FA                  ;3        
    asl                             ;2        
    adc     #$0b                    ;2        
    sta     ram_FB                  ;3        
    lda     ram_B4                  ;3        
    beq     Lf8bb                   ;2/3      
    lda     ram_C1                  ;3         *
    and     #$02                    ;2         *
    beq     Lf8bb                   ;2/3       *
    inc     ram_FC                  ;5         *
    dec     ram_FA                  ;5   = 103 *
Lf8bb
    lda     ram_AE                  ;3        
    adc     #$06                    ;2        
    tay                             ;2        
    bcc     Lf8fb                   ;2/3 =   9
Lf8c2
    ldy     ram_B5                  ;3         *
    bne     Lf8c8                   ;2/3       *
    inc     ram_B5                  ;5   =  10 *
Lf8c8
    lda     #$01                    ;2        
    sta     ram_FA                  ;3        
    lda     ram_A4                  ;3        
    lsr                             ;2        
    eor     #$0f                    ;2        
    tay                             ;2        
    lda     ram_80                  ;3        
    bit     ram_EC                  ;3        
    beq     Lf8de                   ;2/3      
    dec     ram_EC                  ;5         *
    lda     #$1e                    ;2         *
    bne     Lf8e8                   ;2/3 =  31 *
Lf8de
    cmp     ram_E1                  ;3        
    bcs     Lf8ed                   ;2/3      
    and     #$07                    ;2         *
    tay                             ;2         *
    lda     Lfebf,y                 ;4   =  13 *
Lf8e8
    sta     COLUP1                  ;3         *
    jmp     Lf903                   ;3   =   6 *
    
Lf8ed
    cmp     ram_E7                  ;3        
    bcs     Lf8f5                   ;2/3      
    lda     #$08                    ;2         *
    bne     Lf8e8                   ;2/3 =   9 *
Lf8f5
    lda     ram_B7                  ;3        
    bne     Lf8fb                   ;2/3      
    ldy     #$05                    ;2   =   7
Lf8fb
    lda     Lfd8a,y                 ;4        
    sta     COLUP1                  ;3        
    lda     Lfec5,y                 ;4   =  11
Lf903
    sta     COLUBK                  ;3        
    sta     ram_FD                  ;3   =   6
Lf907
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     Lfe27,x                 ;4        
    sta     PF0                     ;3        
    lda     Lff01,x                 ;4        
    sta     PF1                     ;3        
    lda     Lfe31,x                 ;4        
    sta     PF2                     ;3        
    sta     HMCLR                   ;3        
    inx                             ;2        
    cpx     #$0a                    ;2        
    bcc     Lf907                   ;2/3      
    ldx     ram_FA                  ;3   =  36
Lf923
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     #$30                    ;2        
    sta     PF0                     ;3        
    lda     #$00                    ;2        
    sta     PF1                     ;3        
    sta     PF2                     ;3        
    dex                             ;2        
    bne     Lf923                   ;2/3      
    lda     ram_B7                  ;3        
    beq     Lf991                   ;2/3      
    lda     ram_B5                  ;3        
    bne     Lf991                   ;2/3      
    sta     REFP0                   ;3        
    lda     #$07                    ;2        
    sta     NUSIZ0                  ;3        
    lda     ram_86                  ;3        
    jsr     Lfe00                   ;6        
    ldx     #$0a                    ;2   =  46
Lf947
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     Lfe78,x                 ;4        
    sta     GRP0                    ;3        
    lda     Lfea0,x                 ;4        
    adc     ram_AE                  ;3        
    sta     COLUP0                  ;3        
    lda     Lfe8c,x                 ;4        
    sta     HMP0                    ;3        
    dex                             ;2        
    bne     Lf947                   ;2/3      
    stx     WSYNC                   ;3   =  34
;---------------------------------------
    stx     GRP0                    ;3        
    ldx     ram_FB                  ;3   =   6
Lf965
    stx     WSYNC                   ;3   =   3
;---------------------------------------
    dex                             ;2        
    bne     Lf965                   ;2/3      
    lda     ram_87                  ;3        
    jsr     Lfe00                   ;6        
    ldx     #$0a                    ;2   =  15
Lf971
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     Lfe82,x                 ;4        
    sta     GRP0                    ;3        
    lda     Lfea0,x                 ;4        
    adc     ram_AE                  ;3        
    sta     COLUP0                  ;3        
    lda     Lfe96,x                 ;4        
    sta     HMP0                    ;3        
    dex                             ;2        
    bne     Lf971                   ;2/3      
    stx     WSYNC                   ;3   =  34
;---------------------------------------
    stx     GRP0                    ;3        
    ldx     ram_FC                  ;3        
    bne     Lf9fe                   ;2/3 =   8
Lf991
    cmp     #$04                    ;2        
    beq     Lfa01                   ;2/3!     
    txs                             ;2        
    lda     #$04                    ;2        
    sta     ram_FC                  ;3        
    ldy     ram_89                  ;3        
    lda     #$3c                    ;2        
    sta     ram_FB                  ;3   =  19
Lf9a0
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     (ram_DC),y              ;5        
    sta     GRP0                    ;3        
    lda     Lfeb9,y                 ;4        
    sta     ENAM1                   ;3        
    dec     ram_FB                  ;5        
    bmi     Lf9f1                   ;2/3      
    dey                             ;2        
    sta     HMCLR                   ;3        
    bmi     Lf9e5                   ;2/3      
    cpy     #$02                    ;2        
    beq     Lf9c2                   ;2/3      
    cpy     #$08                    ;2        
    beq     Lf9c2                   ;2/3      
    cpy     #$0e                    ;2        
    bne     Lf9a0                   ;2/3 =  44
Lf9c2
    tsx                             ;2        
    inx                             ;2        
    txs                             ;2        
    lda     ram_89,x                ;4        
    sta     ram_FA                  ;3        
    lda     (ram_DC),y              ;5        
    dey                             ;2        
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    sta     GRP0                    ;3        
    lda     ram_FA                  ;3   =   6
Lf9d2
    sbc     #$0f                    ;2        
    bcs     Lf9d2                   ;2/3      
    eor     #$0f                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    adc     #$80                    ;2        
    sta     HMM1                    ;3        
    sta     RESM1                   ;3        
    jmp     Lf9a0                   ;3   =  25
    
Lf9e5
    dec     ram_FC                  ;5        
    ldx     ram_FC                  ;3        
    lda     ram_D8,x                ;4        
    sta     ram_DC                  ;3        
    ldy     #$11                    ;2        
    bne     Lf9a0                   ;2/3 =  19
Lf9f1
    ldx     #$ff                    ;2        
    txs                             ;2        
    inx                             ;2        
    ldy     ram_89                  ;3        
    cpy     #$14                    ;2        
    bne     Lf9fd                   ;2/3      
    sta     WSYNC                   ;3   =  16
;---------------------------------------
Lf9fd
    inx                             ;2   =   2
Lf9fe
    jmp     Lfba7                   ;3   =   3
    
Lfa01
    ldx     ram_A5                  ;3   =   3 *
Lfa03
    sta     WSYNC                   ;3   =   3 *
;---------------------------------------
    dex                             ;2         *
    bpl     Lfa03                   ;2/3       *
    lda     ram_88                  ;3         *
    tax                             ;2         *
    eor     #$1f                    ;2         *
    sec                             ;2         *
    sbc     #$0f                    ;2         *
    sta     ram_FC                  ;3   =  18 *
Lfa12
    sta     WSYNC                   ;3   =   3 *
;---------------------------------------
    dex                             ;2         *
    bpl     Lfa12                   ;2/3       *
    inx                             ;2         *
    stx     REFP0                   ;3         *
    lda     #$05                    ;2         *
    sta     NUSIZ0                  ;3         *
    lda     ram_86                  ;3         *
    jsr     Lfdf5                   ;6         *
    ldx     #$07                    ;2   =  25 *
Lfa25
    sta     WSYNC                   ;3   =   3 *
;---------------------------------------
    sta     HMOVE                   ;3         *
    lda     Lfdc5,x                 ;4         *
    jsr     Lfb9c                   ;6         *
    bne     Lfa25                   ;2/3       *
    lda     ram_87                  ;3         *
    jsr     Lfdf5                   ;6         *
    ldx     #$07                    ;2   =  26 *
Lfa38
    sta     WSYNC                   ;3   =   3 *
;---------------------------------------
    sta     HMOVE                   ;3         *
    lda     Lfdcc,x                 ;4         *
    jsr     Lfb9c                   ;6         *
    bne     Lfa38                   ;2/3       *
    lda     ram_B2                  ;3         *
    clc                             ;2         *
    adc     #$6d                    ;2         *
    bcs     Lfa4f                   ;2/3       *
    cmp     #$a0                    ;2         *
    bcc     Lfa51                   ;2/3 =  28 *
Lfa4f
    sbc     #$a0                    ;2   =   2 *
Lfa51
    jsr     Lfe00                   ;6         *
    inx                             ;2         *
    lda     ram_B2                  ;3         *
    clc                             ;2         *
    adc     #$24                    ;2         *
    cmp     #$a0                    ;2         *
    bcc     Lfa60                   ;2/3       *
    sbc     #$a0                    ;2   =  21 *
Lfa60
    jsr     Lfe00                   ;6         *
    lda     #$17                    ;2         *
    sta     NUSIZ0                  ;3         *
    sta     NUSIZ1                  ;3         *
    ldy     #$24                    ;2         *
    sty     COLUP0                  ;3         *
    sty     COLUP1                  ;3         *
    ldx     #$04                    ;2   =  24 *
Lfa71
    sta     WSYNC                   ;3   =   3 *
;---------------------------------------
    sta     HMOVE                   ;3         *
    lda     Lfdd7,x                 ;4         *
    sta     COLUBK                  ;3         *
    lda     Lfdd3,x                 ;4         *
    sta     GRP0                    ;3         *
    sta     GRP1                    ;3         *
    dex                             ;2         *
    sty     HMP0                    ;3         *
    sty     HMP1                    ;3         *
    bne     Lfa71                   ;2/3       *
    sta     WSYNC                   ;3   =  33 *
;---------------------------------------
    stx     GRP0                    ;3         *
    stx     GRP1                    ;3         *
    sty     COLUBK                  ;3         *
    lda     ram_B2                  ;3         *
    cmp     #$50                    ;2         *
    bcc     Lfa99                   ;2/3       *
    adc     #$5f                    ;2         *
    clc                             ;2   =  20 *
Lfa99
    adc     #$52                    ;2         *
    tay                             ;2         *
    ldx     #$02                    ;2         *
    jsr     Lfe00                   ;6         *
    tya                             ;2         *
    clc                             ;2         *
    adc     #$01                    ;2         *
    inx                             ;2         *
    jsr     Lfe00                   ;6         *
    sta     WSYNC                   ;3   =  29 *
;---------------------------------------
    sta     HMOVE                   ;3         *
    jsr     Lff00                   ;6         *
    ldy     #$00                    ;2         *
    lda     ram_B2                  ;3         *
    cmp     #$50                    ;2         *
    bcc     Lfac5                   ;2/3       *
    ldx     #$10                    ;2         *
    stx     HMM0                    ;3         *
    ldx     #$01                    ;2         *
    stx     HMM1                    ;3         *
    cmp     #$78                    ;2         *
    jmp     Lfacf                   ;3   =  33 *
    
Lfac5
    ldx     #$f0                    ;2         *
    stx     HMM1                    ;3         *
    ldx     #$00                    ;2         *
    stx     HMM0                    ;3         *
    cmp     #$28                    ;2   =  12 *
Lfacf
    beq     Lfad7                   ;2/3       *
    ldy     #$10                    ;2         *
    bcc     Lfad7                   ;2/3       *
    ldy     #$f0                    ;2   =   8 *
Lfad7
    sty     ram_E4,x                ;4         *
    sta     WSYNC                   ;3   =   7 *
;---------------------------------------
    lda     #$20                    ;2         *
    sta     COLUBK                  ;3         *
    lda     ram_B2                  ;3         *
    cmp     #$50                    ;2         *
    bcc     Lfae7                   ;2/3       *
    sbc     #$50                    ;2   =  14 *
Lfae7
    cmp     #$29                    ;2         *
    bcc     Lfaef                   ;2/3       *
    eor     #$ff                    ;2         *
    sbc     #$b0                    ;2   =   8 *
Lfaef
    lsr                             ;2         *
    sta     ram_FA                  ;3         *
    lda     ram_E8                  ;3         *
    beq     Lfb02                   ;2/3!      *
    lda     ram_C1                  ;3         *
    and     #$03                    ;2         *
    bne     Lfb02                   ;2/3!      *
    lda     ram_B6                  ;3         *
    beq     Lfb02                   ;2/3       *
    dec     ram_E9                  ;5   =  27 *
Lfb02
    lda     ram_E9                  ;3         *
    eor     #$ff                    ;2         *
    clc                             ;2         *
    adc     #$36                    ;2         *
    sta     ram_FD                  ;3         *
    sta     WSYNC                   ;3   =  15 *
;---------------------------------------
    lda     #$28                    ;2         *
    sta     COLUBK                  ;3         *
    lda     #$1d                    ;2         *
    sec                             ;2         *
    sbc     ram_A5                  ;3         *
    clc                             ;2         *
    adc     ram_FC                  ;3         *
    sta     ram_FC                  ;3         *
    ldy     #$0f                    ;2         *
    lda     ram_C1                  ;3         *
    and     #$10                    ;2         *
    bne     Lfb25                   ;2/3       *
    ldy     #$0a                    ;2   =  31 *
Lfb25
    sty     COLUP0                  ;3         *
    sty     COLUP1                  ;3         *
    sty     CXCLR                   ;3         *
    ldy     #$00                    ;2   =  11 *
Lfb2d
    sta     WSYNC                   ;3   =   3 *
;---------------------------------------
    sta     HMOVE                   ;3         *
    iny                             ;2         *
    cpy     ram_FA                  ;3         *
    bcc     Lfb3e                   ;2/3       *
    ldy     #$00                    ;2         *
    lda     ram_E4,x                ;4         *
    sta     HMM0,x                  ;4         *
    bcs     Lfb42                   ;2/3 =  22 *
Lfb3e
    lda     #$00                    ;2         *
    sta     HMM0,x                  ;4   =   6 *
Lfb42
    lda     #$00                    ;2         *
    dec     ram_FD                  ;5         *
    bpl     Lfb4c                   ;2/3       *
    sta     ENAM0                   ;3         *
    bmi     Lfb5c                   ;2/3 =  14 *
Lfb4c
    bit     CXM0FB                  ;3         *
    bmi     Lfb52                   ;2/3       *
    lda     #$02                    ;2   =   7 *
Lfb52
    sta     ENAM0                   ;3         *
    lda     #$00                    ;2         *
    bit     CXM1FB                  ;3         *
    bmi     Lfb5c                   ;2/3       *
    lda     #$02                    ;2   =  12 *
Lfb5c
    sta     ENAM1                   ;3         *
    dec     ram_FC                  ;5         *
    bpl     Lfb2d                   ;2/3       *
    txs                             ;2         *
    ldx     #$0a                    ;2   =  14 *
Lfb65
    sta     WSYNC                   ;3   =   3 *
;---------------------------------------
    sta     HMOVE                   ;3         *
    lda     Lfe26,x                 ;4         *
    sta     PF0                     ;3         *
    lda     Lff00,x                 ;4         *
    sta     PF1                     ;3         *
    lda     Lfe30,x                 ;4         *
    sta     PF2                     ;3         *
    stx     ram_FB                  ;3         *
    tsx                             ;2         *
    iny                             ;2         *
    cpy     ram_FA                  ;3         *
    bcc     Lfb88                   ;2/3       *
    ldy     #$00                    ;2         *
    lda     ram_E4,x                ;4         *
    sta     HMM0,x                  ;4         *
    bcs     Lfb8c                   ;2/3 =  48 *
Lfb88
    lda     #$00                    ;2         *
    sta     HMM0,x                  ;4   =   6 *
Lfb8c
    dec     ram_FD                  ;5         *
    bpl     Lfb94                   ;2/3       *
    sta     ENAM0                   ;3         *
    sta     ENAM1                   ;3   =  13 *
Lfb94
    ldx     ram_FB                  ;3         *
    dex                             ;2         *
    bne     Lfb65                   ;2/3       *
    jmp     Lfc38                   ;3   =  10 *
    
Lfb9c
    sta     GRP0                    ;3         *
    lda     Lfdbe,x                 ;4         *
    sta     COLUP0                  ;3         *
    sta     HMCLR                   ;3         *
    dex                             ;2         *
    rts                             ;6   =  21 *
    
Lfba7
    stx     WSYNC                   ;3   =   3
;---------------------------------------
    dex                             ;2        
    bne     Lfba7                   ;2/3      
    stx     GRP0                    ;3        
    stx     ENAM1                   ;3        
    lda     ram_B7                  ;3        
    beq     Lfbc2                   ;2/3      
    lda     #$10                    ;2        
    ldy     ram_B5                  ;3        
    beq     Lfbc2                   ;2/3      
    ldy     ram_FD                  ;3         *
    cpy     #$90                    ;2         *
    bne     Lfbc2                   ;2/3       *
    lda     ram_88                  ;3   =  32 *
Lfbc2
    sta     ram_FC                  ;3        
    lda     ram_EF                  ;3        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    and     #$0f                    ;2        
    tay                             ;2        
    nop                             ;2        
    stx     COLUP0                  ;3        
    ldx     #$11                    ;2        
    stx     CTRLPF                  ;3        
    lda     #$15                    ;2        
    ldx     #$05                    ;2        
    sta     NUSIZ0                  ;3   =  21
Lfbd8
    dex                             ;2        
    bne     Lfbd8                   ;2/3      
    sta     RESM0                   ;3        
    sta     HMM0,x                  ;4        
    lda     #$c0                    ;2        
    sta     HMP0,x                  ;4        
    dex                             ;2        
    stx     ENAM0                   ;3        
    ldx     #$11                    ;2        
    lda     #PURPLE|$9              ;2        
    sta     RESP0                   ;3   =  29
Lfbec
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sta     HMOVE                   ;3        
    sta     COLUPF                  ;3        
    lda     Lfe33,x                 ;4        
    sta     PF0                     ;3        
    lda     Lfe3b,x                 ;4        
    sta     PF1                     ;3        
    lda     Lfe43,x                 ;4        
    sta     PF2                     ;3   =  27
Lfc01
    lda     #$ff                    ;2        
    sta     GRP0                    ;3        
    dey                             ;2        
    bpl     Lfc0a                   ;2/3      
    ldy     #$0f                    ;2   =  11
Lfc0a
    sta     HMCLR                   ;3        
    lda     ram_FD                  ;3        
    dec     ram_FC                  ;5        
    bpl     Lfc18                   ;2/3      
    lda     Lfe55,y                 ;4        
    sec                             ;2        
    adc     ram_EA                  ;3   =  22
Lfc18
    dex                             ;2        
    beq     Lfc38                   ;2/3      
    cpx     #$0a                    ;2        
    bcs     Lfbec                   ;2/3!     
    sta     WSYNC                   ;3   =  11
;---------------------------------------
    sta     COLUBK                  ;3        
    lda     #BLACK|$0               ;2        
    sta     COLUPF                  ;3        
    lda     Lfe26,x                 ;4        
    sta     PF0                     ;3        
    lda     Lff00,x                 ;4        
    sta     PF1                     ;3        
    lda     Lfe30,x                 ;4        
    sta     PF2                     ;3        
    bcc     Lfc01                   ;2/3 =  31
Lfc38
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    jmp     Lf000                   ;3   =   3
    
Bank1_Handler
    bit     ram_F9                  ;3        
    bmi     Lfc89                   ;2/3      
    lda     ram_B5                  ;3        
    cmp     #$02                    ;2        
    bne     Lfc60                   ;2/3      
    bit     ram_C6                  ;3         *
    bmi     Lfc5c                   ;2/3       *
    lda     ram_E6                  ;3         *
    bpl     Lfc60                   ;2/3       *
    ldx     #$41                    ;2         *
    stx     ram_CA                  ;3         *
    cmp     #$ff                    ;2         *
    bne     Lfc60                   ;2/3       *
    ldx     #$85                    ;2         *
    jsr     Lfcc7                   ;6   =  39 *
Lfc5c
    ldx     #$01                    ;2         *
    stx     ram_E6                  ;3   =   5 *
Lfc60
    lda     ram_ED                  ;3        
    bne     Lfc89                   ;2/3      
    bit     ram_C6                  ;3        
    bpl     Lfc85                   ;2/3      
    ldx     #$50                    ;2         *
    lda     ram_B5                  ;3         *
    beq     Lfc89                   ;2/3       *
    and     #$01                    ;2         *
    bne     Lfc82                   ;2/3       *
    lda     ram_F0                  ;3         *
    bne     Lfc89                   ;2/3       *
    dec     ram_F0                  ;5         *
    .byte   $2c ;bit                ;4-5 =  30 *
Lfc79
    inc     ram_F0                  ;5         *
    ldx     #$72                    ;2         *
    jsr     Lfce6                   ;6         *
    bne     Lfc89                   ;2/3 =  15 *
Lfc82
    jsr     Lfcc7                   ;6   =   6 *
Lfc85
    lda     ram_F0                  ;3        
    bne     Lfc79                   ;2/3 =   5
Lfc89
    ldx     INTIM                   ;4        
    bne     Lfc89                   ;2/3      
    ldy     #$02                    ;2        
    sty     WSYNC                   ;3   =  11
;---------------------------------------
    sty     VSYNC                   ;3        
    lda     ram_F2                  ;3        
    beq     Lfca2                   ;2/3      
    lda     ram_CA                  ;3         *
    bne     Lfca2                   ;2/3       *
    dec     ram_F2                  ;5         *
    lda     #$82                    ;2         *
    sta     ram_CA                  ;3   =  23 *
Lfca2
    lda     ram_DE                  ;3        
    jsr     Lfe00                   ;6        
    stx     WSYNC                   ;3   =  12
;---------------------------------------
    stx     VSYNC                   ;3        
    jmp     Lf011                   ;3   =   6
    
Lfcae
    bit     ram_F8                  ;3        
    bmi     Lfd09                   ;2/3!     
    sed                             ;2        
    sec                             ;2        
    sta     ram_FA                  ;3        
    lda     ram_9A                  ;3        
    sbc     ram_FA                  ;3        
    sta     ram_9A                  ;3        
    lda     ram_9B                  ;3        
    sbc     #$00                    ;2        
    sta     ram_9B                  ;3        
    cld                             ;2        
    bcs     Lfd09                   ;2/3!     
    ldx     #$99                    ;2   =  35 *
Lfcc7
    sec                             ;2         *
    lda     ram_B6                  ;3         *
    beq     Lfd09                   ;2/3!      *
    stx     ram_9F                  ;3         *
    txa                             ;2         *
    ldx     ram_F8                  ;3         *
    beq     Lfced                   ;2/3       *
    inx                             ;2         *
    ldy     #$05                    ;2   =  21 *
Lfcd6
    cmp     Lfd09,y                 ;4         *
    beq     Lfcf1                   ;2/3       *
    dey                             ;2         *
    bne     Lfcd6                   ;2/3       *
    bit     ram_F9                  ;3         *
    bmi     Lfd09                   ;2/3!      *
    sta     ram_F1                  ;3         *
    ldx     #$b4                    ;2   =  20 *
Lfce6
    stx     ram_CA                  ;3         *
    ldx     #$fe                    ;2         *
    stx     ram_C9                  ;3         *
    rts                             ;6   =  14 *
    
Lfced
    stx     ram_9A                  ;3         *
    stx     ram_9B                  ;3   =   6 *
Lfcf1
    stx     ram_BA                  ;3         *
    stx     ram_F1                  ;3         *
    stx     ram_B6                  ;3         *
    stx     ram_B4                  ;3         *
    stx     ram_AB                  ;3         *
    stx     ram_E1                  ;3         *
    stx     ram_B3                  ;3         *
    stx     ram_E7                  ;3         *
    stx     ram_F9                  ;3         *
    stx     ram_F8                  ;3         *
    ldx     #$1b                    ;2         *
    stx     ram_85                  ;3   =  35 *
Lfd09
    rts                             ;6   =   6
    
    .byte   $95,$70,$75,$80,$10             ; $fd0a (*)
    
Lfd0f
    ora     ram_AB                  ;3         *
    sta     ram_AB                  ;3         *
    lda     ram_E3                  ;3         *
    beq     Lfd1d                   ;2/3       *
    bit     ram_80                  ;3         *
    bmi     Lfd1d                   ;2/3       *
    inc     ram_B0                  ;5   =  21 *
Lfd1d
    ldx     #$0f                    ;2         *
    rts                             ;6   =   8 *
    
Lfd20
    lda     ram_A5                  ;3         *
    cmp     #$ff                    ;2         *
    beq     Lfd40                   ;2/3       *
    sed                             ;2         *
    lda     ram_98                  ;3         *
    clc                             ;2         *
    adc     #$01                    ;2         *
    sta     ram_98                  ;3         *
    bcc     Lfd38                   ;2/3       *
    lda     ram_99                  ;3         *
    adc     #$00                    ;2         *
    sta     ram_99                  ;3         *
    inc     ram_A4                  ;5   =  34 *
Lfd38
    lda     ram_98                  ;3         *
    and     #$0f                    ;2         *
    bne     Lfd40                   ;2/3 =   7 *
Lfd3e
    inc     ram_A5                  ;5   =   5 *
Lfd40
    cld                             ;2         *
    rts                             ;6   =   8 *
    
Lfd42
    sed                             ;2         *
    lda     ram_98                  ;3         *
    sec                             ;2         *
    sbc     #$01                    ;2         *
    sta     ram_98                  ;3         *
    bcs     Lfd54                   ;2/3       *
    lda     ram_99                  ;3         *
    sbc     #$00                    ;2         *
    sta     ram_99                  ;3         *
    dec     ram_A4                  ;5   =  27 *
Lfd54
    lda     ram_98                  ;3         *
    and     #$0f                    ;2         *
    bne     Lfd40                   ;2/3       *
    dec     ram_A5                  ;5         *
    lda     ram_A5                  ;3         *
    cmp     #$ff                    ;2         *
    bne     Lfd40                   ;2/3       *
    beq     Lfd3e                   ;2/3 =  21 *
    
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
    cpy     #$04                    ;2        
    bcs     Lfde6                   ;2/3      
    bit     ram_E1                  ;3         *
    bpl     Lfde6                   ;2/3       *
    lda     #$50                    ;2   =  11 *
Lfde6
    sta     ram_CC,x                ;4        
    lda     #$de                    ;2        
    sta     ram_CD,x                ;4        
    dex                             ;2        
    rts                             ;6   =  18
    
Lfdee
    .byte   $15,$20,$25,$30,$35,$40,$40     ; $fdee (*)
    
Lfdf5
    clc                             ;2         *
    adc     ram_B2                  ;3         *
    bcs     Lfdfe                   ;2/3       *
    cmp     #$a0                    ;2         *
    bcc     Lfe00                   ;2/3!=  11 *
Lfdfe
    sbc     #$9f                    ;2   =   2 *
Lfe00
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    sec                             ;2   =   2
Lfe03
    sbc     #$0f                    ;2        
    bcs     Lfe03                   ;2/3      
    eor     #$0f                    ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    adc     #$80                    ;2        
    sta     RESP0,x                 ;4        
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    sta     HMP0,x                  ;4        
    rts                             ;6   =  10
    
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
    lda     #$00                    ;2        
    ldx     #$77                    ;2   =   4
Lfe69
    sta     ram_80,x                ;4        
    dex                             ;2        
    bne     Lfe69                   ;2/3 =   8
Lfe6e
    ldx     #$21                    ;2   =   2
Lfe70
    lda     Lfd64,x                 ;4        
    sta     ram_80,x                ;4        
    dex                             ;2        
    bpl     Lfe70                   ;2/3 =  12
Lfe78
    rts                             ;6   =   6
    
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
    inc     ram_A6                  ;5         *
    bne     Lfedc                   ;2/3       *
    dec     ram_A6                  ;5         *
    rts                             ;6   =  18 *
    
Lfedc
    sed                             ;2         *
    lda     ram_96                  ;3         *
    clc                             ;2         *
    adc     #$01                    ;2         *
    sta     ram_96                  ;3         *
    lda     ram_97                  ;3         *
    adc     #$00                    ;2         *
    bcc     Lfefd                   ;2/3 =  19 *
Lfeea
    dec     ram_A6                  ;5         *
    bne     Lfef1                   ;2/3       *
    inc     ram_A6                  ;5         *
    rts                             ;6   =  18 *
    
Lfef1
    sed                             ;2         *
    sec                             ;2         *
    lda     ram_96                  ;3         *
    sbc     #$01                    ;2         *
    sta     ram_96                  ;3         *
    lda     ram_97                  ;3         *
    sbc     #$00                    ;2   =  17 *
Lfefd
    sta     ram_97                  ;3         *
    cld                             ;2   =   5 *
Lff00
    rts                             ;6   =   6 *
    
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
