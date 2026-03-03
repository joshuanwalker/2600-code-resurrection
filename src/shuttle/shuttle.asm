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

;--- RNG / Display Pointers ---
rngSeed                 = $80   ; Pseudo-random number generator seed (LFSR)

;--- Thrust Arrow Positions ---
thrustArrowX            = $81   ; Player-controlled thrust arrow X position
computerArrowX          = $82   ; Computer-controlled target arrow X position
computerArrowAuxX       = $83   ; Auxiliary X for computer arrow animation

;--- Screen / Display State ---
currentScreenId         = $84   ; Current screen/phase display identifier
statusDisplayId         = $85   ; Selects which instrument value to show (ALT, SPD, etc.)

;--- Trajectory Tracking ---
trajectoryDotPos        = $86   ; Trajectory reference dot position on screen
planeDotPos             = $87   ; Plane/shuttle trajectory dot position
pitchValue              = $88   ; Current pitch angle value (signed)

;--- Starfield Column Array ---
starfieldColumnIndex    = $89   ; Index for cycling through starfield columns ($8A-$94)
starfieldColumns        = $8a   ; Array of 12 starfield column X-positions ($8A-$95)
;                         $8b  (i)  ; starfieldColumns+1
;                         $8c  (i)  ; starfieldColumns+2
;                         $8d  (i)  ; starfieldColumns+3
;                         $8e  (i)  ; starfieldColumns+4
;                         $8f  (i)  ; starfieldColumns+5
;                         $90  (i)  ; starfieldColumns+6
;                         $91  (i)  ; starfieldColumns+7
;                         $92  (i)  ; starfieldColumns+8
;                         $93  (i)  ; starfieldColumns+9
;                         $94  (i)  ; starfieldColumns+10
starfieldColumnLast     = $95   ; Last element of starfield column array (rotation overflow)

;--- Speed Display ---
speedDisplayLow         = $96   ; BCD speed display value (low byte: tens+units)
speedDisplayHigh        = $97   ; BCD speed display value (high byte: hundreds)

;--- Speed Fraction Accumulators ---
speedFractionLow        = $98   ; BCD fractional speed accumulator (low); wraps trigger altitude change
speedFractionHigh       = $99   ; BCD fractional speed accumulator (high); carries from low

;--- Fuel / MET Counters ---
fuelLow                 = $9a   ; Fuel remaining (BCD low byte)
fuelHigh                = $9b   ; Fuel remaining (BCD high byte)
metLow                  = $9c   ; Mission Elapsed Time (BCD low byte: seconds)
metHigh                 = $9d   ; Mission Elapsed Time (BCD high byte: minutes)

;--- Score / Abort ---
missionScore            = $9e   ; BCD mission score; incremented on each successful docking
abortCode               = $9f   ; Mission abort/failure reason code for display

;--- Difficulty / Alignment ---
difficultyLevel         = $a0   ; Difficulty/flight program level (0=easiest, 3=hardest)
arrowsMisaligned        = $a1   ; Non-zero when thrust arrows are misaligned

;--- Display Conversion Temps ---
displayDigitsLow        = $a2   ; BCD tens+ones digits for instrument display
displayDigitsHigh       = $a3   ; BCD sign+hundreds digit ($A0=negative, $00=positive)

;--- Descent / Flight State ---
descentRateIndex        = $a4   ; Index into descent rate table; tracks speed bracket
altitude                = $a5   ; Current altitude (0-255 nautical miles)
speed                   = $a6   ; Current speed value

;--- Update Timers ---
speedUpdateTimer        = $a7   ; Countdown for speed/trajectory updates during ascent
altitudeUpdateTimer     = $a8   ; Countdown for altitude changes during ascent

;--- Thrust / Trajectory ---
thrustDirection         = $a9   ; Current thrust direction setting
planeCorrection         = $aa   ; Plane trajectory correction offset

;--- Movement / View Offsets ---
movementFlags           = $ab   ; Bitfield: bit0=fwd, 1=down, 2=up, 3=right
viewVerticalOffset      = $ac   ; Vertical offset for cockpit view graphic
viewHorizontalOffset    = $ad   ; Horizontal fine-position offset for cockpit view

;--- Countdown / Launch ---
countdownTimer          = $ae   ; Pre-launch countdown timer; also used in color calc

;--- OMS / Orbit ---
omsBurnActive           = $af   ; OMS burn active flag (non-zero = burn in progress)
satelliteOrbitalPos     = $b0   ; Satellite's orbital position for docking
shuttleOrbitalPos       = $b1   ; Shuttle's orbital position; must match satellite
yAxisPlane              = $b2   ; Y-axis plane correction value
omsYaw                  = $b3   ; OMS yaw angle; 0=correct alignment for deorbit

;--- Game Phase Tracking ---
launchPhase             = $b4   ; Current phase within launch sequence
flightPhase             = $b5   ; Overall flight phase (launch/orbit/reentry/landing)
gameActive              = $b6   ; Game active flag (non-zero = mission in progress)
enginePowerOn           = $b7   ; Engine power state (non-zero = engines on)

;--- Attract Mode ---
screenBlankFlags        = $b8   ; Screen blanking/attract mode flags for VBLANK
attractSubTimer         = $b9   ; Attract sub-timer; wraps every 256 frames
attractTimer            = $ba   ; Attract mode timer; triggers demo then blanking

;--- Launch Events ---
launchEventIndex        = $bb   ; Index into launch event table
autoThrustCommand       = $bc   ; Autopilot thrust command value
;                         $bd  (unused)

;--- Orbital Movement ---
orbitalMoveFraction     = $be   ; Fractional accumulator for orbital position advancement

;--- Input / Frame Timing ---
inputDelayTimer         = $c0   ; Input rate-limiter countdown for joystick
frameCounter            = $c1   ; Global frame counter; incremented every frame

;--- Descent Timers ---
descentFrameCounter     = $c2   ; Descent frame counter; triggers speed decrease at interval
descentSpeedTimer       = $c3   ; Countdown for altitude decreases during reentry
descentRate             = $c4   ; Current descent rate value from table/pitch calc

;--- MET / Console ---
metFrameCounter         = $c5   ; Frame sub-counter for MET clock updates
consoleSwitches         = $c6   ; Cached SWCHB console switch state

;--- Switch / Sound ---
switchDebounceTimer     = $c7   ; Debounce/hold counter for Reset and Select buttons
engineOnTimer           = $c8   ; Short-lived timer set on engine power-on; display effect
soundSequenceIndex      = $c9   ; Sound engine sequence index; $FE=start new sound
soundEffectId           = $ca   ; Current sound effect ID/type loaded into AUDC0 (0=silent)
soundEnvelopeCounter    = $cb   ; Sound envelope duration counter within current effect

;--- Screen Data Pointers (6 pairs) ---
screenPtr1L             = $cc   ; Screen data pointer 1 (low byte)
screenPtr1H             = $cd   ; Screen data pointer 1 (high byte)
screenPtr2L             = $ce   ; Screen data pointer 2 (low byte)
screenPtr2H             = $cf   ; Screen data pointer 2 (high byte)
screenPtr3L             = $d0   ; Screen data pointer 3 (low byte)
screenPtr3H             = $d1   ; Screen data pointer 3 (high byte)
screenPtr4L             = $d2   ; Screen data pointer 4 (low byte)
screenPtr4H             = $d3   ; Screen data pointer 4 (high byte)
screenPtr5L             = $d4   ; Screen data pointer 5 (low byte)
screenPtr5H             = $d5   ; Screen data pointer 5 (high byte)
screenPtr6L             = $d6   ; Screen data pointer 6 (low byte)
screenPtr6H             = $d7   ; Screen data pointer 6 (high byte)

;--- Starfield GFX Pointers ---
columnGfxPtrTable       = $d8   ; Array of 4 graphic data pointer low bytes for starfield
;                         $d9  (i)  ; columnGfxPtrTable+1
;                         $da  (i)  ; columnGfxPtrTable+2
;                         $db  (i)  ; columnGfxPtrTable+3
gfxDataPtrL             = $dc   ; Low byte of indirect pointer for kernel gfx data
gfxDataPtrH             = $dd   ; High byte of indirect pointer (always $FF = ROM)

;--- Docking / Target Position ---
targetHorizPos          = $de   ; Horizontal screen position of satellite ($47-$4F=dock range)
targetHorizPosSmooth    = $df   ; Smoothed horizontal target pos; converges to ideal X

;--- Orbital / Reentry Counters ---
orbitalSubCounter       = $e0   ; Sub-frame counter; odd values trigger orbital advance
heatEffectTimer         = $e1   ; Reentry ionization heating effect timer
dockingProgress         = $e2   ; Docking progress (0-$7F=approaching, $80=docked, $FF=done)
dockingCount            = $e3   ; Total successful dockings this mission (max 6)

;--- Starfield ---
starfieldHorizontalMotion = $e4 ; Horizontal scroll motion value for starfield
;                         $e5  (unused)

;--- Cargo / Reentry ---
cargoDoorTimer          = $e6   ; Cargo door warning timer; overflows negative = abort
atmosphereDensity       = $e7   ; Atmosphere density counter for reentry grey tint effect
reentryContactState     = $e8   ; Reentry heating / ground contact flag (0=none, $FF=active)
starfieldScrollY        = $e9   ; Starfield vertical scroll Y position

;--- Satellite / Landing ---
satelliteColorOffset    = $ea   ; Color offset oscillator for satellite rendering (0-7)
approachFrameTimer      = $eb   ; Landing approach frame countdown; ticks starfieldScrollY
separationFlashTimer    = $ec   ; SRB/ET separation flash timer; counts down from 5
separationEventTimer    = $ed   ; SRB/ET separation event countdown; wraps for sound+flash
landingFlickerTimer     = $ee   ; Landing approach display flicker timer
cockpitAnimCounter      = $ef   ; Cockpit instrument animation counter; low nibble=offset

;--- Game State Flags ---
cargoDoorState          = $f0   ; Cargo bay door open/close state
errorDisplayFlag        = $f1   ; Error/abort display override (non-zero=show error)
landingDisplayMode      = $f2   ; Landing display mode counter; triggers visual/audio
landingPitchSuccess     = $f3   ; Set when touchdown with perfect pitch (pitchValue=0)
pendingSpeedEffect      = $f4   ; Deferred speed effect: 0=none, 1=decrease, 2=increase
fuelPenaltyAccum        = $f5   ; Accumulated fuel penalty from trajectory misalignment
trajectoryThreshold     = $f6   ; Trajectory alignment threshold for fuel penalty calc

;--- Mode Flags ---
joystickDetected        = $f7   ; Joystick activity detected ($FF=input seen)
trainingModeFlag        = $f8   ; Training mode flag ($FF=active: no fuel burn)

;--- Autopilot ---
autopilotMode           = $f9   ; Autopilot mode (bit 7: on/off)

;--- Scratch / Temp Variables ---
tempVar                 = $fa   ; General-purpose temporary variable (reused everywhere)
tempVar2                = $fb   ; General-purpose temporary variable 2
tempVar3                = $fc   ; General-purpose temporary variable 3

;--- Starfield / Stack ---
starfieldVerticalCounter = $fd  ; Starfield vertical position counter
;                         $fe  (s)  ; Stack area
;                         $ff  (s)  ; Stack area


;-----------------------------------------------------------
;      Non Locatable Labels
;-----------------------------------------------------------


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
    lda     computerArrowX
    sta     HMCLR
    jsr     $d958
    lda     computerArrowAuxX
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
    ldx     dockingCount
    cpx     #$04
    bcc     beginDashboardKernel
    lda     flightPhase
    cmp     #$02
    bne     beginDashboardKernel
    lda     frameCounter
    and     $debd,x
    bne     beginDashboardKernel
    bit     rngSeed
    bmi     windShearIncY
    dec     yAxisPlane
    .byte   $2c ;bit                ;4-5 =  60 *
windShearIncY
    inc     yAxisPlane
beginDashboardKernel
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
kernelDrawThrustBar
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
    bne     kernelDrawThrustBar
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
    lda     arrowsMisaligned
    beq     setArrowColors
    lda     frameCounter
    and     #$20
    beq     setArrowColors
    ldx     #$82
setArrowColors
    stx     COLUP1
    ldx     #$08
    lda     thrustArrowX
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sec
thrustArrowPosLoop
    sbc     #$0f
    bcs     thrustArrowPosLoop
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
    lda     shuttleOrbitalPos
    sec
    sbc     satelliteOrbitalPos
    cmp     #$80
    bne     setupInstrumentBG
    ldx     dockingProgress
    cpx     #$ff
    bne     setupInstrumentBG
    inc     dockingProgress
    lda     rngSeed
    sta     yAxisPlane
setupInstrumentBG
    ldx     #BLACK|$4
drawBlankBGLoop
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    dex
    bne     drawBlankBGLoop
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
    lda     fuelHigh
    cmp     #$10
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sty     COLUBK
    lda     #$fe
    sta     PF2
    bcs     setNormalFuelColor
    lda     frameCounter
    and     #$10
    beq     setNormalFuelColor
    ldx     #$86
    stx     COLUPF
    ldy     #$80
    bne     applyDigitColors
setNormalFuelColor
    stx     COLUPF
    ldy     #BLACK|$a
    lda     statusDisplayId
    cmp     #$19
    bcc     setDigitColorNormal
    beq     applyDigitColors
    lda     frameCounter
    and     #$10
    beq     applyDigitColors
setDigitColorNormal
    ldy     #$58
applyDigitColors
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
    sta     tempVar
    jsr     $d979
    ldy     #$00
    sty     GRP0
    sty     GRP1
    sta     HMOVE
    sty     GRP0
    sty     pendingSpeedEffect
    sty     fuelPenaltyAccum
    sty     NUSIZ0
    lda     errorDisplayFlag
    bne     setupMainView
    lda     statusDisplayId
    cmp     #$17
    beq     enableStatusDot
    cmp     #$05
    bcs     setupMainView
enableStatusDot
    asl
    sta     ENAM0
setupMainView
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    ldx     #$03
    stx     NUSIZ0
    inx
    stx     ENAM0
    lda     viewHorizontalOffset
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
    beq     loadScreenPtrs
    lda     #$7d
loadScreenPtrs
    ldx     #$08
    clc
    sta     HMCLR
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
fillScreenPtrLoop
    sta     screenPtr2L,x
    adc     #$19
    dex
    dex
    bpl     fillScreenPtrLoop
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
    lda     frameCounter
    ldx     currentScreenId
    bit     heatEffectTimer
    bpl     checkViewUpdate
    and     #$fc
    beq     setScrollPosition
    bne     setStaticViewPtr
checkViewUpdate
    and     $d9f8,x
    beq     setStaticViewPtr
setScrollPosition
    lda     #$e5
    sec
    sbc     viewVerticalOffset
    .byte   $2c ;bit                ;4-2 =   9 *
setStaticViewPtr
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
    sty     tempVar2
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
    ldy     tempVar2
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
    lda     engineOnTimer
    and     #$1f
    cmp     #$14
    stx     HMBL
    sta     RESBL
    bcs     calcFlameOffset
    ldy     #$00
    cmp     #$0c
    bcc     calcFlameOffset
    sbc     #$0c
    tay
calcFlameOffset
    sty     tempVar
    tya
    eor     #$07
    sta     tempVar2
    lda     #$e8
    ldx     #$08
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sec
fillFlamePtrLoop
    sta     screenPtr2L,x
    sbc     #$08
    sta     screenPtr1L,x
    sbc     #$08
    dex
    dex
    dex
    dex
    bpl     fillFlamePtrLoop
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
    dec     tempVar2
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
    ldx     launchPhase
    beq     launchPhaseWSYNC
    lda     frameCounter
    and     #$02
    bne     extraWSYNC
    inx
launchPhaseWSYNC
    stx     WSYNC
;---------------------------------------
extraWSYNC
    stx     WSYNC
;---------------------------------------
    cpx     #$05
    beq     beginOverscanLogic
    stx     WSYNC
;---------------------------------------
beginOverscanLogic
    ldx     #$dc
    stx     TIM8T
    bit     autopilotMode
    bmi     checkArrowAlignment
    lda     gameActive
    beq     checkArrowAlignment
    lda     descentRateIndex
    beq     checkArrowAlignment
    ldy     currentScreenId
    beq     loadTrajectoryPenalty
    cpy     #$03
    bne     checkArrowAlignment
    adc     #$14
loadTrajectoryPenalty
    tax
    lda     $d868,x
    ldx     trainingModeFlag
    inx
    clc
    adc     $dafa,x
    cmp     trajectoryThreshold
    bcc     addFuelPenalty
    sbc     $d9e6,x
    cmp     trajectoryThreshold
    bcc     clearWarningIfActive
    beq     clearWarningIfActive
addFuelPenalty
    inc     fuelPenaltyAccum
    ldx     dockingCount
    cpx     #$06
    bne     queueWarningSound
    inc     fuelPenaltyAccum
queueWarningSound
    lda     #$41
    ldx     soundEffectId
    bne     checkArrowAlignment
    beq     writeSoundEffectId
clearWarningIfActive
    lda     #$00
    ldx     soundEffectId
    cpx     #$41
    bne     checkArrowAlignment
writeSoundEffectId
    sta     soundEffectId
checkArrowAlignment
    ldx     #$00
    lda     thrustArrowX
    cmp     computerArrowAuxX
    bcc     setArrowMisaligned
    sbc     #$0b
    cmp     computerArrowAuxX
    bcc     storeAlignmentResult
setArrowMisaligned
    dex
storeAlignmentResult
    stx     arrowsMisaligned
    lda     SWCHA
    cmp     #$ff
    beq     updateAttractMode
    ldx     #$ff
    stx     joystickDetected
    inx
    stx     screenBlankFlags
updateAttractMode
    inc     attractSubTimer
    bne     checkLandingSound
    lda     attractTimer
    cmp     #$0f
    bcc     incAttractTimer
    ldx     gameActive
    stx     enginePowerOn
incAttractTimer
    inc     attractTimer
    bne     checkLandingSound
    inc     screenBlankFlags
    lda     screenBlankFlags
    and     #$04
    beq     checkLandingSound
    sta     screenBlankFlags
    lda     #$00
    sta     gameActive
    sta     enginePowerOn
checkLandingSound
    lda     currentScreenId
    cmp     #$04
    bne     everyEighthFrame
    lda     starfieldScrollY
    bmi     everyEighthFrame
    dec     landingFlickerTimer
    bne     everyEighthFrame
    asl
    ora     #$02
    sta     landingFlickerTimer
    lda     soundEffectId
    bne     everyEighthFrame
    lda     #$34
    sta     soundEffectId
everyEighthFrame
    lda     frameCounter
    and     #$07
    bne     checkSoundSequenceInit
    bit     autopilotMode
    bpl     resetEngineTimer
    lda     flightPhase
    cmp     #$02
    bne     checkDeorbitPitch
    dec     pitchValue
    bpl     resetEngineTimer
autoPitchUp
    inc     pitchValue
    bpl     resetEngineTimer
checkDeorbitPitch
    cmp     #$04
    beq     resetEngineTimer
    lda     pitchValue
    cmp     #$0d
    bne     autoPitchUp
resetEngineTimer
    lda     enginePowerOn
    beq     tickEngineTimer
    ldx     #$01
    stx     engineOnTimer
tickEngineTimer
    dec     engineOnTimer
checkSoundSequenceInit
    ldx     soundSequenceIndex
    cpx     #$fe
    bne     processSoundCh0
    stx     soundEnvelopeCounter
    inc     soundSequenceIndex
processSoundCh0
    lda     soundEffectId
    beq     silenceChannel0
    sta     AUDC0
    jsr     $ddfb
    tax
    lda     $d94c,x
    inc     soundEnvelopeCounter
    cmp     soundEnvelopeCounter
    bcs     processEngineDrone
    lda     #$00
    sta     soundEnvelopeCounter
    inc     soundSequenceIndex
    lda     $d8f7,x
    adc     soundSequenceIndex
    tax
    lda     $d903,x
    beq     silenceChannel0
    sta     AUDF0
    jsr     $ddfb
    ldx     gameActive
    bne     setSoundVolume
    txa
setSoundVolume
    sta     AUDV0
    jmp     $d4d0
    
silenceChannel0
    sta     AUDV0
    sta     soundEffectId
    ldx     #$fe
    stx     soundSequenceIndex
processEngineDrone
    lda     gameActive
    beq     silenceBothChannels
    
    .byte   $a9,$03,$85,$1a,$a9,$06,$85,$16 ; $d4d4 *)
    .byte   $a9,$1f,$85,$18,$a5,$81,$c9,$0f ; $d4dc *)
    .byte   $d0,$04,$a6,$b4,$f0,$1c,$a2,$08 ; $d4e4 *)
    .byte   $86,$16,$20,$fb,$dd,$18,$69,$02 ; $d4ec *)
    .byte   $65,$b4,$a6,$ca,$d0,$0a,$a2,$02 ; $d4f4 *)
    .byte   $86,$15,$a2,$0e,$86,$17         ; $d4fc *)
    
silenceBothChannels
    sta     AUDV0
    sta     AUDV1
    lda     enginePowerOn
    beq     calcOrbitalMoveRate
    lda     flightPhase
    bne     orbitalPhaseSetup
    jmp     $d728
    
jumpToSatelliteCalc
    jmp     $d647
    
orbitalPhaseSetup
    .byte   $a5,$b4,$f0,$06,$a9,$02,$85,$ab ; $d514 *)
    .byte   $d0,$1e,$a5,$c4,$d0,$1a,$a5,$a6 ; $d51c *)
    .byte   $c9,$df,$b0,$02,$a9,$e0         ; $d524 *)
    
calcOrbitalMoveRate
    sbc     #$08
    eor     #$ff
    bit     autopilotMode
    bpl     storeMovementThreshold
    
    .byte   $a9,$18                         ; $d532 *)
    
storeMovementThreshold
    sta     tempVar
    ldx     omsBurnActive
    beq     advanceOrbitalFraction
    
    .byte   $e6,$be                         ; $d53a *)
    
advanceOrbitalFraction
    inc     orbitalMoveFraction
    cmp     orbitalMoveFraction
    bcs     jumpToSatelliteCalc
    lda     #$00
    sta     orbitalMoveFraction
    inc     shuttleOrbitalPos
    bpl     decSatelliteColor
    
    .byte   $a5,$ea,$c9,$08,$f0,$08         ; $d54a *)
    
incSatelliteColor
    inc     satelliteColorOffset
    bpl     advanceOrbitalCounters
decSatelliteColor
    dec     satelliteColorOffset
    bmi     incSatelliteColor
advanceOrbitalCounters
    inc     orbitalSubCounter
    lda     flightPhase
    cmp     #$02
    bne     advanceSatelliteOrbit
    
    .byte   $24,$f9,$30,$05                 ; $d560 *)
    
advanceSatelliteOrbit
    lda     orbitalSubCounter
    lsr
    bcc     advanceCockpitAnim
    inc     satelliteOrbitalPos
advanceCockpitAnim
    inc     cockpitAnimCounter
    lda     enginePowerOn
    beq     rotateStarfieldColumns
    
    .byte   $a5,$ab,$29,$01,$f0,$1d         ; $d571 *)
    
rotateStarfieldColumns
    dec     starfieldColumnIndex
    lda     starfieldColumnIndex
    cmp     #$0e
    bne     processRightMovement
    lda     #$14
    sta     starfieldColumnIndex
    ldx     #$0b
    lda     starfieldColumns,x
    tay
    dex
    bmi     storeWrappedColumn
    lda     starfieldColumns,x
    sty     starfieldColumns,x
    jmp     $d587
    
storeWrappedColumn
    sty     starfieldColumnLast
processRightMovement
    lda     movementFlags
    and     #$08
    beq     processDownMovement
    
    .byte   $a2,$0b,$d6,$8a,$b5,$8a,$c9,$ff ; $d59a *)
    .byte   $d0,$04,$a9,$84,$95,$8a,$ca,$10 ; $d5a2 *)
    .byte   $f1                             ; $d5aa *)
    
processDownMovement
    lda     movementFlags
    and     #$02
    beq     processUpMovement
    
    .byte   $e6,$89,$a5,$89,$c9,$15,$d0,$17 ; $d5b1 *)
    .byte   $a9,$0f,$85,$89,$a2,$00,$b5,$8a ; $d5b9 *)
    .byte   $a8,$e8,$e0,$0c,$f0,$07,$b5,$8a ; $d5c1 *)
    .byte   $94,$8a,$4c,$c1,$d5,$85,$8a,$c6 ; $d5c9 *)
    .byte   $ef,$c6,$ef                     ; $d5d1 *)
    
processUpMovement
    lda     movementFlags
    and     #$04
    beq     checkEngineAndJump
    
    .byte   $a2,$0b,$f6,$8a,$b5,$8a,$c9,$85 ; $d5da *)
    .byte   $90,$04,$a9,$00,$95,$8a,$ca,$10 ; $d5e2 *)
    .byte   $f1                             ; $d5ea *)
    
checkEngineAndJump
    lda     enginePowerOn
    beq     jumpToStarfieldSetup
    
    .byte   $a5,$b5,$c9,$02,$f0,$03         ; $d5ef *)
    
jumpToStarfieldSetup
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
    
calcSatelliteDistance
    lda     satelliteOrbitalPos
    sec
    sbc     shuttleOrbitalPos
    cmp     #$10
    bcc     getSatelliteSize
    jmp     $d6d2
    
getSatelliteSize
    lsr
    tax
    lda     $d893,x
    sta     NUSIZ0
    lda     $dfde,x
    sta     tempVar2
    lda     $d9c2,x
    clc
    adc     satelliteColorOffset
    cmp     #$9f
    bcc     setSatelliteColor
    
    .byte   $a9,$9f                         ; $d669 *)
    
setSatelliteColor
    sta     COLUP0
    ldy     #$00
    lda     yAxisPlane
    clc
    adc     #$50
    cmp     #$a0
    bcs     branchOnSatDistance
    adc     #$b0
    bcs     checkYBoundary
    
    .byte   $c8,$49,$ff,$69,$01             ; $d67c *)
    
checkYBoundary
    cmp     $dfe6,x
branchOnSatDistance
    bcs     satelliteTooFar
    cpx     #$00
    beq     calcSatelliteYOffset
    
    .byte   $86,$fc,$e0,$06,$90,$01,$98,$4a ; $d68a *)
    .byte   $c6,$fc,$d0,$fb                 ; $d692 *)
    
calcSatelliteYOffset
    sta     tempVar
    lda     yAxisPlane
    cpy     #$01
    beq     calcSmoothTargetPos
    clc
    adc     tempVar
    .byte   $2c ;bit                ;4-3 =  22
calcSmoothTargetPos
    sbc     tempVar
    clc
    adc     #$50
    cmp     targetHorizPosSmooth
    beq     setupSatRenderFlip
    lda     frameCounter
    and     #$03
    bne     setupSatRenderFlip
    bcs     incTargetHorizSmooth
    
    .byte   $c6,$df,$2c                     ; $d6b3 *)
    
incTargetHorizSmooth
    inc     targetHorizPosSmooth
setupSatRenderFlip
    ldy     targetHorizPosSmooth
    lda     frameCounter
    lsr
    sta     REFP0
    and     #$08
    bne     calcSatScreenX
    txa
    ora     #$08
    tax
calcSatScreenX
    sec
    tya
    sbc     $d9ca,x
    bcc     setupStarfieldPointers
    
    .byte   $b0,$3b                         ; $d6ce *)
satelliteTooFar
    .byte   $b0,$56                         ; $d6d0 *)
    
checkSatBehind
    cmp     #$80
    bcc     satelliteMediumRange
    ldx     #$ce
    ldy     #$05
    lda     #BLUE_CYAN|$d
    bcs     applySatSpriteParams
    
satelliteMediumRange
    .byte   $c9,$40,$90,$46,$a2,$0e,$a0,$00 ; $d6de *)
    .byte   $a9,$90                         ; $d6e6 *)
    
applySatSpriteParams
    stx     tempVar2
    sty     NUSIZ0
    sta     COLUP0
    lda     targetHorizPosSmooth
    beq     setupStarfieldPointers
    cmp     #$94
    beq     setupStarfieldPointers
    lda     #$94
    ldx     yAxisPlane
    cpx     #$80
    bcc     compareSatToTarget
    
    .byte   $a9,$00                         ; $d6fe *)
    
compareSatToTarget
    cmp     targetHorizPosSmooth
    bcs     updateSatTargetPos
    
    .byte   $c6,$df,$2c                     ; $d704 *)
    
updateSatTargetPos
    inc     targetHorizPosSmooth
    lda     targetHorizPosSmooth
    sta     targetHorizPos
    lda     flightPhase
    cmp     #$02
    bne     setupStarfieldPointers
    
    .byte   $a5,$88,$d0,$11,$a5,$b3,$69,$0f ; $d713 *)
    .byte   $c9,$21,$b0,$09,$a5,$b7,$f0,$05 ; $d71b *)
    .byte   $a5,$a5,$e9,$c2,$2c             ; $d723 *)
    
setupStarfieldPointers
    lda     #$6a
    sta     tempVar
    ldx     #$04
    lda     #$14
    sec
    sbc     starfieldColumnIndex
    adc     tempVar
    sec
    sbc     #$15
    sta     tempVar
    bpl     storeColumnPtrLoop
    
    .byte   $69,$16,$18,$65,$fb,$4c,$58,$d7 ; $d73c *)
    
calcNextColumnPtr
    lda     tempVar
    sec
    sbc     #$12
    sta     tempVar
    bpl     storeColumnPtrLoop
    
    .byte   $c9,$e2,$90,$05,$69,$15,$4c,$3e ; $d74d *)
    .byte   $d7                             ; $d755 *)
    
storeColumnPtrLoop
    lda     #$04
    sta     columnGfxPtrTable,x
    dex
    bpl     calcNextColumnPtr
    lda     #$ff
    sta     gfxDataPtrH
    lda     currentScreenId
    bne     endOfKernelSwitch
    
calcViewOffset3
    .byte   $20,$da,$d9,$b0,$0f,$a5,$aa,$69 ; $d765 *)
    .byte   $02,$85,$ad,$85,$f6,$a5,$a4     ; $d76d *)
    
bankSwitch0to1
    sta     viewVerticalOffset
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
    beq     calcViewOffset3
    cmp     #$02
    bne     calcViewOffset1
    
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
    
calcViewOffset1
    cmp     #$01
    bne     calcViewOffset4
    
    .byte   $20,$a3,$d9,$20,$da,$d9,$a5,$b0 ; $d7ee *)
    .byte   $b0,$02,$a5,$b1,$4a,$4a,$4a,$aa ; $d7f6 *)
    .byte   $18,$69,$05,$85,$ad,$bd,$ec,$de ; $d7fe *)
    .byte   $d0,$bf                         ; $d806 *)
    
calcViewOffset4
    cmp     #$04
    bne     evaluateLandingScore
    
    .byte   $20,$da,$d9,$b0,$14,$a5,$e9,$10 ; $d80c *)
    .byte   $02,$a9,$00,$4a,$4a,$85,$ad,$a5 ; $d814 *)
    .byte   $a5,$4a,$d0,$02,$a9,$ff,$38,$b0 ; $d81c *)
    .byte   $1d,$a5,$b2,$69,$4f,$c9,$a0,$90 ; $d824 *)
    .byte   $02,$e9,$9f,$4a,$4a,$4a,$18,$69 ; $d82c *)
    .byte   $13,$85,$ad,$a5,$a5,$4a,$f0,$03 ; $d834 *)
    .byte   $49,$0f,$2c,$a9,$10,$18,$69,$03 ; $d83c *)
    
jumpToBankSwitch
    jmp     $d774
    
evaluateLandingScore
    lda     #$00
    sta     viewHorizontalOffset
    ldx     fuelHigh
    ldy     dockingCount
    lda     #$03
    cpx     #$35
    bcc     checkPilotRank
    cpy     #$02
    bcc     checkPilotRank
    
    .byte   $a9,$09                         ; $d859 *)
    
checkPilotRank
    cpx     #$45
    bcc     applyRankAndSwitch
    cpy     #$04
    bcc     applyRankAndSwitch
    
    .byte   $a9,$0f                         ; $d863 *)
    
applyRankAndSwitch
    bne     jumpToBankSwitch
    
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
    
positionSpriteHorizB0
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sec
horizPosLoopB0
    sbc     #$0f
    bcs     horizPosLoopB0
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
    
kernelDrawInstruments
    ldy     tempVar
    lda     (screenPtr1L),y
    sta     tempVar3
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
    ldy     tempVar3
    stx     GRP1
    sta     GRP0
    sty     GRP1
    sty     GRP0
    dec     tempVar
    bpl     kernelDrawInstruments
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
    txs                                 ; Reset stack pointer
    inx                                 ; X = 0
    txa                                 ; A = 0
;-----------------------------------------------------------
;      Vertical Blank / Logic (Bank 1)
;-----------------------------------------------------------
logicStart
    sta     VSYNC,x                     ; Clear TIA registers $00-$FF
    inx
    bne     logicStart
    jsr     initGameVars                ; Initialize game variables from ROM defaults
;-----------------------------------------------------------
;      Main Frame Loop Entry Point
;-----------------------------------------------------------
mainFrameLoop
    ldy     #$29                        ; Set VBLANK timer (~2.7ms, enough for game logic)
    sty     TIM64T
    ;-----------------------------------------------------------
    ; PRNG: Linear Feedback Shift Register (LFSR)
    ; Generates pseudo-random numbers each frame.
    ;-----------------------------------------------------------
    lda     rngSeed
    asl
    asl
    asl
    eor     rngSeed                     ; XOR shifted seed with original
    asl
    rol     rngSeed                     ; Rotate feedback bit into seed
    ;-----------------------------------------------------------
    ; Global Frame Counter (frameCounter)
    ; Used throughout for timing various periodic events.
    ;-----------------------------------------------------------
    inc     frameCounter
    lda     frameCounter
    tay                                 ; Y = frameCounter (used later for timing checks)
    ;-----------------------------------------------------------
    ; Demo Mode / Auto-Start Check
    ; Every 64 frames (and #$3f), if pre-launch and countdown
    ; has advanced enough (countdownTimer=9), start the game automatically
    ; in autopilot mode (Flight #1) or check console switches.
    ;-----------------------------------------------------------
    and     #$3f
    bne     .skipAutoStart
    lda     flightPhase
    bne     .skipAutoStart
    lda     countdownTimer
    beq     .skipAutoStart
    inc     countdownTimer
    cmp     #$09                        ; Countdown reached threshold?
    bne     .skipAutoStart
    sta     countdownTimer
    lda     gameActive
    bne     .skipAutoStart
    bit     autopilotMode               ; Autopilot (Flight #1)?
    bmi     .activateAutoStart
    ldx     #$09
    lda     consoleSwitches
    lsr
    and     #$64
    bne     .setAutoStartScreen
.activateAutoStart
    dec     gameActive                  ; Set gameActive = $FF (true)
    sta     currentScreenId
    ldx     #$07
.setAutoStartScreen
    sta     missionScore
    stx     statusDisplayId
.skipAutoStart
    ;-----------------------------------------------------------
    ; Fuel Consumption
    ; Burns fuel every 16 frames (and #$0f). Extra burn if
    ; T/C arrows misaligned or during active launch phase.
    ;-----------------------------------------------------------
    lda     fuelPenaltyAccum
    jsr     subtractFuel
    tya
    and     #$0f
    bne     .skipFuelBurn
    inc     cargoDoorTimer
    lda     gameActive
    beq     .skipFuelBurn
    clc
    lda     #$01                        ; Base fuel burn = 1
    ldx     launchPhase
    beq     .checkMisalignment
    adc     #$06                        ; +6 during launch (engines firing)
.checkMisalignment
    ldx     arrowsMisaligned
    beq     .doFuelBurn
    adc     #$02                        ; +2 if T/C arrows misaligned (wasting fuel!)
.doFuelBurn
    jsr     subtractFuel
.skipFuelBurn
    ;-----------------------------------------------------------
    ; MECO (Main Engine Cut-Off) Check
    ; In autopilot: triggers at altitude $D2 (210 nautical miles).
    ; Manual: checks Color/BW + Left Diff switches for engine state.
    ;-----------------------------------------------------------
    bit     autopilotMode
    bpl     .checkEngineSwitch
    lda     altitude
    cmp     #$d2                        ; Target orbit altitude = 210nm
    bne     .skipMECO
    lda     #$00                        ; Autopilot: auto-MECO at 210nm
    beq     .doMECO
.checkEngineSwitch
    lda     consoleSwitches
    and     #$48                        ; Check Color/BW ($40) + Left Diff ($08)
    bne     .skipMECO                   ; If either set, engines still on
.doMECO
    ;-----------------------------------------------------------
    ; Transition: Launch -> Orbit
    ; Clear launch phase, set flightPhase to 2 (orbit),
    ; calculate initial orbital position from plane offset.
    ;-----------------------------------------------------------
    sta     launchPhase                 ; Clear launch phase (A=0)
    ldx     gameActive
    beq     .skipMECO                   ; Not active? Skip
    ldx     flightPhase
    cpx     #$01                        ; Must be in countdown/launch phase
    bne     .skipMECO
    stx     movementFlags                      ; Store movement flags
    sta     shuttleOrbitalPos
    inc     flightPhase                 ; flightPhase = 2 (orbit)
    sta     soundEffectId                      ; Clear sound effect
    sta     cargoDoorTimer
    ;-----------------------------------------------------------
    ; Evaluate Orbit Quality
    ; Per manual: "The closer you come to 210 altitude, the
    ; closer you'll be to the satellite's orbit."
    ;-----------------------------------------------------------
    ldx     #$95                        ; Default abort code
    lda     speed
    cmp     #$d2                        ; Speed check
    bcc     .orbitAbortCheck
    lda     #$00
    bit     autopilotMode
    bmi     .setOrbitPosition           ; Autopilot gets perfect orbit
    ldx     #$90
    lda     #$21
    sbc     planeCorrection             ; Plane error affects orbit quality
    cmp     #$08
    bcc     .calcOrbitOffset
.orbitAbortCheck
    jsr     abortMission                ; Too slow or bad plane = abort
    bcs     .skipMECO
.calcOrbitOffset
    asl                                 ; Convert plane error to orbit offset
    asl
    asl
    asl
    adc     #$40
.setOrbitPosition
    sta     satelliteOrbitalPos                      ; Set shuttle's orbital position
    bit     joystickDetected                      ; Check training mode flag
    bpl     .setPlanePosition
    lda     #$10
.setPlanePosition
    bit     autopilotMode
    bmi     .storeYAxis
    lda     yAxisPlane                  ; In manual mode, use current Y position
    asl
    asl
    asl
    asl
.storeYAxis
    sta     yAxisPlane
    inc     separationEventTimer                      ; Trigger separation flash
    inc     currentScreenId             ; Advance to orbit screen
.skipMECO
    ;-----------------------------------------------------------
    ; T/C Thrust Arrow Movement
    ; The Computer Arrow ("C") moves automatically based on
    ; autoThrustCommand during launch events. Player controls
    ; the Thrust Arrow ("T") using the fire button.
    ; Per manual: "Keep T directly under C during countdown."
    ;-----------------------------------------------------------
    lda     launchPhase
    beq     .moveComputerArrow          ; No launch? Just move C arrow
    tya
    and     #$02
    bne     .afterThrustLogic           ; Only update arrows every other frame
    lda     autoThrustCommand           ; Auto-command direction from launch events
.checkAutoCmd
    beq     .afterComputerArrow         ; No command = skip C arrow movement
.moveComputerArrow
    ldx     computerArrowAuxX           ; Aux tracks sub-pixel position
    lsr                                 ; Carry = direction (0=left, 1=right)
    bcs     .moveComputerRight
.moveComputerLeft
    dec     computerArrowX
    dec     computerArrowAuxX
    cpx     #$0d                        ; Left boundary
    bne     .afterComputerArrow
.moveComputerRight
    inc     computerArrowX
    inc     computerArrowAuxX
    cpx     #$81                        ; Right boundary => bounce left
    beq     .moveComputerLeft
.afterComputerArrow
    ;-----------------------------------------------------------
    ; Thrust Arrow (T) - Player Control
    ; During launch: fire button moves T arrow toward C arrow.
    ; Direction latched until button released.
    ; Autopilot uses autoThrustCommand instead of fire button.
    ;-----------------------------------------------------------
    lda     gameActive
    beq     .decThrustArrow             ; Not active? Drift left
    lda     launchPhase
    beq     .decThrustArrow             ; Pre-launch? Drift left
    bit     autopilotMode
    bpl     .checkFireButton
    lda     autoThrustCommand           ; Autopilot: use auto-command
    beq     .clearThrustDir             ; No command = clear direction
    bpl     .applyThrustDir             ; Apply direction directly
.checkFireButton
    bit     INPT4
    bmi     .clearThrustDir             ; Fire not pressed = clear
    ldx     thrustDirection             ; Already have a direction?
    bne     .loadThrustDir
    inx                                 ; Default: right (X=1)
    lda     computerArrowX
    cmp     thrustArrowX                ; C arrow right of T?
    bcs     .storeThrustDir             ; Yes: move right
    inx                                 ; No: move left (X=2)
.storeThrustDir
    stx     thrustDirection
.loadThrustDir
    lda     thrustDirection
.applyThrustDir
    lsr                                 ; Bit 0: 1=right, 0=left
    bcs     .incThrustArrow
.decThrustArrow
    dec     thrustArrowX
    .byte   $2c ;bit                ;4-5 =   4  ; Skip next instruction (bit trick)
.incThrustArrow
    inc     thrustArrowX
    ;-----------------------------------------------------------
    ; Clamp T arrow to screen bounds ($0F - $8B)
    ; If fire released pre-launch with game active, scrub launch.
    ;-----------------------------------------------------------
    lda     thrustArrowX
    cmp     #$0f
    bcc     .incThrustArrow             ; Below min? Force right
    cmp     #$8c
    beq     .decThrustArrow             ; At max? Force left
    bit     INPT4
    bmi     .afterThrustLogic           ; Fire pressed = done
    lda     launchPhase
    bne     .afterThrustLogic           ; Already launching = skip scrub check
    lda     gameActive
    beq     .afterThrustLogic           ; Not active = skip
    lda     flightPhase
    bne     .afterThrustLogic           ; Already in flight = skip
    bit     autopilotMode
    bmi     .afterThrustLogic           ; Autopilot handles itself
    jmp     handleLaunchScrub           ; Player moved T without fire = SCRUB!
    
.clearThrustDir
    lda     #$00
    sta     thrustDirection
.afterThrustLogic
    ;-----------------------------------------------------------
    ; Trajectory / Plane Dot Position Update
    ; Every 32 frames, scroll the trajectory and plane guide
    ; dots on the launch screen. Trajectory moves left (descending),
    ; plane dot moves right (ascending). Reset to $9F at zero.
    ; Skipped during launch phase 5 (MECO approach).
    ;-----------------------------------------------------------
    tya
    and     #$1f
    bne     .skipDotUpdate
    ldx     launchPhase
    cpx     #$05
    beq     .skipDotUpdate
    ldx     #$9f                        ; Wrap-around value for dots
    dec     trajectoryDotPos
    lda     trajectoryDotPos
    bne     .checkPlaneDot
    stx     trajectoryDotPos            ; Reset trajectory dot to $9F
.checkPlaneDot
    and     #$03                        ; Plane dot updates every 4th cycle
    bne     .skipDotUpdate
    dec     planeDotPos
    bne     .skipDotUpdate
    stx     planeDotPos                 ; Reset plane dot to $9F
.skipDotUpdate
    ;-----------------------------------------------------------
    ; Separation Flash & Sound Effect
    ; separationEventTimer counts the SRB/ET separation flash timer.
    ; When it wraps ($FF->$00), triggers the separation sound (#$38)
    ; and sets a visual effect timer (separationFlashTimer = 5 frames).
    ;-----------------------------------------------------------
    lda     separationEventTimer
    beq     .skipSeparationFlash
    inc     separationEventTimer
    bne     .skipSeparationFlash        ; Still counting? Skip
    ldx     #$38                        ; Separation sound effect index
    jsr     queueSoundEffect                       ; Play sound
    lda     #$05
    sta     separationFlashTimer                      ; Set visual flash timer
.skipSeparationFlash
    ;-----------------------------------------------------------
    ; Console Switch Handling
    ; Read SWCHB (console switches). Bit 0 = Game Reset.
    ; Game Reset held: starts countdown sequence.
    ; switchDebounceTimer = switch debounce/hold counter.
    ;-----------------------------------------------------------
    lda     SWCHB
    sta     consoleSwitches
    lsr                                 ; Carry = Reset switch (0=pressed)
    bcs     .resetNotPressed
    ;-----------------------------------------------------------
    ; Game Reset IS pressed
    ; If game not active: trigger launch on first press (cmp #$01).
    ; If game active: hold for 127 frames ($7F) to force restart.
    ;-----------------------------------------------------------
    lda     switchDebounceTimer
    ldx     gameActive
    beq     .checkFirstPress            ; Not active yet
    inc     switchDebounceTimer
    cmp     #$7f                        ; Held long enough to force restart?
    bne     launchMETLogic              ; No: continue normal game
    beq     .activateCountdown          ; Yes: restart
.checkFirstPress
    cmp     #$01
    bne     launchMETLogic              ; Debounce: must see exactly 1
.activateCountdown
    ;-----------------------------------------------------------
    ; Activate Countdown / Restart Game
    ; Sets autopilot mode based on difficulty switches (difficultyLevel).
    ; Per manual: Left Diff A = no autopilot, B = autopilot.
    ; difficultyLevel tracks difficulty level (0-3).
    ;-----------------------------------------------------------
    ldx     #$ff
    ldy     difficultyLevel                      ; Current difficulty setting
    cpy     #$02
    bcs     .skipAutopilotSet
    stx     autopilotMode               ; Enable autopilot (Flight #1/#2)
.skipAutopilotSet
    cpy     #$03
    bcs     .skipTrainingSet
    stx     trainingModeFlag                      ; Set training mode flag
.skipTrainingSet
    jsr     coldStartClearRAM                       ; Reset game state for new flight
    sty     difficultyLevel
    ldy     #$08
    sty     switchDebounceTimer                      ; Reset debounce counter
    sty     enginePowerOn               ; Engines ON
    ldy     #$00
    sty     attractTimer
    sty     screenBlankFlags
    iny
    sty     countdownTimer              ; Start countdown at 1
.jumpToDisplaySetup
    jmp     renderDigits                ; Skip to display setup
    
.resetNotPressed
    ;-----------------------------------------------------------
    ; Game Select Switch (SWCHB bit 1)
    ; Cycles through flight programs when pressed.
    ; Each press changes statusDisplayId to show flight parameters.
    ; Also handles blinking display during pre-launch.
    ;-----------------------------------------------------------
    lsr                                 ; Carry = Select switch
    bcs     .selectNotPressed
    dec     switchDebounceTimer
    bne     launchMETLogic              ; Debounce
    lda     #$30
    sta     switchDebounceTimer                      ; Auto-repeat delay
    lda     errorDisplayFlag                      ; Error flag?
    beq     .noError
    lda     #$00
    sta     errorDisplayFlag                      ; Clear error
    sta     abortCode
    beq     .checkSoundTimer
.noError
    lda     statusDisplayId
    ldx     enginePowerOn
    bne     .cycleFlightDisplay         ; Engine on: cycle flight displays
    cmp     #$0b                        ; At flight program screen?
    beq     .advanceDifficulty
    ldx     #$0b
    bne     .setDisplay
.advanceDifficulty
    inc     difficultyLevel                      ; Next difficulty level
    lda     difficultyLevel
    cmp     #$04                        ; Wrap at 4?
    bne     .checkSoundTimer
    inx                                 ; X=1 (was 0)
    stx     difficultyLevel                      ; Reset to level 1
    bne     .checkSoundTimer
.cycleFlightDisplay
    adc     #$02                        ; Skip by 2 in display cycle
    tax
    cmp     #$0b
    bcc     .setDisplay
    ldx     #$01                        ; Wrap to first display
.setDisplay
    stx     statusDisplayId
.checkSoundTimer
    lda     soundEffectId                      ; Sound effect timer
    bne     launchMETLogic
    lda     #$34                        ; Queue button click sound
    sta     soundEffectId
    bne     launchMETLogic
.selectNotPressed
resetDebounceTimer
    lda     #$01
    sta     switchDebounceTimer                      ; Reset debounce
;-----------------------------------------------------------
;      Mission Elapsed Time (MET) Clock
;      Counts real time in BCD format (metHigh:metLow).
;      Timer ticks every 59 frames ($3B). Drives all launch events.
;      metHigh=$A0 = countdown mode (counts DOWN from T-16).
;      metHigh=$00 = count UP after liftoff (MET positive).
;-----------------------------------------------------------
launchMETLogic
    lda     gameActive
    beq     .jumpToDisplaySetup         ; Not active? Skip to display
    dec     metFrameCounter
    bne     checkLaunchEvent            ; Not time to tick? Check events only
    sed                                 ; BCD mode for time arithmetic
    lda     #$3b
    sta     metFrameCounter             ; Reset frame counter (59 frames/tick)
    ;-----------------------------------------------------------
    ; Countdown Phase (metHigh = $A0 = "T-minus")
    ; Counts down metLow. At T-0: checks thrust arrow position
    ; to decide if launch proceeds or scrubs.
    ;-----------------------------------------------------------
    lda     metHigh
    cmp     #$a0                        ; In countdown?
    bne     incrementMET                ; No: count up (MET positive)
    ldx     #$04                        ; Countdown beep sound
    jsr     queueSoundEffect
    ldx     #$07                        ; Display: countdown screen
    lda     metLow
    sbc     #$01                        ; Decrement countdown seconds
    sta     metLow
    bne     .setCountdownDisplay        ; Not T-0 yet
    ;-----------------------------------------------------------
    ; T-0: Launch Go/No-Go Decision
    ; Thrust arrow must be at position >= $75 to proceed.
    ; Otherwise: SCRUB (abort countdown).
    ;-----------------------------------------------------------
    ldx     thrustArrowX
    cpx     #$75                        ; Thrust arrow in valid zone?
    bcc     handleLaunchScrub           ; No: scrub the launch!
    sta     metHigh                     ; Clear metHigh (now counting up)
    ldx     #$03                        ; Display: liftoff screen
.setCountdownDisplay
    stx     statusDisplayId
    bne     .exitMETLogic
handleLaunchScrub
    ;-----------------------------------------------------------
    ; Launch Scrub - Reset to pre-launch state
    ; Resets MET to T-16, clears launch phase, plays scrub sound.
    ;-----------------------------------------------------------
    sta     launchEventIndex            ; A=0: reset event index
    sta     launchPhase                 ; Clear launch phase
    lda     #$b8
    sta     metFrameCounter             ; Long delay before next tick
    ldx     #$16
    stx     metLow                      ; Reset to T-16 seconds
    ldx     #$41                        ; Scrub sound effect
    stx     soundEffectId
    ldx     #$1d                        ; Display: scrub message
    bne     .setCountdownDisplay
incrementMET
    ;-----------------------------------------------------------
    ; Positive MET: Count Up After Liftoff
    ; Increments metLow:metHigh in BCD. Triggers key events:
    ;   MET 0:03 = launch phase 5 (SRB separation warning)
    ;   MET 0:25 = ET separation flash + sound ($68) + phase 7
    ;-----------------------------------------------------------
    lda     metLow
    adc     #$01
    sta     metLow
    lda     metHigh
    adc     #$00
    sta     metHigh
    bne     .exitMETLogic               ; metHigh>0: past early events
    ldx     #$01                        ; Default: launchPhase=1
    lda     metLow
    cmp     #$25                        ; MET 0:25 = ET separation
    bne     .checkPhase5
    lda     #$07                        ; ET separation flash timer
    sta     separationFlashTimer
    lda     #$68                        ; ET separation sound
    sta     soundEffectId
    bne     .setLaunchPhase
.checkPhase5
    cmp     #$03                        ; MET 0:03 = SRB sep warning
    bne     .exitMETLogic
    ldx     #$05                        ; Launch phase 5
.setLaunchPhase
    stx     launchPhase
.exitMETLogic
    cld                                 ; Exit BCD mode
    ;-----------------------------------------------------------
    ; Launch Event Dispatcher
    ; Compares MET against launchEventTimings table to trigger
    ; automated thrust commands. Events change C arrow direction.
    ; Once launchEventIndex >= 3, transition to ascent logic.
    ;-----------------------------------------------------------
    lda     flightPhase
    cmp     #$02
    bcs     .skipLaunchEvents           ; Already in orbit? Skip
    ldx     launchEventIndex
    lda     launchEventTimings,x        ; Expected MET for next event
    cmp     metLow
checkLaunchEvent
    bne     .noEventMatch
    inc     launchEventIndex            ; Advance to next event
    lda     launchEventParams,x         ; Get event parameters
    and     #$0f                        ; Lower nibble = thrust command
    sta     autoThrustCommand           ; Set C arrow movement direction
    cpx     #$00                        ; First event (liftoff)?
    bne     .noEventMatch
    inx
    stx     launchPhase                 ; Start launch phase 1
    inc     thrustArrowX                ; Nudge T arrow right
.noEventMatch
    lda     launchEventIndex
    cmp     #$03                        ; All events triggered?
    bcs     ascentLogic                 ; Yes: enter ascent phase
.skipLaunchEvents
    jmp     .orbitPhaseDispatch
    
;-----------------------------------------------------------
;      Ascent Logic
;      Active after all 3 launch events triggered.
;      Controls speed increase rate based on thrust arrow position,
;      updates trajectory/plane dots, and handles joystick for
;      pitch/yaw corrections during powered ascent.
;-----------------------------------------------------------
ascentLogic
    lda     thrustArrowX
    cmp     #$10                        ; Thrust arrow above minimum?
    bcs     .doAscent
    ldx     launchPhase
    beq     .skipLaunchEvents           ; No launch phase = skip ascent
.doAscent
    ;-----------------------------------------------------------
    ; Calculate Speed Increment Rate
    ; Higher thrust arrow position = faster speed increase.
    ; T arrow pos / 32 maps to acceleration rate.
    ;-----------------------------------------------------------
    lsr
    lsr
    lsr
    lsr
    lsr                                 ; thrustArrowX / 32
    clc
    adc     #$0b
    eor     #$0f                        ; Invert for rate calculation
    adc     #$14
    dec     altitudeUpdateTimer                      ; Altitude change timer
    bpl     .skipSpeedInc
    sta     altitudeUpdateTimer                      ; Reset timer with calculated rate
    jsr     incSpeedAndDisplay                       ; Increment altitude
.skipSpeedInc
    ;-----------------------------------------------------------
    ; Speed-Dependent Update Rate
    ; Controls how often trajectory/plane dots and speed update.
    ; Slower at low speed, faster at high speed:
    ;   speed < $20: rate=5, < $80: rate=3, < $D8: rate=1, else: rate=0
    ;-----------------------------------------------------------
    ldx     speed
    lda     #$05
    cpx     #$20
    bcc     .setUpdateRate
    lda     #$03
.setUpdateRate
    cpx     #$80
    bcc     .applyUpdateRate
    lda     #$01
.applyUpdateRate
    cpx     #$d8
    bcc     .doSpeedUpdate
    lda     #$00                        ; Max speed: update every frame
.doSpeedUpdate
    dec     speedUpdateTimer                      ; Speed update countdown
    bpl     .checkWindShear
    sta     speedUpdateTimer                      ; Reset with rate value
    ;-----------------------------------------------------------
    ; Update Trajectory & Plane Dots During Ascent
    ; Trajectory dot converges left (closer to target orbit line),
    ; plane dot drifts right. Speed increases via increaseSpeed.
    ;-----------------------------------------------------------
    lda     trajectoryDotPos
    sbc     #$04                        ; Move trajectory dot left
    bcc     .updatePlaneDot             ; Don't go negative
    sta     trajectoryDotPos
.updatePlaneDot
    lda     planeDotPos
    adc     #$03                        ; Move plane dot right
    cmp     #$a0
    bcs     .incSpeed                   ; Clamp at $A0
    sta     planeDotPos
.incSpeed
    jsr     increaseSpeed                       ; Increase shuttle speed
.checkWindShear
    ;-----------------------------------------------------------
    ; Random Wind Shear During Ascent
    ; Every 32 frames, random chance of Y-axis perturbation.
    ; Simulates atmospheric turbulence during ascent.
    ;-----------------------------------------------------------
    lda     frameCounter
    and     #$1f
    bne     .doneAscentInput
    lda     rngSeed
    cmp     #$b0                        ; 70% chance: no wind
    bcc     handleJoystickInput
    lsr
    bcs     .windDown
    inc     yAxisPlane                  ; Wind pushes up
    .byte   $2c ;bit                ;4-5 =  22 *  ; Skip next instruction
.windDown
    dec     yAxisPlane                  ; Wind pushes down
handleJoystickInput
    ;-----------------------------------------------------------
    ; Joystick Pitch/Yaw Control (During Ascent)
    ; Up/Down = adjust yAxisPlane (pitch)
    ; Left/Right = adjust planeCorrection (roll/yaw)
    ; Per manual: "Keep the plane indicator on its guide line."
    ;-----------------------------------------------------------
    lda     SWCHA
    asl                                 ; Bit 7 -> carry: P1 Up
    bcs     .noJoyUp
    inc     yAxisPlane                  ; Joystick Up: pitch up
.noJoyUp
    asl                                 ; Bit 6 -> carry: P1 Down
    bcs     .noJoyDown
    dec     yAxisPlane                  ; Joystick Down: pitch down
.noJoyDown
    ldy     planeCorrection
    asl                                 ; Bit 5 -> carry: P1 Left
    bcs     .noJoyLeft
    cpy     #$21                        ; Max left correction?
    beq     .noJoyLeft
    inc     planeCorrection             ; Joystick Left: roll left
.noJoyLeft
    asl                                 ; Bit 4 -> carry: P1 Right
    bcs     .doneAscentInput
    cpy     #$00                        ; Min correction?
    beq     .doneAscentInput
    dec     planeCorrection             ; Joystick Right: roll right
.doneAscentInput
    cld
    jmp     .afterOrbitLogic
    
;-----------------------------------------------------------
;      Orbit Phase Dispatcher
;      Routes to appropriate handler based on flightPhase:
;        3 = deorbit/reentry speed handling
;        4 = docking maneuvers (OMS/RCS)
;        other = general orbit/joystick input
;-----------------------------------------------------------
.orbitPhaseDispatch
    lda     flightPhase
    cmp     #$03
    beq     .checkWindShear             ; Phase 3: reuse speed update path
    cmp     #$04
    bne     .checkOrbitInput
    ;-----------------------------------------------------------
    ; Docking Phase (flightPhase=4) - Fire Button Display
    ; Fire button shows OMS fuel gauge (screen $03),
    ; otherwise shows docking status (screen $17).
    ;-----------------------------------------------------------
    ldx     #$17                        ; Default: docking display
    bit     INPT4
    bmi     .setDockDisplay
    ldx     #$03                        ; Fire pressed: fuel gauge
.setDockDisplay
    stx     statusDisplayId
    ldy     #$01
    lda     SWCHA
    and     #$f0                        ; Check all joystick directions
    cmp     #$f0                        ; No input?
    beq     .resetInputDelay            ; Yes: reset delay counter
    dec     inputDelayTimer                      ; Input delay countdown
.checkOrbitInput
    ;-----------------------------------------------------------
    ; Orbital Maneuvering (General Orbit Input)
    ; Processes joystick directional input with delay counter.
    ; Wraps yAxisPlane around 0/$9F boundary (circular orbit).
    ; Left/Right = adjust pitchValue for OMS targeting.
    ;-----------------------------------------------------------
    bne     .afterOrbitLogic
    ldx     #$04
    stx     inputDelayTimer                      ; Reset input delay
    tax
    txa
    asl                                 ; Shift through joystick bits
    tax
    bcs     .noOrbitDown
    ;--- Joystick Down: Decrease Y (descend in orbit) ---
    dec     yAxisPlane
    lda     yAxisPlane
    cmp     #$ff                        ; Wrapped below 0?
    bne     .checkOrbitLowBound
    lda     #$9f                        ; Wrap to top of orbit circle
    sta     yAxisPlane
.checkOrbitLowBound
    cmp     #$4f                        ; Hit center boundary?
    bne     .noOrbitDown
    inc     yAxisPlane                  ; Bounce back
.noOrbitDown
    txa
    asl
    tax
    bcs     .noOrbitUp
    ;--- Joystick Up: Increase Y (ascend in orbit) ---
    inc     yAxisPlane
    lda     yAxisPlane
    cmp     #$a0                        ; Wrapped above $9F?
    bne     .checkOrbitHighBound
    lda     #$00                        ; Wrap to bottom of orbit circle
    sta     yAxisPlane
.checkOrbitHighBound
    cmp     #$50                        ; Hit center boundary?
    bne     .noOrbitUp
    dec     yAxisPlane                  ; Bounce back
.noOrbitUp
    ;-----------------------------------------------------------
    ; Pitch Control (Left/Right in Orbit)
    ; Adjusts pitchValue for OMS engine targeting.
    ; Blocked if landingPitchSuccess set (landing phase) or during deorbit burn ($98).
    ;-----------------------------------------------------------
    lda     landingPitchSuccess
    bne     .afterOrbitLogic            ; Landing: no pitch control
    lda     soundEffectId
    cmp     #$98                        ; Deorbit burn sound active?
    beq     .afterOrbitLogic
    txa
    asl
    tax
    bcs     .noOrbitLeft
    lda     pitchValue
    cmp     #$10                        ; Max pitch?
    beq     .afterOrbitLogic
    inc     pitchValue                  ; Left: increase pitch
.noOrbitLeft
    txa
    asl
    bcs     .afterOrbitLogic
    lda     pitchValue
    beq     .afterOrbitLogic            ; Min pitch?
    dec     pitchValue                  ; Right: decrease pitch
    bpl     .afterOrbitLogic
.resetInputDelay
    sty     inputDelayTimer                      ; Reset joystick delay (Y=1)
.afterOrbitLogic
    ;-----------------------------------------------------------
    ; Docking Alignment Check
    ; Checks all conditions for successful satellite docking:
    ;   - yAxisPlane = 0 (aligned vertically)
    ;   - pitchValue = 0 (aligned in pitch)
    ;   - targetHorizPos between $47-$4F (horizontal proximity)
    ;   - shuttleOrbitalPos = satelliteOrbitalPos (matching orbital position)
    ;   - altitude = $D2 (correct orbit altitude: 210nm)
    ; Per manual: "Align cross-hairs with satellite."
    ;-----------------------------------------------------------
    lda     yAxisPlane
    bne     .checkDockingTimer          ; Not aligned vertically
    lda     pitchValue
    bne     .checkDockingTimer          ; Not aligned in pitch
    lda     targetHorizPos
    cmp     #$50
    bcs     .checkDockingTimer          ; Too far right
    cmp     #$47
    bcc     .checkDockingTimer          ; Too far left
    lda     shuttleOrbitalPos
    sbc     satelliteOrbitalPos
    bne     .checkDockingTimer          ; Orbital positions don't match
    lda     altitude
    cmp     #$d2                        ; At target orbit altitude?
    bne     .checkDockingTimer
    ;-----------------------------------------------------------
    ; Docking Approach Active
    ; dockingProgress = docking progress counter.
    ; Counts up to $80 = successful dock.
    ; At $80: refuel, decrement orbital position, play sound.
    ;-----------------------------------------------------------
.dockingProgress
    inc     dockingProgress
    bne     .checkDockComplete
    dec     dockingProgress                      ; Clamp at $FF
.checkDockComplete
    lda     dockingProgress
    cmp     #$80                        ; Docking complete?
    bne     .checkDockPhase
    ;-----------------------------------------------------------
    ; Successful Docking!
    ; Refuel from satellite, track total dockings (dockingCount).
    ; Fuel bonus depends on docking count (fuelRefillTable table).
    ; Per manual: "Maximum of 6 satellite dockings possible."
    ;-----------------------------------------------------------
    ldx     #$24                        ; Docking sound
    stx     soundEffectId
    dec     satelliteOrbitalPos                      ; Move satellite to new position
    ldx     dockingCount                      ; Docking count
    cpx     #$06                        ; Max 6 dockings
    beq     .doRefuel
    inc     dockingCount                      ; Increment docking count
.doRefuel
    sed
    clc
    lda     missionScore                      ; Score counter
    adc     #$01
    sta     missionScore
    lda     fuelHigh
    adc     fuelRefillTable,x                     ; Fuel bonus per docking
    sta     fuelHigh
    bcc     .refuelDone
    lda     #$99                        ; Cap fuel at 99xx
    sta     fuelHigh
.refuelDone
    cld
.checkDockPhase
    bcc     .afterDockingCheck          ; Not docked yet
    ldx     #$05                        ; Display: docked status
    cmp     #$fe                        ; dockingProgress = post-dock phase?
    beq     .setDockDisplay2
    cmp     #$ff                        ; Fully complete?
    beq     .afterDockingCheck
    ldx     #$1f                        ; Display: satellite refuel info
.setDockDisplay2
    stx     statusDisplayId
    bne     .jumpToOmsBurn
    ;-----------------------------------------------------------
    ; Check Docking Timer State
    ; If dockingProgress >= $80 but < $FF, continue docking sequence.
    ; If < $80, reset counter (alignment lost).
    ;-----------------------------------------------------------
.checkDockingTimer
    lda     dockingProgress
    cmp     #$80
    bcc     .resetDockCounter
    cmp     #$ff
    bne     .dockingProgress            ; Continue docking sequence
    .byte   $2c ;bit                ;4-2 =  13 *  ; Skip next lda
.resetDockCounter
    lda     #$00
    sta     dockingProgress
.afterDockingCheck
    ;-----------------------------------------------------------
    ; Autopilot Station-Keeping (Docking Phase Only)
    ; In manual docking mode, automatically nudges yAxisPlane
    ; toward center ($50) at rate determined by difficulty/docking count.
    ; Only active when: flightPhase=4, altitude>=$0B, no autopilot,
    ; and joystick mostly centered (>= $CF).
    ;-----------------------------------------------------------
    lda     flightPhase
    cmp     #$04
    bne     .checkAltitudeAbort
    lda     altitude
    cmp     #$0b
    bcc     .checkAltitudeAbort         ; Too low for station keeping
    ldx     autopilotMode
    bmi     .checkAltitudeAbort         ; Autopilot handles its own docking
    lda     SWCHA
    cmp     #$cf                        ; Joystick mostly centered?
    bcc     .checkAltitudeAbort
    ldx     dockingCount                      ; Use docking count as rate index
    cpx     #$06
    beq     .useStationRate             ; Max dockings: use table directly
    ldx     difficultyLevel                      ; Otherwise use difficulty level
.useStationRate
    lda     frameCounter
    and     satelliteSectionPad,x                     ; Rate mask from table
    bne     .checkAltitudeAbort         ; Skip frames based on difficulty
    lda     yAxisPlane
    cmp     #$51                        ; Below center?
    bcs     .stationDown
    cmp     #$4f                        ; Above center?
    bcs     .checkAltitudeAbort         ; At center: do nothing
    inc     yAxisPlane                  ; Nudge toward center
    .byte   $2c ;bit                ;4-5 =  24 *  ; Skip next dec
.stationDown
    dec     yAxisPlane                  ; Nudge toward center
.checkAltitudeAbort
    ;-----------------------------------------------------------
    ; Altitude/Speed Abort Check
    ; altitude=$FF = immediate abort (crashed/lost).
    ; flightPhase=2 (orbit): check altitude>=$C3 and speed=$A9
    ; for proper orbital insertion, else abort.
    ;-----------------------------------------------------------
    ldx     #$75                        ; Abort code: altitude failure
    lda     altitude
    cmp     #$ff                        ; Altitude overflow?
    beq     .doAbort
    ldx     flightPhase
    cpx     #$02
    beq     .checkOrbitQuality
    jmp     .deorbitLogic               ; Not in orbit insertion: skip to deorbit
    
.jumpToOmsBurn
    jmp     .omsBurnSetup
    
.checkOrbitQuality
    ldx     #$70                        ; Abort code: orbit too low
    cmp     #$c3                        ; Minimum orbit altitude
    bcc     .doAbort
    ldx     #$80                        ; Abort code: speed mismatch
    lda     speed
    cmp     #$a9                        ; Required orbital speed
    bne     .orbitOmsInput
.doAbort
    jsr     abortMission                ; Abort! (abortMission)
;-----------------------------------------------------------
;      OMS (Orbital Maneuvering System) Input
;      Handles joystick-driven OMS burns for orbit changes.
;      Each burn costs fuel (rate depends on burn type).
;      X register tracks joystick direction bits for dispatch.
;      Per manual: "Fire OMS engines to change orbit."
;-----------------------------------------------------------
.orbitOmsInput
    lda     SWCHA
    and     #$f0                        ; Joystick directions
    cmp     #$f0                        ; No input?
    beq     .jumpToOmsBurn              ; Skip to OMS burn state handler
    dec     inputDelayTimer                      ; Input rate limiter
    bne     .endOmsInput
    ldx     #$18
    stx     inputDelayTimer                      ; Reset rate limiter (24 frames)
    stx     omsBurnActive                      ; Mark OMS burn active
    jsr     queueSoundEffect                       ; Play OMS burn sound
    ;-----------------------------------------------------------
    ; Calculate Fuel Cost for OMS Burn
    ; Higher joystick nibble inverted = burn intensity.
    ; At display $15 (satellite approach), reduced fuel cost.
    ;-----------------------------------------------------------
    lsr
    lsr
    lsr
    lsr
    eor     #$0f                        ; Invert for cost
    tax
    lda     #$09                        ; Base fuel cost
    ldy     statusDisplayId
    cpy     #$15                        ; Near satellite approach?
    bne     .doOmsBurn
    lda     #$01                        ; Reduced cost near satellite
.doOmsBurn
    jsr     subtractFuel                ; Burn fuel
    lda     gameActive
    beq     .jumpToOmsBurn              ; Out of fuel? Stop
    ;-----------------------------------------------------------
    ; OMS Burn Direction Dispatch
    ; Autopilot: always burn via direction bits.
    ; Manual: check console switches for MECO override ($48).
    ;-----------------------------------------------------------
    bit     autopilotMode
    bmi     .dispatchOmsBurn            ; Autopilot: always process
    lda     consoleSwitches
    and     #$48                        ; Color/BW + Left Diff
    bne     .manualOmsBurn              ; Manual controls active
.dispatchOmsBurn
    txa
    lsr
    tay
    bcc     .checkOmsUp
    ;--- OMS Down: decrease altitude ---
    bit     INPT4
    bmi     .omsDownNoFire
    jsr     decreaseSpeed                       ; Fire+Down: decrease speed
    jmp     .setOmsDisplay
    
.omsDownNoFire
    ldx     #$0d
    jsr     incSpeedAndDisplay                       ; Increase altitude (retrograde)
.checkOmsUp
    tya
    lsr
    tay
    bcc     .checkOmsFireOnly
    ;--- OMS Up: increase altitude ---
    lda     #$00
    sta     omsBurnActive                      ; Clear burn flag
    bit     INPT4
    bmi     .omsUpNoFire
    jsr     increaseSpeed                       ; Fire+Up: increase speed
.setOmsDisplay
    ldx     #$11                        ; Display: OMS burn status
    bne     .storeOmsDisplay
.omsUpNoFire
    ldx     #$0d
    jsr     decSpeedAndDisplay                       ; Decrease altitude (prograde)
.checkOmsFireOnly
    ;--- Fire button only (no up/down): Y-axis adjustment ---
    bit     INPT4
    bpl     .endOmsInput                ; Fire not pressed: done
    tya
    lsr
    tay
    bcc     .checkOmsRight
    ;--- OMS Left: pitch/yaw + Y up ---
    lda     #$04
    inc     yAxisPlane
    jsr     setMovementAndPerturb                       ; Store movement flags
.checkOmsRight
    tya
    lsr
    bcc     .storeOmsDisplay
    ;--- OMS Right: pitch/yaw + Y down ---
    dec     yAxisPlane
    lda     #$08
    jsr     setMovementAndPerturb                       ; Store movement flags
.storeOmsDisplay
    stx     statusDisplayId
.endOmsInput
    jmp     .deorbitLogic
    
;-----------------------------------------------------------
;      Manual OMS Burns (Console Switches Active)
;      When Color/BW or Left Diff switches are set,
;      joystick controls pitch value directly instead of
;      orbital position. Used for deorbit burn targeting.
;-----------------------------------------------------------
;-----------------------------------------------------------
;      Manual OMS Burns (Console Switches Active)
;      When Color/BW or Left Diff switches are set,
;      joystick controls pitch value directly instead of
;      orbital position. Used for deorbit burn targeting.
;-----------------------------------------------------------
.manualOmsBurn
    txa
    lsr
    tay
    bcc     .manualUp
    ;--- Down: decrease pitchValue ---
    lda     pitchValue
    dec     pitchValue
    bpl     .storePitchDir
    sta     pitchValue                  ; Clamp at 0
.storePitchDir
    ldx     #$01                        ; Direction: down
.setManualDisplay
    lda     #$13                        ; Display: manual OMS
    bne     .storeManualOms
.manualUp
    tya
    lsr
    tay
    bcc     .manualLeft
    ;--- Up: increase pitchValue ---
    lda     pitchValue
    cmp     #$10                        ; Max pitch
    beq     .cappedPitch
    inc     pitchValue
.cappedPitch
    ldx     #$02                        ; Direction: up
    bne     .setManualDisplay
.manualLeft
    tya
    lsr
    tay
    bcc     .manualRight
    ;--- Left: decrease omsYaw (OMS yaw) ---
    dec     omsYaw
    ldx     #$04
    bne     .setManualRate
.manualRight
    tya
    lsr
    bcc     .omsBurnSetup
    ;--- Right: increase omsYaw (OMS yaw) ---
    inc     omsYaw
    ldx     #$08
.setManualRate
    lda     #$05
    sta     inputDelayTimer                      ; Shorter input delay for pitch/yaw
    lda     #$15                        ; Display: OMS targeting
.storeManualOms
    sta     statusDisplayId
    stx     movementFlags                      ; Store movement direction flags
    bne     .omsBurnProcess
;-----------------------------------------------------------
;      OMS Burn State Handler
;      When no joystick input, calculates orbital drift from
;      omsYaw (OMS pitch). Triggers deorbit burn when console
;      switches active + fire button pressed.
;-----------------------------------------------------------
.omsBurnSetup
    lda     omsYaw
    clc
    adc     #$10
    lsr
    lsr
    lsr
    lsr
    lsr                                 ; Convert omsYaw to table index
    tax
    lda     soundEffectId
    cmp     #$58                        ; Deorbit burn sound active?
    beq     .checkDeorbitBurn
    lda     difficultyMultTable,x                     ; Get drift direction from table
    sta     movementFlags
    ldy     #$00
    sty     orbitalSubCounter
    sty     omsBurnActive                      ; Clear burn flag
    lda     soundEffectId
    cmp     #$18
    bne     .resetBurnInput
    sty     soundEffectId                      ; Clear sound timer
.resetBurnInput
    iny
    sty     inputDelayTimer                      ; Reset input delay
.checkDeorbitBurn
    ;-----------------------------------------------------------
    ; Deorbit Burn Trigger
    ; Requires: console switches ($48) active + fire button pressed.
    ; Initiates deorbit burn sound ($58) and sets burn active flag.
    ; Per manual: "Flip switches and press fire to deorbit."
    ;-----------------------------------------------------------
    lda     consoleSwitches
    and     #$48
    beq     .deorbitLogic               ; Switches not set: skip
    bit     INPT4
    bmi     .deorbitLogic               ; Fire not pressed: skip
    lda     #$fe
    sta     soundSequenceIndex                      ; Deorbit burn flag
    lda     #$58
    sta     soundEffectId                      ; Deorbit burn sound
    sta     omsBurnActive                      ; Burn active
    lda     frameCounter
    and     #$0f
.omsBurnProcess
    bne     .deorbitLogic               ; Rate-limit burn processing
    ;-----------------------------------------------------------
    ; Fuel Cost During Deorbit/OMS Burn
    ; pitchValue = 7 optimal: costs 1 fuel unit.
    ; Otherwise: costs 2 fuel units (waste!).
    ;-----------------------------------------------------------
    lda     #$01
    ldy     pitchValue
    cpy     #$07                        ; Optimal pitch for deorbit?
    beq     .doBurnFuel
    lda     #$02                        ; Extra fuel if not optimal
.doBurnFuel
    jsr     subtractFuel
    cpx     #$02
    bcs     .burnResult2Plus
.burnIncAlt
    jsr     incSpeedAndDisplay                       ; Increase altitude
    cpy     #$08
    bcs     .deorbitLogic
    bcc     .burnDecSpeed
.burnResult2Plus
    cpx     #$02
    bne     .burnResult3Plus
    dec     yAxisPlane                  ; Pitch down during burn
    lda     #$05
    bne     .storeBurnMove
.burnResult3Plus
    cpx     #$06
    bcs     .burnResult6Plus
    jsr     decSpeedAndDisplay                       ; Decrease altitude
    cpy     #$07
    bcs     .burnIncSpeed
    bcc     .deorbitLogic
.burnResult6Plus
    cpx     #$07
    beq     .burnIncAlt
    inc     yAxisPlane                  ; Pitch up during burn
    lda     #$09
.storeBurnMove
    sta     movementFlags                      ; Movement direction
    ldx     #$0f                        ; Display: burn status
    stx     statusDisplayId
    cpy     #$07
    beq     .deorbitLogic               ; Optimal: no speed change
    bcc     .burnDecSpeed
.burnIncSpeed
    jsr     increaseSpeed                       ; Increase speed
    jmp     .deorbitLogic
    
.burnDecSpeed
    jsr     decreaseSpeed                       ; Decrease speed
;-----------------------------------------------------------
;      Deorbit / Reentry Transition Logic
;      Checks if conditions met to begin deorbit:
;        - flightPhase >= 2 (in orbit or beyond)
;        - Speed < $BF (below orbital velocity)
;          (Autopilot with dockings overrides speed check)
;        - Altitude < $D7 (below stable orbit)
;      At altitude $C8: triggers transition to flightPhase 3 (reentry).
;      Per manual: "Deorbit at correct angle for safe reentry."
;-----------------------------------------------------------
.deorbitLogic
    lda     flightPhase
    cmp     #$02
    bcs     .checkDeorbitConditions
.skipToFlightEffects
    jmp     .flightEffects              ; Not in orbit yet
    
.checkDeorbitConditions
    bit     autopilotMode
    bpl     .checkSpeed
    lda     dockingCount                      ; Autopilot with dockings?
    bne     .checkAltitude              ; Skip speed check
.checkSpeed
    lda     speed
    cmp     #$bf                        ; Below deorbit speed?
    bcs     .skipToFlightEffects        ; Too fast: still in stable orbit
.checkAltitude
    lda     altitude
    cmp     #$d7                        ; Below deorbit altitude?
    bcs     .skipToFlightEffects        ; Too high: still stable
    sta     omsBurnActive
    cmp     #$c8                        ; Transition altitude?
    bne     .checkReentryState
    ;-----------------------------------------------------------
    ; Reentry Transition (altitude hits $C8)
    ; Initialize reentry state: reset Y-axis, set flightPhase=3,
    ; switch to reentry screen. Evaluate deorbit quality based
    ; on omsYaw (OMS pitch) and pitchValue for abort check.
    ;-----------------------------------------------------------
    dec     altitude                    ; Drop below $C8 threshold
    ldy     #$00
    sty     yAxisPlane                  ; Level out
    sty     movementFlags                      ; Clear movement flags
    ldx     #$03
    stx     flightPhase                 ; Phase 3: reentry
    stx     statusDisplayId
    stx     currentScreenId             ; Switch to reentry screen
    ldx     #$09
    stx     planeCorrection             ; Center correction
    stx     trajectoryThreshold
    ;-----------------------------------------------------------
    ; Deorbit Quality Check
    ; omsYaw != 0: bad yaw angle -> abort ($65)
    ; pitchValue > $0D: too steep -> abort ($55 if not $0D)
    ; pitchValue = $0D: perfect angle -> don't abort
    ; pitchValue < $0D: abort ($60 = too shallow)
    ;-----------------------------------------------------------
    ldx     #$65                        ; Abort: bad yaw
    lda     omsYaw
    bne     .deorbitAbort
    ldx     #$55                        ; Abort: too steep
    lda     pitchValue
    cmp     #$0d                        ; Perfect reentry angle?
    beq     .checkReentryState          ; Yes: proceed
    bcs     .deorbitAbort               ; Too steep
    ldx     #$60                        ; Abort: too shallow
.deorbitAbort
    jsr     abortMission                       ; Process abort
;-----------------------------------------------------------
;      Reentry Descent Logic
;      Controls the shuttle's descent through atmosphere.
;      reentryContactState = reentry heating state (non-zero = heating active).
;      movementFlags movement flags updated for reentry attitude.
;      Speed decreases as altitude drops. Visual heating effects
;      triggered at specific altitude bands.
;-----------------------------------------------------------
.checkReentryState
    ldx     reentryContactState                      ; Reentry heating state
    beq     .doReentryDescent
.jumpToReentryEffects
    jmp     .reentryHeatEffects
    
.doReentryDescent
    ;--- Set reentry movement flags (keep nose-up attitude) ---
    lda     movementFlags
    and     #$0c                        ; Preserve left/right bits
    ora     #$01                        ; Add forward movement
    sta     movementFlags
    ;-----------------------------------------------------------
    ; Descent Rate Calculation
    ; In landing screen ($04): rate based on pitchValue + switch state.
    ; Otherwise: rate from table (descentRateTable) indexed by descentRateIndex.
    ;-----------------------------------------------------------
    lda     currentScreenId
    cmp     #$04                        ; Landing screen?
    bne     .useDescentTable
    lda     pitchValue
    adc     #$03                        ; Pitch affects descent rate
    ldx     consoleSwitches
    bpl     .setDescentRate             ; Check switch state
    lsr
    bpl     .setDescentRate
.useDescentTable
    ldx     descentRateIndex                      ; Descent rate table index
    lda     descentRateTable,x
    sta     descentRate                      ; Store descent rate
.setDescentRate
    inc     descentFrameCounter                      ; Descent frame counter
    cmp     descentFrameCounter                      ; Reached descent interval?
    bcs     .jumpToReentryEffects       ; Not yet: skip
    lda     #$00
    sta     descentFrameCounter                      ; Reset counter
    jsr     decreaseSpeed                       ; Decrease speed
    ;-----------------------------------------------------------
    ; Altitude Band Checks During Reentry
    ; $A7+: no heating effects (too high)
    ; $78-$A6: ionization heating zone (heatEffectTimer = heat timer)
    ; $1E-$77: lower atmosphere
    ; $1E exactly: transition to landing phase (flightPhase=4)
    ;-----------------------------------------------------------
    lda     altitude
    cmp     #$a7
    bcs     .noHeatingEffect            ; Above heating zone
    cmp     #$78
    bcc     .checkLandingTransition     ; Below heating zone
    ;--- Ionization heating band ($78-$A6) ---
    inc     heatEffectTimer                      ; Increase heat effect timer
    bne     .noHeatingEffect
    dec     heatEffectTimer                      ; Clamp at $FF
    bne     .noHeatingEffect
.checkLandingTransition
    cmp     #$1e                        ; Landing transition altitude?
    bne     .checkHeatDecay
    ;-----------------------------------------------------------
    ; Landing Phase Transition (altitude = $1E)
    ; Switch to flightPhase 4 (landing), set up landing screen,
    ; initialize landing parameters. Autopilot sets specific pitch.
    ; Per manual: "Line up runway for final approach."
    ;-----------------------------------------------------------
    inc     flightPhase                 ; Phase 4: landing
    inc     currentScreenId             ; Landing screen
    ldx     #$02
    stx     landingDisplayMode                      ; Landing display mode
    lda     #$40
    sta     starfieldScrollY                      ; Landing approach timer
    dec     altitude                    ; Drop below $1E
    lda     #$00
    sta     atmosphereDensity                      ; Clear atmosphere timer
    bit     autopilotMode
    bpl     .setManualLanding
    stx     pitchValue                  ; Autopilot: set pitch to 2
    bit     joystickDetected                      ; Training mode?
    bpl     .setLandingY
    lda     #$27                        ; Training: start at Y=$27
    .byte   $2c ;bit                ;4-2 =  51 *  ; Skip next lda
.setManualLanding
    lda     #$4f                        ; Manual: start at Y=$4F
.setLandingY
    sta     yAxisPlane
    ldx     #$35                        ; Abort: missed approach
    lda     soundEffectId
    cmp     #$41                        ; Abort sound playing?
    beq     .deorbitAbort               ; Yes: abort
.checkHeatDecay
    ;--- Heat Effect Decay ---
    lda     heatEffectTimer
    beq     .noHeatingEffect
    dec     heatEffectTimer                      ; Cool down
.noHeatingEffect
    ;-----------------------------------------------------------
    ; Low Atmosphere Effects (altitude $1E-$30)
    ; atmosphereDensity = atmosphere density counter for visual effects.
    ;-----------------------------------------------------------
    lda     altitude
    cmp     #$30
    bcs     .doDescentSpeedDec
    cmp     #$1e
    bcc     .doDescentSpeedDec
    inc     atmosphereDensity                      ; Atmosphere density increases
    bne     .doDescentSpeedDec
    dec     atmosphereDensity                      ; Clamp
.doDescentSpeedDec
    dec     descentSpeedTimer                      ; Descent speed timer
    bpl     .reentryHeatEffects
    lda     #$0a
    sta     descentSpeedTimer                      ; Reset speed decrease interval
    jsr     decSpeedAndDisplay                       ; Decrease altitude
;-----------------------------------------------------------
;      Reentry Heat Effects & Landing Approach
;      Handles landing screen timer (approachFrameTimer/starfieldScrollY),
;      atmosphere reentry audio (wind noise), and
;      final approach/touchdown logic.
;-----------------------------------------------------------
.reentryHeatEffects
    lda     currentScreenId
    cmp     #$04                        ; Landing screen?
    bne     .checkReentryAudio
    ;--- Landing Approach Timer ---
    dec     approachFrameTimer                      ; Approach frame counter
    bpl     .checkReentryAudio
    lda     #$2a
    sta     approachFrameTimer                      ; Reset timer (42 frames)
    dec     starfieldScrollY                      ; Decrease approach timer
    bpl     .checkReentryAudio
    ;--- Check Cargo Door for Landing ---
    bit     cargoDoorState
    bmi     .checkReentryAudio          ; Door already closed
    lda     soundEffectId
    bne     .checkReentryAudio          ; Sound already playing
    lda     #$44                        ; Cargo door warning sound
    sta     soundEffectId
.checkReentryAudio
    ;-----------------------------------------------------------
    ; Reentry Wind/Heating Audio
    ; Below altitude $78: play reentry wind noise.
    ; Volume depends on cargo door state.
    ;-----------------------------------------------------------
    lda     altitude
    cmp     #$78                        ; Below heating zone?
    bcs     .skipToFlightEffects2       ; No: skip audio
    ldx     #$1e
    stx     AUDF1                       ; Wind noise frequency
    ldx     #$08
    stx     AUDC1                       ; Noise waveform
    ldx     #$01                        ; Low volume
    bit     cargoDoorState
    bpl     .setReentryVol
    ldx     #$03                        ; Higher volume with door open
.setReentryVol
    stx     AUDV1
    ;-----------------------------------------------------------
    ; Touchdown Detection
    ; speedFractionLow/speedFractionHigh = touchdown state flags.
    ; reentryContactState = landing gear/ground contact state.
    ;-----------------------------------------------------------
    lda     speedFractionLow
    bne     .checkTouchdownResult       ; Already touching down
    ldy     speedFractionHigh
    bne     .checkTouchdownResult
    lda     reentryContactState                      ; Ground contact?
    bne     .checkLandingPosition
    ;--- First Ground Contact ---
    dec     reentryContactState                      ; Set contact flag ($FF)
    ldx     #$98                        ; Touchdown sound
    jsr     queueSoundEffect
    ;-----------------------------------------------------------
    ; Landing Quality Check
    ; starfieldScrollY determines landing quality:
    ;   >= 0: check further parameters
    ;   < 0 ($80-$FF): check cargo door and pitch
    ; Per manual: "Touch down gently with gear down."
    ;-----------------------------------------------------------
    ldx     #$15                        ; Abort: gear not down
    lda     starfieldScrollY
    bpl     .landingAbort
    ldx     #$20                        ; Abort: approach too fast
    cmp     #$eb
    bcc     .landingAbort
    ldx     #$40                        ; Abort: cargo door issue
    lda     cargoDoorState
    beq     .landingAbort               ; Door not deployed properly
    ;--- Check Pitch on Touchdown ---
    lda     pitchValue
    adc     #$05
    sta     pitchValue
    and     #$10                        ; Pitch overflow?
    beq     .checkLandingPosition
    sta     pitchValue                  ; Cap pitch
.checkLandingPosition
    ;--- Check Landing Y Position ---
    lda     yAxisPlane
    cmp     #$18                        ; Too low?
    bcc     .checkLandingSuccess        ; Below runway = check success
    cmp     #$97                        ; Too high?
    bcs     .checkLandingSuccess        ; Above limit = check success
    ldx     #$10                        ; Abort: off runway
.landingAbort
    jsr     abortMission                       ; Process landing abort
.skipToFlightEffects2
    jmp     .flightEffects
    
;-----------------------------------------------------------
;      Landing Success Check
;      Evaluates pitch, approach timer, and configuration
;      to determine mission outcome rating.
;-----------------------------------------------------------
.checkLandingSuccess
    lda     landingPitchSuccess
    bne     .checkFinalResult
    lda     pitchValue
    bne     .checkFinalResult
    ;--- Perfect Pitch Landing ---
    inc     landingPitchSuccess                      ; Mark successful pitch landing
    lda     #$a2                        ; Success sound
    sta     soundEffectId
.checkFinalResult
    lda     starfieldScrollY
    cmp     #$90                        ; Final approach complete?
.checkTouchdownResult
    bne     .flightEffects              ; Not yet
    ;-----------------------------------------------------------
    ; Mission Complete Scoring
    ; Evaluates landing quality, docking count, and fuel remaining
    ; to determine final screen (mission rating).
    ;   Screen $05: basic completion
    ;   Screen $06: with dockings
    ;   Screen $07: commander patch (6 dockings + 75+ fuel)
    ;-----------------------------------------------------------
    ldx     #$30                        ; Abort: bad final pitch
    lda     pitchValue
    beq     .scoreMission
    jsr     abortMission                       ; Abort for bad pitch
    bcs     .flightEffects
.scoreMission
    sty     speedDisplayLow             ; Clear speed display
    sty     gameActive                  ; Game over
    sty     attractTimer
    sty     autopilotMode               ; Clear autopilot
    ldx     #$21                        ; Default: mission success display
    bit     trainingModeFlag                      ; Training mode?
    bpl     .setEndDisplay
    ldx     #$19                        ; Training: different display
.setEndDisplay
    stx     statusDisplayId
    ;--- Determine End Screen Based on Performance ---
    ldx     #$05                        ; Screen: basic completion
    bit     trainingModeFlag
    bmi     .setEndScreen               ; Training: always screen $05
    ldy     dockingCount                      ; Docking count
    beq     .setEndScreen               ; No dockings: screen $05
    inx                                 ; Screen $06: with dockings
    cpy     #$06
    bne     .setEndScreen               ; Not max dockings
    lda     fuelHigh
    cmp     #$75                        ; Enough fuel for commander patch?
    bcc     .setEndScreen
    inx                                 ; Screen $07: COMMANDER PATCH!
.setEndScreen
    stx     currentScreenId
    sty     trainingModeFlag
;-----------------------------------------------------------
;      Flight Effects Handler (pendingSpeedEffect)
;      Processes deferred speed changes from display kernel.
;      pendingSpeedEffect: 0=none, 1=decrease speed, 2=increase speed.
;-----------------------------------------------------------
.flightEffects
    ldy     pendingSpeedEffect
    beq     .statusDisplay              ; No pending effect
    cpy     #$01
    bne     .checkEffect2
    beq     .doDecSpeed
bank1EntryFromBank0
    jmp     bank1Handler
    
.checkEffect2
    cpy     #$02
    bne     .statusDisplay
    jsr     increaseSpeed                       ; Increase speed
    jmp     .statusDisplay
    
.doDecSpeed
    jsr     decreaseSpeed                       ; Decrease speed
;-----------------------------------------------------------
;      Status Display Value Calculation
;      Converts the current flight parameter (selected by
;      statusDisplayId) into BCD digits for the 7-segment
;      cockpit display. Each display ID shows a different value.
;-----------------------------------------------------------
.statusDisplay
    ldx     statusDisplayId
    cpx     #$0d                        ; Orbital distance display?
    bne     .checkDisplayType
    ;--- Display $0D: Orbital Separation (shuttleOrbitalPos - satelliteOrbitalPos) ---
    clc
    lda     shuttleOrbitalPos
    sbc     satelliteOrbitalPos
    eor     #$ff                        ; Negate for display
    jmp     .convertToDisplay
    
.checkDisplayType
    ;-----------------------------------------------------------
    ; Display Parameter Selection
    ; Maps statusDisplayId to the corresponding flight value:
    ;   $0F = yAxisPlane (pitch indicator position)
    ;   $17 = starfieldScrollY (approach timer / docking proximity)
    ;   $11 = $D2 - altitude (distance from target orbit)
    ;   $13 = pitchValue*4 - $1B (pitch angle in degrees)
    ;   $15 = omsYaw (OMS yaw value)
    ;   Other = skip to digit rendering
    ;-----------------------------------------------------------
    lda     yAxisPlane                  ; Default value
    cpx     #$0f
    beq     .signedConvert              ; Display $0F: Y-axis position
    lda     starfieldScrollY
    cpx     #$17
    beq     .signedConvert              ; Display $17: approach timer
    lda     #$d2
    sec
    sbc     altitude                    ; Distance from 210nm orbit
    cpx     #$11
    beq     .signedConvert              ; Display $11: altitude delta
    lda     pitchValue
    asl
    asl
    sbc     #$1b                        ; Convert to displayable angle
    cpx     #$13
    beq     .signedConvert              ; Display $13: pitch angle
    cpx     #$15
    bne     .skipConversion             ; Not a known display: skip
    lda     omsYaw                      ; Display $15: OMS yaw
.signedConvert
    ;-----------------------------------------------------------
    ; Signed-to-BCD Conversion
    ; Input: A = signed byte value to display.
    ; If negative (>= $80): negate and set sign flag ($A0).
    ; Converts to 3-digit BCD in displayDigitsHigh:displayDigitsLow.
    ;-----------------------------------------------------------
    cmp     #$80
.convertToDisplay
    ldx     #$00                        ; Sign: positive
    bcc     .positiveValue
    ldx     #$a0                        ; Sign: negative (display "-")
    eor     #$ff                        ; Negate
    adc     #$00
.positiveValue
    stx     displayDigitsHigh                      ; Store sign/hundreds digit
    tay                                 ; Save value
    lsr
    lsr
    lsr
    lsr                                 ; Upper nibble = tens estimate
    tax
    lda     nibbleToTensTable,x                     ; BCD lookup table
    sta     displayDigitsLow                      ; Store tens digit
    cpx     #$0d
    bcc     .noHundredsInc
    inc     displayDigitsHigh                      ; Adjust hundreds
.noHundredsInc
    cpx     #$07
    bcc     .noBcdAdjust
    inc     displayDigitsHigh                      ; BCD correction
.noBcdAdjust
    tya
    and     #$0f                        ; Lower nibble = ones
    cmp     #$0a
    bcc     .addOnes
    adc     #$05                        ; BCD adjust for values >= 10
.addOnes
    sed
    adc     displayDigitsLow                      ; Add ones to tens
    sta     displayDigitsLow
    bcc     .skipConversion
    inc     displayDigitsHigh                      ; Carry into hundreds
.skipConversion
    cld
;-----------------------------------------------------------
;      7-Segment Digit Rendering
;      Converts BCD flight data into graphic pointers for the
;      cockpit instrument display. Processes 7 digits (X=6..0)
;      from the speedDisplayLow array. Each digit's high/low
;      nibble is converted to a graphic offset via storeDigitGfxPtr.
;-----------------------------------------------------------
renderDigits
    ldx     #$06                        ; 7 digit positions (0-6)
    ldy     statusDisplayId
    lda     errorDisplayFlag                      ; Error flag?
    beq     .setDisplayIndex
    ldy     #$09                        ; Override: error display
.setDisplayIndex
    sty     tempVar                      ; Save active display index
    cpy     #$0d
    bcc     .digitLoop
    ldy     #$0d                        ; Clamp to max display index
.digitLoop
    lda.wy  speedDisplayLow,y           ; Get digit pair
    and     #$f0                        ; Upper nibble (tens)
    lsr                                 ; Convert to graphic offset
    jsr     storeDigitGfxPtr                       ; Store graphic pointer
    dex
    lda.wy  speedDisplayLow,y           ; Same digit pair
    and     #$0f                        ; Lower nibble (ones)
    asl
    asl
    asl                                 ; Convert to graphic offset
    jsr     storeDigitGfxPtr                       ; Store graphic pointer
    dey
    dex
    bpl     .digitLoop
    ;-----------------------------------------------------------
    ; Blank Leading Zeros
    ; Replace zero graphic pointers ($00) with blank ($54).
    ;-----------------------------------------------------------
    ldx     #$06
    ldy     #$54                        ; Blank digit pattern offset
.blankLeadingZeros
    lda     screenPtr1L,x
    bne     .selectGraphicSet           ; Non-zero: stop blanking
    sty     screenPtr1L,x              ; Replace with blank
    dex
    dex
    bne     .blankLeadingZeros
.selectGraphicSet
    ;-----------------------------------------------------------
    ; Select Graphic Set for Status Display
    ; tempVar/2 indexes into graphicOffsetTable to choose the
    ; appropriate instrument graphic (fuel, speed, altitude, etc).
    ; Displays >= $0C use the multi-screen pointer setup.
    ;-----------------------------------------------------------
    lda     tempVar
    lsr
    tax
    lda     graphicOffsetTable,x        ; Get graphic base offset
    cpx     #$0c
    bcs     .multiScreenSetup
    ;--- Standard 2-line graphic ---
    sta     screenPtr6L
    adc     #$06
    sta     screenPtr5L
    lda     #$de                        ; High byte: graphic ROM page
    sta     screenPtr6H
    sta     screenPtr5H
    bne     .waitForOverscan
.multiScreenSetup
    ;--- Multi-line graphic (6 screen pointers) ---
    ldx     #$0a
    ldy     #$df                        ; High byte for multi-screen
    clc
.setupScreenPtrs
    sta     screenPtr1L,x
    sty     screenPtr1H,x
    adc     #$07                        ; Each graphic is 7 bytes apart
    dex
    dex
    bpl     .setupScreenPtrs
;-----------------------------------------------------------
;      Wait for VBLANK Timer / Overscan End
;      Spins until INTIM reaches 0, then begins visible frame.
;      Launch phase adds extra WSYNC lines for thrust arrow
;      flicker effect during countdown (phase 5 = double wait).
;-----------------------------------------------------------
.waitForOverscan
    ldx     INTIM
    bne     .waitForOverscan
    stx     WSYNC
;---------------------------------------
    ldy     launchPhase
    beq     .startVisibleFrame
    lda     frameCounter
    and     #$02                        ; Flicker every 2 frames
    beq     .startVisibleFrame
    cpy     #$05                        ; Launch phase 5 = extra line
    bne     .normalLaunchWait
    stx     WSYNC                       ; Extra scanline for SRB sep effect
;---------------------------------------
.normalLaunchWait
    stx     WSYNC                       ; Additional WSYNC for launch flicker
;---------------------------------------
.startVisibleFrame
;-----------------------------------------------------------
;      Visible Frame Setup
;      Configures TIA for visible display area.
;      Sets up VBLANK end, playfield reflection, player stretch,
;      and determines which screen kernel to use based on
;      current game state (launch, orbit, reentry, landing).
;-----------------------------------------------------------
    stx     WSYNC
;---------------------------------------
    sta     tempVar
    lda     #$0f
    and     screenBlankFlags                      ; Screen blanking flags
    lsr
    sta     VBLANK                      ; End vertical blank (bit 1=0 = display on)
    dex
    stx     PF2                         ; PF2 = $FF (solid playfield)
    inx
    stx     VDELP0                      ; Enable vertical delay for P0
    stx     VDELP1                      ; Enable vertical delay for P1
    lda     #$80
    sta     HMBL,x                      ; Reset ball horizontal motion
    lda     #$15
    sta     CTRLPF                      ; Reflected PF, ball size 2, PF priority
    ;-----------------------------------------------------------
    ; Screen Type Branching
    ; speedFractionLow: animation/screen state counter.
    ;   >= 9: use orbit/cockpit kernel
    ;   Engine off + pre-launch: use title screen
    ;   In flight (flightPhase > 0): use cockpit kernel
    ;   Otherwise: use launch screen with T/C arrows
    ;-----------------------------------------------------------
    lda     speedFractionLow
    cmp     #$09
    sta     RESBL                       ; Position ball (timing-sensitive)
    bcs     .useCockpitKernel
    ldy     enginePowerOn
    beq     .useFlightKernel
    ldy     flightPhase
    bne     .useCockpitKernel
    ;-----------------------------------------------------------
    ; Launch Screen Kernel Setup
    ; Calculates vertical positions for T arrow, C arrow,
    ; and the gap between them on the launch display.
    ; tempVar/tempVar3 = top/bottom section heights.
    ; tempVar2 = middle gap size.
    ;-----------------------------------------------------------
    eor     #$0f
    sbc     #$05
    rol
    sta     tempVar                      ; Top section height
    sta     tempVar3                      ; Bottom section (same initially)
    lda     #$14
    sbc     tempVar
    asl
    adc     #$0b
    sta     tempVar2                      ; Middle gap height
    ;--- Launch flicker adjustment for countdown display ---
    lda     launchPhase
    beq     .calcLaunchColor
    lda     frameCounter
    and     #$02
    beq     .calcLaunchColor
    inc     tempVar3                      ; Flicker: shift arrow positions
    dec     tempVar
.calcLaunchColor
    lda     countdownTimer
    adc     #$06
    tay                                 ; Y = color index for countdown
    bcc     .setGroundColors            ; No overflow: use ground palette
.useCockpitKernel
    ldy     flightPhase
    bne     .useFlightKernel
    inc     flightPhase                 ; Auto-advance to flight phase 1
.useFlightKernel
    ;-----------------------------------------------------------
    ; Flight/Cockpit Display Colors
    ; Sets background and player colors based on game state:
    ;   - separationFlashTimer: separation flash override (white flash)
    ;   - heatEffectTimer: ionization heating (random flash colors from launchEventData3)
    ;   - atmosphereDensity: atmosphere effects (grey tint)
    ;   - Normal: sky color from launchEventData4 table, player from initialParamsExtended
    ;   - Engine off: use index 5 (dark sky for title)
    ;-----------------------------------------------------------
    lda     #$01
    sta     tempVar                      ; Minimum section height
    lda     descentRateIndex
    lsr
    eor     #$0f
    tay                                 ; Y = color table index
    lda     rngSeed
    bit     separationFlashTimer                      ; Separation flash active?
    beq     .checkHeating
    dec     separationFlashTimer                      ; Decrement flash timer
    lda     #$1e                        ; White flash color
    bne     .setPlayerColor
.checkHeating
    cmp     heatEffectTimer                      ; In heating zone?
    bcs     .checkAtmosphere
    and     #$07                        ; Random color from heating palette
    tay
    lda     launchEventData3,y                     ; Heating color table
.setPlayerColor
    sta     COLUP1
    jmp     .setBackgroundColor
    
.checkAtmosphere
    cmp     atmosphereDensity                      ; In atmosphere zone?
    bcs     .normalColors
    lda     #$08                        ; Grey atmosphere tint
    bne     .setPlayerColor
.normalColors
    lda     enginePowerOn
    bne     .setGroundColors
    ldy     #$05                        ; Engine off: title screen color
.setGroundColors
    lda     initialParamsExtended,y                     ; Player color from table
    sta     COLUP1
    lda     launchEventData4,y                     ; Background color from table
.setBackgroundColor
    sta     COLUBK
    sta     starfieldVerticalCounter
;-----------------------------------------------------------
;      Ground/Mountain Playfield Kernel
;      Draws the ground view with mountains/terrain using
;      PF0/PF1/PF2 registers. 10 scanlines of terrain data
;      from groundPF0Data/groundPF1Data/groundPF2Data tables.
;-----------------------------------------------------------
groundKernel
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     groundPF0Data,x                     ; PF0: left edge terrain
    sta     PF0
    lda     groundPF1Data,x                     ; PF1: center terrain
    sta     PF1
    lda     groundPF2Data,x                     ; PF2: right terrain
    sta     PF2
    sta     HMCLR
    inx
    cpx     #$0a
    bcc     groundKernel
    ldx     tempVar
groundPaddingLoop
    sta     WSYNC
;---------------------------------------
    lda     #$30
    sta     PF0
    lda     #$00
    sta     PF1
    sta     PF2
    dex
    bne     groundPaddingLoop
    lda     enginePowerOn
    beq     dispatchViewKernel
    lda     flightPhase
    bne     dispatchViewKernel
    sta     REFP0
    lda     #$07
    sta     NUSIZ0
    lda     trajectoryDotPos
    jsr     positionSpriteHoriz
    ldx     #$0a
drawTrajectoryDotLoop
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     returnFromInit,x
    sta     GRP0
    lda     dotColorCycleTable,x
    adc     countdownTimer
    sta     COLUP0
    lda     trajectoryDotMotion,x
    sta     HMP0
    dex
    bne     drawTrajectoryDotLoop
    stx     WSYNC
;---------------------------------------
    stx     GRP0
    ldx     tempVar2
dotGapLoop
    stx     WSYNC
;---------------------------------------
    dex
    bne     dotGapLoop
    lda     planeDotPos
    jsr     positionSpriteHoriz
    ldx     #$0a
drawPlaneDotLoop
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     planeDotSpriteData,x
    sta     GRP0
    lda     dotColorCycleTable,x
    adc     countdownTimer
    sta     COLUP0
    lda     launchEventParams2,x
    sta     HMP0
    dex
    bne     drawPlaneDotLoop
    stx     WSYNC
;---------------------------------------
    stx     GRP0
    ldx     tempVar3
    bne     jumpToBlankLoop
dispatchViewKernel
    cmp     #$04
    beq     landingKernelEntry
    txs
    lda     #$04
    sta     tempVar3
    ldy     starfieldColumnIndex
    lda     #$3c
    sta     tempVar2
starfieldMainLoop
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     (gfxDataPtrL),y
    sta     GRP0
    lda     starfieldEnableTable,y
    sta     ENAM1
    dec     tempVar2
    bmi     endStarfieldLoop
    dey
    sta     HMCLR
    bmi     loadNextColumn
    cpy     #$02
    beq     switchStarColumn
    cpy     #$08
    beq     switchStarColumn
    cpy     #$0e
    bne     starfieldMainLoop
switchStarColumn
    tsx
    inx
    txs
    lda     starfieldColumnIndex,x
    sta     tempVar
    lda     (gfxDataPtrL),y
    dey
    sta     WSYNC
;---------------------------------------
    sta     GRP0
    lda     tempVar
starPositionLoop
    sbc     #$0f
    bcs     starPositionLoop
    eor     #$0f
    asl
    asl
    asl
    asl
    adc     #$80
    sta     HMM1
    sta     RESM1
    jmp     starfieldMainLoop
    
loadNextColumn
    dec     tempVar3
    ldx     tempVar3
    lda     columnGfxPtrTable,x
    sta     gfxDataPtrL
    ldy     #$11
    bne     starfieldMainLoop
endStarfieldLoop
    ldx     #$ff
    txs
    inx
    ldy     starfieldColumnIndex
    cpy     #$14
    bne     prepBlankLines
    sta     WSYNC
;---------------------------------------
prepBlankLines
    inx
jumpToBlankLoop
    jmp     blankScanlineCountdown
    
landingKernelEntry
    ldx     altitude
altitudeBlankLoop
    sta     WSYNC
;---------------------------------------
    dex
    bpl     altitudeBlankLoop
    lda     pitchValue
    tax
    eor     #$1f
    sec
    sbc     #$0f
    sta     tempVar3
pitchAngleBlankLoop
    sta     WSYNC
;---------------------------------------
    dex
    bpl     pitchAngleBlankLoop
    inx
    stx     REFP0
    lda     #$05
    sta     NUSIZ0
    lda     trajectoryDotPos
    jsr     addYAxisAndPosition
    ldx     #$07
landingTrajectoryLoop
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     trajectoryDotGfx,x
    jsr     setMarkerGfxAndColor
    bne     landingTrajectoryLoop
    lda     planeDotPos
    jsr     addYAxisAndPosition
    ldx     #$07
landingPlaneDotLoop
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     planeDotGfx,x
    jsr     setMarkerGfxAndColor
    bne     landingPlaneDotLoop
    lda     yAxisPlane
    clc
    adc     #$6d
    bcs     wrapYToRange
    cmp     #$a0
    bcc     positionShuttleIndicator
wrapYToRange
    sbc     #$a0
positionShuttleIndicator
    jsr     positionSpriteHoriz
    inx
    lda     yAxisPlane
    clc
    adc     #$24
    cmp     #$a0
    bcc     positionRunwaySprites
    sbc     #$a0
positionRunwaySprites
    jsr     positionSpriteHoriz
    lda     #$17
    sta     NUSIZ0
    sta     NUSIZ1
    ldy     #$24
    sty     COLUP0
    sty     COLUP1
    ldx     #$04
drawRunwayLoop
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     runwayColorData,x
    sta     COLUBK
    lda     runwayGfxData,x
    sta     GRP0
    sta     GRP1
    dex
    sty     HMP0
    sty     HMP1
    bne     drawRunwayLoop
    sta     WSYNC
;---------------------------------------
    stx     GRP0
    stx     GRP1
    sty     COLUBK
    lda     yAxisPlane
    cmp     #$50
    bcc     positionRunwayMarkers
    adc     #$5f
    clc
positionRunwayMarkers
    adc     #$52
    tay
    ldx     #$02
    jsr     positionSpriteHoriz
    tya
    clc
    adc     #$01
    inx
    jsr     positionSpriteHoriz
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    jsr     returnFromSpeedUpdate
    ldy     #$00
    lda     yAxisPlane
    cmp     #$50
    bcc     setLeftDriftMotion
    ldx     #$10
    stx     HMM0
    ldx     #$01
    stx     HMM1
    cmp     #$78
    jmp     setDriftDirection
    
setLeftDriftMotion
    ldx     #$f0
    stx     HMM1
    ldx     #$00
    stx     HMM0
    cmp     #$28
setDriftDirection
    beq     applyDriftAndSync
    ldy     #$10
    bcc     applyDriftAndSync
    ldy     #$f0
applyDriftAndSync
    sty     starfieldHorizontalMotion,x
    sta     WSYNC
;---------------------------------------
    lda     #$20
    sta     COLUBK
    lda     yAxisPlane
    cmp     #$50
    bcc     calcCenterDeviation
    sbc     #$50
calcCenterDeviation
    cmp     #$29
    bcc     storeDeviationCheck
    eor     #$ff
    sbc     #$b0
storeDeviationCheck
    lsr
    sta     tempVar
    lda     reentryContactState
    beq     calcApproachScrollPos
    lda     frameCounter
    and     #$03
    bne     calcApproachScrollPos
    lda     gameActive
    beq     calcApproachScrollPos
    dec     starfieldScrollY
calcApproachScrollPos
    lda     starfieldScrollY
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
    sbc     altitude
    clc
    adc     tempVar3
    sta     tempVar3
    ldy     #$0f
    lda     frameCounter
    and     #$10
    bne     setApproachColors
    ldy     #$0a
setApproachColors
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
    cpy     tempVar                      ; Compare loop counter to Motion Threshold?
    bcc     clearStarMotion                       ; Branch if "fast motion" update not needed yet.
    ldy     #$00
    lda     starfieldHorizontalMotion,x ; Load motion value from table.
    sta     HMM0,x                      ; Apply to HMM0 (or HMM1 if X=1).
    bcs     checkStarVertCounter
clearStarMotion
    lda     #$00
    sta     HMM0,x                      ; Clear motion if below threshold.
checkStarVertCounter
    lda     #$00
    dec     starfieldVerticalCounter    ; Decrement the star/line pattern counter.
    bpl     collisionClipM0
    sta     ENAM0                       ; If counter wrapped, clear ENAM0.
    bmi     storeM1AndLoop                       ; And clear ENAM1 (via fallthrough/branch logic).
collisionClipM0
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
    bmi     storeM0Enable                       ; If hit (inside wall/frame), skip enable (keep 0/Off).
    lda     #$02                        ; Else, Enable M0 (1 pixel).
storeM0Enable
    sta     ENAM0

    lda     #$00                        ; Prepare 0 (Off).
    ; Check Missile 1 (Green Stars).
    bit     CXM1FB                      ; Check M1 collision with Playfield/Player.
    bmi     storeM1AndLoop                       ; If hit (inside wall/frame), skip enable (keep 0/Off).
    lda     #$02                        ; Else, Enable M1 (1 pixel).
storeM1AndLoop
    sta     ENAM1
    dec     tempVar3                      ; Decrement scanline counter.
    bpl     kernelDrawCockpitWindow     ; Loop for next scanline.
    txs
    ldx     #$0a
kernelCockpitBorder
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    lda     cockpitBorderPF0,x
    sta     PF0
    lda     returnFromSpeedUpdate,x
    sta     PF1
    lda     cockpitBorderPF2,x
    sta     PF2
    stx     tempVar2
    tsx
    iny
    cpy     tempVar
    bcc     clearBorderMotion
    ldy     #$00
    lda     starfieldHorizontalMotion,x
    sta     HMM0,x
    bcs     checkBorderVertCounter
clearBorderMotion
    lda     #$00
    sta     HMM0,x
checkBorderVertCounter
    dec     starfieldVerticalCounter
    bpl     borderLoopAndExit
    sta     ENAM0
    sta     ENAM1
borderLoopAndExit
    ldx     tempVar2
    dex
    bne     kernelCockpitBorder
    jmp     exitLogicToKernel
    
setMarkerGfxAndColor
    sta     GRP0
    lda     markerColorTable,x
    sta     COLUP0
    sta     HMCLR
    dex
    rts
    
blankScanlineCountdown
    stx     WSYNC
;---------------------------------------
    dex
    bne     blankScanlineCountdown
    stx     GRP0
    stx     ENAM1
    lda     enginePowerOn
    beq     beginSatelliteSection
    lda     #$10
    ldy     flightPhase
    beq     beginSatelliteSection
    ldy     starfieldVerticalCounter
    cpy     #$90
    bne     beginSatelliteSection
    lda     pitchValue
beginSatelliteSection
    sta     tempVar3
    lda     cockpitAnimCounter
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
positionSatMissile
    dex
    bne     positionSatMissile
    sta     RESM0
    sta     HMM0,x
    lda     #$c0
    sta     HMP0,x
    dex
    stx     ENAM0
    ldx     #$11
    lda     #PURPLE|$9
    sta     RESP0
satelliteDisplayLoop
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    sta     COLUPF
    lda     satelliteSectionPF0,x
    sta     PF0
    lda     satelliteSectionPF1,x
    sta     PF1
    lda     satelliteSectionPF2,x
    sta     PF2
writeSatSpriteLine
    lda     #$ff
    sta     GRP0
    dey
    bpl     checkSatTransition
    ldy     #$0f
checkSatTransition
    sta     HMCLR
    lda     starfieldVerticalCounter
    dec     tempVar3
    bpl     countSatLinesAndExit
    lda     satelliteColorGradient,y
    sec
    adc     satelliteColorOffset
countSatLinesAndExit
    dex
    beq     exitLogicToKernel
    cpx     #$0a
    bcs     satelliteDisplayLoop
    sta     WSYNC
;---------------------------------------
    sta     COLUBK
    lda     #BLACK|$0
    sta     COLUPF
    lda     cockpitBorderPF0,x
    sta     PF0
    lda     returnFromSpeedUpdate,x
    sta     PF1
    lda     cockpitBorderPF2,x
    sta     PF2
    bcc     writeSatSpriteLine
;-----------------------------------------------------------
;      Exit Logic -> Switch to Kernel (Bank 1 -> Bank 0)
;-----------------------------------------------------------
exitLogicToKernel
    sta     WSYNC
;---------------------------------------
    jmp     resetBank1
    
bank1Handler
    bit     autopilotMode
    bmi     waitForTimerVSYNC
    lda     flightPhase
    cmp     #$02
    bne     checkSeparationEvent
    bit     consoleSwitches
    bmi     resetCargoDoorTimer
    lda     cargoDoorTimer
    bpl     checkSeparationEvent
    ldx     #$41
    stx     soundEffectId
    cmp     #$ff
    bne     checkSeparationEvent
    ldx     #$85
    jsr     abortMission
resetCargoDoorTimer
    ldx     #$01
    stx     cargoDoorTimer
checkSeparationEvent
    lda     separationEventTimer
    bne     waitForTimerVSYNC
    bit     consoleSwitches
    bpl     checkDoorCloseNeeded
    ldx     #$50
    lda     flightPhase
    beq     waitForTimerVSYNC
    and     #$01
    bne     callAbortMission
    lda     cargoDoorState
    bne     waitForTimerVSYNC
    dec     cargoDoorState
    .byte   $2c ;bit                ;4-5 =  30 *
toggleCargoDoors
    inc     cargoDoorState
    ldx     #$72
    jsr     queueSoundEffect
    bne     waitForTimerVSYNC
callAbortMission
    jsr     abortMission
checkDoorCloseNeeded
    lda     cargoDoorState
    bne     toggleCargoDoors
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
    lda     landingDisplayMode
    beq     positionTargetVSYNC
    lda     soundEffectId
    bne     positionTargetVSYNC
    dec     landingDisplayMode
    lda     #$82
    sta     soundEffectId
positionTargetVSYNC
    lda     targetHorizPos
    jsr     positionSpriteHoriz
    stx     WSYNC
;---------------------------------------
    stx     VSYNC
    jmp     mainFrameLoop
    
subtractFuel
    bit     trainingModeFlag
    bmi     returnFromSubtractFuel
    sed
    sec
    sta     tempVar
    lda     fuelLow
    sbc     tempVar
    sta     fuelLow
    lda     fuelHigh
    sbc     #$00
    sta     fuelHigh
    cld
    bcs     returnFromSubtractFuel
    ldx     #$99
abortMission
    sec
    lda     gameActive
    beq     returnFromSubtractFuel
    stx     abortCode
    txa
    ldx     trainingModeFlag
    beq     clearFuel
    inx
    ldy     #$05
abortCodeFilterLoop
    cmp     returnFromSubtractFuel,y
    beq     resetAllGameState
    dey
    bne     abortCodeFilterLoop
    bit     autopilotMode
    bmi     returnFromSubtractFuel
    sta     errorDisplayFlag
    ldx     #$b4
queueSoundEffect
    stx     soundEffectId
    ldx     #$fe
    stx     soundSequenceIndex
    rts
    
clearFuel
    stx     fuelLow
    stx     fuelHigh
resetAllGameState
    stx     attractTimer
    stx     errorDisplayFlag
    stx     gameActive
    stx     launchPhase
    stx     movementFlags
    stx     heatEffectTimer
    stx     omsYaw
    stx     atmosphereDensity
    stx     autopilotMode
    stx     trainingModeFlag
    ldx     #$1b
    stx     statusDisplayId
returnFromSubtractFuel
    rts
    
    .byte   $95,$70,$75,$80,$10             ; $fd0a (*)
    
setMovementAndPerturb
    ora     movementFlags
    sta     movementFlags
    lda     dockingCount
    beq     returnWithX0F
    bit     rngSeed
    bmi     returnWithX0F
    inc     satelliteOrbitalPos
returnWithX0F
    ldx     #$0f
    rts
    
increaseSpeed
    lda     altitude
    cmp     #$ff
    beq     returnFromSpeedChange
    sed
    lda     speedFractionLow
    clc
    adc     #$01
    sta     speedFractionLow
    bcc     checkSpeedTensCarry
    lda     speedFractionHigh
    adc     #$00
    sta     speedFractionHigh
    inc     descentRateIndex
checkSpeedTensCarry
    lda     speedFractionLow
    and     #$0f
    bne     returnFromSpeedChange
incAltitude
    inc     altitude
returnFromSpeedChange
    cld
    rts
    
decreaseSpeed
    sed
    lda     speedFractionLow
    sec
    sbc     #$01
    sta     speedFractionLow
    bcs     checkSpeedDecTens
    lda     speedFractionHigh
    sbc     #$00
    sta     speedFractionHigh
    dec     descentRateIndex
checkSpeedDecTens
    lda     speedFractionLow
    and     #$0f
    bne     returnFromSpeedChange
    dec     altitude
    lda     altitude
    cmp     #$ff
    bne     returnFromSpeedChange
    beq     incAltitude
    
initialGameVarsTable
    .byte   $01,$0f,$12,$0d,$05,$19,$66,$6e ; $fd64 (D)
    .byte   $0d,$14,$07,$47,$2b,$67,$17,$57 ; $fd6c (D)
    .byte   $0e,$76,$2f,$4a,$7f,$5c,$00,$00 ; $fd74 (D)
    .byte   $00,$00,$99,$99,$15,$a0,$00,$00 ; $fd7c (D)
difficultyMultTable
    .byte   $01,$05                         ; $fd84 (D)
    .byte   $04,$06,$02,$0a                 ; $fd86 (*)
initialParamsExtended
    .byte   $08,$09,$0f,$0f,$0f             ; $fd8a (*)
    
    .byte   BLACK|$f                        ; $fd8f (C)
    
    .byte   $0b                             ; $fd90 (*)
    
    .byte   BLACK|$8                        ; $fd91 (C)
    
    .byte   $88,$88,$86,$86,$68,$68,$86,$86 ; $fd92 (*)
nibbleToTensTable
    .byte   $00,$16,$32,$48,$64,$80,$96,$12 ; $fd9a (*)
    .byte   $28,$44,$60,$76,$92,$08         ; $fda2 (*)
descentRateTable
    .byte   $24,$40,$03,$04,$04,$03,$03,$02 ; $fda8 (*)
    .byte   $02,$02,$02,$02,$02,$02,$03,$03 ; $fdb0 (*)
    .byte   $06,$06,$06,$07,$14,$14         ; $fdb8 (*)
markerColorTable
    .byte   $0e,$0e,$08,$1a,$0c,$0e,$0e     ; $fdbe (*)
trajectoryDotGfx
    .byte   $0e,$00,$1f,$3c,$ff,$7f,$3e     ; $fdc5 (*)
planeDotGfx
    .byte   $18,$00,$1f,$7c,$fe,$3f,$7c     ; $fdcc (*)
runwayGfxData
    .byte   $38,$ff,$ee,$cc                 ; $fdd3 (*)
runwayColorData
    .byte   $88,$ae,$7c,$7a,$78             ; $fdd7 (*)
    
storeDigitGfxPtr
    cpy     #$04
    bcs     writeGfxPointer
    bit     heatEffectTimer
    bpl     writeGfxPointer
    lda     #$50
writeGfxPointer
    sta     screenPtr1L,x
    lda     #$de
    sta     screenPtr1H,x
    dex
    rts
    
fuelRefillTable
    .byte   $15,$20,$25,$30,$35,$40,$40     ; $fdee (*)
    
addYAxisAndPosition
    clc
    adc     yAxisPlane
    bcs     wrapPositionSub
    cmp     #$a0
    bcc     positionSpriteHoriz
wrapPositionSub
    sbc     #$9f
positionSpriteHoriz
    sta     WSYNC
;---------------------------------------
    sec
horizPosDivLoop
    sbc     #$0f
    bcs     horizPosDivLoop
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
cockpitBorderPF0
    .byte   $35                             ; $fe26 (*)
    
groundPF0Data
    .byte   %11110000 ; |****    |            $fe27 (P)
    .byte   %11110000 ; |****    |            $fe28 (P)
    .byte   %11110000 ; |****    |            $fe29 (P)
    .byte   %11110000 ; |****    |            $fe2a (P)
    .byte   %11110000 ; |****    |            $fe2b (P)
    .byte   %11110000 ; |****    |            $fe2c (P)
    .byte   %01110000 ; | ***    |            $fe2d (P)
    .byte   %01110000 ; | ***    |            $fe2e (P)
    .byte   %01110000 ; | ***    |            $fe2f (P)
cockpitBorderPF2
    .byte   %01110000 ; | ***    |            $fe30 (P)
groundPF2Data
    .byte   %11100000 ; |***     |            $fe31 (P)
    .byte   %11000000 ; |**      |            $fe32 (P)
satelliteSectionPF0
    .byte   %11000000 ; |**      |            $fe33 (P)
    .byte   %10000000 ; |*       |            $fe34 (P)
    .byte   %10000000 ; |*       |            $fe35 (P)
    .byte   %10000000 ; |*       |            $fe36 (P)
    .byte   %00000000 ; |        |            $fe37 (P)
    .byte   %00000000 ; |        |            $fe38 (P)
satelliteSectionPad
    .byte   %00000000 ; |        |            $fe39 (P)
    .byte   %00000000 ; |        |            $fe3a (P)
    
satelliteSectionPF1
    .byte   $02,$01                         ; $fe3b (*)
    
    .byte   %11110000 ; |****    |            $fe3d (P)
    .byte   %10000000 ; |*       |            $fe3e (P)
    .byte   %00000000 ; |        |            $fe3f (P)
    .byte   %00000000 ; |        |            $fe40 (P)
    .byte   %00000000 ; |        |            $fe41 (P)
    .byte   %00000000 ; |        |            $fe42 (P)
satelliteSectionPF2
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
    
satelliteColorGradient
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
    
coldStartClearRAM
    lda     #$00
    ldx     #$77
clearRAMLoop
    sta     rngSeed,x
    dex
    bne     clearRAMLoop
initGameVars
    ldx     #$21
copyInitVarsLoop
    lda     initialGameVarsTable,x
    sta     rngSeed,x
    dex
    bpl     copyInitVarsLoop
returnFromInit
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
planeDotSpriteData
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
trajectoryDotMotion
    .byte   %10001001 ; |#   #  #|            $fe8c (G)
    
launchEventParams
    .byte   $01,$e0,$e0,$f2,$00,$11,$20,$21 ; $fe8d (D)
    .byte   $e2                             ; $fe95 (D)
launchEventParams2
    .byte   $20,$02,$e1,$22,$e0,$e1,$f0,$01 ; $fe96 (D)
    .byte   $12,$21                         ; $fe9e (D)
dotColorCycleTable
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
    
launchEventTimings
    .byte   $04,$00,$03,$07,$10,$13,$15,$22 ; $feab (*)
    .byte   $40,$41,$65,$66,$70,$72         ; $feb3 (*)
starfieldEnableTable
    .byte   $76,$77                         ; $feb9 (D)
    .byte   $80                             ; $febb (*)
    .byte   $85,$88,$89                     ; $febc (D)
launchEventData3
    .byte   $e6,$32                         ; $febf (D)
    .byte   $f4                             ; $fec1 (*)
    .byte   $00,$24,$20                     ; $fec2 (D)
launchEventData4
    .byte   $36,$46                         ; $fec5 (D)
    .byte   $90                             ; $fec7 (*)
    .byte   $90,$90                         ; $fec8 (D)
    
    .byte   BLUE_CYAN|$0                    ; $feca (CB)
    
    .byte   $90                             ; $fecb (D)
    
    .byte   BLUE_CYAN|$0                    ; $fecc (CB)
    
    .byte   $80                             ; $fecd (D)
    .byte   $60,$70,$82,$62,$64,$84,$86     ; $fece (*)
    
incSpeedAndDisplay
    inc     speed
    bne     bcdIncSpeedDisplay
    dec     speed
    rts
    
bcdIncSpeedDisplay
    sed
    lda     speedDisplayLow
    clc
    adc     #$01
    sta     speedDisplayLow
    lda     speedDisplayHigh
    adc     #$00
    bcc     storeSpeedHigh
decSpeedAndDisplay
    dec     speed
    bne     bcdDecSpeedDisplay
    inc     speed
    rts
    
bcdDecSpeedDisplay
    sed
    sec
    lda     speedDisplayLow
    sbc     #$01
    sta     speedDisplayLow
    lda     speedDisplayHigh
    sbc     #$00
storeSpeedHigh
    sta     speedDisplayHigh
    cld
returnFromSpeedUpdate
    rts
    
groundPF1Data
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
bank1Vectors
    .byte   $00                             ; $fff8 (D)
    .byte   $00,$00,$00                     ; $fff9 (*)
    .byte	$03,$f0
	.byte	$00,$00                 ; $fffc (D)
