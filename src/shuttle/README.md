# Space Shuttle: A Journey Into Space (Atari 2600) - Reverse Engineered Source

**Original Game (1983) by Activision**  
**Original Designer:** Steve Kitchen  
**Disassembly & Analysis:** Halkun

---

## Overview

This repository contains the reverse-engineered and annotated source code for the Atari 2600 game *Space Shuttle: A Journey Into Space*. The game simulates a complete Space Shuttle mission — countdown, launch, orbital maneuvering, satellite docking, deorbit burn, reentry, and landing — all within 8KB of ROM.

## Project Structure

* **`src/shuttle/`**: Main assembly source (`shuttle.asm`) and documentation.
* **`src/shuttle/docs/`**: Original game manual (`manual.txt`) and walkthrough (`walkthrough.txt`).
* **`bin/`**: Build tools (DASM) and emulator (Stella).
* **`inc/`**: Shared include files for TIA/VCS constants.
* **`out/`**: Compiled binaries, symbol files, and listings (auto-generated).
* **`orig/`**: Original ROM binary for verification.
* **`Makefile`**: Cross-platform build system.

## How to Build & Run

### Prerequisites

* **Windows**: `dasm.exe` and `Stella.exe` in `bin/`.
* **Linux**: `dasm` and `stella` installed system-wide.

### Compiling

```cmd
make shuttle
```

The build automatically verifies the output binary matches the original ROM (`FC: no differences encountered`).

### Running

```cmd
make run GAME=shuttle
```

---

## Technical Documentation

### ROM Architecture

The game uses a **2-bank ROM** (8KB total, F8 bankswitch) with strobes at `bank0Strobe` (`$FFF8`) and `bank1Strobe` (`$FFF9`). Unlike more complex bankswitching schemes, F8 uses simple address reads — any access to the strobe address causes the hardware to swap banks.

| Aspect | Bank 0 (`$D000`–`$DFFF`) | Bank 1 (`$F000`–`$FFFF`) |
| -------- | -------------------------- | -------------------------- |
| **Role** | Display kernels & graphic data | Game logic & frame setup |
| **Contains** | Upper dashboard rendering, cockpit view, instrument readout, Activision logo, sound engine, orbital movement, all graphic/sprite data tables | Main game loop (all mission phases), visible frame lower kernels (starfield, ground, landing), subroutines, game data tables |

Both banks begin with a **safety guard**: Bank 0's entry (`resetBank0`) immediately reads `bank1Strobe`, and Bank 1's entry (`resetBank1`) immediately reads `bank0Strobe`. This ensures that regardless of which bank is active at power-on, execution always reaches the correct initialization in Bank 1.

### Console Switch Mapping

The Atari 2600's console switches are repurposed as flight deck controls:

| Console Switch | Flight Deck Function |
| ---------------- | --------------------- |
| Power On/Off | Internal Power |
| Color/B&W | Primary Engines |
| Left Difficulty | Backup Engines |
| Right Difficulty | Cargo Doors / Landing Gear |
| Game Select | Status Display |
| Game Reset | Activate Countdown |

### Game Loop

The program is structured around the NTSC television signal — one complete pass through the loop produces one frame of video (~60 fps). The frame is divided into four phases: **VSYNC**, **VBLANK** (game logic), **Kernel** (visible picture split across both banks), and **Overscan**. Two bank switches occur every frame — one to enter Bank 0 for the upper display, and one to return to Bank 1 for the lower display and post-kernel logic.

#### Frame Overview

```text
mainFrameLoop ──► VBLANK timer set ──► Game Logic (Bank 1)
    │
    ├── PRNG update, frame counter increment
    ├── Auto-start / attract mode check
    ├── Fuel consumption (every 16 frames)
    ├── MECO check (engine cutoff)
    ├── Launch→orbit transition evaluation
    ├── T/C arrow logic (thrust/computer alignment)
    ├── Trajectory & plane dot updates
    ├── Console switch handling (Reset/Select)
    ├── Mission phase dispatch:
    │   ├── Launch MET clock & event sequencer
    │   ├── Ascent logic (speed/altitude/joystick)
    │   ├── Orbit phase (docking/OMS/station-keeping)
    │   ├── Deorbit burn
    │   ├── Reentry descent (heating/atmosphere)
    │   └── Landing approach & touchdown scoring
    ├── Flight effects (deferred speed changes)
    ├── Status display conversion (BCD)
    ├── renderDigits (graphic pointer setup)
    │
    ├── Wait for VBLANK timer
    │
    ├── ── Visible Frame (Bank 1 lower kernels) ──
    │   ├── Ground/terrain kernel (10 lines)
    │   ├── Starfield / launch dots / landing view
    │   └── Cockpit window (starfield through frame)
    │
    ├── exitLogicToKernel ──► jmp resetBank1
    │                          bit bank0Strobe ──► Bank 0
    │
    ├── ── Visible Frame (Bank 0 upper kernels) ──
    │   ├── Top dashboard border (8 lines)
    │   ├── Thrust bar strip (5 lines)
    │   ├── Cockpit window frame (8 lines)
    │   ├── Instrument readout (6 lines, 48px multiplex)
    │   ├── Main view window (25 lines, 6-column graphic)
    │   ├── Engine flame area (8 lines)
    │   └── Activision logo / copyright bar (8 lines)
    │
    ├── ── Overscan (Bank 0) ──
    │   ├── Trajectory/fuel penalty calculation
    │   ├── Autopilot pitch corrections
    │   ├── Sound engine (2 channels)
    │   ├── Orbital movement & starfield rotation
    │   └── Satellite rendering calculations
    │
    ├── endOfKernelSwitch ──► bit bank1Strobe ──► Bank 1
    │
    ├── bank1Handler
    │   ├── Cargo door abort check
    │   └── Separation event processing
    │
    └── waitForTimerVSYNC ──► VSYNC pulse ──► jmp mainFrameLoop
```

#### Phase 1: VSYNC (2 Scanlines)

**Entry point**: `waitForTimerVSYNC` (Bank 1)

The CPU spins on `INTIM` until the overscan timer expires, then asserts VSYNC for 2 scanlines:

| Scanline | Work |
| ---------- | ------ |
| 1 | Assert VSYNC (`sty VSYNC` where Y=2). Process landing display mode — if `landingDisplayMode` is non-zero and no sound is playing, decrement it and trigger landing sound (`$82`). Position satellite sprite horizontally via `Lfe00`. |
| 2 | De-assert VSYNC (`stx VSYNC` where X=0). Jump to `mainFrameLoop`. |

#### Phase 2: VBLANK (~41 Scanlines of CPU Time)

**Entry point**: `mainFrameLoop` (Bank 1, `$F012`)

All game logic runs while the TIA outputs a blank screen. The VBLANK timer is set to `$29` (41 × 64 = 2,624 cycles ≈ 34.5 scanlines).

##### Initialization & Housekeeping

| Step | Label | Description |
| ------ | ------- | ------------- |
| 1 | `mainFrameLoop` | Set `TIM64T = $29`. Update PRNG (8-bit LFSR). Increment `frameCounter`. |
| 2 | *(auto-start)* | Every 64 frames: if `countdownTimer` reaches 9 and game is inactive, auto-launch in autopilot mode. |
| 3 | *(fuel burn)* | Every 16 frames: base burn = 1. During launch (`flightPhase=1`): +6 extra. If T/C arrows misaligned: +2 penalty. Call `subtractFuel`. |

##### MECO & Orbit Transition

| Step | Label | Description |
| ------ | ------- | ------------- |
| 4 | `.checkEngineSwitch` | **MECO (Main Engine Cut-Off)**: Autopilot mode triggers at altitude $D2 (210 nm). Manual mode: reads Color/B&W switch (Primary Engines) and Left Difficulty (Backup Engines). |
| 5 | `.orbitAbortCheck` | On MECO: evaluates orbit quality from `planeCorrection`. If plane angle too far off (≥$10 error) → `abortMission`. Otherwise transitions to `flightPhase=2` (orbit). |

##### T/C Arrow Logic

| Step | Label | Description |
| ------ | ------- | ------------- |
| 6 | `.moveComputerArrow` | Computer arrow auto-tracks to target position derived from `autoThrustCommand`. |
| 7 | `.checkFireButton` | Player controls thrust arrow (T) via fire button + joystick up/down. Clamped to range $0F–$8B. Arrow misalignment indicator (`arrowsMisaligned`) set when T≠C. |

##### Trajectory & Timing

| Step | Label | Description |
| ------ | ------- | ------------- |
| 8 | `.checkPlaneDot` | Updates trajectory and plane dot positions based on pitch and plane correction values. |
| 9 | `.skipSeparationFlash` | Manages SRB/ET separation visual effect — `separationEventTimer` overflow triggers white flash and sound. |

##### Console Switch Handling

| Step | Label | Description |
| ------ | ------- | ------------- |
| 10 | `.checkFirstPress` | **Game Reset** (Activate Countdown): first press starts countdown. Hold 2 seconds → full game restart. Debounced via `switchDebounceTimer`. |
| 11 | `.cycleFlightDisplay` | **Game Select** (Status): cycles `statusDisplayId` through instrument displays. Also selects difficulty level (Flight #1–#3) before launch via repeated presses, and toggles autopilot/training mode accordingly. |

##### Mission Phase Dispatch

The remainder of VBLANK is a large phase dispatcher based on `flightPhase` and `launchPhase`. Each mission phase runs its own logic block:

###### Launch Phase (`flightPhase=1`)

| Step | Label | Description |
| ------ | ------- | ------------- |
| 12 | `launchMETLogic` | **MET (Mission Elapsed Time) Clock**: BCD-ticks every 59 frames. Countdown phase: `metHigh=$A0` (T-minus). T-0 check at `metLow=$00`. Post-liftoff: phase 5 at MET 0:03 (hold-down bolt release), phase 7 at MET 0:25 with ET separation flash+sound. |
| 13 | `handleLaunchScrub` | On countdown abort: resets MET to T-16, clears phase, plays scrub sound ($38). |
| 14 | *(event dispatcher)* | Matches `metLow` against `launchEventTimings` table (14 entries) to set `autoThrustCommand` — drives automated thrust changes during ascent. |
| 15 | `ascentLogic` | Speed increment rate derived from thrust arrow position. Altitude increment rate from ascent table. Wind shear perturbation every 32 frames. Joystick input: up/down adjusts pitch, left/right adjusts plane. |

###### Orbit Phase (`flightPhase=2`)

| Step | Label | Description |
| ------ | ------- | ------------- |
| 16 | `.orbitPhaseDispatch` | Routes to sub-phases: `flightPhase=3` → reentry speed management, `flightPhase=4` → docking display, else → general orbital operations. |
| 17 | `.checkOrbitInput` | Joystick input with rate-limiting (`inputDelayTimer`): forward/back adjusts speed (X-axis), left/right adjusts Y-axis, fire+up/down adjusts Z-axis (altitude). |
| 18 | `.dockingProgress` | Checks 5 docking conditions simultaneously: `yAxisPlane=0`, `pitchValue=0`, `targetHorizPos` in $47–$4F range, orbital position match (`shuttleOrbitalPos=satelliteOrbitalPos`), altitude=$D2. All must hold for 2 seconds (`dockingProgress` counter reaches $80). |
| 19 | `.doRefuel` | On successful dock: adds fuel from `Lfdee` table (15→40 units, increasing per dock), increments `missionScore` (BCD), `dockingCount` (max 6). |
| 20 | `.checkAltitudeAbort` | Orbit abort conditions: altitude=$FF (overflow) → abort. Altitude<$C3 (195 nm) → abort $70 (atmospheric burn-up). Speed≠$A9 → abort $80. |

###### OMS Burns (`flightPhase=2`, with engines active)

| Step | Label | Description |
| ------ | ------- | ------------- |
| 21 | `.orbitOmsInput` | **OMS (Orbital Maneuvering System)**: joystick drives orbital burns. Fuel cost = inverted SWCHA nibble × base rate. Dispatches autopilot vs. manual burns. |
| 22 | `.manualOmsBurn` | Console switches active: joystick directly controls pitch and yaw. Deorbit burn trigger: switches + fire button simultaneously. |
| 23 | `.omsBurnProcess` | Processes OMS fuel consumption with BCD subtraction. Updates speed/altitude based on burn parameters. |

###### Deorbit & Reentry (`flightPhase=3`)

| Step | Label | Description |
| ------ | ------- | ------------- |
| 24 | `.deorbitLogic` | Monitors descent: `speed < $BF` and `altitude < $D7` triggers deorbit evaluation. Quality check: `omsYaw≠0` → abort $65, `pitchValue` outside tolerance → abort. At altitude $C8: transitions to `flightPhase=3` (reentry mode). |
| 25 | `.doReentryDescent` | **Three altitude bands**: ≥$A7 (167nm+) = no heating; $78–$A6 = ionization zone (increments `heatEffectTimer` — random color flashes); <$78 = lower atmosphere (increments `atmosphereDensity` — grey tint effect). Descent rate from `Lfda8` table or pitch+switch formula. |
| 26 | *(landing transition)* | At altitude $1E (30nm): transitions to `flightPhase=4` (landing). Sets `landingDisplayMode=2`, `starfieldScrollY=$80` (approach timer). |

###### Landing (`flightPhase=4`)

| Step | Label | Description |
| ------ | ------- | ------------- |
| 27 | *(approach)* | `approachFrameTimer` ticks down; on underflow resets to $2A (42 frames) and decrements `starfieldScrollY` (approach progress). Cargo door check → abort if doors still open. Reentry wind audio. |
| 28 | `.checkLandingSuccess` | **Touchdown detection**: checks pitch (must be 0), Y-axis position, approach timer ≤ 0. On success: evaluates landing quality for screen selection. |
| 29 | `.scoreMission` | Screen $05 = basic landing. Screen $06 = with dockings. Screen $07 = **Commander Patch** (requires 6 dockings AND fuel ≥ 75 units). |

##### Post-Phase Logic

| Step | Label | Description |
| ------ | ------- | ------------- |
| 30 | `.flightEffects` | Processes `pendingSpeedEffect` — deferred speed increment/decrement flagged by the display kernel during the previous frame. |
| 31 | `.statusDisplay` | Converts `statusDisplayId` to a displayable value. Maps IDs to flight parameters: $0D=orbital separation (`shuttleOrbitalPos−satelliteOrbitalPos`), $0F=Y-axis, $11=altitude, $13=pitch, $15=OMS yaw, $17=approach timer. |
| 32 | `.convertToDisplay` | Signed-to-BCD conversion: produces 3 BCD digits in `displayDigitsHigh`:`displayDigitsLow`. Negative values get $A0 sign prefix. Uses `Lfd9a` nibble-to-tens lookup. |
| 33 | `renderDigits` | Converts 7 BCD digits into graphic pointers (`screenPtr1L`–`screenPtr6L`) by indexing into `numberSprites` table. Blanks leading zeros. Selects instrument label sprite from `graphicOffsetTable`. |

The VBLANK logic then waits for the timer to expire before beginning the visible frame.

#### Phase 3: Visible Kernel (~192 Scanlines)

The visible frame is **split across both banks** — an unusual architecture where Bank 1 draws the lower portion of the screen first (ground, starfield, cockpit window), then bank-switches to Bank 0 which draws the upper portion (dashboard, instruments, main view).

This inverted rendering order is possible because the TIA has no framebuffer — each scanline is generated in real-time as the electron beam sweeps. The code simply outputs scanlines in the hardware's sequence while switching banks mid-frame.

##### Bank 1 Lower Kernels (Variable, ~80-100 Scanlines)

**Entry point**: `.startVisibleFrame`

Immediately after the VBLANK timer expires, Bank 1 clears the VBLANK register and sets up the TIA (`VDELP0/1` enabled, `CTRLPF` reflected + priority). The kernel then branches based on game state:

| Kernel | Condition | Scanlines | Description |
| -------- | ----------- | ----------- | ------------- |
| **Ground Kernel** | All flight phases | 10 | `groundKernel` ($F907): Draws terrain strip using `PF0`/`PF1`/`PF2` from three playfield data tables (`Lfe27`, `Lff01`, `Lfe31`). Color set from sky/ground table based on flight phase. |
| **Launch Dot Kernel** | `flightPhase=1` | 2×10 | Two 10-line sections drawing the trajectory reference dot and plane correction dot. Each dot uses color cycling and horizontal motion to indicate alignment. The gap between dots shows the countdown counter or phase number. |
| **Starfield Kernel** | Orbit phases | 60 | 60-line starfield display: 4 columns of stars using indirect `(gfxDataPtrL),y` reads from `columnGfxPtrTable`. Stars scroll via column rotation and vertical counter. Missile sprites enabled/disabled per-scanline for additional star points. |
| **Landing Kernel** | `flightPhase=4` | ~60 | Altitude-dependent blank lines above terrain. Trajectory and plane dots repositioned for descent indicator. Y-axis indicator via missile. Runway rendering with earth curvature colors. |
| **Cockpit Window** | All flight phases | ~40 | `kernelDrawCockpitWindow`: Renders the starfield visible through the cockpit frame. Uses missile/playfield collision registers (`CXM0FB`/`CXM1FB`) to mask stars behind the cockpit border — a clever technique that uses the TIA's collision detection hardware for display clipping. Horizontal star drift at variable rate based on `movementFlags`. |

##### Bank Switch: Bank 1 → Bank 0

At `exitLogicToKernel`, execution does `sta WSYNC` then `jmp resetBank1`. The first instruction of `resetBank1` is `bit bank0Strobe`, which switches to Bank 0. Execution continues at `startBank0Kernel` — the upper display kernel.

##### Bank 0 Upper Kernels (~90 Scanlines)

**Entry point**: `startBank0Kernel` ($D003)

Bank 0 draws the upper cockpit dashboard from top to bottom:

| Section | Lines | Scanlines | Description |
| --------- | ------- | ----------- | ------------- |
| **TIA Setup** | — | 0 | Clears all sprites, sets colors (BLUE players, YELLOW playfield), positions computer arrow sprites via horizontal positioning subroutine (`$D958`). |
| **Top Dashboard** | `waitForVBlankTimer` | 8 | Playfield border pattern (`PF0`/`PF1`/`PF2`). Draws arrow graphics via `GRP0`/`GRP1`. During orbit with ≥4 dockings, adds random `yAxisPlane` jitter simulating satellite turbulence. |
| **Thrust Bar** | `Ld0bf` | 5 | Draws the "T"/"C" thrust alignment bar. Uses a repeating `GRP0`/`GRP1` write pattern with `RESP0`/`RESP1` coarse repositioning (11 `sta.w` writes) to create a multi-copy bar effect spanning the screen width. |
| **Cockpit Frame** | `kernelDrawCockpitWindows` | 8 | Cockpit window border with `GRP0`/`GRP1` window sprites. Color flash logic for misaligned T/C arrows. Positions thrust arrow (T) sprite horizontally. |
| **Instrument Readout** | `Ld979` subroutine | 6 | **48-pixel multiplexed display**: 6 indirect pointers (`screenPtr1L`–`screenPtr6L`) drive `GRP0`/`GRP1` writes with vertical delay (`VDELP0`/`VDELP1`), rendering a full 6-digit flight instrument readout. The same technique used in many 2600 games for wide score displays. Fuel warning: low fuel blinks `COLUPF`. |
| **Main View Window** | `kernelDrawMainView` | 25 | The central cockpit display showing context-dependent graphics selected by `currentScreenId`. Uses the same 6 indirect pointers to draw one of 7 screen graphics: launch pad, orbit view, satellite approach, reentry, landing/runway, STS-101 mission complete, or an alternate reentry view. Each screen is stored as 5 columns × 25 bytes in Bank 0's data section. |
| **Engine Flame** | *(below main view)* | 8 | Draws engine exhaust below the main view. Conditionally renders flame graphic based on `engineOnTimer`. |
| **Activision Logo** | `kernelDrawStatusBar` | 8 | Renders the Activision copyright strip using 6 `GRP0`/`GRP1` writes per line with `COLUPF` color cycling for the rainbow effect. Sets `VBLANK` at the end — overscan begins. |

##### Display Kernel Order (Screen Top to Bottom)

The complete scanline layout from top to bottom of the visible frame:

```text
 ┌─────────────────────────────────────┐
 │      Cockpit Window / Starfield     │  Bank 1: kernelDrawCockpitWindow
 │         (stars through frame)       │  (~40 lines)
 ├─────────────────────────────────────┤
 │     Starfield / Launch Dots /       │  Bank 1: context-dependent
 │         Landing View                │  (variable, ~60 lines)
 ├─────────────────────────────────────┤
 │          Ground Terrain             │  Bank 1: groundKernel (10 lines)
 ╞═════════════════════════════════════╡
 │  ── Bank Switch: Bank 1 → Bank 0 ──│  exitLogicToKernel
 ╞═════════════════════════════════════╡
 │        Top Dashboard Border         │  Bank 0: waitForVBlankTimer (8 lines)
 ├─────────────────────────────────────┤
 │     T/C Thrust Alignment Bar        │  Bank 0: Ld0bf (5 lines)
 ├─────────────────────────────────────┤
 │       Cockpit Window Frame          │  Bank 0: kernelDrawCockpitWindows (8 lines)
 ├─────────────────────────────────────┤
 │     Instrument Readout (6 digits)   │  Bank 0: Ld979 (6 lines)
 ├─────────────────────────────────────┤
 │       Main View Window              │  Bank 0: kernelDrawMainView (25 lines)
 │   (launch/orbit/satellite/reentry/  │
 │    landing/STS-101 graphics)        │
 ├─────────────────────────────────────┤
 │        Engine Flame Area            │  Bank 0: (8 lines)
 ├─────────────────────────────────────┤
 │   Activision Logo / Copyright       │  Bank 0: kernelDrawStatusBar (8 lines)
 └─────────────────────────────────────┘
```

Note: The screen renders **bottom-up** in code execution order (Bank 1 draws the top of the visible image first, Bank 0 draws the bottom), but the electron beam scans top-to-bottom. The Bank 1 sections actually correspond to the **upper** portion of the TV image, and the Bank 0 sections to the **lower** portion. The diagram above shows the correct spatial layout as seen on screen.

#### Phase 4: Overscan (~30 Scanlines of CPU Time)

**Entry point**: End of `kernelDrawStatusBar` (Bank 0)

After the Activision logo, `VBLANK` is asserted and the overscan timer is armed via `TIM8T`. During overscan, Bank 0 handles time-critical display calculations:

| Step | Label | Description |
| ------ | ------- | ------------- |
| 1 | *(fuel penalty)* | Calculate trajectory/fuel penalty from `fuelPenaltyAccum`. Autopilot: auto-correct pitch toward target value. |
| 2 | **Sound Engine** | Two-channel audio processing. Channel 0: reads `soundEffectId`, advances `soundSequenceIndex` through sound data, sets `AUDC0`/`AUDF0`/`AUDV0`. Sound IDs encode effect type ($24=dock, $34=click, $41=warning, $44=cargo, $58=deorbit, $82=landing, $98=touchdown, $A2=success, $B4=general). Channel 1: engine drone. |
| 3 | **Orbital Movement** | Advances `shuttleOrbitalPos` based on speed and `orbitalMoveFraction`. Rotates starfield column array (`starfieldColumns`, 12 elements) to create parallax scrolling effect. Processes `movementFlags` (bit0=forward, bit1=down, bit2=up, bit3=right). |
| 4 | **Satellite Rendering** | Calculates satellite sprite size (`NUSIZ0`), color (from `satelliteColorOffset`, oscillates 0–7), and horizontal position based on orbital separation (`shuttleOrbitalPos − satelliteOrbitalPos`). Three distance bands determine sprite scale: close (large), medium, far (small dot). |
| 5 | **View Offsets** | Per-screen view offset calculation for `viewVerticalOffset` and `viewHorizontalOffset`. Each `currentScreenId` (0–4) has its own positioning formula for the main cockpit view graphic. |

##### Bank Switch: Bank 0 → Bank 1

At `endOfKernelSwitch`, Bank 0 reads `bank1Strobe` to return to Bank 1. Execution enters `bank1Handler`.

##### Post-Kernel Logic (Bank 1)

| Step | Label | Description |
| ------ | ------- | ------------- |
| 6 | `bank1Handler` | **Cargo door check**: during orbit (`flightPhase=2`), if `cargoDoorTimer` overflows negative, triggers warning sound ($41). If it reaches $FF, aborts mission ($85 — heat buildup). Also handles separation event sequencing. |
| 7 | `waitForTimerVSYNC` | Spins on `INTIM` until overscan timer expires. Pulses VSYNC (2 scanlines). Positions target satellite sprite. Jumps to `mainFrameLoop` — the cycle repeats. |

### Bank Switching Summary

| # | Direction | Trigger | Purpose |
| --- | ----------- | --------- | --------- |
| 1 | Bank 1 → 0 | `exitLogicToKernel` → `jmp resetBank1` → `bit bank0Strobe` | Enter Bank 0 for upper display kernel |
| 2 | Bank 0 → 1 | `endOfKernelSwitch` → `bit bank1Strobe` | Return to Bank 1 for post-kernel logic |

Only **2 bank switches per frame** — significantly simpler than many 2600 games that switch 4+ times.

### Display Techniques

#### 48-Pixel Multiplexed Display

The instrument readout uses the classic 2600 "6-digit score" technique. With vertical delay enabled (`VDELP0`/`VDELP1`), the code writes:

```text
lda (screenPtr1L),y → sta GRP0    ; Digit 1 latched (delayed)
lda (screenPtr2L),y → sta GRP1    ; Digit 2 displayed, Digit 1 released
lda (screenPtr3L),y → sta GRP0    ; Digit 3 latched, Digit 2 released
...repeat for 6 digits...
```

Each write triggers the previously latched value to display, creating a pipeline that renders 6 sprites (48 pixels) across one scanline using only 2 hardware player sprites.

#### Starfield Parallax

The starfield uses a 12-element column array (`starfieldColumns`, $8A–$95) that rotates each frame. The rotation speed depends on orbital velocity, creating a parallax scrolling effect. Four columns are drawn per frame using indirect reads through `columnGfxPtrTable`, with missiles providing additional star points. The `starfieldHorizontalMotion` value shifts columns left/right based on `movementFlags`.

#### Collision-Based Display Clipping

The cockpit window kernel uses an unconventional technique: stars are rendered behind the cockpit frame playfield, then the **TIA collision registers** (`CXM0FB`/`CXM1FB`) are read each scanline to detect whether missile sprites (used as star points) overlap the playfield border. If a collision is detected, the star is hidden — effectively using the hardware collision detection system as a display clipping mask.

#### Reentry Visual Effects

During reentry, two visual effects overlay the normal display:

1. **Ionization heating** (`heatEffectTimer`): In the altitude band $78–$A6, the heat timer increments. The timer value is compared against the PRNG seed — when they match, random color values are written to `COLUP1`, creating the characteristic flickering color flashes of ionization heating.

2. **Atmosphere density** (`atmosphereDensity`): Below the ionization zone, density increments. A similar PRNG comparison produces a grey tint effect simulating atmospheric haze during descent.

### Mission Phases

The game tracks mission progress through two variables: `flightPhase` (overall phase) and `launchPhase` (sub-phase within launch). The complete mission flow:

```text
flightPhase=0  Pre-launch (countdown)
    └── launchPhase 0→1: Countdown activated
    └── launchPhase 3: Engines ignited (MET counting)
    └── launchPhase 5: T+0:03, hold-down bolts release (liftoff)
    └── launchPhase 7: T+0:25, ET separation

flightPhase=1  Ascent (launch to MECO)
    └── MECO at altitude $D2 (210nm) → orbit transition

flightPhase=2  Orbit
    ├── Stabilize: pitch to -28°, open cargo doors
    ├── Docking: match all 5 axes for 2 seconds
    ├── OMS burns: manual or autopilot orbital adjustments
    └── Deorbit burn: reverse orientation, decelerate

flightPhase=3  Reentry
    ├── Entry interface: heating zone ($A7→$78)
    ├── Ionization blackout ($78–$A6)
    └── TAEM: terminal area management

flightPhase=4  Landing
    ├── Final approach (starfieldScrollY countdown)
    └── Touchdown: pitch=0, Y-axis centered, timer expired
```

### Automated Launch Sequencer

During ascent, the game uses a table-driven event system to automate thrust changes. The `launchEventTimings` table contains 14 MET timestamps. Each frame, `metLow` is compared against the table — on a match, `autoThrustCommand` is loaded from the corresponding `launchEventParams` entry. This drives the computer arrow (C), which the player must track with the thrust arrow (T).

| MET Time | Event |
| ---------- | ------- |
| 0:04 | Initial thrust command |
| 0:07 | Roll program |
| 0:10 | Max-Q throttle down |
| 0:13 | Throttle up |
| 0:15 | Phase 2 acceleration |
| 0:22 | Continue acceleration |
| 0:40 | Approaching MECO |
| 0:41 | Fine adjustment |
| 1:05 | High altitude trim |
| 1:06 | Final trim |
| 1:10 | Engine reduction |
| 1:12 | MECO approach |

### Docking System

Docking requires simultaneous satisfaction of 5 conditions, all of which must hold for approximately 2 seconds:

| Condition | Variable | Required Value |
| ----------- | ---------- | ---------------- |
| Z-axis (altitude) aligned | `altitude` | $D2 (210 nm) |
| Y-axis (lateral) aligned | `yAxisPlane` | $00 |
| Pitch aligned | `pitchValue` | $00 |
| X-axis (range) aligned | `targetHorizPos` | $47–$4F range |
| Orbital position matched | `shuttleOrbitalPos` | = `satelliteOrbitalPos` |

The `dockingProgress` counter increments while all conditions hold. At $80, docking is complete. Fuel is added from a progressive table ($15, $20, $25, $30, $35, $40, $40 for dockings 1–7), `missionScore` increments in BCD, and the satellite is ejected to a new position.

After each successful docking, the satellite becomes more erratic — `dockingCount` increases random perturbation applied to `satelliteOrbitalPos` and `yAxisPlane` during the kernel.

### Scoring & Rankings

Upon successful landing (STS-101 mission in Flight #3), the game evaluates performance:

| Screen | Condition | Meaning |
| -------- | ----------- | --------- |
| $05 | Basic landing | Mission Specialist / Payload Specialist |
| $06 | ≥1 successful docking | Pilot rank |
| $07 | 6 dockings AND fuel ≥ 75 | **Commander Patch** (Activision badge) |

The Commander Patch was one of Activision's famous achievement patches — players who earned it could mail a photo of their TV screen to Activision and receive a physical embroidered mission patch.

### Abort System

Critical failures trigger `abortMission` with specific error codes:

| Code | Condition |
| ------ | ----------- |
| $10 | Orbit quality too poor at MECO |
| $65 | Yaw not zeroed for deorbit |
| $70 | Altitude below 195nm (atmospheric burn-up) |
| $75 | Deorbit pitch incorrect |
| $80 | Speed deviation in orbit |
| $85 | Cargo doors not opened (heat buildup) |
| $95 | OMS burn quality failure |
| $99 | Fuel exhausted |

In Training Mode (`trainingModeFlag=$FF`): most aborts are suppressed. The error is displayed on the status indicator but the mission continues. Fuel is not consumed. In Flight #1 (Autosimulator): the autopilot handles most controls automatically; the player only needs to correct Y-axis and land.

### Attract Mode

If no input is detected for an extended period, the game enters attract mode. The `attractSubTimer` increments each frame; when it wraps (every 256 frames), `attractTimer` advances. At threshold $0F, the game auto-starts in autopilot demo mode. Further timeout triggers `screenBlankFlags` cycling, which alternates the VBLANK state to prevent CRT burn-in — a standard Atari practice.

### Key Subroutines

| Label | Address | Description |
| ------- | --------- | ------------- |
| `subtractFuel` | $FCAE | BCD fuel subtraction. Skips in training mode. Underflow → abort ($99). |
| `abortMission` | $FCC7 | Sets `abortCode`, clears game state. Training mode filters abort severity. |
| `Lfce6` | $FCE6 | Queues sound: sets `soundEffectId` and `soundSequenceIndex=$FE`. |
| `Lfd20` | $FD20 | Increase speed: BCD increment `speedFractionLow`/`High`, carry → `inc altitude`. |
| `Lfd42` | $FD42 | Decrease speed: BCD decrement with borrow → `dec altitude`. |
| `initGameVars` | $FE6E | Copies 34 bytes from ROM table (`Lfd64`) to RAM $80–$A1. |
| `Lfe00` | $FE00 | Horizontal positioning: divide-by-15 coarse + HMP0 fine adjust. |
| `Ld958` | $D958 | Bank 0 version of horizontal positioning (identical algorithm). |
| `Ld979` | $D979 | 48-pixel multiplex renderer for instrument readout. |

### Zero-Page RAM Map

The game uses RAM addresses $80–$FD for all variables. Key groups:

| Range | Purpose |
| ------- | --------- |
| $80 | PRNG seed |
| $81–$83 | Thrust arrow positions (player, computer, auxiliary) |
| $84–$85 | Screen ID, status display selection |
| $86–$88 | Trajectory dot, plane dot, pitch |
| $89–$95 | Starfield column array (12 elements + index) |
| $96–$9D | Speed display, fuel, MET (all BCD) |
| $9E–$9F | Mission score, abort code |
| $A0–$A4 | Difficulty, alignment, display conversion, descent rate |
| $A5–$A9 | Altitude, speed, update timers, thrust direction |
| $AA–$AF | Plane correction, movement flags, view offsets, countdown, OMS |
| $B0–$B7 | Orbital positions, yaw, phase tracking, game/engine state |
| $B8–$BC | Attract mode, launch events, auto-thrust command |
| $BE | Orbital movement fraction |
| $C0–$CB | Input timing, frame counter, descent timers, MET frames, console, sound |
| $CC–$D7 | 6 screen data pointer pairs (12 bytes) |
| $D8–$DD | Starfield GFX pointer table + indirect data pointer |
| $DE–$E4 | Docking target, orbital/heat/docking counters, starfield motion |
| $E6–$EF | Cargo door, atmosphere, reentry, satellite, landing, separation, cockpit |
| $F0–$F9 | Cargo door state, error/landing/speed flags, fuel penalty, mode flags, autopilot |
| $FA–$FC | General-purpose scratch variables |
| $FD | Starfield vertical counter |
| $FE–$FF | Stack |
