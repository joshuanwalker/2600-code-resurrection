# Haunted House (Atari 2600) - Reverse Engineered Source

**Original Game (1982) by Atari, Inc.**  
**Original Designer:** James Andreasen  
**Disassembly & Analysis:** Halkun (That's me!)

---

## Overview

This repository contains the fully reverse-engineered and commented source code for the Atari 2600 classic, *Haunted House*.

## Project Structure

The project has been reorganized for a clean development workflow:

* **`src/`**: Contains the main assembly source (`haunted.asm`).
* **`inc/`**: Header files (`tia_constants.h`, `vcs_constants.h`).
* **`bin/`**: Contains build tools (DASM) and emulator executable (Stella).
* **`out/`**: Destination for compiled binaries (`.bin`), symbol files (`.sym`), and listing files (`.lst`). (auto generated at compile time)
* **`Makefile`**: Makefile to compile and launch the compiled ROM in Windows with PowerShell or Linux with make.
* **`docs/`**: Game manual and walkthrough.

## How to Build & Run

### Prerequisites

* Windows OS
* **DASM**: `dasm.exe` must be in the `bin/` folder.
* **Stella**: `Stella.exe` must be in the `bin/` folder (optional, for running).

* Linux
* **dasm** `dasm` must be installed on the system.
* **stella**: `stella` must be installed on the system.

### Compiling

Build the ROM by running make:

```cmd
make haunted
```

### Running

Launch the compiled ROM in Stella:

```cmd
make run GAME=haunted
```

---

## Technical Documentation

### ROM Architecture

The game uses a **single 4K ROM** with no bank-switching, mapped at `$F000`–`$FFFF`. Every byte is accounted for — the ROM has **zero free bytes** remaining. The stack is placed at `$F6`–`$FF`, giving only 10 bytes of stack space (enough for 5 nested subroutine calls). Zero-page RAM (`$80`–`$F5`) is used extensively, with many addresses serving double or triple duty as temporary variables during different phases of the frame.

### Game Loop

Like all Atari 2600 games, the program is structured around the NTSC television signal — one complete pass through the loop produces one frame of video (~60 fps). The frame is divided into four phases: **VSYNC**, **VBLANK**, **Kernel** (visible picture), and **Overscan**. Game logic is split across VBLANK and Overscan to stay within the CPU time budgets of each phase.

#### Frame Overview

```text
mainGameLoop
  ├── VSYNC (3 scanlines)
  │     Increment frameCount
  │
  ├── VBLANK (TIM64T = 45, ~38 scanlines of CPU time)
  │   ├── readSwitches
  │   │     Reset → clear RAM, scatter items, init room
  │   │     Select → cycle game selection (1-9)
  │   │
  │   ├── handlePlayerMovement (skipped if gameState ≠ 0)
  │   │     updatePlayerDelta → joystick → deltas
  │   │     Wall collision via wallBitPattern lookup
  │   │     Stair boundary detection
  │   │
  │   ├── checkForWinning (skipped if gameState ≠ 0)
  │   │     Floor 0, Room 2, carrying completed urn?
  │   │     Torch countdown (20-second timer)
  │   │     Torch flickering animation
  │   │
  │   └── updateGameVisualsAndState
  │         Sprite setup (NUSIZ, colors, enable/disable)
  │         Rolling eyes animation
  │         Item visibility (torch light radius check)
  │         Creature rendering and collision checks
  │         Stair graphic setup
  │         Playfield color/scroll preparation
  │         Random number generation
  │         Object positioning (coarse/fine HMOVE)
  │         ──► Wait on INTIM
  │
  ├── VISIBLE KERNEL (TIM64T = 228, ~192 scanlines)
  │   ├── playfieldKernel
  │   │     Black lines (top border)
  │   │     Playfield loop (PF0/PF1/PF2 walls + sprites)
  │   │     Two interleaved sprite channels (P0 + P1)
  │   │
  │   ├── statusKernel
  │   │     Floor/game number display
  │   │     Inventory item display
  │   │     Torch count (2-digit BCD)
  │   │     Lives count
  │   │
  │   └── Audio engine (runs in kernel exit scanlines)
  │         Wind sound (randomized from program code!)
  │         Creature noise / death sound
  │         Footstep sound
  │         Ending music ("Twilight Zone" melody)
  │
  └── OVERSCAN (TIM64T = 36, ~30 scanlines of CPU time)
      ├── handlePlayerDeath
      │     Death timer countdown, respawn, life loss
      │     Bat steals item (games 7-9)
      │     processCreatures: AI movement + room transitions
      │
      ├── handleStairs
      │     Floor transitions (up/down)
      │     initRoom: room data setup
      │     handleInput: joystick + fire button
      │     Torch lighting (fire button)
      │     Item pickup/drop logic
      │
      ├── checkP0P1Collision
      │     Creature kill check (scepter immunity)
      │     Urn piece assembly (3 pieces → complete urn)
      │     Player death on creature contact
      │
      └── checkDoorwayPassages
            Doorway boundary detection (6 doorways)
            Room transitions via roomToRoomOffsets
            Creature repositioning (games 5-9)
            ──► mainGameLoop (loop closes)
```

#### Phase 1: VSYNC (3 Scanlines)

**Entry point**: `mainGameLoop`

The CPU asserts the VSYNC signal for exactly 3 scanlines. During this time, `VBLANK` is also asserted with `DUMP_PORTS` to ground the paddle inputs, and `frameCount` is incremented. The VBLANK timer is then armed: `TIM64T = 45`, giving 45 × 64 = 2,880 cycles ≈ 38 scanlines of CPU time.

#### Phase 2: VBLANK (~38 Scanlines of CPU Time)

All game logic runs while the TIA outputs a blank screen. If `gameState` has bits 0, 1, or 6 set (death, win, or selection screen), movement and win-checking are skipped.

##### readSwitches

| Condition | Action |
| --------- | ------ |
| RESET pressed | Swap random seeds, clear RAM `$80`–`$95`, copy `initialGameVariables` into `$96`–`$9F`, call `scatterTheItems`, set creature count (3 for games 1-4, 5 for games 5-9), initialize player at (`$80`, `$86`). |
| SELECT pressed | Debounced via `selectSwitchDebounce` (~45 frame cooldown). Increments `gameSelection` (0-8), wraps at 9. Calls `setSelectionVariables` to extinguish torch and reset state. |

##### handlePlayerMovement

Reads `SWCHA` joystick directions and converts them into `playerDeltaX` / `playerDeltaY` movement deltas.

Movement is validated against the room's wall layout using a two-stage collision check:

1. **Horizontal check**: The player's proposed X position is divided by 16 to get a coarse grid column, then `bitmaskThing,y` is ANDed against `wallBitPattern,x` (where X is derived from the Y scroll position). A nonzero result means a wall blocks movement.
2. **Vertical check**: Same process with the Y scroll position as input.

If the player reaches a screen boundary (`playerPosX` = `$04` or `$94`), or a vertical boundary (`playerScrollY` = `$01` or `$FB`), the stair collision flag is set in `roomStairsStatus` bit 7.

Movement produces a footstep sound (`NOISY3SOUND`) on each valid step.

##### checkForWinning

Checks the win condition every frame:

1. `roomStairsStatus` bit 7 must be set (player at a boundary)
2. `playerCurrentFloor` must be 0 (ground floor)
3. `playerCurrentRoom` must be 2 (the exit door)
4. `itemBeingCarried` must be 2 (completed urn)
5. `urnAssembly0` must equal 8 (all pieces assembled)

If all conditions are met, `gameState` is set to `$44` (win state), triggering the ending sequence.

The torch countdown runs here: `secondsCounter` decrements each frame, and every 60 frames (1 second at 60fps), `torchTimer` decrements. When `torchTimer` reaches 0, `unlightTorch` is called. If creatures are present in the room, the torch is also extinguished immediately. The torch flame flickers by toggling bit 7 of `torchAnimationIdx` every frame.

##### updateGameVisualsAndState

This is the largest VBLANK subroutine, handling all per-frame visual setup:

| Step | Description |
| ---- | ----------- |
| Sprite init | Set `NUSIZ0` to 8-pixel missile, clear background/ball/missile, clear `NUSIZ1`. |
| Game state dispatch | If `gameState = $02` (selection mode) skip to input. If bit 2 set (win), run selection variables on frame 0. Otherwise enter rolling eyes / torch / item logic. |
| Rolling eyes | When `rollingEyesTimer > 0`: check collisions, index into `rollingEyesTable` for eye pupil pattern (left/center/right), write to `eyeRAM` (3 bytes in zero-page). |
| Item visibility | `checkItemVisibility`: iterate through 5 items, skip carried items and items on other floors. For each visible item, call `torchLightUpItem` to check if within the torch's light radius. |
| Torch light radius | `torchLightUpItem`: compute Manhattan-ish distance between player and item using `abs(playerPosX - objPosX)` and `abs(playerScrollY - objPosY)`, halve the smaller axis, sum, and compare against `$11`. Items within radius are rendered. |
| Creature rendering | `checkCollisions` → `updateCreaturePosition`: check if creature is on same floor, in same room or adjacent room during door crossing, within torch light. Set creature sprite from `creatureGraphics` with random animation frame selection via `randomSeed` bit 6. |
| Stair rendering | `checkForStairs`: if `roomStairsStatus` low nibble ≠ `$0F`, calculate stair graphic pointer from `stairGraphics` (4 types: up-left/right, down-left/right, up-top/bottom, down-top/bottom). Position via `stairHPositionsTable`. |
| Playfield prep | `preparePlayfield`: set torch sprite (alternating `torch_0`/`torch_1`), calculate ball position for doorway markers, set wall/floor colors from `gameColorTable`, handle lightning flashes (random wall color toggle when creatures present and difficulty switch is set). |
| Color cycling | Apply color EOR and cycle mode to `colorTableRAM` (5 entries: stairs, creature, torch, floor, background). |
| Object positioning | Position all 5 TIA objects (P0, P1, M0, M1, Ball) using the standard coarse/fine HMOVE technique. **Note**: Contains a known bug at the division step — `sbc $0f` reads from zero-page address `$0F` instead of using immediate value `#$0F`. |
| Scroll setup | Calculate playfield scroll offset from `playerScrollY`, set `temporaryOne` for the kernel. |
| Random number | `nextRandom`: 16-bit LFSR using `randomSeed` and `randomSeedAlternate` with XOR and rotate. |

#### Phase 3: Visible Kernel (~192 Scanlines)

**Entry point**: `playfieldKernel`

The kernel is a single unified display routine (no dispatch table) that draws the room walls, sprites, and status bar in one pass.

##### Black Lines (Top Border)

Scanlines count down from `$4F` (79). Lines above `lightningColorMask` are drawn as black pairs (2 WSYNCs per iteration). This creates a variable-height black border at the top of the screen.

##### Playfield Loop

The main display loop draws the room interior:

* **Playfield data**: `pf0Data`, `pf1Data`, `pf2Data` are indexed by X (incremented every 16 scanlines via `temporaryOne` modulo check). The playfield is reflected (`CTRLPF` bit 0) to create symmetric room walls.
* **Player 0 (P0)**: Draws items, creatures, or stairs. Enabled when `scanline - playerPFScrollValue < spriteHeight`. Graphics read from `(sprite0GraphicPTRs),y`.
* **Player 1 (P1)**: Draws the torch flame or rolling eyes. Enabled when `scanline - playerVertOffset < playerVertSize`. Graphics read from `(sprite1GraphicPTRs),y`.
* **Ball + Missile 0**: Used for doorway markers. Enabled/disabled based on `doorwayCrossingStatus` and `gameSelection`.

The two sprite channels are interleaved across alternating scanline pairs — P0 is updated on even lines, P1 on odd lines — allowing both to be drawn with correct timing despite sharing CPU cycles.

##### Status Kernel

After the playfield area ends, a status bar is drawn at the bottom of the screen:

| Element | Position | Method |
| ------- | -------- | ------ |
| Floor/Game number | Left | Single digit via `floorNumberPTRs`, `NUSIZ0` = two medium copies |
| Inventory item | Right of floor | Sprite from `setInvItemPTRs` (key/scepter/urn graphic) |
| Torch count | Center | BCD split into two digits (`torchesHNumberPTRs` / `torchesLNumberPTRs`) |
| Lives count | Right | Single digit via `livesNumberPTRs` |

The status kernel uses `TWO_MED_COPIES` and `TWO_WIDE_COPIES` NUSIZ modes to position the digit sprites. Number graphics are stored at `$FF00`–`$FF4F` (digits 0-9, 8 bytes each), with LSBs calculated arithmetically rather than from a lookup table.

##### Audio Engine

The audio engine runs in the scanlines after the status kernel (piggy-backing on the kernel's return path rather than using a separate overscan phase). It handles:

| Sound | Channel | Trigger |
| ----- | ------- | ------- |
| Wind | AUDC0 | Ambient when no creatures present. Volume oscillates via `windSoundCounter`. **Uses program ROM as random data** (`lda start,x` with `randomSeed` as index) to generate naturalistic wind variation. |
| Creature noise | AUDC0 | Random noise bursts when `gameState` bit 0 set (death). Frequency from `audioFrequency0Value`. |
| Ending music | AUDC0 | "Twilight Zone" melody from `endingMusic` table (4 notes: 19, 18, 19, 23) when `gameState = $44`. |
| Footsteps | AUDC1 | 3-frame pulse every 8 frames while joystick is active. |
| Thunk | AUDC1 | Sound #2 — hitting a wall or wrong item at exit. |
| Item sounds | AUDC1 | Sound #5 (lead) for item drop, #6/#7 for urn assembly, #11 for urn completion. |
| Stair sounds | AUDC1 | Sound #9 (ascending) / #10 (descending) from `stairwaySound` table, played forward/reversed. |

The sound system uses `audioSoundIndex` as a priority dispatcher and `audioVolume1Value` as a decay timer loaded from `audioVolume1ValueTable`.

#### Phase 4: Overscan (~30 Scanlines of CPU Time)

The TIA is blanked and the overscan timer is armed: `TIM64T = 36`, giving 36 × 64 = 2,304 cycles ≈ 30 scanlines. Overscan is skipped entirely if `gameState` bit 6 is set (selection screen).

##### handlePlayerDeath

If `gameState` bit 0 is set (death in progress):

1. Decrement `rollingEyesTimer`. When it reaches 0, clear `gameState` and `audioSoundIndex`.
2. If `playerLives` = 0, call `setSelectionVariables` (game over → return to selection screen).
3. Call `scatterTheItems` to re-randomize item positions.
4. **Bat theft** (games 7-9 only): if `creatureCollisionFlag = 1` (killed by bat) and player was carrying an item, the bat steals it — the item is relocated to the bat's random position (`randFloorLoc1`, `randPosX1`, `randPosY1`).

##### processCreatures (called from handlePlayerDeath)

The creature AI runs every frame during overscan, regardless of death state:

| Step | Description |
| ---- | ----------- |
| Room check | For each creature (X = `numberOfCreatures` down to 0): if on same floor AND same room as player (or same room as `playerDoorCrossing`), increment `creaturesInRoom`. |
| Scepter immunity | If player carries the scepter (`itemBeingCarried = 1`), skip creature movement (except creature 0 / ghost in games 8-9). |
| Speed lookup | `lookupCreatureSpeed`: games 1-7 use `easyCreatureSpeeds`, games 8-9 use `hardCreatureSpeeds`. Speed is a bitmask ANDed with `bitmaskThing,y` (where Y = `frameCount & 7`) to throttle movement to specific frame slots. |
| Movement | Creatures move toward a target position — either the player (if in the same room) or a random room destination. Movement is 1 pixel per eligible frame on each axis independently. |
| Room transitions | When a creature reaches its target position, `creatureProcessMask` is set. `moveCreatureRoom` then navigates the creature to an adjacent room or floor using `objectRoomLocations` (encoded as direction + room) and `roomToRoomOffsets`. |
| Randomization | `randomizeCreature`: picks a random adjacent room for the creature's next destination, validates it against the floor layout tables, and stores the encoded movement in `objectRoomLocations`. |

**Creature speed table:**

| Creature | Easy (games 1-7) | Hard (games 8-9) |
| -------- | ----------------- | ----------------- |
| Ghost | `$AA` (fast) | `$AA` (fast) |
| Bat | `$91` (medium) | `$AA` (fast) |
| Spider(s) | `$88` (slow) | `$91` (medium) |

##### handleStairs

Stair transitions are triggered when `roomStairsStatus` bit 7 is set (player at a stair boundary) and bit 6 is clear (not already transitioning):

1. Call `resetCreaturePositions` (games 5-9 only) to reposition creatures when changing floors.
2. Check `roomStairsStatus` bit 2: if set, increment `playerCurrentFloor` (going up); otherwise decrement (going down).
3. Play stair sound (#9 ascending, #10 descending).
4. Call `initRoom` to set up the new floor's room data.
5. Set `roomStairsStatus` bit 6 (transition complete flag).

##### handleInput (called from handleStairs)

Reads joystick into `movementValue` and handles the fire button:

| Action | Condition | Result |
| ------ | --------- | ------ |
| Light torch | Fire pressed, torch not lit, no creatures in room | Set `torchTimer = TORCH_DURATION` (20 seconds), increment `torchesUsed` (BCD), set `torchAnimationIdx` bit 6. |
| Drop item | Fire pressed, torch lit, carrying item | Play lead sound (#5), call `pickupItem` to finalize drop, set `itemBeingCarried = $FF` (empty). |
| Pick up item | Ball-playfield collision (`CXP0FB`) while `itemGatheredFlag` active | `pickupItem`: set `itemGatheredFlag = 5`, store item index, calculate drop position from `playerScrollOffsets` and player deltas. |

##### checkP0P1Collision

Checks the hardware collision register `CXPPMM` (player 0 vs player 1 overlap):

| Condition | Result |
| --------- | ------ |
| Torch lit + creature overlap | **Player death**: unless carrying scepter (games 1-7) or scepter is ineffective against ghost (games 8-9). Sets `gameState = 1`, `rollingEyesTimer = $FF`, decrements `playerLives`. |
| Torch unlit + item overlap | **Item pickup**: if carrying an urn piece and touching another urn piece, attempt assembly. |
| Urn assembly | Two urn pieces combine: `urnAssembly0`/`1`/`2` track which pieces are joined. When all 3 are combined (`urnAssembly0 = 8`), plays completion sound (#11). Partial assemblies play sound #7. |
| Player hit creature (torch off) | `deadPlayerHandler`: set death state, max out `rollingEyesTimer`, decrement lives, store collision creature index. |

##### checkDoorwayPassages

Tests 6 doorway boundaries (3 horizontal pairs, 3 vertical pairs) from `doorwayBoundaryTable`:

```text
Horizontal doorways: X = $54/$5C, $A4/$AC  (left/right sides of room)
Vertical doorways:   Y = $58/$4F, $A8/$9F, $4F/$47, $48/$50  (top/bottom)
```

Each boundary pair consists of an entry edge and an exit edge. The joystick direction is validated against `joystickValues` to ensure the player is moving through the doorway (not just standing at the edge).

When a doorway transition triggers:

1. `handleDoorway`: set `playerDoorCrossing` to the new room, add `roomToRoomOffsets` (+1, -1, +2, -2) to `playerCurrentRoom`.
2. Call `initRoom` to load the new room's wall/door configuration.
3. In games 5-9, `changeRoom` also calls `resetCreaturePositions` to reposition creatures.

On doorway entry (not yet crossed): set `doorwayCrossingStatus` with bit 7. The game differentiates between "entering a doorway" and "exiting a doorway" for proper two-room rendering (the ball and missile mark the doorway openings).

If the player is carrying an item through a doorway, `initDoorMove` sets `doorwayMovementFlag` to auto-walk the player through the transition.

### Room and Floor System

The mansion is organized as a grid of **rooms** across **4 floors**. Each floor contains up to 6 rooms, and room connectivity is defined by the layout tables.

#### Floor Layout

| Floor | Games 1-8 Layout | Description |
| ----- | ----------------- | ----------- |
| 0 (Ground) | `$21` = rooms 0, 5 | Entrance floor (room 2 is the exit) |
| 1 | `$3B` = rooms 0, 1, 3, 4, 5 | Most rooms accessible |
| 2 | `$3F` = rooms 0, 1, 2, 3, 4, 5 | All rooms accessible |
| 3 (Top) | `$25` = rooms 0, 2, 5 | Attic, fewest rooms |

Game 9 uses an alternate expanded layout (`startRoomLayout + 4`) with different room accessibility.

#### Room Connectivity

Rooms are connected by **doorways** (left/right/up/down passages) and **stairs** (floor transitions). The `roomLayouts` table encodes 24 entries (6 rooms × 4 directions) that define adjacent rooms and stair locations.

`doorsByRoom` defines which directions have doors for each of the 6 rooms:

```text
Room 0: left+right (3)    Room 3: none (0)
Room 1: left+right (3)    Room 4: up (2)
Room 2: right only (1)    Room 5: up (2)
```

`validateRoomExistence` checks whether a target room actually exists on a given floor by looking up `startRoomLayout` and testing the appropriate bit in `bitmaskThing`.

#### Stair Types

Stairs are rendered as P0 sprites from `stairGraphics` (4 variants, 17 scanlines each):

| Type | Pattern | Direction |
| ---- | ------- | --------- |
| `stairs_Ulr` | Left-right rungs going up | Ascending, lateral |
| `stairs_Dlr` | Left-right rungs going down | Descending, lateral |
| `stairs_Dtb` | Horizontal bars going down | Descending, vertical |
| `stairs_Utb` | Horizontal bars going up | Ascending, vertical |

Stair position is looked up from `stairHPositionsTable` indexed by `playerCurrentRoom`.

### Darkness and Torch System

The core mechanic of Haunted House is **darkness** — the player navigates a pitch-black mansion using limited torches.

#### Torch State Machine

`torchAnimationIdx` is a multi-purpose bitfield:

| Bit | Meaning |
| --- | ------- |
| 7 | Flicker toggle (alternates every frame for flame animation) |
| 6 | Torch is lit |
| 5-3 | Unused |
| 2-0 | Animation sub-state |

When **torch is lit** (bit 6 set):

* Room walls are visible (colored by `gameColorTable` based on floor number).
* Items within the torch's light radius are rendered (distance check in `torchLightUpItem`).
* Creatures are visible and can collide with the player.
* The torch flame is drawn as P1 using alternating `torch_0`/`torch_1` graphics (28 scanlines tall, quad-sized sprite).

When **torch is unlit** (bit 6 clear):

* Only the player's **eyes** are visible (3-scanline `eyeRAM` sprite).
* The rolling eyes track joystick direction with animated pupil patterns from `rollingEyesTable`.
* Walls are black, items are invisible, creatures are invisible (but still active!).

#### Torch Timer

Each torch lasts **20 seconds** (`TORCH_DURATION = 20`). The countdown works as:

* `secondsCounter` decrements every frame (starts at 60).
* Every 60 frames (1 second), `torchTimer` decrements.
* When `torchTimer` reaches 0, or when a creature enters the room, `unlightTorch` is called.

Torches used are tracked in BCD format in `torchesUsed` and displayed in the status bar.

#### Light Radius

`torchLightUpItem` calculates an approximate circular distance:

```text
dx = abs(playerPosX - objPosX)
dy = abs(playerScrollY - objPosY)
if dy >= dx: distance = dy + dx/2
else:        distance = dx + dy/2
```

If `distance < $11` (17 pixels), the item is visible. This creates a diamond-shaped visibility area around the player.

#### Lightning Flashes

When creatures are in the room and the left difficulty switch is set (amateur mode, `SWCHB` bit 6 clear), random lightning flashes occur. The flash is triggered when `frameCount & $27 == 0` AND `randomSeed` bit 0 is set, briefly setting `wallColor = 1` (near-black flash that reveals room layout for one frame).

### Creatures

The game features 3 creature types, each with two animation frames stored in `creatureGraphics`:

| ID | Creature | Color | Behavior |
| -- | -------- | ----- | -------- |
| 0 | Ghost | WHITE | Fastest. Immune to scepter in games 8-9. |
| 1 | Bat | GREEN_YELLOW+7 | Medium speed. Steals carried item on contact (games 7-9). |
| 2-4 | Spider(s) | RED+7, ORANGE+7, BLUE_CYAN+7 | Slowest. Multiple spiders in higher games. |

Creature count scales with difficulty:

* **Games 1-4**: 3 creatures (ghost + bat + 1 spider)
* **Games 5-9**: 5 creatures (ghost + bat + 3 spiders)

Creature sprites are 10 scanlines tall with two alternating frames selected randomly via `randomSeed` bit 6.

### Items and Urn Assembly

There are **5 collectible items** scattered randomly across the mansion:

| Index | Item | Graphic | Purpose |
| ----- | ---- | ------- | ------- |
| 0 | Master Key | 8×8 key sprite | Required in some game variations to unlock doors |
| 1 | Scepter | 8×8 scepter sprite | Protects from creatures (except ghost in games 8-9) |
| 2 | Left Urn Piece | Half-urn graphic | Combine to build the urn |
| 3 | Center Urn Piece | Urn stem/neck | Combine to build the urn |
| 4 | Right Urn Piece | Half-urn graphic | Combine to build the urn |

Items are scattered at game start by `scatterTheItems`, which ensures no item is placed in the player's starting location (room 2, floor 1). Game 3 has special handling that places the master key near the player's starting position.

#### Urn Assembly

The urn is assembled by carrying one piece and touching another. Three tracking variables handle the combinations:

* `urnAssembly0`: set to partial combination ID (e.g., 5 for left+center, 7 for center+right) or 8 when complete.
* `urnAssembly1`: set to `$FF` when pieces 2+3 or 2+4 are combined.
* `urnAssembly2`: set to `$FF` when pieces 3+4 or all three are combined.

The assembled urn graphics progress through partial sprites (`object 5-7` in `bitmaskThing` graphics area) to the `Complete Urn` at object 8. The urn must be fully assembled (`urnAssembly0 = 8`) and carried to the exit (floor 0, room 2) to win.

### Game Variations

There are **9 game variations** selected via the SELECT switch:

| Game | Key Required | Creatures | Scepter | Bat Steals | Creature Speed | Floor Layout |
| ---- | ------------ | --------- | ------- | ---------- | -------------- | ------------ |
| 1 | No | Always visible | Protects | No | Easy | Standard |
| 2 | No | In room only | Protects | No | Easy | Standard |
| 3 | Yes (nearby) | In room only | Protects | No | Easy | Standard |
| 4 | Yes | In room only | Protects | No | Easy | Standard |
| 5 | Yes | In room only | Protects | No | Easy | Standard |
| 6 | Yes | In room only | Protects | No | Easy | Standard |
| 7 | Yes | In room only | Protects | Yes | Easy | Standard |
| 8 | Yes | In room only | No vs ghost | Yes | Hard | Standard |
| 9 | Yes | In room only | No vs ghost | Yes | Hard | Expanded |

* **Games 1-2**: Key not required. Game 1 shows creatures everywhere (no room restriction).
* **Games 3-6**: Escalating difficulty with key required.
* **Games 7-9**: Bat can steal your items. Ghost ignores scepter in games 8-9. Game 9 uses expanded floor layout.

### Display Techniques

#### Interleaved Two-Sprite Kernel

The playfield kernel draws two independent sprite channels using a single-pass interleaved approach:

* **Even sub-lines**: Update `GRP0` (items/creatures/stairs) by comparing `scanline - playerPFScrollValue` against `spriteHeight`.
* **Odd sub-lines**: Update `GRP1` (torch/eyes) by comparing `scanline - playerVertOffset` against `playerVertSize`.

Each loop iteration covers 2 scanlines. The wall color (`COLUPF`) is written once per iteration, and PF0/PF1/PF2 are updated every 16 scanlines.

#### Playfield Scrolling

Room walls are defined in a shared `pf0Data`/`pf1Data`/`pf2Data` table (interleaved 3-byte groups). The kernel indexes into this table with X, which is incremented every 16 display lines. Vertical scrolling within a room is achieved by varying the starting X offset based on `playerScrollY`.

The scroll calculation converts `playerScrollY` into a coarse offset (`temporaryOne`) and fine offset (`temporaryOne + 1`) using division by 16.

#### Doorway Markers

Doorways between rooms are marked by the **Ball** and **Missile 0** TIA objects, positioned at the doorway boundaries. These are only enabled when `gameSelection > 0` (not in game 1) and `doorwayCrossingStatus` bit 7 is clear (not mid-transition).

The ball position tracks `playerPosX - $0D` (slightly left of player), clamped to screen bounds.

#### Status Bar Number Display

Number fonts are stored at page `$FF00` with each digit occupying exactly 8 bytes. The LSB for any digit is calculated as `digit * 8`, eliminating the need for a lookup table. The status kernel uses `NUSIZ` mode switching between `TWO_MED_COPIES` and `TWO_WIDE_COPIES` to position the torch count digits and lives counter at different horizontal offsets from a single `RESP0`/`RESP1` strobe.

### Known Bug

In `updateGameVisualsAndState` at the object positioning loop, the instruction `sbc $0f` reads from **zero-page address `$0F`** (which is in the TIA register space) instead of the intended **immediate value `#$0F`**. This means the division step uses whatever value happens to be in `$0F` (CXM1FB — a collision latch) instead of dividing by 15. The bug is present in all original cartridges and does not crash the game, but causes slightly imprecise horizontal object positioning.

### Win Condition

Implemented in `checkForWinning`. All five conditions must be true simultaneously:

1. `roomStairsStatus` bit 7 is set (player at boundary)
2. `playerCurrentFloor == 0` (ground floor)
3. `playerCurrentRoom == 2` (exit door)
4. `itemBeingCarried == 2` (carrying the urn)
5. `urnAssembly0 == 8` (urn fully assembled)

When all conditions are met, `gameState` is set to `$44` and the ending sequence begins — the "Twilight Zone" melody plays and the screen enters color cycling mode.

### Data Tables

| Table | Address | Size | Purpose |
| ----- | ------- | ---- | ------- |
| `bitmaskThing` | `$FE00` | 8 bytes | Power-of-2 bitmask lookup (bit 0-7) |
| `creatureGraphics` | `$FE50` | 50 bytes | Ghost, bat, spider sprites (2 frames each) |
| `stairGraphics` | `$FE8C` | 68 bytes | 4 stair types (17 bytes each) |
| `pf0Data`/`pf1Data`/`pf2Data` | `$FED0` | 48 bytes | Room wall playfield patterns |
| Number fonts | `$FF00` | 80 bytes | Digits 0-9 (8 bytes each) |
| `torchGraphics` | `$FF50` | 56 bytes | 2 torch flame frames (28 bytes each) |
| `gameColorTable` | `$FFE0` | 24 bytes | Color + B/W palettes (12 entries each) |
| `wallBitPattern` | `$FFD0` | 16 bytes | Wall collision grid |
| `itemIDTable` | `$F49F` | 5 bytes | Item type IDs for the 5 objects |
| `initialGameVariables` | `$FDF6` | 10 bytes | Default RAM values on reset |
| `easyCreatureSpeeds` | `$FCB4` | 5 bytes | Per-creature speed masks (games 1-7) |
| `hardCreatureSpeeds` | `$FCB9` | 5 bytes | Per-creature speed masks (games 8-9) |
| `startRoomLayout` | `$FDDD` | 8 bytes | Room existence bitmasks per floor |
| `startFloorLayout` | `$FDE5` | 7 bytes | Stair existence bitmasks per floor |
| `doorsByRoom` | `$FD2C` | 6 bytes | Door direction bits per room |
| `randomLocationsTableH/V` | various | 19+19 bytes | Item scatter position tables |
| `roomToRoomOffsets` | various | 4 bytes | Room adjacency deltas (+1, -1, +2, -2) |
| `audioVolume1ValueTable` | various | 12 bytes | Sound effect volume/duration values |
| `endingMusic` | various | 4 bytes | Twilight Zone note sequence |
| `stairwaySound` | various | 4 bytes | Ascending/descending stair frequencies |

### Zero-Page RAM Map

| Address | Variable | Description |
| ------- | -------- | ----------- |
| `$80` | `rollingEyesTimer` | Death animation timer (counts down from $FF) |
| `$81` | `itemGatheredFlag` | Item pickup state (5 = just picked up, counts down) |
| `$82` | `torchesUsed` | BCD count of torches lit |
| `$83` | `creaturesInRoom` | Number of creatures on current screen |
| `$84` | `colorCycTimer` | Color cycling animation timer |
| `$85` | `collisionIndex` | Current collision entity being checked |
| `$86` | `itemLastSeen` | Index of last item rendered |
| `$87` | `secondsCounter` | Frame-to-second converter (60→0) |
| `$88` | `doorwayCrossingStatus` | Doorway transition state (bit 7 = active) |
| `$89` | `frameCount` | Frame counter |
| `$8A` | `torchAnimationIdx` | Torch state (bit 7=flicker, bit 6=lit) |
| `$8B`-`$8C` | `pfScrollOffsetA/B` | Playfield scroll positions for adjacent rooms |
| `$8D` | `playerCurrentFloor` | Current floor (0-3) |
| `$8E` | `movementValue` | Joystick direction value |
| `$8F` | `audioFrequency0Value` | Channel 0 frequency counter |
| `$90` | `audioVolume1Value` | Channel 1 volume/decay timer |
| `$91` | `doorwayMovementFlag` | Auto-walk through doorway flag |
| `$92` | `audioWindSoundBit` | Wind direction bit for ambient sound |
| `$93` | `audioSoundIndex` | Current sound effect priority index |
| `$94` | `creaturesPresentMask` | Bitmask of creatures in current room |
| `$95` | `creatureProcessMask` | Bitmask of creatures awaiting room transition |
| `$96` | `playerLives` | Remaining lives |
| `$97` | `playerDoorCrossing` | Room number being crossed into |
| `$98` | `windSoundCounter` | Wind sound oscillator counter |
| `$99` | `gameState` | State machine (0=play, 1=death, 2=select, $44=win) |
| `$9A` | `itemBeingCarried` | Item index ($FF = none) |
| `$9B` | `roomStairsStatus` | Stair/door bitfield for current room |
| `$9C` | `playerCurrentRoom` | Current room number (0-5) |
| `$9D`-`$9F` | `urnAssembly0/1/2` | Urn piece assembly state |
| `$A0`-`$A4` | `objFloorLoc[5]` | Floor locations for 5 items |
| `$A5`-`$A9` | `randFloorLoc[5]` | Randomized floor locations for creatures |
| `$AA` | `playerPosX` | Player horizontal position |
| `$AB`-`$AF` | `objPosX[5]` | Item horizontal positions |
| `$B0`-`$B4` | `randPosX[5]` | Creature horizontal positions |
| `$B5` | `playerScrollY` | Player vertical scroll position |
| `$B6`-`$BA` | `objPosY[5]` | Item vertical positions |
| `$BB`-`$BF` | `randPosY[5]` | Creature vertical positions |
| `$C0`-`$C4` | `objectRoomLocations[5]` | Encoded room+direction for creatures |
| `$C5`-`$C9` | `randomRoomLocations[5]` | Current room for each creature |
| `$CA` | `creatureCollisionFlag` | Which creature killed the player |
| `$CB` | `playerAbsPosY` | Player absolute Y (clamped scroll) |
| `$CC` | `gameSelection` | Game variation (0-8) |
| `$CD` | `scanline` | Kernel scanline counter |
| `$CF` | `tmpBallHorizPosition` | Ball position staging |
| `$D0`-`$D3` | *(temporaries)* | Multi-use scratch variables |
| `$D4`-`$DB` | *(temporaries/pointers)* | Status bar digit pointers, eye RAM |
| `$DC`-`$E0` | `spriteHorizPositions[5]` | TIA object X positions (P0,P1,M0,M1,Ball) |
| `$E1`-`$E4` | `sprite0/1GraphicPTRs` | 16-bit pointers to sprite graphics |
| `$E5`-`$E6` | `selectSwitchDebounce` / `actionButtonDebounce` | Input debounce counters |
| `$E7`-`$E8` | `playerDeltaX/Y` | Movement deltas |
| `$E9` | `itemActionIndex` | Item being manipulated |
| `$EA` | `numberOfCreatures` | Total creature count (2 or 4) |
| `$EB`-`$EC` | `randomSeed` / `randomSeedAlternate` | 16-bit LFSR state |
| `$ED` | `spriteHeight` | P0 sprite height |
| `$EE` | `playerVertSize` | P1 sprite height |
| `$EF` | `playerPFScrollValue` | P0 vertical scroll offset |
| `$F0` | `playerVertOffset` | P1 vertical offset |
| `$F1`-`$F5` | `colorTableRAM[5]` | Runtime color palette |
| `$F6`-`$FF` | *(stack)* | Hardware stack (10 bytes) |
