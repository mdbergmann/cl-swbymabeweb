### A bit of history

ACE is a freely distributable AmigaBASIC compiler originally written by David Benn. It takes BASIC source code and produces Motorola 68000 assembly, which is then assembled and linked into a native Amiga executable. ACE supports a large subset of AmigaBASIC and adds many features on top: recursion, structures, turtle graphics, shared library access, subprogram modules, and more.

The last official release was in 1998. After that, ACE went silent.

Last year I was looking for a high-level language to program on the Amiga. I evaluated BlitzBasic and a few other BASIC dialects, but ACE stood out. It is simple, produces standalone executables, and gives you direct access to the Amiga operating system. I tried a few things, liked what I saw, and just recently decided to pick up development. The project now lives on <a href="https://github.com/mdbergmann/ACEBasic" target="_blank" class="link">\[GitHub\]</a> (please file tickets there if you find any bugs).

### What's new in v2.5

Version 2.5 is the first release under the new stewardship. Here is a summary of what changed:

- **AGA Screen Support (modes 7-12)**: Full support for AGA chipset screens with up to 256 colors (8-bit depth), including HAM8 modes.
- **Modern toolchain**: vasm and vlink replace the legacy a68k assembler and blink linker.
- **FFP/vbcc compatibility fix**: Fixed Motorola Fast Floating Point handling in the runtime library for vbcc compatibility.
- **GNU Makefile build system**: New Makefiles replacing the old AmigaDOS build scripts.
- **Project housekeeping**: Directory restructuring, documentation consolidated, and a test suite with 35 test cases covering syntax, arithmetic, floats, and control flow.

The most visible new feature is AGA support, so let's look at that in more detail.

### AGA Screen Support

AGA (Advanced Graphics Architecture) is the Amiga's third-generation chipset, found in the A1200, A4000, and CD32. It supports screen modes with up to 256 colors from a 24-bit palette (8 bitplanes), as well as HAM8 which can display up to 262,144 colors.

Previous versions of ACE only supported OCS/ECS screen modes (modes 1-6) with a maximum of 32 colors (5 bitplanes). Version 2.5 adds six new modes:

| Mode | Description | Max Colors |
|------|-------------|------------|
| 7 | Lores AGA | 256 |
| 8 | Hires AGA | 256 |
| 9 | Super-Hires AGA | 256 |
| 10 | HAM8 Lores | 262,144 |
| 11 | HAM8 Hires | 262,144 |
| 12 | HAM8 Super-Hires | 262,144 |

A new `CHIPSET` function allows runtime detection of the installed chipset (0 = OCS, 1 = ECS, 2 = AGA), so programs can check for AGA before attempting to open an AGA screen. The `PALETTE` command now works with all 256 color registers using 24-bit precision via the system's `SetRGB32()` call on AGA hardware.

### Example: 256 colors on an AGA screen

Let's walk through a complete example that opens an AGA screen and displays all 256 colors as gradient bars.

#### Checking for AGA

First, we check whether the machine actually has an AGA chipset. If not, the program prints a message and stops.

```basic
DEFLNG a-z

IF CHIPSET < 2 THEN
  PRINT "This demo requires AGA chipset."
  PRINT "Please run on A1200, A4000, or CD32."
  STOP
END IF
```

`CHIPSET` returns 0 for OCS, 1 for ECS, and 2 for AGA. The `DEFLNG a-z` directive at the top makes all variables default to long integers, which is generally a good idea for performance on the 68000.

#### Opening the screen

Next, we open a 256-color AGA lores screen:

```basic
SCREEN 1,320,200,8,7
```

The parameters are: screen ID (1), width (320), height (200), depth (8 bitplanes = 256 colors), and mode (7 = AGA lores). Under the hood, ACE uses `OpenScreenTagList()` with the appropriate AGA mode ID to set up the screen.

#### Setting up the palette

With 256 color registers available, we set up four gradient ramps: red, green, blue, and gray. Each gradient uses 64 colors.

```basic
'..Colors 0-63: Red gradient
FOR i = 0 TO 63
  PALETTE i, i/63, 0, 0
NEXT i

'..Colors 64-127: Green gradient
FOR i = 0 TO 63
  PALETTE i+64, 0, i/63, 0
NEXT i

'..Colors 128-191: Blue gradient
FOR i = 0 TO 63
  PALETTE i+128, 0, 0, i/63
NEXT i

'..Colors 192-255: Gray gradient
FOR i = 0 TO 63
  PALETTE i+192, i/63, i/63, i/63
NEXT i
```

The `PALETTE` command takes a color index and three floating-point values for red, green, and blue intensity in the range 0.0 to 1.0. On AGA hardware this maps to full 24-bit color precision via `SetRGB32()`. On OCS/ECS the same command uses `SetRGB4()` with only 12-bit precision.

#### Drawing the color bars

We open a borderless window on the screen and draw four horizontal bars, one for each gradient:

```basic
WINDOW 1,,(0,0)-(320,200),32,1

PRINT "AGA 256-Color Demo"
PRINT "Mode 7: 320x200, 8 bitplanes"
PRINT

'..Red bar
FOR c = 0 TO 63
  COLOR c
  LINE (c*5,50)-(c*5+4,70),,bf
NEXT c

'..Green bar
FOR c = 0 TO 63
  COLOR c+64
  LINE (c*5,80)-(c*5+4,100),,bf
NEXT c

'..Blue bar
FOR c = 0 TO 63
  COLOR c+128
  LINE (c*5,110)-(c*5+4,130),,bf
NEXT c

'..Gray bar
FOR c = 0 TO 63
  COLOR c+192
  LINE (c*5,140)-(c*5+4,160),,bf
NEXT c
```

`COLOR` sets the current drawing color to a palette index. `LINE (x1,y1)-(x2,y2),,bf` draws a filled rectangle (the `bf` flag stands for "box fill"). Each bar consists of 64 small rectangles, each in a slightly different shade.

#### Waiting and cleanup

Finally, we wait for a keypress and close everything:

```basic
COLOR 255
LOCATE 23,1
PRINT "Press any key to exit";

WHILE INKEY$="":SLEEP:WEND

WINDOW CLOSE 1
SCREEN CLOSE 1
```

The `SLEEP` in the wait loop is important -- without it the program would busy-wait and hog the CPU. On the Amiga, being friendly to other tasks matters.

### Conclusion

AGA support in ACE v2.5 opens up 256-color and HAM8 screen modes for BASIC programmers on A1200, A4000, and CD32 hardware. Combined with runtime chipset detection, programs can gracefully handle different Amiga configurations while taking advantage of the more capable hardware when available.

There is more to come. Version 2.6 already adds GadTools gadget support, an ASSERT statement, and native 68020 code generation. But that is a topic for another post.
