### Picking up where we left off

In the previous post I introduced ACE BASIC v2.5 and its new AGA screen support. Since then, development has continued and version 2.6 is now available. This release focuses on three main areas: a high-level interface for GadTools gadgets, an ASSERT statement for defensive programming, and native 68020 code generation for faster arithmetic.

Let's dive in.

### GadTools Gadget Support

GadTools is an Amiga system library that provides standardized, Intuition-aware gadgets with a modern look and feel. Before v2.6, using GadTools from ACE required about 230 lines of boilerplate code: opening libraries, setting up visual info structures, creating gadget lists, and carefully managing memory. Now, all of that is handled by a single `GADGET` statement.

The new syntax supports these gadget types:

| Type | Description |
|------|-------------|
| BUTTON_KIND | Push button |
| CHECKBOX_KIND | Boolean checkbox |
| INTEGER_KIND | Numeric input field |
| STRING_KIND | Text input field |
| LISTVIEW_KIND | Scrollable list |
| MX_KIND | Mutual-exclude radio buttons |
| CYCLE_KIND | Dropdown cycle gadget |
| PALETTE_KIND | Color palette chooser |
| SCROLLER_KIND | Scroll bar |
| SLIDER_KIND | Horizontal or vertical slider |
| TEXT_KIND | Read-only text display |
| NUMBER_KIND | Read-only numeric display |

Each gadget is created with a single line that specifies its ID, position, type, and any GadTools tags for customization.

**Note:** ACE already had a `GADGET` command for legacy Intuition gadgets. The syntax is similar but not identical. Legacy gadgets use numeric types (1=BUTTON, 2=STRING, 3=LONGINT, 4=POTX, 5=POTY) and numeric style parameters. GadTools gadgets use `_KIND` constants and flexible `TAG=value` pairs for configuration. They also require a label parameter and provide the modern 2.0+ look. Both syntaxes remain available -- use legacy gadgets for simple cases or Kickstart 1.x compatibility, and GadTools gadgets for richer interfaces on AmigaOS 2.0+.

### Example: A GadTools GUI

Let's walk through a complete example that creates a window with a slider, a string gadget, and a button. This is based on <a href="https://github.com/mdbergmann/ACEBasic/blob/master/examples/gui/GTGadgets.b" target="_blank" class="link">\[examples/gui/GTGadgets.b\]</a> in the ACE distribution.

#### Setting up constants

First, we define constants for our gadget IDs and the window close event:

```basic
CONST GAD_SLIDER = 1
CONST GAD_STRING = 2
CONST GAD_BUTTON = 3
CONST WIN_CLOSE = 256
```

Each gadget needs a unique ID so we can identify which one triggered an event. The special value 256 indicates that the user clicked the window's close button.

#### Opening a window

```basic
WINDOW 1,"GadTools Gadget Demo",(0,0)-(400,100),30
```

This opens window 1 with the given title, positioned at (0,0) with a size of 400x100 pixels. The flags value 30 enables the close button, drag bar, depth gadget, and sizing gadget.

#### Setting the gadget font (optional)

```basic
GADGET FONT "topaz.font", 8
```

The `GADGET FONT` command specifies which font GadTools should use for rendering gadget labels and text. Here we use the classic Topaz 8-point font. If omitted, the system default font is used.

#### Creating the gadgets

Now we create our three gadgets:

```basic
GADGET GAD_SLIDER, ON, "Speed:   ", (100,20)-(300,32), SLIDER_KIND, GTSL_Min=1, GTSL_Max=20, GTSL_Level=5, GTSL_LevelFormat="%2ld", GTSL_MaxLevelLen=2
GADGET GAD_STRING, ON, "Type Here:", (100,40)-(300,54), STRING_KIND, GTST_String="Hello World!", GTST_MaxChars=50
GADGET GAD_BUTTON, OFF, "Click Here", (150,60)-(250,72), BUTTON_KIND
```

The `GADGET` statement takes these parameters:

- **ID**: The gadget's unique identifier (our constants)
- **State**: `ON` to enable or `OFF` to initially disable the gadget
- **Label**: Text displayed next to the gadget
- **Position**: Bounding rectangle as `(left,top)-(right,bottom)`
- **Type**: One of the `_KIND` constants
- **Tags**: Optional GadTools tag=value pairs for customization

The slider uses `GTSL_Min`, `GTSL_Max`, and `GTSL_Level` to set its range and initial value. The `GTSL_LevelFormat` tag displays the current value using printf-style formatting. The string gadget uses `GTST_String` for its initial content and `GTST_MaxChars` to limit input length.

#### The event loop

With the gadgets created, we enter an event loop:

```basic
LONGINT terminated, gad
terminated = 0

WHILE terminated = 0
  GADGET WAIT 0
  gad = GADGET(1)

  CASE
    gad = GAD_SLIDER : MsgBox "Speed: "+STR$(GADGET(3)),"OK"
    gad = GAD_STRING : MsgBox CSTR(GADGET(2)),"OK"
    gad = GAD_BUTTON : BEEP
    gad = WIN_CLOSE  : terminated = 1
  END CASE
WEND
```

`GADGET WAIT 0` blocks until the user interacts with a gadget or the window. The parameter 0 means wait indefinitely. After an event, `GADGET(1)` returns the ID of the gadget that was activated. For sliders and string gadgets, `GADGET(3)` and `GADGET(2)` return the current numeric value and string content respectively.

The `CASE` statement dispatches based on which gadget triggered the event. When the slider changes, we show its value in a message box. When the user presses Enter in the string gadget, we display what they typed. The button just beeps, and the window close event sets the termination flag.

#### Cleanup

Finally, we close everything in reverse order:

```basic
GADGET CLOSE GAD_BUTTON
GADGET CLOSE GAD_STRING
GADGET CLOSE GAD_SLIDER
WINDOW CLOSE 1
END
```

That's it -- a complete GadTools GUI in about 30 lines instead of 230.

#### Runtime attribute access

Version 2.6 also adds `GADGET SETATTR` and `GADGET GETATTR` for modifying gadget properties at runtime:

```basic
GADGET SETATTR GAD_SLIDER, GTSL_Level=10   ' Set slider to 10
level& = GADGET GETATTR(GAD_SLIDER, GTSL_Level)  ' Read current value
```

This allows dynamic UI updates without recreating gadgets.

### The ASSERT Statement

Defensive programming is about catching errors early. The new `ASSERT` statement helps with that:

```basic
ASSERT expression [, "message"]
```

If the expression evaluates to false (zero), ACE halts execution and prints an error message. If the expression is true (non-zero), execution continues silently.

```basic
SUB ProcessData(ADDRESS buffer, LONGINT size)
  ASSERT buffer <> 0, "ProcessData: buffer cannot be null"
  ASSERT size > 0, "ProcessData: size must be positive"

  ' ... proceed with processing
END SUB
```

When an assertion fails, you get immediate feedback about what went wrong and where. The optional message string helps identify the problem without needing a debugger.

ASSERT is particularly useful during development. You can sprinkle assertions throughout your code to verify invariants, check preconditions, and catch logic errors before they cause mysterious crashes. In production, the assertions serve as documentation of your assumptions.

### 68020 Code Generation

The Motorola 68000 CPU in the original Amiga does not have native 32-bit multiply and divide instructions. ACE works around this by calling library routines for these operations. This works, but it is slow.

The 68020 and later processors (68030, 68040, 68060, and the Vampire accelerators) do have native 32-bit arithmetic instructions: `MULS.L`, `DIVS.L`, and `DIVSL.L`. Version 2.6 can now generate these directly.

To enable 68020 code generation, compile with the `-2` flag:

```
ace -2 myprogram.b
```

When should you use this? If your target hardware is an A1200, A3000, A4000, or any accelerated Amiga, the -2 flag can significantly speed up integer-heavy code. Loops with multiplication or division inside see the biggest gains -- the native instructions are several times faster than the library calls.

Here is a simple benchmark:

```basic
DEFLNG a-z

start& = TIMER
FOR i = 1 TO 1000000
  result = i * 7 / 3
NEXT i
elapsed& = TIMER - start&

PRINT "Time: "; elapsed&; " ticks"
```

On a 68060 at 50 MHz, this loop runs about 3x faster when compiled with -2. On a Vampire V4 the difference is even more pronounced because the FPGA-based CPU executes the native instructions very efficiently.

Note that executables compiled with -2 will not run on a stock 68000 machine. If you need to support all Amigas, compile without the flag and accept the slower library calls. If you know your audience has accelerated hardware, use -2 for the extra speed.

### What's Coming in v2.7

Development continues. Here is a preview of what's planned for version 2.7:

- **MUI (Magic User Interface)**: High-level support for MUI, the popular object-oriented GUI toolkit for AmigaOS. MUI offers sophisticated widgets, automatic layout, and a consistent look across applications.

- **INVOKE and BIND**: These new statements enable functional programming patterns. `BIND` captures the current value of variables and associates them with a subroutine, and `INVOKE` calls it. This is effectively currying rather than true closures -- the bound values are captured at bind time and do not reflect later changes to the original variables. True closure semantics may be added in a future version.

- **Bug fixes**: As always, various fixes and improvements based on user feedback.

### Conclusion

ACE v2.6 makes GUI programming dramatically easier with built-in GadTools support, adds ASSERT for catching bugs early, and offers 68020 code generation for faster arithmetic on accelerated hardware. Combined with the AGA support from v2.5, ACE is becoming a capable tool for modern Amiga development.

The project lives on <a href="https://github.com/mdbergmann/ACEBasic" target="_blank" class="link">\[GitHub\]</a>. Bug reports and feature requests are welcome.
