### Another round of updates

The previous posts covered ACE BASIC up to v2.8 -- closures, MUI, linked lists, and CubicIDE integration. Version 2.9 is out now with struct enhancements, new string functions, RTG graphics, tail-call optimization, an HTTP client, double-precision floats, and SAGA audio.

### Struct Enhancements

Structs in ACE have been fairly basic until now -- flat collections of scalar and string fields. Version 2.9 changes that. Structs can now contain typed arrays, embed other structs, hold typed pointers to structs, reference their own type, and contain arrays of structs. This makes it possible to model real data structures without falling back to raw PEEK/POKE arithmetic.

#### Typed array members

Previously, only `STRING` could use the `SIZE` keyword to declare a fixed-size buffer inside a struct. Now any base type works:

```basic
STRUCT Packet
  BYTE header SIZE 4
  LONGINT values SIZE 10
  SHORTINT flags SIZE 8
  SINGLE coords SIZE 3
  STRING name SIZE 32
END STRUCT
```

Each array member reserves the appropriate amount of space inline in the struct. Access is through indexed notation:

```basic
DECLARE STRUCT Packet p

p->values(0) = 100&
p->values(1) = 200&
p->coords(0) = 1.5
p->coords(2) = 3.5

FOR i% = 0 TO 3
  p->header(i%) = 65 + i%
NEXT i%
```

The index can be a constant, a variable, or an expression. The compiler generates the correct element size multiplication and offset calculation automatically.

#### Nested structs

Structs can now embed other structs as members. The `->` operator chains through the nesting:

```basic
STRUCT Vec2
  LONGINT x
  LONGINT y
END STRUCT

STRUCT Rect
  Vec2 topLeft
  Vec2 bottomRight
END STRUCT

DECLARE STRUCT Rect r

r->topLeft->x = 10&
r->topLeft->y = 20&
r->bottomRight->x = 100&
r->bottomRight->y = 200&
```

This also works with deeper nesting. A struct that embeds a struct that embeds another struct gives you three levels of `->` chaining. The compiler resolves the offsets at compile time, so there is no runtime cost for the nesting.

#### Typed struct pointers

A struct member can be declared as a pointer to a specific struct type. This tells the compiler what type lives at the other end, so you can chain `->` through the pointer:

```basic
STRUCT Inner
  LONGINT x
  LONGINT y
END STRUCT

STRUCT Outer
  Inner *ptr
  LONGINT z
END STRUCT

DECLARE STRUCT Outer o

o->ptr = ALLOC(SIZEOF(Inner))
o->ptr->x = 42&
o->ptr->y = 99&
```

Without typed pointers, you would have to store a plain `ADDRESS`, cast it manually, and use PEEK/POKE. With typed pointers, the compiler knows the layout and does the offset math for you.

#### Self-referential structs and struct arrays

A struct can contain a pointer to its own type, which is the classic building block for linked lists and trees:

```basic
STRUCT Node
  STRING name
  Node *next
END STRUCT
```

And you can embed a fixed-size array of structs inside another struct:

```basic
STRUCT Vec2
  LONGINT x
  LONGINT y
END STRUCT

STRUCT Polygon
  Vec2 pts SIZE 5
  LONGINT count
END STRUCT

DECLARE STRUCT Polygon poly

poly->pts(0)->x = 10&
poly->pts(0)->y = 20&
poly->pts(2)->x = 50&
```

The syntax `poly->pts(i)->field` combines struct array indexing with field access in a single expression. This is probably the most complex access pattern ACE supports now, and it works with variable indices and in loops.

### New String Functions

Version 2.9 adds eleven new string functions. Here is a quick overview:

| Function | Description |
|----------|-------------|
| `TRIM$` / `LTRIM$` / `RTRIM$` | Strip leading/trailing whitespace |
| `STARTSWITH` / `ENDSWITH` | Test prefix or suffix (returns boolean) |
| `RINSTR` | Search for substring from the right |
| `REPLACE$` | Replace all occurrences of a substring |
| `REVERSE$` | Reverse a string |
| `REPEAT$` | Repeat a string N times |
| `LPAD$` / `RPAD$` | Pad to a given width |
| `FMT$` | sprintf-style formatting |
| `MID$` (statement) | In-place modification of a substring |

The one I find most useful is `FMT$`. It works like C's `sprintf` but returns a BASIC string:

```basic
msg$ = FMT$("%s has %d items", "list", 42)
'..result: "list has 42 items"

hex$ = FMT$("addr: %08x", 255)
'..result: "addr: 000000FF"
```

It supports `%s`, `%d`, `%x`, `%c`, and `%%` with up to eight arguments. This is much cleaner than concatenating strings with `STR$` calls.

### P96/RTG Screen Support

Until now, ACE only supported planar Amiga screens -- OCS, ECS, and AGA modes that use bitplane graphics. Version 2.9 adds Picasso96 retargetable graphics with a new screen mode 13. This gives you chunky (linear) framebuffers with 8, 15, 16, 24, or 32-bit color depth. It works on any P96-compatible hardware.

Opening a P96 screen is straightforward:

```basic
SCREEN 1, 800, 600, 8, 13
```

Mode 13 tells ACE to use P96 instead of the native chipset. The depth parameter sets the color depth in bits. For 8-bit screens, you get a 256-color palette just like AGA, but the framebuffer is chunky instead of planar. For HiColor and TrueColor depths, there is a new `COLOR r,g,b` syntax for direct RGB drawing.

All the standard drawing commands -- `LINE`, `CIRCLE`, `PRINT`, `LOCATE` -- work on P96 screens because Picasso96 patches the graphics library. But you can also write directly to the framebuffer via `POKE` for maximum speed:

```basic
' Get the chunky framebuffer address
bitmapAddr& = SCREEN(4)
frameAddr& = PEEKL(bitmapAddr& + 8)

' Write a pixel at (x, y) on an 8-bit screen
POKE frameAddr& + CLNG(y%) * CLNG(800) + CLNG(x%), colorIndex%
```

This opens up possibilities for software rendering, chunky-to-chunky blitting, and effects that would be painful on planar screens.

The following example combines direct framebuffer writes (the rainbow gradient) with OS drawing primitives (lines, rectangles, circles) on the same chunky buffer:

```basic
' P96 Chunky Screen - 800x600, 256 colors
SCREEN 1, 800, 600, 8, 13
WINDOW 1,"",(0,0)-(799,599),0,1

' Rainbow palette
FOR i% = 0 TO 253
  ' ... compute r%, g%, b% for rainbow gradient ...
  PALETTE i%, r%/255, g%/255, b%/255
NEXT

' Direct framebuffer fill with rainbow bars
bitmapAddr& = SCREEN(4)
frameAddr& = PEEKL(bitmapAddr& + 8)
FOR y% = 0 TO 599
  i% = (y% * 254) / 600
  FOR x% = 0 TO 799
    POKE frameAddr& + CLNG(y%) * 800& + CLNG(x%), i%
  NEXT
NEXT

' OS drawing on top: lines, circles, filled shapes
COLOR 255, 0
LINE (20,80)-(380,80)
CIRCLE (200,180),80
CIRCLE (200,180),60,,,,f
LINE (400,80)-(600,180),200,bf
```

### Tail-Call Optimization

Recursive functions in ACE use the system stack. Each call pushes a frame, and if the recursion is deep enough, you run out of stack and crash. Version 2.9 adds automatic tail-call optimization (TCO) for self-recursive SUBs with numeric parameters. When the compiler detects that a recursive call is the last thing a SUB does, it replaces the call with a jump back to the top of the SUB, reusing the same stack frame.

Enable it with `OPTION O+`:

```basic
OPTION O+

SUB LONGINT Gcd(a&, b&)
  IF b& = 0 THEN
    Gcd = a&
  ELSE
    Gcd = Gcd(b&, a& MOD b&)
  END IF
END SUB

' This would overflow the stack without TCO
ASSERT Gcd(1000000, 3) = 1
```

Without TCO, `Gcd(1000000, 3)` requires around 333,000 recursive calls and would need roughly 48 KB of stack space -- well beyond the default. With TCO, it uses about 24 bytes regardless of depth.

The optimization works through the peephole optimizer. After the compiler generates the recursive `JSR` instruction, the peephole pass recognizes the pattern (restore frame, JSR to self, return) and replaces it with parameter shuffling and a `BRA` to the function entry. No parser changes were needed.

TCO only applies when the recursive call is in tail position -- there must be no computation after the call. The accumulator pattern is the standard way to write tail-recursive functions:

```basic
OPTION O+

SUB LONGINT Factorial(n&, acc&)
  IF n& <= 1 THEN
    Factorial = acc&
  ELSE
    Factorial = Factorial(n& - 1, n& * acc&)
  END IF
END SUB

result& = Factorial(12, 1)   '..479001600
```

### HTTP Client

Version 2.9 ships an HTTP client submodule that provides networking from ACE BASIC. It supports HTTP and HTTPS (via AmiSSL), chunked transfer encoding, and streaming responses. The implementation is split into three submodules: `tcpclient.b` for raw TCP sockets, `amissl.b` for TLS, and `httpclient.b` for the HTTP protocol layer.

The API is struct-based. You declare connection and request/response structs, then call the appropriate functions:

```basic
#include <submods/httpclient.h>

DECLARE STRUCT TcpConn conn
DECLARE STRUCT HttpRequest req
DECLARE STRUCT HttpResponse resp

LONGINT status

' High-level: one-call HTTP HEAD
status = HttpHead(req, resp, conn, "http://www.google.com/")
PRINT "Status:"; status    '..prints 200
```

For more control, there is a low-level API that lets you open a connection, send a request, read headers, and read the body in chunks:

```basic
rc = HttpOpen(req, conn, "www.example.com", 80, 0)
rc = HttpSendRequest(req, conn, "GET", "/")
status = HttpReadStatus(conn, resp)
bytes = HttpReadBody(conn, resp, buffer, bufferSize)
HttpClose(conn)
```

This is the first time ACE BASIC has any kind of network access built in. Fetching data from a web API, downloading files, or even posting data to a server is now possible from BASIC.

### Brief Mentions

A few more additions worth noting:

- **DP-Float submodule**: Double-precision floating-point math via the `mathieeedoubbas` and `mathieeedoubtrans` libraries. ACE's native `SINGLE` type uses Motorola Fast Floating Point (32-bit). The DP-Float submodule gives you 64-bit IEEE doubles with 32 functions covering arithmetic, trigonometry, hyperbolic functions, exponents, and string conversion. Originally written by David Benn (the creator of ACE), now integrated as an external submodule.

- **SAGA Sound submodule**: 16-bit audio playback on Vampire V4 hardware using the SAGA chipset. Supports 16 channels, 8 or 16-bit samples, stereo volume control, and sample rates up to 56 kHz.

- **Turtle graphics moved to submodule**: The 13 built-in turtle commands (`FORWARD`, `BACK`, `TURNRIGHT`, etc.) have been removed from the compiler and runtime. They now live in the `turtle.b` submodule. Existing programs just need to add `#include <submods/turtle.h>` and link the submodule. This keeps the compiler smaller and makes the turtle library easier to maintain independently.

- **Buffered File I/O**: The runtime functions behind `LINE INPUT #`, `INPUT #`, and `INPUT$` now use bulk `Read`+`Seek` calls instead of reading one character at a time. This gives roughly 12x throughput improvement for file reading operations.

- **CubicIDE plugin improvements**: The plugin now uses `regedit` for proper preset and filetype registration. The autocase dictionary, syntax highlighting, and quickinfo have been updated to cover all current keywords and functions.

### Conclusion

Version 2.9 makes ACE BASIC significantly more capable for systems programming. Structs now support the kind of nesting and composition you need for working with OS data structures and building your own. RTG support opens up high-resolution, high-color graphics beyond the Amiga chipset. Tail-call optimization makes recursive algorithms practical. And the HTTP client brings network access to ACE BASIC for the first time.

The project lives on <a href="https://github.com/mdbergmann/ACEBasic" target="_blank" class="link">GitHub</a>. Bug reports and feature requests are welcome.
