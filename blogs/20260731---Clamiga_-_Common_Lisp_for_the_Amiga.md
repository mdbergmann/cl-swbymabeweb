### Clamiga -- Common Lisp for the Amiga

After the ACE BASIC posts of the last months, this one is about a different project I have been working on for the last half year: **CL-Amiga**, or **Clamiga** for short. It is a Common Lisp implementation built for the Amiga family -- classic AmigaOS 3 on 68k and MorphOS as a fully native PPC build, AROS and AmigaOS 4 maybe to come -- but it also runs on macOS (I use it as my main Common Lisp impl here) and Linux, where most of the development actually happens.

The name is simple: *Common Lisp for the Amiga* becomes CL-Amiga, and said out loud that is "Clamiga". And since *amiga* is Spanish/Portuguese for a (female) friend (Amiga users should know), the name does double duty: the Lisp that runs on your Amiga, and the Lisp that is your *amiga* ;).

<img src="../static/gfx/blogs/clamiga-aos3_1.jpg" alt="Clamiga running on AmigaOS 3" width="720" />

### A few words about Common Lisp

Common Lisp is one of the older programming languages still in active use. Yes, it is still used in industry and many other domains. The ANSI standard is from 1994 and has not changed since -- and yet the language feels surprisingly modern. It has a full object system with multiple dispatch (CLOS), a condition system that goes beyond exceptions, macros that let you extend the language itself, and an incredibly interactive, image-based development style where you compile and redefine functions in a live running system. Many "new" language features of the last decades existed in Common Lisp long before.

I won't repeat all of that here. If you want a proper introduction, I wrote a primer a few years ago: <a href="/blog/Common+Lisp+-+Oldie+but+goldie" class="link">[Common Lisp - Oldie but goldie]</a>. There is also a post about <a href="/blog/Functional+Programming+in+(Common)+Lisp" class="link">[functional programming in Common Lisp]</a> if you find that interesting.

### Why another Common Lisp implementation?

There are excellent Common Lisp implementations out there -- SBCL, CCL, ECL, Clasp, CLISP (unmaintained in decades), or even commercial ones like LispWorks and Allegro.

**Because none of them run on the Amiga.** The high-performance implementations (SBCL, CCL) are native-code compilers tied to modern architectures -- x86-64, ARM, PPC -- with no 68k backend and a memory footprint measured in tens of megabytes. Clasp is built on LLVM. CLISP, the closest in spirit -- a compact bytecode interpreter written in C -- has not had a maintained AmigaOS build in decades.

Clamiga is built to run on m68k Amigas. It has a self-contained bytecode VM in portable C with no external runtime dependencies -- no libffi, no LLVM, no C compiler needed at runtime. A full Common Lisp implementation, its language and runtime features, means that it won't break performance records compared to just C or assembler programs on the Amiga. So it's not meant to code games with it that need super-fast scrolling or so. The m68k-JIT tries to squeeze more performance out of it but it has its limits.

It has other features, i.e. the full numeric tower, from bits (bitvector) over fractions and complex numbers to big integers out of the box. It has a repl for interactive development, a debugger, an inspector. Reciliency using the condition system, restarts and all that (see my Oldie but goldie article above). You have sockets, threads, file system access, streams that are implemented natively to the AmigaOS APIs so that Common Lisp code stays compliant and often needs no porting to other systems. CLOS (Common Lisp Object System) is the most advanced object-system I came across. And you could do functional programming also if you wanted to.

But let's look at more technical things.

### Some upfront glossary that is mentioned below

- <a href="https://asdf.common-lisp.dev/" class="link" target="_blank">[ASDF]</a>: Another System Definition Facility, is the de fact standard build facility for Common Lisp. Most Common Lisp 'libraries' in ASDF chargon called 'systems' are build with ASDF. Practically all Common Lisp implementation that are maintained today ship ASDF and so does Clamiga.
- FASL: FASt Load. FASL files are generated when compiling Lisp source code, i.e. via (compile-file "foo.lisp"). fasl files are faster to load than lisp source code files because they are already compiled to a serialized format. All Common Lisp variants implement FASL, so does Clamiga. The Clamiga generated FASL files, when generated on either m68k or PPC are interchangeable between those two architectures.
- <a href="https://www.quicklisp.org/beta/" class="link" target="_blank">[Quicklisp]</a> is a library manager for Common Lisp containing over 1500 libraries. Clamiga ships with compatibility shims so that (theoretically) many libraries available on Quicklisp can be used. Though many require more computing power and are probably out of reach for a m68020.

### How it works

Clamiga is a single-pass compiler from S-expressions to bytecode, executed by a stack-based VM. A few design decisions follow directly from the Amiga constraint:

- **Tagged 32-bit values.** Every Lisp value is a 32-bit word. Heap pointers are arena-relative offsets, which keeps the whole object model 32-bit-clean and compact (see below for packed arrays).
- **Compacting GC.** A small heap fragments quickly. The mark-and-sweep collector can slide-compact the heap when fragmentation blocks an allocation, so a long-running session on 4 or 8 MB does not slowly die.
- **Architecture-agnostic bytecode.** Because execution is bytecode, the same compiled Lisp runs unchanged on 68k and PowerPC. The compiled FASL files are byte-compatible between AmigaOS 3 and MorphOS.

The nice side effect of the portable C core: the exact same system builds and runs on macOS and Linux. Development, debugging, and most testing happen on a fast host, and the result behaves identically on the Amiga.

### On the classic Amiga: living with 4 MB, well, less also works

Even with accelerators providing more RAM, memory is still often a constraint on a 68k Amiga, and a lot of work went into respecting it.

The full Common Lisp core -- CLOS, conditions, the numeric tower, format, loop -- boots in about **0.5 MB** of heap. For writing simple programs, `clamiga --heap 1M` is enough. The defaults scale up from there:

| Use case                                   | Heap        | Stack          |
|--------------------------------------------|-------------|----------------|
| Simple programs (no Quicklisp/ASDF)        | `--heap 1M` | 64K (default)  |
| small to medium programs                   | 4M (default)| 64K (default)  |
| Loading ASDF                               | `--heap 11M`| 64K (default)  |
| Quicklisp + quickload libraries            | `--heap 24M`| `stack 128000` |

A few things make small heaps practical:

- **Packed byte vectors**: `(make-array n :element-type '(unsigned-byte 8))` stores 1 byte per element instead of a 4-byte tagged value, and the GC never scans the contents. On an 8 MB machine that makes I/O buffers, graphics plane data, and audio samples 4x smaller and essentially free to collect. There are packed byte vectors for 16 bit values as well.
- **Bulk sequence I/O**: `read-sequence` and `write-sequence` on byte vectors move whole chunks per OS call instead of one VM round-trip per byte. On a 14 MHz 68020 that turns loading a 20 KB asset file from seconds into file-I/O speed.
- **Precompiled boot FASLs**: the standard library and CLOS ship precompiled. On the low-end 020 baseline this cuts cold boot from ~92 seconds to ~9 seconds.

And if the stack is too small for a deeply nested form, Clamiga signals a clean `C stack nearly exhausted` error telling you to raise it -- it doesn't corrupt the session.

### The m68k JIT

On the AmigaOS build (68020+), Clamiga translates bytecode functions to native m68k machine code at definition time. The VM dispatcher then jumps straight into the native body instead of interpreting bytecode.

The translator covers a broad core of the instruction set: integer arithmetic, branches, list operations, struct slot access, function calls and self-recursive tail calls, closures, multiple values, non-local exits, dynamic binding, and the AmigaOS FFI. Anything it does not handle yet falls back to the interpreter transparently -- you never notice, things just run.

Some A/B numbers, measured on an emulated A4000/68040 with identical function bodies and only the dispatch path toggled:

| Benchmark     | Shape                        | Bytecode |   JIT  | Speedup |
|---------------|------------------------------|---------:|-------:|--------:|
| `sum-to`      | `tagbody`/`go` fixnum loop   |   400 ms |  20 ms |  20.0x  |
| `struct-loop` | struct-slot reads in a loop  |   260 ms |  20 ms |  13.0x  |
| `arith-chain` | chained binary ops           |   300 ms |  40 ms |   7.5x  |
| `call-loop`   | function call in the loop    |   340 ms | 240 ms |   1.4x  |

Compute-bound code sees the largest wins. On the real-world bouncing-lines graphics demo (under examples/gfx/ in the source repo) -- which is dominated by FFI calls into `graphics.library` -- the JIT reaches about **615 FPS** versus **500 FPS** on the bytecode VM. For comparison, compiled ACE BASIC does ~1900 FPS through the same ROM calls; the remaining gap is the structural cost of a dynamic, garbage-collected, tagged-value language, not codegen.

The JIT is on by default; `--no-jit` keeps functions bytecode-only if you want to compare or isolate something.

### MorphOS: the NG Amiga

The MorphOS build is a fully native PowerPC binary, compiled under MorphOS with the SDK's GCC -- not a 68k binary running under emulation (though the m68k binary works, too). Threading, sockets, and the whole Amiga FFI/GUI/audio stack work like on classic AmigaOS; Amiga library calls are dispatched from PPC code to the library bases through MorphOS's ABox layer. PPC is 32-bit and big-endian like m68k, so FASL files compiled on one system load on the other.

The one thing the MorphOS build omits is the JIT, which is m68k-only -- it runs the portable bytecode VM like the host build. But on a G4 or G5 that VM is *fast*. Fast enough that MorphOS is a specific target for the full **Quicklisp** experience: installing the client, downloading dists, and quickloading real libraries with their whole dependency graphs is entirely practical there, where on a 14 MHz 68020 it is not.

<!-- screenshot may be replaced with a more recent one later -->
<img src="/static/gfx/blogs/clamiga-mos.png" alt="Clamiga booting and running on MorphOS" width="720" />

### Setting up Quicklisp

Quicklisp is Common Lisp's de-facto library manager, and it runs on Clamiga. The stock client does not know this implementation yet, so the project ships a small compat layer plus a set of maintained library forks that carry first-class Clamiga support behind `#+cl-amiga` feature branches -- with the goal to upstream them once the remaining API gaps close.

Installing is a one-time thing:

```lisp
(require "asdf")
(load "lib/quicklisp-install.lisp")
(cl-amiga-ql:install)
```

And in any later session:

```lisp
(load #P"~/quicklisp/setup.lisp")
(load "lib/quicklisp-compat.lisp")
;; load alexandria library
(ql:quickload "alexandria")
```

Libraries (selection) confirmed working via `quickload` plus their own `asdf:test-system` suites include:

 - <a href="https://github.com/kmx-io/alexandria" class="link" target="_blank">[alexandria]</a> the de-facto standard utility collection
 - <a href="https://github.com/lispci/fiveam" class="link" target="_blank">[fiveam]</a> my favorite unit testing framework
 - <a href="https://github.com/slburson/fset" class="link" target="_blank">[FSet]</a> a functional collection library
 - <a href="https://vindarel.github.io/cl-str/" class="link" target="_blank">[str]</a> a string utility library
 - <a href="https://github.com/edicl/drakma" class="link" target="_blank">[Drakma]</a> HTTP/HTTPS client
 - <a href="https://github.com/edicl/hunchentoot" class="link" target="_blank">[Hunchentoot]</a> web server

(Drakma and Hunchentoot must be started without SSL, an binding to an Amiga SSL is missing at the moment)

### Native Amiga GUI from Lisp

Clamiga ships Lisp bindings for Intuition, Graphics, and GadTools, loaded on demand via `require`. These bindings are work in progress -- they grew out of what my own projects needed, mainly the bouncing-lines demo and my Lambda's Tale engine (a Bard's Tale style dungeon crawler engine), so they cover common use cases rather than the full API surface. More AOS APIs (or specific MorphOS APIs) are being implemented as needed (pull-requests welcome). Opening a window and drawing into it looks like this:

```lisp
;;; hello-window.lisp

;; The REQUIREs must stay ahead of the DEFPACKAGE: LOAD reads and
;; evaluates one top-level form at a time, so the packages exist by the
;; time the forms mentioning them are read.
(require "amiga/intuition")
(require "amiga/graphics")

(defpackage :hello-amiga
  (:use :cl)
  ;; using nicknames here, 
  ;; alternative is import full amiga.intuition and amiga.gfx in :use declaration, 
  ;; but that pulls in all symbols
  (:local-nicknames (:it :amiga.intuition)
                    (:gfx :amiga.gfx)))
                      
(in-package :hello-amiga)

(defun main ()
  (it:with-window (win :title "Hello Amiga"
                       :width 320 :height 200
                       :idcmp it:+idcmp-closewindow+)
    (let ((rp (it:window-rastport win)))
      (gfx:set-a-pen rp 1)
      (gfx:move-to rp 20 40)
      (gfx:gfx-text rp "Hello from Clamiga!")
      (it:event-loop win
        (it:+idcmp-closewindow+ (msg) (return))))))

(main)
```

There are also custom screens, RTG-safe offscreen bitmaps with blitter compositing, GadTools gadgets and menus, audio.device playback -- and when the abstractions are not enough, raw register-based library calls into any AmigaOS library through a hand-written 68k trampoline. So even where a binding is missing, nothing is out of reach.

<img src="../static/gfx/blogs/clamiga-aos3_demo.jpg" alt="Clamiga native GUI example" width="720" />

### Inspector

### Debugger

### Development on the host

Because the same binary behavior exists on macOS and Linux, you get a comfortable development setup: Clamiga speaks the SLYNK protocol, so you can drive it from Emacs with SLY -- REPL, completion, jump-to-definition, the inspector, and the SLDB debugger. Write and test your code on the fast host, then run the same sources (or even the same FASLs, when targeting MorphOS) on the Amiga.

### Status

Clamiga is still a young project, but in its current state it is stable for what it does. The core language runs real-world libraries with their full dependency graphs, and a broad test suite covers threading, CLOS, conditions, the numeric tower, FFI, the JIT, and the Amiga GUI. Full ANSI conformance is the goal but not reached yet -- the Paul Dietz ANSI test suite is the working spec, and the CONS, SYMBOLS, NUMBERS, and SEQUENCES sections pass.

### Conclusion

Clamiga exists to bring a modern, library-capable Common Lisp to hardware every other implementation left behind. On a classic 68k Amiga you get a full Common Lisp with a native JIT that fits in a few megabytes of RAM. On MorphOS you get a native PPC build fast enough for the whole Quicklisp ecosystem. And on macOS or Linux you get the same system with comfortable Emacs tooling for development.

The project lives on <a href="https://github.com/mdbergmann/cl-amiga" target="_blank" class="link">[GitHub]</a>. Bug reports, feature requests, and curious REPL sessions are welcome.
