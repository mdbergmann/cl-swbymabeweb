My __private__, I repeat, 'private' open-source and freeware software projects. Of course have done many more things in my professional life.

During the last 25 years I've developed a range of larger and smaller tools and applications and libraries as freeware or open-source in my spare time which I'd like to list here. Some of the things are still being developed or maintained (top), some not (below):

#### Programming languages/Compilers

- Clamiga - Common Lisp implementation for the Amiga (C, Common Lisp, hot)  
<a href="https://github.com/mdbergmann/cl-amiga" class="link" target="_blank">GitHub project</a>

    A bytecode-compiled Common Lisp environment for classic Amigas (68020+), MorphOS and AmigaOS 4. It aims at ANSI compatibility with a small footprint, so it runs on a 14 MHz 68020 with 4 MB RAM. It comes with native GUI bindings (Intuition, Graphics, GadTools), threads, a FFI, ASDF and Quicklisp support. It also builds and runs on modern POSIX hosts.  
    I wrote a <a href="/blog/Clamiga+-+Common+Lisp+for+the+Amiga" class="link" target="_blank">blog entry</a> about it.

- ACE-Basic - BASIC compiler for the Amiga (K&R C, m68k Assembler, hot)  
<a href="https://github.com/mdbergmann/ACEBasic" class="link" target="_blank">GitHub project</a>

    ACE compiles BASIC source code to native Amiga executables by generating 68000/68020 assembler code. It has an object system with classes and methods, IEEE 754 floating point support and deep Amiga OS integration: GUI (MUI, GadTools, Intuition), networking (HTTP/HTTPS clients), graphics, file operations and audio.

#### Libraries/Frameworks

- Chipi - (House) Automation Bus (Common Lisp, hot)  
<a href="https://github.com/mdbergmann/chipi" class="link" target="_blank">GitHub project</a>

This project is based on Sento. Uses <a href="https://github.com/rabbibotton/clog" class="link" target="_blank">CLOG UI</a> framework with Bootstrap 5 for responsiveness.

- knx-conn - KNX building automation connectivity library (Common Lisp, hot)  
<a href="https://github.com/mdbergmann/knx-conn" class="link" target="_blank">GitHub project</a>

    Connects to KNX gateways via KNXnet/IP tunnel connections. It can listen for bus events, read values and write to group addresses. It implements a range of data point types (DPTs) and offers both an asynchronous and a blocking API. Used by Chipi.

- Sento - Actor framework (Common Lisp, hot)  
<a href="https://github.com/mdbergmann/cl-gserver" class="link" target="_blank">GitHub project</a>

#### Applications

- Clamacs - Emacs-flavoured Common Lisp IDE for AmigaOS 3 and MorphOS (C, Common Lisp, ARexx, MUI, hot)  
<a href="https://github.com/mdbergmann/clamacs" class="link" target="_blank">GitHub project</a>

    A native MUI editor with Emacs-style key bindings that talks to a Clamiga runtime. It has a REPL window, compiles and evaluates code, does syntax highlighting, code introspection (argument lists, completion, jump to definition) and interactive debugging.

- Eloquent - Bible study tool (macOS, Objective-C/Swift, Cocoa, maintained)  
<a href="https://github.com/mdbergmann/Eloquent" class="link" target="_blank">GitHub project</a>

#### Web applications

- Personal blog web page (Common Lisp, maintained)  
I did a <a href="/blog/Test-driven+Web+application+development+with+Common+Lisp" class="link" target="_blank">blog entry</a> capturing the development of this using TDD/ATDD: <a href="https://github.com/mdbergmann/cl-swbymabeweb" class="link" target="_blank">GitHub project</a>, <a href="/blog" class="link" target="_blank">Blog page</a>

    The previous version developed with <a href="https://wicket.apache.org" class="link" target="_blank">Apache Wicket</a> was using a database and had an administration area. The new 'retro' version is more simple and based on conventions.

- House automation data capture (Elixir)  
Custom web application done in Elixir that can capture my house reader states and post them to <a href="https://www.openhab.org/" class="link" target="_blank">openHAB</a>:

    I also did a <a href="/blog/MVC+Web+Application+with+Elixir" class="link" target="_blank">blog entry</a> capturing the development of this using TDD:  
<a href="https://github.com/mdbergmann/elixir_house_stat_util" class="link" target="_blank">GitHub project</a>

#### Tools

- <a href="https://github.com/lfe/lfe" class="link" target="_blank">LFE</a> (Lisp Flavoured Erlang) language server protocol (LSP) implementation  
<a href="https://github.com/mdbergmann/lfe-ls" class="link" target="_blank">GitHub project</a>

#### Other utilities, Emacs plugins, etc.

- emacs-bloopunit - Emacs plugin for testing Scala code using Bloop (Emacs Lisp)  
<a href="https://github.com/mdbergmann/emacs-bloopunit" class="link" target="_blank">GitHub project</a>

- emacs-ocamlunit - Emacs plugin for testing OCaml code (Emacs Lisp)  
<a href="https://github.com/mdbergmann/emacs-ocamlunit" class="link" target="_blank">GitHub project</a>

- emacs-lfeunit - Emacs plugin for testing LFE code (Emacs Lisp)  
<a href="https://github.com/mdbergmann/emacs-lfeunit" class="link" target="_blank">GitHub project</a>

- emacs-tcr-mode - Emacs plugin for Test && Commit || Revert (Emacs Lisp)  
<a href="https://github.com/mdbergmann/emacs-tcr-mode" class="link" target="_blank">GitHub project</a>

- ObjCSword - Objective-C Sword wrapper library (Objective-C, Sword). Used in Eloquent.  
<a href="https://github.com/mdbergmann/ObjCSword" class="link" target="_blank">GitHub project</a>

#### Older, unmaintained projects

- iKnow & Manage - data storage application (macOS, Objective-C, Cocoa, development ceased)  
<a href="https://github.com/mdbergmann/iKnowAndManage" class="link" target="_blank">GitHub project</a>

- FooLogger - Objective-C logging library (Objective-C)  
<a href="https://github.com/mdbergmann/FooLogger" class="link" target="_blank">GitHub project</a>

- CocoPCRE - Objective-C regular expression wrapper library (Objective-C)  
<a href="https://github.com/mdbergmann/CocoPCRE" class="link" target="_blank">GitHub project</a>

- fp-lib-m68k - m68k Assembler floating point library (Assembler)  
<a href="https://github.com/mdbergmann/fp-lib-m68k" class="link" target="_blank">GitHub project</a>

- SwiftLog - Swift logging library (Swift)  
<a href="https://github.com/mdbergmann/SwiftLog" class="link" target="_blank">GitHub project</a>

- Sqlite Query - Sqlite database query tool (Mac OS X, Objective-C, Cocoa, development ceased)  
Edit and maintain existing databases or create new ones using this tool.
SqliteQuery comes with the compiled SQLite database library builtin.  
It can also execute single or batch SQL statements. The result of SELECT statements is conveniently displayed in a tableview.  
The SqliteQuery user-interface is separated in four parts.

    1. setting the database path (defining a not existing path will create a new database - you will actually be asked before creating it
    2. input of SQL statements
    3. showing result of SELECT statements in a tableview
    4. showing a log output

<!-- <img src="/static/gfx/projects/SqliteQuery-0.5.0_shot1.png" alt="Sqlite img" /> -->

- Cocser - Cocoa servers management tool (macOS, Objective-C, Cocoa)  
Cocser is a command starter tool.
Some software requires being started/stopped from command line or shell. Cocser can do this for you with the click of a button. You just have to configure it with the start and stop commands.
It will display the program standard output conveniently in the window.  
It's also possible to start programs with superuser rights.

<!-- <img src="/static/gfx/projects/Cocser-0.6.0_shot1.png" alt="Cocser img" /> -->

- Sysdiag - RAM testing and system diagnose tool (Amiga, C)  
Sysdiag can show information about your memory and hardware of your Amiga.  
For memory testing it uses algorithms like:

    - custom address test
    - own address test
    - walking ones test
    - moving inv (8bit) test
    - moving inv (32 bit) test
    - modulo x (8 bit) test
    - modulo x (32 bit) test
    - bit fading test
    
    Start Amiga without Startup-Sequence and execute from Amiga Shell to test as much memory as possible.

    Some of the tests are executed twice (memory cache enabled and disabled).
    The bit fading test can take much longer. It tests if the memory is refreshed and if the bits are stable.

    Available on <a href="https://aminet.net/package/util/moni/sysdiag-0.1.4" class="link" target="_blank">Aminet</a>.
