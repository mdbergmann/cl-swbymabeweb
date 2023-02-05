My __private__, I repeat, 'private' open-source and freeware software projects. Of course have done many more things in my professional life.

During the last 20 years I've developed a range of larger and smaller tools and applications and libraries as freeware or open-source in my spare time which I'd like to list here. Some of the things are still being developed or maintained (top), some not (below):

#### Libraries/Frameworks

- Sento - Actor framework (Common Lisp, hot)  
<a href="https://github.com/mdbergmann/cl-gserver" target="_blank">[GitHub project]</a>

#### Applications

- Eloquent - Bible study tool (macOS, Objective-C/Swift, Cocoa, maintained)  
<a href="https://github.com/mdbergmann/Eloquent" target="_blank">[GitHub project]</a>

#### Web applications

- Personal blog web page (Common Lisp, maintained)  
I did a <a href="/blog/Test-driven+Web+application+development+with+Common+Lisp" target="_blank" >[blog entry]</a> capturing the development of this using TDD/ATDD: <a href="https://github.com/mdbergmann/cl-swbymabeweb" target="_blank">[GitHub project]</a>, <a href="/blog" target="_blank">[Blog page]</a>

    The previous version developed with <a href="https://wicket.apache.org" target="_blank" >Apache Wicket</a> was using a database and had an administration area. The new 'retro' version is more simple and based on conventions.

- House automation data capture (Elixir)  
Custom web application done in Elixir that can capture my house reader states and post them to <a href="https://www.openhab.org/" target="_blank">[openHAB]</a>:

    I also did a <a href="/blog/MVC+Web+Application+with+Elixir" target="_blank">[blog entry]</a> capturing the development of this using TDD:  
<a href="https://github.com/mdbergmann/elixir_house_stat_util" target="_blank">[GitHub project]</a>

#### Tools

- [\[LFE\]](https://github.com/lfe/lfe) (Lisp Flavoured Erlang) language server protocol (LSP) implementation  
<a href="https://github.com/mdbergmann/lfe-ls" target="_blank">[GitHub project]</a>

#### Other utilities, Emacs plugins, etc.

- emacs-bloopunit - Emacs plugin for testing Scala code using Bloop (Emacs Lisp)  
<a href="https://github.com/mdbergmann/emacs-bloopunit" target="_blank">[GitHub project]</a>

- emacs-ocamlunit - Emacs plugin for testing OCaml code (Emacs Lisp)  
<a href="https://github.com/mdbergmann/emacs-ocamlunit" target="_blank">[GitHub project]</a>

- emacs-lfeunit - Emacs plugin for testing LFE code (Emacs Lisp)  
<a href="https://github.com/mdbergmann/emacs-lfeunit" target="_blank">[GitHub project]</a>

- emacs-tcr-mode - Emacs plugin for Test && Commit || Revert (Emacs Lisp)  
<a href="https://github.com/mdbergmann/emacs-tcr-mode" target="_blank">[GitHub project]</a>

- ObjCSword - Objective-C Sword wrapper library (Objective-C, Sword). Used in Eloquent.  
<a href="https://github.com/mdbergmann/ObjCSword" target="_blank">[GitHub project]</a>

#### Older, unmaintained projects

- iKnow & Manage - data storage application (macOS, Objective-C, Cocoa, development ceased)  
<a href="https://github.com/mdbergmann/iKnowAndManage" target="_blank">[GitHub project]</a>

- FooLogger - Objective-C logging library (Objective-C)  
<a href="https://github.com/mdbergmann/FooLogger" target="_blank">[GitHub project]</a>

- CocoPCRE - Objective-C regular expression wrapper library (Objective-C)  
<a href="https://github.com/mdbergmann/CocoPCRE" target="_blank">[GitHub project]</a>

- fp-lib-m68k - m68k Assembler floating point library (Assembler)  
<a href="https://github.com/mdbergmann/fp-lib-m68k" target="_blank">[GitHub project]</a>

- SwiftLog - Swift logging library (Swift)  
<a href="https://github.com/mdbergmann/SwiftLog" target="_blank">[GitHub project]</a>

- Sqlite Query - Sqlite database query tool (Mac OS X, Objective-C, Cocoa, development ceased)  
Edit and maintain existing databases or create new ones using this tool.
SqliteQuery comes with the compiled SQLite database library builtin.  
It can also execute single or batch SQL statements. The result of SELECT statements is conveniently displayed in a tableview.  
The SqliteQuery user-interface is separated in four parts.

    1. setting the database path (defining a not existing path will create a new database - you will actualy be asked before creating it
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
Sysdiag can shows information about you memory and hardware of your Amiga.  
For memory testing it uses algorithms like:

    - custum address test
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

    Available on <a href="https://aminet.net/package/util/moni/sysdiag-0.1.4" target="_blank">[Aminet]</a>.
