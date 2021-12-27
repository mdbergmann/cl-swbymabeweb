My open-source and freeware software projects

During the last 20 years I've developed a range of larger and smaller tools and applications and libraries as freeware or open-source which I'd like to list here. Some of the things are still being developed or maintained, some not:

### Applications

#### Eloquent - Bible study tool (macOS, Objective-C/Swift, Cocoa, maintained)

<a href="https://github.com/mdbergmann/Eloquent" class="link" target="_blank">GitHub project</a>

#### iKnow & Manage - data storage application (macOS, Objective-C, Cocoa, development ceased)

<a href="https://github.com/mdbergmann/iKnowAndManage" class="link" target="_blank">GitHub project</a>

### Web applications

#### Software by MaBe blog (Common Lisp)

I did a <a href="/blog/Test-driven+Web+application+development+with+Common+Lisp" target="_blank" class="link">blog entry</a> capturing the development of this using TDD/ATDD:

<a href="https://github.com/mdbergmann/cl-swbymabeweb" target="_blank" class="link">GitHub project</a>  
<a href="http://retro-style.software-by-mabe.com/blog" target="_blank" class="link">Blog page</a>

The previous version developed with <a href="https://wicket.apache.org" target="_blank" class="link">Apache Wicket</a> was using a database and had an administration area. The new 'retro' version is more simple and based on conventions.

#### House automation data capture (Elixir)

Custom web application done in Elixir that can capture my house reader states and post them to <a href="https://www.openhab.org/" target="_blank" class="link">openHAB</a>:

I also did a <a href="/blog/MVC+Web+Application+with+Elixir" target="_blank" class="link">blog entry</a> capturing the development of this using TDD:

<a href="https://github.com/mdbergmann/elixir_house_stat_util" target="_blank" class="link">GitHub project</a>


### Tools

#### Sqlite Query - Sqlite database query tool (Mac OS X, Objective-C, Cocoa, development ceased)

Edit and maintain existing databases or create new ones using this tool.
SqliteQuery comes with the compiled SQLite database library builtin.

It can also execute single or batch SQL statements. The result of SELECT statements is conveniently displayed in a tableview.

The SqliteQuery user-interface is separated in four parts.
1. setting the database path (defining a not existing path will create a new database - you will actualy be asked before creating it
2. input of SQL statements
3. showing result of SELECT statements in a tableview
4. showing a log output

<!-- <img src="/static/gfx/projects/SqliteQuery-0.5.0_shot1.png" alt="Sqlite img" /> -->

#### Cocser - Cocoa servers management tool (macOS, Objective-C, Cocoa)

Cocser is a command starter tool.
Some software requires being started/stopped from command line or shell. Cocser can do this for you with the click of a button. You just have to configure it with the start and stop commands.
It will display the program standard output conveniently in the window.

It's also possible to start programs with superuser rights.

<!-- <img src="/static/gfx/projects/Cocser-0.6.0_shot1.png" alt="Cocser img" /> -->

#### Sysdiag - RAM testing and system diagnose tool (Amiga, C)

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

Available on <a href="https://aminet.net/package/util/moni/sysdiag-0.1.4" class="link" target="_blank">Aminet</a>.

### Libraries

#### cl-gserver - Actor framework (Common Lisp, hot)

<a href="https://github.com/mdbergmann/cl-gserver" class="link" target="_blank">GitHub project</a>

#### emacs-bloopunit - Emacs plugin for testing Scala code using Bloop (Emacs Lisp)

<a href="https://github.com/mdbergmann/emacs-bloopunit" class="link" target="_blank">GitHub project</a>

#### emacs-tcr-mode - Emacs plugin for Test && Commit || Revert (Emacs Lisp)

<a href="https://github.com/mdbergmann/emacs-tcr-mode" class="link" target="_blank">GitHub project</a>

#### ObjCSword - Objective-C Sword wrapper library (Objective-C, Sword)

Used in Eloquent.

<a href="https://github.com/mdbergmann/ObjCSword" class="link" target="_blank">GitHub project</a>

#### FooLogger - Objective-C logging library (Objective-C)

<a href="https://github.com/mdbergmann/FooLogger" class="link" target="_blank">GitHub project</a>

#### CocoPCRE - Objective-C regular expression wrapper library (Objective-C)

<a href="https://github.com/mdbergmann/CocoPCRE" class="link" target="_blank">GitHub project</a>

#### fp-lib-m68k - m68k Assembler floating point library (Assembler)

<a href="https://github.com/mdbergmann/fp-lib-m68k" class="link" target="_blank">GitHub project</a>

#### SwiftLog - Swift logging library (Swift)

<a href="https://github.com/mdbergmann/SwiftLog" class="link" target="_blank">GitHub project</a>

