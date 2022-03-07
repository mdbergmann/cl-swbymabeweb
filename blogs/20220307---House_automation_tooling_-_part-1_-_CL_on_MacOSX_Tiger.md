In light of the current events in the world - I mean the war in Ukraine, this article in completely unimportant.  
Please stop this and all wars in this world. Let's live together in peace. Let's be kind to one another.


#### The project

The goal of this project is to create a tool that can read sensor data from my ETA wood chip boiler (main heating) and push this data to my <a href="https://www.openhab.org" class="link" target="_blank">openHAB</a> system. I use openHAB as a hub for various other data. It has database integrations and can do visualization from the stored data.

This ETA boiler has a serial interface where one can communicate with the boiler, retrieve temperatur and other data. It also allows to control the boiler, to a certain degree, by sending commands to it.

The tooling will be done in <a href="https://common-lisp.net" class="link" target="_blank">Common Lisp</a>.

For the hardware I'd want to utilize an old PowerPC iBook I still have lying here. So, Common Lisp should run on this old version of Mac OSX 10.4 (Tiger), including a library that can use the serial port. The data will eventually be sent via HTTP to a REST interface of openHAB. For that we probably use <a href="https://edicl.github.io/drakma/" class="link" target="_blank">drakma</a>.

In this first part of the project I'd want to choose the Common Lisp implementation and spend some time to get <a href="https://www.quicklisp.org/beta/" class="link" target="_blank">quicklisp</a> and <a href="https://gitlab.common-lisp.net/asdf/asdf" class="link" target="_blank">ASDF</a> working in order to download and work with additional libraries that we may need in a convenient way.

#### TDD and the 'double loop'

Once this initial research and prove of concept (with CL on this hardware and the serial port) is done we'll continue with developing this tool *guided by tests* (TDD). Similarly as in this <a href="blog/Test-driven+Web+application+development+with+Common+Lisp" class="link" target="_blank">blog article</a> we'll also try to do an outer acceptance test loop.

#### Finding the Common Lisp implementation

I've settled on CCL (Clozure Common Lisp). I did briefly look at an older version of SBCL but that went nowhere. CCL version 1.4 works nicely on this version of Mac OSX. Those older CCL versions can be downloaded <a href="https://ccl.clozure.com/ftp/pub/release/" class="link" target="_blank">here</a>.

#### quicklisp and ASDF

Now, in order to have some convenience I'd want to have quicklisp work on this version of CCL. It doesn't work out-of-the box because of the outdated ASDF version.

When we go through the standard quicklisp installation procedure `(quicklisp-quickstart:install)` the installation attempt bails out at this error:

```plain
Read error between positions 173577 and 179054 in 
/Users/manfred/quicklisp/asdf.lisp.
> Error: Could not load ASDF "3.0" or newer
> While executing: ENSURE-ASDF-LOADED, in process listener(1).
> Type :POP to abort, :R for a list of available restarts.
> Type :? for other options.

```

There is no restart available that could overcome this. At this point, however, we already have an unfinished quicklisp installation at `~/quicklisp`.

What we try now is to 

- download the latest version of ASDF from <a href="https://asdf.common-lisp.dev/archives/asdf.lisp" class="link" target="_blank">https://asdf.common-lisp.dev/archives/asdf.lisp</a>
- replace the old asdf.lisp version in the quicklisp folder with the new one (you can as well rename the old one to 'asdf_old.lisp' or so. 

Then, while being at the REPL we compile the new ASDF version:

- compile asdf: `(compile-file #P"~/quicklisp/asdf.lisp")`

We are thrown into the debugger because it seems that this version of CCL does not have an exported function `delete-directory`. But we have a restart available (2) that allows us to 'Create and use the internal symbol CCL::DELETE-DIRECTORY'. Choosing this restart we can overcome the missing function error. It is possible though that this will limit the functionality of quicklisp, or ASDF.

```plain
> Error: Reader error: No external symbol named "DELETE-DIRECTORY" 
> in package #<Package "CCL"> .
> While executing: CCL::%PARSE-TOKEN, in process listener(1).
> Type :GO to continue, :POP to abort, :R for a list of available restarts.
> If continued: Create and use the internal symbol CCL::DELETE-DIRECTORY
> Type :? for other options.
? :R
>   Type (:C <n>) to invoke one of the following restarts:
0. Return to break level 1.
1. #<RESTART CCL:ABORT-BREAK #x294556>
2. Create and use the internal symbol CCL::DELETE-DIRECTORY
3. Retry loading #P"/Users/manfred/quicklisp/asdf.lisp"
4. Skip loading #P"/Users/manfred/quicklisp/asdf.lisp"
5. Load other file instead of #P"/Users/manfred/quicklisp/asdf.lisp"
6. Return to toplevel.
7. #<RESTART CCL:ABORT-BREAK #x294C0E>
8. Reset this thread
9. Kill this thread

:C 2
```

From here the compilation of ASDF can resume. When done we have a binary file right next to the lisp source file. We'll see shortly how to use it.

Now we have to finish our quicklisp installation by:

- manually loading 'setup.lisp': `(load #P"~/quicklisp/setup.lisp")`

Ones this is through we can instruct quicklisp to create an init file for CCL and add initialization code for quicklisp to it. This init file is loaded by CCL on every startup. We can do this by calling:

- `(ql:add-to-init-file)`

When this is done we can close the repl and modify the created `~/.ccl-init.lisp` init file by adding:

- `#-asdf (load #P"~/quicklisp/asdf")`

to the top of the file. This instruction will load the compiled binary of asdf (notice we use 'asdf' here instead of 'asdf.lisp' for the `load` function). The `#-` is a lisp reader instruction that basically says: if `asdf` is not part of `*features*` evaluate the following expression.

When the repl is fully loaded we can check `*features*`:

```lisp
? *features*
(:QUICKLISP :ASDF3.3 :ASDF3.2 :ASDF3.1 :ASDF3 :ASDF2 :ASDF :OS-MACOSX :OS-UNIX 
:ASDF-UNICODE :PRIMARY-CLASSES :COMMON-LISP :OPENMCL :CCL :CCL-1.2 :CCL-1.3 :CCL-1.4
:CLOZURE :CLOZURE-COMMON-LISP :ANSI-CL :UNIX :OPENMCL-UNICODE-STRINGS
:OPENMCL-NATIVE-THREADS :OPENMCL-PARTIAL-MOP :MCL-COMMON-MOP-SUBSET
:OPENMCL-MOP-2 :OPENMCL-PRIVATE-HASH-TABLES :POWERPC :PPC-TARGET :PPC-CLOS
:PPC32-TARGET :PPC32-HOST :DARWINPPC-TARGET :DARWINPPC-HOST :DARWIN-TARGET
:DARWIN-HOST :DARWIN-TARGET :POWEROPEN-TARGET :32-BIT-TARGET :32-BIT-HOST
:BIG-ENDIAN-TARGET :BIG-ENDIAN-HOST :DARWIN)
```
There we are.

Let's check the installation by loading a library:

- load cl-gserver: `(ql:quickload :cl-gserver)`

```plain
To load "cl-gserver":
  Load 1 ASDF system:
    cl-gserver
; Loading "cl-gserver"
[package cl-gserver.logif]........................
[package cl-gserver.atomic].......................
[package cl-gserver.config].......................
[package cl-gserver.wheel-timer]..................
[package cl-gserver.utils]........................
[package cl-gserver.actor]........................
[package cl-gserver.dispatcher]...................
[package cl-gserver.queue]........................
[package cl-gserver.messageb].....................
[package cl-gserver.eventstream]..................
[package cl-gserver.actor-system].................
[package cl-gserver.actor-context]................
[package cl-gserver.future].......................
[package cl-gserver.actor-cell]...................
[package cl-gserver.agent]........................
[package cl-gserver.tasks]........................
[package cl-gserver.router].......................
[package cl-gserver.agent.usecase-commons]........
[package cl-gserver.agent.hash]...................
[package cl-gserver.agent.array].
(:CL-GSERVER)
```

The library was fully loaded and compiled proper. Ready for use.

Next stop is getting the serial port working.