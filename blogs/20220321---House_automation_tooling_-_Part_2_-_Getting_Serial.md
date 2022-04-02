The war in Ukraine is ongoing.  
Stop the war and any violence against humans and animals and Gods creation.

#### Getting Serial

Last post I prepared <a href="https://ccl.clozure.com" class="link" target="_blank">Clozure CL</a> on an iBook with MacOS X 10.4 (Tiger) including getting <a href="https://www.quicklisp.org" class="link" target="_blank">quicklisp</a> ready. quicklisp is not absolutely necessary, but it helps. Otherwise all libraries that you want to use you have to download and load manually in the REPL.

##### The adapter

In this post I'd want to check feasability and prepare the serial communication. Now, this iBook as well as more modern computers don't have a Sub-D serial port adapter anymore. However, the device (the boiler) this software should communicate with has a 9 pin Sub-D connector. So we need a USB to serial adapter. There are a number of them available. But we also need drivers for this version of Mac OS. This one, a Keyspan (<a href="https://www.tripplite.com/keyspan-high-speed-usb-to-serial-adapter~USA19HS" class="link" target="_blank">USA19HS</a>) works with this version of Mac OSX and drivers are available.

=> picture of the adapter

##### Development peer

OK, in order to 'simulate' the boiler we use an Amiga 1200, which still has a serial port and a nice software called 'Term' which allows to act as a serial peer for development. The application 'Term' has an Amiga Rexx (ARexx) scripting interface which allows to script behavior in Term. In the end this could be handy to create a half-automated test environment for system tests.  
However, for now we only do feasability work to figure out if and how the serial library works in order to plan a bit ahead what has to be done in the serial interface module of the automation tool. This should be the only (sort of) manual testing. From there we structure the code in a way to abstract the serial interface in order to fake or mock the serial communication which allows an easier and faster feedback development.

=> picture of the adapter cable 9-pin to 24pin.

=> picture of the Amiga


##### The Common Lisp serial interface library

There are two CL libraries based on FFI (Foreign Function Interface) that would work. I've experimented with both.

1. <a href="https://github.com/snmsts/cserial-port" class="link" target="_blank">cserial-port</a>
2. <a href="https://github.com/jetmonk/cl-libserialport" class="link" target="_blank">cl-libserialport</a>

In my opinion cl-libserialport offers a few more features and I'd settle on it. I.e. it allows to specify a termination character for the read operation where when received the read will automatically return.  
The disadvantage, cl-libserialport requires an additional C shared library (<a href="https://github.com/sigrokproject/libserialport" class="link" target="_blank">libserialport</a>) to exist in the system which has to be installed first. cserial-port also uses FFI but works with existing POSIX/Windows library calls. cl-libserialport is actually a CL layer on top of libserialport.  
On my development machine I can just install this library via <a href="https://brew.sh" class="link" target="_blank">Homebrew</a>. On the target machine (the iBook) I had to download and compile the library. But it is straight forward and not more than: `autogen.sh && make && make install`.

cl-libserialport is not on quicklisp, so in order to still load it in the REPL via quicklisp we have to clone it to `~/quicklisp/local-projects`, then quicklisp will find it and load it from there. Btw: this is a nice way to override versions from the quicklisp distribution.

With all the additional work for cl-libserialport (which is actually not that much and a one-time effort) I hope it pays off by being easier to work with.


##### Prototyping some code







```nohighlight
Read error between positions 173577 and 179054 in 
/Users/manfred/quicklisp/asdf.lisp.
> Error: Could not load ASDF "3.0" or newer
> While executing: ENSURE-ASDF-LOADED, in process listener(1).
> Type :POP to abort, :R for a list of available restarts.
> Type :? for other options.

```

<a href="https://common-lisp.net" class="link" target="_blank">Common Lisp</a>