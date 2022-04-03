The war in Ukraine is ongoing.  
Stop the war and any violence against humans and animals and Gods creation.

#### Getting Serial

Last post I prepared <a href="https://ccl.clozure.com" class="link" target="_blank">Clozure CL</a> on an iBook with MacOS X 10.4 (Tiger) including getting <a href="https://www.quicklisp.org" class="link" target="_blank">quicklisp</a> ready. quicklisp is not absolutely necessary, but it helps. Otherwise all libraries that you want to use you have to download and load manually in the REPL.

In this post I'd want to check feasability and prepare the serial communication.  
We'll do some CL coding and use the actor pattern for this prove of concept.

##### The adapter

This iBook as well as more modern computers don't have a Sub-D serial port adapter anymore. However, the device (the boiler) this software should communicate with has a 9 pin Sub-D connector. So we need a USB to serial adapter. There are a number of them available. But we also need drivers for this version of Mac OS. This one, a Keyspan (<a href="https://www.tripplite.com/keyspan-high-speed-usb-to-serial-adapter~USA19HS" class="link" target="_blank">USA19HS</a>) works with this version of Mac OSX and drivers are available.

=> picture of the adapter

##### Development peer

OK, in order to 'simulate' the boiler we use an Amiga 1200, which still has a serial port and a nice software called 'Term' which allows to act as a serial peer for development. The application 'Term' has an Amiga Rexx (ARexx) scripting interface which allows to script behavior in Term. In the end this could be handy to create a half-automated test environment for system tests.  
However, for now we only do feasability work to figure out if and how the serial library works in order to plan a bit ahead what has to be done in the serial interface module of the automation tool. This should be the only (sort of) manual testing. From there we structure the code in a way to abstract the serial interface in order to fake or mock the serial communication which allows an easier and faster feedback loop for development.

<img src="/static/gfx/blogs/a1200_cropped.jpg" alt="A1200" width="720" />

(Since the Amiga has a 25 pin Sub-D interface but the Keyspan adapter has a 9 pin interface I had to build a 25<->9 pin converter. Of course I could have bought it but I like doing some soldering work from time to time.)

<img src="/static/gfx/blogs/serial-adapter.jpg" alt="serial-adapter" width="300" />


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

The boiler serial protocol will require to send commands to the boiler, and receive sensor data. One of the commands is a 'start-record' command which instructs the boiler to start sending data repeatedly every x seconds until it received a 'stop-record' command. Since it is not possible to send and receive on the serial device at the same time we have to somehow serialize the send and receive of data. One way to do this is to use a queue. We enqueue send and read commands and when dequeued the command is executed. Now, this cries for an actor. Fortunately there is a good actor library for Common Lisp called <a href="https://github.com/mdbergmann/cl-gserver" class="link" target="_blank">cl-gserver</a> which we can utilize for this and hack together some prove of concept. (Though if I read correctly then libserialport internally uses semaphores to manage concurrent access to the device resource. Nontheless I'd like to use an actor.)

For this we have to implement to initialize the serial interface, set the right baud value and such. Then we want to write/send and read/receive data.

The initialization, opening the serial device can look like this:

```lisp
(defparameter *serial* "/dev/cu.usbserial-143340")
(defparameter *serport* nil)

(defun open-serial (&optional (speed 19200))
  (setf *serport*
        (libserialport:open-serial-port
         *serial*
         :baud speed :bits 8 :stopbits 1 :parity :sp-parity-none
         :rts :sp-rts-off
         :flowcontrol :sp-flowcontrol-none)))
```

The opened serial device will be stored in `*serport*`. The baud rate we need is 19200 and there should be no flow control and such. Just plain serial communication.

Now write and read will look like this:

```lisp
(defun read-serial ()
  (libserialport:serial-read-octets-until
   *serport*
   #\}
   :timeout 2000))

(defun write-serial (data)
  (libserialport:serial-write-data *serport* data))
```

The read function utilizes the termination character because I know already that the boiler data uses start and end characters `{` and `}`. The timeout is used to terminate the read command in case there is no data available to read. When we queue the commands and there is a write after a read we have a delay for the write not longer than 2 seconds. This might be acceptable in production because sending new commands doesn't need to be immediate.

Now let's see how the actor can look like in a simple way that can work for this example:

```lisp
(defun receive (actor msg state)
  (case (car msg)
    (:init
     (open-serial))
    (:read
     (progn
       (let ((read-bytes (read-serial)))
         (when (> (length read-bytes) 0)
           (format t "read: ~a~%" read-bytes)
           (format t "read string: ~a~%" (babel:octets-to-string read-bytes))))
       (tell actor msg)))
    (:write
     (write-serial (cdr msg))))
  (cons nil state))

(defvar *asys* (asys:make-actor-system))
(defparameter *serial-act* (ac:actor-of
                            *asys*
                            :receive (lambda (a b c) (receive a b c))))
```

The last part creates the actor-system and a `*serial-act*` actor. Messages sent to the actor should be pairs of a key: `:init`, `:read` and `:write`, and something else. This something else is only used for `:write` to transport the string to be written and can be `nil` otherwise.

For the `:receive` key argument to the `actor-of` function we could just use `#'receive`, but then we couldn't make adjustments to the `receive` function and have it applied immediately when evaluated. The `#'receive`, which is actually `(function receive)`, seems to pass a function symbol that is static.

To initialize the serial device we do: 

```lisp
(tell *serial-act* '(:init . nil))
```

To write to the serial device we do:

```lisp
(tell *serial-act* '(:write . "Hello World"))
```

Having done that we see in the 'Term' application the string "Hello World". So this works.

<img src="/static/gfx/blogs/term-hello.jpg" alt="term-hello" width="720" />

The read has a speciality: sending `'(:read . nil)` will not only read from the device but also enqueue again the same command, because we want to test receiving data continuously but mixing in write, or other commands in between. This should reflect the reality pretty well.

When I type something in the 'Term' program the REPL will print the read data:

```nohighlight
SERIAL> (tell *serial-act* '(:write . "Hello World"))
T
SERIAL> (tell *serial-act* '(:read . nil))
T
SERIAL> 
read: #(13)
read string: 
read: #(104 101 108)
read string: hel
read: #(108 111 32 102 114 111 109 32 116 101 114 109)
read string: lo from term
read: #(105 110 97 108)
read string: inal
read: #(125)
read string: }
; No values
SERIAL> (tell *serial-act* '(:write . "Hello World2"))
T
SERIAL> 
read: #(102)
read string: f
read: #(111 111 125)
read string: oo}
; No values
```

So this seems to work. I need to think about the next step now. Since I'd like to develop outside-in with a double test loop the next thing to do is figure out a use-case and create a test for it that basically sets the bounds of what should be developed in smaller increments.
