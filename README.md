# NoiseFunge

NoiseFunge is an obfuscated language for music livecoding. It is a member of
the befunge family of languages. Some features of the language are:

- Terse, visually striking syntax.
- Animation of running programs.
- MIDI support (ALSA).
- A lightweight VM capable of supporting thousands of threads.
- Network-based runtime control.

## Installation

Currently, the recommended method for installing NoiseFunge is to use cabal
to install the executable into a sandbox. The following steps can be used to
install NoiseFunge. However, this method may still require installation of
system packages for alsa and ncurses.

```
git clone https://github.com/revnull/noisefunge.git

cd noisefunge

cabal sandbox init
cabal configure
cabal build
cabal install
```

## Executables

If NoiseFunge is installed in a cabal sandbox, then the `noisefunge.env` file
can be used to add the executables to the PATH and configure environmental
variables for communicating with a running NoiseFunge program over the loopback
network (port 4545).

### funged

This is the NoiseFunge runtime daemon. It requires a config file specifying
the tempo and network interfaces. An example named `test.cfg` is provided
in the source repository.

### nfops

This program lists all of the operators built in to NoiseFunge.

### nfloader

This program loads a NoiseFunge file into the VM. The required command line
parameters are a filename, and names of input and output buffers.

### nfkill

This program kills a NoiseFunge process in the VM. There are three ways to run
this.

- `nfkill` kills all processes
- `nfkill -n FILENAME` kills all processes matching the given filename
- `nfkill -p PID` kills the process with the specified PID

### nftop

This is the "top-like" program for inspecting the VM.

- `q` or `Q` to quit.

### nfviewer

This is the process animator. The only controls are:

- `r` or `R` to re-tile the processes.
- `q` or `Q` to quit.

## NoiseFunge opcodes

The NoiseFunge VM supports several opcodes that are not standard in befunge-98.

### A-F

NoiseFunge supports opcodes for A-F that work like the 0-9 opcodes but push the
corresponding hex values onto the stack.

### Fork

The `K` opcode forks a new process. The number 0 is pushed onto the parent's
stack and 1 is pushed onto the child's stack.

### Call

The `c` opcode is similar to the `g` opcode, but instead of accessing a byte
from memory and pushing it onto the stack, the `c` opcode instead executes
that byte as an opcode.

### I/O and buffers

NoiseFunge utilizes a non-standard form of input and output. Each process is
assigned to one input buffer and one output buffer. Opcodes can be used to
read and write from these buffers. Each process also has its own text buffer
for writing characters and numbers.

- `~` reads a byte from the input buffer and pushes it onto the stack. This
blocks if there is no process writing to the buffer.
- `.` pops a byte from the stack and writes it to the output buffer. This
blocks if there is no process reading from the buffer.
- `;` pops a byte from the stack and sends it to all processes waiting on the
output buffer. This does not block.
- `,` pops a byte and writes it to the text buffer.
- `&` pops a byte and outputs it as a base 10 number to the text buffer.

###  Note buffer opcodes

The note buffer is a virtual device that can be read, written, or played
through the use of opcodes.

- `z` pops duration, velocity, pitch, and channel and writes them to the note
buffer.
- `Z` plays the note in the note buffer.
- `y` pops a value off of the stack and sets the notebuf channel.
- `Y` reads the channel from the notebuf and pushes it to the stack.
- `x` pops a value off of the stack and sets the notebuf pitch.
- `X` reads the pitch from the notebuf and pushes it to the stack.
- `w` pops a value off of the stack and sets the notebuf velocity.
- `W` reads the velocity from the notebuf and pushes it to the stack.
- `u` pops a value off of the stack and sets the notebuf duration.
- `U` reads the duration from the notebuf and pushes it to the stack.

### Conditional trampoline

- `'` pops a value and jumps over the next opcode iff the value is 0.

