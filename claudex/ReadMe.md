# Claudex

A Common Lisp web application that reads from SQLite and serves results over HTTP.

## Prerequisites

- SBCL (Steel Bank Common Lisp)
- Quicklisp package manager
- Emacs with Sly (optional, for interactive development)

## Installing Quicklisp

If you don't have Quicklisp installed:

```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
```

In the SBCL REPL:

```lisp
(quicklisp-quickstart:install)
(ql:add-to-init-file)  ; This adds Quicklisp to ~/.sbclrc so it loads automatically
(quit)
```

This automatically configures SBCL to load Quicklisp on startup by adding it to `~/.sbclrc`.

## Project Structure

```
claudex/
├── claudex.asd          # ASDF system definition
├── package.lisp         # Package definition
├── main.lisp           # Main application code
├── tests.lisp          # Test suite
├── start.lisp          # Development startup script
└── test.db             # SQLite database (created on first run)
```

## Dependencies

- **hunchentoot** - Web server
- **sqlite** - SQLite database interface
- **cl-who** - HTML generation
- **fiveam** - Testing framework

## Quick Start

### Option 1: Command Line

```bash
cd /path/to/claudex
sbcl --load start.lisp
```

The server will start automatically on `http://localhost:8080`

### Option 2: Using Sly in Emacs

1. Open any `.lisp` file in the project
2. `M-x sly` to start the REPL
3. In the REPL:

```lisp
;; Load the startup script
(load "start.lisp")
```

Or compile and load `start.lisp` directly:
- Open `start.lisp` in Emacs
- `C-c C-k` to compile and load the file

## Interactive Development with Sly

Once the server is running, you can modify code on the fly:

1. Edit functions in `main.lisp`
2. `C-c C-c` to recompile the current function
3. `C-c C-k` to recompile the entire file
4. Changes take effect immediately without restarting!

### Useful Sly Commands

- `C-c C-z` - Switch to REPL
- `M-.` - Jump to definition
- `M-,` - Jump back
- `C-c C-d d` - Describe symbol at point
- `C-c M-m` - Macroexpand

## Running Tests

```lisp
;; In the REPL:
(ql:quickload :claudex/tests)
(asdf:test-system :claudex)
```

## Building a Standalone Binary

Create `build.lisp`:

```lisp
(load "~/quicklisp/setup.lisp")
(push #P"./" asdf:*central-registry*)
(ql:quickload :claudex)

(defun main ()
  "Entry point for the standalone binary"
  (claudex:start-server)
  ;; Keep the server running - wait forever
  (handler-case 
      (loop (sleep 1))
    ;; Exit gracefully on Ctrl-C
    (sb-sys:interactive-interrupt ()
      (format t "~%Shutting down...~%")
      (claudex:stop-server)
      (sb-ext:exit :code 0))))

(sb-ext:save-lisp-and-die "claudex"
  :toplevel #'main
  :executable t
  :compression t)
```

Build:

```bash
sbcl --load build.lisp
```

Run:

```bash
./claudex
# Press Ctrl-C to stop
```

**Important**: The `main` function includes a loop to keep the server running. Without it, the binary would start the server and immediately exit.

## Key Learnings

### ASDF System Discovery

When using `(ql:quickload :your-system)`, Quicklisp/ASDF needs to know where your system is:

1. **Temporary (for development)**: Add to `asdf:*central-registry*` in your startup script
   ```lisp
   (push #P"./" asdf:*central-registry*)
   ```

2. **Permanent**: Symlink to Quicklisp's local projects
   ```bash
   ln -s /path/to/claudex ~/quicklisp/local-projects/claudex
   ql:register-local-projects
   ```

### The `start.lisp` Pattern

A startup script is the Common Lisp equivalent of a Makefile. It:
- Loads Quicklisp
- Registers your project temporarily
- Loads dependencies
- Starts your application
- Keeps everything project-specific and clean

### SQLite Library Note

We use the `:sqlite` package (not `:cl-sqlite`) as it's available in Quicklisp's default distribution.

## API

### Functions

- `(start-server &optional (port 8080))` - Start the web server
- `(stop-server)` - Stop the web server
- `(init-db)` - Initialize the database with sample data
- `(get-all-users)` - Fetch all users from the database

## Development Workflow

1. Start Sly: `M-x sly`
2. Load project: `(load "start.lisp")`
3. Make changes to code
4. Recompile changed functions: `C-c C-c`
5. Test in browser: `http://localhost:8080`
6. Run tests: `(asdf:test-system :claudex)`
7. Repeat!

## License

MIT

## Acknowledgments

Built with help from Claude, where Gemini CLI failed. 🎉
