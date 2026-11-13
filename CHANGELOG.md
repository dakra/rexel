# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Fixed
- An Emacs input method such as `chinese-py` composes in a block, whose
  buffer was read-only.

## [0.3.0] — 2026-11-11

### Added
- `rexel-destroy-empty-sessions`, `t` by default, destroys an attached
  session once its last block closes, whatever closes it: a killed
  buffer, `rexel-close-block`, a shell that exits, or another client.
  With nil the session stays on the server without a tab.

### Changed
- Requires ghostel 0.64.0, and uses its public functions for terminals
  without a process.
- Size requests follow ghostel in copy mode: a window split there asks
  for no size, and the size is asked for when copy mode is left.  The
  program under the frozen view was resized.  A size another client
  gives the block still applies in copy mode.
- The commands that open a session ask to install ghostel's native
  module before connecting, or signal a `user-error`.  Without the
  module they connected and then failed once per block.
- Attaching a session without a terminal block, from `rexel-attach` or
  consult-rexel, says so.  It showed nothing before.

## [0.2.0] — 2026-11-11

### Added
- Rex servers over the network: a host can be an https or http URL, or a
  bare address reached over https, such as the address Rex's Tailscale
  gives a server.  `rexel-hosts` takes one endpoint or a list, tried in
  order, and an address without a port is also tried on Rex's port 7297.
  Hosts known to Rex with only network endpoints are listed.
- A connection silent for a while is pinged and closed when the server
  does not answer, which redials or resumes it.  Typing while a write is
  unanswered pings at once.
- File uploads: `rexel-upload-file` (`C-c C-r u`) sends a file read from
  the minibuffer or the files marked in Dired to the block's machine, and
  the server types the uploaded path.  Dropped files and `yank-media`
  upload too.  A file on a TRAMP host is uploaded, a local file on this
  machine's server is typed as its path.  `rexel-cancel-upload` stops an
  upload.
- `rexel` opens a terminal of the session `rexel-default-session`
  ("emacs") on this machine's server, as `M-x ghostel` does: a prefix
  argument opens a tab with a shell in `default-directory`, a numeric
  one shows that tab.  `rexel-default-session-function` names the
  session per buffer; `rexel-project-session-label` picks the project's.
- `rexel-project`, `rexel-project-new` and `rexel-project-kill`, in
  `rexel-project.el`: one session per project, labelled with the
  project's name, its shells starting in the project's root.
  `rexel-project-kill` kills the project's buffers and destroys its
  session.
- In a TRAMP directory, `rexel`, `rexel-project` and the consult-rexel
  project picker use the Rex server of that machine, found through
  `rexel-tramp-hosts`, a host labelled with its name, or an address
  containing it.  A block's directory is under the TRAMP prefix.
- `rexel-attach-display` chooses what `rexel-attach` shows: a block or
  tab layout picked from a list (`ask`, the default), the focused block,
  or the active tab's layout.  `C-u rexel-attach` always asks.
- consult-rexel, a package of its own: `consult-rexel` and
  `consult-rexel-project` list block buffers by session, sessions
  without buffers and new tabs and sessions, with previews, and include
  consult-ghostel's sources when it is installed.  `consult-rexel-tab`
  rebuilds the windows from a tab.  `consult-rexel-mode` previews the
  block and tab prompts of rexel and adds the blocks to
  `consult-ghostel` and `consult-buffer`.  With Embark it adds actions
  on sessions and block buffers.
- `rexel-kill-buffer-action`: killing a block buffer closes the block,
  asking first when it runs a command.  `keep` leaves it running and
  `ask` asks.  The block kept running and its buffer came back with the
  next view change.
- Emacs takes back the block in the selected window of a focused frame,
  when the window is selected, the frame gets the focus or the window
  changes size.  While another client sizes a block,
  `mode-line-misc-info` shows "[Sized by another client WxH]", and
  `mouse-1` on it takes the block.
- `a` and `n` in `rexel-prefix-map` run `rexel-attach` and
  `rexel-new-session`, which work in any buffer, so the map can be bound
  globally.
- The Emacs colors are sent to the server, which answers programs' color
  queries with them.
- Block buffers show "connecting" or "disconnected" in the buffer
  pickers while their stream is not live.

### Changed
- Requires ghostel 0.63.0 and a Rex server with control API revision 11.
- The Lisp files are in `lisp/`; a `package-vc` recipe before Emacs 31.1
  names it with `:lisp-dir "lisp"`.
- Block buffers are named `*rex:SESSION:TAB*` after the block's tab,
  with `TAB.2` and so on for further blocks of a tab.  They were named
  after the end of the block ID.
- New shells start in the current directory: a session's in
  `default-directory`, a split's or tab's in the directory of the current
  block's shell.  They started in the home directory.
- `3` and `2` behind the prefix split right and below; `%` and `"` are
  gone.
- Exiting Emacs leaves the servers at once.  Other clients saw Emacs as
  lost for 60 seconds.

### Fixed
- A write to a connection that stopped taking data hung Emacs, and `C-g`
  did not help.  Such a write is given up and its connection redialled
  or resumed.
- `rexel-prefix-key` set through Customize stopped rexel from loading.
- A block whose `data.connect` answer was lost with the control
  connection, or whose first HELLO was refused, stayed blank.
- After a redial the server does not resume, keys typed before the
  session was attached again were lost and resizes failed.
- Typing or resizing during a large write could stop typing or sizing
  until the next reconnect, or send a second write ahead of the first.
- A resumed stream kept the size it had before, though its window changed
  meanwhile.
- A key typed while a dropped connection had a write in flight was
  dropped silently; rexel says the keys may be lost.
- The answer to a view request sent before a detach brought the
  session's buffers back, as did a redial's attach answered after a
  detach.
- The login wrapper, or `rex` before it execs login, was taken for a
  block's shell and counted as a running command.
- The preview of a buffer picker took every block it passed over from
  the other clients.
- `C-g` out of a prompt opened by a server answer dropped the messages
  after it.
- `rexel-sync-layout` found no tab for a block in a floating layer, and
  reported an error for a tab with an empty tiled layout.

## [0.1.0] — 2026-11-06

### Added
- First release.  `rexel-attach` opens a session of a Rex server as one
  ghostel buffer per terminal block: the server's snapshot and PTY output
  feed ghostel's terminal, and typing goes back over Rex's control API.
- Several servers: this machine's, the hosts Rex knows with a Unix
  socket, and `rexel-hosts`.  `rexel-start-server` starts this machine's,
  and `rexel-autostart-server` decides whether attaching offers to.
- `rexel-new-session`, with `rexel-session-label-function` to rewrite the
  label; `rexel-detach`.
- The prefix map on `C-c C-r` in block buffers: splits to every side,
  `rexel-new-tab`, `rexel-rename-session`, `rexel-close-block` and
  `rexel-sync-layout`, which rebuilds the frame's windows from a tab.
- A split shows the new block beside its window, on the side of the Rex
  split, and selecting a block's window focuses it on the server.
- A dropped stream resumes where it stopped, and a lost control
  connection is redialled with backoff while sessions are attached.
- The shell's directory and running command follow Rex's process
  events, until the shell's own OSC 133 marks take over.
- Session label changes rename the block buffers.
