# rexel end-to-end scenarios (elate)

[`elate`](https://github.com/dakra/elate) scenarios that drive rexel in a sandboxed
Emacs against a running Rex server: the commands, their prompts and the keys a user
types, with the shell in the block as the oracle for what arrived.  They need a live
server, so they run on a developer's machine and not in CI; `make` covers what runs
without one.

## Running

```sh
test/elate/run.sh                                  # every scenario, every variant
test/elate/run.sh test/elate/matrix/upload.json    # one scenario
test/elate/run.sh -v unix,tcp test/elate/matrix/typing.json
ELATE_FORMAT=json test/elate/run.sh -v local test/elate/matrix/session.json
```

`run.sh` runs each scenario once per variant with `elate run --keep-going`, so every
group reports, and exits non-zero when one failed.  It needs `uvx` (or `ELATE` naming
the elate command, 0.16 or later), `socat`, `python3` and a running Rex server on this
machine.  A single run without it works too:

```sh
uvx elate run --keep-going --variant unix test/elate/matrix/typing.json
```

Environment, all optional:

| Variable | Meaning | Default |
|---|---|---|
| `REXEL_ELATE_SOCK` | socket of the Rex server | this user's server |
| `REXEL_ELATE_GHOSTEL` | ghostel checkout, with its built module | `~/.emacs.d/lib/ghostel`, `~/.cache/ghostel` |
| `REXEL_ELATE_WEBSOCKET` | websocket.el checkout | `~/.emacs.d/lib/websocket`, `~/.cache/emacs-websocket` |
| `REXEL_ELATE_COMPAT` | compat checkout, if ghostel needs it | `~/.emacs.d/lib/compat` |
| `REXEL_ELATE_TAILNET` | the server's https address on the tailnet; enables the `tls` variants | unset, `tls` is skipped |
| `REXEL_ELATE_LABEL` | prefix of the session labels | `rexel-elate`; `run.sh` adds its pid |

## What a run touches

Every scenario creates its own Rex session, labelled `rexel-elate-…`, and destroys it
in its `cleanup` group; `run.sh` also kills the sessions and relays a run that was cut
short left behind.  Other sessions on the server are listed in prompts and never
attached or changed.  Files to upload are made under the temporary directory and
removed again.

## Files

- `lib/rexel-setup.el` is loaded at startup.  It puts rexel from this checkout,
  ghostel and websocket.el on the load-path and defines the `rexel-elate-` helpers the
  steps call.
- `matrix/*.json` are the scenarios.  A `group` is one behaviour and reports one
  verdict.
- `run.sh` runs them.

### Hosts

The sandbox's `HOME` is not the user's, so the rex program finds no server there.
`rexel--hosts` is replaced by the hosts a scenario chooses with `rexel-elate-use`;
everything after host discovery is rexel's own code.  The variants are these hosts:

| Variant | Server address | Counts as |
|---|---|---|
| `local` | the server's Unix socket, no label | this machine's server |
| `unix` | a socat relay on another Unix socket | a server on another machine |
| `tcp` | a socat relay on `http://127.0.0.1:PORT` | a server on another machine |
| `fallback` | a dead address, then the `tcp` relay | a host with several endpoints |
| `tls` | `REXEL_ELATE_TAILNET` | a server on the tailnet |

Emacs starts the relays, so a scenario can kill one, freeze its connections and start
it again.

### Scenarios

| Scenario | Variants | Groups |
|---|---|---|
| `session.json` | local, unix, tcp | `new-session`, `type`, `directory`, `rename`, `label-function`, `server-label`, `detach`, `attach`, `kill-buffer`, `host-prompt`, `attach-tabs`, `attach-again`, `detach-refresh`, `destroyed-elsewhere`, `no-server` |
| `typing.json` | local, unix, tcp, fallback, tls | `echo`, `burst`, `edit`, `paste`, `unicode`, `interrupt`, `full-screen`, `keys`, `scrollback`, `copy-mode`, `flood`, `large-paste`, `bracketed-paste` |
| `layout.json` | local, unix | `size`, `resize`, `resize-full-screen`, `taken-back`, `smallest-window`, `split-right`, `split-below`, `split-left`, `split-above`, `split`, `sync-layout`, `new-tab`, `sync-tab`, `close-block`, `exit` |
| `reconnect.json` | unix, tcp | `stream-drop`, `control-drop`, `write-in-flight`, `upload-redial`, `relay-restart`, `half-open`, `upload-stall`, `upload-cut`, `lost-connect`, `refused-hello` |
| `upload.json` | unix, tcp, tls | `upload-file`, `several`, `empty`, `boundaries`, `symlink`, `names`, `compressed`, `dnd`, `drop`, `yank-media`, `dired`, `seventeen`, `remote-file`, `not-a-file`, `too-large`, `stays-usable`, `slow`, `file-changes`, `cancel`, `after-cancel`, `cancel-idle`, `two-blocks`, `kill-buffer` |
| `local.json` | — | `directory`, `command-running`, `upload-file`, `remote-file`, `drop`, `yank-media`, `exec-shell` |

Each scenario's `comment` and its groups' comments say what is checked.

## Conventions

- A block's first step replaces its shell with `/bin/bash --norc --noprofile` and the
  prompt `$ ` (`rexel-elate-shell`), so the user's shell configuration does not matter.
- Commands print a marker the typed text does not contain, such as `echo t1-$((6*7))`
  for `t1-42`, so a match proves the command ran.
- Text is read from the terminal with `rexel-elate-has`, which also sees a buffer no
  window shows.  What Emacs rendered is checked with `wait until (rexel-elate-shows …)`;
  the buffer is redrawn a moment after the terminal has the text.
- `type` sends raw terminal input.  Wait until the shell shows the text before a step
  relies on it, and turn the tty's echo off before sending bytes a program reads raw.
- A marker that proves nothing was typed after it is not anchored at the line start:
  a long message in the echo area resizes the terminal and the shell redraws its line.
- Sizes are checked with `rexel-elate-fits-p`, the terminal against the windows showing
  it, and with `echo TAG $(stty size)`, the program against the terminal.  After a
  `resize` step, wait for `(frame-width)` first.
- `session.json` and `local.json` make `ghostel--osc133-marker` ignore the marks of the
  user's shell startup files where they check `default-directory` or a running command,
  so the block's shell counts as one without integration.
- `bracketed-paste` needs a bash of 5.1 or later and takes the first it finds.
- Uploads are checked with `cksum`: it is typed first, Rex types the stored paths after
  it, and the output is compared with the checksum of the local file.
  `rexel-elate-hold-upload` stops an upload after a number of bytes, for a step that
  has to see it halfway through a file.
- A terminal Emacs has no drops and no image clipboard.  `rexel-elate-dnd` calls the
  `dnd` dispatch of X11 and pgtk, `rexel-elate-drop` queues the `drag-n-drop` event of
  NS and w32, and `rexel-elate-yank-media` calls the handler `yank-media` registered.
- A check that fails because of a known defect in rexel carries `"expect": "fail"` and
  its `"reason"`, or an `"xfail"` map when only some variants fail; it turns into an
  XPASS, which fails the run, once the defect is gone.
