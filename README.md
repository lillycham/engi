# engi

Engi archives closed agent sessions. It copies each session transcript into a
folder, strips the image data and thinking signatures, compresses it with zstd and records it in a
manifest. It never changes or deletes an original.

Status as of 2026-11-07: `engi archive`, `engi verify`, `engi export` and
`engi strip` work and have tests. The strip rule is written in
[docs/strip-rule.md](docs/strip-rule.md) and covers Claude Code, Codex, omp and
pi sessions. The service definition is not part of this repository.

On one machine, one run over every session (262 files, 846 MB) took about 10
seconds at zstd level 9 and made an archive of about 82 MB. A second run reads
nothing and finishes at once, because it compares size and time with the last
copy.

The name comes from the Gensokyo Engi, the chronicle of origins in Touhou.

## Why

Agents keep full transcripts on disk, and some of them delete old sessions.
[akyuu](https://github.com/lillycham/akyuu) wants to point at the part of a
session that a memory came from. An archive keeps those sessions, so that the
pointers can still be resolved.

## Design (decided)

- Written in Haskell. MIT licence.
- The archive layer does not parse a transcript. It stores opaque bytes and a
  manifest. A reader for each agent format parses them later.
- Layout: `<agent>/<session id>.jsonl.zst` and an append-only `manifest.jsonl`.
  Files never change after they are written, so `rsync` copies the folder in
  increments.
- A session is closed when its file has not changed for 6 hours (a setting).
- A session that grows gets a new copy (`<session id>.2.jsonl.zst`). The old copy
  stays. Engi checks that the old copy is a prefix of the new one. It deletes
  nothing, on the machine or on the drive.
- It runs as a scheduled service. The person declares the service in their system
  configuration.
- Export is a separate step. It encrypts each file and the manifest with `age`
  and copies them to the external drive. It needs only the public key. It
  remembers which files it sent, because `age` output differs on every run.
- Image data and thinking signatures are stripped by the rule in
  `docs/strip-rule.md`.

## Commands

```
engi archive --archive DIR --source AGENT=DIR [--source ...] [--idle-hours 6] [--level 9] [-v]
engi verify  --archive DIR
engi strip   < transcript.jsonl > archive-form.jsonl
engi export  --archive DIR --dest DIR (--recipient KEY | --recipients-file FILE) [--age COMMAND] [-v]
```

`archive` prints the files it archived and the failures, and a summary. It exits
with status 1 if a file failed. `verify` exits with status 1 if a file is bad. A
file in the archive that the manifest does not list is reported and is not an
error.

Example:

```
engi archive --archive ~/archive/sessions \
  --source claude=~/.claude/projects \
  --source codex=~/.codex/sessions \
  --source omp=~/.omp/agent/sessions \
  --source pi=~/.pi/agent/sessions
```

## Export to a drive

`engi export` encrypts the archive with [age](https://age-encryption.org) and
copies it to a folder on a drive. It needs only the public key of the recipient,
so the private key can stay off this machine.

```
engi export --archive ~/archive/sessions --dest /Volumes/Backup/sessions \
  --recipient age1...
```

- Each compressed file goes to the drive as `<agent>/<name>.jsonl.zst.age`. The
  manifest goes as a snapshot, `manifest.N.jsonl.age`, where N is the number of
  entries. An older snapshot stays.
- Nothing on the drive is ever changed or deleted. A file that is on the drive
  counts as sent, because age output differs on every run and cannot be compared.
  A file is written to a temporary name and renamed, so a file that is there is
  complete.
- The destination folder must exist. If the drive is not mounted, the export
  stops with an error and does not make a folder on the disk.
- A private key (`AGE-SECRET-KEY-...`), as a recipient or in a recipients file, is
  refused.
- Each file is checked against the manifest before it is sent. A damaged file is
  not copied.
- The names on the drive (the agent and the session) are not encrypted. The
  content and the manifest are.
- The installed program has age on its PATH, so a scheduled job can run it.

To read a file back:

```
age -d -i key.txt -o session.jsonl.zst claude/<name>.jsonl.zst.age
zstd -d session.jsonl.zst
```

## One session in more than one place

An agent can hold one session in two folders. Claude Code copies a session when
a person resumes it from another folder, and it copies a subagent file into the
sessions that share it. Engi compares the content, not the path:

- the same content as the newest copy: nothing to do.
- a start of the newest copy: nothing to do, it is already contained.
- an extension of the newest copy: a new version.
- anything else with the same name: an error. The file is not archived.

The manifest records the path of each version.

## Build

```
nix develop
cabal build
```

Run `cabal2nix . > engi.nix` after a change to `engi.cabal`.
