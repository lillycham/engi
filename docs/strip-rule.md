# The strip rule

Status as of 2026-11-07: version 1, implemented in `Engi.Strip` (`engi strip`
reads stdin and writes stdout). Engi and any reader that checks a hash must
follow this text exactly.

## Why a rule

The archive does not keep the data of images, or the signatures of thinking
blocks. A record in the archive is the record from the original with those bytes
replaced. A pointer to part of a
session carries a hash. The hash must be the same for the live file and for the
archive. So both sides apply the same rule before they hash, and the rule must
be exact and fixed.

## What the rule does to a file

- The input is the bytes of a JSON Lines file. Records are separated by one
  line feed (byte 0x0A). The rule works on bytes. It does not decode the text.
- The rule changes the bytes of image data and signatures, and no other byte. It does not parse
  the JSON and it does not write it again. That would change key order, spacing
  and escapes, and every hash would change.
- Line endings, the last line feed (or its absence), and invalid UTF-8 pass
  through unchanged.

## What is stripped (version 1)

An anchor is a fixed text. A match is an anchor, then a run of base64
characters, then a closing quote (`"`, byte 0x22). The run is the value.

| Anchor | Where it comes from | What the run is |
|---|---|---|
| `"type":"base64","data":"` | Claude Code | image or document data |
| `"type":"base64","media_type":"` + media type + `","data":"` | Claude Code (the other field order) | image or document data |
| `"type":"image","data":"` | omp and pi | image data |
| `data:` + media type + `;base64,` | Codex, any data URI in a JSON string | image data |
| `"signature":"` | Claude Code | thinking signature |
| `"thinkingSignature":"` | omp and pi | thinking signature |
| `"encrypted_content":"` | Codex | encrypted reasoning |

The media type of a data URI, or of the second Claude Code anchor, is one or more of `A-Z`, `a-z`, `0-9`, `/`, `.`,
`+` and `-`.

A base64 character is `A-Z`, `a-z`, `0-9`, `+`, `/`, `=`, `-` or `_`. This is the
alphabet of base64 and of base64url together, because Codex uses base64url for
`encrypted_content`. A run shorter than 200 characters is not stripped: it is
not a real image or signature, and a short value may be something else. A run
that the closing quote does not follow at once is not stripped either (for
example, a value inside an escaped string, which ends with a backslash).

The text before the run (the anchor, and for a data URI the media type) is kept.
The text after the closing quote is kept. For example, `media_type` stays.

A signature and encrypted reasoning are not needed to read a session. They are
only needed to resume one, and the originals stay for that.

omp already stores an image outside its session file, as
`"data":"blob:sha256:<hash>"`, and keeps the images in a blob folder. That value
is short and is not base64, so the rule does not touch it. Engi does not copy the
blob folder: images are dropped by decision, and the reference stays in the
record.

Whether to strip `encrypted_content` was not decided by the person yet. It is the
Codex equivalent of a thinking signature, so version 1 includes it. Remove the
row if that is wrong. Nothing depends on version 1 until the code ships.

## What replaces the data

The run is replaced by

```
engi-stripped-v1:<length>:<sha256>
```

- `<length>` is the number of characters in the removed run, in decimal.
- `<sha256>` is the SHA-256 of those characters, in lower case hex.

The replacement is a valid JSON string body. It contains `-` and `:`, which are
not base64 characters. So the rule gives the same result when it is applied twice:
the replacement is never stripped again.

## What the hash covers

The hash of a span of records is the SHA-256 of the records of the span after the
rule, each with its line feed, in order.

## Version

The version is 1. The manifest of the archive and every reader record it. If the
rule changes in any way, the version changes, and the hashes change with it. A
pointer made under an old version then reports `changed` and not `intact`. That
is the correct answer.

## Why signatures are stripped

Claude Code stores a long base64 value in each thinking block, under the key
`signature`. It is high-entropy and does not compress. Decided on 2026-11-07:
version 1 strips it with the images.

Measurement on one 45 MB session (zstd level 3):

| Form | Compressed |
|---|---|
| The original | 12.5 MB |
| Image data stripped | 11.3 MB |
| Image data and signatures stripped | 9.9 MB |

Stripping images alone saves about 10 percent here, not most of the size. The rest
of the file is text that compresses about three times. The signatures are 919
values and about 1.8 MB before compression.

Measurement on every session on one machine (262 files from Claude Code, Codex,
omp and pi), one zstd level 3 stream over all the files:

| Form | Bytes | Compressed |
|---|---|---|
| The original | 845.6 MB | 195.1 MB |
| After the rule | 610.5 MB | 94.4 MB |

So the rule more than halves the compressed size. Engi compresses one file at a
time, which compresses a little worse than one stream, so expect a bit more.
Check with the rule on every file: no line is added or lost, and the rule
applied twice gives the same bytes. The rule left no value after an anchor
unstripped. The only long base64-like values left are file paths in two Claude
Code fields (`scratchpadDirectory`, `realParentDir`), which the rule must not
touch.

The first measurement used the pattern `"(data|signature)":"[A-Za-z0-9+/=]{200,}"`.
The rule above is narrower for images: it needs the anchor
`"type":"base64","data":"`. Check on real files that the narrow rule strips the
same runs.

## Tests the implementation needs

- A Claude Code style record with an image: the run is replaced, every other byte
  is the same.
- An omp style record with an image (`"type":"image","data":"<run>"`): the run is
  replaced. A blob reference (`blob:sha256:...`) stays.
- A Codex style record with a data URI: the media type stays, the run is replaced.
- Codex `encrypted_content` with base64url characters (`-` and `_`): the run is
  replaced.
- A run that the closing quote does not follow (a backslash, a space) stays.
- A thinking block with a signature: the run is replaced, the thinking text stays.
- A run of 199 characters is not stripped. A run of 200 is.
- A `signature` value shorter than 200 characters stays.
- Applying the rule twice gives the same bytes as applying it once.
- A file with no images comes out byte for byte the same.
- Invalid UTF-8, a missing final line feed and an empty file pass through.
- The hash of a span is the same for a live file and for its archived copy.
