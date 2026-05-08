# coco-connect-hdl-releases

Release assets and runtime scripts for `coco-connect-hdl`.

## Contents

- Windows executable release asset:
  - `coco-connect-hdl.exe`
- HDL SKILL bridge scripts:
  - `skill/bridge.il`
  - `skill/util.il`
  - `skill/hdl.il`
  - `skill/read_model.il`

## Bridge Commands

- `ping`
- `status`
- `read_model`
- `quit`

`coco-connect-hdl` uses file-based IPC because SKILL `infile` / `outfile`
cannot open Windows named pipes. The bridge exchanges request and response files
under the configured work directory, normally `%TEMP%`.

## File Layout

For an IPC base name such as `coco-hdl`, the bridge uses:

- `coco-hdl-rdy.txt` - written by `coco-connect-hdl` when it is ready
- `coco-hdl-req.txt` - written by `coco-connect-hdl`, read by SKILL
- `coco-hdl-res.txt` - written by SKILL, read by `coco-connect-hdl`
- `coco-hdl-lock.txt` - written by SKILL while one bridge instance is active

## SKILL Usage

Manual SKILL startup in the HDL CommandConsole:

```skill
system("cmd /c cd /d C:\\path\\to\\skill && start /b cnskill.exe -i -nongraph bridge.il")
```

`bridge.il` loads the other `*.il` files from `_coco_bridge_dir`; by default
that is the current directory. If a wrapper loads `bridge.il` by absolute path,
set `_coco_bridge_dir` first.

When Chat CoCo creates a generated bridge wrapper, it sets:

```skill
(setq _coco_pipe_base "coco-hdl-<INSTANCE_ID>")
(setq _coco_instance_id "<INSTANCE_ID>")
(setq _coco_work_dir "C:\\Users\\<USER>\\AppData\\Local\\Temp")
(setq _coco_bridge_dir "C:\\path\\to\\skill")
(load "C:\\path\\to\\bridge.il")
```

The values must match the CLI or MCP server options:

```text
coco-connect-hdl --instance-id <INSTANCE_ID> mcp
coco-connect-hdl --pipe-name coco-hdl-<INSTANCE_ID> status
```

## CLI Usage Examples

```text
coco-connect-hdl status
coco-connect-hdl ping
coco-connect-hdl session-status
coco-connect-hdl read-model
```

Session-scoped IPC examples:

```text
coco-connect-hdl --instance-id HDL_1 status
coco-connect-hdl --instance-id HDL_1 ping
coco-connect-hdl --instance-id HDL_1 read-model
```

## Request Format

The request file contains four newline-separated fields:

```text
id
token
op
arg
```

`arg` is empty for the public HDL commands.

## Response Format

The response line uses:

- `id<TAB>status<TAB>data`

`status` is `ok` on success or an error code on failure.

Success examples:

```text
<id>	ok	pong
<id>	ok	1|
<id>	ok	{"page":{"name":"page1"},"components":[],"wires":[]}
```

Error example:

```text
<id>	HDL_NOT_EXPORTED	cnmpsImport raised an error
```

The Rust CLI and MCP server convert successful payloads into JSON for callers:

```json
{"imported":true,"detail":""}
```

```json
{"page":{"name":"page1"},"components":[],"wires":[]}
```
