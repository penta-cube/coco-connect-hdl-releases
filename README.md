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
  - `skill/component_detail.il`
  - `skill/overview.il`
  - `skill/property_crud.il`
  - `skill/connectivity.il`
  - `skill/export_block.il`
  - `skill/net_detail.il`

## Bridge Commands

- `ping`
- `status`
- `project_info`
- `component_detail`
- `net_detail`
- `design_overview`
- `page_overview`
- `export_block`
- `property_get`
- `property_set`
- `rename`
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
coco-connect-hdl project-info
coco-connect-hdl component-detail U1
coco-connect-hdl component-detail U1 --page 3
coco-connect-hdl net-detail VCC_3V3
coco-connect-hdl design-overview
coco-connect-hdl page-overview
coco-connect-hdl page-overview 3
coco-connect-hdl ui-navigate-part U1 --page 3
coco-connect-hdl ui-navigate-net VCC_3V3 --page 3
coco-connect-hdl export-block U100 --output block.json
coco-connect-hdl property-get U1 VALUE --page 3
coco-connect-hdl property-set U1 VALUE 10k --page 3
coco-connect-hdl rename R20_0 R20_1 --page 3
```

`component-detail` returns each pin's connected state, first wire DB id, and
resolved net name. Pass that net name to `net-detail` for net-wide pins and
wire coordinates.

Session-scoped IPC examples:

```text
coco-connect-hdl --instance-id HDL_1 status
coco-connect-hdl --instance-id HDL_1 ping
coco-connect-hdl --instance-id HDL_1 project-info
coco-connect-hdl --instance-id HDL_1 component-detail U1
coco-connect-hdl --instance-id HDL_1 component-detail U1 --page 3
coco-connect-hdl --instance-id HDL_1 net-detail VCC_3V3
coco-connect-hdl --instance-id HDL_1 design-overview
coco-connect-hdl --instance-id HDL_1 page-overview 3
coco-connect-hdl --instance-id HDL_1 ui-navigate-part U1 --page 3
coco-connect-hdl --instance-id HDL_1 ui-navigate-net VCC_3V3 --page 3
coco-connect-hdl --instance-id HDL_1 export-block U100
coco-connect-hdl --instance-id HDL_1 property-get U1 VALUE --page 3
coco-connect-hdl --instance-id HDL_1 property-set U1 VALUE 10k --page 3
coco-connect-hdl --instance-id HDL_1 rename R20_0 R20_1 --page 3
```

## Request Format

`property_set` and `rename` mutate the active design
immediately. They do not provide dry-run, audit, conflict preflight, or
rollback.

Property deletion is not exposed. Design Entry HDL does not reliably return a
mutable property DBID for either inherited or user-created instance properties,
so the available delete command cannot be verified as a safe deletion path.

The request file contains four newline-separated fields:

```text
id
token
op
arg
```

`arg` is empty for most public HDL commands. For `component_detail`, `arg`
contains `REFDES|PAGE`, where PAGE may be empty. For `net_detail`, `arg`
contains the logical net name. For `export_block`, `arg` contains a RefDes such
as `U1`; the Rust command converts its minimal raw payload to Schematic Document v1.
For `ui_navigate_part` and `ui_navigate_net`, `arg` contains `TARGET` or
`TARGET|PAGE` when a page hint is provided.
For `page_overview`, `arg` is empty for the active page or contains a page
number/canonical page name.
Property access and rename arguments use `|`-separated fields; empty optional
fields use the reserved `__COCO_EMPTY__` sentinel.

`project_info` has an empty argument and returns active design metadata plus
the current view's on-disk `view_source_path`.

## Response Format

The response line uses:

- `id<TAB>status<TAB>data`

`status` is `ok` on success or an error code on failure.

Success examples:

```text
<id>	ok	pong
<id>	ok	1|
<id>	ok	{"page":{"name":"page1"},"components":[],"wires":[]}
<id>	ok	{"page":{"name":"page1"},"components":[{"refdes":"U1","part_name":"IC","value":"","device":"","footprint":"","library_name":"logic","source_properties":[],"properties":[]}]}
<id>	ok	{"page":{"name":"page1"},"summary":{"component_count":1,"issue_count":1,"error_count":1,"warning_count":0,"info_count":0},"rules":["missing_refdes","placeholder_refdes","duplicate_refdes","missing_footprint","placeholder_value","placeholder_device"],"issues":[{"severity":"error","rule_id":"missing_footprint","object_dbid":"1","refdes":"U1","page":"page1","property_name":"footprint","current_value":"","message":"No footprint alias value was found"}]}
<id>	ok	{"refdes":"U1","page":{"name":"page1"},"components":[{"refdes":"U1","dbid":"1","component_name":"IC","library_name":"logic","x":"100","y":"200","angle":"0","mirror":"","page":{"name":"page1"},"pins":[{"name":"1","x":"90","y":"200","properties":[],"connected_wire_dbid":"","net_name":""}],"properties":[]}]}
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

```json
{"page":{"name":"page1"},"components":[{"refdes":"U1","part_name":"IC","value":"","device":"","footprint":"","library_name":"logic","source_properties":[],"properties":[]}]}
```

```json
{"page":{"name":"page1"},"summary":{"component_count":1,"issue_count":1,"error_count":1,"warning_count":0,"info_count":0},"rules":["missing_refdes","placeholder_refdes","duplicate_refdes","missing_footprint","placeholder_value","placeholder_device"],"issues":[{"severity":"error","rule_id":"missing_footprint","object_dbid":"1","refdes":"U1","page":"page1","property_name":"footprint","current_value":"","message":"No footprint alias value was found"}]}
```

```json
{"refdes":"U1","page":{"name":"page1"},"components":[{"refdes":"U1","dbid":"1","component_name":"IC","library_name":"logic","x":"100","y":"200","angle":"0","mirror":"","page":{"name":"page1"},"pins":[{"name":"1","x":"90","y":"200","properties":[],"connected_wire_dbid":"","net_name":""}],"properties":[]}]}
```
