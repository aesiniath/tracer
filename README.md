Add traces and spans to shell scripts
=====================================

Usage
-----

### Setup

```
$ tracer --telemetry=honeycomb --dataset=builder init 235238509232
```

Writes a temporary file (defaulting to _tracedetails.json_ or _.trace_)

```json
{
    "start": 1654910414,
    "trace": "fd533dbf96ecdc610156482ae36c24f7",
    "parent": "1d1e9dbf96ec4649"
}
```

### Run steps

This is then used by two commands _leaf_ and _root_. Use _leaf_ to execute a step 

```
$ tracer --telemetry=honeycomb --dataset=builder leaf "Label" command...
$ tracer --telemetry=honeycomb --dataset=builder leaf "Label" command...
$ tracer --telemetry=honeycomb --dataset=builder leaf "Label" command...
```

By convention the `Label` is the name of the function or step you are
executing. 

### Finalize

Finally, when the sequence of steps is complete, the trace is finished off by
creating a root span to be the parent of the previously created leaves and to
represent the total duration of the process:

```
$ tracer --telemetry=honeycomb --dataset=builder root "Label"
```

<!--
Might help:
```
$ alias trace=tracer --telemetry=honeycomb --dataset=builder
```
-->
