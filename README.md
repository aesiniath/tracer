Add traces and spans to shell scripts
=====================================

Usage
-----

### Setup

The _tracer_ program has three commands, _init_, _exec_, and _send_.

To begin recording a trace of a set of programs or scripts, first call _init_
supplying `honeycomb` as the telemetry exporter and providing the name of the
dataset these events will be sent to:

```
$ tracer --telemetry=honeycomb --dataset=builder init
```

This writes a temporary file (defaulting to _.trace_) along the following
lines:

```json
{
    "start": 1654910414568468483,
    "trace": "fd531dbf96ecdc6ff156482aec6c24f7",
    "parent": "1d1e9d1234ec4649"
}
```

### Run steps

This file is then used by the other two commands _exec_ and _send_ to enable
them to know what trace they are a part of. Use _exec_ to execute a step:

```
$ tracer --telemetry=honeycomb --dataset=builder exec "label-one" command1 ...
$ tracer --telemetry=honeycomb --dataset=builder exec "label-two" command2 ...
$ tracer --telemetry=honeycomb --dataset=builder exec "label-three" command3 ...
```

By convention the "`label`" used as the name of the step you are executing is
the name of the program, script, or function your are executing, rather than
something particularly descriptive.

### Finalize

When the sequence of steps is complete, the trace is finished off by creating
a root span to be the parent of the previously created leaves and to represent
the total duration of the process:

```
$ tracer --telemetry=honeycomb --dataset=builder send "label"
```

<!--
Might help:
```
$ alias trace=tracer --telemetry=honeycomb --dataset=builder
```
-->
