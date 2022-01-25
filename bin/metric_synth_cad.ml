let () =
  Memtrace.trace_if_requested ();
  Core.Command.run Staged_synth.Metric_synth_cad.cmd
