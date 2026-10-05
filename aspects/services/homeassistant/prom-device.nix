{
  den,
  lib,
  fleet,
  ...
}:
# node_exporter metrics as Home Assistant devices: one device per fleet host,
# plus one that aggregates them, every sensor polled out of Prometheus's
# instant-query API by the `prom_device` custom component
# (../../../packages/prom-device.nix).
#
# Why this shape rather than any of the alternatives: Prometheus already
# scrapes these hosts every few seconds, so a second collector (Glances, an
# MQTT bridge, a timer writing to the HA REST API) would be the same data
# gathered twice and two things to keep running. The component only reads.
#
# Why it is derived from `fleet` (../../hosts/fleet.nix) rather than written
# out per host: the ONLY per-host facts a device needs are the host's name and
# whether Prometheus can reach it, both of which are already on the fleet
# record. Giving a Linux host a `lanAddress` is therefore the whole of "add it
# to the monitoring" -- it gets node_exporter, a scrape target, and an HA
# device with the full query set, with nothing written here.
#
# It reads `fleet`, NOT `inputs.self.nixosConfigurations.<host>.config`. Asking
# another host's configuration what its node_exporter port is would make every
# evaluation of this box evaluate that box as well; ../../base/celler/server.nix
# is the one documented exception in this repo and this is not it. The cost is
# that `nodePort` below has to be a constant shared by both ends.
let
  # Prometheus and Home Assistant both run here (../../hosts/secondpc/observability.nix
  # and ./default.nix). Named once: it decides which host grows the scrape
  # config, and which end of the scrape is loopback.
  prometheusHost = "secondpc";

  # NOT 9090. ../../hosts/secondpc/observability.nix moves Prometheus off the
  # default port, and the component talks to the API directly rather than
  # through nginx.
  prometheusPort = 9091;

  # node_exporter's port on EVERY monitored host, including this one. A
  # constant rather than `config.services.prometheus.exporters.node.port`,
  # because the scrape targets are remote hosts whose config this file must not
  # read (see the header). Written as a plain definition, not `mkDefault`, so
  # that if observability.nix ever moves the local exporter the two ends
  # collide loudly at evaluation instead of the scrape quietly failing.
  nodePort = 9000;

  # A job of its own, beside observability.nix's `node`. That job scrapes this
  # host's exporter with no `instance` label of its own, so its series are
  # `instance="localhost:9000"` -- a name no fleet-wide query can derive and
  # that nothing else in the fleet can match the shape of. Rather than rewrite
  # it (which would retroactively change the `instance` of every node series
  # Prometheus has stored, and blank any dashboard pinned to the old value),
  # this job carries an explicit `instance = <fleet host name>` for every host,
  # which is exactly what the per-device queries below match on.
  #
  # The cost is that this host's exporter is scraped by both jobs. Hence the
  # slower interval: the devices poll once a minute, so 30s is already three
  # times the resolution anything here can show, and it cuts the duplicate to
  # a third of the global 10s. Collapsing the two jobs into one is a safe
  # follow-up once nothing is pinned to `instance="localhost:9000"` any more.
  jobName = "node-fleet";

  # /sys/class/powercap/intel-rapl*/energy_uj is 0400 root since the Platypus
  # side channel (CVE-2020-8694), and node_exporter runs as `node-exporter`, so
  # `node_rapl_*` is absent and the "CPU package power" query returns an empty
  # vector -- no entity, no error. Flipping this to `true` adds a unit per
  # monitored host that hands the counters to the exporter's own group (0440
  # root:node-exporter, not world-readable), which is the only way node_exporter
  # can report package power.
  #
  # Left off because it trades a documented key-recovery side channel for a
  # power reading, and that is a decision about the machines rather than about
  # Home Assistant.
  raplReadable = false;

  # Which fleet entries get a device.
  #
  # `lanAddress != null` is the real predicate: it is the only fleet field that
  # says "Prometheus can reach this box at a fixed address". A roaming laptop
  # has none (../../hosts/fleet.nix says why), and a target that is usually
  # down is worse than no target -- it is a device whose every sensor is
  # unavailable.
  #
  # `system` and `hasSuffix "linux"` keep out the darwin hosts, which have no
  # `services.prometheus`, and `fleet.external` entries, which have no `system`
  # at all.
  monitored = lib.filterAttrs (
    _: h: (h.lanAddress or null) != null && lib.hasSuffix "linux" (h.system or "")
  ) fleet;

  hostNames = builtins.attrNames monitored;

  # Per-host opt-outs, by the `needs` tag on a query below -- one of `swap`,
  # `rapl`, `hwmon`, `thermal`, `zfs`, `conntrack` or `systemd`. EMPTY,
  # deliberately:
  # a metric a host cannot produce returns an empty instant vector, which the
  # component turns into no entity rather than an error, so "this host has no
  # hwmon / no RAPL / no second disk" already costs nothing and needs no
  # declaration. The exception is a query that returns a WRONG value rather than
  # nothing -- `0/0` on a swapless host, which arrives as NaN and shows as
  # unknown (left that way on purpose: an unknown swap sensor says "no swap",
  # a missing one says nothing) -- so this is where a host would say
  # `secondpc = [ "swap" ]` to drop the entity instead.
  omitMetrics = { };

  # The query set, written ONCE and instantiated per host. `inst` is the
  # host's `instance` label, which the scrape config below sets to the fleet
  # name.
  #
  # The matcher is spliced INSIDE each selector's braces rather than appended
  # to the expression: a matcher outside cannot reach into `rate(...)`, and
  # without it a query returns one series per host and the component would
  # hang every host's disks off whichever device asked first. Result ordering
  # is not stable, so this is not a thing that fails visibly.
  hostQueries =
    inst:
    let
      i = ''instance="${inst}"'';

      # node_exporter's filesystem collector already drops the pseudo
      # filesystems (`--collector.filesystem.fs-types-exclude` covers overlay,
      # squashfs, devtmpfs, nsfs and friends). `tmpfs` is the one it keeps and
      # that nobody wants a disk gauge for; `overlay` is belt and braces for a
      # container runtime that mounts one somewhere the default exclusion
      # misses.
      fs = ''fstype!~"tmpfs|overlay",${i}'';

      # `loop.*` is a mounted image, `dm-.*` the mapper half of a device that
      # is already counted under its own name.
      blk = ''device!~"loop.*|dm-.*",${i}'';

      # `lo` double-counts every local connection; the rest are container and
      # bridge halves of traffic that also crosses the physical NIC.
      nic = ''device!~"lo|veth.*|docker.*|br-.*",${i}'';

      # One CPU mode as a percentage of all of them. `avg` over the `cpu`
      # label, so a 16-thread box and a Pi both read 0-100.
      cpuMode = mode: ''100 * avg(rate(node_cpu_seconds_total{mode="${mode}",${i}}[2m]))'';

      # Pressure stall information: the share of the last two minutes in which
      # at least one task was waiting on this resource. The single best
      # "is this machine actually struggling" number, and nothing else here
      # reports it -- a box can sit at 30% CPU and still be unusable.
      pressure = resource: "100 * rate(node_pressure_${resource}_waiting_seconds_total{${i}}[2m])";

      bytesPerSec = expr: {
        query = "${expr} / 1e6";
        unit = "MB/s";
        device_class = "data_rate";
        digits = 2;
      };
      gigabytes = expr: {
        query = "${expr} / 1e9";
        unit = "GB";
        device_class = "data_size";
      };
      percent = query: {
        inherit query;
        unit = "%";
      };
      # No `digits`: the component already defaults to 1, and setting it here
      # would win over an explicit `digits` on the left of the `//` that merges
      # this in -- which is exactly what it did to the two counters below that
      # want whole numbers.
      perSecond = query: {
        inherit query;
        unit = "/s";
      };
    in
    [
      # --- CPU ---
      (
        {
          name = "CPU utilisation";
        }
        // percent ''100 * (1 - avg(rate(node_cpu_seconds_total{mode="idle",${i}}[2m])))''
      )
      (
        {
          name = "CPU user";
          digits = 2;
        }
        // percent (cpuMode "user")
      )
      (
        {
          name = "CPU system";
          digits = 2;
        }
        // percent (cpuMode "system")
      )
      (
        {
          name = "CPU iowait";
          digits = 2;
        }
        // percent (cpuMode "iowait")
      )
      # Nonzero only in a VM, where it is the hypervisor taking the CPU away.
      (
        {
          name = "CPU steal";
          digits = 2;
        }
        // percent (cpuMode "steal")
      )
      (
        {
          name = "CPU pressure";
          digits = 2;
        }
        // percent (pressure "cpu")
      )
      {
        name = "CPU cores";
        # `count by (cpu)` collapses the mode series to one per thread, and
        # the outer `count` turns that into how many there are.
        query = "count(count by (cpu) (node_cpu_seconds_total{${i}}))";
        digits = 0;
      }
      {
        name = "CPU frequency";
        query = "avg(node_cpu_scaling_frequency_hertz{${i}}) / 1e6";
        unit = "MHz";
        device_class = "frequency";
        digits = 0;
      }
      {
        # One selector, three metrics. The `{__name__}` placeholder is also
        # what tells the component to keep `__name__` in the entity key -- it
        # does that whenever a query returns more than one metric name, which
        # is true here and nowhere else below.
        name = "Load {__name__}";
        query = ''{__name__=~"node_load(1|5|15)",${i}}'';
        digits = 2;
      }
      {
        # Empty vector unless `raplReadable` is on -- see its comment. `sum()`
        # of nothing is nothing, so this is an entity that does not appear,
        # not one that reads 0 W.
        name = "CPU package power";
        query = "sum(rate(node_rapl_package_joules_total{${i}}[2m]))";
        unit = "W";
        device_class = "power";
        needs = "rapl";
      }

      # --- Memory ---
      (
        {
          name = "RAM utilisation";
        }
        // percent "100 * (1 - node_memory_MemAvailable_bytes{${i}} / node_memory_MemTotal_bytes{${i}})"
      )
      # `MemAvailable`, not `MemFree`: page cache is not memory you have lost.
      (
        {
          name = "RAM used";
        }
        // gigabytes "node_memory_MemTotal_bytes{${i}} - node_memory_MemAvailable_bytes{${i}}"
      )
      ({ name = "RAM available"; } // gigabytes "node_memory_MemAvailable_bytes{${i}}")
      ({ name = "RAM total"; } // gigabytes "node_memory_MemTotal_bytes{${i}}")
      (
        {
          name = "RAM cached";
        }
        // gigabytes "node_memory_Cached_bytes{${i}} + node_memory_Buffers_bytes{${i}}"
      )
      {
        name = "RAM dirty";
        query = "node_memory_Dirty_bytes{${i}} / 1e6";
        unit = "MB";
        device_class = "data_size";
      }
      (
        {
          name = "Memory pressure";
          digits = 2;
        }
        // percent (pressure "memory")
      )
      {
        # NaN, hence unknown, on a host with no swap -- see `omitMetrics`.
        name = "Swap utilisation";
        query = "100 * (1 - node_memory_SwapFree_bytes{${i}} / node_memory_SwapTotal_bytes{${i}})";
        unit = "%";
        needs = "swap";
      }
      (
        {
          name = "Swap used";
          needs = "swap";
        }
        // gigabytes "node_memory_SwapTotal_bytes{${i}} - node_memory_SwapFree_bytes{${i}}"
      )
      # Absent, so no entity, on a host with no zpool.
      (
        {
          name = "ZFS ARC size";
          needs = "zfs";
        }
        // gigabytes "node_zfs_arcstats_size{${i}}"
      )

      # --- Storage, per mount ---
      (
        {
          name = "Disk utilisation {mountpoint}";
        }
        // percent "100 * (1 - node_filesystem_avail_bytes{${fs}} / node_filesystem_size_bytes{${fs}})"
      )
      (
        {
          name = "Disk used {mountpoint}";
        }
        // gigabytes "node_filesystem_size_bytes{${fs}} - node_filesystem_avail_bytes{${fs}}"
      )
      # GB, not GiB: `data_size` accepts both, and the divisor has to match the
      # unit or HA records a number three percent wrong rather than rejecting
      # it.
      ({ name = "Disk free {mountpoint}"; } // gigabytes "node_filesystem_avail_bytes{${fs}}")
      ({ name = "Disk size {mountpoint}"; } // gigabytes "node_filesystem_size_bytes{${fs}}")
      # Runs out independently of the bytes, and when it does everything fails
      # with ENOSPC on a filesystem that looks half empty.
      (
        {
          name = "Inodes free {mountpoint}";
        }
        // percent "100 * node_filesystem_files_free{${fs}} / node_filesystem_files{${fs}}"
      )

      # --- Storage, per block device ---
      ({ name = "Disk read {device}"; } // bytesPerSec "rate(node_disk_read_bytes_total{${blk}}[2m])")
      ({ name = "Disk write {device}"; } // bytesPerSec "rate(node_disk_written_bytes_total{${blk}}[2m])")
      ({ name = "Disk reads {device}"; } // perSecond "rate(node_disk_reads_completed_total{${blk}}[2m])")
      (
        { name = "Disk writes {device}"; } // perSecond "rate(node_disk_writes_completed_total{${blk}}[2m])"
      )
      # Share of wall time the queue was non-empty: saturation rather than
      # throughput, which is what tells a slow disk from a busy one.
      (
        {
          name = "Disk busy {device}";
          digits = 1;
        }
        // percent "100 * rate(node_disk_io_time_seconds_total{${blk}}[2m])"
      )
      (
        {
          name = "IO pressure";
          digits = 2;
        }
        // percent (pressure "io")
      )

      # --- Network, per interface ---
      ({ name = "Net rx {device}"; } // bytesPerSec "rate(node_network_receive_bytes_total{${nic}}[2m])")
      ({ name = "Net tx {device}"; } // bytesPerSec "rate(node_network_transmit_bytes_total{${nic}}[2m])")
      (
        {
          name = "Net rx dropped {device}";
        }
        // perSecond "rate(node_network_receive_drop_total{${nic}}[2m])"
      )
      (
        {
          name = "Net tx dropped {device}";
        }
        // perSecond "rate(node_network_transmit_drop_total{${nic}}[2m])"
      )
      (
        {
          name = "Net errors {device}";
        }
        // perSecond "rate(node_network_receive_errs_total{${nic}}[2m]) + rate(node_network_transmit_errs_total{${nic}}[2m])"
      )
      {
        # `> 0` is a filter, not a comparison to plot: the kernel reports -1
        # for an interface with no fixed link speed, and a negative data_rate
        # is a value HA would record as a number nobody can read.
        name = "Link speed {device}";
        query = "(node_network_speed_bytes{${nic}} > 0) * 8 / 1e6";
        unit = "Mbit/s";
        device_class = "data_rate";
        digits = 0;
      }
      {
        name = "TCP established";
        query = "node_netstat_Tcp_CurrEstab{${i}}";
        digits = 0;
      }
      # Both absent where nf_conntrack is not loaded. The percentage is the one
      # that matters: a full table drops connections silently.
      {
        name = "Conntrack entries";
        query = "node_nf_conntrack_entries{${i}}";
        digits = 0;
        needs = "conntrack";
      }
      (
        {
          name = "Conntrack used";
          needs = "conntrack";
        }
        // percent "100 * node_nf_conntrack_entries{${i}} / node_nf_conntrack_entries_limit{${i}}"
      )

      # --- System ---
      {
        name = "Uptime";
        query = "time() - node_boot_time_seconds{${i}}";
        unit = "s";
        device_class = "duration";
        digits = 0;
      }
      {
        # `or vector(0)` makes "nothing failed" a reading instead of a missing
        # entity. The price is that it also reads 0 when the host is not being
        # scraped at all; "Hosts down" on the fleet device is how you tell
        # those apart. Needs the `systemd` collector, which the exporter
        # config below enables.
        name = "Failed units";
        query = ''count(node_systemd_unit_state{state="failed",${i}} == 1) or vector(0)'';
        digits = 0;
        needs = "systemd";
      }
      {
        name = "Active units";
        query = ''count(node_systemd_unit_state{state="active",${i}} == 1) or vector(0)'';
        digits = 0;
        needs = "systemd";
      }
      {
        name = "Processes running";
        query = "node_procs_running{${i}}";
        digits = 0;
      }
      {
        # Stuck in uninterruptible sleep: almost always storage or NFS, and
        # the thing load average is actually counting when nothing is on CPU.
        name = "Processes blocked";
        query = "node_procs_blocked{${i}}";
        digits = 0;
      }
      ({ name = "Forks"; } // perSecond "rate(node_forks_total{${i}}[2m])")
      (
        {
          name = "Context switches";
          digits = 0;
        }
        // perSecond "rate(node_context_switches_total{${i}}[2m])"
      )
      (
        {
          name = "Interrupts";
          digits = 0;
        }
        // perSecond "rate(node_intr_total{${i}}[2m])"
      )
      {
        name = "Open file descriptors";
        query = "node_filefd_allocated{${i}}";
        digits = 0;
      }
      {
        # Milliseconds, and no `device_class`: this is the one reading here
        # that is legitimately NEGATIVE (the clock can be ahead), and a
        # `duration` that goes below zero is not a thing worth finding out
        # whether HA accepts. The bare unit displays the same.
        name = "Clock offset";
        query = "node_timex_offset_seconds{${i}} * 1000";
        unit = "ms";
        digits = 1;
      }

      # --- Sensors ---
      {
        # `group_left(chip_name)` is what turns `platform_coretemp_0` into
        # `coretemp`. The multiplication drops `__name__`, so the entity key
        # falls back to chip/chip_name/sensor, which is what makes a hwmon
        # sensor that appears after a kernel upgrade a new entity rather than
        # a renamed one.
        name = "Temp {chip_name} {sensor}";
        query = "node_hwmon_temp_celsius{${i}} * on(chip, instance) group_left(chip_name) node_hwmon_chip_names{${i}}";
        unit = "°C";
        device_class = "temperature";
        needs = "hwmon";
      }
      {
        name = "Fan {chip_name} {sensor}";
        query = "node_hwmon_fan_rpm{${i}} * on(chip, instance) group_left(chip_name) node_hwmon_chip_names{${i}}";
        unit = "rpm";
        digits = 0;
        needs = "hwmon";
      }
      {
        # ACPI thermal zones, which overlap hwmon on a desktop but are the
        # only temperatures some platforms expose at all.
        name = "Thermal {type} {zone}";
        query = "node_thermal_zone_temp{${i}}";
        unit = "°C";
        device_class = "temperature";
        needs = "thermal";
      }
    ];

  # The fleet as one device: what the whole house is drawing and running,
  # rather than what any one box is.
  #
  # Every selector pins `job`, not `instance`. That is load bearing: this
  # host's exporter is scraped by observability.nix's `node` job as well as by
  # this one (see `jobName`), so an aggregate that did not pin the job would
  # count secondpc twice.
  fleetQueries =
    let
      j = ''job="${jobName}"'';
      nic = ''device!~"lo|veth.*|docker.*|br-.*",${j}'';

      # `max by (instance, device)` before summing, because a bind mount --
      # and on NixOS /nix is usually one -- is reported once per mountpoint
      # over the same device. Summing the mountpoints would count that storage
      # twice.
      perDevice = metric: ''sum(max by (instance, device) (${metric}{fstype!~"tmpfs|overlay",${j}}))'';
    in
    [
      {
        name = "Hosts up";
        query = "count(up{${j}} == 1) or vector(0)";
        digits = 0;
      }
      {
        name = "Hosts down";
        query = "count(up{${j}} == 0) or vector(0)";
        digits = 0;
      }
      {
        # ── WHICH HOST, WHICH THE TWO COUNTS ABOVE CANNOT SAY ──
        #
        # "Hosts up" and "Hosts down" answer HOW MANY and never WHICH, and the
        # per-host devices do not answer it either: a host that stops being
        # scraped makes every one of its ~120 entities UNAVAILABLE, and
        # unavailable is Home Assistant's word for "no state" rather than a
        # state -- nothing can be templated or automated against it.
        #
        # `up` is Prometheus's own synthetic 1/0 per target, so `sum by` is not
        # needed: the series is already one per instance. Like the `{instance}`
        # queries below it, this becomes one entity per host, created and
        # removed as the scrape config gains and loses targets, with nothing
        # written here per host.
        name = "Reachable {instance}";
        query = "up{${j}}";
        digits = 0;
      }
      {
        name = "CPU utilisation";
        query = ''100 * (1 - avg(rate(node_cpu_seconds_total{mode="idle",${j}}[2m])))'';
        unit = "%";
      }
      {
        name = "CPU cores";
        query = "count(count by (instance, cpu) (node_cpu_seconds_total{${j}}))";
        digits = 0;
      }
      {
        # Weighted by each host's RAM rather than an average of percentages,
        # which would let a Pi's 8G count as much as a server's 64G.
        name = "RAM utilisation";
        query = "100 * (1 - sum(node_memory_MemAvailable_bytes{${j}}) / sum(node_memory_MemTotal_bytes{${j}}))";
        unit = "%";
      }
      {
        name = "RAM used";
        query = "(sum(node_memory_MemTotal_bytes{${j}}) - sum(node_memory_MemAvailable_bytes{${j}})) / 1e9";
        unit = "GB";
        device_class = "data_size";
      }
      {
        name = "RAM total";
        query = "sum(node_memory_MemTotal_bytes{${j}}) / 1e9";
        unit = "GB";
        device_class = "data_size";
      }
      {
        name = "Disk free";
        query = "${perDevice "node_filesystem_avail_bytes"} / 1e9";
        unit = "GB";
        device_class = "data_size";
      }
      {
        name = "Disk size";
        query = "${perDevice "node_filesystem_size_bytes"} / 1e9";
        unit = "GB";
        device_class = "data_size";
      }
      {
        name = "Disk utilisation";
        query = "100 * (1 - ${perDevice "node_filesystem_avail_bytes"} / ${perDevice "node_filesystem_size_bytes"})";
        unit = "%";
      }
      {
        name = "CPU package power";
        query = "sum(rate(node_rapl_package_joules_total{${j}}[2m]))";
        unit = "W";
        device_class = "power";
      }
      {
        # `sum by (instance)`, so this one query becomes one entity per host --
        # created and removed as hosts join and leave the job, with no config
        # change. The `{instance}` placeholder names them; `instance` stays in
        # the entity key (it is the only label distinguishing them), which is
        # why the component drops `job` from the key but not `instance`.
        name = "Power {instance}";
        query = "sum by (instance) (rate(node_rapl_package_joules_total{${j}}[2m]))";
        unit = "W";
        device_class = "power";
      }
      {
        name = "CPU utilisation {instance}";
        query = ''100 * (1 - avg by (instance) (rate(node_cpu_seconds_total{mode="idle",${j}}[2m])))'';
        unit = "%";
      }
      {
        name = "RAM utilisation {instance}";
        query = "100 * (1 - node_memory_MemAvailable_bytes{${j}} / node_memory_MemTotal_bytes{${j}})";
        unit = "%";
      }
      {
        name = "Hottest sensor {instance}";
        query = "max by (instance) (node_hwmon_temp_celsius{${j}})";
        unit = "°C";
        device_class = "temperature";
      }
      {
        name = "Uptime {instance}";
        query = "time() - node_boot_time_seconds{${j}}";
        unit = "s";
        device_class = "duration";
        digits = 0;
      }
      {
        # The whole fleet's last reboot, which is the one number that says
        # "something restarted" without naming what.
        name = "Shortest uptime";
        query = "min(time() - node_boot_time_seconds{${j}})";
        unit = "s";
        device_class = "duration";
        digits = 0;
      }
      {
        name = "Failed units";
        query = ''count(node_systemd_unit_state{state="failed",${j}} == 1) or vector(0)'';
        digits = 0;
      }
      {
        name = "Net rx";
        query = "sum(rate(node_network_receive_bytes_total{${nic}}[2m])) / 1e6";
        unit = "MB/s";
        device_class = "data_rate";
        digits = 2;
      }
      {
        name = "Net tx";
        query = "sum(rate(node_network_transmit_bytes_total{${nic}}[2m])) / 1e6";
        unit = "MB/s";
        device_class = "data_rate";
        digits = 2;
      }
      {
        name = "Disk read";
        query = ''sum(rate(node_disk_read_bytes_total{device!~"loop.*|dm-.*",${j}}[2m])) / 1e6'';
        unit = "MB/s";
        device_class = "data_rate";
        digits = 2;
      }
      {
        name = "Disk write";
        query = ''sum(rate(node_disk_written_bytes_total{device!~"loop.*|dm-.*",${j}}[2m])) / 1e6'';
        unit = "MB/s";
        device_class = "data_rate";
        digits = 2;
      }
    ];

  # `needs` is this file's own tag, not part of the component's schema.
  renderQueries =
    omit: queries:
    map (q: builtins.removeAttrs q [ "needs" ]) (
      lib.filter (q: !(lib.elem (q.needs or null) omit)) queries
    );

  # Loopback: the component runs on the Prometheus host, so the public name
  # would leave through the cloudflare tunnel and come back in. Same escape
  # hatch ../audiobookshelf.nix uses for Music Assistant.
  url = "http://127.0.0.1:${toString prometheusPort}";

  devices = lib.mapAttrsToList (name: h: {
    # The device's name AND its unique_id (the component keys config entries on
    # it), so renaming a host in `den.hosts` makes a new HA device and orphans
    # the old one. That is the same trade `lanAddress` already carries.
    inherit name;
    inherit url;
    model = "node_exporter (${h.system})";
    # 60s. Each device is ~50 queries and several return a series per mount,
    # per disk, per NIC and per hwmon sensor, so a host lands somewhere around
    # 120 entities and the three devices together around 300. That is ~300
    # recorder rows a minute into the `hass` database; if it ever matters, the
    # levers are this interval and `recorder.exclude.entity_globs`, not a
    # shorter query list -- the long-term statistics HA derives from these are
    # hourly regardless.
    interval = 60;
    queries = renderQueries (omitMetrics.${name} or [ ]) (hostQueries name);
  }) monitored;

  fleetDevice = {
    name = "Fleet";
    inherit url;
    model = "node_exporter, job ${jobName}";
    interval = 60;
    queries = renderQueries [ ] fleetQueries;
  };
in
{
  den.aspects.prom-device = {
    includes = [ den.aspects.packages.prom-device ];

    nixos =
      { pkgs, ... }:
      {
        # Added from HERE and not from ./default.nix's `services.home-assistant`
        # block, which this file may not have to touch to add a component:
        # both options are NixOS options, so `customComponents` concatenates
        # and `config` merges by attribute. The same reasoning
        # ../audiobookshelf.nix gives for putting its Music Assistant provider
        # beside the service it belongs to.
        services.home-assistant.customComponents = [ pkgs.prom-device ];

        # YAML, not a UI flow: the component's config flow has an import step
        # only, so this list IS the set of devices, and a query added here
        # reloads the matching config entry on the next restart.
        services.home-assistant.config.prom_device = devices ++ [ fleetDevice ];

        # Fails loudly if the Prometheus host ever stops being one of the hosts
        # this file monitors -- the scrape config, the loopback target and the
        # fleet device's job all assume that it is.
        assertions = [
          {
            assertion = lib.elem prometheusHost hostNames;
            message = "prom-device: ${prometheusHost} runs Prometheus but is not in the monitored set (${lib.concatStringsSep " " hostNames}); it needs a lanAddress.";
          }
        ];
      };
  };

  # Attached to the aspect that owns Home Assistant rather than to the host, so
  # the devices follow HA if it ever moves. `includes` is list-typed and merges
  # across files -- ../../nixos/kernels/cachyos.nix and ../../input/corsair
  # extend `den.aspects.auspc` from their own files the same way.
  den.aspects.homeassistant.includes = [ den.aspects.prom-device ];

  # The exporter half, and -- on the Prometheus host only -- the scrape config
  # that gives every target its `instance` label.
  den.aspects.node-metrics.nixos =
    { host, pkgs, ... }:
    # ── THE SAME PREDICATE THE DEVICES USE, NOT A SECOND SPELLING OF IT ──
    #
    # This was `host.lanAddress != null`, which looks like `monitored` above
    # and is not: `monitored` filters `fleet`, whose `lanAddress` is DERIVED
    # from `attachments.lan-home.address` (../../hosts/fleet.nix), while the
    # HOST ENTITY's `lanAddress` is the flat field that derivation falls back
    # to -- and both auspc and secondpc declare the attachment and leave the
    # flat field unset.
    #
    # So `host.lanAddress` was null on EVERY host in this fleet and this whole
    # body was inert. MEASURED on secondpc before the change: the scrape jobs
    # were `node, ncps, fail2ban, exportarr, nextcloud, nextcloud-phpfpm` with
    # no `node-fleet`, while `services.home-assistant.config.prom_device`
    # carried its three devices and 53 queries for auspc alone. Every one of
    # those queries selects `job="node-fleet"` or `instance="<fleet name>"`, so
    # all of them returned an empty vector -- which this component renders as
    # NO ENTITY rather than as an error. A fully configured monitoring
    # integration with nothing behind it, and nothing anywhere saying so.
    #
    # `hostNames` is `attrNames monitored`, so the exporter is now enabled on
    # exactly the hosts that get a device, by construction rather than by two
    # predicates agreeing.
    lib.mkIf (lib.elem host.hostName hostNames) {
      services.prometheus.exporters.node = {
        enable = true;
        port = nodePort;

        # `mkDefault`, so observability.nix's own `enabledCollectors` wins on
        # this host rather than being concatenated: the option renders one
        # `--collector.<x>` flag per element with no deduplication, and
        # node_exporter refuses a flag it is given twice. The list there is
        # already `[ "systemd" ]`; anywhere else this is the definition that
        # applies. "Failed units" above is what needs it.
        enabledCollectors = lib.mkDefault [ "systemd" ];

        # NOT `openFirewall`. It would be the obvious way to do this, and it
        # is a trap here: the module turns `firewallFilter` into
        # `ip46tables -A nixos-fw <filter>`, which runs the filter through
        # ip6tables as well -- and an IPv4 `-s` is not a thing ip6tables
        # accepts. The firewall's start script is `#!sh -e`, so that failure
        # aborts it BEFORE `ip46tables -A INPUT -j nixos-fw`, leaving the host
        # with a half-built chain that nothing is pointed at. A firewall that
        # silently is not one.
        #
        # So the rule is written v4-only, by hand, below.
        openFirewall = false;
      };

      # Open node_exporter to the scraper and to nothing else: these metrics
      # are a detailed inventory of the machine -- every mount, every
      # interface, every unit -- and the house LAN is one flat /16 (see
      # ../../hosts/auspc.nix). Nothing on the Prometheus host, which scrapes
      # itself over loopback.
      #
      # Both backends, because which option is read depends on
      # `networking.firewall.backend` and getting it wrong fails as a scrape
      # that times out rather than as an error. `extraCommands` is ignored
      # under nftables and `extraInputRules` under iptables.
      networking.firewall = lib.optionalAttrs (host.hostName != prometheusHost) {
        extraCommands = ''
          iptables -w -A nixos-fw -p tcp -m tcp --dport ${toString nodePort} \
            -s ${fleet.${prometheusHost}.lanAddress} -j nixos-fw-accept
        '';
        extraInputRules = ''
          ip saddr ${
            fleet.${prometheusHost}.lanAddress
          } tcp dport ${toString nodePort} accept comment "node-exporter scrape"
        '';
      };

      # Hands the RAPL counters to the exporter's group. Off by default -- see
      # `raplReadable`. A unit rather than a udev rule so that it is ordered
      # against the exporter explicitly and shows up in `systemctl status`.
      # `optionalAttrs`, not `mkIf` on the value: an `attrsOf` attribute whose
      # only definition is filtered out still EXISTS, which would leave a unit
      # named after this one with nothing but defaults in it.
      systemd.services = lib.optionalAttrs raplReadable {
        node-exporter-rapl = {
          description = "Make the RAPL energy counters readable by node_exporter";
          wantedBy = [ "prometheus-node-exporter.service" ];
          before = [ "prometheus-node-exporter.service" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          path = [ pkgs.coreutils ];
          script = ''
            shopt -s nullglob
            for f in /sys/class/powercap/intel-rapl*/energy_uj; do
              chgrp node-exporter "$f" && chmod g+r "$f"
            done
          '';
        };
      };

      services.prometheus.scrapeConfigs = lib.mkIf (host.hostName == prometheusHost) [
        {
          job_name = jobName;
          # Slower than the 10s global -- see `jobName`.
          scrape_interval = "30s";
          static_configs = lib.mapAttrsToList (n: h: {
            targets = [
              "${if n == prometheusHost then "127.0.0.1" else h.lanAddress}:${toString nodePort}"
            ];
            # The whole point of this job. An explicit `instance` target label
            # replaces the `<address>:<port>` Prometheus would otherwise
            # derive, which makes the fleet name -- the one identifier this
            # file, the device names and the queries all already share -- the
            # thing that distinguishes two hosts' series.
            labels.instance = n;
          }) monitored;
        }
      ];
    };

  # Every host, rather than an include per host: the aspect is inert on a host
  # with no `lanAddress` (and on darwin, which has no `nixos` class content
  # here at all), so giving a Linux host a `lanAddress` is the only edit
  # needed to monitor it. Same hook ../../base/builders.nix uses to make every
  # host a build client.
  den.schema.host.includes = [ den.aspects.node-metrics ];
}
