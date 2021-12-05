(local lume (require :lume))
(fn merge_files [tablea ...]
  (var tableb tablea)
  (each [num value (ipairs [...])]
    (let [t (require value)]
      (set tableb (lume.merge tableb t))))
  tableb)

{: merge_files}
