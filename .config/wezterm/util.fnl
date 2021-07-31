(fn merge [tablea ...]
  (each [num te (ipairs [...])]
    (print te)
   (each [key value (pairs te)]
     (tset tablea key value))))

(fn merge_files [tablea ...]
  (each [num value (ipairs [...])]
        (merge tablea (require value))))

{: merge : merge_files}
