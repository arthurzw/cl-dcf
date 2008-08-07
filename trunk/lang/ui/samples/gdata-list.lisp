(in-package :com.google.fubar.client)

(def-var user-id (string))
(def-var my-data (table)
  (gdata (feed-uri "http://hubsqa.fubar.com/feeds/" user-id)))

(def-form gdata-list
  (list (data-source my-data))
  (label (value (lambda () (length my-data)))))
