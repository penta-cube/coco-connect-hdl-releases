;;; list.il - list operations for coco-connect-hdl bridge

(defun _coco_op_list_nets ()
  (let ((handle (_coco_import_handle))
        (values nil))
    (setq values (_coco_collect_values_from_find handle "find sig_name=*" (list "SIG_NAME")))
    (_coco_join_list values "|")))

(defun _coco_op_list_parts ()
  (let ((handle (_coco_import_handle))
        (values nil)
        (extra_values nil))
    (setq values (_coco_collect_values_from_find handle "find cds_location=*" (list "CDS_LOCATION")))
    (setq extra_values (_coco_collect_values_from_find handle "find $location=*" (list "$LOCATION")))
    (setq values (_coco_merge_unique values extra_values))
    (_coco_join_list values "|")))
