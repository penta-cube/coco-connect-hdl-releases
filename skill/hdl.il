;;; hdl.il - Design Entry HDL access helpers

(setq _coco_run_opts (cnSetRunOptions ?result cnOUT_VERBOSE))

(defun _coco_send_cmd (handle cmd)
  (cnSendCommand handle cmd ?runOptions _coco_run_opts))

(defun _coco_import_status ()
  (let ((r (errset (cnmpsImport) t)))
    (cond
      ((not r) (list nil "cnmpsImport raised an error"))
      ((car r) (list t ""))
      (t (list nil (sprintf nil "cnmpsImport returned %L" (car r)))))))

(defun _coco_import_handle ()
  (let ((status (_coco_import_status)))
    (if (car status)
      (car (errset (cnmpsImport) t))
      (error "HDL_NOT_EXPORTED" (cadr status)))))

(defun _coco_make_iterator (group items)
  (list (safeStr group) items))

(defun _coco_iterator_group (iter)
  (if iter (safeStr (car iter)) ""))

(defun _coco_iterator_items (iter)
  (if iter (cadr iter) nil))

(defun _coco_open_iterator (handle find_cmd item_getter)
  (let ((find_result nil)
        (group "")
        (group_obj nil)
        (items nil)
        (result nil))
    (setq result (errset (_coco_send_cmd handle find_cmd) nil))
    (if (not result)
      (_coco_log_error "_coco_open_iterator" (strcat "find failed: " (safeStr find_cmd)))
      (progn
        (setq find_result (car result))
        (setq result (errset (cnGetGroupName find_result) nil))
        (if result
          (setq group (safeStr (car result)))
          (_coco_log_error "_coco_open_iterator" "cnGetGroupName failed"))
        (when (not (equal group ""))
          (setq result (errset (cnGetGroupedObjectSet find_result) nil))
          (if result
            (setq group_obj (car result))
            (_coco_log_error "_coco_open_iterator" "cnGetGroupedObjectSet failed"))
          (when group_obj
            (setq result (errset (apply item_getter (list group_obj)) nil))
            (if result
              (setq items (car result))
              (_coco_log_error "_coco_open_iterator" (strcat "item getter failed: " (safeStr item_getter))))))))
    (_coco_make_iterator group items)))

(defun _coco_cleanup_iterator (handle iter)
  (let ((group (_coco_iterator_group iter))
        (result nil))
    (when (not (equal group ""))
      (setq result (errset (_coco_send_cmd handle (sprintf nil "EXCLUDE %s %s" group group)) nil))
      (when (not result)
        (_coco_log_error "_coco_cleanup_iterator" (strcat "EXCLUDE failed: " group)))))
  t)

(defun _coco_each_iterator (iter item_handler)
  (let ((count 0)
        (errors 0)
        (result nil))
    (foreach item (_coco_iterator_items iter)
      (setq result (errset (apply item_handler (list item)) nil))
      (if result
        (setq count (add1 count))
        (progn
          (setq errors (add1 errors))
          (_coco_log_error "_coco_each_iterator" (strcat "item handler failed: " (safeStr item_handler))))))
    (list count errors)))

(defun _coco_with_find_iterator (handle find_cmd item_getter item_handler)
  (let ((iter nil)
        (result nil))
    (setq iter (_coco_open_iterator handle find_cmd item_getter))
    (setq result (errset (_coco_each_iterator iter item_handler) nil))
    (_coco_cleanup_iterator handle iter)
    (if result
      (car result)
      (progn
        (_coco_log_error "_coco_with_find_iterator" (strcat "iteration failed: " (safeStr find_cmd)))
        (list 0 1)))))

(setq _coco_collect_allowed_names nil)
(setq _coco_collect_values nil)

(defun _coco_collect_prop_value_item (prop)
  (let ((name (safeGetProp prop "name"))
        (value ""))
    (when (_coco_member_string name _coco_collect_allowed_names)
      (setq value (safeGetProp prop "value"))
      (when (and (not (equal value ""))
                 (not (_coco_member_string value _coco_collect_values)))
        (setq _coco_collect_values (cons value _coco_collect_values))))))

(defun _coco_collect_prop_values (props allowed_names)
  (let ((values nil)
        (name "")
        (value ""))
    (foreach prop props
      (setq name (safeGetProp prop "name"))
      (when (_coco_member_string name allowed_names)
        (setq value (safeGetProp prop "value"))
        (when (and (not (equal value ""))
                   (not (_coco_member_string value values)))
          (setq values (cons value values)))))
    (reverse values)))

(defun _coco_collect_values_from_find (handle find_cmd allowed_names)
  (setq _coco_collect_allowed_names allowed_names)
  (setq _coco_collect_values nil)
  (_coco_with_find_iterator handle find_cmd 'cnGetProps '_coco_collect_prop_value_item)
  (reverse _coco_collect_values))

(defun _coco_find_group (handle find_cmd allowed_names)
  (let ((iter nil)
        (values nil))
    (setq iter (_coco_open_iterator handle find_cmd 'cnGetProps))
    (setq values (_coco_collect_prop_values (_coco_iterator_items iter) allowed_names))
    (list (_coco_iterator_group iter) (length values))))
