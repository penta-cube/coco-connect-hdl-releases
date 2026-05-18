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

(defun _coco_unique_sorted_numbers (items)
  (let ((result nil))
    (foreach item items
      (when (and item (not (member item result)))
        (setq result (append result (list item)))))
    result))

(defun _coco_page_canonical_name (dwg page_num)
  (sprintf nil "@%s.%s(%s):page%s"
    (safeStr (_coco_safe_apply 'cnGetDwgLibName (list dwg)))
    (safeStr (_coco_safe_apply 'cnGetDwgCellName (list dwg)))
    (safeStr (_coco_safe_apply 'cnGetDwgViewName (list dwg)))
    (safeStr page_num)))

(defun _coco_page_number_from_name (page_name)
  (let ((value ""))
    (when (and page_name (rexMatchp ".*:page\\([1-9][0-9]*\\)$" page_name))
      (setq value (rexSubstitute "\\1")))
    value))

(defun _coco_page_number_from_file_name (file_name)
  (let ((value ""))
    (when (and file_name
               (rexMatchp "^page\\([1-9][0-9]*\\)[.]cs[ab]$" (lowerCase file_name)))
      (setq value (rexSubstitute "\\1")))
    value))

(defun _coco_view_file_names (views)
  (let ((result nil)
        (view_info nil))
    (setq view_info (if (and (listp views) (listp (car views))) (car views) views))
    (setq result (car (errset (cnlGetViewFileNames view_info) nil)))
    result))

(defun _coco_design_page_names_from_files (handle dwg)
  (let ((lib (safeStr (_coco_safe_apply 'cnGetDwgLibName (list dwg))))
        (cell (safeStr (_coco_safe_apply 'cnGetDwgCellName (list dwg))))
        (view (safeStr (_coco_safe_apply 'cnGetDwgViewName (list dwg))))
        (vinfo nil)
        (views nil)
        (files nil)
        (pages nil)
        (page_num "")
        (page_names nil))
    (rexCompile "^page[1-9][0-9]*[.]cs[ab]$")
    (setq vinfo (car (errset (cnlGetViewInfo handle (list (list lib cell view))) nil)))
    (when vinfo
      (setq views (car (errset (cnlGetViews vinfo) nil))))
    (when views
      (setq files (_coco_view_file_names views)))
    (foreach file files
      (when (stringp file)
        (setq page_num (_coco_page_number_from_file_name file))
        (when (not (equal page_num ""))
          (setq pages (append pages (list (atoi page_num)))))))
    (foreach page_num (_coco_unique_sorted_numbers pages)
      (setq page_names (append page_names (list (_coco_page_canonical_name dwg page_num)))))
    page_names))

(defun _coco_design_page_names (handle dwg)
  (_coco_design_page_names_from_files handle dwg))

(defun _coco_restore_page_context (handle original_page page_names)
  (when (and original_page (member original_page page_names))
    (_coco_send_cmd handle (sprintf nil "nmapedit %s;" original_page))))

(defun _coco_each_design_page (handle dwg original_page page_handler)
  (let ((page_names (_coco_design_page_names handle dwg)))
    (when page_names
      (foreach page_name page_names
        (apply page_handler (list handle page_name)))
      (_coco_restore_page_context handle original_page page_names))
    page_names))
