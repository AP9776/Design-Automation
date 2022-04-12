;......................................................................................................................................................

;|Function #0 : ali_main

this function connects column longitudinal rebars of first and second storeys .

local parameters :

global parameters :

Sub-functions :
1- 
2- 
3- 
4- 
5- 
6- 
7- 

returns :

 .

|;


(defun ali_main (/ ali_bars ali_column ali_ctype)
  
  (setq ali_elist (ali_findrebar))
  
  (setq ali_type_bounds (ali_find_bounds ali_elist 3))

  (setq ali_elist (ali_find_types ali_elist ali_type_bounds))

  (setq ali_ctype 1)

  (princ "bla1")
  
  (while (<= ali_ctype 3)
    
    (setq ali_column (ali_findgp ali_elist ali_ctype))
    (princ "bla2")
    
    (setq ali_bars (ali_target_bars ali_column ali_ctype "L" 1))
    (princ "bla3")
    (ali_join_bars ali_bars)

    (setq ali_bars (ali_target_bars ali_column ali_ctype "R" 1))
    (princ "bla4")
    (ali_join_bars ali_bars)
    
    (setq ali_ctypes (+ ali_ctypes 1))
    )

  (princ "finished")

)



;......................................................................................................................................................

;|Function #1 : 

Finds the rebars of the columns and creates a list of point information for every rebar .
local parameters :
non
Sub-functions :
1- ali_pextract
2- al_assoc_match
3- ali_gothru
4-
5-
6-
7-

|;


(defun ali_findrebar (/ ali_elist_flag ali_gpnum)

  (ali_conv_lwpl "-8" 6)

  ;gpnum is the group number , assigned to every polyline ( every rebar ) . every rebar is a group .
  (setq ali_gpnum 1)
  (setq ali_elist_flag t)

  (setq ali_enc (entnext))
  (while ali_enc

    (setq ali_en (entget ali_enc))

    ;filter the entities .
    (if (and (ali_assoc_match ali_en 8 "-8")
	     (ali_assoc_match ali_en 62 6)
	     )
      
      ;go through polyline vertexes and collect data .
      (ali_gothru ali_gpnum)

      );end if
      
    (setq ali_enc (entnext ali_enc))

    );end while

  (setq ali_elist (reverse ali_elist))

  (princ "\n ali_findrebar successfully executed .")

  ali_elist

  )




;|Function #2 : ali_gothru
A function that goes through polyline vertex entity names and collects their info .
local parameters :
1- ali_flag
Sub-functions :
1- ali_pextract

|;



(defun ali_gothru (ali_lgpnum / ali_flag)

  (setq ali_plist (list (list ali_lgpnum "ctype" "rorl" "storey")))
  (ali_pextract ali_en (list ali_lgpnum 0 -1))
  (setq ali_plist (cons ali_pdata ali_plist))
  
  (setq ali_flag 1)

  ;collect point data .
  (while (= ali_flag 1)

    (setq ali_enc (entnext ali_enc))
    (setq ali_en (entget ali_enc))
    
    (setq ali_pdata (ali_pextract ali_en (list ali_lgpnum 10 0 -1))
	  ali_plist (cons ali_pdata ali_plist)
	  )

    (if (= (cdr (assoc 0 ali_en)) "SEQEND")
      (setq ali_pdata (ali_pextract ali_en (list ali_lgpnum 0 -1))
	    ali_plist (cons ali_pdata (cdr ali_plist))
	    ali_flag 0)
      )
    
    );loop end

  (setq ali_plist (reverse ali_plist))

  (if ali_elist_flag
    (progn
      (setq ali_elist (list ali_plist))
      (setq ali_elist_flag nil)
      (setq ali_gpnum (+ 1 ali_lgpnum))
      )
    (progn
      (setq ali_elist (cons ali_plist ali_elist))
      (setq ali_gpnum (+ 1 ali_lgpnum))
      )
    )

  )



;| Function #3 : ali_pextract
extracts needed data ( entity name, type, handle, point coordinate ) from a given entity data list . then creates a list with them .

local parameters :
1- ali_len : entity name
2- ali_lgpnum : group number (number of the rebar)
3- ali_pextract_check : checks if this function has worked .

Sub-functions :
none

|;

(defun ali_pextract (ali_len ali_lheader / ali_lpcodes)

  (setq ali_lpcodes (cdr ali_lheader)
	ali_pdata (list 0)
	)
  
  (foreach ali_counter ali_lpcodes
    (setq ali_pdata (cons (cdr (assoc ali_counter ali_len)) ali_pdata))
    )
  (setq ali_pdata (cdr (reverse ali_pdata))
	ali_pdata (cons (car ali_lheader) (reverse ali_pdata))
	)
  

  ali_pdata
  
  )


;......................................................................................................................................................

;| Function #4 : ali_assoc_match
recieves the entity name , dxf code , the match respectively . checks if the associate off dxf code is equal to the match .

local parameters :
1- ali_entity : entity name
2- ali_code : the dxf code
3- ali_match : the associate of dxf code that you want it to be
4- ali_result : the result of the test (T or nil)

Sub-functions :
none

|;

(defun ali_assoc_match (ali_entity ali_code ali_match / ali_result)
  (setq ali_result nil)
  (if (= (cdr (assoc ali_code ali_entity)) ali_match)
    (setq ali_result t)
    )
  ali_result
  )



;| Function #5 : ali_rebar_print
prints an orderly presentation of the rebar info list .

local parameters :
1- ali_printlis : rebar list
2- ali_innerlist : a point list in rebar list

Sub-functions :
none

|;

(defun ali_rebar_print (ali_printlist / ali_innerlist)

  (foreach ali_counter1 ali_printlist
    (setq ali_innerlist ali_counter1)
    (foreach ali_counter2 ali_innerlist
      (princ ali_counter2)
      (princ "\n")
      )
    (princ "\n")
    (princ "\n")
    )
  (princ)
  )



;| Function #6 : ali_conv_lwpl
converts lwpolylines in the (ali_llayer) with (ali_lcolor) color , to normal polylines .

local parameters :
1- ali_llayer : the layer which lwpolylines are in .
2- ali_lcolor: the color which lwpolylines are in .

Sub-functions :
none

|;

(defun ali_conv_lwpl (ali_llayer ali_lcolor)

  (command "convertpoly" "h" (ssget "X" (list (cons 8 ali_llayer) (cons 62 ali_lcolor))) "")

  )


;......................................................................................................................................................

;|Function # : 

 .

local parameters :

global parameters :

Sub-functions :
1- 
2- 
3- 
4- 
5- 
6- 
7- 

returns :

 .

|;


(defun ali_find_bounds (ali_lelist ali_lctypes / ali_totalbound ali_lends ali_type_bounds ali_flag ali_lleftend ali_lrightend ali_bound_length ali_lnewend)

  (setq ali_lends (ali_find_xends ali_lelist)
	ali_lleftend (car ali_lends)
	ali_lrightend (cadr ali_lends)
	ali_totalbound (- ali_lrightend ali_lleftend)
	)

  
  (setq ali_bound_length (/ ali_totalbound ali_lctypes))
	

  ;create boundaries for column types

  
  (setq ali_flag ali_lctypes
	ali_lnewend (- ali_lrightend ali_bound_length)
	ali_type_bounds (list (list ali_lnewend (+ ali_lrightend (/ ali_bound_length 10))))
	)
  
  (while (/= ali_flag 2)

    (setq ali_type_bounds (cons (list (- ali_lnewend ali_bound_length) ali_lnewend) ali_type_bounds)
	  ali_flag (- ali_flag 1)
	  ali_lnewend (- ali_lnewend ali_bound_length)
	  )
    )


  (setq ali_type_bounds (cons (list (- ali_lleftend (/ ali_bound_length 10)) (+ ali_lleftend ali_bound_length)) ali_type_bounds))

  (princ "\n ali_find_bounds successfully executed .")

  ali_type_bounds

  )


;......................................................................................................................................................

;|Function # : 

 .

local parameters :

global parameters :

Sub-functions :
1- 
2- 
3- 
4- 
5- 
6- 
7- 

returns :

 .

|;


;finds extreme ends of the rebars in x direction in the plan .
(defun ali_find_xends (ali_lelist / ali_flag ali_lrightend ali_lleftend ali_lxends)


  (setq ali_flag t)
  
  (foreach ali_counter1 ali_lelist

    (foreach ali_counter2 ali_counter1
      
      (if (= (nth 2 ali_counter2) "VERTEX")

	(if ali_flag

	  (progn
	    (setq ali_lrightend (car (nth 3 ali_counter2))
		  ali_lleftend (car (nth 3 ali_counter2))
		  ali_flag nil
		  )
	    )
	  
	  (progn
	    (if (< (car (nth 3 ali_counter2)) ali_lleftend)
	      (setq ali_lleftend (car (nth 3 ali_counter2)))
	      )
	    (if (> (car (nth 3 ali_counter2)) ali_lrightend)
	      (setq ali_lrightend (car (nth 3 ali_counter2)))
	      )
	    )
	  
	  )
	

	)
      )
    )
		  
  
  (setq ali_lxends (list ali_lleftend ali_lrightend))

  ali_lxends
  
  )


;......................................................................................................................................................

;|Function # : 

 .

local parameters :

global parameters :

Sub-functions :
1- 
2- 
3- 
4- 
5- 
6- 
7- 

returns :

 .

|;



(defun ali_find_types (ali_lelist ali_ltype_bounds / ali_flag ali_lnewlist ali_lxpos ali_lctype ali_lpspecs ali_lnew_pl)

  (setq ali_lnewlist (list 0))
  
  (foreach ali_counter ali_lelist

    (setq ali_flag t
	  ali_lxpos (car (nth 3 (nth 2 ali_counter)))
	  ali_lctype 0
	  )
    

    (while ali_flag

      (if (and (<= ali_lxpos (cadr (nth ali_lctype ali_ltype_bounds)))
	       (>= ali_lxpos (car (nth ali_lctype ali_ltype_bounds)))
	       )
	
	(progn

	  (setq ali_lpspecs (car ali_counter)
	        ali_lpspecs (subst (+ 1 ali_lctype) "ctype" ali_lpspecs)
	        ali_lnew_pl (cons ali_lpspecs (cdr ali_counter))
	        ali_flag nil
	        )
	  )
	)

      (setq ali_lctype (+ ali_lctype 1))
      
      )

    (setq ali_lnewlist (cons ali_lnew_pl ali_lnewlist))
    
    
    )

  
  (setq ali_lnewlist (cdr (reverse ali_lnewlist)))

  (princ "\n ali_find_types successfully executed .")

  ali_lnewlist
  
  )




;......................................................................................................................................................

;|Function # : 

 .

local parameters :

global parameters :

Sub-functions :
1- 
2- 
3- 
4- 
5- 
6- 
7- 

returns :

 .

|;


(defun ali_findgp (ali_lelist ali_lctype / ali_lcolumn)

  (setq ali_lcolumn (list 0))

  (foreach ali_counter ali_lelist

    (if (= (nth 1 (car ali_counter)) ali_lctype)
      (setq ali_lcolumn (cons ali_counter ali_lcolumn)))
    )


  (setq ali_lcolumn (cdr (reverse ali_lcolumn)))

  (setq ali_lcolumn (ali_fill_header ali_lcolumn))

  (princ "\n ali_findgp successfully executed")


  ali_lcolumn

  )


;......................................................................................................................................................

;|Function #0 : ali_base_program

Finds the rebars of the columns and creates a list of point information for every rebar .

local parameters :
non

global parameters :
non

Sub-functions :
1- ali_pextract
2- al_assoc_match
3- ali_gothru
4-
5-
6-
7-

returns :


|;

(defun ali_fill_header (ali_lcolumn / ali_flag ali_leftbars ali_rightbars ali_col_midline ali_down_vertex ali_lpspecs ali_lnew_mem ali_lnewlist)

  (setq ali_leftbars (list 0)
	ali_rightbars (list 0)
	ali_col_midline (ali_find_midline ali_lcolumn)
	ali_flag 1
	)


  (foreach ali_counter ali_lcolumn

    (setq ali_down_vertex (cadr (ali_find_plends ali_counter)))

      (if (< (car (nth 3 ali_down_vertex)) ali_col_midline)

	(setq ali_lpspecs (car ali_counter)
	      ali_lpspecs (subst "L" "rorl" ali_lpspecs)
	      ali_lnew_mem (cons ali_lpspecs (cdr ali_counter))
	      ali_leftbars (cons ali_lnew_mem ali_leftbars)
	      )
	)
      
      (if (> (car (nth 3 ali_down_vertex)) ali_col_midline)

	(setq ali_lpspecs (car ali_counter)
	      ali_lpspecs (subst "R" "rorl" ali_lpspecs)
	      ali_lnew_mem (cons ali_lpspecs (cdr ali_counter))
	      ali_rightbars (cons ali_lnew_mem ali_rightbars)
	      )
	)


    )

  (princ "\n shaba2")

  (setq ali_leftbars (cdr (reverse ali_leftbars))
	ali_rightbars (cdr (reverse ali_rightbars))
	ali_leftbars (ali_find_storey ali_leftbars)
	ali_rightbars (ali_find_storey ali_rightbars)
	)

  (setq ali_lnewlist (append ali_leftbars ali_rightbars))

  ali_lnewlist

  )


;......................................................................................................................................................

;|Function # : 

 .

local parameters :

global parameters :

Sub-functions :
1- 
2- 
3- 
4- 
5- 
6- 
7- 

returns :

 .

|;


(defun ali_find_midline (ali_lcolumn)

  (setq ali_lxends (ali_find_xends ali_lcolumn))

  (setq ali_lmidline (/ (+ (car ali_lxends) (cadr ali_lxends)) 2))

  ali_lmidline

  )


;......................................................................................................................................................

;|Function # : 

 .

local parameters :

global parameters :

Sub-functions :
1- 
2- 
3- 
4- 
5- 
6- 
7- 

returns :

 .

|;


(defun ali_find_side (ali_lcolumn / ali_flag ali_lnewlist ali_col_midline ali_down_vertex ali_lnew_mem ali_lpspecs)

  (setq ali_lnewlist (list 0)
	ali_col_midline (ali_find_midline ali_lcolumn)
	ali_flag 1
	)

  (foreach ali_counter ali_lcolumn

      (setq ali_down_vertex (cadr (ali_find_plends ali_counter)))

      (if (< (car (nth 3 ali_down_vertex)) ali_col_midline)

	(setq ali_lpspecs (car ali_counter)
	      ali_lpspecs (subst "L" "rorl" ali_lpspecs)
	      ali_lnew_mem (cons ali_lpspecs (cdr ali_counter))
	      )
	)
      
      (if (> (car (nth 3 ali_down_vertex)) ali_col_midline)

	(setq ali_lpspecs (car ali_counter)
	      ali_lpspecs (subst "R" "rorl" ali_lpspecs)
	      ali_lnew_mem (cons ali_lpspecs (cdr ali_counter))
	      )
	)


    (setq ali_lnewlist (cons ali_lnew_mem ali_lnewlist))

    )

  (setq ali_lnewlist (cdr (reverse ali_lnewlist)))

  ali_lnewlist

  )



;......................................................................................................................................................

;|Function # : 

 .

local parameters :

global parameters :

Sub-functions :
1- 
2- 
3- 
4- 
5- 
6- 
7- 

returns :

 .

|;


(defun ali_find_storey (ali_lbarlist / ali_flag ali_flag1 ali_lnew_mem ali_lpspecs ali_lbarpos ali_lbarnum ali_lstorey ali_lcnum ali_down_vertex ali_lbottom ali_lnewlist)

  (setq ali_lbarnum (length ali_lbarlist)
	ali_lstorey 0
	ali_lnewlist (list 0)
	)

  (while (/= ali_lbarnum 0)

    (setq ali_flag1 t
	  ali_lcnum 1
	  )
    
    (foreach ali_counter ali_lbarlist

      (setq ali_down_vertex (cdr (ali_find_plends ali_counter)))

      (if (or ali_flag1
	      (< (cadr (nth 3 ali_down_vertex)) ali_lbottom)
	      )

	(setq ali_lowbar ali_counter
	      ali_lbottom (cadr (nth 3 ali_down_vertex))
	      ali_lbarpos ali_lcnum
	      ali_lcnum (+ ali_lcnum 1)
	      )
	)
      
      )

    (setq ali_lpspecs (car lowbar)
	  ali_lpspecs (subst ali_lstorey "storey" ali_lpspecs)
	  ali_lnew_mem (cons ali_lpspecs (cdr ali_counter))
	  ali_lnewlist (cons ali_lnew_mem ali_lnewlist)
	  )

    (setq ali_lbarlist (ali_del_nth ali_lbarlist ali_lbarpos))

    (setq ali_lstorey (+ ali_lstorey 1))

    (setq ali_lbarnum (- ali_lbarnum 1))

    
    )


  (setq ali_lnewlist (cdr (reverse ali_lnewlist)))

  ali_lnewlist
  )



;......................................................................................................................................................

;|Function # : 

 .

local parameters :

global parameters :

Sub-functions :
1- 
2- 
3- 
4- 
5- 
6- 
7- 

returns :

 .

|;


(defun ali_target_bars (ali_lelist ali_lcolumn ali_lside ali_lstorey / ali_result ali_lpspecs ali_firstbar ali_secondbar)

  (foreach ali_counter ali_lelist

    (setq ali_lpspecs (car ali_counter))

    (if (and (= (nth 1 ali_lpspecs) ali_lcolumn)
	     (= (nth 2 ali_lpspecs) ali_lside)
	     (= (nth 3 ali_lpspecs) ali_lstorey)
	     )
      (setq ali_firstbar ali_counter)
      )
    
    (if (and (= (nth 1 ali_lpspecs) ali_lcolumn)
	     (= (nth 2 ali_lpspecs) ali_lside)
	     (= (nth 3 ali_lpspecs) (+ 1 ali_lstorey))
	     )
      (setq ali_secondbar ali_counter)
      )

    )

  (setq ali_result (list ali_firstbar ali_secondbar))

  ali_result

  
  )




;......................................................................................................................................................

;|Function # : 

 .

local parameters :

global parameters :

Sub-functions :
1- 
2- 
3- 
4- 
5- 
6- 
7- 

returns :

 .

|;


;this function collects the highest and lowest vertex data list of every rebar polyline .

(defun ali_find_plends (ali_lplist / ali_lupp ali_ldown ali_pline_ends)

  (setq ali_lupp (nth 2 ali_lplist)
	ali_ldown (nth 2 ali_lplist)
	)

  (foreach ali_counter ali_lplist
    (if (= (nth 2 ali_counter) "VERTEX")
      
      (progn
	
       (if (<= (cadr (last ali_counter)) (cadr (nth 3 ali_ldown)))
	 (setq ali_ldown ali_counter)
	 )
       (if (> (cadr (last ali_counter)) (cadr (nth 3 ali_lupp)))
	 (setq ali_lupp ali_counter)
	 )
       
       )

      )
    )

  (setq ali_pline_ends (list ali_lupp ali_ldown))

  ali_pline_ends

  )


;......................................................................................................................................................

;|Function # : 

 .

local parameters :

global parameters :

Sub-functions :
1- 
2- 
3- 
4- 
5- 
6- 
7- 

returns :

 .

|;


(defun ali_join_bars (ali_lbars / ali_lower ali_higher ali_first_x ali_first_y ali_second_x ali_second_y)

  (setq ali_lower (cdr ali_lbars)
	ali_higher (car ali_lbars)
	)

  (setq ali_first_x (car (nth 3 (cdr (ali_find_ends ali_lower))))
	ali_first_y (cadr (nth 3 (cdr (ali_find_ends ali_lower))))
	ali_second_x (car (nth 3 (cdr (ali_find_ends ali_higher))))
	ali_second_y (cadr (nth 3 (cdr (ali_find_ends ali_higher))))
	)

  (entmake (list (cons 0 "POLYLINE") (cons 8 "-8") (cons 62 6)))
  (entmake (list (cons 0 "VERTEX") (cons 10 '(ali_first_x ali_first_y))))
  (entmake (list (cons 0 "VERTEX") (cons 10 '(ali_second_x ali_second_y))))
  (entmake (list (cons 0 "SEQEND")))

  (entdel (cadr (nth 1 ali_lower)))

  )


;......................................................................................................................................................

;|Function # : 

 .

local parameters :

global parameters :

Sub-functions :
1- 
2- 
3- 
4- 
5- 
6- 
7- 

returns :

 .

|;


(defun ali_del_nth (ali_loldlist ali_mem_pos / ali_counter ali_lnewlist ali_lcnum)

  (setq ali_lcnum 1)
  (setq ali_lnewlist (list 0))
  (foreach ali_counter ali_loldlist
    (if (/= ali_lcnum ali_mem_pos)
      (setq ali_lnewlist (append ali_lnewlist (list ali_counter)))
      )
    (setq ali_lcnum (+ ali_lcnum 1))
    )
  (setq ali_lnewlist (cdr ali_lnewlist))
  ali_lnewlist
  )



;......................................................................................................................................................

;|Function # : 

 .

local parameters :

global parameters :

Sub-functions :
1- 
2- 
3- 
4- 
5- 
6- 
7- 

returns :

 .

|;


(defun ali_findpos (ali_list ali_lmem / ali_lcnum ali_lpos ali_flag)
  (setq ali_lcnum 1
	ali_flag t
	ali_lpos nil)
  (foreach ali_member ali_list
    (if (and ali_flag
	     (= ali_member ali_lmem)
	     )
      (setq ali_lpos ali_lcnum
	    ali_flag nil)
      )
    (setq ali_lcnum (+ ali_lcnum 1))
    )
  ali_lpos
  )



;......................................................................................................................................................

;|Function # : 

 .

local parameters :

global parameters :

Sub-functions :
1- 
2- 
3- 
4- 
5- 
6- 
7- 

returns :

 .

|;


;goes through a list and finds the smallest number in the list
(defun ali_findlowest (ali_list / ali_member ali_lowest)
  (setq ali_lowest (car ali_list))
  (foreach ali_member ali_list
    (if (< ali_member ali_lowest)
      (setq ali_lowest ali_member)
      )
    )
  ali_lowest
  )



;......................................................................................................................................................

;|Function # : 

 .

local parameters :

global parameters :

Sub-functions :
1- 
2- 
3- 
4- 
5- 
6- 
7- 

returns :

 .

|;


(defun ali_sort_desc (ali_orglist / ali_newlist ali_lsmall ali_flag)

  (setq ali_flag t)
  (while ali_orglist
    
    (setq ali_lsmall (ali_findlowest ali_orglist))
    
    (if ali_flag
      (setq ali_newlist (list ali_lsmall)
	    ali_flag nil)
      (setq ali_newlist (cons ali_lsmall ali_newlist))
      )
    
    (setq ali_lsmall_pos (ali_findpos ali_orglist ali_lsmall)
	  ali_orglist (ali_del_nth ali_orglist ali_lsmall_pos)
	  )
    )
  
  ali_newlist
  
  )



;recieves the entity name , dxf code , the match respectively . checks if the associate off dxf code is equal to the match .
;symbols :
;ali_edatalist : entity datalist
;ali_code : the dxf code
;ali_match : the associate of dxf code that you want it to be
;ali_result : the result of the test (T or nil)
(defun ali_assoc_match (ali_edatalist ali_code ali_match / ali_result)
  (setq ali_result nil)
  (if (= (cdr (assoc ali_code ali_edatalist)) ali_match)
    (setq ali_result t)
    )
  ali_result
  )