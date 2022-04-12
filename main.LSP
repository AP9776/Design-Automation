;......................................................................................................................................................
;|Function #0 : ali_main

this function connects the longitudinal rebars of columns in first and second storeys .

local parameters :
1- ali_bars : list of rebars to be connected .
2- ali_column : list of entity data lists of rebars in a column type .
3- ali_ctype : column type number .

global parameters :
1- ali_elist : full entity data list of rebars in the plan .
2- ali_en : used to get entity data list of every entity .
3- ali_enc : used as pointer to entities .
4- ali_gpnum : used to assign a number for every rebar in plan .

Sub-functions :
1- ali_findrebar
2- ali_find_bounds
3- ali_find_types
4- ali_findgp
5- ali_target_bars
6- ali_join_bars
7- 


|;


(defun ali_main (/ ali_bars ali_column ali_ctype)
  
  (setq ali_elist (ali_findrebar))
  
  (setq ali_type_bounds (ali_find_bounds ali_elist 3))

  (setq ali_elist (ali_find_types ali_elist ali_type_bounds))

  (setq ali_ctype 1)
  
  (while (<= ali_ctype 3)
    
    (setq ali_column (ali_findgp ali_elist ali_ctype))

    (setq ali_bars (ali_target_bars ali_column ali_ctype "L" 1))
    (ali_join_bars ali_bars)

    (setq ali_bars (ali_target_bars ali_column ali_ctype "R" 1))
    (ali_join_bars ali_bars)
    
    (setq ali_ctype (+ ali_ctype 1))
    
    )


  (princ "ali_main succesfully executed")
  (princ)

)



;......................................................................................................................................................
;|Function #1 : ali_findrebar

Finds the rebars of the columns and creates a list of point information for every rebar .

local parameters :
1- ali_elist_flag
2- ali_gpnum

Sub-functions :
1- ali_pextract
2- al_assoc_match
3- ali_gothru

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



;......................................................................................................................................................
;|Function #2 : ali_gothru

A function that goes through polyline vertex entity names and collects their info .

local parameters :
1- ali_lgpnum : the number of the polyline .
2- ali_flag : checks if the entity pointer has reached the end of the polyline (SEQEND)

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


;......................................................................................................................................................
;| Function #3 : ali_pextract

extracts needed data ( entity name, type, handle, point coordinate ) from a given entity data list . then creates a list with them .

local parameters :
1- ali_len : entity name
2- ali_lheader : a list that contains polyline number, and dxf codes that we want to extract the data attached to them .
3- ali_lpcodes : dxf codes that we want to extract the data attached to them .

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


;......................................................................................................................................................
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
    (princ "\n #")
    (princ "\n")
    )
  (princ)
  )


;......................................................................................................................................................
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

;|Function #7 : ali_find_bounds

finds the boundaries of rebars in the whole plan and also every column type , in x direction .

local parameters :
1- ali_lelist : entity data list
2- ali_lctypes : column type number
3- ali_totalbound : the boundaries of the rebars in the whole plan .
4- ali_lends : list containing boundary coordinates of rebars in plan .
5- ali_type_bounds : list containing boundaries for every column type .
6- ali_flag : checks if the loop is executed a specific number of times .
7- ali_lleftend : left limit of rebars in whole plan .
8- ali_lrightend : right limit of rebars in whole plan .
9- ali_bound_length : length of the boundaries of every column type .
10- ali_lnewend : temporary boundary , set in every round of loop .

Sub-functions :
ali_find_xends

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

;|Function #8 : ali_find_xends

finds the boundaries of rebar polylines in the plan in x direction .

local parameters :
ali_lelist : full entity data list of rebars in the plan .
ali_flag : checks if the boundaries are set for the first time in the plan .
ali_lrightend : the x boundary of rebars right direction .
ali_lleftend : the x boundary of rebars left direction .
ali_lxends : a list containing left (1st element) and right (2nd element) boundaries of rebars in plan .

global parameters :
none

Sub-functions :
none


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

;|Function #9 : ali_find_types

sets the column types for every rebar in it's entity data list .

local parameters :
1- ali_lelist : full entity data list of all rebars in plan .
2- ali_ltype_bounds : list of boundaries of rebars of column types in x direction .
3- ali_flag
4- ali_lnewlist : updated entity data list of rebars . ( column type number added )
5- ali_lxpos : x coordinate of first vertex of every rebar in loop .
6- ali_lctype : type number of a column .
7- ali_lpspecs : specifications list of a rebar's entity data list .
8- ali_lnew_pl : updated rebar entity data list .

global parameters :
none

Sub-functions :
none

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

;|Function #10 : ali_findgp

sorts entity data list by column type and then assigns the side and storey of every rebar in it's column . returns updated lists .

local parameters :
ali_lelist : full entity data list of all rebars in plan .
ali_lctype : type number of the columns .
ali_lcolumn : entity data list of rebars in a column .

global parameters :

Sub-functions :
1- ali_fill_header
2- ali_find_side

|;


(defun ali_findgp (ali_lelist ali_lctype / ali_lcolumn)

  (setq ali_lcolumn (list 0))

  (foreach ali_counter ali_lelist

    (if (= (nth 1 (car ali_counter)) ali_lctype)
      (setq ali_lcolumn (cons ali_counter ali_lcolumn)))
    )

  
  (setq ali_lcolumn (cdr (reverse ali_lcolumn)))

  (setq ali_lcolumn (ali_find_side ali_lcolumn))

  (setq ali_lcolumn (ali_fill_header ali_lcolumn))

  (princ "\n ali_findgp successfully executed")


  ali_lcolumn

  )


;......................................................................................................................................................
;|Function #11 : ali_fill_header

Finds the rebars of the columns and creates a list of point information for every rebar .

local parameters :
1- ali_lcolumn : entity data list of rebars in a column
2- ali_leftbars : list containing entity data list of rebars in left side of column .
3- ali_rightbars : list containing entity data list of rebars in right side of column .
5- ali_col_midline : x coordinate of midline of the column .
6- ali_down_vertex : lowest vertex of a rebar in loop .
7- ali_lpspecs : specifications list of a rebar's entity data list .
8- ali_lnew_mem : updated rebar entity data list to be added to rebar list of right or left side of column .
9- ali_lnewlist : updated rebars entity data list of a column .

global parameters :
non

Sub-functions :
1- ali_find_storey
2- ali_find_plends
3- ali_find_midline

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


  (setq ali_leftbars (cdr (reverse ali_leftbars))
	ali_rightbars (cdr (reverse ali_rightbars))
	)

  
  (setq ali_leftbars (ali_find_storey ali_leftbars)
	ali_rightbars (ali_find_storey ali_rightbars)
	)


  (setq ali_lnewlist (append ali_leftbars ali_rightbars))

  ali_lnewlist

  )


;......................................................................................................................................................

;|Function #12 : ali_find_midline

returns the x coordinate of midline of a column .

local parameters :
1- ali_lcolumn : the entity data list of rebars in a column .
2- ali_lxends : a list contsining biggest (1st) and smallest (2nd) x coordinate of vertexes of rebars in a column .
3- ali_lmidline : x coordinate of midline of a column .

global parameters :
none

Sub-functions :
1- ali_find_xends

|;


(defun ali_find_midline (ali_lcolumn / ali_lxends ali_lmidline)

  (setq ali_lxends (ali_find_xends ali_lcolumn))

  (setq ali_lmidline (/ (+ (car ali_lxends) (cadr ali_lxends)) 2))

  ali_lmidline

  )


;......................................................................................................................................................

;|Function #13 : ali_find_side

sets the side in the entity data list of rebars of a column .

local parameters :
1- ali_lcolumn : the entity data list of rebars of a column .
2- ali_lnewlist : the updated entity data list of rebars of a column .
3- ali_col_midline : the x coordinate of midline of the column .
4- ali_down_vertex :  data list of lowest vertex of rebars in column .
5- ali_lnew_mem : updated rebar data list .
6- ali_lpspecs : specifications list in entity data list of a rebar .

global parameters :
none

Sub-functions :
1- ali_find_plends
2- ali_find_midline

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

;|Function #14 : ali_find_storey

sets the storey numbers of the entity data list , in every polyline's specification list (car of every polyline list) . (only in one side of one column)

local parameters :
1- ali_lbarlist : the entity data lists of rebars in one side of a column .
2- ali_posnum_check : checks if the ali_posnum list is created for the first time .
3- ali_flag : checks 
4- ali_lcheck : checks if the current pointer in the loop has not been kicked out of process before .
5- ali_posnum : a list of numbers (positions of the lowest rebar in rebar list) , used to kick the previous lowest rebar out of the loop process .
6- ali_lowest : lowest rebar in every round of the loop process .
7- ali_lstorey_num : current storey number , aimed at the lowest rebar in every round of loop .
8- ali_lbarnum : number of rebars in ali_lbarlist .
9- ali_lnew_mem : the updated rebar entity data list . (with storey number)
10- ali_lspecs : the specifications list of the polyline (rebar) .
11- ali_bottom1 : y coordinate of lowest vertex of the pointed rebar in every round of loop .
12- ali_bottom2 : y coordinate of lowest vertex of the lowest rebar in every round of loop .
13- ali_lcnum : used as counter for the loop . specifies the position of lowest rebar in every loop .
14- ali_lnewlist : the updated entity data list of rebars in a side of the column .

global parameters :
none

Sub-functions :
none


|;


(defun ali_find_storey (ali_lbarlist / ali_posnum_check ali_flag ali_lcheck ali_posnum ali_lowest ali_lstorey_num ali_lbarnum ali_lnew_mem ali_lspecs ali_bottom1 ali_bottom2 ali_lcnum ali_lnewlist)


  (setq ali_lbarnum (length ali_lbarlist)
	ali_lnewlist (list 0)
	ali_posnum (list 0)
	ali_posnum_check t
	ali_lstorey_num 0)

  (while (< (length ali_posnum) ali_lbarnum)

    (setq ali_lcnum 1
	  ali_flag t)
    
    (foreach ali_counter1 ali_lbarlist

      (setq ali_bottom1 (cadr (nth 3 (cadr (ali_find_plends ali_counter1))))
	    ali_lcheck t)

      (foreach ali_counter2 ali_posnum

	(if (= ali_lcnum ali_counter2)
	  (setq ali_lcheck nil)
	  )
	)

      (if (and ali_lcheck
	       ali_flag)
	(setq ali_lowest ali_counter1
	      ali_lownum ali_lcnum
	      ali_flag nil)
	)

      (if ali_lcheck

	(progn
	  (if (< ali_bottom1 ali_bottom2)
	    (setq ali_lowest ali_counter1
		  ali_lownum ali_lcnum)
	    )
	  )
	)

      (setq ali_bottom2 (cadr (nth 3 (cadr (ali_find_plends ali_lowest))))
	    ali_lcnum (+ ali_lcnum 1)
	    )
      
      )
    

    (setq ali_lspecs (car ali_lowest)
	  ali_lspecs (subst ali_lstorey_num "storey" ali_lspecs)
	  ali_lnew_mem (cons ali_lspecs (cdr ali_lowest))
	  )
    (setq ali_lnewlist (cons ali_lnew_mem ali_lnewlist)
	  ali_lstorey_num (+ ali_lstorey_num 1)
	  )

    (Setq ali_posnum (cons ali_lownum ali_posnum))
    (if ali_posnum_check
      (setq ali_posnum (cdr (reverse ali_posnum))
	    ali_posnum_check nil)
      )

    )

  (setq ali_lnewlist (cdr (reverse ali_lnewlist)))

  ali_lnewlist

  )


;......................................................................................................................................................
;|Function #15 : ali_target_bars

returns the entity data list of two tandem polylines (rebars) we want to connect . (only in one column)

local parameters :
1- ali_lcolumn : the entity data list of polylines (rebars) in a specific column .
2- ali_lctype : type number of the column .
3- ali_lside : specifies on which side of the column we want to connect two rebars .
4- ali_lstorey : specifies the storey number of the lower rebar .
5- ali_result : a list containing the entity data list of downstairs (1st element) and uppstairs (2nd element) rebars we want to connect .
6- ali_lpspecs : polyline specifications list .
7- ali_firstbar : the lower rebar entity data list .
8- ali_secondbar : the uppstairs rebar entity data list .

global parameters :
none

Sub-functions :
none


|;


(defun ali_target_bars (ali_lcolumn ali_lctype ali_lside ali_lstorey / ali_result ali_lpspecs ali_firstbar ali_secondbar)

  (foreach ali_counter ali_lcolumn

    (setq ali_lpspecs (car ali_counter))

    (if (and (= (nth 1 ali_lpspecs) ali_lctype)
	     (= (nth 2 ali_lpspecs) ali_lside)
	     (= (nth 3 ali_lpspecs) ali_lstorey)
	     )
      (setq ali_firstbar ali_counter)
      )
    
    (if (and (= (nth 1 ali_lpspecs) ali_lctype)
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

;|Function #16 : ali_find_plends

returns a list containing the entity data of the lowest (1st element) and highest (2nd element) vertexes of a polyline .
if the polyline is L shape , the vertex which is far from the rebar axis , is not included in process .

local parameters :
1- ali_lplist : the entity data list of a polyline .
2- ali_lydown : y coordinate of the (temporary) lowest vertex .
3- ali_lyupp : y coordinate of the (temporary) highest vertex .
4- ali_ldirect : direction of the polyline (1 if uppwards and -1 if downwars )
5- ali_lxlast : x coordinate of the last vertex .
6- ali_lx_bl : x coordinate of the vertex before the last vertex .
7- ali_lxsecond : x coordinate of the second vertex .
8- ali_lxfirst : x coordinate of the first vertex .
9- ali_lylast : y coordinate of the last vertex .
10- ali_ly_bl : y coordinate of the vertex before the last vertex .
11- ali_lysecond : y coordinate of the second vertex .
12- ali_lyfirst : y coordinate of the first vertex .
13- ali_lp_length : number of elements in the polyline list ( specification list + header list + vertexes + seqend )
14- ali_llastv : last vertex in the polyline list .
15- ali_before_last : the vertex before the last vertex .
16- ali_lsecondv : second vertex in the polyline list .
17- ali_lfirstv : first vertex in the polyline list .
18- ali_lupp : the highest vertex .
19- ali_ldown : the lowest vertex .
20- ali_pline_ends : a list containing highest (1st element) and lowest (2nd element) vertex entity data list of the polyline .

global parameters :
none

Sub-functions :
none

|;


;this function collects the highest and lowest vertex data list of every rebar polyline .

(defun ali_find_plends (ali_lplist / ali_lydown ali_lyupp ali_ldirect ali_lxlast ali_lx_bl ali_lxsecond ali_lxfirst ali_lylast ali_ly_bl ali_lysecond ali_lyfirst ali_lp_length ali_llastv ali_before_last ali_lsecondv ali_lfirstv ali_lupp ali_ldown ali_pline_ends)

  (setq ali_lp_length (length ali_lplist)
	ali_lfirstv (nth 2 ali_lplist)
	ali_lsecondv (nth 3 ali_lplist)
	ali_before_last (nth (- (length ali_lplist) 3) ali_lplist)
	ali_llastv (nth (- (length ali_lplist) 2) ali_lplist)
	)
  (setq ali_lyfirst (cadr (last ali_lfirstv))
	ali_lysecond (cadr (last ali_lsecondv))
	ali_ly_bl (cadr (last ali_before_last))
	ali_lylast (cadr (last ali_llastv))
	)
  (setq ali_lxfirst (car (last ali_lfirstv))
	ali_lxsecond (car (last ali_lsecondv))
	ali_lx_bl (car (last ali_before_last))
	ali_lxlast (car (last ali_llastv))
	)
  
  (if (< ali_lyfirst ali_lylast)
    (setq ali_ldirect 1
	  ali_lupp ali_llastv
	  ali_ldown ali_lfirstv
	  )
    (setq ali_ldirect -1
	  ali_lupp ali_lfirstv
	  ali_ldown ali_llastv
	  )
    )
  
  (setq ali_lydown (cadr (last ali_ldown))
	ali_lyupp (cadr (last ali_lupp))
	)

  (if (= ali_direct 1)
    (progn
      (if (= ali_lyupp ali_ly_bl)
	(setq ali_lupp ali_before_last)
	)
      (if (= ali_lydown ali_lysecond)
	(setq ali_down ali_lsecondv)
	)
      )
    (progn
      (if (= ali_lydown ali_ly_bl)
	(setq ali_down ali_before_last)
	)
      (if (= ali_lyupp ali_lysecond)
	(setq ali_lupp ali_lsecondv)
	)
      )
    )
      
  

  (setq ali_pline_ends (list ali_lupp ali_ldown))

  ali_pline_ends

  )


;......................................................................................................................................................
;|Function #17 : ali_join_bars

connects a rebar to it's uppstairs rebar . then deletes the lowers rebar polyline .

local parameters :
1- ali_lbars : the entity data list of two rebars we want to connect .
2- ali_pl_width1 : starting width of the lower rebar .
3- ali_pl_width2 : ending width of the lowwer rebar .
4- ali_lcoord1 : starting coordination (x,y) of the lower rebar .
5- ali_lcoord2 : starting coordination (x,y) of the uppstairs rebar .
6- ali_lower : entity data list of lower rebar .
7- ali_higher : entity data list of uppstairs rebar .
8- ali_first_x : x coordinate of the lower rebar .
9- ali_first_y : y coordinate of the lower rebar .
10- ali_second_x : x coordinate of the uppstairs rebar .
11- ali_second_y : y coordinate of the uppstairs rebar .

global parameters :
none

Sub-functions :
1- command entmake
2- command entdel

|;


(defun ali_join_bars (ali_lbars / ali_pl_width1 ali_pl_width2 ali_lcoord1 ali_lcoord2 ali_lower ali_higher ali_first_x ali_first_y ali_second_x ali_second_y)

  (setq ali_higher (cadr ali_lbars)
	ali_lower (car ali_lbars)
	)

  (setq ali_first_x (car (nth 3 (cadr (ali_find_plends ali_lower))))
	ali_first_y (cadr (nth 3 (cadr (ali_find_plends ali_lower))))
	ali_second_x (car (nth 3 (cadr (ali_find_plends ali_higher))))
	ali_second_y (cadr (nth 3 (cadr (ali_find_plends ali_higher))))
	)

  (setq ali_lcoord1 (list ali_first_x ali_first_y)
	ali_lcoord2 (list ali_second_x ali_second_y)
	)

  (setq ali_pl_width1 (cdr (assoc 40 (entget (cadr (nth 1 ali_lower)))))
	ali_pl_width2 (cdr (assoc 41 (entget (cadr (nth 1 ali_lower)))))
	)

  (entmake (list (cons 0 "POLYLINE") (cons 8 "-8") (cons 40 ali_pl_width1) (cons 41 ali_pl_width2) (cons 62 6)))
  (entmake (list (cons 0 "VERTEX") (cons 10 ali_lcoord1)))
  (entmake (list (cons 0 "VERTEX") (cons 10 ali_lcoord2)))
  (entmake (list (cons 0 "SEQEND")))

  (entdel (cadr (nth 1 ali_lower)))

  )




;......................................................................................................................................................

;|Function #18 : ali_del_nth

deletes the nth member of a list .

local parameters :
1- ali_loldlist : the original list .
2- ali_mem_pos : the position of the member we want to delete .
3- ali_counter : used as a pointer for the loop .
4- ali_lnewlist : the new list without the deleted member .
5- ali_lcnum : used as a counter for the loop .

global parameters :
none

Sub-functions :
none

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

;|Function #19 : ali_findpos

finds the position of the first occurance of an element in the list .

local parameters :
1- ali_list : the input list .
2- ali_lmem : the member that we want to find it's position .
3- ali_lcnum : used as a counter for the loop .
4- ali_lpos : the position of the element in list .
5- ali_flag : checks if the element has been found for the first time .

global parameters :
none

Sub-functions :
none


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

;|Function #20 : ali_findlowest

finds the smallest member of a list of numbers .

local parameters :
1- ali_list : the input list
2- ali_member : used as a pointer for the loop
3- ali_lowest : the lowest ( smallest ) number in list .

global parameters :
none

Sub-functions :
none

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

;|Function #21 : ali_sort_desc

sorts a list of numbers in descending order .

local parameters :
1- ali_orglist : the original list of numbers .
2- ali_newlist : the sorted list .
3- ali_lsmall : the smallest number , in one loop of process .
4- ali_flag : checks if its the first round of the loop ( used to creat the sorted list ) .

global parameters :
none

Sub-functions :
1- ali_findlowest
2- ali_findpos
3- ali_del_nth


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

