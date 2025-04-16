FUNCTION Create_Entry, d
 t=PTR_NEW(CREATE_STRUCT('data', d, 'prev', PTR_NEW()))
 return, t
END

FUNCTION CreateStack, d
 t=Create_Entry(d)
 return, t
END

FUNCTION Push, l, d
 t=Create_Entry(d)
 IF PTR_VALID(l) THEN BEGIN
     (*t).prev=l
 END
 return, t
END


FUNCTION Pop, l, d, FIFO=fifo
IF PTR_VALID(l) THEN BEGIN
    IF keyword_set(fifo) THEN BEGIN
        ;; go to the bottom of the stack
        tmp=l
        tmp2=tmp
        While PTR_VALID((*tmp).prev) Do BEGIN
            tmp2=tmp
            tmp=(*tmp).prev
        END
        ;; now there is no element underneath anymore
        d=(*tmp).data
        (*tmp2).prev=PTR_NEW()
        PTR_FREE, tmp
    END ELSE BEGIN
        d=(*l).data
        tmp=l
        l=(*l).prev
        PTR_FREE, tmp
    END
END
return, l
END

FUNCTION StackInfo, l, N_ELEMENTS=n_elements, PRINT=print
  count=0
  IF PTR_VALID(l) THEN BEGIN
     If keyword_set(n_elements) THEN BEGIN
        ;; go to the bottom of the stack
        count+=1
        tmp=l
        tmp2=tmp
        While PTR_VALID((*tmp).prev) Do BEGIN
            tmp2=tmp
            tmp=(*tmp).prev
            count+=1
        END
     END
     If keyword_set(print) THEN BEGIN
        ;; go to the bottom of the stack
        tmp=l
        tmp2=tmp
        print, (*tmp).data
        While PTR_VALID((*tmp).prev) Do BEGIN
           tmp2=tmp
           tmp=(*tmp).prev
           print, (*tmp).data
        END
     END
  END
return, count
END

PRO ClearStack, l, d
 WHILE PTR_VALID(Pop(l, d)) DO BEGIN
 END
END



;;PRO TestStack
;;print, "Creating lifo stack wih elements 1.0, 2.0, 3.0, 4.0"
;;x=1.0
;;s=CreateStack(x)
;;FOR I=2,4 DO BEGIN
;;    x=1.0*I
;;    s=Push(s,x)
;;END
;;print, "Reading lifo stack elements"
;;WHILE PTR_Valid(s) DO BEGIN
;;    s=Pop(s,x)
;;    print, x
;;END
;;print, "Clearing stack"
;;ClearStack, s, x
;;print, "Creating fifo stack wih elements 1.0, 2.0, 3.0, 4.0"
;;x=1.0
;;s=CreateStack(x)
;;FOR I=2,4 DO BEGIN
;;    x=1.0*I
;;    s=Push(s,x)
;;END
;;print, "Reading lifo stack elements"
;;WHILE PTR_Valid(s) DO BEGIN
;;    s=Pop(s,x, /FIFO)
;;    print, x
;;END
;;print, "Clearing stack"
;;ClearStack, s, x
;;END
