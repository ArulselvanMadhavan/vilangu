; source file: methodDecl.t
; T version: 1.0
; compiled: Mon Jul 19 10:22:33 EDT 2021

; declarations for the runtime support functions
declare void @t_rt_alloc_init()
declare i8* @t_rt_alloc(i64, i32)
declare void @t_rt_dealloc(i8*)
declare void @t_rt_print_int(i32)
declare void @t_rt_print_divide_by_zero_error(i32)
declare void @t_rt_null_reference_error(i32)
declare void @t_rt_index_out_of_bounds_error(i32)
declare i8* @t_rt_newArray(i32, i8*, i32, i32, ...)
declare void @t_rt_check_cast(i32, i8*, i8*)
declare void @t_rt_check_array_cast(i32, i8*, i32, i8*)
declare i32 @t_rt_array_assignment_int(i32, i8*, i32, i32)
declare i8* @t_rt_array_assignment_ref(i32, i8*, i32, i8*)
declare i32 @Object$equals$Object$(%class$Object*, %class$Object*)

; array runtime object types
%array$int = type {i8*, i8*, i32, i32, [0 x i32]}
%array$ref = type {i8*, i8*, i32, i32, [0 x i8*]}

; VMTs
@arrayVMT = global [1 x i8*] [i8* bitcast ([3 x i8*]* @Object$VMT to i8*)]
@intVMT = global [1 x i8*] [i8* null]
@A$VMT = global [6 x i8*] [i8* bitcast ([3 x i8*]* @Object$VMT to i8*), i8* bitcast ( void (%class$A*)*  @A$de$tructor$ to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth1$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth2$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth3$ to i8*)]
@B$VMT = global [8 x i8*] [i8* bitcast ([6 x i8*]* @A$VMT to i8*), i8* bitcast ( void (%class$B*)*  @B$de$tructor$ to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth1$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth2$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth3$ to i8*), i8* bitcast (%class$A* (%class$B*)* @B$meth4$ to i8*), i8* bitcast (%class$B* (%class$B*)* @B$meth5$ to i8*)]
@Object$VMT = global [3 x i8*] [i8* null, i8* bitcast ( void (%class$Object*)*  @Object$de$tructor$ to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]

; class object types
%class$A =  type { i8* }
%class$B =  type { i8* }
%class$Object =  type { i8* }

; default constructors and destructors
define void @A$con$tructor$(%class$A* %ths) {
  %temp0 = bitcast %class$A* %ths to %class$Object*
  call void @Object$con$tructor$(%class$Object* %temp0)
  ret void
}
define void @A$de$tructor$(%class$A* %ths) {
  %temp1 = bitcast %class$A* %ths to %class$Object*
  call void @Object$de$tructor$(%class$Object* %temp1)
  ret void
}
define void @B$con$tructor$(%class$B* %ths) {
  %temp2 = bitcast %class$B* %ths to %class$A*
  call void @A$con$tructor$(%class$A* %temp2)
  ret void
}
define void @B$de$tructor$(%class$B* %ths) {
  %temp3 = bitcast %class$B* %ths to %class$A*
  call void @A$de$tructor$(%class$A* %temp3)
  ret void
}
define void @Object$con$tructor$(%class$Object* %ths) {
  ret void
}
define void @Object$de$tructor$(%class$Object* %ths) {
  ret void
}

; class A method meth1 line 3
define i32 @A$meth1$(%class$A* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 1)
  ret i32 0
}

; class A method meth2 line 4
define i32 @A$meth2$(%class$A* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 2)
  ret i32 0
}

; class A method meth3 line 5
define i32 @A$meth3$(%class$A* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 3)
  ret i32 0
}

; class B method meth4 line 13
define %class$A* @B$meth4$(%class$B* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 4)
  ret %class$A* null
}

; class B method meth5 line 14
define %class$B* @B$meth5$(%class$B* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 5)
  ret %class$B* null
}

; main block
define i32 @main() {
  call void @t_rt_alloc_init()
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 42)
  ret i32 0
}
