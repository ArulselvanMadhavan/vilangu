; source file: delete3.t
; T version: 1.0
; compiled: Wed Nov 28 13:26:15 EST 2018

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
@A$VMT = global [3 x i8*] [i8* bitcast ([3 x i8*]* @Object$VMT to i8*), i8* bitcast ( void (%class$A*)*  @A$destructor$ to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]
@B$VMT = global [3 x i8*] [i8* bitcast ([3 x i8*]* @A$VMT to i8*), i8* bitcast ( void (%class$B*)*  @B$destructor$ to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]
@Object$VMT = global [3 x i8*] [i8* null, i8* bitcast ( void (%class$Object*)*  @Object$destructor$ to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]

; class object types
%class$A =  type { i8* }
%class$B =  type { i8* }
%class$Object =  type { i8* }

; default constructors and destructors
define void @A$constructor$(%class$A* %ths) {
  %temp0 = bitcast %class$A* %ths to %class$Object*
  call void @Object$constructor$(%class$Object* %temp0)
  ret void
}
define void @B$constructor$(%class$B* %ths) {
  %temp1 = bitcast %class$B* %ths to %class$A*
  call void @A$constructor$(%class$A* %temp1)
  ret void
}
define void @Object$constructor$(%class$Object* %ths) {
  ret void
}
define void @Object$destructor$(%class$Object* %ths) {
  ret void
}

; class A destructor line 2
define void @A$destructor$ (%class$A* %param0) {
  ; copy param temp into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 41)
  %temp2 = bitcast %class$A* %param0 to %class$Object*
  call void @Object$destructor$(%class$Object* %temp2)
  ret void
}

; class B destructor line 6
define void @B$destructor$ (%class$B* %param0) {
  ; copy param temp into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 40)
  %temp3 = bitcast %class$B* %param0 to %class$A*
  call void @A$destructor$(%class$A* %temp3)
  ret void
}

; main block
define i32 @main() {
  call void @t_rt_alloc_init()
  %a = alloca %class$A*
  store %class$A* null, %class$A** %a
  %temp4 = getelementptr %class$B, %class$B* null, i32 1
  %temp5 = ptrtoint %class$B* %temp4 to i64
  %temp6 = call i8* @t_rt_alloc(i64 %temp5, i32 11)
  %temp7 = bitcast i8* %temp6 to [3 x i8*]**
  store [3 x i8*]* @B$VMT, [3 x i8*]** %temp7
  %temp8 = bitcast i8* %temp6 to %class$B*
  call void @B$constructor$(%class$B* %temp8)
  %temp9 = bitcast i8* %temp6 to %class$B*
  ; assign
  %temp11 = bitcast %class$B* %temp9 to %class$A*
  store %class$A* %temp11, %class$A** %a
  ; deref
  %temp12 = load %class$A*, %class$A** %a
  ; delete
  %temp13 = getelementptr %class$A, %class$A* %temp12, i32 0, i32 0
  %temp14 = load i8*, i8** %temp13
  %temp15 = bitcast i8* %temp14 to [1 x i8*]*
  %temp16 = getelementptr [1 x i8*], [1 x i8*]* %temp15, i32 0, i32 1
  %temp17 = load i8*, i8** %temp16
  %temp18 = bitcast %class$A* %temp12 to i8*
  %temp19 = bitcast i8* %temp17 to void (i8*)*
  call void %temp19 (i8* %temp18)
  call void @t_rt_dealloc(i8* %temp18)
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 42)
  ret i32 0
}
