; source file: constructorCall2.t
; T version: 1.0
; compiled: Tue Nov 06 15:26:42 EST 2018

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
@arrayVMT = global [1 x i8*] [i8* bitcast ([2 x i8*]* @Object$VMT to i8*)]
@intVMT = global [1 x i8*] [i8* null]
@A$VMT = global [2 x i8*] [i8* bitcast ([2 x i8*]* @Object$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]
@Object$VMT = global [2 x i8*] [i8* null, i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]

; class object types
%class$A =  type { i8* }
%class$Object =  type { i8* }

; class default constructors and destructors
define void @defaultConstructor$A(i8* %ths) {
  call void @defaultConstructor$Object(i8* %ths)
  ret void
}
define void @defaultDestructor$A(i8* %ths) {
  call void @defaultDestructor$Object(i8* %ths)
  ret void
}
define void @defaultConstructor$Object(i8* %ths) {
  ret void
}
define void @defaultDestructor$Object(i8* %ths) {
  ret void
}

; class A constructor line 2
define void @A$onstructor$int$(i8* %param0, i32 %param1) {
  ; copy param temps into stack locations
  %temp0 = bitcast i8* %param0 to %class$A*
  %this = alloca %class$A*
  store %class$A* %temp0, %class$A** %this
  %i = alloca i32
  store i32 %param1, i32* %i
  %temp1 = load %class$A*, %class$A** %this
  %temp2 = bitcast %class$A* %temp1 to %class$Object*
  %temp3 = bitcast %class$Object* %temp2 to i8*
  ; constructor invocation
  call void @defaultConstructor$Object(i8* %temp3)
  ; deref
  %temp4 = load i32, i32* %i
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 %temp4)
  ret void
}

; main block
define void @main() {
  call void @t_rt_alloc_init()
  %a = alloca %class$A*
  store %class$A* null, %class$A** %a
  %temp5 = getelementptr %class$A, %class$A* null, i32 1
  %temp6 = ptrtoint %class$A* %temp5 to i64
  %temp7 = call i8* @t_rt_alloc(i64 %temp6, i32 7)
  %temp8 = bitcast i8* %temp7 to [2 x i8*]**
  store [2 x i8*]* @A$VMT, [2 x i8*]** %temp8
  call void @A$onstructor$int$(i8* %temp7, i32 42)
  %temp9 = bitcast i8* %temp7 to %class$A*
  ; assign
  store %class$A* %temp9, %class$A** %a
  ret void
}
