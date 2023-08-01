; source file: constructorCall.t
; T version: 1.0
; compiled: Tue Nov 06 15:26:27 EST 2018

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
@B$VMT = global [2 x i8*] [i8* bitcast ([2 x i8*]* @A$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]
@Object$VMT = global [2 x i8*] [i8* null, i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]

; class object types
%class$A =  type { i8* }
%class$B =  type { i8* }
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
define void @defaultConstructor$B(i8* %ths) {
  call void @defaultConstructor$A(i8* %ths)
  ret void
}
define void @defaultDestructor$B(i8* %ths) {
  call void @defaultDestructor$A(i8* %ths)
  ret void
}
define void @defaultConstructor$Object(i8* %ths) {
  ret void
}
define void @defaultDestructor$Object(i8* %ths) {
  ret void
}

; class B constructor line 2
define void @B$onstructor$(i8* %param0) {
  ; copy param temps into stack locations
  %temp0 = bitcast i8* %param0 to %class$B*
  %this = alloca %class$B*
  store %class$B* %temp0, %class$B** %this
  %temp1 = load %class$B*, %class$B** %this
  %temp2 = bitcast %class$B* %temp1 to %class$A*
  %temp3 = bitcast %class$A* %temp2 to i8*
  ; constructor invocation
  call void @A$onstructor$(i8* %temp3)
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 17)
  ret void
}

; class A constructor line 13
define void @A$onstructor$(i8* %param0) {
  ; copy param temps into stack locations
  %temp4 = bitcast i8* %param0 to %class$A*
  %this = alloca %class$A*
  store %class$A* %temp4, %class$A** %this
  %temp5 = load %class$A*, %class$A** %this
  %temp6 = bitcast %class$A* %temp5 to %class$Object*
  %temp7 = bitcast %class$Object* %temp6 to i8*
  ; constructor invocation
  call void @defaultConstructor$Object(i8* %temp7)
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 7)
  ret void
}

; main block
define void @main() {
  call void @t_rt_alloc_init()
  %a = alloca %class$A*
  store %class$A* null, %class$A** %a
  %temp8 = getelementptr %class$A, %class$A* null, i32 1
  %temp9 = ptrtoint %class$A* %temp8 to i64
  %temp10 = call i8* @t_rt_alloc(i64 %temp9, i32 7)
  %temp11 = bitcast i8* %temp10 to [2 x i8*]**
  store [2 x i8*]* @A$VMT, [2 x i8*]** %temp11
  call void @A$onstructor$(i8* %temp10)
  %temp12 = bitcast i8* %temp10 to %class$A*
  ; assign
  store %class$A* %temp12, %class$A** %a
  %b = alloca %class$B*
  store %class$B* null, %class$B** %b
  %temp14 = getelementptr %class$B, %class$B* null, i32 1
  %temp15 = ptrtoint %class$B* %temp14 to i64
  %temp16 = call i8* @t_rt_alloc(i64 %temp15, i32 9)
  %temp17 = bitcast i8* %temp16 to [2 x i8*]**
  store [2 x i8*]* @B$VMT, [2 x i8*]** %temp17
  call void @B$onstructor$(i8* %temp16)
  %temp18 = bitcast i8* %temp16 to %class$B*
  ; assign
  store %class$B* %temp18, %class$B** %b
  ret void
}
