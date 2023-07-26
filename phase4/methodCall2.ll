; source file: methodCall2.t
; T version: 1.0
; compiled: Tue Nov 06 15:33:15 EST 2018

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
@A$VMT = global [3 x i8*] [i8* bitcast ([2 x i8*]* @Object$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$A*, i32, i32)* @A$meth1$int$int$ to i8*)]
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

; class A method meth1 line 2
define i32 @A$meth1$int$int$(%class$A* %param0, i32 %param1, i32 %param2) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  %a = alloca i32
  store i32 %param1, i32* %a
  %b = alloca i32
  store i32 %param2, i32* %b
  ; deref
  %temp0 = load i32, i32* %a
  ; deref
  %temp1 = load i32, i32* %b
  ; add
  %temp2 = add i32 %temp0, %temp1
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 %temp2)
  ret i32 0
}

; main block
define void @main() {
  call void @t_rt_alloc_init()
  %a = alloca %class$A*
  store %class$A* null, %class$A** %a
  %temp3 = getelementptr %class$A, %class$A* null, i32 1
  %temp4 = ptrtoint %class$A* %temp3 to i64
  %temp5 = call i8* @t_rt_alloc(i64 %temp4, i32 9)
  %temp6 = bitcast i8* %temp5 to [3 x i8*]**
  store [3 x i8*]* @A$VMT, [3 x i8*]** %temp6
  call void @defaultConstructor$A(i8* %temp5)
  %temp7 = bitcast i8* %temp5 to %class$A*
  ; assign
  store %class$A* %temp7, %class$A** %a
  ; deref
  %temp9 = load %class$A*, %class$A** %a
  ; method call
  %temp10 = icmp eq %class$A* %temp9, null
  br i1 %temp10, label %Label0, label %Label1
  Label0:
  call void @t_rt_null_reference_error(i32 10)
  br label %Label1
  Label1:
  %temp11 = getelementptr %class$A, %class$A* %temp9, i32 0, i32 0
  %temp12 = load i8*, i8** %temp11
  %temp13 = bitcast i8* %temp12 to [1 x i8*]*
  %temp14 = getelementptr [1 x i8*], [1 x i8*]* %temp13, i32 0, i32 2
  %temp15 = load i8*, i8** %temp14
  %temp16 = bitcast i8* %temp15 to i32 (%class$A*, i32, i32)*
  %temp17 = call i32 %temp16 (%class$A* %temp9, i32 20, i32 22)
  ret void
}
