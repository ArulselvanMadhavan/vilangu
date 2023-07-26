; source file: thisCall.t
; T version: 1.0
; compiled: Sun Nov 18 21:36:35 EST 2018

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
@A$VMT = global [4 x i8*] [i8* bitcast ([2 x i8*]* @Object$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth1$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth2$ to i8*)]
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
define i32 @A$meth1$(%class$A* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 1)
  ret i32 0
}

; class A method meth2 line 3
define i32 @A$meth2$(%class$A* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  %temp0 = load %class$A*, %class$A** %this
  ; method call
  %temp1 = getelementptr %class$A, %class$A* %temp0, i32 0, i32 0
  %temp2 = load i8*, i8** %temp1
  %temp3 = bitcast i8* %temp2 to [1 x i8*]*
  %temp4 = getelementptr [1 x i8*], [1 x i8*]* %temp3, i32 0, i32 2
  %temp5 = load i8*, i8** %temp4
  %temp6 = bitcast i8* %temp5 to i32 (%class$A*)*
  %temp7 = call i32 %temp6 (%class$A* %temp0)
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 2)
  ret i32 0
}

; main block
define i32 @main() {
  call void @t_rt_alloc_init()
  %a = alloca %class$A*
  store %class$A* null, %class$A** %a
  %temp8 = getelementptr %class$A, %class$A* null, i32 1
  %temp9 = ptrtoint %class$A* %temp8 to i64
  %temp10 = call i8* @t_rt_alloc(i64 %temp9, i32 8)
  %temp11 = bitcast i8* %temp10 to [4 x i8*]**
  store [4 x i8*]* @A$VMT, [4 x i8*]** %temp11
  call void @defaultConstructor$A(i8* %temp10)
  %temp12 = bitcast i8* %temp10 to %class$A*
  ; assign
  store %class$A* %temp12, %class$A** %a
  ; deref
  %temp14 = load %class$A*, %class$A** %a
  ; method call
  %temp15 = icmp eq %class$A* %temp14, null
  br i1 %temp15, label %Label0, label %Label1
  Label0:
  call void @t_rt_null_reference_error(i32 9)
  br label %Label1
  Label1:
  %temp16 = getelementptr %class$A, %class$A* %temp14, i32 0, i32 0
  %temp17 = load i8*, i8** %temp16
  %temp18 = bitcast i8* %temp17 to [1 x i8*]*
  %temp19 = getelementptr [1 x i8*], [1 x i8*]* %temp18, i32 0, i32 3
  %temp20 = load i8*, i8** %temp19
  %temp21 = bitcast i8* %temp20 to i32 (%class$A*)*
  %temp22 = call i32 %temp21 (%class$A* %temp14)
  ret i32 0
}
