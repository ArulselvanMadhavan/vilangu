; source file: superMethodCall.t
; T version: 1.0
; compiled: Sun Nov 18 21:36:27 EST 2018

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
@A$VMT = global [3 x i8*] [i8* bitcast ([2 x i8*]* @Object$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth1$ to i8*)]
@B$VMT = global [4 x i8*] [i8* bitcast ([3 x i8*]* @A$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$B*)* @B$meth1$ to i8*), i8* bitcast (i32 (%class$B*)* @B$meth2$ to i8*)]
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

; class A method meth1 line 2
define i32 @A$meth1$(%class$A* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 42)
  ret i32 0
}

; class B method meth1 line 6
define i32 @B$meth1$(%class$B* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 4242)
  ret i32 0
}

; class B method meth2 line 7
define i32 @B$meth2$(%class$B* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  %temp0 = load %class$B*, %class$B** %this
  %temp1 = bitcast %class$B* %temp0 to %class$A*
  ; method call
  %temp2 = call i32 @A$meth1$ (%class$A* %temp1)
  ret i32 0
}

; main block
define i32 @main() {
  call void @t_rt_alloc_init()
  %b = alloca %class$B*
  store %class$B* null, %class$B** %b
  %temp3 = getelementptr %class$B, %class$B* null, i32 1
  %temp4 = ptrtoint %class$B* %temp3 to i64
  %temp5 = call i8* @t_rt_alloc(i64 %temp4, i32 12)
  %temp6 = bitcast i8* %temp5 to [4 x i8*]**
  store [4 x i8*]* @B$VMT, [4 x i8*]** %temp6
  call void @defaultConstructor$B(i8* %temp5)
  %temp7 = bitcast i8* %temp5 to %class$B*
  ; assign
  store %class$B* %temp7, %class$B** %b
  ; deref
  %temp9 = load %class$B*, %class$B** %b
  ; method call
  %temp10 = icmp eq %class$B* %temp9, null
  br i1 %temp10, label %Label0, label %Label1
  Label0:
  call void @t_rt_null_reference_error(i32 13)
  br label %Label1
  Label1:
  %temp11 = getelementptr %class$B, %class$B* %temp9, i32 0, i32 0
  %temp12 = load i8*, i8** %temp11
  %temp13 = bitcast i8* %temp12 to [1 x i8*]*
  %temp14 = getelementptr [1 x i8*], [1 x i8*]* %temp13, i32 0, i32 3
  %temp15 = load i8*, i8** %temp14
  %temp16 = bitcast i8* %temp15 to i32 (%class$B*)*
  %temp17 = call i32 %temp16 (%class$B* %temp9)
  ret i32 0
}
