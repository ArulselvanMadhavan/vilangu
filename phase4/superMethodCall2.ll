; source file: superMethodCall2.t
; T version: 1.0
; compiled: Sun Nov 18 21:36:25 EST 2018

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
@A$VMT = global [4 x i8*] [i8* bitcast ([2 x i8*]* @Object$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth1$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth4$ to i8*)]
@B$VMT = global [5 x i8*] [i8* bitcast ([4 x i8*]* @A$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$B*)* @B$meth1$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth4$ to i8*), i8* bitcast (i32 (%class$B*)* @B$meth2$ to i8*)]
@C$VMT = global [6 x i8*] [i8* bitcast ([5 x i8*]* @B$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$B*)* @B$meth1$ to i8*), i8* bitcast (i32 (%class$C*)* @C$meth4$ to i8*), i8* bitcast (i32 (%class$B*)* @B$meth2$ to i8*), i8* bitcast (i32 (%class$C*)* @C$meth3$ to i8*)]
@Object$VMT = global [2 x i8*] [i8* null, i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]

; class object types
%class$A =  type { i8* }
%class$B =  type { i8* }
%class$C =  type { i8* }
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
define void @defaultConstructor$C(i8* %ths) {
  call void @defaultConstructor$B(i8* %ths)
  ret void
}
define void @defaultDestructor$C(i8* %ths) {
  call void @defaultDestructor$B(i8* %ths)
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

; class A method meth4 line 3
define i32 @A$meth4$(%class$A* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 44)
  ret i32 0
}

; class B method meth1 line 7
define i32 @B$meth1$(%class$B* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 4242)
  ret i32 0
}

; class B method meth2 line 8
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

; class C method meth3 line 12
define i32 @C$meth3$(%class$C* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$C*
  store %class$C* %param0, %class$C** %this
  %temp3 = load %class$C*, %class$C** %this
  %temp4 = bitcast %class$C* %temp3 to %class$B*
  ; method call
  %temp5 = bitcast %class$B* %temp4 to %class$A*
  %temp6 = call i32 @A$meth4$ (%class$A* %temp5)
  ret i32 0
}

; class C method meth4 line 13
define i32 @C$meth4$(%class$C* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$C*
  store %class$C* %param0, %class$C** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 444444)
  ret i32 0
}

; main block
define i32 @main() {
  call void @t_rt_alloc_init()
  %c = alloca %class$C*
  store %class$C* null, %class$C** %c
  %temp7 = getelementptr %class$C, %class$C* null, i32 1
  %temp8 = ptrtoint %class$C* %temp7 to i64
  %temp9 = call i8* @t_rt_alloc(i64 %temp8, i32 17)
  %temp10 = bitcast i8* %temp9 to [6 x i8*]**
  store [6 x i8*]* @C$VMT, [6 x i8*]** %temp10
  call void @defaultConstructor$C(i8* %temp9)
  %temp11 = bitcast i8* %temp9 to %class$C*
  ; assign
  store %class$C* %temp11, %class$C** %c
  ; deref
  %temp13 = load %class$C*, %class$C** %c
  ; method call
  %temp14 = icmp eq %class$C* %temp13, null
  br i1 %temp14, label %Label0, label %Label1
  Label0:
  call void @t_rt_null_reference_error(i32 18)
  br label %Label1
  Label1:
  %temp15 = getelementptr %class$C, %class$C* %temp13, i32 0, i32 0
  %temp16 = load i8*, i8** %temp15
  %temp17 = bitcast i8* %temp16 to [1 x i8*]*
  %temp18 = getelementptr [1 x i8*], [1 x i8*]* %temp17, i32 0, i32 5
  %temp19 = load i8*, i8** %temp18
  %temp20 = bitcast i8* %temp19 to i32 (%class$C*)*
  %temp21 = call i32 %temp20 (%class$C* %temp13)
  ret i32 0
}
