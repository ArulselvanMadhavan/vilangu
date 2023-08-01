; source file: methRet3.t
; T version: 1.0
; compiled: Sun Nov 18 21:24:13 EST 2018

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
@A$VMT = global [3 x i8*] [i8* bitcast ([2 x i8*]* @Object$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (%class$A* (%class$A*)* @A$meth1$ to i8*)]
@B$VMT = global [3 x i8*] [i8* bitcast ([3 x i8*]* @A$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (%class$A* (%class$A*)* @A$meth1$ to i8*)]
@Object$VMT = global [2 x i8*] [i8* null, i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]

; class object types
%class$A =  type { i8*, i32, %class$B* }
%class$B =  type { i8*, i32, %class$B* }
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

; class A method meth1 line 4
define %class$A* @A$meth1$(%class$A* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  %temp0 = load %class$A*, %class$A** %this
  ; field access
  %temp2 = icmp eq %class$A* %temp0, null
  br i1 %temp2, label %Label0, label %Label1
  Label0:
  call void @t_rt_null_reference_error(i32 4)
  br label %Label1
  Label1:
  %temp1 = getelementptr %class$A, %class$A* %temp0, i32 0, i32 2
  %temp3 = getelementptr %class$B, %class$B* null, i32 1
  %temp4 = ptrtoint %class$B* %temp3 to i64
  %temp5 = call i8* @t_rt_alloc(i64 %temp4, i32 4)
  %temp6 = bitcast i8* %temp5 to [3 x i8*]**
  store [3 x i8*]* @B$VMT, [3 x i8*]** %temp6
  call void @defaultConstructor$B(i8* %temp5)
  %temp7 = bitcast i8* %temp5 to %class$B*
  ; assign
  store %class$B* %temp7, %class$B** %temp1
  %temp9 = load %class$A*, %class$A** %this
  ; field access
  %temp11 = icmp eq %class$A* %temp9, null
  br i1 %temp11, label %Label2, label %Label3
  Label2:
  call void @t_rt_null_reference_error(i32 4)
  br label %Label3
  Label3:
  %temp10 = getelementptr %class$A, %class$A* %temp9, i32 0, i32 2
  ; deref
  %temp12 = load %class$B*, %class$B** %temp10
  ; field access
  %temp14 = icmp eq %class$B* %temp12, null
  br i1 %temp14, label %Label4, label %Label5
  Label4:
  call void @t_rt_null_reference_error(i32 4)
  br label %Label5
  Label5:
  %temp13 = getelementptr %class$B, %class$B* %temp12, i32 0, i32 1
  ; assign
  store i32 42, i32* %temp13
  ; return
  %temp16 = load %class$A*, %class$A** %this
  ; field access
  %temp18 = icmp eq %class$A* %temp16, null
  br i1 %temp18, label %Label6, label %Label7
  Label6:
  call void @t_rt_null_reference_error(i32 4)
  br label %Label7
  Label7:
  %temp17 = getelementptr %class$A, %class$A* %temp16, i32 0, i32 2
  ; deref
  %temp19 = load %class$B*, %class$B** %temp17
  %temp20 = bitcast %class$B* %temp19 to %class$A*
  ret %class$A* %temp20
  ret %class$A* null
}

; main block
define i32 @main() {
  call void @t_rt_alloc_init()
  %a = alloca %class$A*
  store %class$A* null, %class$A** %a
  %temp21 = getelementptr %class$A, %class$A* null, i32 1
  %temp22 = ptrtoint %class$A* %temp21 to i64
  %temp23 = call i8* @t_rt_alloc(i64 %temp22, i32 12)
  %temp24 = bitcast i8* %temp23 to [3 x i8*]**
  store [3 x i8*]* @A$VMT, [3 x i8*]** %temp24
  call void @defaultConstructor$A(i8* %temp23)
  %temp25 = bitcast i8* %temp23 to %class$A*
  ; assign
  store %class$A* %temp25, %class$A** %a
  ; deref
  %temp27 = load %class$A*, %class$A** %a
  ; method call
  %temp28 = icmp eq %class$A* %temp27, null
  br i1 %temp28, label %Label8, label %Label9
  Label8:
  call void @t_rt_null_reference_error(i32 13)
  br label %Label9
  Label9:
  %temp29 = getelementptr %class$A, %class$A* %temp27, i32 0, i32 0
  %temp30 = load i8*, i8** %temp29
  %temp31 = bitcast i8* %temp30 to [1 x i8*]*
  %temp32 = getelementptr [1 x i8*], [1 x i8*]* %temp31, i32 0, i32 2
  %temp33 = load i8*, i8** %temp32
  %temp34 = bitcast i8* %temp33 to %class$A* (%class$A*)*
  %temp35 = call %class$A* %temp34 (%class$A* %temp27)
  ; field access
  %temp37 = icmp eq %class$A* %temp35, null
  br i1 %temp37, label %Label10, label %Label11
  Label10:
  call void @t_rt_null_reference_error(i32 13)
  br label %Label11
  Label11:
  %temp36 = getelementptr %class$A, %class$A* %temp35, i32 0, i32 1
  ; deref
  %temp38 = load i32, i32* %temp36
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 %temp38)
  ret i32 0
}
