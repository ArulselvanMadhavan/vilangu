; source file: superField.t
; T version: 1.0
; compiled: Sun Nov 18 21:36:17 EST 2018

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
@C$VMT = global [3 x i8*] [i8* bitcast ([2 x i8*]* @B$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$C*)* @C$meth$ to i8*)]
@Object$VMT = global [2 x i8*] [i8* null, i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]

; class object types
%class$A =  type { i8*, i32 }
%class$B =  type { i8*, i32, i32 }
%class$C =  type { i8*, i32, i32, %class$A* }
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

; class C method meth line 10
define i32 @C$meth$(%class$C* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$C*
  store %class$C* %param0, %class$C** %this
  %temp0 = load %class$C*, %class$C** %this
  %temp1 = bitcast %class$C* %temp0 to %class$B*
  ; field access
  %temp3 = icmp eq %class$B* %temp1, null
  br i1 %temp3, label %Label0, label %Label1
  Label0:
  call void @t_rt_null_reference_error(i32 10)
  br label %Label1
  Label1:
  %temp2 = getelementptr %class$B, %class$B* %temp1, i32 0, i32 1
  ; deref
  %temp4 = load i32, i32* %temp2
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 %temp4)
  ret i32 0
}

; main block
define i32 @main() {
  call void @t_rt_alloc_init()
  %c = alloca %class$C*
  store %class$C* null, %class$C** %c
  %a = alloca %class$A*
  store %class$A* null, %class$A** %a
  %temp5 = getelementptr %class$C, %class$C* null, i32 1
  %temp6 = ptrtoint %class$C* %temp5 to i64
  %temp7 = call i8* @t_rt_alloc(i64 %temp6, i32 17)
  %temp8 = bitcast i8* %temp7 to [3 x i8*]**
  store [3 x i8*]* @C$VMT, [3 x i8*]** %temp8
  call void @defaultConstructor$C(i8* %temp7)
  %temp9 = bitcast i8* %temp7 to %class$C*
  ; assign
  store %class$C* %temp9, %class$C** %c
  ; deref
  %temp11 = load %class$C*, %class$C** %c
  ; assign
  %temp13 = bitcast %class$C* %temp11 to %class$A*
  store %class$A* %temp13, %class$A** %a
  ; deref
  %temp14 = load %class$A*, %class$A** %a
  ; field access
  %temp16 = icmp eq %class$A* %temp14, null
  br i1 %temp16, label %Label2, label %Label3
  Label2:
  call void @t_rt_null_reference_error(i32 19)
  br label %Label3
  Label3:
  %temp15 = getelementptr %class$A, %class$A* %temp14, i32 0, i32 1
  ; assign
  store i32 42, i32* %temp15
  ; deref
  %temp18 = load %class$C*, %class$C** %c
  ; field access
  %temp20 = icmp eq %class$C* %temp18, null
  br i1 %temp20, label %Label4, label %Label5
  Label4:
  call void @t_rt_null_reference_error(i32 20)
  br label %Label5
  Label5:
  %temp19 = getelementptr %class$C, %class$C* %temp18, i32 0, i32 2
  ; assign
  store i32 33, i32* %temp19
  ; deref
  %temp22 = load %class$C*, %class$C** %c
  ; field access
  %temp24 = icmp eq %class$C* %temp22, null
  br i1 %temp24, label %Label6, label %Label7
  Label6:
  call void @t_rt_null_reference_error(i32 21)
  br label %Label7
  Label7:
  %temp23 = getelementptr %class$C, %class$C* %temp22, i32 0, i32 3
  ; deref
  %temp25 = load %class$A*, %class$A** %a
  ; assign
  store %class$A* %temp25, %class$A** %temp23
  ; deref
  %temp27 = load %class$C*, %class$C** %c
  ; method call
  %temp28 = icmp eq %class$C* %temp27, null
  br i1 %temp28, label %Label8, label %Label9
  Label8:
  call void @t_rt_null_reference_error(i32 22)
  br label %Label9
  Label9:
  %temp29 = getelementptr %class$C, %class$C* %temp27, i32 0, i32 0
  %temp30 = load i8*, i8** %temp29
  %temp31 = bitcast i8* %temp30 to [1 x i8*]*
  %temp32 = getelementptr [1 x i8*], [1 x i8*]* %temp31, i32 0, i32 2
  %temp33 = load i8*, i8** %temp32
  %temp34 = bitcast i8* %temp33 to i32 (%class$C*)*
  %temp35 = call i32 %temp34 (%class$C* %temp27)
  ret i32 0
}
