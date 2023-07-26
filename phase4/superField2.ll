; source file: superField2.t
; T version: 1.0
; compiled: Sun Nov 18 21:36:16 EST 2018

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
@B$VMT = global [4 x i8*] [i8* bitcast ([2 x i8*]* @A$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$B*)* @B$meth1$ to i8*), i8* bitcast (i32 (%class$B*)* @B$meth2$ to i8*)]
@Object$VMT = global [2 x i8*] [i8* null, i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]

; class object types
%class$A =  type { i8*, i32 }
%class$B =  type { i8*, i32, i32 }
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

; class B method meth1 line 7
define i32 @B$meth1$(%class$B* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  %temp0 = load %class$B*, %class$B** %this
  %temp1 = bitcast %class$B* %temp0 to %class$A*
  ; field access
  %temp3 = icmp eq %class$A* %temp1, null
  br i1 %temp3, label %Label0, label %Label1
  Label0:
  call void @t_rt_null_reference_error(i32 7)
  br label %Label1
  Label1:
  %temp2 = getelementptr %class$A, %class$A* %temp1, i32 0, i32 1
  ; assign
  store i32 33, i32* %temp2
  %temp5 = load %class$B*, %class$B** %this
  ; field access
  %temp7 = icmp eq %class$B* %temp5, null
  br i1 %temp7, label %Label2, label %Label3
  Label2:
  call void @t_rt_null_reference_error(i32 7)
  br label %Label3
  Label3:
  %temp6 = getelementptr %class$B, %class$B* %temp5, i32 0, i32 2
  ; assign
  store i32 9, i32* %temp6
  ret i32 0
}

; class B method meth2 line 8
define i32 @B$meth2$(%class$B* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  %temp9 = load %class$B*, %class$B** %this
  %temp10 = bitcast %class$B* %temp9 to %class$A*
  ; field access
  %temp12 = icmp eq %class$A* %temp10, null
  br i1 %temp12, label %Label4, label %Label5
  Label4:
  call void @t_rt_null_reference_error(i32 8)
  br label %Label5
  Label5:
  %temp11 = getelementptr %class$A, %class$A* %temp10, i32 0, i32 1
  ; deref
  %temp13 = load i32, i32* %temp11
  %temp14 = load %class$B*, %class$B** %this
  ; field access
  %temp16 = icmp eq %class$B* %temp14, null
  br i1 %temp16, label %Label6, label %Label7
  Label6:
  call void @t_rt_null_reference_error(i32 8)
  br label %Label7
  Label7:
  %temp15 = getelementptr %class$B, %class$B* %temp14, i32 0, i32 2
  ; deref
  %temp17 = load i32, i32* %temp15
  ; add
  %temp18 = add i32 %temp13, %temp17
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 %temp18)
  ret i32 0
}

; main block
define i32 @main() {
  call void @t_rt_alloc_init()
  %b = alloca %class$B*
  store %class$B* null, %class$B** %b
  %temp19 = getelementptr %class$B, %class$B* null, i32 1
  %temp20 = ptrtoint %class$B* %temp19 to i64
  %temp21 = call i8* @t_rt_alloc(i64 %temp20, i32 13)
  %temp22 = bitcast i8* %temp21 to [4 x i8*]**
  store [4 x i8*]* @B$VMT, [4 x i8*]** %temp22
  call void @defaultConstructor$B(i8* %temp21)
  %temp23 = bitcast i8* %temp21 to %class$B*
  ; assign
  store %class$B* %temp23, %class$B** %b
  ; deref
  %temp25 = load %class$B*, %class$B** %b
  ; method call
  %temp26 = icmp eq %class$B* %temp25, null
  br i1 %temp26, label %Label8, label %Label9
  Label8:
  call void @t_rt_null_reference_error(i32 14)
  br label %Label9
  Label9:
  %temp27 = getelementptr %class$B, %class$B* %temp25, i32 0, i32 0
  %temp28 = load i8*, i8** %temp27
  %temp29 = bitcast i8* %temp28 to [1 x i8*]*
  %temp30 = getelementptr [1 x i8*], [1 x i8*]* %temp29, i32 0, i32 2
  %temp31 = load i8*, i8** %temp30
  %temp32 = bitcast i8* %temp31 to i32 (%class$B*)*
  %temp33 = call i32 %temp32 (%class$B* %temp25)
  ; deref
  %temp34 = load %class$B*, %class$B** %b
  ; method call
  %temp35 = icmp eq %class$B* %temp34, null
  br i1 %temp35, label %Label10, label %Label11
  Label10:
  call void @t_rt_null_reference_error(i32 15)
  br label %Label11
  Label11:
  %temp36 = getelementptr %class$B, %class$B* %temp34, i32 0, i32 0
  %temp37 = load i8*, i8** %temp36
  %temp38 = bitcast i8* %temp37 to [1 x i8*]*
  %temp39 = getelementptr [1 x i8*], [1 x i8*]* %temp38, i32 0, i32 3
  %temp40 = load i8*, i8** %temp39
  %temp41 = bitcast i8* %temp40 to i32 (%class$B*)*
  %temp42 = call i32 %temp41 (%class$B* %temp34)
  ret i32 0
}
