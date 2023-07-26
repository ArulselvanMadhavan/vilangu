; source file: methRet2.t
; T version: 1.0
; compiled: Thu Dec 13 17:19:32 EST 2018

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
@A$VMT = global [4 x i8*] [i8* bitcast ([3 x i8*]* @Object$VMT to i8*), i8* bitcast ( void (%class$A*)*  @A$destructor$ to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (%class$A* (%class$A*)* @A$meth1$ to i8*)]
@Object$VMT = global [3 x i8*] [i8* null, i8* bitcast ( void (%class$Object*)*  @Object$destructor$ to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]

; class object types
%class$A =  type { i8*, i32, %class$A* }
%class$Object =  type { i8* }

; default constructors and destructors
define void @A$constructor$(%class$A* %ths) {
  %temp0 = bitcast %class$A* %ths to %class$Object*
  call void @Object$constructor$(%class$Object* %temp0)
  ret void
}
define void @A$destructor$(%class$A* %ths) {
  %temp1 = bitcast %class$A* %ths to %class$Object*
  call void @Object$destructor$(%class$Object* %temp1)
  ret void
}
define void @Object$constructor$(%class$Object* %ths) {
  ret void
}
define void @Object$destructor$(%class$Object* %ths) {
  ret void
}

; class A method meth1 line 4
define %class$A* @A$meth1$(%class$A* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  %temp2 = load %class$A*, %class$A** %this
  ; field access
  %temp4 = icmp eq %class$A* %temp2, null
  br i1 %temp4, label %Label0, label %Label1
  Label0:
  call void @t_rt_null_reference_error(i32 4)
  br label %Label1
  Label1:
  %temp3 = getelementptr %class$A, %class$A* %temp2, i32 0, i32 2
  %temp5 = getelementptr %class$A, %class$A* null, i32 1
  %temp6 = ptrtoint %class$A* %temp5 to i64
  %temp7 = call i8* @t_rt_alloc(i64 %temp6, i32 4)
  %temp8 = bitcast i8* %temp7 to [4 x i8*]**
  store [4 x i8*]* @A$VMT, [4 x i8*]** %temp8
  %temp9 = bitcast i8* %temp7 to %class$A*
  call void @A$constructor$(%class$A* %temp9)
  %temp10 = bitcast i8* %temp7 to %class$A*
  ; assign
  store %class$A* %temp10, %class$A** %temp3
  %temp12 = load %class$A*, %class$A** %this
  ; field access
  %temp14 = icmp eq %class$A* %temp12, null
  br i1 %temp14, label %Label2, label %Label3
  Label2:
  call void @t_rt_null_reference_error(i32 4)
  br label %Label3
  Label3:
  %temp13 = getelementptr %class$A, %class$A* %temp12, i32 0, i32 2
  ; deref
  %temp15 = load %class$A*, %class$A** %temp13
  ; field access
  %temp17 = icmp eq %class$A* %temp15, null
  br i1 %temp17, label %Label4, label %Label5
  Label4:
  call void @t_rt_null_reference_error(i32 4)
  br label %Label5
  Label5:
  %temp16 = getelementptr %class$A, %class$A* %temp15, i32 0, i32 1
  ; assign
  store i32 42, i32* %temp16
  ; return
  %temp19 = load %class$A*, %class$A** %this
  ; field access
  %temp21 = icmp eq %class$A* %temp19, null
  br i1 %temp21, label %Label6, label %Label7
  Label6:
  call void @t_rt_null_reference_error(i32 4)
  br label %Label7
  Label7:
  %temp20 = getelementptr %class$A, %class$A* %temp19, i32 0, i32 2
  ; deref
  %temp22 = load %class$A*, %class$A** %temp20
  ret %class$A* %temp22
  ret %class$A* null
}

; main block
define i32 @main() {
  call void @t_rt_alloc_init()
  %a = alloca %class$A*
  store %class$A* null, %class$A** %a
  %temp23 = getelementptr %class$A, %class$A* null, i32 1
  %temp24 = ptrtoint %class$A* %temp23 to i64
  %temp25 = call i8* @t_rt_alloc(i64 %temp24, i32 9)
  %temp26 = bitcast i8* %temp25 to [4 x i8*]**
  store [4 x i8*]* @A$VMT, [4 x i8*]** %temp26
  %temp27 = bitcast i8* %temp25 to %class$A*
  call void @A$constructor$(%class$A* %temp27)
  %temp28 = bitcast i8* %temp25 to %class$A*
  ; assign
  store %class$A* %temp28, %class$A** %a
  ; deref
  %temp30 = load %class$A*, %class$A** %a
  ; method call
  %temp31 = icmp eq %class$A* %temp30, null
  br i1 %temp31, label %Label8, label %Label9
  Label8:
  call void @t_rt_null_reference_error(i32 10)
  br label %Label9
  Label9:
  %temp32 = getelementptr %class$A, %class$A* %temp30, i32 0, i32 0
  %temp33 = load i8*, i8** %temp32
  %temp34 = bitcast i8* %temp33 to [1 x i8*]*
  %temp35 = getelementptr [1 x i8*], [1 x i8*]* %temp34, i32 0, i32 3
  %temp36 = load i8*, i8** %temp35
  %temp37 = bitcast i8* %temp36 to %class$A* (%class$A*)*
  %temp38 = call %class$A* %temp37 (%class$A* %temp30)
  ; field access
  %temp40 = icmp eq %class$A* %temp38, null
  br i1 %temp40, label %Label10, label %Label11
  Label10:
  call void @t_rt_null_reference_error(i32 10)
  br label %Label11
  Label11:
  %temp39 = getelementptr %class$A, %class$A* %temp38, i32 0, i32 1
  ; deref
  %temp41 = load i32, i32* %temp39
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 %temp41)
  ret i32 0
}
