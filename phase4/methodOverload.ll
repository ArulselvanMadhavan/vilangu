; source file: methodOverload.t
; T version: 1.0
; compiled: Tue Nov 06 15:34:07 EST 2018

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
@A$VMT = global [4 x i8*] [i8* bitcast ([2 x i8*]* @Object$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$A*, i32)* @A$meth1$int$ to i8*), i8* bitcast (i32 (%class$A*, i32, i32)* @A$meth1$int$int$ to i8*)]
@B$VMT = global [5 x i8*] [i8* bitcast ([4 x i8*]* @A$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$A*, i32)* @A$meth1$int$ to i8*), i8* bitcast (i32 (%class$A*, i32, i32)* @A$meth1$int$int$ to i8*), i8* bitcast (i32 (%class$B*, i32, i32, i32)* @B$meth1$int$int$int$ to i8*)]
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
define i32 @A$meth1$int$(%class$A* %param0, i32 %param1) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  %i = alloca i32
  store i32 %param1, i32* %i
  ; deref
  %temp0 = load i32, i32* %i
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 %temp0)
  ret i32 0
}

; class A method meth1 line 3
define i32 @A$meth1$int$int$(%class$A* %param0, i32 %param1, i32 %param2) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  %i = alloca i32
  store i32 %param1, i32* %i
  %j = alloca i32
  store i32 %param2, i32* %j
  ; deref
  %temp1 = load i32, i32* %i
  ; deref
  %temp2 = load i32, i32* %j
  ; add
  %temp3 = add i32 %temp1, %temp2
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 %temp3)
  ret i32 0
}

; class B method meth1 line 7
define i32 @B$meth1$int$int$int$(%class$B* %param0, i32 %param1, i32 %param2, i32 %param3) {
  ; copy param temps into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  %i = alloca i32
  store i32 %param1, i32* %i
  %j = alloca i32
  store i32 %param2, i32* %j
  %k = alloca i32
  store i32 %param3, i32* %k
  ; deref
  %temp4 = load i32, i32* %i
  ; deref
  %temp5 = load i32, i32* %j
  ; add
  %temp6 = add i32 %temp4, %temp5
  ; deref
  %temp7 = load i32, i32* %k
  ; add
  %temp8 = add i32 %temp6, %temp7
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 %temp8)
  ret i32 0
}

; main block
define void @main() {
  call void @t_rt_alloc_init()
  %a = alloca i32
  store i32 0, i32* %a
  ; assign
  store i32 15, i32* %a
  %b = alloca %class$B*
  store %class$B* null, %class$B** %b
  %temp10 = getelementptr %class$B, %class$B* null, i32 1
  %temp11 = ptrtoint %class$B* %temp10 to i64
  %temp12 = call i8* @t_rt_alloc(i64 %temp11, i32 14)
  %temp13 = bitcast i8* %temp12 to [5 x i8*]**
  store [5 x i8*]* @B$VMT, [5 x i8*]** %temp13
  call void @defaultConstructor$B(i8* %temp12)
  %temp14 = bitcast i8* %temp12 to %class$B*
  ; assign
  store %class$B* %temp14, %class$B** %b
  ; deref
  %temp16 = load %class$B*, %class$B** %b
  ; method call
  %temp17 = bitcast %class$B* %temp16 to %class$A*
  %temp18 = icmp eq %class$A* %temp17, null
  br i1 %temp18, label %Label0, label %Label1
  Label0:
  call void @t_rt_null_reference_error(i32 15)
  br label %Label1
  Label1:
  %temp19 = getelementptr %class$A, %class$A* %temp17, i32 0, i32 0
  %temp20 = load i8*, i8** %temp19
  %temp21 = bitcast i8* %temp20 to [1 x i8*]*
  %temp22 = getelementptr [1 x i8*], [1 x i8*]* %temp21, i32 0, i32 2
  %temp23 = load i8*, i8** %temp22
  %temp24 = bitcast i8* %temp23 to i32 (%class$A*, i32)*
  %temp25 = call i32 %temp24 (%class$A* %temp17, i32 42)
  ; deref
  %temp26 = load %class$B*, %class$B** %b
  ; method call
  %temp27 = bitcast %class$B* %temp26 to %class$A*
  %temp28 = icmp eq %class$A* %temp27, null
  br i1 %temp28, label %Label2, label %Label3
  Label2:
  call void @t_rt_null_reference_error(i32 16)
  br label %Label3
  Label3:
  %temp29 = getelementptr %class$A, %class$A* %temp27, i32 0, i32 0
  %temp30 = load i8*, i8** %temp29
  %temp31 = bitcast i8* %temp30 to [1 x i8*]*
  %temp32 = getelementptr [1 x i8*], [1 x i8*]* %temp31, i32 0, i32 3
  %temp33 = load i8*, i8** %temp32
  %temp34 = bitcast i8* %temp33 to i32 (%class$A*, i32, i32)*
  %temp35 = call i32 %temp34 (%class$A* %temp27, i32 29, i32 13)
  ; deref
  %temp36 = load %class$B*, %class$B** %b
  ; deref
  %temp37 = load i32, i32* %a
  ; deref
  %temp38 = load i32, i32* %a
  ; add
  %temp39 = add i32 %temp38, 2
  ; method call
  %temp40 = icmp eq %class$B* %temp36, null
  br i1 %temp40, label %Label4, label %Label5
  Label4:
  call void @t_rt_null_reference_error(i32 17)
  br label %Label5
  Label5:
  %temp41 = getelementptr %class$B, %class$B* %temp36, i32 0, i32 0
  %temp42 = load i8*, i8** %temp41
  %temp43 = bitcast i8* %temp42 to [1 x i8*]*
  %temp44 = getelementptr [1 x i8*], [1 x i8*]* %temp43, i32 0, i32 4
  %temp45 = load i8*, i8** %temp44
  %temp46 = bitcast i8* %temp45 to i32 (%class$B*, i32, i32, i32)*
  %temp47 = call i32 %temp46 (%class$B* %temp36, i32 %temp37, i32 %temp39, i32 10)
  ret void
}
