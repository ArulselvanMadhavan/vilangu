; source file: superConstructorCall.t
; T version: 1.0
; compiled: Sun Nov 18 21:36:03 EST 2018

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
%class$A =  type { i8*, i32 }
%class$B =  type { i8*, i32 }
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

; class A constructor line 2
define void @A$onstructor$int$int$(i8* %param0, i32 %param1, i32 %param2) {
  ; copy param temps into stack locations
  %temp0 = bitcast i8* %param0 to %class$A*
  %this = alloca %class$A*
  store %class$A* %temp0, %class$A** %this
  %i = alloca i32
  store i32 %param1, i32* %i
  %j = alloca i32
  store i32 %param2, i32* %j
  %temp1 = load %class$A*, %class$A** %this
  %temp2 = bitcast %class$A* %temp1 to %class$Object*
  %temp3 = bitcast %class$Object* %temp2 to i8*
  ; constructor invocation
  call void @defaultConstructor$Object(i8* %temp3)
  %temp4 = load %class$A*, %class$A** %this
  ; field access
  %temp6 = icmp eq %class$A* %temp4, null
  br i1 %temp6, label %Label0, label %Label1
  Label0:
  call void @t_rt_null_reference_error(i32 2)
  br label %Label1
  Label1:
  %temp5 = getelementptr %class$A, %class$A* %temp4, i32 0, i32 1
  ; deref
  %temp7 = load i32, i32* %i
  ; deref
  %temp8 = load i32, i32* %j
  ; add
  %temp9 = add i32 %temp7, %temp8
  ; assign
  store i32 %temp9, i32* %temp5
  ret void
}

; class B constructor line 7
define void @B$onstructor$int$(i8* %param0, i32 %param1) {
  ; copy param temps into stack locations
  %temp11 = bitcast i8* %param0 to %class$B*
  %this = alloca %class$B*
  store %class$B* %temp11, %class$B** %this
  %i = alloca i32
  store i32 %param1, i32* %i
  %temp12 = load %class$B*, %class$B** %this
  %temp13 = bitcast %class$B* %temp12 to %class$A*
  ; deref
  %temp14 = load i32, i32* %i
  ; deref
  %temp15 = load i32, i32* %i
  ; add
  %temp16 = add i32 %temp15, 5
  %temp17 = bitcast %class$A* %temp13 to i8*
  ; constructor invocation
  call void @A$onstructor$int$int$(i8* %temp17, i32 %temp14, i32 %temp16)
  ret void
}

; main block
define i32 @main() {
  call void @t_rt_alloc_init()
  %b = alloca %class$B*
  store %class$B* null, %class$B** %b
  %temp18 = getelementptr %class$B, %class$B* null, i32 1
  %temp19 = ptrtoint %class$B* %temp18 to i64
  %temp20 = call i8* @t_rt_alloc(i64 %temp19, i32 12)
  %temp21 = bitcast i8* %temp20 to [2 x i8*]**
  store [2 x i8*]* @B$VMT, [2 x i8*]** %temp21
  call void @B$onstructor$int$(i8* %temp20, i32 13)
  %temp22 = bitcast i8* %temp20 to %class$B*
  ; assign
  store %class$B* %temp22, %class$B** %b
  ; deref
  %temp24 = load %class$B*, %class$B** %b
  ; field access
  %temp26 = icmp eq %class$B* %temp24, null
  br i1 %temp26, label %Label2, label %Label3
  Label2:
  call void @t_rt_null_reference_error(i32 13)
  br label %Label3
  Label3:
  %temp25 = getelementptr %class$B, %class$B* %temp24, i32 0, i32 1
  ; deref
  %temp27 = load i32, i32* %temp25
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 %temp27)
  ret i32 0
}
