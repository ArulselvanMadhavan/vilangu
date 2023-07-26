; source file: thisConstructorCall.t
; T version: 1.0
; compiled: Sun Nov 18 21:36:44 EST 2018

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
@Object$VMT = global [2 x i8*] [i8* null, i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]

; class object types
%class$A =  type { i8*, i32 }
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

; class A constructor line 2
define void @A$onstructor$int$(i8* %param0, i32 %param1) {
  ; copy param temps into stack locations
  %temp0 = bitcast i8* %param0 to %class$A*
  %this = alloca %class$A*
  store %class$A* %temp0, %class$A** %this
  %i = alloca i32
  store i32 %param1, i32* %i
  %temp1 = load %class$A*, %class$A** %this
  ; deref
  %temp2 = load i32, i32* %i
  ; deref
  %temp3 = load i32, i32* %i
  ; add
  %temp4 = add i32 %temp3, 5
  %temp5 = bitcast %class$A* %temp1 to i8*
  ; constructor invocation
  call void @A$onstructor$int$int$(i8* %temp5, i32 %temp2, i32 %temp4)
  ret void
}

; class A constructor line 3
define void @A$onstructor$int$int$(i8* %param0, i32 %param1, i32 %param2) {
  ; copy param temps into stack locations
  %temp6 = bitcast i8* %param0 to %class$A*
  %this = alloca %class$A*
  store %class$A* %temp6, %class$A** %this
  %i = alloca i32
  store i32 %param1, i32* %i
  %j = alloca i32
  store i32 %param2, i32* %j
  %temp7 = load %class$A*, %class$A** %this
  %temp8 = bitcast %class$A* %temp7 to %class$Object*
  %temp9 = bitcast %class$Object* %temp8 to i8*
  ; constructor invocation
  call void @defaultConstructor$Object(i8* %temp9)
  %temp10 = load %class$A*, %class$A** %this
  ; field access
  %temp12 = icmp eq %class$A* %temp10, null
  br i1 %temp12, label %Label0, label %Label1
  Label0:
  call void @t_rt_null_reference_error(i32 3)
  br label %Label1
  Label1:
  %temp11 = getelementptr %class$A, %class$A* %temp10, i32 0, i32 1
  ; deref
  %temp13 = load i32, i32* %i
  ; deref
  %temp14 = load i32, i32* %j
  ; add
  %temp15 = add i32 %temp13, %temp14
  ; assign
  store i32 %temp15, i32* %temp11
  ret void
}

; main block
define i32 @main() {
  call void @t_rt_alloc_init()
  %a = alloca %class$A*
  store %class$A* null, %class$A** %a
  %temp17 = getelementptr %class$A, %class$A* null, i32 1
  %temp18 = ptrtoint %class$A* %temp17 to i64
  %temp19 = call i8* @t_rt_alloc(i64 %temp18, i32 9)
  %temp20 = bitcast i8* %temp19 to [2 x i8*]**
  store [2 x i8*]* @A$VMT, [2 x i8*]** %temp20
  call void @A$onstructor$int$(i8* %temp19, i32 13)
  %temp21 = bitcast i8* %temp19 to %class$A*
  ; assign
  store %class$A* %temp21, %class$A** %a
  ; deref
  %temp23 = load %class$A*, %class$A** %a
  ; field access
  %temp25 = icmp eq %class$A* %temp23, null
  br i1 %temp25, label %Label2, label %Label3
  Label2:
  call void @t_rt_null_reference_error(i32 10)
  br label %Label3
  Label3:
  %temp24 = getelementptr %class$A, %class$A* %temp23, i32 0, i32 1
  ; deref
  %temp26 = load i32, i32* %temp24
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 %temp26)
  ret i32 0
}
