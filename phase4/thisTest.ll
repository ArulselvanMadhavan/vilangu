; source file: thisTest.t
; T version: 1.0
; compiled: Sun Nov 18 21:36:53 EST 2018

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
@A$VMT = global [4 x i8*] [i8* bitcast ([2 x i8*]* @Object$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$A*, %class$B*)* @A$meth1$B$ to i8*), i8* bitcast (i32 (%class$A*, i32)* @A$meth2$int$ to i8*)]
@B$VMT = global [3 x i8*] [i8* bitcast ([2 x i8*]* @Object$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$B*, %class$A*)* @B$meth2$A$ to i8*)]
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
  call void @defaultConstructor$Object(i8* %ths)
  ret void
}
define void @defaultDestructor$B(i8* %ths) {
  call void @defaultDestructor$Object(i8* %ths)
  ret void
}
define void @defaultConstructor$Object(i8* %ths) {
  ret void
}
define void @defaultDestructor$Object(i8* %ths) {
  ret void
}

; class A method meth1 line 3
define i32 @A$meth1$B$(%class$A* %param0, %class$B* %param1) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  %b = alloca %class$B*
  store %class$B* %param1, %class$B** %b
  %temp0 = load %class$A*, %class$A** %this
  ; field access
  %temp2 = icmp eq %class$A* %temp0, null
  br i1 %temp2, label %Label0, label %Label1
  Label0:
  call void @t_rt_null_reference_error(i32 4)
  br label %Label1
  Label1:
  %temp1 = getelementptr %class$A, %class$A* %temp0, i32 0, i32 1
  ; assign
  store i32 17, i32* %temp1
  %temp4 = getelementptr %class$B, %class$B* null, i32 1
  %temp5 = ptrtoint %class$B* %temp4 to i64
  %temp6 = call i8* @t_rt_alloc(i64 %temp5, i32 5)
  %temp7 = bitcast i8* %temp6 to [3 x i8*]**
  store [3 x i8*]* @B$VMT, [3 x i8*]** %temp7
  call void @defaultConstructor$B(i8* %temp6)
  %temp8 = bitcast i8* %temp6 to %class$B*
  ; assign
  store %class$B* %temp8, %class$B** %b
  ; deref
  %temp10 = load %class$B*, %class$B** %b
  ; field access
  %temp12 = icmp eq %class$B* %temp10, null
  br i1 %temp12, label %Label2, label %Label3
  Label2:
  call void @t_rt_null_reference_error(i32 6)
  br label %Label3
  Label3:
  %temp11 = getelementptr %class$B, %class$B* %temp10, i32 0, i32 1
  ; assign
  store i32 19, i32* %temp11
  ; deref
  %temp14 = load %class$B*, %class$B** %b
  %temp15 = load %class$A*, %class$A** %this
  ; method call
  %temp16 = icmp eq %class$B* %temp14, null
  br i1 %temp16, label %Label4, label %Label5
  Label4:
  call void @t_rt_null_reference_error(i32 7)
  br label %Label5
  Label5:
  %temp17 = getelementptr %class$B, %class$B* %temp14, i32 0, i32 0
  %temp18 = load i8*, i8** %temp17
  %temp19 = bitcast i8* %temp18 to [1 x i8*]*
  %temp20 = getelementptr [1 x i8*], [1 x i8*]* %temp19, i32 0, i32 2
  %temp21 = load i8*, i8** %temp20
  %temp22 = bitcast i8* %temp21 to i32 (%class$B*, %class$A*)*
  %temp23 = call i32 %temp22 (%class$B* %temp14, %class$A* %temp15)
  ret i32 0
}

; class A method meth2 line 9
define i32 @A$meth2$int$(%class$A* %param0, i32 %param1) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  %i = alloca i32
  store i32 %param1, i32* %i
  %temp24 = load %class$A*, %class$A** %this
  ; field access
  %temp26 = icmp eq %class$A* %temp24, null
  br i1 %temp26, label %Label6, label %Label7
  Label6:
  call void @t_rt_null_reference_error(i32 10)
  br label %Label7
  Label7:
  %temp25 = getelementptr %class$A, %class$A* %temp24, i32 0, i32 1
  ; deref
  %temp27 = load i32, i32* %i
  ; assign
  store i32 %temp27, i32* %temp25
  ret i32 0
}

; class B method meth2 line 25
define i32 @B$meth2$A$(%class$B* %param0, %class$A* %param1) {
  ; copy param temps into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  %a = alloca %class$A*
  store %class$A* %param1, %class$A** %a
  ; deref
  %temp29 = load %class$A*, %class$A** %a
  ; field access
  %temp31 = icmp eq %class$A* %temp29, null
  br i1 %temp31, label %Label8, label %Label9
  Label8:
  call void @t_rt_null_reference_error(i32 26)
  br label %Label9
  Label9:
  %temp30 = getelementptr %class$A, %class$A* %temp29, i32 0, i32 1
  ; deref
  %temp32 = load i32, i32* %temp30
  %temp33 = load %class$B*, %class$B** %this
  ; field access
  %temp35 = icmp eq %class$B* %temp33, null
  br i1 %temp35, label %Label10, label %Label11
  Label10:
  call void @t_rt_null_reference_error(i32 26)
  br label %Label11
  Label11:
  %temp34 = getelementptr %class$B, %class$B* %temp33, i32 0, i32 1
  ; deref
  %temp36 = load i32, i32* %temp34
  ; add
  %temp37 = add i32 %temp32, %temp36
  ; add
  %temp38 = add i32 %temp37, 6
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 %temp38)
  ret i32 0
}

; main block
define i32 @main() {
  call void @t_rt_alloc_init()
  %a = alloca %class$A*
  store %class$A* null, %class$A** %a
  %b = alloca %class$B*
  store %class$B* null, %class$B** %b
  %temp39 = getelementptr %class$A, %class$A* null, i32 1
  %temp40 = ptrtoint %class$A* %temp39 to i64
  %temp41 = call i8* @t_rt_alloc(i64 %temp40, i32 17)
  %temp42 = bitcast i8* %temp41 to [4 x i8*]**
  store [4 x i8*]* @A$VMT, [4 x i8*]** %temp42
  call void @defaultConstructor$A(i8* %temp41)
  %temp43 = bitcast i8* %temp41 to %class$A*
  ; assign
  store %class$A* %temp43, %class$A** %a
  ; deref
  %temp45 = load %class$A*, %class$A** %a
  ; deref
  %temp46 = load %class$B*, %class$B** %b
  ; method call
  %temp47 = icmp eq %class$A* %temp45, null
  br i1 %temp47, label %Label12, label %Label13
  Label12:
  call void @t_rt_null_reference_error(i32 18)
  br label %Label13
  Label13:
  %temp48 = getelementptr %class$A, %class$A* %temp45, i32 0, i32 0
  %temp49 = load i8*, i8** %temp48
  %temp50 = bitcast i8* %temp49 to [1 x i8*]*
  %temp51 = getelementptr [1 x i8*], [1 x i8*]* %temp50, i32 0, i32 2
  %temp52 = load i8*, i8** %temp51
  %temp53 = bitcast i8* %temp52 to i32 (%class$A*, %class$B*)*
  %temp54 = call i32 %temp53 (%class$A* %temp45, %class$B* %temp46)
  ; deref
  %temp55 = load %class$A*, %class$A** %a
  ; method call
  %temp56 = icmp eq %class$A* %temp55, null
  br i1 %temp56, label %Label14, label %Label15
  Label14:
  call void @t_rt_null_reference_error(i32 19)
  br label %Label15
  Label15:
  %temp57 = getelementptr %class$A, %class$A* %temp55, i32 0, i32 0
  %temp58 = load i8*, i8** %temp57
  %temp59 = bitcast i8* %temp58 to [1 x i8*]*
  %temp60 = getelementptr [1 x i8*], [1 x i8*]* %temp59, i32 0, i32 3
  %temp61 = load i8*, i8** %temp60
  %temp62 = bitcast i8* %temp61 to i32 (%class$A*, i32)*
  %temp63 = call i32 %temp62 (%class$A* %temp55, i32 39)
  ; deref
  %temp64 = load %class$A*, %class$A** %a
  ; field access
  %temp66 = icmp eq %class$A* %temp64, null
  br i1 %temp66, label %Label16, label %Label17
  Label16:
  call void @t_rt_null_reference_error(i32 20)
  br label %Label17
  Label17:
  %temp65 = getelementptr %class$A, %class$A* %temp64, i32 0, i32 1
  ; deref
  %temp67 = load i32, i32* %temp65
  ; add
  %temp68 = add i32 %temp67, 3
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 %temp68)
  ret i32 0
}
