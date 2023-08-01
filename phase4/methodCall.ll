; source file: methodCall.t
; T version: 1.0
; compiled: Tue Nov 06 15:33:06 EST 2018

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
@A$VMT = global [5 x i8*] [i8* bitcast ([2 x i8*]* @Object$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth1$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth2$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth3$ to i8*)]
@B$VMT = global [6 x i8*] [i8* bitcast ([5 x i8*]* @A$VMT to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth1$ to i8*), i8* bitcast (i32 (%class$B*)* @B$meth2$ to i8*), i8* bitcast (i32 (%class$A*)* @A$meth3$ to i8*), i8* bitcast (i32 (%class$B*)* @B$meth4$ to i8*)]
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
  call void @t_rt_print_int(i32 1)
  ret i32 0
}

; class A method meth2 line 3
define i32 @A$meth2$(%class$A* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 2)
  ret i32 0
}

; class A method meth3 line 4
define i32 @A$meth3$(%class$A* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 3)
  ret i32 0
}

; class B method meth2 line 26
define i32 @B$meth2$(%class$B* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 22)
  ret i32 0
}

; class B method meth4 line 27
define i32 @B$meth4$(%class$B* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 44)
  ret i32 0
}

; main block
define void @main() {
  call void @t_rt_alloc_init()
  %a = alloca %class$A*
  store %class$A* null, %class$A** %a
  %temp0 = getelementptr %class$A, %class$A* null, i32 1
  %temp1 = ptrtoint %class$A* %temp0 to i64
  %temp2 = call i8* @t_rt_alloc(i64 %temp1, i32 9)
  %temp3 = bitcast i8* %temp2 to [5 x i8*]**
  store [5 x i8*]* @A$VMT, [5 x i8*]** %temp3
  call void @defaultConstructor$A(i8* %temp2)
  %temp4 = bitcast i8* %temp2 to %class$A*
  ; assign
  store %class$A* %temp4, %class$A** %a
  ; deref
  %temp6 = load %class$A*, %class$A** %a
  ; method call
  %temp7 = icmp eq %class$A* %temp6, null
  br i1 %temp7, label %Label0, label %Label1
  Label0:
  call void @t_rt_null_reference_error(i32 10)
  br label %Label1
  Label1:
  %temp8 = getelementptr %class$A, %class$A* %temp6, i32 0, i32 0
  %temp9 = load i8*, i8** %temp8
  %temp10 = bitcast i8* %temp9 to [1 x i8*]*
  %temp11 = getelementptr [1 x i8*], [1 x i8*]* %temp10, i32 0, i32 2
  %temp12 = load i8*, i8** %temp11
  %temp13 = bitcast i8* %temp12 to i32 (%class$A*)*
  %temp14 = call i32 %temp13 (%class$A* %temp6)
  ; deref
  %temp15 = load %class$A*, %class$A** %a
  ; method call
  %temp16 = icmp eq %class$A* %temp15, null
  br i1 %temp16, label %Label2, label %Label3
  Label2:
  call void @t_rt_null_reference_error(i32 11)
  br label %Label3
  Label3:
  %temp17 = getelementptr %class$A, %class$A* %temp15, i32 0, i32 0
  %temp18 = load i8*, i8** %temp17
  %temp19 = bitcast i8* %temp18 to [1 x i8*]*
  %temp20 = getelementptr [1 x i8*], [1 x i8*]* %temp19, i32 0, i32 3
  %temp21 = load i8*, i8** %temp20
  %temp22 = bitcast i8* %temp21 to i32 (%class$A*)*
  %temp23 = call i32 %temp22 (%class$A* %temp15)
  ; deref
  %temp24 = load %class$A*, %class$A** %a
  ; method call
  %temp25 = icmp eq %class$A* %temp24, null
  br i1 %temp25, label %Label4, label %Label5
  Label4:
  call void @t_rt_null_reference_error(i32 12)
  br label %Label5
  Label5:
  %temp26 = getelementptr %class$A, %class$A* %temp24, i32 0, i32 0
  %temp27 = load i8*, i8** %temp26
  %temp28 = bitcast i8* %temp27 to [1 x i8*]*
  %temp29 = getelementptr [1 x i8*], [1 x i8*]* %temp28, i32 0, i32 4
  %temp30 = load i8*, i8** %temp29
  %temp31 = bitcast i8* %temp30 to i32 (%class$A*)*
  %temp32 = call i32 %temp31 (%class$A* %temp24)
  %temp33 = getelementptr %class$B, %class$B* null, i32 1
  %temp34 = ptrtoint %class$B* %temp33 to i64
  %temp35 = call i8* @t_rt_alloc(i64 %temp34, i32 13)
  %temp36 = bitcast i8* %temp35 to [6 x i8*]**
  store [6 x i8*]* @B$VMT, [6 x i8*]** %temp36
  call void @defaultConstructor$B(i8* %temp35)
  %temp37 = bitcast i8* %temp35 to %class$B*
  ; assign
  %temp39 = bitcast %class$B* %temp37 to %class$A*
  store %class$A* %temp39, %class$A** %a
  ; deref
  %temp40 = load %class$A*, %class$A** %a
  ; method call
  %temp41 = icmp eq %class$A* %temp40, null
  br i1 %temp41, label %Label6, label %Label7
  Label6:
  call void @t_rt_null_reference_error(i32 14)
  br label %Label7
  Label7:
  %temp42 = getelementptr %class$A, %class$A* %temp40, i32 0, i32 0
  %temp43 = load i8*, i8** %temp42
  %temp44 = bitcast i8* %temp43 to [1 x i8*]*
  %temp45 = getelementptr [1 x i8*], [1 x i8*]* %temp44, i32 0, i32 2
  %temp46 = load i8*, i8** %temp45
  %temp47 = bitcast i8* %temp46 to i32 (%class$A*)*
  %temp48 = call i32 %temp47 (%class$A* %temp40)
  ; deref
  %temp49 = load %class$A*, %class$A** %a
  ; method call
  %temp50 = icmp eq %class$A* %temp49, null
  br i1 %temp50, label %Label8, label %Label9
  Label8:
  call void @t_rt_null_reference_error(i32 15)
  br label %Label9
  Label9:
  %temp51 = getelementptr %class$A, %class$A* %temp49, i32 0, i32 0
  %temp52 = load i8*, i8** %temp51
  %temp53 = bitcast i8* %temp52 to [1 x i8*]*
  %temp54 = getelementptr [1 x i8*], [1 x i8*]* %temp53, i32 0, i32 3
  %temp55 = load i8*, i8** %temp54
  %temp56 = bitcast i8* %temp55 to i32 (%class$A*)*
  %temp57 = call i32 %temp56 (%class$A* %temp49)
  ; deref
  %temp58 = load %class$A*, %class$A** %a
  ; method call
  %temp59 = icmp eq %class$A* %temp58, null
  br i1 %temp59, label %Label10, label %Label11
  Label10:
  call void @t_rt_null_reference_error(i32 16)
  br label %Label11
  Label11:
  %temp60 = getelementptr %class$A, %class$A* %temp58, i32 0, i32 0
  %temp61 = load i8*, i8** %temp60
  %temp62 = bitcast i8* %temp61 to [1 x i8*]*
  %temp63 = getelementptr [1 x i8*], [1 x i8*]* %temp62, i32 0, i32 4
  %temp64 = load i8*, i8** %temp63
  %temp65 = bitcast i8* %temp64 to i32 (%class$A*)*
  %temp66 = call i32 %temp65 (%class$A* %temp58)
  %b = alloca %class$B*
  store %class$B* null, %class$B** %b
  %temp67 = getelementptr %class$B, %class$B* null, i32 1
  %temp68 = ptrtoint %class$B* %temp67 to i64
  %temp69 = call i8* @t_rt_alloc(i64 %temp68, i32 18)
  %temp70 = bitcast i8* %temp69 to [6 x i8*]**
  store [6 x i8*]* @B$VMT, [6 x i8*]** %temp70
  call void @defaultConstructor$B(i8* %temp69)
  %temp71 = bitcast i8* %temp69 to %class$B*
  ; assign
  store %class$B* %temp71, %class$B** %b
  ; deref
  %temp73 = load %class$B*, %class$B** %b
  ; method call
  %temp74 = bitcast %class$B* %temp73 to %class$A*
  %temp75 = icmp eq %class$A* %temp74, null
  br i1 %temp75, label %Label12, label %Label13
  Label12:
  call void @t_rt_null_reference_error(i32 19)
  br label %Label13
  Label13:
  %temp76 = getelementptr %class$A, %class$A* %temp74, i32 0, i32 0
  %temp77 = load i8*, i8** %temp76
  %temp78 = bitcast i8* %temp77 to [1 x i8*]*
  %temp79 = getelementptr [1 x i8*], [1 x i8*]* %temp78, i32 0, i32 2
  %temp80 = load i8*, i8** %temp79
  %temp81 = bitcast i8* %temp80 to i32 (%class$A*)*
  %temp82 = call i32 %temp81 (%class$A* %temp74)
  ; deref
  %temp83 = load %class$B*, %class$B** %b
  ; method call
  %temp84 = icmp eq %class$B* %temp83, null
  br i1 %temp84, label %Label14, label %Label15
  Label14:
  call void @t_rt_null_reference_error(i32 20)
  br label %Label15
  Label15:
  %temp85 = getelementptr %class$B, %class$B* %temp83, i32 0, i32 0
  %temp86 = load i8*, i8** %temp85
  %temp87 = bitcast i8* %temp86 to [1 x i8*]*
  %temp88 = getelementptr [1 x i8*], [1 x i8*]* %temp87, i32 0, i32 3
  %temp89 = load i8*, i8** %temp88
  %temp90 = bitcast i8* %temp89 to i32 (%class$B*)*
  %temp91 = call i32 %temp90 (%class$B* %temp83)
  ; deref
  %temp92 = load %class$B*, %class$B** %b
  ; method call
  %temp93 = bitcast %class$B* %temp92 to %class$A*
  %temp94 = icmp eq %class$A* %temp93, null
  br i1 %temp94, label %Label16, label %Label17
  Label16:
  call void @t_rt_null_reference_error(i32 21)
  br label %Label17
  Label17:
  %temp95 = getelementptr %class$A, %class$A* %temp93, i32 0, i32 0
  %temp96 = load i8*, i8** %temp95
  %temp97 = bitcast i8* %temp96 to [1 x i8*]*
  %temp98 = getelementptr [1 x i8*], [1 x i8*]* %temp97, i32 0, i32 4
  %temp99 = load i8*, i8** %temp98
  %temp100 = bitcast i8* %temp99 to i32 (%class$A*)*
  %temp101 = call i32 %temp100 (%class$A* %temp93)
  ; deref
  %temp102 = load %class$B*, %class$B** %b
  ; method call
  %temp103 = icmp eq %class$B* %temp102, null
  br i1 %temp103, label %Label18, label %Label19
  Label18:
  call void @t_rt_null_reference_error(i32 22)
  br label %Label19
  Label19:
  %temp104 = getelementptr %class$B, %class$B* %temp102, i32 0, i32 0
  %temp105 = load i8*, i8** %temp104
  %temp106 = bitcast i8* %temp105 to [1 x i8*]*
  %temp107 = getelementptr [1 x i8*], [1 x i8*]* %temp106, i32 0, i32 5
  %temp108 = load i8*, i8** %temp107
  %temp109 = bitcast i8* %temp108 to i32 (%class$B*)*
  %temp110 = call i32 %temp109 (%class$B* %temp102)
  ret void
}
