; source file: phase4.t
; T version: 1.0
; compiled: Wed Dec 05 10:01:50 EST 2018

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
@A$VMT = global [5 x i8*] [i8* bitcast ([3 x i8*]* @Object$VMT to i8*), i8* bitcast ( void (%class$A*)*  @A$destructor$ to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$A*, i32, %class$B*)* @A$m1$int$B$ to i8*), i8* bitcast (i32 (%class$A*)* @A$m2$ to i8*)]
@B$VMT = global [6 x i8*] [i8* bitcast ([5 x i8*]* @A$VMT to i8*), i8* bitcast ( void (%class$B*)*  @B$destructor$ to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*), i8* bitcast (i32 (%class$A*, i32, %class$B*)* @A$m1$int$B$ to i8*), i8* bitcast (i32 (%class$B*)* @B$m2$ to i8*), i8* bitcast (i32 (%class$B*)* @B$m3$ to i8*)]
@C$VMT = global [3 x i8*] [i8* bitcast ([3 x i8*]* @Object$VMT to i8*), i8* bitcast ( void (%class$C*)*  @C$destructor$ to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]
@Object$VMT = global [3 x i8*] [i8* null, i8* bitcast ( void (%class$Object*)*  @Object$destructor$ to i8*), i8* bitcast (i32 (%class$Object*, %class$Object*)* @Object$equals$Object$ to i8*)]

; class object types
%class$A =  type { i8*, i32 }
%class$B =  type { i8*, i32 }
%class$C =  type { i8*, i32 }
%class$Object =  type { i8* }

; default constructors and destructors
define void @C$destructor$(%class$C* %ths) {
  %temp0 = bitcast %class$C* %ths to %class$Object*
  call void @Object$destructor$(%class$Object* %temp0)
  ret void
}
define void @Object$constructor$(%class$Object* %ths) {
  ret void
}
define void @Object$destructor$(%class$Object* %ths) {
  ret void
}

; class A constructor line 2
define void @A$constructor$int$(%class$A* %param0, i32 %param1) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  %f = alloca i32
  store i32 %param1, i32* %f
  %temp1 = load %class$A*, %class$A** %this
  %temp2 = bitcast %class$A* %temp1 to %class$Object*
  ; constructor invocation
  call void @Object$constructor$(%class$Object* %temp2)
  %temp3 = load %class$A*, %class$A** %this
  ; field access
  %temp5 = icmp eq %class$A* %temp3, null
  br i1 %temp5, label %Label0, label %Label1
  Label0:
  call void @t_rt_null_reference_error(i32 2)
  br label %Label1
  Label1:
  %temp4 = getelementptr %class$A, %class$A* %temp3, i32 0, i32 1
  ; deref
  %temp6 = load i32, i32* %f
  ; assign
  store i32 %temp6, i32* %temp4
  ret void
}

; class A destructor line 3
define void @A$destructor$ (%class$A* %param0) {
  ; copy param temp into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 9)
  %temp8 = bitcast %class$A* %param0 to %class$Object*
  call void @Object$destructor$(%class$Object* %temp8)
  ret void
}

; class A method m1 line 5
define i32 @A$m1$int$B$(%class$A* %param0, i32 %param1, %class$B* %param2) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  %i = alloca i32
  store i32 %param1, i32* %i
  %b = alloca %class$B*
  store %class$B* %param2, %class$B** %b
  ; return
  ; deref
  %temp9 = load i32, i32* %i
  %temp10 = load %class$A*, %class$A** %this
  ; field access
  %temp12 = icmp eq %class$A* %temp10, null
  br i1 %temp12, label %Label2, label %Label3
  Label2:
  call void @t_rt_null_reference_error(i32 6)
  br label %Label3
  Label3:
  %temp11 = getelementptr %class$A, %class$A* %temp10, i32 0, i32 1
  ; deref
  %temp13 = load i32, i32* %temp11
  ; add
  %temp14 = add i32 %temp9, %temp13
  ; deref
  %temp15 = load %class$B*, %class$B** %b
  ; field access
  %temp17 = icmp eq %class$B* %temp15, null
  br i1 %temp17, label %Label4, label %Label5
  Label4:
  call void @t_rt_null_reference_error(i32 6)
  br label %Label5
  Label5:
  %temp16 = getelementptr %class$B, %class$B* %temp15, i32 0, i32 1
  ; deref
  %temp18 = load i32, i32* %temp16
  ; add
  %temp19 = add i32 %temp14, %temp18
  ret i32 %temp19
  ret i32 0
}

; class A method m2 line 8
define i32 @A$m2$(%class$A* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$A*
  store %class$A* %param0, %class$A** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 17)
  ret i32 0
}

; class B constructor line 12
define void @B$constructor$int$(%class$B* %param0, i32 %param1) {
  ; copy param temps into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  %k = alloca i32
  store i32 %param1, i32* %k
  %temp20 = load %class$B*, %class$B** %this
  %temp21 = bitcast %class$B* %temp20 to %class$A*
  ; deref
  %temp22 = load i32, i32* %k
  ; constructor invocation
  call void @A$constructor$int$(%class$A* %temp21, i32 %temp22)
  %temp23 = load %class$B*, %class$B** %this
  ; field access
  %temp25 = icmp eq %class$B* %temp23, null
  br i1 %temp25, label %Label6, label %Label7
  Label6:
  call void @t_rt_null_reference_error(i32 12)
  br label %Label7
  Label7:
  %temp24 = getelementptr %class$B, %class$B* %temp23, i32 0, i32 1
  %temp26 = load %class$B*, %class$B** %this
  ; field access
  %temp28 = icmp eq %class$B* %temp26, null
  br i1 %temp28, label %Label8, label %Label9
  Label8:
  call void @t_rt_null_reference_error(i32 12)
  br label %Label9
  Label9:
  %temp27 = getelementptr %class$B, %class$B* %temp26, i32 0, i32 1
  ; deref
  %temp29 = load i32, i32* %temp27
  ; add
  %temp30 = add i32 %temp29, 3
  ; assign
  store i32 %temp30, i32* %temp24
  ret void
}

; class B destructor line 13
define void @B$destructor$ (%class$B* %param0) {
  ; copy param temp into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 33)
  %temp32 = bitcast %class$B* %param0 to %class$A*
  call void @A$destructor$(%class$A* %temp32)
  ret void
}

; class B method m2 line 14
define i32 @B$m2$(%class$B* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 199)
  ret i32 0
}

; class B method m3 line 15
define i32 @B$m3$(%class$B* %param0) {
  ; copy param temps into stack locations
  %this = alloca %class$B*
  store %class$B* %param0, %class$B** %this
  %temp33 = load %class$B*, %class$B** %this
  %temp34 = bitcast %class$B* %temp33 to %class$A*
  ; method call
  %temp35 = call i32 @A$m2$ (%class$A* %temp34)
  ret i32 0
}

; class C constructor line 20
define void @C$constructor$int$int$(%class$C* %param0, i32 %param1, i32 %param2) {
  ; copy param temps into stack locations
  %this = alloca %class$C*
  store %class$C* %param0, %class$C** %this
  %i = alloca i32
  store i32 %param1, i32* %i
  %j = alloca i32
  store i32 %param2, i32* %j
  %temp36 = load %class$C*, %class$C** %this
  ; deref
  %temp37 = load i32, i32* %i
  ; deref
  %temp38 = load i32, i32* %j
  ; add
  %temp39 = add i32 %temp37, %temp38
  ; constructor invocation
  call void @C$constructor$int$(%class$C* %temp36, i32 %temp39)
  ret void
}

; class C constructor line 21
define void @C$constructor$int$(%class$C* %param0, i32 %param1) {
  ; copy param temps into stack locations
  %this = alloca %class$C*
  store %class$C* %param0, %class$C** %this
  %i = alloca i32
  store i32 %param1, i32* %i
  %temp40 = load %class$C*, %class$C** %this
  %temp41 = bitcast %class$C* %temp40 to %class$Object*
  ; constructor invocation
  call void @Object$constructor$(%class$Object* %temp41)
  %temp42 = load %class$C*, %class$C** %this
  ; field access
  %temp44 = icmp eq %class$C* %temp42, null
  br i1 %temp44, label %Label10, label %Label11
  Label10:
  call void @t_rt_null_reference_error(i32 21)
  br label %Label11
  Label11:
  %temp43 = getelementptr %class$C, %class$C* %temp42, i32 0, i32 1
  ; deref
  %temp45 = load i32, i32* %i
  ; assign
  store i32 %temp45, i32* %temp43
  ret void
}

; main block
define i32 @main() {
  call void @t_rt_alloc_init()
  %a = alloca %class$A*
  store %class$A* null, %class$A** %a
  %b = alloca %class$B*
  store %class$B* null, %class$B** %b
  %temp47 = getelementptr %class$A, %class$A* null, i32 1
  %temp48 = ptrtoint %class$A* %temp47 to i64
  %temp49 = call i8* @t_rt_alloc(i64 %temp48, i32 27)
  %temp50 = bitcast i8* %temp49 to [5 x i8*]**
  store [5 x i8*]* @A$VMT, [5 x i8*]** %temp50
  %temp51 = bitcast i8* %temp49 to %class$A*
  call void @A$constructor$int$(%class$A* %temp51, i32 55)
  %temp52 = bitcast i8* %temp49 to %class$A*
  ; assign
  store %class$A* %temp52, %class$A** %a
  ; deref
  %temp54 = load %class$A*, %class$A** %a
  %temp55 = getelementptr %class$B, %class$B* null, i32 1
  %temp56 = ptrtoint %class$B* %temp55 to i64
  %temp57 = call i8* @t_rt_alloc(i64 %temp56, i32 28)
  %temp58 = bitcast i8* %temp57 to [6 x i8*]**
  store [6 x i8*]* @B$VMT, [6 x i8*]** %temp58
  %temp59 = bitcast i8* %temp57 to %class$B*
  call void @B$constructor$int$(%class$B* %temp59, i32 77)
  %temp60 = bitcast i8* %temp57 to %class$B*
  ; assign
  store %class$B* %temp60, %class$B** %b
  ; method call
  %temp62 = icmp eq %class$A* %temp54, null
  br i1 %temp62, label %Label12, label %Label13
  Label12:
  call void @t_rt_null_reference_error(i32 28)
  br label %Label13
  Label13:
  %temp63 = getelementptr %class$A, %class$A* %temp54, i32 0, i32 0
  %temp64 = load i8*, i8** %temp63
  %temp65 = bitcast i8* %temp64 to [1 x i8*]*
  %temp66 = getelementptr [1 x i8*], [1 x i8*]* %temp65, i32 0, i32 3
  %temp67 = load i8*, i8** %temp66
  %temp68 = bitcast i8* %temp67 to i32 (%class$A*, i32, %class$B*)*
  %temp69 = call i32 %temp68 (%class$A* %temp54, i32 3, %class$B* %temp60)
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 %temp69)
  ; deref
  %temp70 = load %class$B*, %class$B** %b
  ; method call
  %temp71 = icmp eq %class$B* %temp70, null
  br i1 %temp71, label %Label14, label %Label15
  Label14:
  call void @t_rt_null_reference_error(i32 29)
  br label %Label15
  Label15:
  %temp72 = getelementptr %class$B, %class$B* %temp70, i32 0, i32 0
  %temp73 = load i8*, i8** %temp72
  %temp74 = bitcast i8* %temp73 to [1 x i8*]*
  %temp75 = getelementptr [1 x i8*], [1 x i8*]* %temp74, i32 0, i32 5
  %temp76 = load i8*, i8** %temp75
  %temp77 = bitcast i8* %temp76 to i32 (%class$B*)*
  %temp78 = call i32 %temp77 (%class$B* %temp70)
  ; deref
  %temp79 = load %class$B*, %class$B** %b
  ; method call
  %temp80 = icmp eq %class$B* %temp79, null
  br i1 %temp80, label %Label16, label %Label17
  Label16:
  call void @t_rt_null_reference_error(i32 30)
  br label %Label17
  Label17:
  %temp81 = getelementptr %class$B, %class$B* %temp79, i32 0, i32 0
  %temp82 = load i8*, i8** %temp81
  %temp83 = bitcast i8* %temp82 to [1 x i8*]*
  %temp84 = getelementptr [1 x i8*], [1 x i8*]* %temp83, i32 0, i32 4
  %temp85 = load i8*, i8** %temp84
  %temp86 = bitcast i8* %temp85 to i32 (%class$B*)*
  %temp87 = call i32 %temp86 (%class$B* %temp79)
  ; deref
  %temp88 = load %class$A*, %class$A** %a
  ; delete
  %temp89 = getelementptr %class$A, %class$A* %temp88, i32 0, i32 0
  %temp90 = load i8*, i8** %temp89
  %temp91 = bitcast i8* %temp90 to [1 x i8*]*
  %temp92 = getelementptr [1 x i8*], [1 x i8*]* %temp91, i32 0, i32 1
  %temp93 = load i8*, i8** %temp92
  %temp94 = bitcast %class$A* %temp88 to i8*
  %temp95 = bitcast i8* %temp93 to void (i8*)*
  call void %temp95 (i8* %temp94)
  call void @t_rt_dealloc(i8* %temp94)
  ; deref
  %temp96 = load %class$B*, %class$B** %b
  ; assign
  %temp98 = bitcast %class$B* %temp96 to %class$A*
  store %class$A* %temp98, %class$A** %a
  ; deref
  %temp99 = load %class$A*, %class$A** %a
  ; method call
  %temp100 = icmp eq %class$A* %temp99, null
  br i1 %temp100, label %Label18, label %Label19
  Label18:
  call void @t_rt_null_reference_error(i32 33)
  br label %Label19
  Label19:
  %temp101 = getelementptr %class$A, %class$A* %temp99, i32 0, i32 0
  %temp102 = load i8*, i8** %temp101
  %temp103 = bitcast i8* %temp102 to [1 x i8*]*
  %temp104 = getelementptr [1 x i8*], [1 x i8*]* %temp103, i32 0, i32 4
  %temp105 = load i8*, i8** %temp104
  %temp106 = bitcast i8* %temp105 to i32 (%class$A*)*
  %temp107 = call i32 %temp106 (%class$A* %temp99)
  ; deref
  %temp108 = load %class$A*, %class$A** %a
  ; delete
  %temp109 = getelementptr %class$A, %class$A* %temp108, i32 0, i32 0
  %temp110 = load i8*, i8** %temp109
  %temp111 = bitcast i8* %temp110 to [1 x i8*]*
  %temp112 = getelementptr [1 x i8*], [1 x i8*]* %temp111, i32 0, i32 1
  %temp113 = load i8*, i8** %temp112
  %temp114 = bitcast %class$A* %temp108 to i8*
  %temp115 = bitcast i8* %temp113 to void (i8*)*
  call void %temp115 (i8* %temp114)
  call void @t_rt_dealloc(i8* %temp114)
  %c = alloca %class$C*
  store %class$C* null, %class$C** %c
  %temp116 = getelementptr %class$C, %class$C* null, i32 1
  %temp117 = ptrtoint %class$C* %temp116 to i64
  %temp118 = call i8* @t_rt_alloc(i64 %temp117, i32 36)
  %temp119 = bitcast i8* %temp118 to [3 x i8*]**
  store [3 x i8*]* @C$VMT, [3 x i8*]** %temp119
  %temp120 = bitcast i8* %temp118 to %class$C*
  call void @C$constructor$int$int$(%class$C* %temp120, i32 3, i32 8)
  %temp121 = bitcast i8* %temp118 to %class$C*
  ; assign
  store %class$C* %temp121, %class$C** %c
  ; deref
  %temp123 = load %class$C*, %class$C** %c
  ; field access
  %temp125 = icmp eq %class$C* %temp123, null
  br i1 %temp125, label %Label20, label %Label21
  Label20:
  call void @t_rt_null_reference_error(i32 37)
  br label %Label21
  Label21:
  %temp124 = getelementptr %class$C, %class$C* %temp123, i32 0, i32 1
  ; deref
  %temp126 = load i32, i32* %temp124
  ; call to runtime function to output a value
  call void @t_rt_print_int(i32 %temp126)
  ; deref
  %temp127 = load %class$C*, %class$C** %c
  ; delete
  %temp128 = getelementptr %class$C, %class$C* %temp127, i32 0, i32 0
  %temp129 = load i8*, i8** %temp128
  %temp130 = bitcast i8* %temp129 to [1 x i8*]*
  %temp131 = getelementptr [1 x i8*], [1 x i8*]* %temp130, i32 0, i32 1
  %temp132 = load i8*, i8** %temp131
  %temp133 = bitcast %class$C* %temp127 to i8*
  %temp134 = bitcast i8* %temp132 to void (i8*)*
  call void %temp134 (i8* %temp133)
  call void @t_rt_dealloc(i8* %temp133)
  ret i32 0
}
