; ModuleID = 'Module'
source_filename = "Module"
target triple = "x86_64-pc-linux-gnu"

%Object_Vtable_type = type { %Object_Vtable_type*, i8* }
%i32arr_Vtable_type = type { %Object_Vtable_type*, i8* }
%i32arrarr_Vtable_type = type { %Object_Vtable_type*, i8* }
%Object = type { %Object_Vtable_type* }
%i32arr = type { %i32arr_Vtable_type*, i32*, i32 }
%i32arrarr = type { %i32arrarr_Vtable_type*, %i32arr*, i32 }

@printIntFormat = private constant [4 x i8] c"%d\0A\00", align 1
@castErrFormat = private constant [38 x i8] c"Narrow cast err: %s is not a type %s\0A\00", align 1
@outOfBoundsFormat = private constant [64 x i8] c"Array out of bounds exception at line %d. Index:%d | Length:%d\0A\00", align 1
@Object_class_name = private constant [7 x i8] c"Object\00", align 1
@Object_Vtable = global %Object_Vtable_type { %Object_Vtable_type* null, i8* getelementptr inbounds ([7 x i8], [7 x i8]* @Object_class_name, i32 0, i32 0) }
@i32arr_class_name = private constant [7 x i8] c"i32arr\00", align 1
@i32arr_Vtable = global %i32arr_Vtable_type { %Object_Vtable_type* @Object_Vtable, i8* getelementptr inbounds ([7 x i8], [7 x i8]* @i32arr_class_name, i32 0, i32 0) }
@i32arrarr_class_name = private constant [10 x i8] c"i32arrarr\00", align 1
@i32arrarr_Vtable = global %i32arrarr_Vtable_type { %Object_Vtable_type* @Object_Vtable, i8* getelementptr inbounds ([10 x i8], [10 x i8]* @i32arrarr_class_name, i32 0, i32 0) }

declare i32 @printf(i8*, ...)

declare i8* @malloc(i32)

declare i8* @calloc(i32, i32)

declare void @free(i8*)

declare void @exit(i32)

define i1 @Object_IsA(%Object* %0, %Object_Vtable_type* %1, i8* %2) {
entry:
  %object = alloca %Object*, align 8
  store %Object* %0, %Object** %object, align 8
  %vtbl = alloca %Object_Vtable_type*, align 8
  store %Object_Vtable_type* %1, %Object_Vtable_type** %vtbl, align 8
  %name = alloca i8*, align 8
  store i8* %2, i8** %name, align 8
  %3 = load %Object*, %Object** %object, align 8
  %4 = ptrtoint %Object* %3 to i64
  %equal = icmp eq i64 %4, 0
  %not = xor i1 %equal, true
  br i1 %not, label %then, label %else7

then:                                             ; preds = %entry
  %5 = load %Object*, %Object** %object, align 8
  %6 = getelementptr inbounds %Object, %Object* %5, i32 0, i32 0
  %7 = load %Object_Vtable_type*, %Object_Vtable_type** %6, align 8
  store %Object_Vtable_type* %7, %Object_Vtable_type** %vtbl, align 8
  br label %loopcond

loopcond:                                         ; preds = %ifcont, %then
  %8 = load %Object_Vtable_type*, %Object_Vtable_type** %vtbl, align 8
  %9 = ptrtoint %Object_Vtable_type* %8 to i64
  %equal1 = icmp eq i64 %9, 0
  %not2 = xor i1 %equal1, true
  br i1 %not2, label %loop, label %loopend

loop:                                             ; preds = %loopcond
  %10 = load %Object_Vtable_type*, %Object_Vtable_type** %vtbl, align 8
  %11 = getelementptr inbounds %Object_Vtable_type, %Object_Vtable_type* %10, i32 0, i32 1
  %12 = load i8*, i8** %11, align 8
  %13 = load i8*, i8** %name, align 8
  %14 = ptrtoint i8* %12 to i64
  %15 = ptrtoint i8* %13 to i64
  %equal3 = icmp eq i64 %14, %15
  br i1 %equal3, label %then4, label %else

then4:                                            ; preds = %loop
  br label %loopend

else:                                             ; preds = %loop
  %16 = load %Object_Vtable_type*, %Object_Vtable_type** %vtbl, align 8
  %17 = getelementptr inbounds %Object_Vtable_type, %Object_Vtable_type* %16, i32 0, i32 0
  %18 = load %Object_Vtable_type*, %Object_Vtable_type** %17, align 8
  store %Object_Vtable_type* %18, %Object_Vtable_type** %vtbl, align 8
  br label %ifcont

ifcont:                                           ; preds = %else
  br label %loopcond

loopend:                                          ; preds = %then4, %loopcond
  %19 = load %Object_Vtable_type*, %Object_Vtable_type** %vtbl, align 8
  %20 = ptrtoint %Object_Vtable_type* %19 to i64
  %equal5 = icmp eq i64 %20, 0
  %not6 = xor i1 %equal5, true
  br label %ifcont8

else7:                                            ; preds = %entry
  br label %ifcont8

ifcont8:                                          ; preds = %else7, %loopend
  %iftmp = phi i1 [ %not6, %loopend ], [ false, %else7 ]
  ret i1 %iftmp
}

define void @i32arr_Constructor(%i32arr* %0, %i32arr_Vtable_type* %1, i32* %2, i32 %3) {
entry:
  %this = alloca %i32arr*, align 8
  store %i32arr* %0, %i32arr** %this, align 8
  %vtable = alloca %i32arr_Vtable_type*, align 8
  store %i32arr_Vtable_type* %1, %i32arr_Vtable_type** %vtable, align 8
  %data = alloca i32*, align 8
  store i32* %2, i32** %data, align 8
  %length = alloca i32, align 4
  store i32 %3, i32* %length, align 4
  %4 = load %i32arr_Vtable_type*, %i32arr_Vtable_type** %vtable, align 8
  %5 = load %i32arr*, %i32arr** %this, align 8
  %6 = getelementptr inbounds %i32arr, %i32arr* %5, i32 0, i32 0
  store %i32arr_Vtable_type* %4, %i32arr_Vtable_type** %6, align 8
  %7 = load i32*, i32** %data, align 8
  %8 = load %i32arr*, %i32arr** %this, align 8
  %9 = getelementptr inbounds %i32arr, %i32arr* %8, i32 0, i32 1
  store i32* %7, i32** %9, align 8
  %10 = load i32, i32* %length, align 4
  %11 = load %i32arr*, %i32arr** %this, align 8
  %12 = getelementptr inbounds %i32arr, %i32arr* %11, i32 0, i32 2
  store i32 %10, i32* %12, align 4
  ret void
}

define void @i32arrarr_Constructor(%i32arrarr* %0, %i32arrarr_Vtable_type* %1, %i32arr* %2, i32 %3) {
entry:
  %this = alloca %i32arrarr*, align 8
  store %i32arrarr* %0, %i32arrarr** %this, align 8
  %vtable = alloca %i32arrarr_Vtable_type*, align 8
  store %i32arrarr_Vtable_type* %1, %i32arrarr_Vtable_type** %vtable, align 8
  %data = alloca %i32arr*, align 8
  store %i32arr* %2, %i32arr** %data, align 8
  %length = alloca i32, align 4
  store i32 %3, i32* %length, align 4
  %4 = load %i32arrarr_Vtable_type*, %i32arrarr_Vtable_type** %vtable, align 8
  %5 = load %i32arrarr*, %i32arrarr** %this, align 8
  %6 = getelementptr inbounds %i32arrarr, %i32arrarr* %5, i32 0, i32 0
  store %i32arrarr_Vtable_type* %4, %i32arrarr_Vtable_type** %6, align 8
  %7 = load %i32arr*, %i32arr** %data, align 8
  %8 = load %i32arrarr*, %i32arrarr** %this, align 8
  %9 = getelementptr inbounds %i32arrarr, %i32arrarr* %8, i32 0, i32 1
  store %i32arr* %7, %i32arr** %9, align 8
  %10 = load i32, i32* %length, align 4
  %11 = load %i32arrarr*, %i32arrarr** %this, align 8
  %12 = getelementptr inbounds %i32arrarr, %i32arrarr* %11, i32 0, i32 2
  store i32 %10, i32* %12, align 4
  ret void
}

define i32 @main() {
entry:
  %a = alloca %i32arrarr*, align 8
  store %i32arrarr* null, %i32arrarr** %a, align 8
  %newArrayResult = alloca %i32arrarr, align 8
  %0 = call dereferenceable_or_null(120) i8* @calloc(i32 5, i32 24)
  %1 = bitcast i8* %0 to %i32arr*
  call void @i32arrarr_Constructor(%i32arrarr* nonnull %newArrayResult, %i32arrarr_Vtable_type* nonnull @i32arrarr_Vtable, %i32arr* %1, i32 5)
  store %i32arrarr* %newArrayResult, %i32arrarr** %a, align 8
  %newArrayResult1 = alloca %i32arr, align 8
  %2 = call dereferenceable_or_null(16) i8* @calloc(i32 4, i32 4)
  %3 = bitcast i8* %2 to i32*
  call void @i32arr_Constructor(%i32arr* nonnull %newArrayResult1, %i32arr_Vtable_type* nonnull @i32arr_Vtable, i32* %3, i32 4)
  %4 = getelementptr inbounds %i32arrarr, %i32arrarr* %newArrayResult, i64 0, i32 2
  %5 = load i32, i32* %4, align 8
  %6 = icmp sgt i32 %5, 0
  br i1 %6, label %boundsCheckThen2, label %boundsCheckElse

boundsCheckThen2:                                 ; preds = %entry
  %7 = getelementptr inbounds %i32arrarr, %i32arrarr* %newArrayResult, i64 0, i32 1
  %baseArrPtr = load %i32arr*, %i32arr** %7, align 8
  br label %boundsCheckIfcont

boundsCheckElse:                                  ; preds = %entry
  %8 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([64 x i8], [64 x i8]* @outOfBoundsFormat, i64 0, i64 0), i32 4, i32 0, i32 %5)
  call void @exit(i32 -1)
  br label %boundsCheckIfcont

boundsCheckIfcont:                                ; preds = %boundsCheckElse, %boundsCheckThen2
  %loadRefTypeBeforeSave = load %i32arr, %i32arr* %newArrayResult1, align 8
  store %i32arr %loadRefTypeBeforeSave, %i32arr* %baseArrPtr, align 8
  %9 = load %i32arrarr*, %i32arrarr** %a, align 8
  %10 = getelementptr inbounds %i32arrarr, %i32arrarr* %9, i64 0, i32 2
  %11 = load i32, i32* %10, align 4
  %12 = icmp sgt i32 %11, 0
  br i1 %12, label %boundsCheckThen4, label %boundsCheckElse5

boundsCheckThen4:                                 ; preds = %boundsCheckIfcont
  %13 = load %i32arrarr*, %i32arrarr** %a, align 8
  %14 = getelementptr inbounds %i32arrarr, %i32arrarr* %13, i64 0, i32 1
  %baseArrPtr3 = load %i32arr*, %i32arr** %14, align 8
  br label %boundsCheckIfcont6

boundsCheckElse5:                                 ; preds = %boundsCheckIfcont
  %15 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([64 x i8], [64 x i8]* @outOfBoundsFormat, i64 0, i64 0), i32 5, i32 0, i32 %11)
  call void @exit(i32 -1)
  br label %boundsCheckIfcont6

boundsCheckIfcont6:                               ; preds = %boundsCheckElse5, %boundsCheckThen4
  %16 = getelementptr inbounds %i32arr, %i32arr* %baseArrPtr3, i64 0, i32 1
  %baseArrPtr7 = load i32*, i32** %16, align 8
  %17 = load %i32arrarr*, %i32arrarr** %a, align 8
  %18 = getelementptr inbounds %i32arrarr, %i32arrarr* %17, i64 0, i32 2
  %19 = load i32, i32* %18, align 4
  %20 = icmp sgt i32 %19, 0
  br i1 %20, label %boundsCheckThen9, label %boundsCheckElse10

boundsCheckThen9:                                 ; preds = %boundsCheckIfcont6
  %21 = load %i32arrarr*, %i32arrarr** %a, align 8
  %22 = getelementptr inbounds %i32arrarr, %i32arrarr* %21, i64 0, i32 1
  %baseArrPtr8 = load %i32arr*, %i32arr** %22, align 8
  br label %boundsCheckIfcont11

boundsCheckElse10:                                ; preds = %boundsCheckIfcont6
  %23 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([64 x i8], [64 x i8]* @outOfBoundsFormat, i64 0, i64 0), i32 5, i32 0, i32 %19)
  call void @exit(i32 -1)
  br label %boundsCheckIfcont11

boundsCheckIfcont11:                              ; preds = %boundsCheckElse10, %boundsCheckThen9
  %24 = getelementptr inbounds %i32arr, %i32arr* %baseArrPtr8, i64 0, i32 2
  %25 = load i32, i32* %24, align 4
  %26 = icmp sgt i32 %25, 0
  br i1 %26, label %boundsCheckThen12, label %boundsCheckElse13

boundsCheckThen12:                                ; preds = %boundsCheckIfcont11
  br label %boundsCheckIfcont14

boundsCheckElse13:                                ; preds = %boundsCheckIfcont11
  %27 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([64 x i8], [64 x i8]* @outOfBoundsFormat, i64 0, i64 0), i32 5, i32 0, i32 %25)
  call void @exit(i32 -1)
  br label %boundsCheckIfcont14

boundsCheckIfcont14:                              ; preds = %boundsCheckElse13, %boundsCheckThen12
  store i32 44, i32* %baseArrPtr7, align 4
  %28 = load %i32arrarr*, %i32arrarr** %a, align 8
  %29 = getelementptr inbounds %i32arrarr, %i32arrarr* %28, i64 0, i32 2
  %30 = load i32, i32* %29, align 4
  %sub = add i32 %30, -1
  %.not = icmp eq i32 %30, -2147483648
  br i1 %.not, label %boundsCheckElse17, label %boundsCheckThen16

boundsCheckThen16:                                ; preds = %boundsCheckIfcont14
  %31 = getelementptr inbounds %i32arrarr, %i32arrarr* %28, i64 0, i32 1
  %baseArrPtr15 = load %i32arr*, %i32arr** %31, align 8
  %32 = sext i32 %sub to i64
  br label %boundsCheckIfcont18

boundsCheckElse17:                                ; preds = %boundsCheckIfcont14
  %33 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([64 x i8], [64 x i8]* @outOfBoundsFormat, i64 0, i64 0), i32 6, i32 %sub, i32 %30)
  call void @exit(i32 -1)
  br label %boundsCheckIfcont18

boundsCheckIfcont18:                              ; preds = %boundsCheckElse17, %boundsCheckThen16
  %34 = getelementptr inbounds %i32arr, %i32arr* %baseArrPtr15, i64 %32, i32 1
  %baseArrPtr19 = load i32*, i32** %34, align 8
  %35 = load %i32arrarr*, %i32arrarr** %a, align 8
  %36 = getelementptr inbounds %i32arrarr, %i32arrarr* %35, i64 0, i32 2
  %37 = load i32, i32* %36, align 4
  %sub20 = add i32 %37, -1
  %.not28 = icmp eq i32 %37, -2147483648
  br i1 %.not28, label %boundsCheckElse23, label %boundsCheckThen22

boundsCheckThen22:                                ; preds = %boundsCheckIfcont18
  %38 = getelementptr inbounds %i32arrarr, %i32arrarr* %35, i64 0, i32 1
  %baseArrPtr21 = load %i32arr*, %i32arr** %38, align 8
  %39 = sext i32 %sub20 to i64
  br label %boundsCheckIfcont24

boundsCheckElse23:                                ; preds = %boundsCheckIfcont18
  %40 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([64 x i8], [64 x i8]* @outOfBoundsFormat, i64 0, i64 0), i32 6, i32 %sub20, i32 %37)
  call void @exit(i32 -1)
  br label %boundsCheckIfcont24

boundsCheckIfcont24:                              ; preds = %boundsCheckElse23, %boundsCheckThen22
  %41 = getelementptr inbounds %i32arr, %i32arr* %baseArrPtr21, i64 %39, i32 2
  %42 = load i32, i32* %41, align 4
  %43 = icmp sgt i32 %42, 0
  br i1 %43, label %boundsCheckThen25, label %boundsCheckElse26

boundsCheckThen25:                                ; preds = %boundsCheckIfcont24
  br label %boundsCheckIfcont27

boundsCheckElse26:                                ; preds = %boundsCheckIfcont24
  %44 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([64 x i8], [64 x i8]* @outOfBoundsFormat, i64 0, i64 0), i32 6, i32 0, i32 %42)
  call void @exit(i32 -1)
  br label %boundsCheckIfcont27

boundsCheckIfcont27:                              ; preds = %boundsCheckElse26, %boundsCheckThen25
  %45 = load i32, i32* %baseArrPtr19, align 4
  %46 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([4 x i8], [4 x i8]* @printIntFormat, i64 0, i64 0), i32 %45)
  ret i32 0
}
