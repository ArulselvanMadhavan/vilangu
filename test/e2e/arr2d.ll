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
  %col = alloca i32, align 4
  %j = alloca i32, align 4
  %i = alloca i32, align 4
  %a = alloca %i32arrarr*, align 8
  store %i32arrarr* null, %i32arrarr** %a, align 8
  store i32 0, i32* %i, align 4
  store i32 0, i32* %j, align 4
  store i32 0, i32* %col, align 4
  %newArrayResult = alloca %i32arrarr, align 8
  %0 = call dereferenceable_or_null(120) i8* @calloc(i32 5, i32 24)
  %1 = bitcast i8* %0 to %i32arr*
  call void @i32arrarr_Constructor(%i32arrarr* nonnull %newArrayResult, %i32arrarr_Vtable_type* nonnull @i32arrarr_Vtable, %i32arr* %1, i32 5)
  store %i32arrarr* %newArrayResult, %i32arrarr** %a, align 8
  store i32 5, i32* %i, align 4
  store i32 4, i32* %col, align 4
  br label %loopcond

loopcond:                                         ; preds = %loopend, %entry
  %2 = load i32, i32* %i, align 4
  %sgt = icmp sgt i32 %2, 0
  br i1 %sgt, label %loop, label %loopend31

loop:                                             ; preds = %loopcond
  %3 = load i32, i32* %i, align 4
  %sub = add i32 %3, -1
  store i32 %sub, i32* %i, align 4
  %4 = load i32, i32* %col, align 4
  store i32 %4, i32* %j, align 4
  %newArrayResult1 = alloca %i32arr, align 8
  %5 = call i8* @calloc(i32 %4, i32 4)
  %6 = bitcast i8* %5 to i32*
  call void @i32arr_Constructor(%i32arr* nonnull %newArrayResult1, %i32arr_Vtable_type* nonnull @i32arr_Vtable, i32* %6, i32 %4)
  %7 = load i32, i32* %i, align 4
  %8 = load %i32arrarr*, %i32arrarr** %a, align 8
  %9 = getelementptr inbounds %i32arrarr, %i32arrarr* %8, i64 0, i32 2
  %10 = load i32, i32* %9, align 4
  %11 = icmp slt i32 %7, %10
  br i1 %11, label %boundsCheckThen2, label %boundsCheckElse

boundsCheckThen2:                                 ; preds = %loop
  %12 = load %i32arrarr*, %i32arrarr** %a, align 8
  %13 = getelementptr inbounds %i32arrarr, %i32arrarr* %12, i64 0, i32 1
  %baseArrPtr = load %i32arr*, %i32arr** %13, align 8
  %14 = sext i32 %7 to i64
  %15 = getelementptr inbounds %i32arr, %i32arr* %baseArrPtr, i64 %14
  br label %boundsCheckIfcont

boundsCheckElse:                                  ; preds = %loop
  %16 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([64 x i8], [64 x i8]* @outOfBoundsFormat, i64 0, i64 0), i32 11, i32 %7, i32 %10)
  call void @exit(i32 -1)
  br label %boundsCheckIfcont

boundsCheckIfcont:                                ; preds = %boundsCheckElse, %boundsCheckThen2
  %loadRefTypeBeforeSave = load %i32arr, %i32arr* %newArrayResult1, align 8
  store %i32arr %loadRefTypeBeforeSave, %i32arr* %15, align 8
  br label %loopcond3

loopcond3:                                        ; preds = %boundsCheckIfcont30, %boundsCheckIfcont
  %17 = load i32, i32* %j, align 4
  %sgt4 = icmp sgt i32 %17, 0
  br i1 %sgt4, label %loop5, label %loopend

loop5:                                            ; preds = %loopcond3
  %18 = load i32, i32* %j, align 4
  %sub6 = add i32 %18, -1
  store i32 %sub6, i32* %j, align 4
  %19 = load i32, i32* %i, align 4
  %20 = load i32, i32* %col, align 4
  %mul = mul i32 %19, %20
  %add = add i32 %mul, %sub6
  %21 = load %i32arrarr*, %i32arrarr** %a, align 8
  %22 = getelementptr inbounds %i32arrarr, %i32arrarr* %21, i64 0, i32 2
  %23 = load i32, i32* %22, align 4
  %24 = icmp slt i32 %19, %23
  br i1 %24, label %boundsCheckThen8, label %boundsCheckElse9

boundsCheckThen8:                                 ; preds = %loop5
  %25 = load %i32arrarr*, %i32arrarr** %a, align 8
  %26 = getelementptr inbounds %i32arrarr, %i32arrarr* %25, i64 0, i32 1
  %baseArrPtr7 = load %i32arr*, %i32arr** %26, align 8
  %27 = sext i32 %19 to i64
  br label %boundsCheckIfcont10

boundsCheckElse9:                                 ; preds = %loop5
  %28 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([64 x i8], [64 x i8]* @outOfBoundsFormat, i64 0, i64 0), i32 14, i32 %19, i32 %23)
  call void @exit(i32 -1)
  br label %boundsCheckIfcont10

boundsCheckIfcont10:                              ; preds = %boundsCheckElse9, %boundsCheckThen8
  %29 = getelementptr inbounds %i32arr, %i32arr* %baseArrPtr7, i64 %27, i32 1
  %baseArrPtr11 = load i32*, i32** %29, align 8
  %30 = load i32, i32* %i, align 4
  %31 = load %i32arrarr*, %i32arrarr** %a, align 8
  %32 = getelementptr inbounds %i32arrarr, %i32arrarr* %31, i64 0, i32 2
  %33 = load i32, i32* %32, align 4
  %34 = icmp slt i32 %30, %33
  br i1 %34, label %boundsCheckThen13, label %boundsCheckElse14

boundsCheckThen13:                                ; preds = %boundsCheckIfcont10
  %35 = load %i32arrarr*, %i32arrarr** %a, align 8
  %36 = getelementptr inbounds %i32arrarr, %i32arrarr* %35, i64 0, i32 1
  %baseArrPtr12 = load %i32arr*, %i32arr** %36, align 8
  %37 = sext i32 %30 to i64
  br label %boundsCheckIfcont15

boundsCheckElse14:                                ; preds = %boundsCheckIfcont10
  %38 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([64 x i8], [64 x i8]* @outOfBoundsFormat, i64 0, i64 0), i32 14, i32 %30, i32 %33)
  call void @exit(i32 -1)
  br label %boundsCheckIfcont15

boundsCheckIfcont15:                              ; preds = %boundsCheckElse14, %boundsCheckThen13
  %39 = getelementptr inbounds %i32arr, %i32arr* %baseArrPtr12, i64 %37, i32 2
  %40 = load i32, i32* %39, align 4
  %41 = icmp slt i32 %sub6, %40
  br i1 %41, label %boundsCheckThen16, label %boundsCheckElse17

boundsCheckThen16:                                ; preds = %boundsCheckIfcont15
  %42 = sext i32 %sub6 to i64
  %43 = getelementptr inbounds i32, i32* %baseArrPtr11, i64 %42
  br label %boundsCheckIfcont18

boundsCheckElse17:                                ; preds = %boundsCheckIfcont15
  %44 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([64 x i8], [64 x i8]* @outOfBoundsFormat, i64 0, i64 0), i32 14, i32 %sub6, i32 %40)
  call void @exit(i32 -1)
  br label %boundsCheckIfcont18

boundsCheckIfcont18:                              ; preds = %boundsCheckElse17, %boundsCheckThen16
  store i32 %add, i32* %43, align 4
  %45 = load i32, i32* %j, align 4
  %46 = load i32, i32* %i, align 4
  %47 = load %i32arrarr*, %i32arrarr** %a, align 8
  %48 = getelementptr inbounds %i32arrarr, %i32arrarr* %47, i64 0, i32 2
  %49 = load i32, i32* %48, align 4
  %50 = icmp slt i32 %46, %49
  br i1 %50, label %boundsCheckThen20, label %boundsCheckElse21

boundsCheckThen20:                                ; preds = %boundsCheckIfcont18
  %51 = load %i32arrarr*, %i32arrarr** %a, align 8
  %52 = getelementptr inbounds %i32arrarr, %i32arrarr* %51, i64 0, i32 1
  %baseArrPtr19 = load %i32arr*, %i32arr** %52, align 8
  %53 = sext i32 %46 to i64
  br label %boundsCheckIfcont22

boundsCheckElse21:                                ; preds = %boundsCheckIfcont18
  %54 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([64 x i8], [64 x i8]* @outOfBoundsFormat, i64 0, i64 0), i32 15, i32 %46, i32 %49)
  call void @exit(i32 -1)
  br label %boundsCheckIfcont22

boundsCheckIfcont22:                              ; preds = %boundsCheckElse21, %boundsCheckThen20
  %55 = getelementptr inbounds %i32arr, %i32arr* %baseArrPtr19, i64 %53, i32 1
  %baseArrPtr23 = load i32*, i32** %55, align 8
  %56 = load i32, i32* %i, align 4
  %57 = load %i32arrarr*, %i32arrarr** %a, align 8
  %58 = getelementptr inbounds %i32arrarr, %i32arrarr* %57, i64 0, i32 2
  %59 = load i32, i32* %58, align 4
  %60 = icmp slt i32 %56, %59
  br i1 %60, label %boundsCheckThen25, label %boundsCheckElse26

boundsCheckThen25:                                ; preds = %boundsCheckIfcont22
  %61 = load %i32arrarr*, %i32arrarr** %a, align 8
  %62 = getelementptr inbounds %i32arrarr, %i32arrarr* %61, i64 0, i32 1
  %baseArrPtr24 = load %i32arr*, %i32arr** %62, align 8
  %63 = sext i32 %56 to i64
  br label %boundsCheckIfcont27

boundsCheckElse26:                                ; preds = %boundsCheckIfcont22
  %64 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([64 x i8], [64 x i8]* @outOfBoundsFormat, i64 0, i64 0), i32 15, i32 %56, i32 %59)
  call void @exit(i32 -1)
  br label %boundsCheckIfcont27

boundsCheckIfcont27:                              ; preds = %boundsCheckElse26, %boundsCheckThen25
  %65 = getelementptr inbounds %i32arr, %i32arr* %baseArrPtr24, i64 %63, i32 2
  %66 = load i32, i32* %65, align 4
  %67 = icmp slt i32 %45, %66
  br i1 %67, label %boundsCheckThen28, label %boundsCheckElse29

boundsCheckThen28:                                ; preds = %boundsCheckIfcont27
  %68 = sext i32 %45 to i64
  %69 = getelementptr inbounds i32, i32* %baseArrPtr23, i64 %68
  br label %boundsCheckIfcont30

boundsCheckElse29:                                ; preds = %boundsCheckIfcont27
  %70 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([64 x i8], [64 x i8]* @outOfBoundsFormat, i64 0, i64 0), i32 15, i32 %45, i32 %66)
  call void @exit(i32 -1)
  br label %boundsCheckIfcont30

boundsCheckIfcont30:                              ; preds = %boundsCheckElse29, %boundsCheckThen28
  %71 = load i32, i32* %69, align 4
  %72 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([4 x i8], [4 x i8]* @printIntFormat, i64 0, i64 0), i32 %71)
  br label %loopcond3

loopend:                                          ; preds = %loopcond3
  br label %loopcond

loopend31:                                        ; preds = %loopcond
  ret i32 0
}
