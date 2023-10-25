;; Boilerplate: a wrapping class for cc code

.class public good03
.super java/lang/Object

.method public <init>()V
   .limit locals 1

  aload_0
  invokespecial java/lang/Object/<init>()V
  return

.end method

;; The java-style main method calls the cc main

.method public static main([Ljava/lang/String;)V
  .limit locals 1
  .limit stack  1

  invokestatic good03/main()I
  pop
  return

.end method

;; Program

.method public static main()I
  .limit locals 3
  .limit stack  3

;; int arg = readInt();

invokestatic Runtime/readInt()I
istore_0

;; int ret = 1;

iconst_1
istore_1

;; int i = 1;

iconst_1
istore_2

;; while (i < arg + 1)

L0:            ;; // beginning of loop, check condition
iload_2        ;; i
iload_0
iconst_1
iadd           ;; arg + 1
if_icmplt L2   ;; test i < arg + 1
iconst_0
goto L3
L2:            ;; i < arg + 1 is true
iconst_1
L3:            ;; i < arg + 1 is false
iconst_0
if_icmpeq L1   ;; if last comparison was false, exit while loop

;; ret = i * ret

iload_2
iload_1
imul
istore_1
iload_1
pop

;; ++i

iload_2
iconst_1
iadd
istore_2  ;; // i = i + 1
iload_2
pop

;; // continue loop
goto L0

;; printInt(ret)

L1:
iload_1
invokestatic Runtime/printInt(I)V
nop

;; return 0

iconst_0
ireturn

.end method
