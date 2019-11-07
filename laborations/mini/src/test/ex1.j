.class public ex1
.super java/lang/Object

.method public <init>()V
  .limit locals 1

  aload_0
  invokespecial java/lang/Object/<init>()V
  return

.end method

.method public static main([Ljava/lang/String;)V
  .limit locals 1000
  .limit stack  1000

  ldc 0
  invokestatic Runtime/print(I)V
  ldc 5
  istore 0
  iload 0
  invokestatic Runtime/print(I)V
  iload 0
  ldc 3
  iadd
  istore 0
  iload 0
  invokestatic Runtime/print(I)V
  return

.end method
