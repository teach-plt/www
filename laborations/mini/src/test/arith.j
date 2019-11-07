.class public arith
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

  ldc 1
  ldc 2
  ldc 3
  imul
  iadd
  ldc 4
  ldc 5
  imul
  iadd
  invokestatic Runtime/print(I)V
  return

.end method
