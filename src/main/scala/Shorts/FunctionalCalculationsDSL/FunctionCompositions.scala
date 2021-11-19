package Shorts.FunctionalCalculationsDSL

import scala.language.implicitConversions

class FunctionCompositions[A,B] private(val f:A=>B)extends AnyVal{
  private[this] def composition[D,E,F](f1:D=>E)(f2:E=>F):D=>F = a=> f2(f1(a))
  private[this] def forkThread[D,E,F](f1:D=>E)(f2:E=>F):D=>E = composition(f1)((a:E)=> {
    new Thread(() => {
      f2(a)
    }).start();
    a
  })
  def >>=[C](f1:B=>C):A=>C = composition(f)(f1)
  def >>[C](f1: B=>Unit):A=>Unit = >>=(f1)
  def <<[C](f1:B=>C):A=>B = forkThread(f)(f1)
  def compose:A=>B = f
}

object FunctionCompositions{
  implicit def function2FunctionCompositions[A,B](f:A=>B):FunctionCompositions[A,B] = FunctionCompositions(f)
  implicit def FunctionCompositions2function[A,B](comp: FunctionCompositions[A,B]):A=>B = comp.compose
  def apply[A, B](function: A => B): FunctionCompositions[A, B] = new FunctionCompositions(function)
}