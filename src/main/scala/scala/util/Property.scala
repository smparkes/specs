package scala.util

case class Property[T](init: T) {
  private var value: T = init
  private var setter: T => T = identity[T]
  private var toStringer: T => String = (v: T) => v.toString
  private var getter: T => T = identity[T]
  def apply(): T = getter(value)
  def update(newValue: T) = {value = setter(newValue); this}
  def get(newGetter: T => T) = {getter = newGetter; this}
  def set(newSetter: T => T) = {setter = newSetter; this}
  def toString(newToStringer: T => String) = {toStringer = newToStringer; this}
  override def toString = toStringer(value)
}
