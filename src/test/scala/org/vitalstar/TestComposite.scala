package org.vitalstar

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._
import org.junit.runner.RunWith

import collection.mutable.ListBuffer

trait Valuable {
  def cost: Double
}

// We define a concept, which is behavior
trait FoodItemish {
  def name: String
  def sold: Boolean
  def consumed: Boolean = sold
  def calories: Double
  def addFoodItem(item: FoodItemish)
  def getFoodItems: List[FoodItemish]
  def price: Double
  def isSingle: Boolean
}

// This is the implementation
abstract class FoodItemImpl(_name: String ="", val _price: Double = 0.0) extends FoodItemish{
  val name: String = _name
  val items: ListBuffer[FoodItemish] = new ListBuffer[FoodItemish]()
  def addFoodItem(item: FoodItemish): Unit = items.append(item)
  def getFoodItems: List[FoodItemish] = items.toList
  def price: Double = {
    if (isSingle)
        _price
    else
      items.foldLeft(0.0){(price, i) => i.price + price}
  }
  override def toString: String = name
}

class Food(name:String, _price: Double) extends FoodItemImpl(name, _price) {
  def isSingle = true
  def discounted80: Double = discountPrice(0.8)
  def discountPrice(_discount: Double) = price * _discount
  def cost = 1

  def sold: Boolean = false
  def calories: Double = 0

  override def addFoodItem(item: FoodItemish): Unit = {}
  override def getFoodItems: List[FoodItemish] = List(this)

}

class Meal(name: String)  extends FoodItemImpl(name) {
  def isSingle = false
//  def discountPrice(_discount: Double) = a.discountPrice(_discount) + b.discountPrice(_discount) + c.discountPrice(_discount)
//  def discountPrice(_discount: Double) = price * _discount
//  def cost = a.cost + b.cost + c.cost

  def sold: Boolean = false
  def calories: Double = 0
  override def toString = name + " " + getFoodItems.toString
}


@RunWith(classOf[JUnitRunner])
class TestComposite extends FunSuite with BeforeAndAfterAll {

  test("Encapsulation") {
    val fry = new Food("fry",1.95)
    val burger = new Food("double double",3.40)
    val drink = new Food("soda", 1.5)
    val breakfast = new Meal("snack")
    breakfast.addFoodItem(fry)
    breakfast.addFoodItem(burger)
    breakfast.addFoodItem(drink)

    assertEquals(1.95, fry.price, 10e-5)
    assertEquals(1.56, fry.discounted80, 10e-5)
//    assertEquals(6.85, breakfast.price, 10e-5)

    assertEquals(1, fry.cost, 10e-5)
//    assertEquals(3, breakfast.cost, 10e-5)
  }

}
