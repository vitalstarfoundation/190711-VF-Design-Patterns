package org.vitalstar

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._
import org.junit.runner.RunWith

import collection.mutable.ListBuffer

// We define a concept, which is behavior
trait FoodItemish {
  def name: String = ""
  def sold: Boolean = false
  def consumed: Boolean = sold
  def calories: Double = 0.0
  def addFoodItem(item: FoodItemish) = {}
  def getFoodItems: List[FoodItemish] = List()
  def price: Double
  def isSingle: Boolean = true
}

// This is the implementation
abstract class FoodItemImpl(_name: String ="", val _price: Double = 0.0) extends FoodItemish {
  override val name: String = _name
  val items: ListBuffer[FoodItemish] = new ListBuffer[FoodItemish]()
  override def addFoodItem(item: FoodItemish): Unit = items.append(item)
  override def getFoodItems: List[FoodItemish] = items.toList
  override def price: Double = {
    if (isSingle)
        _price
    else
      items.foldLeft(0.0){(price: Double, i: FoodItemish) => i.price + price}
  }
  override def toString: String = name
}

class Toy extends FoodItemish {
  def price = 100
}

class Food(name:String, _price: Double) extends FoodItemImpl(name, _price) {
  override def addFoodItem(item: FoodItemish): Unit = {}
  override def getFoodItems: List[FoodItemish] = List(this)
}

class Meal(name: String)  extends FoodItemImpl(name) {
  override def isSingle = false
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

    val ironman = new Toy
    breakfast.addFoodItem(ironman)

    assertEquals(1.95, fry.price, 10e-5)
//    assertEquals(6.85, breakfast.price, 10e-5)

//    assertEquals(1, fry.cost, 10e-5)
//    assertEquals(3, breakfast.cost, 10e-5)
  }

}
