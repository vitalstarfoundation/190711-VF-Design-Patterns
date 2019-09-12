package org.vitalstar

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._
import org.junit.runner.RunWith

import collection.mutable.ListBuffer
// We define a concept, which is behavior
trait Item {
  def name: String = ""
  def ingredient: String = ""
  def sold: Boolean = false
  def consumed: Boolean = sold
  def calories: Double = 0.0
  def addItem(item: Item) = {}
  def getItems: List[Item] = List()
  def price: Double
  def isSingle: Boolean = true
  def doYouLikeMe(customer: Customer): Boolean = false
}

// This is the implementation
class ItemBase(_name: String ="", _ingredient: String = "", val _price: Double = 0.0)
    extends Item {

  override val name: String = _name
  val bag: ListBuffer[Item] = new ListBuffer[Item]()

  override def addItem(item: Item): Unit = bag.append(item)
  override def getItems: List[Item] = bag.toList
  override def price: Double = {
    if (isSingle)
        _price
    else
      bag.foldLeft(0.0){(price: Double, i: Item) => i.price + price}
  }
  override def ingredient: String = {
    if (isSingle)
        _ingredient.trim()
    else
      bag.foldLeft(""){ (e1,e2) => e1 + " " + e2.ingredient}.trim()
  }

  override def doYouLikeMe(customer: Customer): Boolean = {
    customer.doYouLike(this)
  }

  override def toString: String = name
}

class Food(name:String, _ingredient: String = "", _price: Double) extends ItemBase(name, _ingredient, _price) {
  override def addItem(item: Item): Unit = {}
  override def getItems: List[Item] = List(this)
}

class Meal(name: String)  extends ItemBase(name) {
  override def isSingle = false
  override def toString = name + " " + getItems.toString
}

class Toy(name: String = "", _price: Double = 0.0)
  extends ItemBase(name, "plastics", _price) {
}

class Store(address: String) {
  val shelf: ListBuffer[Item] = new ListBuffer[Item]()
  def replenish(items: List[Item]): Unit = shelf.appendAll(items)
  def numberOfItems = shelf.length
  def order(name: String): Item = {
    val items = shelf.filter{e => e.name == name}
    if (!items.isEmpty) {
      val item = items.head
      shelf -= item
      item
    } else null
  }

  override def toString = shelf.length + " items in" + address
}

class World {
  private var aug: Store = null

  def getStore: Store = {
    if (aug == null) {
      aug = new Store("1500 Augustine drive")
    }
    aug
  }

  def reset = {
    aug = null
  }
}

trait Customer {
  def doYouLike(item: Item): Boolean = false
}

class CustomerBase(allergy: String = "") extends Customer {
  def isAllergic(item: Item): Boolean = {
    item.ingredient.contains(allergy)
  }

  def canYouAfford(item: Item): Boolean = {
    item.price < 5
  }

}

class Patient(allergy: String = "") extends CustomerBase(allergy) {
  override def doYouLike(item: Item): Boolean = {
    !isAllergic(item)
  }
}

class Kid(allergy: String = "") extends CustomerBase(allergy) {
  override def doYouLike(item: Item): Boolean = {
    canYouAfford(item)
  }
}

@RunWith(classOf[JUnitRunner])
class TestComposite extends FunSuite with BeforeAndAfterAll {
  val earth = new World()

  // Create some items
  val fry = new Food("fry","potatos",1.95)
  val burger = new Food("double double","beef bread condiment pickle tomato onion",3.40)
  val drink = new Food("soda", "sugar water co2",1.5)
  val ironman = new Toy("ironman",0)
  val breakfast = new Meal("snack")
  breakfast.addItem(fry)
  breakfast.addItem(burger)
  breakfast.addItem(drink)
  val john = new Patient("pickle")
  val jonathan = new Kid()
  val amour = new Patient("onion")

  test("Encapsulation") {
    val ironman = new Toy
    breakfast.addItem(ironman)

    assertEquals(1.95, fry.price, 10e-5)
    assertEquals(6.85, breakfast.price, 10e-5)
  }

  test("create store") {
    // Create a store
    val store = new Store("1500 Augustine drive")
    store.replenish(List.fill(10)(fry))
    store.replenish(List.fill(10)(burger))
    store.replenish(List.fill(10)(drink))
    store.replenish(List.fill(10)(ironman))
    store.replenish(List.fill(10)(breakfast))
    assertEquals(50, store.numberOfItems)

    val food = store.order("fry")
    assertEquals(49, store.numberOfItems)

    val meal = store.order("snack")
    assertEquals(48, store.numberOfItems)
  }

  test("singleton") {
    // demostrate the singleton
    var aug = earth.getStore
    assertEquals(0, aug.numberOfItems)

    aug = earth.getStore
    aug.replenish(List.fill(10)(fry))
    assertEquals(10, aug.numberOfItems)

    aug = earth.getStore
    aug.replenish(List.fill(10)(burger))
    assertEquals(20, aug.numberOfItems)

    // demonstrate the reset
    earth.reset
    aug = earth.getStore
    assertEquals(0, aug.numberOfItems)

    aug = earth.getStore
    aug.replenish(List.fill(10)(drink))
    assertEquals(10, aug.numberOfItems)

    aug = earth.getStore
    aug.replenish(List.fill(10)(breakfast))
    assertEquals(20, aug.numberOfItems)

    // demonstrate the order
    earth.getStore.order("soda")
    assertEquals(19, aug.numberOfItems)
  }

  test("ingredient") {
    val breakfast = new Meal("snack")
      breakfast.addItem(fry)
      breakfast.addItem(burger)
      breakfast.addItem(drink)

    assertEquals("potatos beef bread condiment pickle tomato onion sugar water co2", breakfast.ingredient)


  }
}
