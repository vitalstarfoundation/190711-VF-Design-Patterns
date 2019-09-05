package org.vitalstar

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._
import org.junit.runner.RunWith

import collection.mutable.ListBuffer
// We define a concept, which is behavior
trait Item {
  def name: String = ""
  def sold: Boolean = false
  def consumed: Boolean = sold
  def calories: Double = 0.0
  def addItem(item: Item) = {}
  def getItems: List[Item] = List()
  def price: Double
  def isSingle: Boolean = true
}

// This is the implementation
class ItemBase(_name: String ="", val _price: Double = 0.0)
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
  override def toString: String = name
}

class Food(name:String, _price: Double) extends ItemBase(name, _price) {
  override def addItem(item: Item): Unit = {}
  override def getItems: List[Item] = List(this)
}

class Meal(name: String)  extends ItemBase(name) {
  override def isSingle = false
  override def toString = name + " " + getItems.toString
}

class Toy(name: String = "", _price: Double = 0.0)
  extends ItemBase(name, _price) {
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

@RunWith(classOf[JUnitRunner])
class TestComposite extends FunSuite with BeforeAndAfterAll {
  val earth = new World()

  // Create some items
  val fry = new Food("fry",1.95)
  val burger = new Food("double double",3.40)
  val drink = new Food("soda", 1.5)
  val breakfast = new Meal("snack")
  breakfast.addItem(fry)
  breakfast.addItem(burger)
  breakfast.addItem(drink)

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
    store.replenish(List.fill(10)(breakfast))

    val food = store.order("fry")
    assertEquals(39, store.numberOfItems)

    val meal = store.order("snack")
    assertEquals(38, store.numberOfItems)
  }

  test("singleton") {
    var aug = earth.getStore
    assertEquals(0, aug.numberOfItems)

    aug = earth.getStore
    aug.replenish(List.fill(10)(fry))
    assertEquals(10, aug.numberOfItems)

    aug = earth.getStore
    aug.replenish(List.fill(10)(burger))
    assertEquals(20, aug.numberOfItems)

    aug = earth.getStore
    aug.replenish(List.fill(10)(drink))
    assertEquals(30, aug.numberOfItems)

    aug = earth.getStore
    aug.replenish(List.fill(10)(breakfast))
    assertEquals(40, aug.numberOfItems)

    earth.getStore.order("fry")
    assertEquals(39, aug.numberOfItems)

  }
}
