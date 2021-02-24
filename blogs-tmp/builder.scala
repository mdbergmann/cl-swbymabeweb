package dungeon

import java.util.Random

case class Difficulty(value: Int)
object Difficulty {
  val NotDifficultAtAll = Difficulty(1)
  val VeryDifficult = Difficulty(10)
}
case class Monster(monsterType: Int, creepyFactor: Int)
object Monster {
  val JadeWarrior = Monster(1, 2)
  val Mercenary = Monster(2, 4)
  val BlackKnight = Monster(3, 4)
  val HugeSpider = Monster(4, 9)
  val Monsters = Vector(JadeWarrior, Mercenary, BlackKnight, HugeSpider)
}
case class SpecialItem(itemType: Int)
object SpecialItem {
  val RingBronze = SpecialItem(1)
  val LongswordBronze = SpecialItem(2)
  val LongbowBronze = SpecialItem(3)
  val RingSilver = SpecialItem(4)
  val LongswordSilver = SpecialItem(5)
  val LongbowSilver = SpecialItem(6)
  val HarkinsFlute = SpecialItem(7)
}

class Dungeon(private _kind: DungeonKind) {
  private var _difficulty: Difficulty = Difficulty.NotDifficultAtAll
  private var _monsters: List[Monster] = Nil
  private var _specialItems: List[SpecialItem] = Nil

  def difficulty: Difficulty = _difficulty
  private[dungeon]
  def difficulty_=(d: Difficulty): Unit = _difficulty = d

  def monsters: List[Monster] = _monsters.copy
  private[dungeon]
  def monsters_=(list: List[Monster]): Unit = _monsters = list.copy

  def specialItems: List[SpecialItem] = _specialItems.copy
  private[dungeon]
  def specialItems_=(list: List[SpecialItems]): Unit = _specialItems = list.copy
}

trait IDungeonBuilder {
  protected val theDungeon: Dungeon

  def setDifficulty(difficulty: Difficulty): IDungeonBuilder = {
    theDungeon.difficulty = difficulty
    this
  }
  def addMonsters(n: Int): IDungeonBuilder = {
    theDungeon.monsters = for(i <- 0 until n) yield Monster(new Random().nextInt(3), new Random().nextInt(10))
    this
  }
  def addSpecialItems(n: Int): IDungeonBuilder = {
    theDungeon.specialItems = for(i <- 0 until n) yield SpecialItem(new Random().nextInt(7))
    this
  }
  def get: Dungeon = theDungeon
}

class CastleDungeonBuilder extends IDungeonBuilder {
  override protected val theDungeon = new Dungeon(CastleDungeonKind)

  def addMonsters(n: Int): IDungeonBuilder = {
    // add nice monsters
    val filteredMonsters = Monster.Monsters.filter(m => m.creepyFactor < 5)
    theDungeon.monsters = (0 until n).map(filteredMonsters(new Random().nextInt(filteredMonsters.size)))
    this
  }
}

class CellarDungeonBuilder {
  override protected val theDungeon = new Dungeon(CellarDungeonKind)

  def addMonsters(n: Int): IDungeonBuilder = {
    // add creepy monsters
    val filteredMonsters = Monster.Monsters.filter(m => m.creepyFactor >= 5)
    theDungeon.monsters = (0 until n).map(filteredMonsters(new Random().nextInt(filteredMonsters.size)))
    this
  }
}

object Dungeon {
  val dungeon = new CastleDungeonBuilder()
    .setDifficulty(VeryDifficult)
    .addMonsters(15)
    .addSpecialItems(5)
    .get()
}
