
/**
 * Est ce qu'on montre des tests ? 
 * => ca peut être pas mal pour montrer le DSL de spec 2 ?
 */


package empire

object CoteObscure_v1 {

  /**
   * Ici, on montre les conpets de base sur l'héritage / les objets :
   * - trait / class / object
   * - var, puis on remplace par val
   * - def
   * - visibilité (rapidement)
   * - type inférence 
   */
  
  /**
   * Ici: présenter les traits: interfaces, def pour méthodes, 
   * type déclaré après le "nom:". 
   */
  trait EmpireSoldier {
    def strength: Int
    def attack : Int
    def canUseTheForce : Boolean
  }
  
  //pas sûr de vouloir montrer ca ?
  trait TrainedSoldier extends EmpireSoldier {
    abstract override def attack = super.attack + 50
  }

  /**
   * Ici montrer:
   * - le constructeur par défaut, les constructeurs auxiliaires qui utilisent le constructeur principal
   * - les valeurs par défaut
   * - le fait que strength soit un attribut, que "val" donne la visibilité publique
   * - le fait que val override def
   * - la différence val / def
   * - var
   * - type Unit, le fait qu'on a toujours un type de retour
   */
  class StormTrooper(override val strength : Int = 10) extends EmpireSoldier {
    
    def this(strength: Int, bonus: Int) = this(strength+bonus)
    
    private var experience : Int = 0
    
    def attack = strength + experience
    
    def trains : Unit = {
      if(this.experience < 20) { this.experience = experience + 1 }
    }
    
    val canUseTheForce = false
  }

  val trooper = new StormTrooper(42) with TrainedSoldier
  
  while(trooper.attack < 50) {
    trooper.trains
  }

  
  trait IsASith {
    val canUseTheForce = true
  }
  
  trait TieFighterPilot {
    val canPilotTieFighter = true
  }

  class Sith(override val strength:Int) extends EmpireSoldier with IsASith {
    def attack = strength * 2
  }
  
  class RogueSith(strength:Int) extends IsASith

  object DarthVader extends Sith(200) with TieFighterPilot {
    override def attack = strength * 3
  }
  
}


object CoteObscure_v2 {

  /**
   * Data definition
   * 
   * Je pense qu'il y en a déjà trop !
   */
  
  trait Weapon {
    def attack : Int
    def wearing: Int
  }
  trait Ranged

  case class LightSaber(wearing:Int) extends Weapon {
    val attack = 50
  }
  case class BlasterGun(wearing:Int) extends Weapon with Ranged {
    val attack = 5
  }  
  case object DarthVaderLightSaber extends Weapon {
   val attack = 100
   val wearing = 1337
  }
  
 

  case class EmperorGuardSword(wearing:Int) extends Weapon {
    val attack = 20
  }  
  case class HeavyBlasterGun(wearing:Int) extends Weapon with Ranged {
   val attack = 10
  }
  
  def isRangeWeapon(weapon:Weapon) = weapon match {
    case BlasterGun(_) => true
    case LightSaber(_) => false
    case _:Ranged => true
    case _ => false
  }
  
  trait EmpireSoldier {
    def strength: Int
    def weapon: Weapon
    def canUseTheForce : Boolean
    
    final def attack : Int = {
      val bonus = { if(canUseTheForce) 50 else 0 }
      bonus + strength + weapon.attack
    }
    
  }
  
  trait IsASith {
    val canUseTheForce = true
    def useTheForce[T](power: Seq[T] => Seq[T], targets :Seq[T]) = power(targets)
  }
  
  trait IsNotASith {
    val canUseTheForce = false
  }

  case class StormTrooper(strength: Int, weapon: Weapon with Ranged) extends EmpireSoldier with IsNotASith
  case class EmperorGuard(strength: Int, weapon: EmperorGuardSword) extends EmpireSoldier with IsNotASith
  case class Sith(strength: Int, weapon: LightSaber) extends EmpireSoldier with IsASith
  case object DarthVader  extends EmpireSoldier with IsASith {
    val strength = 200
    val weapon = DarthVaderLightSaber
  }
  
  /**
   * Démonstration de quelques fonctions sur les collections, en particulier les fonctions
   * d'odre supérieur. 
   */
  
  val noviceTrooper = StormTrooper(strength = 5, BlasterGun(wearing = 0))
  val experiencedTrooper = StormTrooper(strength = 25, HeavyBlasterGun(wearing = 17))  
  val veteranRoyalGuard = EmperorGuard(strength = 50, EmperorGuardSword(wearing = 43))
  val sith = Sith(strength = 50, LightSaber(wearing = 26))
  
  //ici, préciser le type pour montrer les constructeurs de type
  val squad : Seq[EmpireSoldier] = Seq(noviceTrooper, veteranRoyalGuard,  DarthVader, experiencedTrooper)
  
  
  //pattern matching
  
  //pattern matching simple
  for(value <- Seq("42", 42, true) ) {
    value match {
      case "42"  => println("The string 42")
      case i:Int => println("An int: " + i.toString)
      case x     => println("Something else: " + x.toString)
    }
  }
  
  //pattern matching plus complexe (descend dans les case classes)
  def inMind(soldier:EmpireSoldier) ={
    val mind = soldier  match {
    case StormTrooper(_, BlasterGun(currentWearing)) => 
      "Lunchtime soon...only %d shots this morning".format(currentWearing)
    case Sith(strength,_) if strength > 50 => 
      "Stop reading my mind"
    case DarthVader => 
      "Where ... is .. Padme ?"
    case _:Sith => 
      "No one can read my mind!"
    case _ => "They are not the droid I was looking for"
  }
  println(mind)
  }
  
  val findWeapon: EmpireSoldier => Weapon = (soldier:EmpireSoldier) => soldier.weapon
  
  //ici, on cherche à connaitre la puissance d'attaque totale de notre équipe
  val squadAttack = squad.map { soldier => soldier.attack }.sum
  
  /*
   * On cherche à connaitre tous les soldats qui ont des armes abimés, 
   * c'est à dire avec un wearing de plus de 15. 
   */
  val soldierWithOldWeapons = squad.filter { soldier => soldier.weapon.wearing > 15 }
  
  /*
   * Les bons storm trooper peuvent être promus en garde impériaux. 
   * La condition est qu'ils doivent effectivement être un Storm Trooper, 
   * et avoir une force de plus de 20. 
   * Note: le type de la collection est plus précis que le type de départ. 
   */
  val promotableTroopers : Seq[StormTrooper] = squad.collect { case trooper @ StormTrooper(strength, weapon) if(strength > 20) => trooper }
  
  /*
   * Quelques fonctions représentant l'usage de la force par les sith
   * On met en avant l'aspect fonctionnel de Scala
   */
  def senseForceUser(soldiers :Seq[EmpireSoldier]):Seq[EmpireSoldier] =
   soldiers.filter(soldier => soldier.canUseTheForce) 

  def deflectLaser(soldiers :Seq[EmpireSoldier]):Seq[EmpireSoldier] = soldiers.map{
    _ match {
      case StormTrooper(_,w) if isRangeWeapon(w) => StormTrooper(0,BlasterGun(0))
      case s:Sith => s
    }
  }
  
  def scanSquad(squad :Seq[EmpireSoldier]) = squad.foreach(inMind(_))
  
  def senseForceUser2(soldiers:Seq[EmpireSoldier]) = for (
    soldier <- soldiers
    if soldier.canUseTheForce
  ) yield soldier
      
  
  /*
   * On veut se défendre contre des rebelles qui attaques à la fois de positions lointaine et au corps à corps.
   * Il faut donc séparer la squad en une équipe qui tirera sur les embusqués (armes de tir nécessaire) et une
   * équipe qui se battra au corp à corp (épées et sabres).
   */
  
  val teams = squad.groupBy { soldier => isRangeWeapon(soldier.weapon) }
  
  //d'autres méthodes sympa ?
}