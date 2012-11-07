
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
   
  object DarthVader extends EmpireSoldier with IsASith {
    val strength = 200
    def attack = strength
  }
  
}


object CoteObscure_v2 {

  /**
   * Data definition
   * 
   * Je pense qu'il y en a déjà trop !
   */
  
  trait Weapon {
    def isRangeWeapon : Boolean
    def attack : Int
    def wearing: Int
  }
  
  case class EmperorRoyalGuardSword(wearing:Int) extends Weapon {
    val isRangeWeapon = false
    val attack = 20
  }

  case class StormTrooperGun(wearing:Int) extends Weapon {
    val isRangeWeapon = true
    val attack = 5
  }
  
  case object DarthVaderLaserSaber extends Weapon {
    val isRangeWeapon = false
    val attack = 100
    val wearing = 0
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
  
  trait IsAJedi {
    val canUseTheForce = true
  }
  
  trait IsNotAJedi {
    val canUseTheForce = false
  }

  case class StormTrooper(strength: Int, weapon: StormTrooperGun) extends EmpireSoldier with IsNotAJedi
  case class EmperorRoyalGuard(strength: Int, weapon: EmperorRoyalGuardSword) extends EmpireSoldier with IsNotAJedi
  case object DarthVader extends EmpireSoldier with IsAJedi {
    val strength = 200
    val weapon = DarthVaderLaserSaber
  }
  
  
  /**
   * Démonstration de quelques fonctions sur les collections, en particulier les fonctions
   * d'odre supérieur. 
   */
  
  val noviceTrooper = StormTrooper(strength = 5, StormTrooperGun(wearing = 0))
  val experiencedTrooper = StormTrooper(strength = 25, StormTrooperGun(wearing = 17))  
  val veteranRoyalGuard = EmperorRoyalGuard(strength = 50, EmperorRoyalGuardSword(wearing = 43))
  
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
  (noviceTrooper:EmpireSoldier) match {
    case StormTrooper(itsstrength, StormTrooperGun(currentWearing)) => 
      println("Found a soldier with %s strength and a weapon with a wearing of".format(itsstrength, currentWearing))
    case _ => println("Not a storm trooper")
  }
  
  
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
   * On veut se défendre contre des rebelles qui attaques à la fois de positions lointaine et au corps à corps.
   * Il faut donc séparer la squad en une équipe qui tirera sur les embusqués (armes de tir nécessaire) et une
   * équipe qui se battra au corp à corp (épées et sabres).
   */
  
  val teams = squad.groupBy { soldier => soldier.weapon.isRangeWeapon }
  
  //d'autres méthodes sympa ?
}