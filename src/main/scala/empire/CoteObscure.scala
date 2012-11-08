
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
    def strength      : Int
    def attack        : Int
    def canUseTheForce: Boolean
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
    
    def getATrainingSession : Unit = {
      if(this.experience < 20) { this.experience = experience + 1 }
    }
    
    override def attack = strength + experience
    
    override val canUseTheForce = false
  }

  val trooper = new StormTrooper(42) 
  
  while(trooper.attack < 50) {
    trooper.getATrainingSession
  }

  
  
  object StormTrooper {
    /*
     * Apply est une méthode magique en Scala: on a le droit de ne pas mettre 
     * ".apply" pour l'appeler. 
     * Ainsi, StormTrooper.apply(42) peut s'abbréger en StormTrooper(42).
     * Et comme nous définisson une valeur par défaut, nous pouvons même réduire en
     * "StormTrooper()" pour créer un storm trooper de force 10, voir "StormTrooper" 
     * puisque les parenthèses sont optionnelles dans ce cas. 
     */
    def apply(strength : Int = 10) = new StormTrooper(strength)
    
    //permet de créer un certain nombre de nouveau Storm Troopers.
    def makeClones(numberOfClones: Int) = Array.fill(numberOfClones)(StormTrooper)
  }
  
  
  
  
  trait IsASith {
    val canUseTheForce = true
  }
  
  trait TieFighterPilot {
    val canPilotTieFighter = true
  }

  class Sith(override val strength: Int) extends EmpireSoldier with IsASith {
    def attack = strength * 2
  }
  
  class RogueSith(strength: Int) extends IsASith

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
    //puissance d'attaque de l'arme
    def attack : Int
    //usure de l'arme
    def wearing: Int
  }
  
  //indique qu'un arme peut attaquer de loin
  trait Ranged

  //quelques armes standards
  case class BlasterGun(wearing: Int) extends Weapon with Ranged {
    val attack = 5
  }  
  case class HeavyBlasterGun(wearing: Int) extends Weapon with Ranged {
   val attack = 10
  }
  case class EmperorGuardSword(wearing: Int) extends Weapon {
    val attack = 20
  }
  
  //le sabre laser unique de Darth Vader
  case object DarthVaderLightSaber extends Weapon {
   val attack  = 100
   val wearing = 0
  }


  
  
  
//  //préciser que bien sûr, on complexifie pour montrer l'utilisation du pattern matching
//  def isRangeWeapon(weapon:Weapon) = weapon match {
//    case BlasterGun(_)        => true
//    case DarthVaderLightSaber => false
//    case _: Ranged            => true
//    case _                    => false
//  }
  
  /*
   * Ceci écrira: "The string 42", "An int: 42", "Something else!"
   */
  for(value <- Seq("42", 42, "43") ) {
    value match {
      case "42"  => println("The string 42")
      case i:Int => println("An int: " + i.toString)
      case _     => println("Something else!")
    }
  }  
  
//conserver la premiere définition
  trait EmpireSoldier {
    def strength      : Int
    def weapon        : Weapon
    def canUseTheForce: Boolean
  }  
//    final def attack : Int = {
//      val bonus = { if(canUseTheForce) 50 else 0 }
//      bonus + strength + weapon.attack
//    }
//    
//  }
  
  trait IsASith { val canUseTheForce = true }
  
  trait IsNotASith { val canUseTheForce = false }

  
  case class StormTrooper(strength: Int, weapon: Weapon with Ranged) 
    extends EmpireSoldier with IsNotASith
  
  case class EmperorGuard(strength: Int, weapon: EmperorGuardSword) 
    extends EmpireSoldier with IsNotASith
  
  case object DarthVader extends EmpireSoldier with IsASith {
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
  
  /*
   * Ici, nous voyons que Seq est un type paramétré par le type de ses éléments
   */
  val squad : Seq[EmpireSoldier] = 
    Seq(noviceTrooper, veteranRoyalGuard,  DarthVader, experiencedTrooper)
  
  
  //pattern matching
  
  //pattern matching simple
  
  
  

  
  //pattern matching plus complexe (descend dans les case classes)
  
  def showMeYourMind(soldier:EmpireSoldier) : Unit = {
    val inMind = soldier match {
      case StormTrooper(_, BlasterGun(currentWearing)) => 
        "Lunchtime soon... And only %d shots this morning".format(currentWearing)
      case DarthVader => 
        "Where ... is .. Padme ?"
      case sith:(IsASith with EmpireSoldier) if sith.strength > 50 => 
        "Stop reading my mind"
      case _ => "They are not the droid I was looking for"
    }
    println(inMind)
  }
  
  
  
  val attack: (EmpireSoldier => Int) = { soldier => 
    val bonus = { if(soldier.canUseTheForce) 50 else 0 }
    bonus + soldier.strength + soldier.weapon.attack
  }
  
  //ici, on cherche à connaitre la puissance d'attaque totale de notre équipe
  
  
  /*
   * On cherche à connaitre tous les soldats qui ont des armes abimées, 
   * c'est à dire avec un wearing de plus de 15. 
   */
 
  
  
  val soldierWithDamagedWeapons = squad.filter { 
    soldier => soldier.weapon.wearing > 15 
  }
  


  /*
   * un soldat essaie de parer un laser. S'il n'est pas Sith,
   * il meurt. 
   */
//   
//  def deflectLaser(soldiers: Seq[EmpireSoldier]): Seq[EmpireSoldier] = {
//    soldiers.map { soldier => 
//      if(soldier.canUseTheForce) soldier else DeadSoldier
//    }
//  }
  



  /*
   * foreach  permet l’application d’une fonction qui ne retourne pas de valeur
   * à chaque élément de la collection.
   * Ici, nous voyons que foreach est une fonction d'ordre supérieur: il
   * prend en argument la fonction "showMeYourMind"
   * A l'exécution, nous obtiendrons sur la console ce que pense chaque
   * membre de l'équipe. 
   */
  def readSquadMinds(squad: Seq[EmpireSoldier]) = squad.foreach( showMeYourMind )
  
  /*
   * Nous pouvons simplement trouver la puissance d'attaque d'une
   * équipe construisant la séquence composée des attaques de chaque 
   * membre, puis en en faisant la somme. 
   */
  val squadAttack = squad.map { soldier => attack(soldier) }.sum
  
  /*
   * filter permet d’appliquer un prédicat sur chaque élément de la liste 
   * et de ne garder que les éléments qui le valide.
   */
  def detectForceUser(soldiers: Seq[EmpireSoldier]): Seq[EmpireSoldier] =
   soldiers.filter(soldier => soldier.canUseTheForce) 

   

   //fonctions plus puissante
   
   
  /*
   * On veut se défendre contre des rebelles qui attaques à la fois de positions
   * lointaine et au corps à corps. Il faut donc séparer la squad en une équipe 
   * qui tirera sur les embusqués (armes de tir nécessaire) et une équipe qui 
   * se battra au corp à corp (épées et sabres).
   */  
  val teams: Map[Boolean, Seq[EmpireSoldier]] = 
    squad.groupBy { soldier => soldier.weapon match {
      case _: Range => true
      case _        => false
    }  
  }
  
  /*
   * Les bons storm trooper peuvent être promus en garde impériaux. 
   * La condition est qu'ils doivent effectivement être un Storm Trooper, 
   * et avoir une force de plus de 20. 
   * Note: le type de la collection est plus précis que le type de départ. 
   */  
  val promotableTroopers : Seq[StormTrooper] = squad.collect { 
    case trooper @ StormTrooper(strength, weapon) if(strength > 20) => trooper 
  }
  
  
  

  //for comprehension

     
  def detectForceUser2(soldiers: Seq[EmpireSoldier]) = for {
    soldier <- soldiers
    if soldier.canUseTheForce
  } yield soldier
      



  
}