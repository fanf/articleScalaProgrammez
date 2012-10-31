
/**
 * Est ce qu'on montre des tests ? 
 * => ca peut être pas mal pour montrer le DSL de spec 2 ?
 */


object CoteObscure_v1 {

  /**
   * Ici, on montre les conpets de base sur l'héritage / les objets :
   * - trait / class / object
   * - var, puis on remplace par val
   * - def
   * - visibilité (rapidement)
   * - type inférence 
   */
  
  
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
    def strenght: Int
    def weapon: Weapon
    def canUseTheForce : Boolean
    
    final def attack : Int = {
      val bonus = { if(canUseTheForce) 50 else 0 }
      bonus + strenght + weapon.attack
    }
    
  }
  
  trait IsAJedi {
    val canUseTheForce = true
  }
  
  trait IsNotAJedi {
    val canUseTheForce = false
  }

  case class StormTrooper(strenght: Int, weapon: StormTrooperGun) extends EmpireSoldier with IsNotAJedi
  case class EmperorRoyalGuard(strenght: Int, weapon: EmperorRoyalGuardSword) extends EmpireSoldier with IsNotAJedi
  case object DarthVader extends EmpireSoldier with IsAJedi {
    val strenght = 200
    val weapon = DarthVaderLaserSaber
  }
  
  
  /**
   * Démonstration de quelques fonctions sur les collections, en particulier les fonctions
   * d'odre supérieur. 
   */
  
  val noviceTrooper = StormTrooper(strenght = 5, StormTrooperGun(wearing = 0))
  val experiencedTrooper = StormTrooper(strenght = 25, StormTrooperGun(wearing = 17))  
  val veteranRoyalGuard = EmperorRoyalGuard(strenght = 50, EmperorRoyalGuardSword(wearing = 43))
  
  //ici, préciser le type pour montrer les constructeurs de type
  val squad : Seq[EmpireSoldier] = Seq(noviceTrooper, veteranRoyalGuard,  DarthVader, experiencedTrooper)
  
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
  val promotableTroopers : Seq[StormTrooper] = squad.collect { case trooper @ StormTrooper(strenght, weapon) if(strenght > 20) => trooper }
  
  
  
  /*
   * On veut se défendre contre des rebelles qui attaques à la fois de positions lointaine et au corps à corps.
   * Il faut donc séparer la squad en une équipe qui tirera sur les embusqués (armes de tir nécessaire) et une
   * équipe qui se battra au corp à corp (épées et sabres).
   */
  
  val teams = squad.groupBy { soldier => soldier.weapon.isRangeWeapon }
  
  //d'autres méthodes sympa ?
}