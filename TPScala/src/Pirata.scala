abstract class Pirata(energia: Int, poderPelea: Int) {
   def poderDeMando(): Int{
    
    
  }
  
  
  def acompaniarRonCon(): Unit =  {
    energia - 100
    
  }
  
  def herido() {
    poderPelea / 2
  }
  
  def esFuerte(): Boolean = {
    poderDeMando > 100
  }

  def tieneMenosDe20Energia(): Boolean = {
    energia < 20
  }
}

class Guerrero(val energia: Int, val poderPelea: Int, val vitalidad: Int) extends Pirata(energia, poderPelea){
  
  override def poderDeMando(): Int = {
   val resultado = poderPelea * vitalidad
    resultado
    
    
  }
  
  override def herido() {
    
      vitalidad / 2
    
  }
  
}


class Navegante(override val energia: Int, override val poderPelea: Int, val inteligencia: Int) extends Pirata(energia, poderPelea){
  override def poderDeMando(): Int = {
    val resultado = inteligencia * inteligencia
    resultado
    
    
  }
  
  def herido() {
    poderPelea / 2
    inteligencia / 2
  }
 
}



class Cocinero(override val energia: Int, override val poderPelea: Int, val moral: Int, val ingredientes: List[String]) extends Pirata(energia, poderPelea) {
  

  override def poderDeMando(): Int = {
    val resultado = moral * ingredientes.size
    resultado
    
  }
  
  def entregarIngrediente(): Unit = {
   
  


 
  }
  
  override def herido() {
    moral / 2
  }
}

trait PiratasComunes {
  
  val energia = 0
  val poderPelea = 0
  val inteligencia = 0
  val listaIngredientes = List(" ")
  
  
}


object jackSparrow extends PiratasComunes {
  val energia = 500
  val poderPelea = 200
  val inteligencia = 300
  val listaIngredientes = List("ron") 
  def tomarRonCon(pirata: Pirata): Unit = {
    jackSparrow.energia + 100
    
  def herido() {
      energia / 2
      poderPelea / 2
      inteligencia / 2
    }
    
  }
}

abstract class Barco(val resistencia: Int, val poderFuego: Int, val municiones: Int, tripulacion: Seq[Pirata]) {
  
  def poderesDeMando(): Seq[Int] = {
    tripulacion.map{Pirata => Pirata.poderDeMando()}
  }
  
  def fuerza(): Int = { 
    val poderesMando  = poderesDeMando();
    poderesMando.sum 
  }
  
 
  def tripulacionHerida(): Unit =  {
    
    tripulacion.foreach(Pirata => Pirata.herido())
  }
  
  def tripulantesFuertes(){
    tripulacion.filter(Pirata => Pirata.esFuerte())
  }
  
  def agregarTripulantesFuertes(barco: Barco) {
    tripulacion.+:(barco.tripulantesFuertes())
    
  }
  
  def desolado(barco: Barco){
    
  }
  
  def tripulacionConMenosDe20(): Unit =  {
    tripulacion.foreach(Pirata => Pirata.tieneMenosDe20Energia())
  }
  
  
  
  def disparar(otroBarco: Barco, canionazos: Int){
    if (canionazos >= this.municiones) {
      municiones - canionazos
      otroBarco.resistencia - (0.5*canionazos)
    }
    
    else {
      println("Errror: se debe tener la misma o mayor cantidad de municiones")
    }
    
    
  }
  
  def enfrentarA(otroBarco: Barco): Unit = {
    if(fuerza() > otroBarco.fuerza()) {
      otroBarco.tripulacionHerida()
      agregarTripulantesFuertes(this)

    }
    
    else {
      tripulacionHerida()
      agregarTripulantesFuertes(otroBarco)
      
    }
  }
}
  
trait Bando{
  
  
}

object armadaInglesa extends Bando{
  
  def aumentarMuniciones(barco: Barco) {
    barco.municiones * 1.30
    
  }
  
}

object unionPirata extends Bando {
  def aumentarMuniciones(barco: Barco) {
    barco.poderFuego * 1.60
  }
  
}

object holandesErrante extends Bando {
  def aumentarTripulacion(barco: Barco){
    
  }
}