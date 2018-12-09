import scala.collection.mutable
import java.util.Random

trait Pirata {
  
  var ingredientes: List[String] = List("nada")
  
  def poderDeMando(): Int
  
  def herido()
  
  def esFuerte(): Boolean = {
    this.poderDeMando() >= 100
  }
  
  def tomarRonConJack()
  
  def tieneMenosDe20Energia(): Boolean
  
}

object Vacio extends Pirata {
  
  var energia = 0
    
  override def poderDeMando(): Int = { return 0 }
  
  override def herido() { energia = 0}
  
  def tomarRonConJack() { energia = 0 }
  
  def tieneMenosDe20Energia(): Boolean = {return false}
}

object Jack extends Pirata {
  var energia = 500
  var poderPelea = 200
  var inteligencia = 300
  ingredientes = List("ron") 
  
  override def poderDeMando(): Int ={
    poderPelea*energia*inteligencia
      
  }
  def tomarRonCon(pirata: Pirata)= {
    energia += 100
    pirata.tomarRonConJack()
  
  }
  
  def tomarRonConJack() {}
  override def herido() {
      poderPelea /= 2
      inteligencia /= 2
    }
  
    def tieneMenosDe20Energia(): Boolean = {
    energia < 20
  }
}

abstract class PirataComun (var energia: Int) extends Pirata{
  
  
  def perderEnergia() {
    energia -= 50
  }

   def tomarRonConJack(){
     perderEnergia()
     
   }
   
   def tieneMenosDe20Energia(): Boolean = {
     energia < 20
  }
}

class Guerrero(_energia: Int, var poderPelea: Int, var vitalidad: Int) extends PirataComun (_energia) {
  
  
  override def poderDeMando(): Int = {
    poderPelea * vitalidad  
   }
  
  override def herido() {
    poderPelea /= 2
    vitalidad /= 2
  }
}


class Navegante(_energia: Int, var inteligencia: Int) extends PirataComun (_energia){
  
  override def poderDeMando(): Int = {
    inteligencia * inteligencia
        
  }
  
   override def herido() {
    inteligencia /= 2
   }
 
}

class Cocinero(_energia: Int, var moral: Int, _ingredientes: List[String]) extends PirataComun(_energia) {
  
  ingredientes = _ingredientes
  
  override def poderDeMando(): Int = {
    moral * ingredientes.size
 
  }
  
   override def herido() {
    moral /= 2
   }
   
  def entregarIngredienteA(Jack:Pirata): Unit = {
   
  val rand = new Random
  val indice = rand.nextInt(ingredientes.length)
  var unIngrediente = ingredientes(indice)
  ingredientes.filterNot{ingrediente => ingrediente == unIngrediente}
  Jack.ingredientes = Jack.ingredientes ::: List(unIngrediente)
  }
  
  override def tomarRonConJack(){
    this.perderEnergia()
    this.entregarIngredienteA(Jack:Pirata)
  }


 
  }


class Monstruo (_energia: Int, var poderPelea: Int) extends PirataComun (_energia) {
  
  
  override def poderDeMando(): Int = {
    poderPelea
    
  }
  
   override def herido() {
    poderPelea /= 2
   }
  
}
  


class Barco(var resistencia: Int, var poderFuego: Double, var municiones: Double, var tripulacion: List[Pirata], var bando: Bando) {
 
  
  def aplicarBonus() {
    bando.bonus(this)  
  }
  
  def cambiarBando(unBando: Bando) {
    bando = unBando
    unBando.bonus(this)
  }
  
  def listaPoderes(): Seq[Int] = {
    tripulacion.map{Pirata => Pirata.poderDeMando()}
  }
  
  def fuerza(): Int = {
    this.listaPoderes().sum
  }
  
    def tripulacionHerida(): Unit =  {
    
    tripulacion.foreach(Pirata => Pirata.herido())
  }
  
  def tripulantesFuertes(){
    tripulacion.filter(Pirata => Pirata.esFuerte())
  }
  
  def tripulacionAlAgua(){
    tripulacion= List(Vacio)
  }
  
  def agregarTripulantesFuertesDe(barco: Barco) {
    tripulacion.+:(barco.tripulantesFuertes())
    
  }
  
  def desolado(){
    resistencia = 0
    poderFuego = 0
    municiones = 0
    
  }
  
  def tripulacionConMenosDe20(): Unit =  {
    tripulacion.foreach(Pirata => Pirata.tieneMenosDe20Energia())
  }
  
  
  
  def disparar(otroBarco: Barco, canionazos: Int){
    if (canionazos >= municiones) {
      throw new RuntimeException("Error: se debe tener la misma o mayor cantidad de municiones")
    }
      municiones = municiones - canionazos
      otroBarco.resistencia - (0.5*canionazos)
    
    
    
  }
  def leGanaA(barco : Barco): Boolean = {
    this.fuerza() > barco.fuerza()
  }
  def enfrentarA(otroBarco: Barco) {
    if(this.leGanaA(otroBarco)) {
      otroBarco.tripulacionHerida()
      this.agregarTripulantesFuertesDe(otroBarco)
      otroBarco.desolado()

    }
    else 
    {
      this.tripulacionHerida()
      otroBarco.agregarTripulantesFuertesDe(this)
      this.desolado()
    }
  }
}
  
trait Bando{
  
  def bonus(unBarco: Barco)
  
}

object armadaInglesa extends Bando{
  
  override def bonus(barco: Barco) {
    barco.municiones *= 1.30
    
  }
  
}

object unionPirata extends Bando {
  override def bonus(barco: Barco) {
    barco.poderFuego *= 1.60
  }
  
}

object holandesErrante extends Bando {
  override def bonus(barco: Barco){
    barco.tripulacion = barco.tripulacion ::: barco.tripulacion
  }

  
  
}
