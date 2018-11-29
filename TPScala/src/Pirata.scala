import scala.collection.mutable
import java.util.Random

trait Pirata {
  
  var ingredientes: List[String] = List("nada")
  
  def poderDeMando(): Int
  
  def herido()
  
  def esFuerte(): Boolean = {
    this.poderDeMando() >= 100
  }
  
  def tomarRonCon(alguien: Pirata)
  
  def tieneMenosDe20Energia(): Boolean
  
}

object Vacio extends Pirata {
  
  var energia = 0
    
  override def poderDeMando(): Int = { return 0 }
  
  override def herido() { energia = 0}
  
  def tomarRonCon(alguien: Pirata) { energia = 0 }
  
  def tieneMenosDe20Energia(): Boolean = {return false}
}

object Jack extends Pirata {
  var energia = 500
  var poderPelea = 200
  var inteligencia = 300
  ingredientes = List("ron") 
  
  override def poderDeMando(): Int ={
    return poderPelea*energia*inteligencia
      
  }
  override def tomarRonCon(pirata: Pirata)= {
    energia += 100
    pirata.tomarRonCon(Jack)
  
  }
  override def herido() {
      poderPelea /= 2
      inteligencia /= 2
    }
  
    def tieneMenosDe20Energia(): Boolean = {
    return energia < 20
  }
}

abstract class PirataComun (_energia: Int) extends Pirata{
  
  var energia: Int = _energia
  
  def perderEnergia() {
    energia -= 50
  }

   def tomarRonCon(jack: Pirata){
     perderEnergia()
     
   }
   
   def tieneMenosDe20Energia(): Boolean = {
    return energia < 20
  }
}

class Guerrero(_energia: Int, _poderPelea: Int, _vitalidad: Int) extends PirataComun (_energia) {
  
  var poderPelea: Int = _poderPelea
  var vitalidad: Int = _vitalidad
  
  override def poderDeMando(): Int = {
   return poderPelea * vitalidad  
   }
  
  override def herido() {
    poderPelea /= 2
    vitalidad /= 2
  }
}


class Navegante(_energia: Int,_inteligencia: Int) extends PirataComun (_energia){
  
  var inteligencia: Int = _inteligencia
  
  override def poderDeMando(): Int = {
    return inteligencia * inteligencia
        
  }
  
   override def herido() {
    inteligencia /= 2
   }
 
}

class Cocinero(_energia: Int, _moral: Int, _ingredientes: List[String]) extends PirataComun(_energia) {
  
  var moral: Int = _moral
  ingredientes = _ingredientes
  

  override def poderDeMando(): Int = {
    return moral * ingredientes.size
 
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
  
  override def tomarRonCon(Jack:Pirata){
    this.perderEnergia()
    this.entregarIngredienteA(Jack:Pirata)
  }


 
  }


class Monstruo (_energia: Int, _poderPelea: Int) extends PirataComun (_energia) {
  
  var poderPelea = _poderPelea
  
  override def poderDeMando(): Int = {
    return poderPelea
    
  }
  
   override def herido() {
    poderPelea /= 2
   }
  
}
  


class Barco(_resistencia: Int, _poderFuego: Int,_municiones: Int, _tripulacion: List[Pirata], _bando: Bando) {
  
  var bando = _bando
  var resistencia: Int = _resistencia
  var poderFuego: Int = _poderFuego
  var municiones: Int  = _municiones
  var tripulacion: List[Pirata] = _tripulacion
  
  
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
    return this.listaPoderes().sum
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
    if (canionazos <= municiones) {
      municiones = municiones - canionazos
      otroBarco.resistencia - (0.5*canionazos)
    }
    
    else {
      println("Error: se debe tener la misma o mayor cantidad de municiones")
    }
    
    
  }
  def leGanaA(barco : Barco): Boolean = {
    return this.fuerza() > barco.fuerza()
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
    barco.municiones * 1.30
    
  }
  
}

object unionPirata extends Bando {
  override def bonus(barco: Barco) {
    barco.poderFuego * 1.60
  }
  
}

object holandesErrante extends Bando {
  override def bonus(barco: Barco){
    barco.tripulacion = barco.tripulacion ::: barco.tripulacion
  }

  
  
}
