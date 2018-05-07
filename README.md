# OKComputer

### Proyecto de Lenguajes de Programación II CI4721

#### Integrantes:
- Alexander Romero 13-11274
- Augusto Hidalgo 13-10665

## Nombre de Lenguaje: OKComputer
## Especificaciones:
1) Lenguaje Imperativo
2) Alcance estático (Bloques anidables)
3) Sistema de tipos, con verificación estática (fuerte)
4) TIpos:
  - Simples (Entero, Bool, Char, Float)
  - Compuestos (Arreglos y Strings)
  - Estructurados (Registros y Uniones)
  - Apuntadores (Heap)
 5) Instrucciones
  - Selector
  - Repetición determinada
  - Repetición indeterminada
 6) Pasaje de parámetros
  - Valor
  - Referencia
 7) Retornos vaciós o de tipos simples
 8) Recursión
 9) EXTRAS:
  9.1) POSIBLE: Tuplas como tipo de datos
  9.2) POSIBLE: Modulos (import) o Templates
  
  
## Estructura de un programa en OKComputer:
      
Todos los programas en OKComputer deben tener un procedimiento maintheme() que puede recibir cualquier numero de argumentos. Todos los bloques comienzan por "youbegin" y terminan por "whereiend". Esto aplica para funciones, procedimientos y cualquier tipo de instruccion de seleccion e iteracion (excepto las seguidas por solo una orden de instruccion).

## Estructura Lexicografica
- Se ignoran los espacios en blanco en el programa
- Para comentarios, se utiliza el "#" y se ignora todo lo consiguiente en esa linea

## Tipos de Datos:
# Enteros
  Numeros enteros sin decimales
  Notacion: int
# Float
  Numeros punto flotantes, separando la parte entera de la decimal por un "." y seguida de al menos un digito
  Notacion: float
# Caracter
  Caracter cualquiera incluido en regex. Debe estar entre comillas simples (' ')
  Notacion: char
# String
  Cadena de caracteres cualesquiera incluidos en regex. Debe estar entre comillas dobles (" ")
  Notacion: string
# Booleanos
  Tipo para operaciones lógicas.
  Notación: boolean
  Notación par verdadero: ok
  Notación para falso: notok
# Arreglos
  Arreglos que pueden contener cualquier tipo, inclusive otros arreglos. Los elementos deben estar entre corchetes ([ ])
  Pueden ser de tipo dinamico o estatico. 
# Registros
  Estructuras que pueden contener elementos adentro, inclusive otros registros. Los elementos dentro deben ser declarados
  Notacion: band
# Union
  Estructuras formadas por varios elementos declarados, pero que en ejecución solo contiene uno de ellos.
  Notacion: union
# Apuntadores
  Apuntadores al heap para pasaje por referencia. Deben comenzar con &

## Instrucciones:
Todas las instrucciones vienen seguidas de bloques de instruccion:
  Para comenzar: youbegin
  Para terminar: whereiend

# Selector: 
	Comenzar una seleccion condicional con if:
  if (true) 
    youbegin
    ...
    whereiend
	Comenzar una seleccion con varios condicionales:
  if (a)
    ...
  ifyouhavetoask (b)
    ...
  otherwise
    ...
  Notacion: "ifyouhavetoask" es equivalente al "elseif" y "otherside" es equivalente al "else" en otros lenguajes.
	
Repeticion indeterminada:
	While -> OKC: cantstop BOOLEAN
	Ejemplo: cantstop (b==true)
	Tener un break -> OKC: breakthru/cuthere/iwanttobreakfree
Repeticion determinada:
	For -> OKC: onemoretime DECLARACION to RANGOCOMPARACION
	Ejemplo: onemoretime i=5 to i<10


