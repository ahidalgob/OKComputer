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
  9.1) Tuplas como tipo de datos
  9.2) Modulos (import)
  9.3) Lists como tipo de datos
  9.4) Funciones polimorficas
  
  
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
  
  Notacion: record
# Union
  Estructuras formadas por varios elementos declarados, pero que en ejecución solo contiene uno de ellos.
  
  Notacion: union
# Apuntadores
  Apuntadores al heap para pasaje por referencia. Deben comenzar con &

# Tuplas
  Tuplas de dos o mas elementos de distintos tipos. Deben estar entre parentesis (1,2,...) y contienen cualquier tipo
  
  Notacion: tuples
  
  Notacion para acceso a elementos: tuple.n donde n es un entero

# Listas
  Listas que pueden ser vacias o contener elementos del mismo tipo

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
  	otherside
   	 ...
	 
Notacion: "ifyouhavetoask" es equivalente al "elseif" y "otherside" es equivalente al "else" en otros lenguajes.
	
# Repeticion indeterminada:

	cantstop (b==true)
		youbegin
			...
		whereiend

Notacion: "cantstop" es equivalente al "while" en otros lenguajes

Para romper el ciclo, se puede utilizar un break

Notacion: breakthru
	
# Repeticion determinada:

	onemoretime int i=5; i<10; i=i+1 
		youbegin
			...
		whereiend

Notacion: "onemoretime" es euivalente al "for" en otros lenguajes

## Entrada y Salida de Datos
# Entrada
 Para entrada de datos desde terminal, equivalente al read se utiliza 
 
 	readmymind(x)
	
# Salida
 Para salida de datos al terminal, equivalente al print se utiliza
 
 	go("Imprimir")
	
 Equivalente, para imprimir con un espacio de por medio, equivalente al println se puede utilizar
 
 	goslowly(n)
	
# Funciones
  Debe ser declarado, recibe cualquier numero de argumentos y retorna un resultado (puede ser vacio, tipo procedimiento)
  
  	dafunk newfunc(int a, boolean b, char c)::int a
		youbegin
			getback a
		whereiend
  Notacion: dafunk nombre(#argumentos)
  
  Para retornar un resultado, equivalente al return, "getback"
  
  Para retornar vacio, equivalente al void, "intothevoid"

# Operadores aritmeticos
	+ - * / div mod -(prefijo)

# Operadores de comparacion
	> < >= <= == !=

# Operadores logicos
	and or not

# Crear variables
newlife() equivalente al "calloc" en C

amnesiac() equivalente al "free" en C

# Forzar salida del programa
  "exitmusic" equivalente al exit, en cualquier parte del programa, termina la ejecución.

# Modulos
  "aroundtheworld" equivalente al import, para traer funciones de otros archivos
