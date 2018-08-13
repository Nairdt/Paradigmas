module Backend exposing(..)
import Models exposing(Movie, Preferences)

completaAca = identity

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- **************

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = List.filter (peliculaTienePalabrasClave palabras)

-- esta función la dejamos casi lista, pero tiene un pequeño bug. ¡Corregilo!
--
-- Además tiene dos problemas, que también deberías corregir:
--
-- * distingue mayúsculas de minúsculas, pero debería encontrar a "Lion King" aunque escriba "kINg"
-- * busca una coincidencia exacta, pero si escribís "Avengers Ultron" debería encontrar a "Avengers: Age Of Ultron"
--
peliculaTienePalabrasClave palabras pelicula = String.contains (String.toUpper palabras) (String.toUpper pelicula.title)


-- **************
-- Requerimiento: visualizar las películas según el género elegido en un selector;
-- **************

filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero = List.filter (peliculaTieneGenero genero)

peliculaTieneGenero genero pelicula = String.contains (String.toUpper genero) (String.toUpper (String.concat pelicula.genre))

-- **************
-- Requerimiento: filtrar las películas que sean aptas para menores de edad,
--                usando un checkbox;
-- **************

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores = List.filter (peliculaEsParaMenor mostrarSoloMenores)

peliculaEsParaMenor mostrarSoloMenores pelicula = pelicula.forKids == True

-- **************
-- Requerimiento: ordenar las películas por su rating;
-- **************

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating = List.reverse<<List.sortBy .rating 

-- **************
-- Requerimiento: dar like a una película
-- **************

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = completaAca --List.filter (darLike id)

--darLike id pelicula = if pelicula.id == id then pelicula.likes + 1

-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias = filtrarPeliculasPorPalabrasClave preferencias.keywords << filtrarPeliculasPorGenero preferencias.genre << filtrarPeliculaPorActor preferencias.favoriteActor

porcentajeTotal preferencias pelicula = porcentajePalabraClave preferencias pelicula + porcentajeGenero preferencias pelicula + porcentajeActor preferencias pelicula

porcentajePalabraClave preferencias pelicula = if (peliculaTienePalabrasClave preferencias.keywords pelicula) then ((String.length (preferencias.keywords) * 100) / String.length (pelicula.title)) else 0

porcentajeGenero preferencias pelicula = if (peliculaTieneGenero preferencias.genre pelicula.genre) then 100 << (//) <<(List.length pelicula.genre) else 0

filtrarPeliculaPorActor : String -> List Movie ->List Movie
filtrarPeliculaPorActor actor = List.filter (peliculaTieneActor actor)
peliculaTieneActor actor pelicula = String.contains (String.toUpper actor) (String.toUpper (String.concat pelicula.actors))

porcentajeActor preferencias pelicula = if (peliculaTieneActor preferencias.favoriteActor) then (100 / List.length pelicula.actors) else 0
