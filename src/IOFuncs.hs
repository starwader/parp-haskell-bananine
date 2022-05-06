module IOFuncs where

import Locations
import GameState

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)
 
printLocation :: Location -> IO ()
printLocation l = do
    let toPrint = "Znajdujesz sie w:"
    printLines [toPrint, l]
 
readCommand :: IO String
readCommand = do
    putStr "> "
    xs <- getLine
    return xs


printDescription :: GameState -> IO ()
printDescription s = do {
    case currentLocation s of {
    "dżungla" -> printLines [
        "Znajdujesz się w gęstej, ciemnej dżungli.",
        "Na północy znajduje się mała polanka która jest domem rodziny goryli."
    ];
    "rozwidlenie" -> printLines [
        "Zbliżasz się do rozwidlenia świeżki.",
        "Na wschodzie wznosi się spory fort kłusowników,",
        "zaś na zachodzie znajduje się polanka Goryli.",
        "Na północy widzisz mroczny kamieniołom.",
        "Na południu dostrzegasz wejście do przerażającej jaskini Kobry"
    ];
    "polanka" -> printLines [
        "Zbliżasz się do sporej polanki,",
        "na której znajdują się dwa goryle z rodu Pumba.",
        "Potężny goryl Koko patrzy się na ciebie spokojnym wzrokiem.",
        "Mały Bobo skacze radośnie.",
        "Na południu znajduje się mroczna dżungla.",
        "Na wschodzie widzisz długą ścieżkę,",
        "a na zachodzie zjawiskowy klasztor Tiu-Fiu."
    ];
    "klasztor" -> printLines [
        "Znajdujesz się w starożytnym budynku - sercu szkoły Tiu-Fiu.",
        "Po środku stoi mistrz starożytnych sztuk walki Tiu-Fiu - Uebe.",
        "Na wschodzie znajduje się Polanka goryli,",
        "a na północy \"jaskinia próby\"."
    ];
    "jpróby" -> printLines [
        "Po wejściu do jaskini,",
        "wrota zamykają się za tobą.",
        "Po wielogodzinnej i ciężkiej tułaczce przez",
        "długą i głęboką jaskinię próby,",
        "odwodniony,",
        "głodny i zmęczony docierasz do dziwnego,",
        "ciemnego i oślizgłego pomieszczenia,",
        "całego pokrytego kurzem,",
        "pajęczynami,",
        "szkieletami i starymi książkami.",
        "Na fotelu zauważasz starego gnoma."
    ];
    "jkobry" -> printLines [
        "Wchodzisz do ciemnej i przerażającej jaskini Kobry.",
        "Wyjście znajduje się na północy." -- inaczej zrobione niż w prologu
    ];
    "dziedziniec" -> printLines [
        "Znajdujesz się przed wejściem fortu,",
        "które jest po twojej wschodniej stronie.",
        "Na zachodzie znajduje się ścieżka, z której przyszedłeś."
    ];
    "fort" -> printLines [
        "Znajdujesz się na dziedzińcu fortu.",
        "Na zachodzie znajduje się wyjście."  -- inaczej niż w prologi
    ];
    "kamieniołom" -> printLines [
        "Stoisz po środku starego kamieniołomu,",
        "w głębi stoi ornamentna złota skrzynia.",
        "Na południu znajduje się wyjście."
    ];
    _ -> printLines ["Brak opisu"];
}
}
