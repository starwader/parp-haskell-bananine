module Consts.TextConstants where
-- stałe tekstowe

introductionText =
  [ "Jest rok 1950.",
    "Jesteś znanym poszukiwaczem przygód Robertem Gałązką.",
    "Niestety, podczas tej wyprawy w głąb afryki,",
    "twoja drużyna została schwytana przez tubylców.",
    "Jesteś skrępowany mocnymi sznurami,",
    "ostatnio straciłeś przytomność od mocnego uderzenia",
    "jakąś dziwną bronią obuchową.",
    "Czujesz, że zbliża się twój koniec,",
    "tubylcy pewnie cię zjedzą albo złożą w ofierze.",
    "",
    "Nagle jednak w dziczy zauważasz goryla, który zaczyna",
    "szarżować na twoich oprawców.",
    "Przerażeni ludzie uciekają w popłochu,",
    "Całkowicie o tobie zapominając.",
    "Upadasz na ziemię i tracisz przytomność.",
    "",
    "...po jakimś czasie budzisz się...",
    ""
  ]

instructionsText =
  [ "Dostępne komendy:",
    "",
    "n,s,w,e                            -- idź w daną stronę",
    "gdzie jestem                       -- wyświetl informacje o miejscu",
    "pomoc                              -- wyświetl to menu",
    "koniec                             -- zakończ grę",
    "rozmawiaj <Postać>                 -- rozmawiaj z postacią",
    "atakuj <Postać>                    -- zaatakuj postać",
    "ekwipunek                          -- wyświetl ekwipunek",
    "postacie                           -- wyświetl postacie",
    "otwórz <Kontener>                  -- otwórz kontener",
    "umiejętności                       -- wyświetl umiejętności",
    "podnieś <Przedmiot>                -- podnieś przedmiot",
    "upuść <Przedmiot>                  -- upuść przedmiot",
    "wyjmij <Przedmiot> z <Kontener>    -- wyjmij przemiot z kontenera",
    "umieść <Przedmiot> w <Kontener>    -- włóż przedmiot do kontenera",
    "zadania                            -- wyświetl zadania",
    ""
  ]

killBadGuysText =
  [ "z każdej strony widzisz uzbrojonych po zęby kłusowników.",
    "Na szczęście jesteś już pełnoprawnym wybrańcem i mistrzem sztuk walki Tiu-Fiu.",
    "Teraz dopełnia się twoje przeznaczenie.",
    "Kłusownicy otwierają ogień,",
    "ty jednak skutecznie unikasz wszystkich ich strzałów,",
    "w międzyczasie strzelając ze swojego pięknego Excalibra.",
    "W mgnieniu oka wszystkich dziesięciu kłusowników pada na ziemię",
    "a ty nabierasz jeszcze większej ochoty na banany z dżungli.",
    "Na zachodzie znajduje się wyjście."
  ]

dzunglaDesc =
  [ "Znajdujesz się w gęstej, ciemnej dżungli.",
    "Na północy znajduje się mała polanka która jest domem rodziny goryli."
  ]

rozwidlenieDesc =
  [ "Zbliżasz się do rozwidlenia ścieżki.",
    "Na wschodzie wznosi się spory fort kłusowników,",
    "zaś na zachodzie znajduje się polanka Goryli.",
    "Na północy widzisz mroczny kamieniołom.",
    "Na południu dostrzegasz wejście do przerażającej jaskini Kobry"
  ]

polankaDesc =
  [ "Zbliżasz się do sporej polanki,",
    "na której znajdują się dwa goryle z rodu Pumba.",
    "Potężny goryl Koko patrzy się na ciebie spokojnym wzrokiem.",
    "Mały Bobo skacze radośnie.",
    "Na południu znajduje się mroczna dżungla.",
    "Na wschodzie widzisz długą ścieżkę,",
    "a na zachodzie zjawiskowy klasztor Tiu-Fiu."
  ]

klasztorDesc =
  [ "Znajdujesz się w starożytnym budynku - sercu szkoły Tiu-Fiu.",
    "Po środku stoi mistrz starożytnych sztuk walki Tiu-Fiu - Uebe.",
    "Na wschodzie znajduje się Polanka goryli,",
    "a na północy \"jaskinia próby\"."
  ]

jprobyDesc =
  [ "Po wejściu do jaskini,",
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
  ]

jkobryDesc =
  [ "Wchodzisz do ciemnej i przerażającej jaskini Kobry.",
    "Wyjście znajduje się na północy." -- inaczej zrobione niż w prologu
  ]

dziedziniecDesc =
  [ "Znajdujesz się przed wejściem fortu,",
    "które jest po twojej wschodniej stronie.",
    "Na zachodzie znajduje się ścieżka, z której przyszedłeś."
  ]

fortDesc =
  [ "Znajdujesz się na dziedzińcu fortu.",
    "Na zachodzie znajduje się wyjście." -- inaczej niż w prologi
  ]

kamieniolomDesc =
  [ "Stoisz po środku starego kamieniołomu,",
    "w głębi stoi ornamentna złota skrzynia.",
    "Na południu znajduje się wyjście."
  ]

kokoWelcome =
  [ "Witaj podróżniku! ",
    "Niestety żyjemy w ciężkich czasach.",
    "Nasza rodzina stoi w obliczu zagłady!",
    "Źli kłusownicy polują na nas i robią z naszych skór dywany! ",
    "Jeśli tak dalej pójdzie, to wszyscy zginiemy!",
    "Czy możesz nam pomóc?",
    "Na pewno zostaniesz sowicie nagrodzony bananami, jeśli ci się uda",
    "",
    "<tak/nie>"
  ]

kokoUebeQuestion =
  [ "Czy porozmawiałeś już z Uebe?"
  ]

kokoEndingPhrase =
  [ "Oto nasz wybawca Bananine!"
  ]

kokoUebeQuestionNo =
  [ "A zatem wszyscy zginiemy!"
  ]

kokoUebeQuestionYes =
  [ "Och, dziękuję!",
    "Powiadomię Uebe, mistrza starożytnych sztuk walki Tiu-Fiu",
    "o twojej dobroduszności, a na pewno ci pomoże!",
    "Poszukaj go na zachodzie."
  ]

gnomDefault =
  [ "SpAdAj StąD!!!1!!"
  ]

gnomQuestion =
  [ "W starym forcie znowu wielka siła się zjawiła,",
    "kłusowników kiść dywanów wiele z nas zrobiła.",
    "Dnia takiego to mrocznego obudzi się w dżungli,",
    "Przedstawiciel brudnej, strasznej i złej rasy ludzi.",
    "Nie będzie to jednak postać zła, ani skalana,",
    "Lecz reinkarnacja naszego starożytnego Banana.",
    "Bananine - tak go zwać lud nasz prosty będzie,",
    "Jego osiągnięcia znane będą światu wszędzie.",
    "",
    "Taka przepowiednia widnieje na starożytnych freskach w moim domu Ghfjg,",
    "w górze Hasdkh.",
    "Ale niestety,",
    "przed każdym potencjalnym wybrańcem stoi test,",
    "którego zawalenie wiąże się ze strasznymi konsekwencjami.",
    "<gnom przystawia ci do czoła karabin AK-47>",
    "Jakie zwierzę chodzi rano na 4 nogach,",
    "na 2 w ciągu dnia i na 3 wieczorem?"
  ]

gnomGoodAnswer =
  [ "<Zaskoczony gnom odstawia AK-47 na bok>",
    "Hmmmm....",
    "Ciekawa odpowiedź...",
    "Jeszcze nikt nie podał do tej pory sensownej odpowiedzi,",
    "<wskazuje palcem na stos szkieletów>",
    "ale twoja faktycznie zdaje się trzymać kupy.",
    "Uznajmy, że przeszedłeś tę próbę, człowieku.",
    "<Gnom uderza cię w głowę i tracisz przytomność>",
    "<Po jakimś czasie budzisz się w klasztorze Tiu-Fiu>"
  ]
gnomTrialFinished = 
  [
    "<Gnom uderza cię w głowę i tracisz przytomność>",
    "<Po jakimś czasie budzisz się w klasztorze Tiu-Fiu>"
  ]
gnomSeenGun =
  [
    "<W ostatnim momencie przypominasz sobie o karabinie od Bobo>",
    "<Jednym ruchem wyciągasz karabin i przystawiasz go do głowy gnoma>",
    "Zaraz zaraz, skąd to masz?",
    "To legendarny złoty karabin mojego dziadka!",
    "Oddawaj to, i wynoś się"
  ]

gnomBadAnswer =
  [ "Żaden z ciebie Bananine!",
    "GIŃ!"
  ]

-- no zepsuty karabin opcja - dla uproszczenia

uebeNoKokoTask =
  [ "Ludzie nie mają czego szukać w tym świętym miejscu.",
    "Żegnam."
  ]

uebeGivingProbaTask =
  [ "Podobno chcesz nam pomóc... Hmmm...",
    "Być może przepowiednia ziści się w twojej osobie?",
    "W każdym razie, jeżeli twoja pomoc okaże się owocna,",
    "nagrodzę cię dożywotnim zapasem bananów.",
    "Jeśli chcesz nauczyć się sztuki Tiu-Fiu,",
    "musisz poprawnie przejść test w Jaskini Próby.",
    "<otwiera wrota jaskini próby>",
    "Nie martw się, to z pewnością bezpieczne."
  ]

uebeProbaTask =
  [ "Wejdź do jaskini próby!",
    "Przekonajmy się czy płynie w tobie krew Banana!"
  ]

uebeAfterProba =
  [ "Interesujące...",
    "Jeśli pragniesz zostać moim uczniem i zgłębić tajniki Tiu-Fiu,",
    "najpierw udowodnij swoją wartość.",
    "Musisz odzyskać mój portfel ze starożytnej jaskini Kobry.",
    "Aby do niej dotrzeć, musisz skierować się na wschód.",
    "Wiele lat temu, złe pająki z rodu Kobry ukradły mi go.",
    "Jednak zanim pójdziesz - musisz nauczyć się zaklęcia \"Potassium\";",
    "tylko wtedy pokonasz pająki z rodu Kobry, które gnieżdżą się w jaskini.",
    "<obsypuje cię potasem>",
    "Teraz, spróbuj rzucić na mnie zaklęcie Potassium! (zaatakuj Uebe)"
  ]

uebeWalletTask =
  [ "Nie przychodź do mnie bez mojego portfela!"
  ]

uebeAttackUebe =
  [ "Zaatakuj mnie zaklęciem Potassium!"
  ]

uebeLearningTiuFiu =
  [ "Niesamowite!",
    "Udało ci się!",
    "Nareszcie mogę spłacić kredyt.",
    "Teraz nauczę cię walczyć w stylu Tiu-Fiu.",
    "<mija wiele miesięcy, a ty stajesz się coraz silniejszy...>",
    "Umiesz teraz sztukę walki Tiu-Fiu!",
    "Jednak wiedz,",
    "że sama umiejętność Tiu-Fiu nie pozwoli ci pokonać wszystkich kłusowników.",
    "Musisz jeszcze odnaleźć starożytny pistolet Luger Parabellum - Excaliber.",
    "On pozwoli ci pokonać złych zbrodniarzy.",
    "Oto starożytny klucz kiedyś znalazłem w zestawie Happy Meal - być może ci pomoże.",
    "... Powodzenia, przyjacielu."
  ]

uebeLearnedTiuFiu =
  [ "Czy odnalazłeś już mój stary pistolet Luger Parabellum - Excaliber?",
    "On pozwoli ci pokonać złych zbrodniarzy.",
    "...Powodzenia przyjacielu."
  ]

uebeEnding =
  [ "Oto nasz wybawca Bananine!!",
    "A oto twoja nagroda: dożywotni zapas bananów;",
    "możesz do końca życia żyć w naszej rodzinie i cieszyć się naszymi bananami.",
    "Pokój z tobą.",
    "<ZWYCIĘSTWO>"
  ]

boboDefault =
  [ "U! U! A! A! AAAA!!",
    "BANANA!"
  ]

boboBananaQuestion =
  [ "BANANA!!! DAJ BANANA!",
    "<tak/nie>" 
  ]

boboRevealGun = 
  [ "DOBRE! BANANA!", 
    "<ucieszony Bobo wskazuje na broń ukrytą w trawie>"
  ]

boboSad = 
  [ "Bobo chce banan :(" ]

andrzejDefault =
  [ "Nie rozmawiam z takimi jak ty!",
    "Wynoś się stąd, albo przerobię cię na dywan!"
  ]

pajakDefault =
  [ "ASSHSHSHHSHSHHHHH!!!"
  ]

-- attacks

uebeAttackDefault =
  [ "Jak śmiesz!?",
    "<Uebe zabija cię magicznym zaklęciem Edudu>"
  ]

uebeAttackTask =
  [ "Dobrze, jesteś gotowy.",
    "Skieruj się teraz na wschód i odzyskaj mój portfel."
  ]

boboAttack =
  [ "\"AAAAAAA!!!!\" - Bobo umiera.",
    "Zaskoczony Koko wyrywa ci ręce."
  ]

kokoAttack =
  [ "Koko patrzy na ciebie zaskoczony,",
    "jednym szybkim ruchem wyrywa ci wszystkie kończyny."
  ]

pajakKill =
  [ "Rzucasz na pająka zaklęcie mistrza Uebe - Potassium.",
    "Pająk momentalnie kurczy się do rozmiaru naparstka, a ty go rozdeptujesz.",
    "Zauważasz, że z pająka wypadł portfel mistrza Uebe."
  ]

pajakAttackDefault =
  [ "SSSDHHSHHSHSHSHHS",
    "<pająk z łatwością cię rozgniata na ścianie swoją wielką nogą>"
  ]

andrzejKill =
  [ "Zastrzeliłeś łotrzyka Andrzeja.",
    "Z jego spodni wypadł klucz do fortu."
  ]

andrzejAttackDefault =
  [ "Andrzej jednym pchnięciem kłusowniczego sztyletu ",
    "morduje cię i przerabia na dywan."
  ]

unkillableDefault =
  [ "HAHA, nie możesz mnie zabić."
  ]
