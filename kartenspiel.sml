datatype reihe = Kreuz 
                 | Pik 
                 | Herz
                 | Karo;
datatype wert = Zahl of int
                 | Bube
                 | Dame
                 | Koenig
                 | Ass;
type karte = reihe * wert;

datatype farbe = Schwarz
                 | Rot
datatype zug = Ablegen of karte 
               | Aufnehmen;

exception IllegalerZug;

fun kartenfarbe (karte) =
let val (farbe, bild) = karte in
  case farbe of 
    Kreuz => Schwarz
    | Pik => Schwarz
    | Herz => Rot
    | Karo => Rot
end;

fun kartenwert (karte) = 
let val (farbe, bild) = karte in 
  case bild of
    Bube => 10
    | Dame => 10
    | Koenig => 10
    | Ass => 11
    | Zahl zahl_karte => zahl_karte
end;

fun entferne_karte (karten_liste, gesuchte_karte) =
  case karten_liste of
    [] => []
    | erste_karte::rest_karten => 
if erste_karte = gesuchte_karte
then rest_karten
else erste_karte::entferne_karte(rest_karten, gesuchte_karte);