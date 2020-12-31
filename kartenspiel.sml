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

fun alle_farben_gleich (karten_liste) =
  case karten_liste of 
    nil => true
    | erste_karte::nil => true
    | erste_karte::zweite_karte::rest_karten =>
kartenfarbe (erste_karte) = kartenfarbe (zweite_karte) andalso alle_farben_gleich(zweite_karte::rest_karten);

fun kartensumme (karten_liste) = 
  case karten_liste of 
    nil => 0
    | erste_karte::nil => kartenwert (erste_karte)
    | erste_karte::rest_karten =>
kartenwert (erste_karte) + kartensumme (rest_karten);

fun berechne_punkte (karten_liste, zielwert) =
if kartensumme (karten_liste) > zielwert
then 3 * (kartensumme (karten_liste) - zielwert)
else zielwert - kartensumme (karten_liste);

fun punktestand (karten_liste, zielwert) =
if alle_farben_gleich (karten_liste)
then berechne_punkte (karten_liste, zielwert) * 2
else berechne_punkte (karten_liste, zielwert);

fun spielablauf (karten_liste, zuege_liste, zielwert) =
  let fun berechne_hand_karten (karten_liste, zuege_liste, hand_karten_liste) =
   case zuege_liste of 
    [] => hand_karten_liste
    | erster_zug::restliche_zuege =>
      case erster_zug of
      Ablegen (karte) => 
        berechne_hand_karten (karten_liste, restliche_zuege, entferne_karte (hand_karten_liste, karte))
      | Aufnehmen => 
        case karten_liste of 
          [] => raise IllegalerZug
          | erste_karte::restliche_karten => 
            berechne_hand_karten (restliche_karten, restliche_zuege, erste_karte::hand_karten_liste)
  in 
  punktestand (berechne_hand_karten (karten_liste, zuege_liste, []), zielwert)
  end;