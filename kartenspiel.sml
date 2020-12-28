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
let val (farbe, _) = karte in
  case farbe of 
  Kreuz => Schwarz
    | Pik => Schwarz
    | Herz => Rot
    | Karo => Rot
end;

fun kartenwert (karte) = 
let val (_, bild) = karte in 
  case bild of
  Bube => 10
    | Dame => 10
    | Koenig => 10
    | Ass => 11
    | Zahl zahlKarte => zahlKarte
end;

kartenfarbe (Pik, Bube);
kartenwert (Pik, Dame);
kartenwert (Kreuz, Zahl 2);