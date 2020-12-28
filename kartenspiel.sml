datatype reihe = Kreuz 
                 | Pik 
                 | Herz
                 | Karo
datatype wert = Zahl of int
                 | Bube
                 | Dame
                 | Koenig
                 | Ass;
type karte = reihe * wert;

datatype farbe = Schwarz
                 | Rot;
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

kartenfarbe (Pik, Bube);