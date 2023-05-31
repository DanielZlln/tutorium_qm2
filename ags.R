library(sf)

load("ags.RDATA")

head(ags$GEN)

head(ags$geometry)
str(ags)


regional <- data$Kreis
regional_id <- data$ID

ags_kreis <- ags$GEN
ags_id <- ags$AGS

# Alle ags_kreise
print(ags_kreis)
print(ags_id)

# Welche Namen stimmen überein
uebereinstimmende_elemente <- intersect(regional, ags_kreis)

# Anzahl der übereinstimmenden Elemente
anzahl_uebereinstimmende_elemente <- length(uebereinstimmende_elemente)

# Ausgabe der Ergebnisse
print(uebereinstimmende_elemente)
print(anzahl_uebereinstimmende_elemente)


uebereinstimmende_elemente_id <- intersect(regional_id, ags_id)
anzahl_uebereinstimmende_elemente_id <- length(uebereinstimmende_elemente_id)

print(uebereinstimmende_elemente_id)
print(anzahl_uebereinstimmende_elemente_id)

# Welche Kreise fehlen
keine_elemente <- setdiff(regional_id, ags_id)
anzahl_keine_elemente <- length(keine_elemente)

print(keine_elemente)
print(anzahl_keine_elemente)

# Welche Kreise fliegen raus
kreise <- subset(data, ID %in% keine_elemente)$Kreis

print(kreise)
